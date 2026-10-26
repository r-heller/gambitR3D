"""Model state, inference, training steps and checkpoints.

Losses
  policy  cross-entropy against the normalised legal-action target over the
          full candidate set (padding masked); rows without a target are
          excluded from the denominator.
  wdl     cross-entropy on verified terminal outcomes only (outcome >= 0);
          truncated or unknown outcomes carry -1 and are masked out.
  aux     mean squared error of the separately named auxiliary heuristic
          head against normalised search scores (aux_mask = 1 only).

Data order is a pure function of (seed, epoch): epoch e uses the
permutation of numpy.random.default_rng([seed, e]); the cursor (epoch,
offset) is therefore enough to resume exactly. The learning rate is a
pure function of the step number, so no scheduler object is pickled.
"""

import json
import math
import os
import shutil
import time

import numpy as np
import torch

from . import tensors as T
from .model import SquareTransformer, parameter_report, resolve_config

CHECKPOINT_FORMAT = "g3-checkpoint/1"
DATASET_FORMAT = "g3-dataset/1"
SHARD_KEYS = (
    "tok_cat", "tok_num", "cand_cat", "cand_num", "n_cand",
    "policy_target", "policy_mask", "outcome", "aux_target", "aux_mask",
)
ADAMW_KEYS = ("lr", "betas", "eps", "weight_decay", "amsgrad", "maximize")


class WorkerError(Exception):
    def __init__(self, code, message):
        super().__init__(message)
        self.code = code
        self.message = message


def lr_at(step, sched, base_lr):
    """Learning rate for the update with zero-based index `step`."""
    warm = int(sched.get("warmup_steps", 0))
    total = int(sched.get("total_steps", 0))
    floor = float(sched.get("min_lr_ratio", 0.1))
    f = 1.0
    if warm > 0 and step < warm:
        f = (step + 1) / warm
    elif sched.get("kind", "cosine") == "cosine" and total > warm:
        prog = min(1.0, (step - warm) / max(1, total - warm))
        f = floor + (1.0 - floor) * 0.5 * (1.0 + math.cos(math.pi * prog))
    return base_lr * f


class Session:
    def __init__(self, layout, schema_hash, threads):
        self.layout = layout
        self.schema_hash = schema_hash
        self.threads = threads
        self.model = None
        self.cfg = None
        self.digest = None
        self.step = 0
        self.cursor = {"epoch": 0, "offset": 0}
        self.optimizer = None
        self.opt_cfg = None
        self.pending_opt = None
        self.train_meta = {}
        self.datasets = {}
        self.rules_hash = None
        self.action_schema = None

    # Model lifecycle -------------------------------------------------------

    def initialize(self, cfg, seed):
        cfg = resolve_config(cfg)
        torch.manual_seed(int(seed))
        model = SquareTransformer(cfg, self.layout)
        model.eval()
        self.model, self.cfg = model, cfg
        self.step = 0
        self.cursor = {"epoch": 0, "offset": 0}
        self.optimizer = None
        self.opt_cfg = None
        self.pending_opt = None
        self.train_meta = {"seed": int(seed)}
        self.refresh_digest()
        return self.describe()

    def refresh_digest(self):
        self.digest = T.tensor_digest(self.model.state_dict())
        return self.digest

    def describe(self):
        total, groups = parameter_report(self.model)
        return {
            "model_config": self.cfg,
            "parameter_count": total,
            "parameter_groups": groups,
            "weights_digest": self.digest,
            "step": self.step,
            "cursor": self.cursor,
            "dtype": "float32",
            "device": "cpu",
        }

    def require_model(self):
        if self.model is None:
            raise WorkerError("model_not_initialized", "initialize_model or load_checkpoint first")

    # Inference ---------------------------------------------------------------

    def batch_from_positions(self, positions):
        lay = self.layout
        nk, nn_, ck, cn = len(lay.token_vocab), len(lay.token_div), len(lay.cand_vocab), len(lay.cand_div)
        b = len(positions)
        counts = []
        parsed = []
        for i, p in enumerate(positions):
            tc = T.as_int_array(p["tokens_cat"], (lay.n_tokens, nk), "tokens_cat[%d]" % i)
            tn = T.as_int_array(p["tokens_num"], (lay.n_tokens, nn_), "tokens_num[%d]" % i)
            ids = p.get("candidate_ids")
            if not isinstance(ids, list) or not ids or not all(isinstance(x, str) for x in ids):
                raise WorkerError("invalid_request", "candidate_ids must be a non-empty list of strings")
            digest = T.sha256_bytes("\n".join(ids).encode("utf-8"))
            if digest != p.get("candidate_digest"):
                raise WorkerError("digest_mismatch", "candidate_digest does not match candidate_ids at position %d" % i)
            n = len(ids)
            cc = T.as_int_array(p["cand_cat"], (n, ck), "cand_cat[%d]" % i)
            cnum = T.as_int_array(p["cand_num"], (n, cn), "cand_num[%d]" % i)
            self.check_ranges(tc, cc)
            counts.append(n)
            parsed.append((tc, tn, cc, cnum))
        a = max(counts)
        tok_cat = np.zeros((b, lay.n_tokens, nk), dtype=np.int64)
        tok_num = np.zeros((b, lay.n_tokens, nn_), dtype=np.int64)
        cand_cat = np.zeros((b, a, ck), dtype=np.int64)
        cand_num = np.zeros((b, a, cn), dtype=np.int64)
        mask = np.zeros((b, a), dtype=bool)
        for i, (tc, tn, cc, cnum) in enumerate(parsed):
            n = counts[i]
            tok_cat[i], tok_num[i] = tc, tn
            cand_cat[i, :n], cand_num[i, :n] = cc, cnum
            mask[i, :n] = True
        return (torch.from_numpy(tok_cat), torch.from_numpy(tok_num), torch.from_numpy(cand_cat),
                torch.from_numpy(cand_num), torch.from_numpy(mask)), counts

    def check_ranges(self, tc, cc):
        lay = self.layout
        tv = np.asarray(lay.token_vocab)
        cv = np.asarray(lay.cand_vocab)
        if (tc < 0).any() or (tc >= tv).any():
            raise WorkerError("invalid_features", "token feature outside its vocabulary")
        if cc.size and ((cc < 0).any() or (cc >= cv).any()):
            raise WorkerError("invalid_features", "candidate feature outside its vocabulary")

    def infer(self, positions):
        self.require_model()
        if not isinstance(positions, list) or not positions:
            raise WorkerError("invalid_request", "positions must be a non-empty list")
        (tc, tn, cc, cn, mask), counts = self.batch_from_positions(positions)
        self.model.eval()
        with torch.no_grad():
            logits, wdl, aux = self.model(tc, tn, cc, cn, mask)
            probs = torch.softmax(wdl, dim=-1)
        out = []
        for i, n in enumerate(counts):
            lg = logits[i, :n].tolist()
            wl = wdl[i].tolist()
            pr = probs[i].tolist()
            vals = lg + wl + pr + [float(aux[i])]
            if not all(math.isfinite(v) for v in vals):
                raise WorkerError("nonfinite_output", "model produced a non-finite value at position %d" % i)
            out.append({
                "candidate_digest": positions[i]["candidate_digest"],
                "n_candidates": n,
                "logits": lg,
                "wdl_logits": wl,
                "wdl": pr,
                "value": pr[0] - pr[2],
                "expected_score": pr[0] + 0.5 * pr[1],
                "aux_heuristic": float(aux[i]),
            })
        return out

    # Data ----------------------------------------------------------------------

    def load_dataset(self, spec, allowed):
        path = os.path.realpath(spec["manifest_path"])
        if not allowed(path):
            raise WorkerError("path_not_allowed", "dataset manifest outside the allowed roots")
        data, digest = T.read_verified(path)
        if spec.get("manifest_sha256") and digest != spec["manifest_sha256"]:
            raise WorkerError("artifact_hash_mismatch", "dataset manifest hash differs from the request")
        split = spec.get("split", "train")
        key = (digest, split)
        if key in self.datasets:
            return self.datasets[key], digest
        man = json.loads(data.decode("utf-8"), parse_constant=_reject_constant)
        if man.get("format") != DATASET_FORMAT:
            raise WorkerError("invalid_request", "unknown dataset format")
        if man.get("feature_schema_hash") != self.schema_hash:
            raise WorkerError("hash_mismatch", "dataset feature schema differs from the worker's")
        root = os.path.dirname(path)
        parts = []
        for sh in man.get("shards", []):
            if sh.get("split") != split:
                continue
            rel = sh["file"]
            if os.path.isabs(rel) or ".." in rel.replace("\\", "/").split("/"):
                raise WorkerError("path_not_allowed", "shard path must be relative to the dataset")
            arrays, _ = _load_shard(os.path.join(root, rel), sh["sha256"])
            parts.append(arrays)
        if not parts:
            raise WorkerError("invalid_request", "dataset has no shard for split '%s'" % split)
        merged = _merge_shards(parts)
        self.datasets[key] = merged
        return merged, digest

    # Training ----------------------------------------------------------------

    def ensure_optimizer(self, opt):
        hyper = {
            "lr": float(opt.get("lr", 1e-3)),
            "betas": [float(x) for x in opt.get("betas", [0.9, 0.999])],
            "eps": float(opt.get("eps", 1e-8)),
            "weight_decay": float(opt.get("weight_decay", 0.01)),
            "amsgrad": False,
            "maximize": False,
        }
        if self.optimizer is not None:
            if hyper != self.opt_cfg:
                raise WorkerError("optimizer_config_mismatch", "optimizer settings differ from the active optimizer")
            return
        self.optimizer = torch.optim.AdamW(
            self.model.parameters(), lr=hyper["lr"], betas=tuple(hyper["betas"]), eps=hyper["eps"],
            weight_decay=hyper["weight_decay"], foreach=False,
        )
        self.opt_cfg = hyper
        if self.pending_opt is not None:
            saved_hyper, state = self.pending_opt
            if saved_hyper != hyper:
                raise WorkerError("optimizer_config_mismatch", "checkpoint optimizer settings differ from the request")
            # Start from the new optimizer's own complete state dict (all
            # group keys of this torch version), then insert the saved
            # per-parameter tensors.
            sd = self.optimizer.state_dict()
            sd["state"] = state
            self.optimizer.load_state_dict(sd)
            self.pending_opt = None

    def batch_indices(self, n, bs, seed):
        ep, off = int(self.cursor["epoch"]), int(self.cursor["offset"])
        perm = np.random.default_rng([int(seed), ep]).permutation(n)
        idx = perm[off:off + bs]
        off += len(idx)
        if off >= n:
            ep, off = ep + 1, 0
        self.cursor = {"epoch": ep, "offset": off}
        return idx

    def train(self, payload, data, should_stop, emit_progress, deadline):
        self.require_model()
        steps = int(payload.get("steps", 1))
        bs = int(payload.get("batch_size", 32))
        if steps < 0 or bs < 1:
            raise WorkerError("invalid_request", "steps must be >= 0 and batch_size >= 1")
        seed = int(payload.get("seed", self.train_meta.get("seed", 0)))
        opt = payload.get("optimizer", {})
        sched = payload.get("schedule", {})
        lw = payload.get("loss_weights", {})
        w_pol, w_wdl, w_aux = float(lw.get("policy", 1.0)), float(lw.get("wdl", 1.0)), float(lw.get("aux", 0.0))
        clip = float(payload.get("clip_norm", 1.0))
        self.ensure_optimizer(opt)
        self.train_meta.update({"seed": seed, "schedule": sched, "loss_weights": lw, "clip_norm": clip, "batch_size": bs})
        n = data["tok_cat"].shape[0]
        digest_before = self.digest
        metrics = []
        stop_reason = "completed"
        t0 = time.time()
        positions_seen = 0
        for _ in range(steps):
            if should_stop():
                stop_reason = "cancelled"
                break
            if deadline is not None and time.time() > deadline:
                stop_reason = "deadline"
                break
            ts = time.time()
            idx = self.batch_indices(n, bs, seed)
            m = self.train_step(data, idx, opt, sched, w_pol, w_wdl, w_aux, clip)
            m["seconds"] = time.time() - ts
            positions_seen += len(idx)
            metrics.append(m)
            emit_progress({"step": self.step, "of": steps, "metrics": m})
        self.model.eval()
        self.refresh_digest()
        elapsed = time.time() - t0
        return {
            "stop_reason": stop_reason,
            "steps_completed": len(metrics),
            "step": self.step,
            "cursor": self.cursor,
            "metrics": metrics,
            "weights_digest_before": digest_before,
            "weights_digest_after": self.digest,
            "weights_changed": digest_before != self.digest,
            "elapsed_seconds": elapsed,
            "positions_per_second": positions_seen / elapsed if elapsed > 0 else None,
        }

    def tensors_for(self, data, idx):
        nmax = int(data["n_cand"][idx].max())
        g = lambda k: torch.from_numpy(np.ascontiguousarray(data[k][idx]))  # noqa: E731
        tc = g("tok_cat").long()
        tn = g("tok_num").long()
        cc = torch.from_numpy(np.ascontiguousarray(data["cand_cat"][idx][:, :nmax])).long()
        cn = torch.from_numpy(np.ascontiguousarray(data["cand_num"][idx][:, :nmax])).long()
        nc = g("n_cand").long()
        mask = torch.arange(nmax).unsqueeze(0) < nc.unsqueeze(1)
        target = torch.from_numpy(np.ascontiguousarray(data["policy_target"][idx][:, :nmax])).float()
        return tc, tn, cc, cn, mask, target, g("policy_mask").bool(), g("outcome").long(), g("aux_target").float(), g("aux_mask").bool()

    def losses(self, data, idx, w_pol, w_wdl, w_aux):
        tc, tn, cc, cn, mask, target, pmask, outcome, aux_t, amask = self.tensors_for(data, idx)
        logits, wdl, aux = self.model(tc, tn, cc, cn, mask)
        logp = torch.log_softmax(logits, dim=-1).masked_fill(~mask, 0.0)
        per_pos = -(target * logp).sum(dim=-1)
        n_pol = int(pmask.sum())
        zero = logits.masked_fill(~mask, 0.0).sum() * 0.0
        policy_loss = per_pos[pmask].mean() if n_pol else zero
        wvalid = outcome >= 0
        n_wdl = int(wvalid.sum())
        wdl_loss = torch.nn.functional.cross_entropy(wdl[wvalid], outcome[wvalid]) if n_wdl else zero
        n_aux = int(amask.sum())
        aux_loss = torch.mean((aux[amask] - aux_t[amask]) ** 2) if n_aux else zero
        loss = w_pol * policy_loss + w_wdl * wdl_loss + w_aux * aux_loss
        with torch.no_grad():
            p = torch.softmax(logits, dim=-1)
            ent = -(p * logp).sum(dim=-1)
            top1 = (logits.argmax(dim=-1) == target.argmax(dim=-1)).float()
            entropy = float(ent[pmask].mean()) if n_pol else None
            agree = float(top1[pmask].mean()) if n_pol else None
            brier = None
            if n_wdl:
                onehot = torch.nn.functional.one_hot(outcome[wvalid], 3).float()
                brier = float(((torch.softmax(wdl[wvalid], -1) - onehot) ** 2).sum(-1).mean())
        stats = {
            "policy_loss": float(policy_loss), "wdl_loss": float(wdl_loss), "aux_loss": float(aux_loss),
            "n_positions": int(len(idx)), "n_policy": n_pol, "n_wdl": n_wdl, "n_aux": n_aux,
            "policy_entropy": entropy, "top1_agreement": agree, "wdl_brier": brier,
        }
        return loss, stats

    def train_step(self, data, idx, opt, sched, w_pol, w_wdl, w_aux, clip):
        self.model.train()
        loss, stats = self.losses(data, idx, w_pol, w_wdl, w_aux)
        lv = float(loss)
        if not math.isfinite(lv):
            raise WorkerError("non_finite_loss", "training loss is not finite at step %d" % (self.step + 1))
        self.optimizer.zero_grad(set_to_none=True)
        loss.backward()
        params = [p for p in self.model.parameters() if p.grad is not None]
        gnorm = torch.nn.utils.clip_grad_norm_(params, max_norm=clip if clip > 0 else float("inf"))
        gnorm = float(gnorm)
        if not math.isfinite(gnorm):
            raise WorkerError("non_finite_gradient", "gradient norm is not finite at step %d" % (self.step + 1))
        nonzero = sum(1 for p in params if bool((p.grad != 0).any()))
        lr = lr_at(self.step, sched, float(opt.get("lr", 1e-3)))
        for grp in self.optimizer.param_groups:
            grp["lr"] = lr
        self.optimizer.step()
        self.step += 1
        stats.update({"step": self.step, "loss": lv, "grad_norm": gnorm, "params_with_gradient": nonzero, "lr": lr})
        return stats

    def evaluate(self, data, w_pol, w_wdl, w_aux, batch=256):
        self.require_model()
        self.model.eval()
        n = data["tok_cat"].shape[0]
        tot = {"loss": 0.0, "policy_loss": 0.0, "wdl_loss": 0.0, "n_policy": 0, "n_wdl": 0, "top1": 0.0, "n": n}
        with torch.no_grad():
            for s in range(0, n, batch):
                idx = np.arange(s, min(n, s + batch))
                loss, st = self.losses(data, idx, w_pol, w_wdl, w_aux)
                tot["loss"] += float(loss) * len(idx)
                tot["policy_loss"] += st["policy_loss"] * st["n_policy"]
                tot["wdl_loss"] += st["wdl_loss"] * st["n_wdl"]
                tot["n_policy"] += st["n_policy"]
                tot["n_wdl"] += st["n_wdl"]
                if st["top1_agreement"] is not None:
                    tot["top1"] += st["top1_agreement"] * st["n_policy"]
        return {
            "n_positions": n,
            "loss": tot["loss"] / n if n else None,
            "policy_loss": tot["policy_loss"] / tot["n_policy"] if tot["n_policy"] else None,
            "wdl_loss": tot["wdl_loss"] / tot["n_wdl"] if tot["n_wdl"] else None,
            "top1_agreement": tot["top1"] / tot["n_policy"] if tot["n_policy"] else None,
            "n_policy": tot["n_policy"],
            "n_wdl": tot["n_wdl"],
        }

    # Checkpoints ---------------------------------------------------------------

    def save_checkpoint(self, parent, name, extra_meta, allowed):
        self.require_model()
        parent = os.path.realpath(parent)
        if not allowed(parent):
            raise WorkerError("path_not_allowed", "checkpoint directory outside the allowed roots")
        if not name or "/" in name or "\\" in name or name.startswith("."):
            raise WorkerError("invalid_request", "invalid checkpoint name")
        final = os.path.join(parent, name)
        if os.path.exists(final):
            raise WorkerError("invalid_request", "checkpoint already exists: %s" % name)
        os.makedirs(parent, exist_ok=True)
        tmp = os.path.join(parent, ".%s.tmp-%d" % (name, os.getpid()))
        if os.path.exists(tmp):
            shutil.rmtree(tmp)
        os.makedirs(tmp)
        try:
            wbytes = T.safetensors_bytes(self.model.state_dict(), {"format": CHECKPOINT_FORMAT})
            T.write_bytes_synced(os.path.join(tmp, "weights.safetensors"), wbytes)
            opt_tensors, opt_meta = self.optimizer_tensors()
            opt_tensors["rng/torch_cpu"] = torch.get_rng_state()
            obytes = T.safetensors_bytes(opt_tensors, {"format": CHECKPOINT_FORMAT})
            T.write_bytes_synced(os.path.join(tmp, "optimizer.safetensors"), obytes)
            meta = {
                "format": CHECKPOINT_FORMAT,
                "model_config": self.cfg,
                "feature_schema": self.layout.name,
                "feature_schema_hash": self.schema_hash,
                "rules_hash": self.rules_hash,
                "action_schema": self.action_schema,
                "step": self.step,
                "data_cursor": self.cursor,
                "optimizer": opt_meta,
                "training": self.train_meta,
                "rng": {"torch_cpu": "optimizer.safetensors:rng/torch_cpu", "data_order": "numpy default_rng([seed, epoch])"},
                "weights_digest": self.digest,
                "files": {
                    "weights.safetensors": T.sha256_bytes(wbytes),
                    "optimizer.safetensors": T.sha256_bytes(obytes),
                },
                "extra": extra_meta or {},
                "saved_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
            }
            mbytes = json.dumps(meta, sort_keys=True, indent=1, allow_nan=False).encode("utf-8")
            T.write_bytes_synced(os.path.join(tmp, "checkpoint.json"), mbytes)
            T.fsync_dir(tmp)
            os.rename(tmp, final)
            T.fsync_dir(parent)
        except Exception:
            shutil.rmtree(tmp, ignore_errors=True)
            raise
        return {
            "path": final,
            "step": self.step,
            "weights_digest": self.digest,
            "files": dict(meta["files"], **{"checkpoint.json": T.sha256_bytes(mbytes)}),
        }

    def optimizer_tensors(self):
        if self.optimizer is None:
            return {}, {"type": None}
        names = [n for n, _ in self.model.named_parameters()]
        st = self.optimizer.state_dict()
        out = {}
        for i, nm in enumerate(names):
            s = st["state"].get(i)
            if not s:
                continue
            out["exp_avg/" + nm] = s["exp_avg"]
            out["exp_avg_sq/" + nm] = s["exp_avg_sq"]
            stp = s["step"]
            stp = stp if torch.is_tensor(stp) else torch.tensor(float(stp))
            out["step/" + nm] = stp.reshape(1).to(torch.float32)
        return out, {"type": "AdamW", "hyper": self.opt_cfg, "param_names": names}

    def load_checkpoint(self, path, expected, with_optimizer, allowed):
        path = os.path.realpath(path)
        if not allowed(path):
            raise WorkerError("path_not_allowed", "checkpoint outside the allowed roots")
        try:
            mbytes, mdig = T.read_verified(os.path.join(path, "checkpoint.json"), expected.get("checkpoint.json"))
            meta = json.loads(mbytes.decode("utf-8"), parse_constant=_reject_constant)
        except ValueError as e:
            raise WorkerError("artifact_hash_mismatch", str(e))
        if meta.get("format") != CHECKPOINT_FORMAT:
            raise WorkerError("invalid_request", "unknown checkpoint format")
        if meta.get("feature_schema_hash") != self.schema_hash:
            raise WorkerError("hash_mismatch", "checkpoint feature schema differs from the worker's")
        if self.rules_hash is not None and meta.get("rules_hash") not in (None, self.rules_hash):
            raise WorkerError("hash_mismatch", "checkpoint rules hash differs from the session's")
        files = meta.get("files", {})
        try:
            wexp = expected.get("weights.safetensors") or files.get("weights.safetensors")
            if wexp != files.get("weights.safetensors"):
                raise ValueError("weights hash differs from the checkpoint record")
            wbytes, _ = T.read_verified(os.path.join(path, "weights.safetensors"), wexp)
            obytes = None
            if with_optimizer:
                oexp = expected.get("optimizer.safetensors") or files.get("optimizer.safetensors")
                if oexp != files.get("optimizer.safetensors"):
                    raise ValueError("optimizer hash differs from the checkpoint record")
                obytes, _ = T.read_verified(os.path.join(path, "optimizer.safetensors"), oexp)
        except (ValueError, OSError) as e:
            raise WorkerError("artifact_hash_mismatch", str(e))
        cfg = resolve_config(meta["model_config"])
        model = SquareTransformer(cfg, self.layout)
        state = T.load_safetensors_bytes(wbytes)
        model.load_state_dict(state, strict=True)
        model.eval()
        self.model, self.cfg = model, cfg
        self.refresh_digest()
        if meta.get("weights_digest") and meta["weights_digest"] != self.digest:
            raise WorkerError("artifact_hash_mismatch", "weights digest differs from the checkpoint record")
        self.step = int(meta.get("step", 0))
        self.cursor = dict(meta.get("data_cursor") or {"epoch": 0, "offset": 0})
        self.train_meta = dict(meta.get("training") or {})
        self.optimizer, self.opt_cfg, self.pending_opt = None, None, None
        if with_optimizer and obytes is not None:
            ot = T.load_safetensors_bytes(obytes)
            om = meta.get("optimizer") or {}
            if om.get("type") == "AdamW":
                names = [n for n, _ in model.named_parameters()]
                if om.get("param_names") != names:
                    raise WorkerError("invalid_request", "optimizer parameter names do not match the model")
                state_d = {}
                for i, nm in enumerate(names):
                    if ("exp_avg/" + nm) in ot:
                        state_d[i] = {
                            "step": ot["step/" + nm].reshape(()).clone(),
                            "exp_avg": ot["exp_avg/" + nm].clone(),
                            "exp_avg_sq": ot["exp_avg_sq/" + nm].clone(),
                        }
                self.pending_opt = (om["hyper"], state_d)
            elif om.get("type") is not None:
                raise WorkerError("invalid_request", "unsupported optimizer type in checkpoint")
            if "rng/torch_cpu" in ot:
                torch.set_rng_state(ot["rng/torch_cpu"].to(torch.uint8))
        out = self.describe()
        out["checkpoint_sha256"] = mdig
        out["rules_hash"] = meta.get("rules_hash")
        out["optimizer_restored"] = self.pending_opt is not None
        return out


def _reject_constant(name):
    raise ValueError("non-finite number %s" % name)


def _load_shard(path, sha):
    try:
        arrays, _ = T.load_numpy_safetensors(path, sha)
    except ValueError as e:
        raise WorkerError("artifact_hash_mismatch", str(e))
    missing = [k for k in SHARD_KEYS if k not in arrays]
    if missing:
        raise WorkerError("invalid_request", "shard lacks tensors: %s" % ", ".join(missing))
    n = arrays["tok_cat"].shape[0]
    for k in SHARD_KEYS:
        if arrays[k].shape[0] != n:
            raise WorkerError("invalid_request", "shard tensor %s has the wrong length" % k)
    for k in ("policy_target", "aux_target"):
        if not np.isfinite(arrays[k]).all():
            raise WorkerError("nonfinite_value", "shard tensor %s is not finite" % k)
    return arrays, sha


def _merge_shards(parts):
    a = max(p["cand_cat"].shape[1] for p in parts)
    out = {}
    for k in SHARD_KEYS:
        xs = []
        for p in parts:
            x = p[k]
            if k in ("cand_cat", "cand_num", "policy_target") and x.shape[1] < a:
                pad = [(0, 0)] * x.ndim
                pad[1] = (0, a - x.shape[1])
                x = np.pad(x, pad)
            xs.append(x)
        out[k] = np.concatenate(xs, axis=0)
    return out
