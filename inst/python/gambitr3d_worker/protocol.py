"""JSONL protocol loop (protocol 1.x, see docs WORKER_PROTOCOL).

stdout carries protocol messages only, one JSON object per line; every log
line goes to stderr. A reader thread queues incoming lines (refusing lines
over the byte limit), a heartbeat thread reports liveness, and the main
thread executes one request at a time. Long requests poll the queue
between steps, so a ``cancel`` for the active request is honoured at the
next safe point.

Envelope: protocol_version, message_type, request_id, job_id, attempt_id,
operation, payload, plus the work-identity fields state_hash, rules_hash,
feature_schema_hash, action_schema_hash, model_hash, budget and deadline,
which every response echoes exactly.
"""

import argparse
import collections
import json
import os
import platform
import queue
import sys
import threading
import time
import traceback

from . import PROTOCOL_MAJOR, PROTOCOL_VERSION, __version__
from .schema import Layout, load_schema
from .training import Session, WorkerError, _reject_constant

ECHO_FIELDS = ("job_id", "attempt_id", "state_hash", "rules_hash", "feature_schema_hash",
               "action_schema_hash", "model_hash", "budget", "deadline")
OPERATIONS = ("hello", "initialize_model", "infer_batch", "train_chunk", "save_checkpoint",
              "load_checkpoint", "cancel", "shutdown")
LONG_OPERATIONS = ("initialize_model", "train_chunk", "save_checkpoint", "load_checkpoint")


def log(*parts):
    sys.stderr.write("[gambitr3d_worker %s] " % time.strftime("%H:%M:%S") + " ".join(str(p) for p in parts) + "\n")
    sys.stderr.flush()


class Worker:
    def __init__(self, args):
        # Keep a private handle on the real stdout for protocol lines and
        # send anything else that prints to stderr.
        self.out = os.fdopen(os.dup(sys.stdout.fileno()), "w", encoding="utf-8", newline="\n")
        sys.stdout = sys.stderr
        self.args = args
        self.max_line = int(args.max_line_bytes)
        self.heartbeat_s = float(args.heartbeat)
        self.allowed_roots = [os.path.realpath(r) for r in (args.allowed_root or [])]
        self.fault_injection = bool(args.enable_fault_injection)
        self.lock = threading.Lock()
        self.inbox = queue.Queue()
        self.pending = collections.deque()
        self.active = None
        self.cancel_requested = False
        self.shutdown_requested = False
        self.suppress_heartbeat = False
        self.seq = 0
        self.alive = True
        doc, self.schema_hash = load_schema()
        self.layout = Layout(doc)
        import torch

        self.torch = torch
        torch.set_num_threads(int(args.threads))
        try:
            torch.set_num_interop_threads(1)
        except RuntimeError:
            pass
        torch.use_deterministic_algorithms(True)
        self.session = Session(self.layout, self.schema_hash, int(args.threads))
        self.handshake = False

    # Output ----------------------------------------------------------------

    def send(self, msg):
        try:
            line = json.dumps(msg, allow_nan=False, separators=(",", ":"))
        except ValueError:
            line = json.dumps({
                "protocol_version": PROTOCOL_VERSION, "message_type": "error",
                "request_id": msg.get("request_id"), "operation": msg.get("operation"),
                "payload": {"code": "nonfinite_output", "message": "a value could not be encoded as finite JSON"},
            }, separators=(",", ":"))
        with self.lock:
            self.seq += 1
            self.out.write(line + "\n")
            self.out.flush()

    def write_raw(self, text):
        with self.lock:
            self.out.write(text)
            self.out.flush()

    def reply(self, req, mtype, payload):
        msg = {
            "protocol_version": PROTOCOL_VERSION,
            "message_type": mtype,
            "request_id": req.get("request_id") if req else None,
            "operation": req.get("operation") if req else None,
            "payload": payload,
        }
        for k in ECHO_FIELDS:
            msg[k] = req.get(k) if req else None
        self.send(msg)

    def error(self, req, code, message):
        self.reply(req, "error", {"code": code, "message": message})

    # Threads -----------------------------------------------------------------

    def reader(self):
        stdin = sys.stdin.buffer
        while True:
            line = stdin.readline(self.max_line + 1)
            if not line:
                self.inbox.put(("eof", None))
                return
            if len(line) > self.max_line and not (len(line) == self.max_line + 1 and line.endswith(b"\n")):
                while not line.endswith(b"\n"):
                    line = stdin.readline(1 << 16)
                    if not line:
                        break
                self.inbox.put(("oversized", None))
                continue
            self.inbox.put(("line", line))

    def heartbeat(self):
        while self.alive:
            time.sleep(self.heartbeat_s)
            if self.alive and not self.suppress_heartbeat:
                self.send({
                    "protocol_version": PROTOCOL_VERSION, "message_type": "heartbeat",
                    "request_id": self.active, "operation": None,
                    "payload": {"time": time.time(), "pid": os.getpid(), "busy": self.active is not None},
                })

    # Input -------------------------------------------------------------------

    def next_item(self):
        if self.pending:
            return self.pending.popleft()
        return self.inbox.get()

    def parse(self, raw):
        try:
            msg = json.loads(raw.decode("utf-8"), parse_constant=_reject_constant)
        except (ValueError, UnicodeDecodeError) as e:
            return None, ("malformed_json" if "non-finite" not in str(e) else "nonfinite_value", str(e))
        if not isinstance(msg, dict):
            return None, ("malformed_json", "a message must be a JSON object")
        return msg, None

    def validate(self, msg):
        pv = str(msg.get("protocol_version", ""))
        try:
            major = int(pv.split(".")[0])
        except ValueError:
            major = -1
        if major != PROTOCOL_MAJOR:
            raise WorkerError("unsupported_protocol", "protocol version %r is not supported (major %d)" % (pv, PROTOCOL_MAJOR))
        if msg.get("message_type") != "request":
            raise WorkerError("invalid_request", "message_type must be 'request'")
        rid = msg.get("request_id")
        if not isinstance(rid, str) or not rid or len(rid) > 200:
            raise WorkerError("invalid_request", "request_id must be a non-empty string")
        if msg.get("operation") not in OPERATIONS and not (self.fault_injection and msg.get("operation") == "debug_fault"):
            raise WorkerError("unknown_operation", "unknown operation %r" % msg.get("operation"))
        if not isinstance(msg.get("payload", {}), dict):
            raise WorkerError("invalid_request", "payload must be an object")
        fsh = msg.get("feature_schema_hash")
        if fsh is not None and fsh != self.schema_hash:
            raise WorkerError("hash_mismatch", "feature_schema_hash differs from the worker's schema")
        if self.handshake and msg.get("operation") not in ("hello", "cancel", "shutdown"):
            if msg.get("rules_hash") not in (None, self.session.rules_hash):
                raise WorkerError("hash_mismatch", "rules_hash differs from the session handshake")
        mh = msg.get("model_hash")
        if mh is not None and msg.get("operation") in ("infer_batch", "train_chunk", "save_checkpoint"):
            if mh != self.session.digest:
                raise WorkerError("hash_mismatch", "model_hash differs from the loaded weights")

    def poll_cancel(self, rid):
        """Drain queued input; honour a cancel or shutdown for `rid`."""
        while True:
            try:
                item = self.inbox.get_nowait()
            except queue.Empty:
                break
            kind, raw = item
            if kind == "line":
                msg, err = self.parse(raw)
                if msg is not None and msg.get("operation") in ("cancel", "shutdown"):
                    target = (msg.get("payload") or {}).get("target_request_id")
                    if msg.get("operation") == "shutdown":
                        self.shutdown_requested = True
                        self.cancel_requested = True
                        self.pending.append(item)
                        continue
                    if target == rid:
                        self.cancel_requested = True
                        self.reply(msg, "result", {"target_request_id": rid, "accepted": True})
                        continue
                    self.reply(msg, "result", {"target_request_id": target, "accepted": False, "reason": "not_active"})
                    continue
            self.pending.append(item)
        return self.cancel_requested

    def allowed(self, path):
        real = os.path.realpath(path)
        return any(real == r or real.startswith(r + os.sep) for r in self.allowed_roots)

    # Main loop -----------------------------------------------------------------

    def run(self):
        threading.Thread(target=self.reader, daemon=True).start()
        threading.Thread(target=self.heartbeat, daemon=True).start()
        log("ready; protocol", PROTOCOL_VERSION, "pid", os.getpid())
        code = 0
        while True:
            kind, raw = self.next_item()
            if kind == "eof":
                log("stdin closed; exiting")
                break
            if kind == "oversized":
                self.error(None, "oversized_line", "line exceeds %d bytes" % self.max_line)
                continue
            msg, err = self.parse(raw)
            if err:
                self.error(None, err[0], err[1])
                continue
            try:
                self.validate(msg)
            except WorkerError as e:
                self.error(msg, e.code, e.message)
                continue
            op = msg["operation"]
            if op == "shutdown":
                self.reply(msg, "result", {"stopped": True})
                break
            if op == "cancel":
                target = (msg.get("payload") or {}).get("target_request_id")
                self.reply(msg, "result", {"target_request_id": target, "accepted": False, "reason": "not_active"})
                continue
            self.active = msg["request_id"]
            self.cancel_requested = False
            try:
                deadline = msg.get("deadline")
                if deadline is not None and time.time() > float(deadline):
                    raise WorkerError("deadline_exceeded", "the request deadline has passed")
                if op in LONG_OPERATIONS:
                    self.reply(msg, "accepted", {"pid": os.getpid()})
                handler = getattr(self, "op_" + op)
                result = handler(msg)
                if isinstance(result, tuple) and result[0] == "suppress":
                    pass
                elif isinstance(result, tuple) and result[0] == "cancelled":
                    self.reply(msg, "cancelled", result[1])
                else:
                    self.reply(msg, "result", result)
            except WorkerError as e:
                self.error(msg, e.code, e.message)
            except Exception as e:  # report, keep serving
                log("internal error:", traceback.format_exc())
                self.error(msg, "internal_error", "%s: %s" % (type(e).__name__, e))
            finally:
                self.active = None
            if self.shutdown_requested:
                break
        self.alive = False
        return code

    # Operations ---------------------------------------------------------------

    def op_hello(self, msg):
        import numpy
        import safetensors

        p = msg.get("payload") or {}
        client = p.get("client_feature_schema_hash", msg.get("feature_schema_hash"))
        if client is not None and client != self.schema_hash:
            raise WorkerError("hash_mismatch", "client feature schema hash differs from the worker's")
        if msg.get("rules_hash") is None:
            raise WorkerError("invalid_request", "hello needs the client's rules_hash")
        self.session.rules_hash = msg.get("rules_hash")
        self.session.action_schema = msg.get("action_schema_hash")
        self.handshake = True
        t = self.torch
        return {
            "worker_version": __version__,
            "protocol_version": PROTOCOL_VERSION,
            "supported_protocol_major": [PROTOCOL_MAJOR],
            "operations": list(OPERATIONS),
            "python": platform.python_version(),
            "implementation": platform.python_implementation(),
            "platform": platform.platform(),
            "torch": t.__version__,
            "numpy": numpy.__version__,
            "safetensors": safetensors.__version__,
            "device": "cpu",
            "cuda_available": bool(t.cuda.is_available()),
            "cuda_built": t.version.cuda,
            "dtypes": ["float32"],
            "threads": t.get_num_threads(),
            "deterministic_algorithms": bool(t.are_deterministic_algorithms_enabled()),
            "feature_schema": self.layout.name,
            "feature_schema_hash": self.schema_hash,
            "max_line_bytes": self.max_line,
            "heartbeat_seconds": self.heartbeat_s,
            "pid": os.getpid(),
            "fault_injection": self.fault_injection,
        }

    def op_initialize_model(self, msg):
        p = msg.get("payload") or {}
        cfg = p.get("model_config") or {}
        out = self.session.initialize(cfg, int(p.get("seed", 0)))
        return out

    def op_infer_batch(self, msg):
        p = msg.get("payload") or {}
        positions = p.get("positions")
        if isinstance(positions, list) and len(positions) > int(self.args.max_batch):
            raise WorkerError("invalid_request", "batch larger than %d positions" % int(self.args.max_batch))
        t0 = time.time()
        outputs = self.session.infer(positions)
        return {"outputs": outputs, "weights_digest": self.session.digest, "seconds": time.time() - t0}

    def op_train_chunk(self, msg):
        p = msg.get("payload") or {}
        s = self.session
        s.require_model()
        data, ds_digest = s.load_dataset(p.get("dataset") or {}, self.allowed)
        rid = msg["request_id"]

        def emit(progress):
            self.reply(msg, "progress", progress)

        res = s.train(p, data, lambda: self.poll_cancel(rid), emit, msg.get("deadline"))
        res["dataset_manifest_sha256"] = ds_digest
        lw = p.get("loss_weights", {})
        evals = {}
        for split in p.get("evaluate", []) or []:
            if self.poll_cancel(rid):
                break
            spec = dict(p.get("dataset") or {})
            spec["split"] = split
            try:
                edata, _ = s.load_dataset(spec, self.allowed)
            except WorkerError as e:
                evals[split] = {"error": e.message}
                continue
            evals[split] = s.evaluate(edata, float(lw.get("policy", 1.0)), float(lw.get("wdl", 1.0)), float(lw.get("aux", 0.0)))
        res["evaluation"] = evals
        if res["stop_reason"] == "cancelled":
            return ("cancelled", res)
        return res

    def op_save_checkpoint(self, msg):
        p = msg.get("payload") or {}
        return self.session.save_checkpoint(p.get("parent"), p.get("name"), p.get("extra"), self.allowed)

    def op_load_checkpoint(self, msg):
        p = msg.get("payload") or {}
        return self.session.load_checkpoint(p.get("path"), p.get("expected") or {}, bool(p.get("with_optimizer", True)), self.allowed)

    def op_debug_fault(self, msg):
        """Fault injection for recovery tests; refused unless enabled at start."""
        if not self.fault_injection:
            raise WorkerError("unknown_operation", "fault injection is disabled")
        kind = (msg.get("payload") or {}).get("kind")
        if kind == "malformed_line":
            self.write_raw('{"protocol_version": "1.0", "message_type": "result", broken\n')
            return {"injected": kind}
        if kind == "crash":
            log("injected crash")
            os._exit(3)
        if kind == "nonfinite":
            self.write_raw('{"protocol_version":"1.0","message_type":"result","request_id":%s,"operation":"debug_fault","payload":{"value":NaN}}\n'
                           % json.dumps(msg["request_id"]))
            return {"injected": kind}
        if kind == "wrong_echo":
            bad = dict(msg)
            bad["rules_hash"] = "0" * 64
            self.reply(bad, "result", {"injected": kind})
            return ("suppress", None)
        if kind == "duplicate_result":
            self.reply(msg, "result", {"injected": kind, "copy": 1})
            return {"injected": kind, "copy": 2}
        if kind == "stale_response":
            other = dict(msg)
            other["request_id"] = "req-stale-" + msg["request_id"]
            self.reply(other, "result", {"injected": kind})
            return {"injected": kind}
        if kind == "hang":
            self.suppress_heartbeat = True
            time.sleep(float((msg.get("payload") or {}).get("seconds", 60)))
            self.suppress_heartbeat = False
            return {"injected": kind}
        raise WorkerError("invalid_request", "unknown fault kind %r" % kind)


def probe():
    """Print versions and the schema hash as one JSON line (setup check)."""
    import numpy
    import safetensors
    import torch

    doc, h = load_schema()
    print(json.dumps({
        "python": platform.python_version(),
        "executable": sys.executable,
        "torch": torch.__version__,
        "numpy": numpy.__version__,
        "safetensors": safetensors.__version__,
        "cuda_available": bool(torch.cuda.is_available()),
        "cuda_built": torch.version.cuda,
        "threads_default": torch.get_num_threads(),
        "worker_version": __version__,
        "protocol_version": PROTOCOL_VERSION,
        "feature_schema": doc["schema"],
        "feature_schema_hash": h,
    }, separators=(",", ":")))
    return 0


def main(argv):
    ap = argparse.ArgumentParser(prog="gambitr3d_worker")
    ap.add_argument("--probe", action="store_true")
    ap.add_argument("--threads", type=int, default=1)
    ap.add_argument("--max-line-bytes", type=int, default=8 * 1024 * 1024)
    ap.add_argument("--max-batch", type=int, default=512)
    ap.add_argument("--heartbeat", type=float, default=2.0)
    ap.add_argument("--allowed-root", action="append")
    ap.add_argument("--enable-fault-injection", action="store_true")
    args = ap.parse_args(argv)
    if args.probe:
        return probe()
    if args.threads < 1 or args.threads > 64:
        log("invalid --threads")
        return 2
    return Worker(args).run()
