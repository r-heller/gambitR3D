"""Square-token policy/value transformer with a shared candidate scorer.

Inputs (all integer, produced by R):
  tok_cat  [B, 72, Kc]  categorical token features (0 = not applicable)
  tok_num  [B, 72, Kn]  numeric token numerators (divided by schema divisors)
  cand_cat [B, A, Cc]   categorical candidate features (all zero = padding)
  cand_num [B, A, Cn]   numeric candidate numerators
  cand_mask [B, A]      True for real candidates

Outputs:
  logits [B, A]  one score per real candidate (padding is -inf)
  wdl    [B, 3]  win/draw/loss logits from the side to act
  aux    [B]     separately named auxiliary heuristic head (tanh), not WDL

Each candidate is scored by one shared MLP from its own features and the
contextual embeddings it gathers (source square, destination square,
destination board, global token, mean of all tokens), so permuting the
candidates permutes the logits in the same way.
"""

import torch
from torch import nn

ARCHITECTURE = "square-transformer"
ARCHITECTURE_VERSION = "g3-square-transformer/1"

PROFILES = {
    "tiny": {"width": 64, "layers": 2, "heads": 4, "mlp_ratio": 4, "dropout": 0.0},
    "research": {"width": 256, "layers": 6, "heads": 8, "mlp_ratio": 4, "dropout": 0.1},
}


def resolve_config(cfg):
    """Fill a model configuration from its profile and check it."""
    out = dict(PROFILES.get(cfg.get("profile", "tiny"), {}))
    for k in ("width", "layers", "heads", "mlp_ratio", "dropout"):
        if cfg.get(k) is not None:
            out[k] = cfg[k]
    out["profile"] = cfg.get("profile", "custom")
    out["architecture"] = ARCHITECTURE
    out["architecture_version"] = ARCHITECTURE_VERSION
    w, h = int(out["width"]), int(out["heads"])
    if w <= 0 or h <= 0 or w % h != 0:
        raise ValueError("width must be a positive multiple of heads")
    if int(out["layers"]) < 1 or int(out["mlp_ratio"]) < 1:
        raise ValueError("layers and mlp_ratio must be at least 1")
    d = float(out["dropout"])
    if not 0.0 <= d < 1.0:
        raise ValueError("dropout must be in [0, 1)")
    out["width"], out["heads"] = w, h
    out["layers"], out["mlp_ratio"], out["dropout"] = int(out["layers"]), int(out["mlp_ratio"]), d
    return out


class SquareTransformer(nn.Module):
    def __init__(self, cfg, layout):
        super().__init__()
        d = cfg["width"]
        self.cfg = cfg
        self.layout = layout
        self.tok_cat = nn.ModuleList(nn.Embedding(v, d, padding_idx=0) for v in layout.token_vocab)
        self.tok_num = nn.Linear(len(layout.token_div), d)
        self.register_buffer("tok_div", torch.tensor(layout.token_div, dtype=torch.float32), persistent=False)
        layer = nn.TransformerEncoderLayer(
            d_model=d, nhead=cfg["heads"], dim_feedforward=d * cfg["mlp_ratio"], dropout=cfg["dropout"],
            activation="gelu", batch_first=True, norm_first=True,
        )
        self.encoder = nn.TransformerEncoder(layer, num_layers=cfg["layers"], enable_nested_tensor=False)
        self.norm = nn.LayerNorm(d)
        self.cand_cat = nn.ModuleList(nn.Embedding(v, d, padding_idx=0) for v in layout.cand_vocab)
        self.cand_num = nn.Linear(len(layout.cand_div), d)
        self.register_buffer("cand_div", torch.tensor(layout.cand_div, dtype=torch.float32), persistent=False)
        self.scorer = nn.Sequential(
            nn.Linear(6 * d, 2 * d), nn.GELU(), nn.Linear(2 * d, d), nn.GELU(), nn.Linear(d, 1)
        )
        self.wdl_head = nn.Sequential(nn.Linear(2 * d, d), nn.GELU(), nn.Linear(d, 3))
        self.aux_head = nn.Sequential(nn.Linear(2 * d, d), nn.GELU(), nn.Linear(d, 1))

    def encode(self, tok_cat, tok_num):
        x = self.tok_num(tok_num.to(torch.float32) / self.tok_div)
        for i, emb in enumerate(self.tok_cat):
            x = x + emb(tok_cat[..., i])
        return self.norm(self.encoder(x))

    def forward(self, tok_cat, tok_num, cand_cat, cand_num, cand_mask):
        lay = self.layout
        h = self.encode(tok_cat, tok_num)
        d = h.shape[-1]
        a = cand_cat.shape[1]
        g = h[:, lay.global_position, :]
        m = h.mean(dim=1)
        pooled = torch.cat([g, m], dim=-1)
        c = self.cand_num(cand_num.to(torch.float32) / self.cand_div)
        for i, emb in enumerate(self.cand_cat):
            c = c + emb(cand_cat[..., i])
        src = (cand_cat[..., lay.i_source] - 1).clamp(min=0)
        dst = (cand_cat[..., lay.i_destination] - 1).clamp(min=0)
        brd = (cand_cat[..., lay.i_dest_board] + lay.board_first - 1).clamp(min=lay.board_first, max=lay.board_last)

        def gather(ix):
            return torch.gather(h, 1, ix.unsqueeze(-1).expand(-1, -1, d))

        feats = torch.cat(
            [gather(src), gather(dst), gather(brd), g.unsqueeze(1).expand(-1, a, -1),
             m.unsqueeze(1).expand(-1, a, -1), c],
            dim=-1,
        )
        logits = self.scorer(feats).squeeze(-1)
        logits = logits.masked_fill(~cand_mask, float("-inf"))
        wdl = self.wdl_head(pooled)
        aux = torch.tanh(self.aux_head(pooled).squeeze(-1))
        return logits, wdl, aux


def parameter_report(model):
    groups = {}
    total = 0
    for name, p in model.named_parameters():
        n = int(p.numel())
        total += n
        key = name.split(".")[0]
        groups[key] = groups.get(key, 0) + n
    return total, groups
