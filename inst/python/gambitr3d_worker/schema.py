"""Feature-schema document shared with R.

The schema is read from the package's ``schemas/features-v1.json``. Its hash
is SHA-256 of the canonical JSON text (keys sorted, no whitespace), which is
exactly how R's ``.g3_canonical_json()`` writes it, so both sides compute the
same digest independently.
"""

import hashlib
import json
import os

HERE = os.path.dirname(os.path.abspath(__file__))


def canonical_json(obj):
    return json.dumps(obj, sort_keys=True, separators=(",", ":"), ensure_ascii=False, allow_nan=False)


def sha256_text(text):
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def schema_path():
    # Source tree: inst/python/gambitr3d_worker -> inst/schemas.
    # Installed package: <lib>/gambitR3D/python/gambitr3d_worker -> <lib>/gambitR3D/schemas.
    return os.path.join(os.path.dirname(os.path.dirname(HERE)), "schemas", "features-v1.json")


def load_schema(path=None):
    path = path or schema_path()
    with open(path, "r", encoding="utf-8") as fh:
        doc = json.load(fh)
    return doc, sha256_text(canonical_json(doc))


class Layout:
    """Slot counts, vocabularies and divisors derived from the schema."""

    def __init__(self, doc):
        self.doc = doc
        self.name = doc["schema"]
        self.n_tokens = int(doc["tokens"]["count"])
        self.token_vocab = [int(s["vocab"]) for s in doc["token_categorical"]]
        self.token_div = [int(s["divisor"]) for s in doc["token_numeric"]]
        self.cand_vocab = [int(s["vocab"]) for s in doc["candidate_categorical"]]
        self.cand_div = [int(s["divisor"]) for s in doc["candidate_numeric"]]
        names = [s["name"] for s in doc["candidate_categorical"]]
        self.i_source = names.index("source")
        self.i_destination = names.index("destination")
        self.i_dest_board = names.index("destination_board")
        self.global_position = int(doc["tokens"]["global_position"])
        self.board_first = int(doc["tokens"]["board_positions"][0])
        self.board_last = int(doc["tokens"]["board_positions"][1])
