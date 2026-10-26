"""Tensor digests and safe, atomic artifact files.

Only safetensors (a JSON header plus raw little-endian bytes) is read or
written; no pickle-based loader is ever called. A weights digest is SHA-256
over the tensors in sorted-name order, each framed as
``name NUL dtype NUL comma-separated shape NUL raw bytes``; R recomputes it
from a safetensors file with its own reader.
"""

import hashlib
import os

import numpy as np
import torch
from safetensors.torch import load as st_load
from safetensors.torch import save as st_save

DTYPE_NAMES = {
    torch.float32: "F32",
    torch.float64: "F64",
    torch.int32: "I32",
    torch.int64: "I64",
    torch.uint8: "U8",
    torch.bool: "BOOL",
}


def tensor_digest(tensors):
    h = hashlib.sha256()
    for name in sorted(tensors):
        t = tensors[name].detach().cpu().contiguous()
        arr = t.numpy()
        if arr.dtype.byteorder == ">":
            arr = arr.astype(arr.dtype.newbyteorder("<"))
        h.update(name.encode("utf-8"))
        h.update(b"\0")
        h.update(DTYPE_NAMES[t.dtype].encode("ascii"))
        h.update(b"\0")
        h.update(",".join(str(int(s)) for s in t.shape).encode("ascii"))
        h.update(b"\0")
        h.update(arr.tobytes(order="C"))
    return h.hexdigest()


def sha256_bytes(data):
    return hashlib.sha256(data).hexdigest()


def sha256_file(path):
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def write_bytes_synced(path, data):
    """Write bytes and flush them to stable storage before returning."""
    with open(path, "wb") as fh:
        fh.write(data)
        fh.flush()
        os.fsync(fh.fileno())


def fsync_dir(path):
    try:
        fd = os.open(path, os.O_RDONLY)
    except OSError:
        return
    try:
        os.fsync(fd)
    except OSError:
        pass
    finally:
        os.close(fd)


def safetensors_bytes(tensors, metadata=None):
    clean = {k: v.detach().cpu().contiguous() for k, v in tensors.items()}
    return st_save(clean, metadata=metadata)


def read_verified(path, expected_sha256=None):
    """Read a file, check its SHA-256 and return (bytes, digest)."""
    with open(path, "rb") as fh:
        data = fh.read()
    digest = sha256_bytes(data)
    if expected_sha256 is not None and digest != expected_sha256:
        raise ValueError("SHA-256 mismatch for %s" % os.path.basename(path))
    return data, digest


def load_safetensors_bytes(data):
    return st_load(data)


def load_numpy_safetensors(path, expected_sha256=None):
    data, digest = read_verified(path, expected_sha256)
    tensors = st_load(data)
    return {k: v.numpy() for k, v in tensors.items()}, digest


def as_int_array(x, shape_tail, what):
    """Convert nested lists to an int64 array, refusing floats and booleans."""
    arr = np.asarray(x)
    if arr.size and arr.dtype.kind not in "iu":
        raise ValueError("%s must contain integers only" % what)
    arr = arr.astype(np.int64, copy=False)
    if arr.ndim != len(shape_tail) or any(t is not None and arr.shape[i] != t for i, t in enumerate(shape_tail)):
        raise ValueError("%s has shape %s, expected %s" % (what, arr.shape, shape_tail))
    return arr
