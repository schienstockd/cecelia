"""Atomic writes for durable outputs — THE Python way to write a file that must survive a kill.

The Python counterpart of Julia's `write_atomic`/`write_json_atomic` (`app/src/utils.jl`), and it
exists for the same reason. Writing straight to the destination TRUNCATES it before the new bytes
land, so anything that ends the process inside that window leaves a half-written file:

- **`task:cancel` kills the Python process by design.** `run_py` registers it for cancellation, so a
  user cancelling a clustering/tracking run while it saves obs is a routine action, not an edge case.
- the Quit button SIGKILLs the whole tree; and crashes/power loss exist.

For `.h5ad` that was the worst case in the codebase. The cell table *is* the measurement data (per-cell
morphology + intensity for up to ~100k cells), a truncated HDF5 is not partially readable the way a
truncated JSON is, and nothing of the previous content survives. `#420` fixed exactly this class on the
Julia side (`ccid.json`) and stopped at the language boundary; the canonical Python cell-data writer
`LabelPropsView.save()` was still rewriting the table in place.

**Temp names carry the extension suffix, never a real one** — `base.h5ad.tmp.a1b2c3d4`, not
`base.tmp.h5ad`. Discovery in several places is a directory listing filtered by extension
(`img_spatial_graph_suffixes` in `app/src/model/image.jl` does `endswith(f, ".h5ad")`;
`convert_centroid_names.py` globs `labelProps/*.h5ad`; `/api/movies` lists `*.mp4`), so a leftover from
a killed process must not match. Getting this backwards would register a half-written file as a real
segmentation or movie.

**Files here; STORES elsewhere.** The multiscales-store equivalent is `zarr_utils.staged_store` /
`promote_store` (`#424`). They are deliberately NOT one helper, because replacing a *directory* is not
replacing a *file*: `os.replace` won't put a dir over an existing dir, so the store path renames the old
one aside first and deletes it only once the new one is in place. Their temp naming differs for a reason
too — a store uses a FIXED `.partial` suffix so debris is sweepable by a maintenance patch, while a file
uses a UNIQUE `.tmp.<uid>` so two concurrent writers of one path can't collide. Use `staged_store` for a
store, this module for a file; don't merge them.

**Scope, deliberately:** this defends against the process dying, which is the trigger we have. It is
not an fsync — a power loss can still lose a rename the OS hasn't flushed. It also costs a temp copy,
so **peak** disk for that one file doubles for the duration of the write, dropping back on the rename
(labelProps tables are KB–MB, so it's noise; it is also a net win when space is tight, since a failed
staged write leaves the original intact where an in-place write would leave it truncated).
"""

import contextlib
import json
import os
import uuid

__all__ = ["atomic_path", "write_atomic", "write_json_atomic", "write_h5ad_atomic"]


def _tmp_path(path):
    """Sibling temp path — same directory (so the replace is atomic, not a copy across filesystems),
    unique (so two writers of one path never clobber each other's temp), and suffixed AFTER the real
    extension so a discovery glob can't pick it up."""
    return "{}.tmp.{}".format(path, uuid.uuid4().hex[:8])


@contextlib.contextmanager
def atomic_path(path):
    """Yield a temp path to write to; on clean exit, atomically move it onto `path`.

    Use this when the writer wants a *filename* rather than a file object (``adata.write_h5ad``,
    an imageio writer, a zarr store). ``os.replace`` — not ``os.rename`` — because only the former
    overwrites an existing destination atomically on Windows.

    On any exception the temp file is removed and `path` is left exactly as it was.

        with atomic_path(out) as tmp:
            adata.write_h5ad(tmp)
    """
    path = str(path)
    parent = os.path.dirname(path)
    if parent:
        os.makedirs(parent, exist_ok=True)
    tmp = _tmp_path(path)
    try:
        yield tmp
        os.replace(tmp, path)
    except BaseException:
        try:
            os.remove(tmp)
        except OSError:
            pass          # writer may have died before creating it
        raise


@contextlib.contextmanager
def write_atomic(path, mode="w", **open_kwargs):
    """`open()` for durable output: writes to a sibling temp, then atomically replaces `path`.

        with write_atomic(qc_path) as f:
            json.dump(payload, f)
    """
    with atomic_path(path) as tmp:
        with open(tmp, mode, **open_kwargs) as f:
            yield f


def write_json_atomic(path, payload, **dump_kwargs):
    """`json.dump` to `path`, atomically. Returns `path`."""
    with write_atomic(path) as f:
        json.dump(payload, f, **dump_kwargs)
    return path


def write_h5ad_atomic(adata, path, **write_kwargs):
    """`adata.write_h5ad(path)`, atomically. Returns `path`.

    THE way to write an `.h5ad` — creating one or rewriting one. Both matter: rewriting in place can
    destroy an existing table, and a half-written *new* file is worse than no file, because discovery
    is a directory listing (see the module docstring) and would present it as a real result.
    """
    with atomic_path(path) as tmp:
        adata.write_h5ad(tmp, **write_kwargs)
    return path
