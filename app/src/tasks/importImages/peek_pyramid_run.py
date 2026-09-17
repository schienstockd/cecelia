"""
Peek the XY (+ ZTC) extent of one or more source images WITHOUT importing them, so the import
wizard can pre-fill the `pyramidLevels` field with a level count that ends at ~one tile.

Called by the Julia `/api/import/peek-pyramid` route. Reads metadata only — no pixels — through
three format-native readers already in the pixi env: `tifffile` (.tif / .ome.tif), `readlif`
(.lif), `h5py` (.ims — Imaris HDF5). Bioformats-only formats (LSM, ND2, CZI, LIF-variants, OIB…)
fall through as ``reader: "unsupported"`` — the wizard shows no recommendation for those (the
import default of 2 still applies). A JVM-backed fallback via ``showinf`` can be bolted on later;
skipped here so the peek stays instantaneous for the common case.

Recommendation formula: ``max(1, ceil(log2(max(nX, nY) / TARGET)) + 1)``. TARGET depends on
whether the image is a timelapse:

  * **Any playback** (T>1): TARGET=256 — the deepest level should be small enough to fetch one
    frame in a playback tick. Measured against VJy1Nx (1057×1111×31×181): 3 levels leave the
    deepest at 278×265×31 (73 MB/frame at 4×u8 — too much), 4 levels give ~139×132×31 (~18 MB)
    which plays cleanly; 5 was over-eager.
  * **Static** (T=1): TARGET=1024 — matches bioformats2raw's XY chunk. The deepest level fits
    one chunk; further downsampling costs storage without changing anyone's zoom-out speed.

The rule agrees with ``qc.jl::pyramid_layout`` at TARGET=1024 (its "one chunk on the long side"
rule); the playback tier is pre-import knowledge that QC can't have (QC looks at the store, which
by then is already whatever it is). Static 3D (Z>1, T=1) intentionally shares the static-2D
target — a Z-stack browser fetches on user gesture, not per frame — and can be tuned separately
if a report shows navigation is sluggish. The `chunk` override in the params contract still
applies — pass it to force a target regardless of the shape.

Parameter contract (JSON written by Julia):
  paths      - list of absolute source file paths
  resultPath - where to write the result JSON
  chunk      - optional; when absent, the shape-based TARGET above is used. Force with an int.

Result JSON:
  {"results": [
     {"path": "...", "reader": "tifffile"|"readlif"|"h5py"|"unsupported"|"error",
      "nX": …, "nY": …, "nZ": …, "nT": …, "nC": …,
      "recommendedPyramidLevels": N, "error": "..." (optional)}, ...]}
"""
import math
import os

import cecelia.utils.script_utils as script_utils
from cecelia.utils.atomic_io import write_json_atomic


DEFAULT_CHUNK = 1024
PLAYBACK_TARGET = 256


def target_for_shape(nz, nt):
    """Per-shape XY target for the deepest level (see module docstring for the tiering rule).
    Two tiers: playback (T>1) uses a smaller target so each frame is cheap to fetch;
    everything else uses bf2raw's XY chunk."""
    return PLAYBACK_TARGET if int(nt or 1) > 1 else DEFAULT_CHUNK


def recommend_levels(nx, ny, chunk=DEFAULT_CHUNK):
    """The smallest N such that the deepest level fits in `chunk` px on the long side.
    Mirrors ``qc.jl::pyramid_layout`` at chunk=1024; smaller targets are the pre-import extra
    depth for 3D/movie playback that a post-import QC can't decide."""
    long_side = max(int(nx or 0), int(ny or 0))
    chunk = max(1, int(chunk))
    if long_side <= chunk:
        return 1
    return int(math.ceil(math.log2(long_side / chunk))) + 1


def _peek_tiff(path):
    """OME-TIFF or plain TIFF — delegates to `ome_xml_utils.peek_tiff_shape` (the sanctioned
    tifffile call site; see `test_zarr_access_convention`)."""
    from cecelia.utils.ome_xml_utils import peek_tiff_shape
    return peek_tiff_shape(path)


def _peek_lif(path):
    """LIF via readlif — mirrors probe_series_run.py's `_probe_lif` (first series only, which
    is the shape a single-series import would consume; multi-series files hit the series probe
    first and register per-series, so peek runs per registered series if we ever want that)."""
    from readlif.reader import LifFile
    lf = LifFile(path)
    im = next(iter(lf.get_iter_image()))
    dims = im.dims
    return (int(getattr(dims, 'x', 1) or 1),
            int(getattr(dims, 'y', 1) or 1),
            int(getattr(dims, 'z', 1) or 1),
            int(getattr(dims, 't', 1) or 1),
            int(getattr(im, 'channels', 1) or 1))


def _peek_ims(path):
    """Imaris HDF5 (.ims). The pixel data lives under
    ``DataSet/ResolutionLevel 0/TimePoint 0/Channel 0/Data`` (Z, Y, X), with X/Y/Z listed in
    ``DataSetInfo/Image``. T count = number of TimePoint groups; C count = number of Channel
    groups under any TimePoint. Reading strings out of attrs is version-sensitive; ints are more
    reliable, so use the Data array shape for XY where present."""
    import h5py
    with h5py.File(path, 'r') as f:
        # Prefer the on-disk shape (authoritative). Anything missing → 1.
        info = f.get('DataSetInfo/Image', None)
        nx = ny = nz = 1
        if info is not None:
            def _a(k):
                v = info.attrs.get(k)
                if v is None: return None
                s = v.tobytes().decode('ascii', errors='ignore') if hasattr(v, 'tobytes') else str(v)
                try: return int(''.join(ch for ch in s if ch.isdigit()))
                except ValueError: return None
            nx = _a('X') or nx
            ny = _a('Y') or ny
            nz = _a('Z') or nz
        ds = f.get('DataSet', None)
        nt = 1
        nc = 1
        if ds is not None:
            lvl0 = ds.get('ResolutionLevel 0', None)
            if lvl0 is not None:
                tps = [k for k in lvl0.keys() if k.startswith('TimePoint')]
                nt = max(1, len(tps))
                if tps:
                    tp = lvl0[tps[0]]
                    chs = [k for k in tp.keys() if k.startswith('Channel')]
                    nc = max(1, len(chs))
                    # If DataSetInfo was silent, fall back to the array shape (Z, Y, X).
                    if chs and (nx == 1 or ny == 1):
                        arr = tp[chs[0]].get('Data', None)
                        if arr is not None and len(arr.shape) >= 2:
                            nz = arr.shape[0] if len(arr.shape) >= 3 else nz
                            ny = arr.shape[-2]
                            nx = arr.shape[-1]
        return nx, ny, nz, nt, nc


_READERS_BY_SUFFIX = (
    ('.ims', 'h5py', _peek_ims),
    ('.lif', 'readlif', _peek_lif),
    ('.ome.tif', 'tifffile', _peek_tiff),
    ('.ome.tiff', 'tifffile', _peek_tiff),
    ('.tif', 'tifffile', _peek_tiff),
    ('.tiff', 'tifffile', _peek_tiff),
)


def _pick_reader(path):
    low = path.lower()
    for suffix, name, fn in _READERS_BY_SUFFIX:
        if low.endswith(suffix):
            return name, fn
    return None, None


def peek_one(path, chunk=None):
    """`chunk` = None ⇒ derive the target from the shape (see `target_for_shape`); an int
    forces that target regardless of shape (the caller's override)."""
    name, fn = _pick_reader(path)
    if fn is None:
        return {'path': path, 'reader': 'unsupported'}
    try:
        nx, ny, nz, nt, nc = fn(path)
    except Exception as e:
        return {'path': path, 'reader': 'error', 'error': f'{type(e).__name__}: {e}'}
    effective_chunk = int(chunk) if chunk is not None else target_for_shape(nz, nt)
    return {
        'path': path, 'reader': name,
        'nX': int(nx), 'nY': int(ny), 'nZ': int(nz), 'nT': int(nt), 'nC': int(nc),
        'recommendedPyramidLevels': recommend_levels(nx, ny, effective_chunk),
        'targetChunk': effective_chunk,
    }


def run(params):
    log = script_utils.get_logfile_utils(params)
    paths = list(params.get('paths', []))
    # `chunk` is a caller override; when absent, the shape-based target picks it per image.
    chunk = params.get('chunk')
    if chunk is not None:
        try: chunk = int(chunk)
        except (ValueError, TypeError): chunk = None
    result_path = params['resultPath']

    results = []
    for i, p in enumerate(paths):
        if not isinstance(p, str) or not os.path.isfile(p):
            results.append({'path': str(p), 'reader': 'error',
                            'error': 'file not found'})
            log.log(f'[{i+1}/{len(paths)}] not a file: {p}')
            continue
        r = peek_one(p, chunk)
        results.append(r)
        summary = f"{r['reader']}"
        if 'recommendedPyramidLevels' in r:
            summary += f" nX={r['nX']} nY={r['nY']} nZ={r['nZ']} nT={r['nT']} nC={r['nC']} → {r['recommendedPyramidLevels']} levels"
        elif 'error' in r:
            summary += f' error={r["error"]}'
        log.log(f'[{i+1}/{len(paths)}] {os.path.basename(p)}: {summary}')

    write_json_atomic(result_path, {'results': results})


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
