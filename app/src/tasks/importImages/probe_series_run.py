"""Enumerate the series of a multi-series microscopy file and render a small preview per series.

Called by the Julia `/api/import/series/probe` route BEFORE any bioformats2raw conversion, so the
import wizard can show an ImageJ-style series picker for a `.lif` (Leica) file. Bioformats2raw + the
Bio-Formats CLI both spin a JVM to answer this, which turns a "which of these 4 series?" into a
20-30 s wait — long enough that the wizard would feel broken. `readlif` reads LIF natively (pure
Python) and returns per-image dims + pixel access without the JVM. Non-LIF multi-series formats
(CZI/ND2/OIR/IMS) fall through to a `format: "unsupported"` result — the wizard will list series
without a preview when a light reader for that format lands.

Params: `imPath` (the source file), `resultPath` (output JSON), `maxPx` (thumbnail long-side, 128).
Output JSON shape (both success and unsupported):
  {"format": "lif"|"unsupported", "path": "...", "series": [
      {"index": 0, "name": "...", "sizeX": …, "sizeY": …, "sizeZ": …,
       "sizeT": …, "sizeC": …, "thumbnailPngB64": "iVBOR…"}, …]}
`thumbnailPngB64` is absent when we couldn't render a preview for that series (still returns dims).
"""
import base64
import io
import os

import cecelia.utils.script_utils as script_utils
from cecelia.utils.atomic_io import write_json_atomic


def _thumbnail_png_b64(arr, max_px):
    """2-D uint array → base64 PNG of size ≤ max_px on the long side, contrast-stretched to 0-255.
    `arr` is a numpy 2-D image (single plane, one channel). Returns None on failure."""
    try:
        import numpy as np
        from PIL import Image
        a = np.asarray(arr)
        if a.ndim != 2 or a.size == 0:
            return None
        lo = float(np.percentile(a, 1.0))
        hi = float(np.percentile(a, 99.5))
        if hi <= lo:
            hi = lo + 1.0
        norm = np.clip((a.astype('float32') - lo) / (hi - lo), 0.0, 1.0)
        img = Image.fromarray((norm * 255).astype('uint8'), mode='L')
        h, w = img.size[1], img.size[0]
        long_side = max(h, w)
        if long_side > max_px:
            scale = max_px / long_side
            img = img.resize((max(1, int(w * scale)), max(1, int(h * scale))), Image.BILINEAR)
        buf = io.BytesIO()
        img.save(buf, format='PNG', optimize=True)
        return base64.b64encode(buf.getvalue()).decode('ascii')
    except Exception:
        return None


def _probe_lif(path, max_px, log):
    from readlif.reader import LifFile
    lf = LifFile(path)
    entries = []
    # readlif exposes each series as a LifImage via get_iter_image() / get_image(n).
    for idx, im in enumerate(lf.get_iter_image()):
        dims = im.dims  # (x, y, z, t)
        sx = int(getattr(dims, 'x', 0) or 0)
        sy = int(getattr(dims, 'y', 0) or 0)
        sz = int(getattr(dims, 'z', 1) or 1)
        st = int(getattr(dims, 't', 1) or 1)
        sc = int(getattr(im, 'channels', 1) or 1)
        entry = {
            'index': idx,
            'name': str(getattr(im, 'name', f'Series {idx}')),
            'sizeX': sx, 'sizeY': sy, 'sizeZ': sz, 'sizeT': st, 'sizeC': sc,
        }
        # Middle Z, t=0, channel 0 — a reader can only ask for one plane at a time; a max-projection
        # across Z is what the user would find useful but reading every Z of every series would defeat
        # the "instant preview" the picker exists to give.
        try:
            z_mid = sz // 2
            frame = im.get_frame(z=z_mid, t=0, c=0)  # PIL image
            import numpy as np
            b64 = _thumbnail_png_b64(np.array(frame), max_px)
            if b64:
                entry['thumbnailPngB64'] = b64
        except Exception as e:
            log.log(f'[WARN] thumbnail failed for series {idx}: {e}')
        entries.append(entry)
    return {'format': 'lif', 'path': path, 'series': entries}


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = script_utils.get_param(params, 'imPath', default=None)
    result_path = script_utils.get_param(params, 'resultPath', default=None)
    max_px      = int(script_utils.get_param(params, 'maxPx', default=128))
    if not im_path or not result_path:
        log.log('[ERROR] imPath and resultPath are required')
        return

    ext = os.path.splitext(im_path)[1].lower()
    result = None
    if ext == '.lif':
        try:
            result = _probe_lif(im_path, max_px, log)
        except Exception as e:
            log.log(f'[ERROR] LIF probe failed: {e}')
    if result is None:
        result = {'format': 'unsupported', 'path': im_path, 'series': []}

    log.log(f">> {result['format']}: {len(result['series'])} series")
    write_json_atomic(result_path, result)


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
