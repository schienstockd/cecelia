"""One computed 2D plane → PNG bytes, for showing it in the browser.

Why this exists on the PYTHON side. The canonical server-side renderer is `api/src/image_render.jl`,
and it is the right thing for anything that lives in an OME-ZARR: it reads the store, composites
channels additively with per-channel colour and contrast, and returns a PNG. Computed planes — flow
metric maps, a probability head's output — are never in a store, so reaching that renderer would mean
getting arrays into Julia, and `app/src/preview.jl` is explicit that Julia passes those payloads
opaquely between two Python processes and never decodes them ("one implementation, both Python
ends"). Writing a Julia decoder to reuse the compositor would duplicate a codec across languages —
where a dtype mismatch reinterprets bytes rather than raising — to reuse, in the end, a single
clamp-to-contrast expression, because the compositor's actual work is a MULTICHANNEL additive blend
and these planes are single-channel.

So: render where the data already is. This is a new producer feeding the same consumer pattern the
crop panel uses (fetch a PNG, wrap it in a blob), not a second compositor.
"""

import io

import numpy as np


def stretch_to_uint8(plane, percentiles=(1.0, 99.5)):
    """Percentile contrast stretch → uint8, ignoring non-finite values.

    Percentiles rather than min/max because a single hot pixel — routine in a derived map, where a
    division can spike — would otherwise flatten the entire image to black.
    """
    arr = np.asarray(plane, dtype=np.float32)
    finite = arr[np.isfinite(arr)]
    if finite.size == 0:
        return np.zeros(arr.shape, np.uint8)
    lo, hi = np.percentile(finite, percentiles)
    if not np.isfinite(lo) or not np.isfinite(hi) or hi <= lo:
        lo, hi = float(finite.min()), float(finite.max())
    if hi <= lo:
        # A genuinely constant plane is information — "this metric is flat here" — so render it as
        # flat mid-grey rather than as an arbitrary black or white that reads like a bug.
        return np.full(arr.shape, 128, np.uint8)
    out = np.clip((np.nan_to_num(arr, nan=lo) - lo) / (hi - lo), 0.0, 1.0)
    return (out * 255).astype(np.uint8)


def plane_png(plane, percentiles=(1.0, 99.5)):
    """A 2D array → PNG bytes, contrast-stretched to 8 bits."""
    import imageio.v3 as iio          # local: only this path needs it, keeps the import cost off others

    img = stretch_to_uint8(plane, percentiles)
    if img.ndim != 2:
        raise ValueError(f'plane_png expects a 2D plane, got shape {img.shape}')
    buf = io.BytesIO()
    iio.imwrite(buf, img, extension='.png')
    return buf.getvalue()
