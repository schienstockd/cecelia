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

import base64
import io

import numpy as np

# Perceptually-uniform colour maps as 256x3 uint8 lookup tables, base64 of the raw bytes.
#
# BAKED IN rather than imported. These are matplotlib's, but this module is part of the light IO tier
# (`python/pyproject.toml`) that an external consumer pip-installs — coastal does — and matplotlib is
# a plotting stack, not an IO dependency. 768 bytes each is a far smaller thing to carry than a
# dependency, and a LUT cannot drift the way a re-implementation of the colour science would.
#
# Regenerate with, and verified against, matplotlib:
#     lut = (np.asarray(matplotlib.colormaps[name](np.linspace(0, 1, 256)))[:, :3] * 255)
#     base64.b64encode(lut.round().astype(np.uint8).tobytes())
# `test_plane_render.py` pins the endpoints and the midpoint of each.

_VIRIDIS_B64 = (
    'RAFURAJWRQRXRQVZRgdaRghcRgpdRgteRw1gRw5hRxBjRxFkRxNlSBRnSBZoSBdpSBhqSBpsSBttSBxuSB1vSB9wSCBx'
    'SCFzSCN0SCR1SCV2SCZ3SCh4SCl5Ryp6Ryx6Ry17Ry58Ry99RjB+RjJ+RjN/RjSARTWBRTeBRTiCRDmDRDqDRDuEQz2E'
    'Qz6FQj+FQkCGQkGGQUKHQUSHQEWIQEaIP0eIP0iJPkmJPkqJPkyKPU2KPU6KPE+KPFCLO1GLO1KLOlOLOlSMOVWMOVaM'
    'OFiMOFmMN1qMN1uNNlyNNl2NNV6NNV+NNGCNNGGNM2KNM2ONMmSOMmWOMWaOMWeOMWiOMGmOMGqOL2uOL2yOLm2OLm6O'
    'Lm+OLXCOLXGOLHGOLHKOLHOOK3SOK3WOKnaOKneOKniOKXmOKXqOKXuOKHyOKH2OJ36OJ3+OJ4COJoGOJoKOJoKOJYOO'
    'JYSOJYWOJIaOJIeOI4iOI4mOI4qNIouNIoyNIo2NIY6NIY+NIZCNIZGMIJKMIJKMIJOMH5SMH5WLH5aLH5eLH5iLH5mK'
    'H5qKHpuKHpyJHp2JH56JH5+IH6CIH6GIH6GHH6KHIKOGIKSGIaWFIaaFIqeFIqiEI6mDJKqDJauCJayCJq2BJ62BKK6A'
    'Ka9/KrB/LLF+LbJ9LrN8L7R8MbV7MrZ6NLZ5Nbd5N7h4OLl3Orp2O7t1Pbx0P7xzQL1yQr5xRL9wRsBvSMFuSsFtTMJs'
    'TsNrUMRqUsVpVMVoVsZnWMdlWshkXMhjXsliYMpgY8tfZcteZ8xcac1bbM1abs5YcM9Xc9BWddBUd9FTetFRfNJQf9NO'
    'gdNNhNRLhtVJidVIi9ZGjtZFkNdDk9dBldhAmNg+m9k8ndk7oNo5oto3pds2qNs0qtwyrdwwsN0vst0ttd4ruN4put4o'
    'vd8mwN8lwt8jxeAhyOAgyuEfzeEd0OEc0uIb1eIa2OIZ2uMZ3eMY3+MY4uQY5eQZ5+QZ6uUa7OUb7+Uc8eUd9OYe9uYg'
    '+OYh++cj/ecl'
)

_MAGMA_B64 = (
    'AAAEAQAFAQEGAQEIAgEJAgILAgINAwMPAwMSBAQUBQQWBgUYBgUaBwYcCAceCQcgCggiCwkkDAkmDQopDgsrEAstEQwv'
    'Eg0xEw00FA42FQ44Fg87GA89GRA/GhBCHBBEHRFHHhFJIBFLIRFOIhFQJBJTJRJVJxJYKRFaKhFcLBFfLRFhLxFjMRFl'
    'MxBnNBBpNhBrOBBsOQ9uOw9wPQ9xPw9yQA90Qg91RA92RRB3RxB4SRB4ShB5TBF6ThF7TxJ7URJ8UhN8VBN9VhR9VxV+'
    'WRV+WhZ+XBZ/XRd/Xxh/YBiAYhmAZBqAZRqAZxuAaByBahyBax2BbR2Bbh6BcB+Bch+BcyCBdSGBdiGBeCKBeSKCeyOC'
    'fCOCfiSCgCWCgSWBgyaBhCaBhieBiCeBiSiBiymBjCmBjiqBkCqBkSuBkyuAlCyAliyAmC2AmS2Amy5/nC5/ni9/oC9/'
    'oTB+ozB+pTF+pjF9qDJ9qjN9qzN8rTR8rjR7sDV7sjV7szZ6tTZ6tzd5uDd5ujh4vDl4vTl3vzp3wDp2wjt1xDx1xTx0'
    'xz1zyD5zyj5yzD9xzUBxz0Bw0EFv0kJv00Nu1URt1kVs2EVs2UZr20dq3Ehp3klo30po4Exn4k1m405l5E9k5VBk51Jj'
    '6FNi6VRi6lZh61dg7Fhg7Vpf7lte711e8F9e8WBd8mJd8mRc82Vc9Gdc9Glc9Wtc9mxc9m5c93Bc93Jc+HRc+HZc+Xhd'
    '+Xld+Xtd+n1e+n9e+oFf+4Nf+4Vg+4dh/Ilh/Ipi/Ixj/I5k/JBl/ZJm/ZRn/ZZo/Zhp/Zpq/Ztr/p1s/p9t/qFu/qNv'
    '/qVx/qdy/qlz/qp0/qx2/q53/rB4/rJ6/rR7/rZ8/rd+/rl//ruB/r2C/r+E/sGF/sKH/sSI/saK/siM/sqN/syP/s2Q'
    '/s+S/tGU/tOV/tWX/teZ/tia/dqc/dye/d6g/eCh/eKj/eOl/eWn/eep/emq/eus/Oyu/O6w/PCy/PK0/PS2/Pa4/Pe5'
    '/Pm7/Pu9/P2/'
)

_LUTS = {'viridis': _VIRIDIS_B64, 'magma': _MAGMA_B64}
_LUT_CACHE = {}

#: The default. Greyscale is still offered because a SIGNED plane (divergence, vorticity) reads more
#: honestly without a colour ramp implying a magnitude ordering it does not have.
DEFAULT_COLORMAP = 'viridis'
COLORMAPS = ('viridis', 'magma', 'grey')


def colormap_lut(name):
    """`[256, 3]` uint8 RGB for a named map, or `None` for greyscale."""
    if name not in _LUTS:
        return None
    if name not in _LUT_CACHE:
        _LUT_CACHE[name] = np.frombuffer(
            base64.b64decode(_LUTS[name]), dtype=np.uint8).reshape(256, 3)
    return _LUT_CACHE[name]


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


def plane_png(plane, percentiles=(1.0, 99.5), colormap=DEFAULT_COLORMAP):
    """A 2D array → PNG bytes, contrast-stretched to 8 bits and colour-mapped.

    Colour-mapped by default: these planes are read by eye, side by side, to answer "which of these
    looks like cells", and grey hides the mid-range structure that question is about. The LUT is
    applied AFTER the stretch, so the colours mean the same thing in every panel of the sheet.
    """
    import imageio.v3 as iio          # local: only this path needs it, keeps the import cost off others

    img = stretch_to_uint8(plane, percentiles)
    if img.ndim != 2:
        raise ValueError(f'plane_png expects a 2D plane, got shape {img.shape}')
    lut = colormap_lut(colormap)
    if lut is not None:
        img = lut[img]                # [Y, X] indices → [Y, X, 3] RGB
    buf = io.BytesIO()
    iio.imwrite(buf, img, extension='.png')
    return buf.getvalue()
