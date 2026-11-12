"""
Registering staining cycles that are whole-slide scans: one cycle per scan, each a gigapixel plane per
channel, each scan covering a slightly different area of the slide.

`editImages.register`'s original path (sitkibex, `register_run.py`) hands SimpleITK the full-resolution
plane of the shared channel and resamples every moving channel whole: on a 1.5 Gpx scan that is
several GB per plane in float copies, the same failure the import probe had. This module does the
same job on the image's pyramid instead:

1. **Estimate** (`estimate_affine`) — a translation from phase correlation on a coarse level (the
   scans can be offset by thousands of pixels, far outside what a gradient optimiser recovers), then
   SimpleITK (correlation metric, regular-step gradient descent): rigid on the coarse level, affine
   on the finest level that fits `REGISTER_FINE_MAX_PX`, rotation about the plane's centre, sampled inside
   the reference's tissue (`tissue_utils`) so the glass around the sections does not vote.
   Coordinates are level-0 pixels throughout: a level-L pixel i sits at ``i·f + (f − 1)/2``.
2. **Apply** (`warp_plane`) — the moving channel resampled onto the reference's level-0 grid one
   `WARP_TILE_PX` tile at a time, reading only the source window each tile maps to; linear
   interpolation, zero outside the moving scan.
3. **Check** (`cycle_checks`) — per cycle, against the reference, on its tissue: the residual offset
   left after registration (phase correlation of full-resolution patches), how much of the
   reference's tissue the cycle covers and still shows as tissue (tissue lost between cycles), and
   the shared channel's signal and sharpness relative to the reference (stain loss, focus).

Phase correlation: Guizar-Sicairos M, Thurman ST, Fienup JR (2008) *Efficient subpixel image
registration algorithms*, Opt Lett 33(2):156-158, doi:10.1364/OL.33.000156 (`skimage.registration.
phase_cross_correlation`). The cycle-to-cycle registration design follows IBEX: Radtke AJ et al.
(2020) *IBEX: A versatile multiplex optical imaging approach for deep phenotyping and spatial analysis
of cells in complex tissues*, PNAS 117(52):33455-33465, doi:10.1073/pnas.2018488117 (sitk-ibex,
https://github.com/niaid/sitk-ibex — vendored as ``python/sitkibex``).
"""
import numpy as np

import cecelia.utils.tissue_utils as tissue_utils

#: A reference plane larger than this (pixels) is registered on the pyramid (this module); smaller
#: ones keep the whole-plane sitkibex path. Same bound the import probe reads whole planes up to.
REGISTER_WHOLE_PLANE_PX = 4096 * 4096
#: The affine is refined on the finest pyramid level whose plane is at most this many pixels: on a
#: 0.5 µm scan that is the 1/8 level (4 µm pixels), which left about one level-0 pixel of residual
#: on the PhenoCycler cycles it was measured on.
REGISTER_FINE_MAX_PX = 32 * 1024 * 1024
#: Metric samples per optimiser level (SimpleITK random sampling).
REGISTER_METRIC_SAMPLES = 60000
#: Output tile (level-0 pixels per side) resampled at once — a multiple of the 512 store chunk.
WARP_TILE_PX = 4096
#: Full-resolution patches the residual check correlates, and their side.
CHECK_PATCHES = 12
CHECK_PATCH_PX = 512
#: Residual (µm) past which a cycle is called misaligned: half a nucleus. Past it, one cell's pixels in
#: a moving cycle's markers come from its neighbour in the reference.
RESIDUAL_WARN_UM = 3.0


def _sitk():
    import SimpleITK as sitk            # lazy: see register_run.py's top-of-file note
    return sitk


def plane_index(lead, c_pos, c, ys=slice(None), xs=slice(None)):
    """Index tuple into a ``[..., y, x]`` level: ``lead`` the leading axes' indices (t, z … = 0),
    the channel axis at ``c_pos`` set to ``c``."""
    sl = list(lead)
    if c_pos is not None:
        sl[c_pos] = int(c)
    return tuple(sl) + (ys, xs)


def _image(plane, factor):
    sitk = _sitk()
    fy, fx = factor
    im = sitk.GetImageFromArray(np.log1p(np.asarray(plane, dtype=np.float32)))
    im.SetSpacing((float(fx), float(fy)))
    im.SetOrigin(((fx - 1) / 2.0, (fy - 1) / 2.0))
    return im


def _upsample_mask(mask, shape):
    """Nearest-neighbour resize of a coarse mask to ``shape``."""
    ry = (np.arange(shape[0]) * mask.shape[0] // shape[0]).clip(0, mask.shape[0] - 1)
    rx = (np.arange(shape[1]) * mask.shape[1] // shape[1]).clip(0, mask.shape[1] - 1)
    return mask[np.ix_(ry, rx)]


def _initial_shift(ref, mov):
    """Fixed→moving translation (x, y) in pixels of these planes, from phase correlation of the two
    zero-padded to a common shape (both start at level-0 pixel 0)."""
    from skimage.registration import phase_cross_correlation
    h, w = max(ref.shape[0], mov.shape[0]), max(ref.shape[1], mov.shape[1])
    a = np.zeros((h, w), np.float32); b = np.zeros((h, w), np.float32)
    a[:ref.shape[0], :ref.shape[1]] = np.log1p(ref.astype(np.float32))
    b[:mov.shape[0], :mov.shape[1]] = np.log1p(mov.astype(np.float32))
    shift, _, _ = phase_cross_correlation(a, b, upsample_factor=4)
    # ref(p) = mov(p - shift): a fixed point p sits at p - shift in the moving image
    return -float(shift[1]), -float(shift[0])


def _refine(fixed, moving, mask, tx, factor, shrink):
    sitk = _sitk()
    reg = sitk.ImageRegistrationMethod()
    reg.SetMetricAsCorrelation()
    if mask is not None and mask.any():
        m = sitk.GetImageFromArray(mask.astype(np.uint8))
        m.CopyInformation(fixed)
        reg.SetMetricFixedMask(m)
    reg.SetMetricSamplingStrategy(reg.RANDOM)
    reg.SetMetricSamplingPercentage(min(0.2, REGISTER_METRIC_SAMPLES / fixed.GetNumberOfPixels()), 1)
    step = float(max(factor))
    # the default gradient tolerance (1e-4) and a 1% minimum step stopped short on a slide's tiny
    # rotation (0.25° found for 0.4° in the tests): a correlation surface this flat needs both lower
    reg.SetOptimizerAsRegularStepGradientDescent(learningRate=2 * step, minStep=1e-3 * step,
                                                 numberOfIterations=500, relaxationFactor=0.7,
                                                 gradientMagnitudeTolerance=1e-10)
    reg.SetOptimizerScalesFromPhysicalShift()
    reg.SetShrinkFactorsPerLevel(list(shrink))
    reg.SetSmoothingSigmasPerLevel([float(s // 2) for s in shrink])
    reg.SetInitialTransform(tx, inPlace=True)
    reg.SetInterpolator(sitk.sitkLinear)
    reg.Execute(fixed, moving)
    return float(reg.GetMetricValue())


def estimate_affine(ref_levels, mov_levels, ref_c, mov_c, lead, c_pos, log=None):
    """The affine that maps a reference level-0 pixel (x, y) to the moving cycle's, estimated on the
    shared channel (``ref_c`` / ``mov_c``) over the pyramids (both level 0 first, same downsampling).
    Returns ``(transform, info)``: a SimpleITK ``AffineTransform`` (fixed → moving, level-0 pixels) and
    ``{initialShiftPx, metric, levels, tissueFraction}``."""
    sitk = _sitk()
    say = log or (lambda _m: None)
    shapes = [tuple(lv.shape) for lv in ref_levels]
    coarse = tissue_utils.coarse_level(shapes)
    fine = next((i for i, s in enumerate(shapes) if s[-2] * s[-1] <= REGISTER_FINE_MAX_PX), coarse)
    fine = min(fine, coarse)
    coarse = min(coarse, len(mov_levels) - 1)
    fine = min(fine, coarse)

    def planes(li):
        return (np.asarray(ref_levels[li][plane_index(lead, c_pos, ref_c)]),
                np.asarray(mov_levels[li][plane_index(lead, c_pos, mov_c)]))

    ref_c_plane, mov_c_plane = planes(coarse)
    factor = tissue_utils.level_factor(shapes[0], shapes[coarse])
    mov_factor = tissue_utils.level_factor(mov_levels[0].shape, mov_levels[coarse].shape)
    tissue = tissue_utils.tissue_mask(ref_c_plane)
    sx, sy = _initial_shift(ref_c_plane, mov_c_plane)
    # rotation about the plane's centre, not pixel (0, 0): about a corner, a small rotation moves the
    # far side of a slide by tens of pixels and the optimiser trades it against the translation
    h0, w0 = shapes[0][-2:]
    rigid = sitk.Euler2DTransform()
    rigid.SetCenter(((w0 - 1) / 2.0, (h0 - 1) / 2.0))
    rigid.SetTranslation((sx * factor[1], sy * factor[0]))
    say(f'   initial shift from phase correlation: {sx * factor[1]:.0f}, {sy * factor[0]:.0f} px')
    # rigid on the coarse level (a slide on a stage shifts and turns); affine only where pixels are
    # fine enough to tell a scale or shear from noise — the order sitkibex uses
    metric = _refine(_image(ref_c_plane, factor), _image(mov_c_plane, mov_factor), tissue, rigid, factor,
                     shrink=(2, 1))
    say(f'   rigid on level {coarse}: correlation {-metric:.3f}')
    tx = sitk.AffineTransform(2)
    tx.SetCenter(rigid.GetCenter())
    tx.SetMatrix(rigid.GetMatrix())
    tx.SetTranslation(rigid.GetTranslation())
    used = [coarse]
    if fine < coarse:
        ref_f, mov_f = planes(fine)
        f_factor = tissue_utils.level_factor(shapes[0], shapes[fine])
        mov_f_factor = tissue_utils.level_factor(mov_levels[0].shape, mov_levels[fine].shape)
        metric = _refine(_image(ref_f, f_factor), _image(mov_f, mov_f_factor),
                         _upsample_mask(tissue, ref_f.shape), tx, f_factor, shrink=(4, 2, 1))
        say(f'   affine on level {fine}: correlation {-metric:.3f}')
        used.append(fine)
    return tx, {'initialShiftPx': [round(sx * factor[1], 1), round(sy * factor[0], 1)],
                'metric': round(-metric, 4), 'levels': used,
                'tissueFraction': round(float(tissue.mean()), 4)}


def affine_matrix(tx):
    """3×3 homogeneous matrix of a 2-D SimpleITK affine (x, y order): ``moving = A @ (x, y, 1)``."""
    m = np.asarray(tx.GetMatrix(), dtype=float).reshape(2, 2)
    c = np.asarray(tx.GetCenter(), dtype=float)
    t = np.asarray(tx.GetTranslation(), dtype=float)
    a = np.eye(3)
    a[:2, :2] = m
    a[:2, 2] = c + t - m @ c
    return a


def describe(a, out_hw):
    """``{shiftPx, rotationDeg, scale}`` of matrix ``a``: the shift at the reference plane's centre
    (x, y, level-0 px), the rotation and the mean scale."""
    h, w = out_hw
    ctr = np.array([(w - 1) / 2.0, (h - 1) / 2.0, 1.0])
    shift = (a @ ctr)[:2] - ctr[:2]
    m = a[:2, :2]
    return {'shiftPx': [round(float(shift[0]), 2), round(float(shift[1]), 2)],
            'rotationDeg': round(float(np.degrees(np.arctan2(m[1, 0], m[0, 0]))), 4),
            'scale': round(float(np.sqrt(abs(np.linalg.det(m)))), 5)}


def _source_window(a, y0, y1, x0, x1, src_hw, pad=2):
    """Moving-image window (ys, xs) a reference tile [y0:y1, x0:x1] maps into, padded and clipped;
    None when it falls outside the moving plane."""
    corners = np.array([[x0, y0, 1], [x1, y0, 1], [x0, y1, 1], [x1, y1, 1]], dtype=float)
    mc = corners @ a.T
    lo = np.floor(mc[:, :2].min(axis=0)).astype(int) - pad
    hi = np.ceil(mc[:, :2].max(axis=0)).astype(int) + pad
    lo = np.maximum(lo, 0)
    hi = np.minimum(hi, [src_hw[1], src_hw[0]])
    if hi[0] <= lo[0] or hi[1] <= lo[1]:
        return None
    return slice(int(lo[1]), int(hi[1])), slice(int(lo[0]), int(hi[0]))


def warp_tile(read_src, src_hw, tx, a, y0, y1, x0, x1, dtype):
    """The moving plane resampled onto reference tile [y0:y1, x0:x1] (linear, 0 outside); None when
    the tile maps entirely outside the moving plane. ``read_src(ys, xs)`` reads the moving level 0."""
    sitk = _sitk()
    win = _source_window(a, y0, y1, x0, x1, src_hw)
    if win is None:
        return None
    ys, xs = win
    src = sitk.GetImageFromArray(np.asarray(read_src(ys, xs), dtype=np.float32))
    src.SetOrigin((float(xs.start), float(ys.start)))
    out = sitk.Resample(src, (int(x1 - x0), int(y1 - y0)), tx, sitk.sitkLinear,
                        (float(x0), float(y0)), (1.0, 1.0), (1.0, 0.0, 0.0, 1.0), 0.0, sitk.sitkFloat32)
    arr = sitk.GetArrayFromImage(out)
    info = np.iinfo(dtype) if np.issubdtype(dtype, np.integer) else None
    if info is not None:
        arr = np.clip(np.rint(arr), info.min, info.max)
    return arr.astype(dtype, copy=False)


def tiles(hw, tile=None):
    tile = tile or WARP_TILE_PX
    h, w = hw
    for y0 in range(0, h, tile):
        for x0 in range(0, w, tile):
            yield y0, min(h, y0 + tile), x0, min(w, x0 + tile)


def warp_plane(read_src, src_hw, tx, out_hw, write, dtype, tile=None):
    """Resample the moving plane onto the reference grid (``out_hw``) tile by tile; ``write(ys, xs,
    arr)`` stores each tile. Tiles that map outside the moving plane are not written (the store's
    fill value, 0, is what they read as). Returns the number of tiles written."""
    a = affine_matrix(tx)
    n = 0
    for y0, y1, x0, x1 in tiles(out_hw, tile):
        arr = warp_tile(read_src, src_hw, tx, a, y0, y1, x0, x1, dtype)
        if arr is None:
            continue
        write(slice(y0, y1), slice(x0, x1), arr)
        n += 1
    return n


def copy_plane(read_src, hw, write, tile=None):
    """Copy a plane tile by tile (the reference's channels into the output)."""
    for y0, y1, x0, x1 in tiles(hw, tile):
        write(slice(y0, y1), slice(x0, x1), np.asarray(read_src(slice(y0, y1), slice(x0, x1))))


def _sharpness(p):
    """Intensity-free focus measure: mean squared gradient over squared mean."""
    p = np.asarray(p, dtype=np.float64)
    mu = p.mean()
    if mu <= 0:
        return None
    gy, gx = np.gradient(p)
    return float((gx * gx + gy * gy).mean() / (mu * mu))


def _only_in_cycle(a, mov_t, mov_factor, ref_t, ref_factor, n_ref):
    """Moving-cycle tissue that maps onto reference pixels with no tissue, over ``n_ref`` reference
    tissue pixels (both at the coarse level). Moving pixels map back through the inverse affine."""
    yy, xx = np.nonzero(mov_t)
    if yy.size == 0 or n_ref == 0:
        return 0.0
    mfy, mfx = mov_factor
    fy, fx = ref_factor
    pts = np.stack([xx * mfx + (mfx - 1) / 2.0, yy * mfy + (mfy - 1) / 2.0, np.ones(yy.size)], axis=1)
    rp = pts @ np.linalg.inv(a).T
    ry = np.floor(rp[:, 1] / fy).astype(int)
    rx = np.floor(rp[:, 0] / fx).astype(int)
    inside = (ry >= 0) & (ry < ref_t.shape[0]) & (rx >= 0) & (rx < ref_t.shape[1])
    near = _dilate(ref_t)
    only = inside & ~near[ry.clip(0, ref_t.shape[0] - 1), rx.clip(0, ref_t.shape[1] - 1)]
    # scale moving-pixel counts to reference-pixel area
    return round(float(only.sum() * (mfy * mfx) / (fy * fx) / n_ref), 4)


def _dilate(mask, iterations=3):
    """The mask grown by a few coarse pixels, so a section's own edge (thresholded slightly
    differently in two cycles) is not counted as tissue only one of them has."""
    from scipy import ndimage as ndi
    return ndi.binary_dilation(mask, iterations=iterations)


def cycle_checks(ref_levels, mov_levels, ref_c, mov_c, lead, c_pos, tx, pixel_um=None, seed=0):
    """Per-cycle checks against the reference, on the reference's tissue (shared channel only):

    - ``residualPx`` — ``{median, p90, n}``: the offset still left between reference and registered
      cycle, from phase correlation of `CHECK_PATCHES` full-resolution patches on tissue the cycle
      also covers (`RESIDUAL_WARN_UM` is where it reads as misaligned).
    - ``tissueCovered`` — fraction of the reference's tissue that the cycle's scan imaged (inside its
      rectangle and not on a tile it skipped).
    - ``tissueRetained`` — of that, the fraction the cycle still shows as tissue (sections lifting or
      tearing over the cycles).
    - ``tissueOnlyInCycle`` — tissue the cycle shows where the reference has none (a section the
      reference's scan skipped), as a fraction of the reference's tissue: its cells carry only the
      cycles' markers, none of the reference's.
    - ``signalRatio`` — the cycle's shared-channel median on that tissue over the reference's.
    - ``sharpnessRatio`` — median per-patch focus (`_sharpness`) of the cycle over the reference's.

    Coarse numbers come from the coarse level (`tissue_utils.coarse_level`); nothing is warped whole.
    """
    from skimage.registration import phase_cross_correlation
    a = affine_matrix(tx)
    shapes = [tuple(lv.shape) for lv in ref_levels]
    li = min(tissue_utils.coarse_level(shapes), len(mov_levels) - 1)
    ref_cp = np.asarray(ref_levels[li][plane_index(lead, c_pos, ref_c)])
    mov_cp = np.asarray(mov_levels[li][plane_index(lead, c_pos, mov_c)])
    ref_t = tissue_utils.tissue_mask(ref_cp)
    mov_t = tissue_utils.tissue_mask(mov_cp)
    fy, fx = tissue_utils.level_factor(shapes[0], shapes[li])
    mfy, mfx = tissue_utils.level_factor(mov_levels[0].shape, mov_levels[li].shape)
    mh0, mw0 = mov_levels[0].shape[-2:]

    yy, xx = np.nonzero(ref_t)
    out = {'tissueCovered': None, 'tissueRetained': None, 'tissueOnlyInCycle': None, 'signalRatio': None,
           'sharpnessRatio': None, 'residualPx': {'median': None, 'p90': None, 'n': 0}}
    if yy.size == 0:
        return out
    out['tissueOnlyInCycle'] = _only_in_cycle(a, mov_t, (mfy, mfx), ref_t, (fy, fx), int(yy.size))
    pts = np.stack([xx * fx + (fx - 1) / 2.0, yy * fy + (fy - 1) / 2.0, np.ones(yy.size)], axis=1)
    mp = pts @ a.T
    inside = (mp[:, 0] >= 0) & (mp[:, 0] < mw0) & (mp[:, 1] >= 0) & (mp[:, 1] < mh0)
    # covered = inside what the cycle's scan IMAGED: the scanner writes exact zeros on tiles it
    # skipped, inside its own rectangle — tissue there was not lost, it was never scanned
    sy = np.clip((mp[:, 1] / mfy).astype(int), 0, mov_cp.shape[0] - 1)
    sx = np.clip((mp[:, 0] / mfx).astype(int), 0, mov_cp.shape[1] - 1)
    inside &= mov_cp[sy, sx] > 0
    out['tissueCovered'] = round(float(inside.mean()), 4)
    if inside.any():
        my = np.clip((mp[inside, 1] / mfy).astype(int), 0, mov_t.shape[0] - 1)
        mx = np.clip((mp[inside, 0] / mfx).astype(int), 0, mov_t.shape[1] - 1)
        out['tissueRetained'] = round(float(mov_t[my, mx].mean()), 4)
        both = mov_t[my, mx]
        if both.any():
            r = float(np.median(ref_cp[yy[inside][both], xx[inside][both]]))
            m = float(np.median(mov_cp[my[both], mx[both]]))
            out['signalRatio'] = round(m / r, 4) if r > 0 else None

    # full-resolution patches on tissue both scans show, spread over the sections
    ok = np.flatnonzero(inside)
    if ok.size:
        my = np.clip((mp[ok, 1] / mfy).astype(int), 0, mov_t.shape[0] - 1)
        mx = np.clip((mp[ok, 0] / mfx).astype(int), 0, mov_t.shape[1] - 1)
        ok = ok[mov_t[my, mx]]
    rng = np.random.default_rng(seed)
    pick = rng.permutation(ok)[:CHECK_PATCHES * 4] if ok.size else ok
    h0, w0 = shapes[0][-2:]
    half = CHECK_PATCH_PX // 2
    res, sharp = [], []
    for k in pick:
        if len(res) >= CHECK_PATCHES:
            break
        cx, cy = int(pts[k, 0]), int(pts[k, 1])
        y0, x0 = min(max(0, cy - half), h0 - CHECK_PATCH_PX), min(max(0, cx - half), w0 - CHECK_PATCH_PX)
        if y0 < 0 or x0 < 0:
            continue
        y1, x1 = y0 + CHECK_PATCH_PX, x0 + CHECK_PATCH_PX
        rp = np.asarray(ref_levels[0][plane_index(lead, c_pos, ref_c, slice(y0, y1), slice(x0, x1))],
                        dtype=np.float32)
        read = lambda ys, xs: mov_levels[0][plane_index(lead, c_pos, mov_c, ys, xs)]
        wp = warp_tile(read, (mh0, mw0), tx, a, y0, y1, x0, x1, np.float32)
        if wp is None or rp.std() <= 0 or wp.std() <= 0 or (wp == 0).mean() > 0.01:
            continue
        shift, _, _ = phase_cross_correlation(rp, wp, upsample_factor=10)
        res.append(float(np.hypot(*shift)))
        # focus on each scan's own pixels at the same tissue (no interpolation blur on the cycle)
        c_m = a @ np.array([(x0 + x1) / 2.0, (y0 + y1) / 2.0, 1.0])
        my0, mx0 = int(round(c_m[1])) - half, int(round(c_m[0])) - half
        if 0 <= my0 and 0 <= mx0 and my0 + CHECK_PATCH_PX <= mh0 and mx0 + CHECK_PATCH_PX <= mw0:
            mp_native = read(slice(my0, my0 + CHECK_PATCH_PX), slice(mx0, mx0 + CHECK_PATCH_PX))
            s_r, s_m = _sharpness(rp), _sharpness(mp_native)
            if s_r and s_m is not None:
                sharp.append(s_m / s_r)
    if res:
        out['residualPx'] = {'median': round(float(np.median(res)), 2),
                             'p90': round(float(np.percentile(res, 90)), 2), 'n': len(res)}
        if pixel_um:
            out['residualUm'] = {'median': round(float(np.median(res)) * pixel_um, 2),
                                 'p90': round(float(np.percentile(res, 90)) * pixel_um, 2)}
    if sharp:
        out['sharpnessRatio'] = round(float(np.median(sharp)), 4)
    return out
