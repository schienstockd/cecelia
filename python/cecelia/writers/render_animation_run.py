"""GPU-accelerated 3D animation renderer for the offline movie rail.

Called via ``run_py`` from ``record_keyframes_view_movie`` (``api/src/movie_render.jl``) when a
keyframe animation has any 3D-view frame (``dims.ndisplay == 3``). Ray-cast MIP + LUT composite via
``torch.nn.functional.grid_sample`` — the same trilinear interpolation the Julia CPU kernel does
(``render_view_frame_3d``), but batched into ONE call per frame on GPU. Two orders of magnitude
faster: measured 13 min → ~5s on fXgbTl (60 frames, 256², CUDA).

Falls back to CPU torch if CUDA is unavailable — still much faster than the Julia loop because torch's
CPU grid_sample is vectorised.

**2D animations still render in Julia.** This entry is 3D-only; the Julia caller filters to 3D-only
states before spawning this. That keeps 2D animations off the GPU dependency and skips a subprocess
they don't need.

**Contract with Julia** — params dict:
    ``zarrPath``       : store path (a bioformats2raw ``0/`` or a flat ``.zarr``)
    ``outPath``        : mp4 to write
    ``states``         : list, one per frame: ``{t, angles: [rx, ry, rz], center: [cz, cy, cx],
                                                  zoom, specs: [{lo, hi, lut, visible}, …]}``
    ``canvasH``/``canvasW``   : output frame size
    ``zAniso``         : physical_z / physical_x (isotropy correction; ``read_scale`` result)
    ``renderQuality``  : ``draft`` | ``standard`` | ``high`` — samples-per-ray multiplier
    ``fps``            : encoder frame rate
    ``titleCard``      : optional; prepended after the render (same rule as ``encode_movie_run.py``)
    ``overlays``       : optional per-frame ``[{timestamp?, scaleBar?}]``; same shape ``encode_movie_
                          run.py`` reads. Drawn onto the composed frame before it hits the encoder.

Each state may also carry ``overlays2d`` — a dict with ``points`` and/or ``segments`` in DRAWN
PIXEL coordinates plus style knobs (``pointSize``, ``segmentWidth``). Julia already projected
(x, y, z) through the same rotation matrix used for the volume MIP, so the projection math never
leaves Julia. This script only rasterises: ``ellipse`` for dots, ``line`` for ribbons with
per-segment ``alpha`` (already computed for the tail fade). One shape, one draw pass — no matter
whether the animation started as a 2D crop path or a 3D rotation.
"""
import cecelia.utils.script_utils as script_utils
from cecelia.utils.movie_io import movie_writer, crop_to_even
from cecelia.utils.zarr_utils import open_as_zarr, fortify
import numpy as np


_QUALITY_MULT = {'draft': 0.5, 'standard': 1.0, 'high': 2.0}


def _draw_overlays2d(frame_np, overlays2d, canvas_h, canvas_w):
    """Rasterise per-frame overlay dots + track ribbons that Julia has ALREADY projected.

    ``overlays2d`` is the JSON dict Julia emits — `(u, v, colour)` for points and
    `(u0, v0, u1, v1, colour, alpha)` for segments. All coords are already in drawn-pixel space
    (matching this canvas), so no projection math lives here — the ray-cast R matrix and the
    overlay R matrix cannot disagree because there IS only one, and it's in Julia. Draw order
    matches the browser overlay stack: ribbons below, dots on top.
    """
    if not overlays2d:
        return frame_np
    from PIL import Image, ImageDraw
    pts = overlays2d.get('points')
    segs = overlays2d.get('segments')
    if pts is None and segs is None:
        return frame_np
    point_r = max(1, int(overlays2d.get('pointSize', 6) // 2))
    seg_w   = max(1, int(overlays2d.get('segmentWidth', 2)))

    img = Image.fromarray(frame_np, mode='RGB').convert('RGBA')
    draw = ImageDraw.Draw(img)

    if segs is not None:
        u0 = segs['u0']; v0 = segs['v0']; u1 = segs['u1']; v1 = segs['v1']
        cols = segs.get('colour') or []
        alphas = segs.get('alpha') or [1.0] * len(cols)
        for i in range(len(cols)):
            r, g, b = cols[i]
            a = int(round(255 * float(alphas[i])))
            # PIL's line() clips its own; skip only when BOTH endpoints are far off-canvas to save
            # the draw call.
            if (max(u0[i], u1[i]) < 0 or min(u0[i], u1[i]) > canvas_w - 1 or
                max(v0[i], v1[i]) < 0 or min(v0[i], v1[i]) > canvas_h - 1):
                continue
            draw.line([(u0[i], v0[i]), (u1[i], v1[i])],
                      fill=(int(r * 255), int(g * 255), int(b * 255), a),
                      width=seg_w)

    if pts is not None:
        us = pts['u']; vs = pts['v']
        cols = pts.get('colour') or []
        for i, (r, g, b) in enumerate(cols):
            uu, vv = us[i], vs[i]
            if not (-point_r <= uu <= canvas_w - 1 + point_r and
                    -point_r <= vv <= canvas_h - 1 + point_r):
                continue
            draw.ellipse([uu - point_r, vv - point_r, uu + point_r, vv + point_r],
                          fill=(int(r * 255), int(g * 255), int(b * 255), 255))

    return np.asarray(img.convert('RGB'), dtype=np.uint8)


def _rotation_matrix(angles, device, dtype):
    """Composed rotation R = Rz * Ry * Rx (vispy Base3DRotationCamera convention). Matches the Julia
    kernel byte-for-byte so a switch from the Julia CPU fallback doesn't change the rendered pixels."""
    import torch
    rx, ry, rz = [float(np.deg2rad(float(a))) for a in angles]
    sx, cx = np.sin(rx), np.cos(rx)
    sy, cy = np.sin(ry), np.cos(ry)
    sz, cz = np.sin(rz), np.cos(rz)
    R = np.array([
        [cz * cy,  cz * sy * sx - sz * cx,  cz * sy * cx + sz * sx],
        [sz * cy,  sz * sy * sx + cz * cx,  sz * sy * cx - cz * sx],
        [-sy,      cy * sx,                  cy * cx],
    ], dtype=np.float64)
    return torch.tensor(R, device=device, dtype=dtype)


def _load_volume_at_t(arr, t_idx, axes):
    """Load a (C, Z, Y, X) float32 numpy volume at timepoint ``t_idx`` from ``arr`` (level-0 zarr array)
    honoring the store's axis order (`read_axes`). The store is (T, C, Z, Y, X) in the common OME-ZARR
    layout; but if the store is missing an axis (2D, no C, …) we broadcast the missing dims to 1."""
    # Find each axis position; missing dims are treated as size-1 broadcasts.
    ax = [a.lower() for a in (axes or [])]
    def pos(name): return ax.index(name) if name in ax else None
    pt, pc, pz, py, px = pos('t'), pos('c'), pos('z'), pos('y'), pos('x')
    idx = [slice(None)] * arr.ndim
    pt is None or (idx.__setitem__(pt, int(t_idx)))
    vol = np.asarray(fortify(arr[tuple(idx)]))
    # Now vol has shape according to whatever axes remain (t dropped). Move to (C, Z, Y, X); a missing
    # C or Z becomes a leading 1.
    remaining_axes = [a for i, a in enumerate(ax) if i != pt]
    def move_to(target_order):
        order = []
        shape = list(vol.shape)
        for tname in target_order:
            if tname in remaining_axes:
                order.append(remaining_axes.index(tname))
        transposed = np.transpose(vol, order) if order else vol
        return transposed
    v = move_to(['c', 'z', 'y', 'x'])
    # Add leading unit dims for any missing axis, in (C, Z, Y, X) order.
    for tname, ax_pos in (('c', 0), ('z', 1)):
        if tname not in remaining_axes:
            v = np.expand_dims(v, axis=ax_pos)
    return v.astype(np.float32, copy=False)


def _render_frame(vol, state, canvas_h, canvas_w, z_aniso, q_mult, device, dtype):
    """One GPU frame: ray-cast MIP through the rotated volume, composite with per-channel LUTs.

    ``vol`` is a (C, Z, Y, X) float32 tensor already on ``device``. Returns a (H, W, 3) uint8 numpy.
    Same math as ``render_view_frame_3d`` in Julia — this is that kernel batched into one
    ``grid_sample`` call. Trilinear interp, MIP over view-Z, additive per-channel composite."""
    import torch
    import torch.nn.functional as F
    C, Z, Y, X = vol.shape
    # Rotation centre. State's `center` is (cz, cy, cx) in native voxels; default = volume midpoint.
    center = state.get('center')
    if center is None:
        cz, cy, cx = (Z - 1) / 2, (Y - 1) / 2, (X - 1) / 2
    else:
        cz, cy, cx = float(center[0]), float(center[1]), float(center[2])
    R = _rotation_matrix(state.get('angles', [0, 0, 0]), device, dtype)
    zoom = float(state.get('zoom', 1.0)) or 1.0

    # Isotropic world extents. xy in native px, z stretched by anisotropy.
    ext_y, ext_x = float(Y), float(X)
    ext_z = float(Z) * float(z_aniso)
    canvas_span = max(ext_x, ext_y)
    world_per_px = canvas_span / (zoom * canvas_w)
    diag = float(np.sqrt(ext_x ** 2 + ext_z ** 2))
    n_samples = max(4, int(np.ceil(diag * q_mult)))
    step_v = diag / n_samples

    # View-space sample coords → world → volume voxels. The X / Y grids are ONCE per frame; the
    # Z grid is chunked so the (1, C, S, H, W) `grid_sample` temporary stays under a memory budget.
    # `SAMPLE_CHUNK` is picked so the peak allocation is ≤ ~256 MiB at fp32 across (C, H, W):
    # `C * chunk * H * W * 4 B ≤ 256 MiB`  →  `chunk ≤ 256 MiB / (C * H * W * 4)`. This chunking is
    # the difference between a 512² render running and OOMing at 1.85 GiB on an 8 GiB card. Old
    # path used ONE big grid and blew the budget by 7× on large canvases.
    js = (torch.arange(canvas_w, device=device, dtype=dtype) - (canvas_w + 1) / 2) * world_per_px
    is_ = (torch.arange(canvas_h, device=device, dtype=dtype) - (canvas_h + 1) / 2) * world_per_px
    ss_all = (torch.arange(n_samples, device=device, dtype=dtype) - (n_samples + 1) / 2) * step_v

    C = vol.shape[0]
    # 256 MiB / bytes_per_slice — round down, cap at 128 so we don't collapse to 1 unnecessarily on
    # tiny canvases.
    bytes_per_slice = int(C) * int(canvas_h) * int(canvas_w) * 4
    max_chunk = max(1, min(128, (256 * 1024 * 1024) // max(bytes_per_slice, 1)))
    sample_chunk = min(int(n_samples), int(max_chunk))

    chw = None
    z_denom = max(1, Z - 1)
    for s0 in range(0, n_samples, sample_chunk):
        s1 = min(s0 + sample_chunk, n_samples)
        S = s1 - s0
        ss = ss_all[s0:s1]
        XV = js.view(1, 1, -1).expand(S, canvas_h, canvas_w)
        YV = is_.view(1, -1, 1).expand(S, canvas_h, canvas_w)
        ZV = ss.view(-1, 1, 1).expand(S, canvas_h, canvas_w)
        xw = R[0, 0] * XV + R[0, 1] * YV + R[0, 2] * ZV
        yw = R[1, 0] * XV + R[1, 1] * YV + R[1, 2] * ZV
        zw = R[2, 0] * XV + R[2, 1] * YV + R[2, 2] * ZV
        vy = yw + cy
        vx = xw + cx
        vz = zw / float(z_aniso) + cz
        gx = 2 * vx / max(1, X - 1) - 1
        gy = 2 * vy / max(1, Y - 1) - 1
        gz = 2 * vz / z_denom - 1
        grid = torch.stack([gx, gy, gz], dim=-1).unsqueeze(0)     # (1, S, H, W, 3)
        sampled = F.grid_sample(vol.unsqueeze(0), grid, mode='bilinear',
                                 padding_mode='zeros', align_corners=True)   # (1, C, S, H, W)
        # Running MIP across chunks — a per-chunk `.max(dim=2)` keeps the memory bounded and gives
        # bit-identical output to the one-shot path (max is associative).
        chunk_mip = sampled.max(dim=2).values.squeeze(0)          # (C, H, W)
        chw = chunk_mip if chw is None else torch.maximum(chw, chunk_mip)
        del sampled, grid, XV, YV, ZV, xw, yw, zw, vx, vy, vz, gx, gy, gz, chunk_mip

    # Composite: per-channel clip+normalise + LUT LINEAR interp + additive blend. Linear (not nearest)
    # is what the Julia `_lut_at` does — a 2-stop black→base ramp reduces to `n * base`, exactly the
    # additive-primary contract the composite needs. Nearest-neighbour turned the ramp into a hard
    # threshold at n=0.5 (contrast looked "wrong": everything either black or fully saturated).
    acc = torch.zeros((canvas_h, canvas_w, 3), device=device, dtype=dtype)
    for c, spec in enumerate(state['specs']):
        if c >= chw.shape[0] or not bool(spec.get('visible', True)):
            continue
        lo, hi = float(spec['lo']), float(spec['hi'])
        rng = (hi - lo) if abs(hi - lo) > 1e-6 else 1.0
        n = ((chw[c] - lo) / rng).clamp(0, 1)                          # (H, W)
        lut = torch.as_tensor(np.asarray(spec['lut'], dtype=np.float32),
                               device=device, dtype=dtype)              # (K, 3)
        K = lut.shape[0]
        if K == 0:
            continue
        if K == 1:
            acc = acc + lut[0].view(1, 1, 3)
            continue
        p = n * (K - 1)                                                # (H, W)
        i0 = p.floor().long().clamp(0, K - 2)                          # (H, W)
        f = (p - i0.to(dtype)).unsqueeze(-1)                           # (H, W, 1)
        a = lut[i0]                                                    # (H, W, 3)
        b = lut[i0 + 1]                                                # (H, W, 3)
        acc = acc + a + f * (b - a)
    acc = acc.clamp(0, 1)
    return (acc * 255).byte().cpu().numpy()


def run(params):
    import torch
    log = script_utils.get_logfile_utils(params)
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    dtype = torch.float32
    log.log(f'[INFO] render_animation_run: device = {device}')

    zarr_path = params['zarrPath']
    out_path = params['outPath']
    states = params['states']
    canvas_h = int(params.get('canvasH', 512))
    canvas_w = int(params.get('canvasW', 512))
    z_aniso = float(params.get('zAniso', 1.0))
    q_mult = _QUALITY_MULT.get(params.get('renderQuality', 'standard'), 1.0)
    fps = float(params.get('fps', 15))
    overlays = params.get('overlays') if isinstance(params.get('overlays'), list) else None

    # Level 0 of the store. axes tells us the (T, C, Z, Y, X) ordering — read once.
    from cecelia.utils.zarr_utils import read_axes
    zarr_data, _ = open_as_zarr(zarr_path)
    arr0 = zarr_data[0]
    axes = read_axes(zarr_path)

    # Cache the volume for the last-seen t (rotation animations hold t constant across all frames, so
    # this hits N-1 times). Load-per-frame is fine for T-varying animations too — the load isn't the
    # bottleneck, grid_sample is.
    cached_t = None
    vol_gpu = None

    written = 0
    staging = f"{out_path}.tmp.mp4"
    try:
        with movie_writer(staging, fps) as writer:
            for i, state in enumerate(states):
                t_idx = int(state['t'])
                if cached_t != t_idx:
                    vol_np = _load_volume_at_t(arr0, t_idx, axes)
                    vol_gpu = torch.from_numpy(vol_np).to(device, dtype=dtype)
                    cached_t = t_idx
                frame = _render_frame(vol_gpu, state, canvas_h, canvas_w, z_aniso,
                                       q_mult, device, dtype)
                # Overlays draw BEFORE the even-crop so the drawn coords Julia computed for THIS
                # canvas size land exactly. The crop trims one row/col AT MOST (canvas_h/w are
                # already even in typical use).
                ov2d = state.get('overlays2d')
                if ov2d:
                    frame = _draw_overlays2d(frame, ov2d, canvas_h, canvas_w)
                frame = crop_to_even(frame)
                # Optional per-frame overlay (timestamp + scale bar). Same helper the CPU encoder uses.
                if overlays is not None and i < len(overlays):
                    item = overlays[i] or {}
                    from cecelia.utils.title_card import draw_frame_overlays
                    frame = draw_frame_overlays(frame,
                                                 timestamp=item.get('timestamp'),
                                                 scale_bar=item.get('scaleBar'))
                writer.append_data(frame)
                written += 1
                if (i + 1) % 20 == 0:
                    log.log(f'[PROGRESS] {i + 1}/{len(states)}')
        # Promote staging → final path only on complete success.
        import os
        os.replace(staging, out_path)
    except BaseException:
        import os
        try:
            os.remove(staging)
        except OSError:
            pass
        raise

    log.log(f'[INFO] rendered {written} frame(s) to {out_path}')

    # Title card is prepended AFTER the encode, at the final movie's exact resolution — same rule as
    # `encode_movie_run.py`; one PIL font stack for every text glyph on a movie frame.
    card = params.get('titleCard')
    if isinstance(card, dict) and card.get('enabled', True):
        from cecelia.utils import title_card
        duration = float(card.get('durationSec', 3.0))
        k = title_card.prepend_title_to_movie(out_path, card, duration_sec=duration)
        log.log(f'[INFO] prepended {k} title-card frame(s)')


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
