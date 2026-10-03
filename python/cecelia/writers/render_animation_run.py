"""Movie renderer for the offline movie rail — the browser viewer's own MIP shader, run headlessly.

Called via ``run_py`` from ``api/src/movie_render.jl`` for every movie: keyframe animations, the
viewer's Record, batch and the compare grid's cells, 2D and 3D. Each frame is drawn by
``shaders/mip.wgsl`` plus its point and track-tail passes on a ``wgpu`` device
(``cecelia.utils.wgpu_host``), so a movie frame is the viewer's frame by construction — camera,
contrast, colours, masks and overlays (``docs/todo/SHARED_RENDERER_PLAN.md`` Phases 2–3). It needs
no CUDA: any GPU through Vulkan / Metal / DX12, else a software adapter.

Stills (card filmstrips, keyframe thumbnails) are the same frames written as PNGs — ``render_stills``,
which the resident preview worker serves with a host it keeps warm (``docs/todo/STILLS_WORKER_PLAN.md``).

A 2D movie is drawn the way the viewer's 2D view is: the same pass, head-on and orthographic, over a
slab of planes (one plane = one sample; several = their max), at the pyramid level its zoom picks.

**Contract with Julia** — params dict:
    ``zarrPath``       : image store (a bioformats2raw ``0/`` or a flat ``.zarr``)
    ``outPath``        : mp4 to write — or ``outPaths``, one PNG per state (a still; no title card)
    ``states``         : one per frame:
        ``t``          : timepoint
        ``camera``     : the view state's camera, as the viewer stored it —
                         ``{angles: [rx°, ry°, rz°], center?: [cz, cy, cx], zoom, perspective?}``
        ``snapH``      : height of the canvas the zoom was measured on (the view state's ``canvas``),
                         or absent
        ``specs``      : per channel ``{lo, hi, lut: [[r, g, b], …], visible}`` (``_as_lut`` stops)
        ``overlays3d`` : optional ``{points: {x, y, z, colour}, segments: {x0, y0, z0, x1, y1, z1,
                         colour}}`` in native voxel coordinates, colours as ``[r, g, b]`` in 0..1
        ``ndisplay``   : 2 or 3 (default 3)
        ``zRange``     : 2D only — the level-0 planes ``[lo, hi]`` shown (default: all)
        ``planeFilter``: 2D only — ``{points: [lo, hi], tracks: [lo, hi]}``, the planes whose points /
                         tail ends are drawn (the viewer's z tolerances); absent = all
    ``canvasH``/``canvasW`` : output frame size
    ``zAniso``         : physical_z / physical_x
    ``renderQuality``  : ``draft`` | ``standard`` | ``high`` — 128 / 256 / 512 ray steps; the viewer's
                         default is 256
    ``fps``            : encoder frame rate
    ``labelsPath``     : optional label store drawn as the viewer's 3D mask (nearest id along the
                         ray, ``label_palette``), with ``labelOpacity`` (default the viewer's
                         ``LABEL_OPACITY``) / ``labelContourPx``; ``labelColouring``
                         ``palette`` (default) or ``table``: ``labelColours`` = ``{ids,
                         colours}`` draws only those labels, in those colours (a population
                         mask) — an empty table draws no mask
    ``pointSizePx`` / ``pointBorderPx`` / ``segmentWidthPx`` : overlay style, in output pixels
    ``titleCard``      : optional; prepended after the render (``title_card.prepend_title_to_movie``)
    ``overlays``       : optional per-frame ``[{timestamp?, scaleBar?}]`` — drawn onto the encoded
                         frame
"""
import os

import numpy as np

import cecelia.utils.script_utils as script_utils
from cecelia.utils.atomic_io import atomic_path
from cecelia.utils import wgpu_host, wgsl_utils
from cecelia.utils.movie_io import movie_writer, crop_to_even
from cecelia.utils.zarr_utils import open_as_zarr, read_axes, fortify

_QUALITY_STEPS = {'draft': 128, 'standard': 256, 'high': 512}


def _load_at_t(arr, t_idx, axes, want=('c', 'z', 'y', 'x'), z_slice=None):
    """``arr`` (one level) at timepoint ``t_idx`` as the axes in ``want``, honouring the store's own
    order (``read_axes``). A missing axis becomes a leading size-1 dim. ``z_slice`` reads only those
    planes of this level (a 2D frame's slab)."""
    ax = [a.lower() for a in (axes or [])]
    idx = [slice(None)] * arr.ndim
    if 't' in ax:
        idx[ax.index('t')] = int(t_idx)
    if z_slice is not None and 'z' in ax:
        idx[ax.index('z')] = z_slice
    vol = np.asarray(fortify(arr[tuple(idx)]))
    remaining = [a for a in ax if a != 't']
    vol = np.transpose(vol, [remaining.index(a) for a in want if a in remaining])
    for i, a in enumerate(want):
        if a not in remaining:
            vol = np.expand_dims(vol, axis=i)
    return vol


def _as_u16(vol):
    """The viewer's ``r16uint``. A u8 store widens exactly (the shader reads integer values); anything
    else is clipped into range."""
    if vol.dtype == np.uint16:
        return np.ascontiguousarray(vol)
    if vol.dtype == np.uint8:
        return vol.astype(np.uint16)
    return np.clip(vol, 0, 65535).astype(np.uint16)


def _pick_level(levels, host, axes):
    """Level 0 when it fits one 3D texture on this device, else the first coarser level that does.
    The movie wants full resolution; the device limit is the only reason to give it up."""
    ax = [a.lower() for a in axes]
    for li, lvl in enumerate(levels):
        shape = dict(zip(ax, lvl.shape))
        czyx = [shape.get(a, 1) for a in ('c', 'z', 'y', 'x')]
        if host.fits(czyx):
            return li
    raise RuntimeError('render_animation_run: no level of this image fits a 3D texture '
                       f'(max {host.max_texture_3d} per side)')


def _pick_level_2d(levels, state, canvas_h):
    """The level the viewer's 2D view would show this state at (``wgpu_host.plane_level``): its
    zoom is L0 pixels per output pixel — the image rows the camera shows over the canvas height."""
    cam = state.get('camera') or {}
    snap_h = float(state.get('snapH') or canvas_h)
    per_px = (snap_h / max(1e-6, float(cam.get('zoom') or 1))) / max(1, canvas_h)
    return wgpu_host.plane_level(per_px, len(levels))


def _level_z(z_range, nz, nz0):
    """The level-0 planes ``z_range`` (inclusive) as a slice of a level with ``nz`` planes — a level
    with fewer planes maps them proportionally."""
    lo, hi = int(z_range[0]), int(z_range[1])
    zl = min(nz - 1, lo * nz // max(1, nz0))
    zh = min(nz - 1, max(zl, hi * nz // max(1, nz0)))
    return slice(zl, zh + 1)


def _point_instances(pts, voxel_um):
    """Julia's per-frame points → ``POINT_STRIDE`` rows: centre in image µm, rgb, z plane — what
    ``buildPointBuffer`` uploads (a centroid's µm is ``pixel × voxel size``)."""
    if not pts or not pts.get('x'):
        return None
    vx, vy, vz = voxel_um
    z = np.asarray(pts.get('z') or [0.0] * len(pts['x']), np.float64)
    out = np.empty((len(pts['x']), wgpu_host.POINT_STRIDE), np.float32)
    out[:, 0] = np.asarray(pts['x']) * vx
    out[:, 1] = np.asarray(pts['y']) * vy
    out[:, 2] = z * vz
    out[:, 3:6] = np.asarray(pts['colour'], np.float64).reshape(-1, 3)
    out[:, 6] = np.floor(z)
    return out


def _segment_instances(segs, voxel_um):
    """Julia's per-frame tail segments → ``SEG_STRIDE`` rows: from, to (image µm), rgb, end plane."""
    if not segs or not segs.get('x0'):
        return None
    vx, vy, vz = voxel_um
    out = np.empty((len(segs['x0']), wgpu_host.SEG_STRIDE), np.float32)
    for k, (a, s) in enumerate((('x0', vx), ('y0', vy), ('z0', vz), ('x1', vx), ('y1', vy), ('z1', vz))):
        out[:, k] = np.asarray(segs[a]) * s
    out[:, 6:9] = np.asarray(segs['colour'], np.float64).reshape(-1, 3)
    out[:, 9] = np.floor(np.asarray(segs['z1'], np.float64))
    return out


def frame_uniforms(state, dims_czyx, l0_zyx, voxel_um, canvas_h, steps, label_style, overlay_style,
                   z_range=None):
    """The uniform lanes for one frame — the same lanes ``volumeRenderer.ts`` writes.

    ``dims_czyx`` is the texture actually uploaded; ``l0_zyx`` and ``voxel_um`` are level 0's, which
    is what the camera, the extents and the overlays are measured in (``meta.nX`` / ``voxelUm`` in
    the viewer). A coarser level covers the same µm with fewer voxels, so only ``dims`` changes.

    ``z_range`` = the level-0 planes ``(lo, hi)`` the texture holds — a 2D frame's slab, as the
    viewer's ``setImage`` / ``setZPlane`` place it (``ext.z`` = its depth, ``zOriginUm`` = its first
    plane). ``None`` = the whole stack. A 2D frame is head-on and orthographic, as in the viewer."""
    nc, nz, ny, nx = dims_czyx
    nz0, ny0, nx0 = l0_zyx
    vx, vy, vz = voxel_um
    z_lo, z_hi = (0, nz0 - 1) if z_range is None else (int(z_range[0]), int(z_range[1]))
    flat = int(state.get('ndisplay', 3)) == 2
    cam = wgpu_host.view_camera(state.get('camera') or {}, state.get('snapH'), nx0, ny0, voxel_um, canvas_h)
    perspective = not flat and float((state.get('camera') or {}).get('perspective') or 0) > 0
    # Overlay plane filters (-1 = off): a 2D frame's state carries the viewer's own windows, the
    # slab ± its z tolerances (`setOverlayDraw` / `setOverlaySegmentDraw`).
    pf = state.get('planeFilter') or {}
    p_lo, p_hi = (pf.get('points') or (-1, -1))
    r_lo, r_hi = (pf.get('tracks') or (-1, -1))
    u = {'cam.yaw': 0 if flat else cam['yaw'], 'cam.pitch': 0 if flat else cam['pitch'],
         'cam.dist': cam['dist'], 'cam.steps': steps,
         'vp.nch': nc, 'vp.ortho': 0 if perspective else 1,
         'ext.x': nx0 * vx, 'ext.y': ny0 * vy, 'ext.z': (z_hi - z_lo + 1) * vz, 'ext.zOriginUm': z_lo * vz,
         'dims.nx': nx, 'dims.ny': ny, 'dims.nz': nz, 'dims.zPerChannel': nz,
         'pan.x': cam['panX'], 'pan.y': cam['panY'],
         'ov.planeLo': p_lo, 'ov.planeHi': p_hi, 'pan.ribbonLo': r_lo, 'pan.ribbonHi': r_hi,
         'ov.pointPx': max(1, overlay_style['pointPx']), 'ov.tailPx': max(1, overlay_style['tailPx']),
         'lab.pointBorderPx': max(0, overlay_style['borderPx'])}
    if label_style is not None:
        u['lab.opacity'] = label_style['opacity']
        u['lab.contourPx'] = label_style['contourPx']
        u['lab.paletteRows'] = label_style['rows']
    # The shader composites the first MAX_CHANNELS; the viewer shows the same subset.
    shown = min(nc, int(wgsl_utils.shader_constants()['MAX_CHANNELS']))
    for c, spec in enumerate(state.get('specs') or []):
        if c >= shown:
            break
        u[f'ch[{c}].lo'] = float(spec['lo'])
        u[f'ch[{c}].hi'] = float(spec['hi'])
        u[f'ch[{c}].visible'] = 1 if spec.get('visible', True) else 0
    return u


def render_frames(params, host, log):
    """One sRGB ``(H, W, 3)`` uint8 frame per ``params['states']``, timestamp / scale bar drawn —
    what the mp4 encodes and what a still saves. ``host`` is a ``MipHost``; its volume, labels and
    palette are replaced here, so one host serves any number of calls in turn."""
    states = params['states']
    canvas_h = int(params.get('canvasH', 512))
    canvas_w = int(params.get('canvasW', 512))
    # x-voxel units: the shader only needs the three extents, the camera and the overlays in ONE
    # unit, and the voxel's x size is the one every other length is already relative to.
    voxel_um = (1.0, 1.0, float(params.get('zAniso', 1.0)))
    steps = _QUALITY_STEPS.get(params.get('renderQuality', 'standard'), 256)
    overlays = params.get('overlays') if isinstance(params.get('overlays'), list) else None
    overlay_style = {'pointPx': float(params.get('pointSizePx', 6)),
                     'borderPx': float(params.get('pointBorderPx', 0)),
                     'tailPx': float(params.get('segmentWidthPx', 2))}

    zarr_path = params['zarrPath']
    levels, _ = open_as_zarr(zarr_path)
    axes = read_axes(zarr_path)
    ax = [a.lower() for a in axes]
    shape0 = dict(zip(ax, levels[0].shape))
    l0_zyx = (shape0.get('z', 1), shape0.get('y', 1), shape0.get('x', 1))
    # A movie is 2D or 3D throughout (Julia never mixes them). 2D draws a slab of planes at the
    # level the viewer's 2D view would pick for its zoom; 3D the whole stack at the finest level
    # that fits.
    flat = bool(states) and all(int(s.get('ndisplay', 3)) == 2 for s in states)
    if flat:
        level = _pick_level_2d(levels, states[0], canvas_h)
    else:
        level = _pick_level(levels, host, axes)
        if level:
            log.log(f'[INFO] level 0 does not fit a 3D texture on this device; rendering level {level}')
    arr = levels[level]
    nz_level = dict(zip(ax, arr.shape)).get('z', 1)

    palette = wgpu_host.label_palette()
    host.set_palette(palette)
    host.set_labels(None)
    labels_arr = label_style = None
    if params.get('labelsPath'):
        # The mask goes on the same grid as the image level, or not at all — a mismatched texture
        # would outline the wrong voxels (Julia's `mask_fits_frame` already checked level 0).
        lab_levels, _ = open_as_zarr(params['labelsPath'])
        colouring = params.get('labelColouring', 'palette')
        if colouring not in ('palette', 'table'):
            raise ValueError(f'render_animation_run: labelColouring {colouring!r} is not palette | table')
        table = params.get('labelColours') or {}
        if colouring == 'table' and not table.get('ids'):
            # A population with no cells on this image — nothing to draw, which is not "every label".
            log.log('[INFO] mask skipped — its populations have no labels on this image')
        elif level < len(lab_levels):
            labels_arr, lab_axes = lab_levels[level], read_axes(params['labelsPath'])
            opacity = params.get('labelOpacity', wgsl_utils.shader_constants()['LABEL_OPACITY'])
            label_style = {'opacity': float(opacity),
                           'contourPx': max(0, int(round(float(params.get('labelContourPx', 0))))),
                           'rows': len(palette)}
            # A population mask: the shader's colour table; labels absent from it are not drawn.
            if colouring == 'table':
                host.set_palette(wgpu_host.label_table(table['ids'], table['colours']))
                label_style['rows'] = -wgpu_host.LABEL_TABLE_W
        else:
            log.log(f'[WARN] mask skipped — its store has no level {level} to match the image')

    cached = None
    dims = None
    for i, state in enumerate(states):
        t_idx = int(state['t'])
        z_range = None
        if flat:
            zr = state.get('zRange')
            z_range = (int(zr[0]), int(zr[1])) if zr else (0, l0_zyx[0] - 1)
        if cached != (t_idx, z_range):
            zs = _level_z(z_range, nz_level, l0_zyx[0]) if z_range else None
            vol = _as_u16(_load_at_t(arr, t_idx, axes, z_slice=zs))
            host.set_volume(vol)
            dims = vol.shape
            if labels_arr is not None:
                lab = _load_at_t(labels_arr, t_idx, lab_axes, want=('z', 'y', 'x'), z_slice=zs)
                host.set_labels(np.ascontiguousarray(lab, dtype=np.uint32))
            cached = (t_idx, z_range)
        host.set_lut(wgpu_host.lut_rows([s.get('lut') or [] for s in state.get('specs') or []]))
        ov = state.get('overlays3d') or {}
        host.set_points(_point_instances(ov.get('points'), voxel_um))
        host.set_segments(_segment_instances(ov.get('segments'), voxel_um))
        # 2D: one sample is the plane; a slab is a top-down max over its planes, one step
        # each, which hits every plane's centre (the viewer's ± window marches it finer).
        n_steps = max(1, dims[1]) if flat else steps
        lanes = frame_uniforms(state, dims, l0_zyx, voxel_um, canvas_h, n_steps,
                               label_style, overlay_style, z_range=z_range)
        frame = host.render(canvas_w, canvas_h, lanes)[..., :3]
        frame = crop_to_even(np.ascontiguousarray(frame))
        # Timestamp / scale bar go onto the ENCODED frame, already sRGB — the same helper
        # and order as the 2D encoder.
        if overlays is not None and i < len(overlays):
            item = overlays[i] or {}
            from cecelia.utils.title_card import draw_frame_overlays
            frame = draw_frame_overlays(frame, timestamp=item.get('timestamp'),
                                        scale_bar=item.get('scaleBar'))
        yield frame
        if (i + 1) % 20 == 0:
            log.log(f'[PROGRESS] {i + 1}/{len(states)}')


def render_stills(params, host, log):
    """``render_frames`` written to ``params['outPaths']`` as PNGs, one per state; returns the paths."""
    from PIL import Image
    out_paths = [str(p) for p in params['outPaths']]
    if len(out_paths) != len(params['states']):
        raise ValueError(f"render_animation_run: {len(out_paths)} outPaths for {len(params['states'])} states")
    for path, frame in zip(out_paths, render_frames(params, host, log)):
        with atomic_path(path) as tmp:
            Image.fromarray(frame).save(tmp, format='PNG')
    return out_paths


def run(params):
    log = script_utils.get_logfile_utils(params)
    host = wgpu_host.MipHost()
    info = host.adapter_info
    log.log(f"[INFO] render_animation_run: {info.get('device')} ({info.get('backend_type')}, "
            f"{info.get('adapter_type')})")
    if params.get('outPaths'):
        log.log(f'[INFO] rendered {len(render_stills(params, host, log))} still(s)')
        return

    written = 0
    out_path = params['outPath']
    staging = f"{out_path}.tmp.mp4"
    try:
        with movie_writer(staging, float(params.get('fps', 15))) as writer:
            for frame in render_frames(params, host, log):
                writer.append_data(frame)
                written += 1
        os.replace(staging, out_path)
    except BaseException:
        try:
            os.remove(staging)
        except OSError:
            pass
        raise

    log.log(f'[INFO] rendered {written} frame(s) to {out_path}')

    # Title card is prepended AFTER the encode, at the final movie's exact resolution — same rule as
    # the compare grid's stitcher; one PIL font stack for every text glyph on a movie frame.
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
