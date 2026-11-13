"""
Register staining cycles — direct port of the old-R inst/modules/sources/cleanupImages/py/register_images.py

Reads N OME-ZARR images (first = fixed = reference, rest = moving cycles), aligns each moving cycle to
the reference on ONE shared channel (`regChannels[i]`, 0-based index per image), and writes the
stacked result as a NEW OME-ZARR multiscale store. Timepoint 0 only — staining cycles are static per
cycle by construction; a set with T > 1 is registered on frame 0 with a WARN.

Improvements over the old R runner:
  - staged_store: cancel-safe (see docs/SEGMENTATION.md → *Stores are written staged, never in place*)
  - persists per-cycle affine transforms as ITK txt files under transformsOut/*.tfm — old R computed
    and threw them away, so the alignment could never be reapplied to labels/tracks without redoing
    the compute

Parameter contract (JSON written by Julia's Register handler):
  imPaths       - absolute .ome.zarr paths, INCLUDING the reference at index 0
  imOutPath     - absolute .ome.zarr path to write the registered stack
  regChannels   - 0-based reg-channel index per source image (same length as imPaths)
  transformsOut - absolute path to a JSON blob summarising per-cycle transforms + a directory
                  sibling holding the raw ITK .tfm files
  doFftInitialization, doAffine2d, doAffine3d, ignoreSpacing, sigma, autoMask,
  samplesPerParameter, expand — direct pass-through to sitkibex.registration
  keepRegChannels - keep each moving cycle's alignment channel in the output (after its markers'
                  place in channel order), to check the alignment and the tissue per cycle
  pixelSizeUm   - the reference's pixel size, for the residual in µm
  qcOutPath     - where the per-cycle checks are written (slide path), read back by the Julia handler

Two paths. A reference plane past `register_utils.REGISTER_WHOLE_PLANE_PX` with one z-plane — a
whole-slide scan, one cycle per scan — is registered on the pyramid and written tile by tile
(`register_utils`: estimate, warp, per-cycle checks); everything else takes the whole-plane sitkibex
path below, as ported.

Vendored engine: python/sitkibex (Apache-2.0, from https://github.com/niaid/sitk-ibex, v0.2.1). See
THIRD_PARTY.md.
"""

import os
import numpy as np

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.atomic_io import write_json_atomic
import cecelia.utils.register_utils as register_utils

# SimpleITK + sitkibex are imported LAZILY inside `run()` — module-load must succeed on every
# platform so the runner-discovery scan (`test_task_result_writes.py`) can import this file. The
# conda-forge SimpleITK Windows build has occasionally shipped with a DLL-resolution failure
# ("DLL load failed while importing _SimpleITK") that a top-level `import SimpleITK` would trip
# on every discovery pass — the task itself is behind a `comingSoon` flag and won't run in the
# GUI, and a REPL/API caller who invokes it will see the import error at the moment they run,
# which is when it belongs.


def _slice_registration_plane(arr, dim_utils, ch_idx):
    """Extract ONE channel × ONE timepoint × full Z/Y/X as a numpy array shaped for SimpleITK.
    The moving/fixed images the ITK optimiser sees. Timepoint 0 by construction — see the module
    docstring.
    """
    slc = [slice(None)] * arr.ndim
    t_idx = dim_utils.dim_idx('T')
    c_idx = dim_utils.dim_idx('C')
    if t_idx is not None:
        slc[t_idx] = 0
    if c_idx is not None:
        slc[c_idx] = int(ch_idx)
    plane = np.asarray(zarr_utils.fortify(arr[tuple(slc)]))
    return np.squeeze(plane)


def output_plan(n_channels, reg_channels, keep_reg):
    """``[(image index, channel index)]`` in output order: every reference channel, then each moving
    cycle's channels, its alignment channel only with ``keep_reg``. The Julia handler builds the
    channel names in the same order (`_register_channel_plan`)."""
    plan = [(0, c) for c in range(n_channels[0])]
    for i in range(1, len(n_channels)):
        plan += [(i, c) for c in range(n_channels[i]) if keep_reg or c != reg_channels[i]]
    return plan


def _is_slide(dim_utils_list, input_arrays):
    ref = input_arrays[0][0]
    flat = all(du.dim_val('Z') <= 1 for du in dim_utils_list)
    return (flat and len(input_arrays[0]) > 1
            and ref.shape[-2] * ref.shape[-1] > register_utils.REGISTER_WHOLE_PLANE_PX)


def run_slide(params, log, im_paths, input_arrays, dim_utils_list, reg_channels, plan, out_shape):
    """The slide path: estimate each cycle on the pyramid, check it, write the output tile by tile.
    Returns the per-cycle records (transform + checks)."""
    import SimpleITK as sitk
    ref_levels = input_arrays[0]
    ref_du = dim_utils_list[0]
    ndim = ref_levels[0].ndim
    c_pos = ref_du.dim_idx('C')
    lead = tuple(None if i == c_pos else 0 for i in range(ndim - 2))
    pixel_um = params.get('pixelSizeUm')
    pixel_um = float(pixel_um) if pixel_um else None
    tx_dir = os.path.splitext(params['transformsOut'])[0] + '_tfms'
    os.makedirs(tx_dir, exist_ok=True)
    n_cycles = len(im_paths)
    n_steps = 1 + (n_cycles - 1) + len(plan) + 1
    step = 1
    transforms, records = [None], []
    for i in range(1, n_cycles):
        log.log(f'>> register cycle {i} ({im_paths[i]}) on channel {reg_channels[i]}, on the pyramid')
        tx, info = register_utils.estimate_affine(ref_levels, input_arrays[i], reg_channels[0],
                                                  reg_channels[i], lead, c_pos, log=log.log)
        a = register_utils.affine_matrix(tx)
        checks = register_utils.cycle_checks(ref_levels, input_arrays[i], reg_channels[0],
                                             reg_channels[i], lead, c_pos, tx, pixel_um=pixel_um)
        tfm_path = os.path.join(tx_dir, f'cycle_{i:03d}.tfm')
        sitk.WriteTransform(tx, tfm_path)
        rec = {'cycle_index': i, 'source_zarr': im_paths[i], 'reg_channel_index': reg_channels[i],
               'transform_path': tfm_path, 'transform_parameters': list(tx.GetParameters()),
               'matrix': a.tolist()}
        rec.update(info)
        rec.update(register_utils.describe(a, ref_levels[0].shape[-2:]))
        rec.update(checks)
        records.append(rec)
        transforms.append(tx)
        res = checks.get('residualUm') or {}
        log.log(f'   shift {rec["shiftPx"]} px, rotation {rec["rotationDeg"]}°, residual '
                f'{checks["residualPx"]["median"]} px median'
                + (f' ({res.get("median")} µm)' if res else '')
                + f', tissue covered {checks["tissueCovered"]}, retained {checks["tissueRetained"]}')
        log.progress(step, n_steps); step += 1

    out_hw = tuple(ref_levels[0].shape[-2:])
    log.log(f'>> write registered image tile by tile: {params["imOutPath"]}')
    with zarr_utils.staged_store(params['imOutPath']) as staging:
        group, level0, pchunks = zarr_utils.open_multiscales_for_writing(
            staging, tuple(out_shape), ref_levels[0].dtype, ref_du,
            nscales=len(ref_levels), reference_zarr=im_paths[0])
        for k, (i, c) in enumerate(plan):
            src = input_arrays[i][0]
            read = lambda ys, xs, src=src, c=c: src[register_utils.plane_index(lead, c_pos, c, ys, xs)]
            def write(ys, xs, arr, k=k):
                level0[register_utils.plane_index(lead, c_pos, k, ys, xs)] = arr
            if i == 0:
                register_utils.copy_plane(read, out_hw, write)
            else:
                register_utils.warp_plane(read, tuple(src.shape[-2:]), transforms[i], out_hw, write,
                                          np.dtype(ref_levels[0].dtype))
            log.progress(step, n_steps); step += 1
        log.log('>> build multiscale pyramid')
        zarr_utils.write_multiscale_pyramid(group, level0, ref_du, len(ref_levels), list(pchunks))
        ome_xml_utils.save_meta_in_zarr(staging, im_paths[0], changed_shape=tuple(out_shape),
                                        dim_utils=ref_du)
        zarr_utils.write_calibration(staging, ref_du)
        # VALID-BOX-EXEMPT: as the whole-plane path below — the moving cycles are resampled onto the
        # reference grid and read as 0 where their scan does not reach.
    log.progress(step, n_steps)
    return records


def run(params):
    log = script_utils.get_logfile_utils(params)
    import SimpleITK as sitk           # lazy — see the top-of-file note
    import sitkibex

    im_paths      = list(params['imPaths'])
    im_out_path   = params['imOutPath']
    # channels arrive as 0-based indices, resolved from names by the Julia handler's `channel_indices`
    # call. A digit string still converts here; only a genuine name is refused with a clear message.
    # See script_utils.channel_indices for the "backend running older Julia" tell.
    reg_channels  = script_utils.channel_indices(
        params['regChannels'], what='regChannels',
        translator='editImages/register.jl (channel_indices per source)')
    tx_out_path   = params['transformsOut']

    if len(im_paths) < 2:
        raise ValueError('register: at least 2 images required (fixed + one moving)')
    if len(reg_channels) != len(im_paths):
        raise ValueError(f'register: regChannels ({len(reg_channels)}) must match imPaths ({len(im_paths)})')

    do_fft_initialization = bool(params.get('doFftInitialization', False))
    do_affine_2d          = bool(params.get('doAffine2d', True))
    do_affine_3d          = bool(params.get('doAffine3d', False))
    ignore_spacing        = bool(params.get('ignoreSpacing', True))
    sigma                 = float(params.get('sigma', 1.0))
    auto_mask             = bool(params.get('autoMask', False))
    samples_per_parameter = int(params.get('samplesPerParameter', 5000))
    expand_val            = int(params.get('expand', 0))
    expand                = expand_val if expand_val > 0 else None
    keep_reg              = bool(params.get('keepRegChannels', False))

    n_cycles = len(im_paths)
    n_steps  = 2 + 2 * (n_cycles - 1)  # open + write + (register + resample) per moving cycle
    step = 0
    log.progress(step, n_steps); step += 1

    log.log(f'>> open {n_cycles} images ({im_paths[0]} = reference)')
    input_arrays = []
    dim_utils_list = []
    for p in im_paths:
        arr, _ = zarr_utils.open_as_zarr(p)
        input_arrays.append(arr)
        omexml = ome_xml_utils.parse_meta(p)
        du = DimUtils(omexml, use_channel_axis=True)
        du.calc_image_dimensions(arr[0].shape, axes=zarr_utils.read_axes(p))
        dim_utils_list.append(du)

    ref_arr = input_arrays[0][0]
    ref_du  = dim_utils_list[0]
    if ref_du.dim_val('T') > 1:
        log.log('[WARN] reference has T > 1 — registering on frame 0 only (staining cycles are static per cycle)')

    # Output shape: reference shape, C = ref_C + Σ cycle_C (less each alignment channel unless kept).
    out_shape = list(ref_arr.shape)
    c_idx     = ref_du.dim_idx('C')
    plan      = output_plan([du.dim_val('C') for du in dim_utils_list], reg_channels, keep_reg)
    total_c   = len(plan)
    if c_idx is not None:
        out_shape[c_idx] = total_c
    log.log(f'>> output shape: {tuple(out_shape)}  (total channels = {total_c})')

    if _is_slide(dim_utils_list, input_arrays):
        records = run_slide(params, log, im_paths, input_arrays, dim_utils_list, reg_channels, plan,
                            out_shape)
        write_json_atomic(tx_out_path, {'reference_zarr': im_paths[0],
                                        'reg_channel_index_reference': reg_channels[0],
                                        'path': 'pyramid', 'cycles': records}, indent=2)
        if params.get('qcOutPath'):
            write_json_atomic(params['qcOutPath'], {'cycles': records}, indent=2)
        log.log(f'>> wrote {len(records)} cycle transform(s); done')
        return

    # Fixed image for ITK — the reference's reg channel, single T, single C.
    fixed_np = _slice_registration_plane(ref_arr, ref_du, reg_channels[0])
    fixed_im = sitk.GetImageFromArray(fixed_np)

    tx_dir = os.path.splitext(tx_out_path)[0] + '_tfms'
    os.makedirs(tx_dir, exist_ok=True)
    tx_records = []

    # Register each moving cycle to the reference.
    reg_tx = [None]  # index 0 = reference, no transform
    for i in range(1, n_cycles):
        log.log(f'>> register cycle {i} ({im_paths[i]}) on channel {reg_channels[i]}')
        mov_np = _slice_registration_plane(input_arrays[i][0], dim_utils_list[i], reg_channels[i])
        mov_im = sitk.GetImageFromArray(mov_np)
        tx = sitkibex.registration(
            fixed_im, mov_im,
            do_fft_initialization=do_fft_initialization,
            do_affine2d=do_affine_2d,
            do_affine3d=do_affine_3d,
            ignore_spacing=ignore_spacing,
            sigma=sigma,
            auto_mask=auto_mask,
            samples_per_parameter=samples_per_parameter,
            expand=expand)
        reg_tx.append(tx)
        # Persist the transform (improvement over old R).
        tfm_path = os.path.join(tx_dir, f'cycle_{i:03d}.tfm')
        sitk.WriteTransform(tx, tfm_path)
        tx_records.append({
            'cycle_index': i,
            'source_zarr': im_paths[i],
            'reg_channel_index': reg_channels[i],
            'transform_path': tfm_path,
            'transform_parameters': list(tx.GetParameters()),
        })
        log.progress(step, n_steps); step += 1

    # Stream the stacked output straight to disk — the assembled image never lives in RAM, only one
    # 3D channel volume at a time. IBEX-scale cycles (multi-channel large 3D) would OOM on a full-
    # shape allocation, which is what `open_multiscales_for_writing` exists to prevent (same
    # streaming path that drift/AF/cellpose correction take — see drift_correct_run.py).
    log.log(f'>> write registered image: {im_out_path}')
    ref_du_out = ref_du   # output geometry mirrors the reference: same T/Z/Y/X, C grows

    def _plane_slice(dim_utils, shape, ch, t=0):
        """Full (Y, Z, …) at one T and one C — the unit we stream per-channel. Any absent axis is
        omitted (T=1 stacks / 2D staining images work the same way)."""
        sl = [slice(None)] * len(shape)
        if dim_utils.dim_idx('T') is not None:
            sl[dim_utils.dim_idx('T')] = 0 if shape[dim_utils.dim_idx('T')] == 1 else t
        if dim_utils.dim_idx('C') is not None:
            sl[dim_utils.dim_idx('C')] = ch
        return tuple(sl)

    with zarr_utils.staged_store(im_out_path) as staging:
        group, level0, pchunks = zarr_utils.open_multiscales_for_writing(
            staging, tuple(out_shape), ref_arr.dtype, ref_du_out,
            nscales=len(input_arrays[0]),
            reference_zarr=im_paths[0])   # inherit source's zarr format (v2/v3), see ZARR_V3_PLAN D9

        # Reference channels → output 0..ref_C-1. One channel at a time keeps peak memory bounded to
        # one 3D volume per write.
        for c in range(ref_du.dim_val('C')):
            src = ref_arr[_plane_slice(ref_du, ref_arr.shape, c)]
            level0[_plane_slice(ref_du_out, tuple(out_shape), c)] = np.asarray(zarr_utils.fortify(src))

        # Moving cycles → output ref_C.. . For each non-reg channel: pull the 3D volume, ITK-resample
        # against the fixed reference using the cycle's affine transform, write the result plane.
        channel_sum = ref_du.dim_val('C')
        for i in range(1, n_cycles):
            mov_arr = input_arrays[i][0]
            mov_du  = dim_utils_list[i]
            k = 0
            for c in range(mov_du.dim_val('C')):
                if c == reg_channels[i] and not keep_reg:
                    continue
                mov_np = np.squeeze(np.asarray(zarr_utils.fortify(
                    mov_arr[_plane_slice(mov_du, mov_arr.shape, c)])))
                mov_im = sitk.GetImageFromArray(mov_np)
                resampled_im = sitkibex.resample(
                    fixed_image=fixed_im,
                    moving_image=mov_im,
                    transform=reg_tx[i])
                # ITK arrays are already numpy — cast to the output dtype and reshape to whatever the
                # destination's non-T non-C axes need (the T axis is length-1, so `_plane_slice`
                # returns a slot the ndarray fits into directly).
                dst_slot = _plane_slice(ref_du_out, tuple(out_shape), channel_sum + k)
                arr = sitk.GetArrayFromImage(resampled_im).astype(ref_arr.dtype, copy=False)
                level0[dst_slot] = arr.reshape(np.asarray(level0[dst_slot]).shape) if arr.ndim != len(dst_slot) else arr
                k += 1
            channel_sum += k
            log.progress(step, n_steps); step += 1

        # Build the downscaled levels FROM the on-disk level 0 — the same helper drift/AF/smooth use.
        # Peak memory stays bounded per level, not the whole pyramid.
        log.log('>> build multiscale pyramid')
        zarr_utils.write_multiscale_pyramid(group, level0, ref_du_out, len(input_arrays[0]), list(pchunks))

        ome_xml_utils.save_meta_in_zarr(
            staging, im_paths[0],
            changed_shape=tuple(out_shape),   # SizeC grows; SizeX/Y/Z/T carry
            dim_utils=ref_du_out,
        )
        zarr_utils.write_calibration(staging, ref_du_out)

        # VALID-BOX-EXEMPT: registration RESAMPLES the moving cycles onto the reference geometry, so
        # each moving cycle's box (if any) is remapped — computing the transformed union is doable
        # but unneeded for a first cut. No box means "all valid", so a consumer skips nothing and is
        # merely slower, never wrong. Revisit when we land the follow-up that reapplies transforms
        # to labels/tracks.

    log.progress(step, n_steps); step += 1

    # Persist the transforms summary.
    write_json_atomic(tx_out_path, {
        'reference_zarr': im_paths[0],
        'reg_channel_index_reference': reg_channels[0],
        'cycles': tx_records,
    }, indent=2)
    log.log(f'>> wrote {len(tx_records)} cycle transform(s) to {tx_dir}')
    log.log('>> done')


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
