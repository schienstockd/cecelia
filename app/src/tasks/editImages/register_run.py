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

Vendored engine: python/sitkibex (Apache-2.0, from https://github.com/niaid/sitk-ibex, v0.2.1). See
THIRD_PARTY.md.
"""

import os
import numpy as np
import SimpleITK as sitk
import sitkibex

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.atomic_io import write_json_atomic


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


def run(params):
    log = script_utils.get_logfile_utils(params)

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

    n_cycles = len(im_paths)
    n_steps  = 2 + 2 * (n_cycles - 1)  # open + write + (register + resample) per moving cycle
    step = 0
    log.progress(step, n_steps); step += 1

    log.log(f'>> open {n_cycles} images ({im_paths[0]} = reference)')
    input_arrays = []
    dim_utils_list = []
    for p in im_paths:
        arr, _ = zarr_utils.open_as_zarr(p, as_dask=True)
        input_arrays.append(arr)
        omexml = ome_xml_utils.parse_meta(p)
        du = DimUtils(omexml, use_channel_axis=True)
        du.calc_image_dimensions(arr[0].shape)
        dim_utils_list.append(du)

    ref_arr = input_arrays[0][0]
    ref_du  = dim_utils_list[0]
    if ref_du.dim_val('T') > 1:
        log.log('[WARN] reference has T > 1 — registering on frame 0 only (staining cycles are static per cycle)')

    # Output shape: reference shape, C = ref_C + Σ(cycle_C - 1).
    out_shape = list(ref_arr.shape)
    c_idx     = ref_du.dim_idx('C')
    total_c   = ref_du.dim_val('C') + sum(dim_utils_list[i].dim_val('C') - 1 for i in range(1, n_cycles))
    if c_idx is not None:
        out_shape[c_idx] = total_c
    log.log(f'>> output shape: {tuple(out_shape)}  (total channels = {total_c})')

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

    # Write the stacked output.
    log.log(f'>> write registered image: {im_out_path}')
    with zarr_utils.staged_store(im_out_path) as staging:
        out_zarr = zarr_utils.zarr_utils_writable_full_shape(
            staging, out_shape, ref_arr.dtype, ref_arr.chunksize,
        ) if hasattr(zarr_utils, 'zarr_utils_writable_full_shape') else None
        # Fall back to a lower-level create_multiscales-based write via a NumPy staging tensor if the
        # helper above is not present in this checkout — the runner is scoped to editImages so we lean
        # on the same primitives crop uses. Simplest and correct: assemble in memory, then hand to
        # create_multiscales. Registration outputs are per-plane small; the cost is fine.
        assembled = np.zeros(tuple(out_shape), dtype=ref_arr.dtype)

        # First push the reference — every channel, frame 0.
        ref_slice = [slice(None)] * len(out_shape)
        if ref_du.dim_idx('T') is not None:
            ref_slice[ref_du.dim_idx('T')] = 0
        # Reference channels 0..ref_C into output channels 0..ref_C.
        for c in range(ref_du.dim_val('C')):
            src_slc = list(ref_slice); src_slc[c_idx] = c
            dst_slc = list(ref_slice); dst_slc[c_idx] = c
            assembled[tuple(dst_slc)] = np.asarray(zarr_utils.fortify(ref_arr[tuple(src_slc)]))

        # Now each moving cycle's non-reg channels, resampled onto the reference geometry.
        channel_sum = ref_du.dim_val('C')
        for i in range(1, n_cycles):
            mov_arr = input_arrays[i][0]
            mov_du  = dim_utils_list[i]
            k = 0
            for c in range(mov_du.dim_val('C')):
                if c == reg_channels[i]:
                    continue
                # Extract the moving cycle's channel at T=0 → SimpleITK → resample using the
                # registered transform → write into the output.
                mov_slc = [slice(None)] * mov_arr.ndim
                if mov_du.dim_idx('T') is not None:
                    mov_slc[mov_du.dim_idx('T')] = 0
                mov_slc[mov_du.dim_idx('C')] = c
                mov_np_c = np.squeeze(np.asarray(zarr_utils.fortify(mov_arr[tuple(mov_slc)])))
                mov_im_c = sitk.GetImageFromArray(mov_np_c)
                resampled_im = sitkibex.resample(
                    fixed_image=fixed_im,
                    moving_image=mov_im_c,
                    transform=reg_tx[i])
                resampled_np = sitk.GetArrayFromImage(resampled_im)

                dst_slc = list(ref_slice); dst_slc[c_idx] = channel_sum + k
                # squeeze/broadcast into the destination's exact shape
                assembled[tuple(dst_slc)] = resampled_np.astype(ref_arr.dtype, copy=False).reshape(
                    assembled[tuple(dst_slc)].shape)
                k += 1
            channel_sum += mov_du.dim_val('C') - 1
            log.progress(step, n_steps); step += 1

        # Wrap the assembled numpy array as dask and hand to create_multiscales (same primitives crop
        # uses). ref_arr is the calibration/format reference.
        import dask.array as da
        out_dask = da.from_array(assembled, chunks=ref_arr.chunksize)
        zarr_utils.create_multiscales(
            out_dask, staging,
            dim_utils=ref_du,
            reference_zarr=ref_arr,
            nscales=len(input_arrays[0]),
        )
        ome_xml_utils.save_meta_in_zarr(
            staging, im_paths[0],
            changed_shape=tuple(out_shape),   # SizeC grows; SizeX/Y/Z/T carry
            dim_utils=ref_du,
        )
        zarr_utils.write_calibration(staging, ref_du)

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
    params = script_utils.script_params(
        flatten_except=['imPaths', 'regChannels']
    )
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
