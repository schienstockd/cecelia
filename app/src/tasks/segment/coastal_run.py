"""
Optical-flow (coastal) segmentation task entry point.

Called by the Julia coastalSegment handler as a subprocess. Structurally identical to
`cellpose_run.py` — open the image, build DimUtils, hand it to a `SegmentationUtils` subclass, bank
the QC counts — because the tiling, streaming and label-store machinery is the base's, not this
segmenter's. The only difference in kind is that coastal's prediction for timepoint `t` also reads
frames around `t`; the base supplies that window (`TEMPORAL_RADIUS`), so nothing here has to.

Parameter contract (JSON written by Julia) — as cellpose_run.py, except `models`:
  models            - dict keyed "0","1",…; each entry:
      model           - ABSOLUTE path to a vault checkpoint (Julia resolves the name)
      matchAs         - 'base' or 'nuc'
      cellChannels    - 0-based channel indices for cell signal
      normalise       - percentile for intensity normalisation (default 99.99)
      seedSize        - local-maximum window for seeding
      seedBlurSigma   - blur applied to the SEED map only (outline unaffected)
      probThreshold, affinityThreshold, minComponentSize, probBlurSigma,
      embeddingBlurSigma, mergeAffinityThreshold, mergeMaxDistance, probWeight, maxIter
      stitchThreshold - IoU for matching labels across Z (3D only)

Stacking a second `models` entry is how a second pass is run: the base offsets each group's label
IDs and fills only unlabelled pixels, so a later group picks up what the first missed without
overwriting it.
"""

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.coastal_utils import CoastalUtils
from cecelia.utils.atomic_io import write_json_atomic


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path = params['imPath']

    log.log(f'>> open image: {im_path}')
    # Plain zarr: `predict_from_zarr` streams per timepoint via `read_timepoint` — no dask compute.
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=False)

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(im_dat[0].shape)

    log.log(f'>> dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')
    log.log(f'>> models: {sorted(params.get("models", {}).keys())}')

    cu = CoastalUtils(params, dim_utils)
    log.log(f'>> GPU: {cu.gpu_device if cu.use_gpu else "none (CPU)"}')
    log.log(f'>> temporal window: +/-{cu.TEMPORAL_RADIUS} frames per tile')
    log.log(f'>> tiling: block={cu.block_size} overlap={cu.overlap} '
            f'normalise_to_whole={cu.normalise_to_whole}')

    label_counts = cu.predict_from_zarr(im_dat)

    qc_out_path = params.get('qcOutPath')
    if qc_out_path:
        write_json_atomic(qc_out_path, {'labelCounts': label_counts})
        log.log(f'>> saved segment QC counts: {label_counts}')

    log.log('>> done')


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
