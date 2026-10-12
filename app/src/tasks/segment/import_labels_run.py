"""
Label import task entry point (`segment.importLabels`).

Called by the Julia handler (import_labels.jl) through run_py. Writes each external label image as a
label store for the image — the streaming work is `cecelia.utils.label_import_utils.import_labels`.

Parameter contract (JSON written by Julia):
  imPath          - absolute path to the image .ome.zarr the labels belong to
  sources         - {role: path}; 'base' required (the cell table), other roles from the matchAs
                    vocabulary ('nuc'). Each path is an OME-Zarr store or a TIFF
  outPaths        - {role: path} of the label store to write per role ({vn}.zarr, {vn}_{role}.zarr)
  matchIds        - overlap-match non-base ids to base ids (default false)
  matchThreshold  - IoU a match needs (default 0.3)
  qcOutPath       - JSON written for the QC the handler banks (segment_counts.json)
"""

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.atomic_io import write_json_atomic
from cecelia.utils.label_import_utils import LabelImportError, import_labels


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path = params['imPath']
    log.log(f'>> image: {im_path}')
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=False)
    dim_utils = DimUtils(ome_xml_utils.parse_meta(im_path), use_channel_axis=True)
    dim_utils.calc_image_dimensions(im_dat[0].shape)
    log.log(f'>> dims: {dim_utils.im_dim_order} {dim_utils.im_dim}  levels: {len(im_dat)}')

    try:
        qc = import_labels(
            dict(params['sources']), dict(params['outPaths']), im_path,
            tuple(im_dat[0].shape), len(im_dat), dim_utils, log,
            match_ids=bool(params.get('matchIds', False)),
            match_threshold=float(params.get('matchThreshold', 0.3)))
    except LabelImportError as e:
        log.log(f'[ERROR] {e}')
        raise SystemExit(1)

    if params.get('qcOutPath'):
        write_json_atomic(params['qcOutPath'], qc)
    log.log(f'>> imported: {qc["labelCounts"]}')
    log.log('>> done')


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
