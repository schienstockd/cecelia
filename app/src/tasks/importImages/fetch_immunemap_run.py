"""Download one Immunemap video and check it against Immunemap's own metadata.

Called by the ``importImages.immunemapFetch`` task (step 1 of ``importImages.immunemap``). Resumable:
a ``.partial`` survives a failed run and the next run continues it. Verified: the file is renamed into
place only when its size and md5 match ``original_file``. Then the file's own voxel size is compared
with Immunemap's ``spacing`` — the conversion uses the file's, so a disagreement is reported, not fixed.

Params: ``videoUid``, ``destPath``, ``sizeBytes``, ``md5``, ``spacingXYZ`` (Immunemap's, µm),
``resultPath``.
"""
import cecelia.utils.script_utils as script_utils
from cecelia.utils.atomic_io import write_json_atomic
from cecelia.utils.immunemap import download, spacing_mismatch, video_url
from cecelia.utils.ims_meta import voxel_size

def run(params):
    log = script_utils.get_logfile_utils(params)
    uid, dest = params['videoUid'], params['destPath']
    size, md5 = params.get('sizeBytes'), params.get('md5')
    url = video_url(uid)
    log.log(f'>> {url} → {dest} ({(size or 0) / 1e6:.0f} MB)')

    last = [-1]

    def progress(done, total):
        if total:
            pct = int(100 * done / total)
            if pct != last[0]:
                last[0] = pct
                log.log(f'[PROGRESS] {pct}/100')

    got_md5 = download(url, dest, size=size, md5=md5, on_progress=progress)
    log.log(f'>> verified md5 {got_md5}')

    vox = voxel_size(dest)
    file_xyz = vox.get('voxelSizeXYZ')
    ref_xyz = params.get('spacingXYZ') or []
    bad = spacing_mismatch(file_xyz, ref_xyz)
    if file_xyz is None:
        log.log(f'[WARNING] could not read the voxel size from the file: {vox.get("reason")}')
    elif bad:
        log.log(f'[WARNING] voxel size differs on {",".join(bad)}: file {file_xyz} µm, '
                f'Immunemap {ref_xyz} µm — the import uses the file\'s')
    write_json_atomic(params['resultPath'], {
        'path': dest, 'md5': got_md5, 'md5Verified': md5 is not None,
        'fileVoxelSizeXYZ': file_xyz, 'immunemapSpacingXYZ': ref_xyz, 'spacingMismatch': bad,
    })


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
