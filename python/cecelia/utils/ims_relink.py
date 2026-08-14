"""Repair Imaris `.ims` files whose dataset sits behind an HDF5 soft link.

Imaris 11 writes "merged"/workflow files with the real data under `/Workflows/InitialImages/DataSet`
and only a **soft link** at `/DataSet`. Bio-Formats reads Imaris through netcdf-java, whose HDF5
reader does not follow symbolic links of any kind — so it enumerates zero variables under `/DataSet`,
`getArray` returns null for the sample block, and the import dies on:

    loci.formats.FormatException: Unknown pixel type: null
        at loci.formats.in.ImarisHDFReader.initFile(ImarisHDFReader.java:339)

(External links fail identically, so a sidecar file pointing into the original is not a way out.)

The repair replaces each root soft link with a **hard link** to the same object. That is a link-table
edit: no pixel data is read or rewritten, and it costs ~1 ms on a 6 GB file because the cost is
independent of size. Both names then resolve to one group — `/Workflows` keeps its copy of the
provenance, and every reader that could already open the file still can.

Size effect: a handful of link entries, so at most a few hundred bytes. HDF5 reuses the freed entries
when the file has a free block to reuse (the three 5–6 GB files this was written for measured a delta
of exactly zero) and appends when it doesn't, which a freshly-created file usually can't.

Registered as the `ims-softlink` data patch in app/src/maintenance.jl (Settings → Data patches), which
is also the safety model: dry-run lists every file and what would change before anything is written.

    pixi run python -m cecelia.utils.ims_relink <project_root>           # dry-run
    pixi run python -m cecelia.utils.ims_relink <project_root> --apply   # repair
"""
import argparse
import glob
import json
import os
import sys

import cecelia.utils.script_utils as script_utils

#: Root entries Imaris links out to a workflow group. Detection never keys off this list — it finds
#: soft links structurally — but the names are what a reader expects to find at the root, so a file
#: with none of them is reported as "not an Imaris layout" rather than silently repaired.
IMARIS_ROOT_ENTRIES = ('DataSet', 'DataSetInfo')


def _h5py():
    """Imported lazily: `cecelia.utils` must stay importable for consumers with no h5py (the IO tier
    does not depend on it), and this patch is the only thing here that needs it."""
    import h5py
    return h5py


def source_paths(root):
    """Every distinct source image the project at `root` was built from, in a stable order.

    Sources live OUTSIDE the project (the project stores converted zarr), so they are found via the
    `meta.ori_path` each image records in `{root}/1/{uid}/ccid.json` — an image whose import failed
    still has one, which is the case this patch exists for.
    """
    paths = []
    for f in sorted(glob.glob(os.path.join(root, '1', '*', 'ccid.json'))):
        try:
            with open(f, encoding='utf-8') as fh:
                raw = json.load(fh)
        except (OSError, ValueError):
            continue                      # unreadable ccid.json is the store sweep's business, not ours
        p = (raw.get('meta') or {}).get('ori_path')
        if p and p not in paths:
            paths.append(p)
    return paths


def inspect(path):
    """Classify one source file: `{path, state, links, detail}`.

    `state` is one of:
      * `repairable`  — root soft links resolving inside the file; `links` maps name -> target
      * `ok`          — an Imaris file whose root entries are already direct
      * `missing`     — the recorded source is not on disk (moved, or an unmounted share)
      * `not-imaris`  — readable HDF5, but no Imaris root entries
      * `unreadable`  — not HDF5, or refused to open
    """
    h5py = _h5py()
    if not os.path.isfile(path):
        return {'path': path, 'state': 'missing', 'links': {}, 'detail': 'file not found'}
    if not path.lower().endswith('.ims'):
        return {'path': path, 'state': 'not-imaris', 'links': {}, 'detail': 'not an .ims file'}
    try:
        with h5py.File(path, 'r') as f:
            names = list(f)
            soft = {}
            for k in names:
                link = f.get(k, getlink=True)
                if not isinstance(link, h5py.SoftLink):
                    continue
                # Only a link whose target actually resolves in THIS file is repairable — a dangling
                # one is a different (and unfixable) problem, and must not be reported as repairable.
                try:
                    f[link.path]
                except KeyError:
                    return {'path': path, 'state': 'unreadable', 'links': {},
                            'detail': f'/{k} points at {link.path}, which does not exist in the file'}
                soft[k] = link.path
    except OSError as e:
        return {'path': path, 'state': 'unreadable', 'links': {}, 'detail': str(e)}

    if not any(k in names for k in IMARIS_ROOT_ENTRIES):
        return {'path': path, 'state': 'not-imaris', 'links': {},
                'detail': 'no /DataSet or /DataSetInfo at the root'}
    if not soft:
        return {'path': path, 'state': 'ok', 'links': {}, 'detail': 'root entries are already direct'}
    return {'path': path, 'state': 'repairable', 'links': soft,
            'detail': 'Bio-Formats cannot follow these and reports "Unknown pixel type: null"'}


def relink(path, links):
    """Replace the named root soft links with hard links to the same objects. Returns the byte delta.

    Opened `r+`: the only writes are link-table entries, so the pixel data is never touched. The delta
    is bounded by the size of those entries — see the module docstring on when HDF5 reuses them.
    """
    h5py = _h5py()
    before = os.path.getsize(path)
    with h5py.File(path, 'r+') as f:
        for name, target in links.items():
            obj = f[target]               # resolve BEFORE deleting the link that names it
            del f[name]
            f[name] = obj                 # hard link — same object, second name
    return os.path.getsize(path) - before


#: Where an Imaris file keeps its first data block. Only used to prove a repair by reading through it.
_FIRST_BLOCK = 'DataSet/ResolutionLevel 0/TimePoint 0/Channel 0/Data'


def verify(path, names):
    """Prove the links just written actually resolve. Returns a one-line description, or raises.

    A link edit that left the file openable but the data unreachable would otherwise only surface at
    the next import — hours later, as the same opaque Bio-Formats error. So every repaired name is
    resolved, and when the file has the standard first data block, a real block is read through it.

    The read is best-effort by design: `_FIRST_BLOCK` is where Imaris puts the data, but a file that
    resolves every link and simply doesn't have that path is repaired, not broken. Raising there would
    report a successful link edit as a failure.
    """
    h5py = _h5py()
    with h5py.File(path, 'r') as f:
        for name in names:
            f[name]                              # raises KeyError if the new link is dangling
        if _FIRST_BLOCK not in f:
            return f'{len(names)} link(s) resolve'
        d = f[_FIRST_BLOCK]
        if d.ndim and all(d.shape):
            _ = d[(0,) * d.ndim]     # one element, any rank — proves the chunk decodes
        return f'/{_FIRST_BLOCK.split("/")[0]} now reads {d.shape} {d.dtype}'


def patch(root, apply=False, log=print):
    """Report (and with `apply`, repair) every soft-linked Imaris source of the project at `root`.

    Returns `(repaired, failed, skipped)`.
    """
    sources = source_paths(root)
    if not sources:
        log('No source images registered in this project — nothing to check.')
        log('[PROGRESS] 1/1')
        return 0, 0, 0

    log(f'Checking {len(sources)} source file(s)'
        + ('' if apply else '  [DRY-RUN — nothing written; use Apply to repair]'))
    log(f'[PROGRESS] 0/{len(sources)}')

    repaired = failed = skipped = 0
    for i, src in enumerate(sources):
        info = inspect(src)
        name = os.path.basename(src)
        if info['state'] != 'repairable':
            skipped += 1
            # `ok` is the common case once a project has been patched; keep it to one line.
            log(f'  {info["state"]}: {name}' + (f'  ({info["detail"]})' if info['state'] != 'ok' else ''))
        elif not apply:
            repaired += 1
            # Name every link and where it points: this line is the whole basis for deciding to let
            # the patch write to a raw acquisition file, so it must not summarise away a surprise.
            # `rpartition`, not `os.path.dirname`: these are HDF5 INTERNAL paths, which are POSIX-style
            # on every platform. Routing them through the OS path module works today only because
            # ntpath also accepts `/`, and it invites a `\` to appear in a group name later.
            targets = sorted({t.rpartition('/')[0] or '/' for t in info['links'].values()})
            log(f'  would repair: {name}')
            log(f'      - {", ".join("/" + k for k in info["links"])} are soft links into '
                f'{", ".join(targets)}')
        elif not os.access(src, os.W_OK):
            failed += 1
            log(f'  !! {name}: no write permission — copy it somewhere writable, or re-export it')
        else:
            try:
                delta = relink(src, info['links'])
                proof = verify(src, info['links'])
                repaired += 1
                log(f'  repaired: {name}  ({len(info["links"])} links, {delta:+d} bytes)')
                log(f'      - {proof}')
            except Exception as e:
                # Broad on purpose: this loop writes to the user's raw data, and one file that fails
                # in an unforeseen way must be reported and stepped over, not abort the run partway
                # and leave the rest of the project unexamined.
                failed += 1
                log(f'  !! {name}: ERROR {type(e).__name__}: {e}')
        log(f'[PROGRESS] {i + 1}/{len(sources)}')

    verb = 'Repaired' if apply else 'Would repair'
    log(f'Done. {verb} {repaired} file(s)'
        + (f'; {failed} failed' if failed else '')
        + (f'; {skipped} needed no change.' if skipped else '.'))
    if repaired and apply:
        log('Re-run the import for these images.')
    return repaired, failed, skipped


# ── run_py entry (Settings → Data patches) ────────────────────────────────────

def run(params):
    root = script_utils.get_param(params, 'root', default=None)
    apply = bool(script_utils.get_param(params, 'apply', default=False))
    if not root:
        print('[ERROR] no root project directory in params', flush=True)
        return
    patch(root, apply=apply, log=lambda m: print(m, flush=True))


def main():
    # Dispatch on the flag, not by trying `script_params()` first — it runs its own argparse over the
    # whole command line and would reject the CLI's positional `root`. (Same as store_sweep.)
    if '--params' in sys.argv:                   # invoked by run_py
        params = script_utils.script_params()
        if params is None:
            print('[ERROR] no --params file', flush=True)
            return
        run(params)
        return
    ap = argparse.ArgumentParser(
        description='Repair Imaris .ims sources whose dataset sits behind an HDF5 soft link.')
    ap.add_argument('root', help='a project root')
    ap.add_argument('--apply', action='store_true', help='actually repair (default: dry-run)')
    a = ap.parse_args()
    if not os.path.isdir(a.root):
        print(f'[ERROR] not a directory: {a.root}', flush=True)
        sys.exit(1)
    patch(a.root, apply=a.apply)


if __name__ == '__main__':
    main()
