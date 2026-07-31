"""Sweep leftover staging stores — the debris a killed task leaves behind.

Background: docs/SEGMENTATION.md → *Stores are written staged, never in place*.

Store writes go through `zarr_utils.staged_store`: data is streamed into a `*.partial` sibling and
renamed onto the real path only once complete. Cancelling a task is a SIGTERM/SIGKILL from the
scheduler, which runs no cleanup — deliberately, because the alternative is deleting the user's
previous store to tidy up. The staging dir is therefore *designed* to survive, and it is invisible:
nothing in `ccid.json` mentions it, so it never shows up in the UI and never gets counted.

This is the broom. Dry-run lists what it found and how big it is; Apply deletes. Registered as the
`store-debris` data patch in app/src/maintenance.jl (Settings → Data patches), which is also the
safety model: the user sees the list before anything is removed.

Suffixes come from `zarr_utils`, the module that creates them, so the sweep cannot drift from the
writer.

    pixi run python -m cecelia.utils.store_sweep <project_root>           # dry-run
    pixi run python -m cecelia.utils.store_sweep <project_root> --apply   # delete
"""
import argparse
import json
import os
import shutil
import sys
import time

import cecelia.utils.script_utils as script_utils
from cecelia.utils.zarr_utils import STAGING_SUFFIX, SUPERSEDED_SUFFIX

_SUFFIXES = (STAGING_SUFFIX, SUPERSEDED_SUFFIX)

# ── Why name-matching alone is not enough ─────────────────────────────────────
#
# `*.partial` / `*.superseded` only ever catches writers that opted into `staged_store`. AF, drift,
# cellpose-correct, crop and rescale all do. **Import does not**: on the 16-bit path bioformats2raw
# writes straight to the FINAL name (`bf2raw_out == zarr_out`), so a cancel leaves a half-written store
# at `ccidImage.ome.zarr` — a name the sweep actively skipped as "a real store". It also leaves
# `ccidImage.16bit.tmp.ome.zarr` and `_stage_src` (a full local copy of the source, often the largest
# item of the three). None contain "partial".
#
# So the detection is STRUCTURAL, because a name list only ever covers the writers someone remembered:
#
#   1. UNREGISTERED — a store sitting in a store location that `ccid.json` does not name. A cancelled
#      import is unregistered by construction: registration is the last thing a successful run does.
#   2. INCOMPLETE — `.zattrs` declares levels 0..N and fewer exist on disk. Catches a truncated store
#      even at a registered path (the `KeyError: '1'` case).
#
# Name-matching stays as the fast path, and every check is advisory: dry-run first, and the
# active-window guard means a store a running task is writing is never removed.

# Directories under an image that hold STORES, and which ccid.json field registers them. Scoping the
# orphan check to these is essential — `data/`, `qc/`, `gating/`, `labelProps/` are legitimately
# unregistered, and treating them as debris would delete the user's analysis.
_STORE_LOCATIONS = {
    '0':            'filepath',        # {proj}/0/{uid}/          image versions
    'labels':       'labels',          # {proj}/1/{uid}/labels/
    'branchLabels': 'branch_labels',   # {proj}/1/{uid}/branchLabels/
}

# Import scratch that is not a store and not suffixed, so neither check above would see it.
_IMPORT_SCRATCH = ('_stage_src',)

# A store whose contents changed this recently is treated as belonging to a RUNNING task and left
# alone on apply. A streaming write creates one chunk file after another, so the level-0 directory's
# mtime keeps moving for as long as the run does. This is a guard rail, not a guarantee — a task
# stalled longer than this (a very slow frame, a paused GPU) would look like debris, which is why
# the patch reports every skip and asks you to run it with nothing in flight.
ACTIVE_WINDOW_S = 300


def _dir_bytes(path):
    total = 0
    for dirpath, _, filenames in os.walk(path):
        for fn in filenames:
            try:
                total += os.path.getsize(os.path.join(dirpath, fn))
            except OSError:                      # vanished mid-walk — not our business
                pass
    return total


def _last_touched(path):
    """Newest mtime among the store dir and its level-0 dir — the cheap liveness signal for a write
    in progress. Walking the whole tree would mean statting every chunk of a multi-GB store."""
    newest = 0.0
    for p in (path, os.path.join(path, '0')):
        try:
            newest = max(newest, os.path.getmtime(p))
        except OSError:
            pass
    return newest


def _registered_names(meta_dir, field):
    """Store basenames `ccid.json` registers under `field`. Empty set when unreadable — which makes the
    orphan check REPORT rather than delete on a missing/corrupt ccid.json, so a caller still has to
    look before applying."""
    try:
        with open(os.path.join(meta_dir, 'ccid.json'), encoding='utf-8') as f:
            raw = json.load(f)
    except (OSError, ValueError):
        return None                    # None = "cannot tell", distinct from "nothing registered"
    val = raw.get(field)
    if val is None:
        return set()
    names = set()
    if isinstance(val, dict):
        for k, v in val.items():
            if k == '_active':                       # the versioned-field marker, not a value
                continue
            if isinstance(v, str):
                names.add(v)                         # filepath: {vn -> "x.ome.zarr"}
            elif isinstance(v, (list, tuple)):
                names.update(str(x) for x in v)       # labels: {vn -> ["x.zarr", "x_nuc.zarr"]}
    elif isinstance(val, (list, tuple)):
        names.update(str(x) for x in val)
    return names


def _incomplete_levels(path):
    """`(declared, present)` pyramid level counts, or None when it can't be judged.

    A store's `.zattrs` lists every level at CREATION; a streaming writer fills level 0 and builds the
    rest at the end. So declared > present means the writer never finished — the `KeyError: '1'` a
    consumer hits when it asks for the level count the metadata promised.
    """
    try:
        from cecelia.utils.zarr_utils import read_multiscales_meta
        ms = read_multiscales_meta(path)
    except Exception:
        return None
    datasets = (ms or {}).get('datasets') or []
    if len(datasets) <= 1:
        return None                    # single-level store: nothing is promised, nothing missing
    base = path
    if os.path.isdir(os.path.join(path, '0')) and not os.path.isdir(os.path.join(path, '0', '0')):
        base = path                    # flat layout: levels are `0`,`1`,… at the root
    else:
        series = os.path.join(path, '0')
        base = series if os.path.isdir(series) else path
    present = sum(1 for d in datasets
                  if os.path.isdir(os.path.join(base, str(d.get('path', '')))))
    return (len(datasets), present)


def find_store_debris(root, active_window_s=ACTIVE_WINDOW_S, now=None, structural=True):
    """Everything under `root` that looks like it was left behind, as dicts with path/bytes/active/why.

    `why` is one of:
      * `staging`      — a `*.partial`/`*.superseded` dir (the fast path; a writer that used `staged_store`)
      * `scratch`      — import scratch (`_stage_src`)
      * `unregistered` — a store in a store location that `ccid.json` does not name
      * `incomplete`   — `.zattrs` declares more pyramid levels than exist on disk

    Prunes the walk at each hit so this doesn't descend into pyramids. `structural=False` restores the
    old name-only behaviour (used by the tests to show what name-matching alone misses).
    """
    now = time.time() if now is None else now
    found = []

    def _hit(full, why, extra=''):
        found.append({
            'path': full,
            'bytes': _dir_bytes(full),
            'active': (now - _last_touched(full)) < active_window_s,
            'why': why,
            'detail': extra,
        })

    for dirpath, dirnames, _ in os.walk(root):
        # Which ccid.json governs stores in THIS directory, if any. `{proj}/0/{uid}` holds image
        # versions but the metadata lives in `{proj}/1/{uid}`; labels/branchLabels sit beside it.
        registered = None
        if structural:
            name   = os.path.basename(dirpath)
            parent = os.path.dirname(dirpath)
            if os.path.basename(parent) == '0':
                # {proj}/0/{uid} holds the image versions; its metadata lives at {proj}/1/{uid}
                meta = os.path.join(os.path.dirname(parent), '1', name)
                registered = _registered_names(meta, 'filepath')
            elif name in ('labels', 'branchLabels'):
                registered = _registered_names(parent, _STORE_LOCATIONS[name])

        keep = []
        for d in sorted(dirnames):
            full = os.path.join(dirpath, d)
            if d.endswith(_SUFFIXES):
                _hit(full, 'staging')
                continue                          # never descend into debris
            if structural and d in _IMPORT_SCRATCH:
                _hit(full, 'scratch', 'a local copy of the import source')
                continue
            if d.endswith('.zarr'):
                # A real store — but "real" used to be assumed from the name alone, which is exactly
                # how a cancelled import (half-written, at the FINAL name) stayed invisible.
                if structural and registered is not None and d not in registered:
                    _hit(full, 'unregistered', 'no ccid.json entry names this store')
                elif structural:
                    lv = _incomplete_levels(full)
                    if lv is not None and lv[1] < lv[0]:
                        _hit(full, 'incomplete',
                             f'.zattrs declares {lv[0]} pyramid levels, {lv[1]} on disk')
                continue                          # never descend into a store either way
            keep.append(d)
        dirnames[:] = keep
    return found


def sweep(root, apply=False, active_window_s=ACTIVE_WINDOW_S, log=print):
    """Report (and with `apply`, delete) the debris under `root`. Returns (removed, skipped, bytes)."""
    debris = find_store_debris(root, active_window_s=active_window_s)
    by_why = {}
    for d in debris:
        by_why[d['why']] = by_why.get(d['why'], 0) + 1
    kinds = ', '.join(f'{n} {w}' for w, n in sorted(by_why.items()))
    log(f'Found {len(debris)} leftover item(s)' + (f' ({kinds})' if kinds else '')
        + ('' if apply else '  [DRY-RUN — nothing deleted; use Apply to remove]'))
    log(f'[PROGRESS] 0/{max(len(debris), 1)}')

    removed = skipped = freed = 0
    for i, d in enumerate(debris):
        mb = d['bytes'] / (1024 * 1024)
        why = f'[{d["why"]}]'
        if d['active']:
            skipped += 1
            log(f'  skipped {why} ({mb:.1f} MB): {d["path"]}')
            log(f'      - changed in the last {active_window_s // 60} min — a task may be writing it')
        elif apply:
            try:
                shutil.rmtree(d['path'])
                removed += 1
                freed += d['bytes']
                log(f'  removed {why} ({mb:.1f} MB): {d["path"]}')
            except OSError as e:
                skipped += 1
                log(f'  !! {d["path"]}: ERROR {e}')
        else:
            removed += 1
            freed += d['bytes']
            log(f'  would remove {why} ({mb:.1f} MB): {d["path"]}')
            if d['detail']:
                log(f'      - {d["detail"]}')
        log(f'[PROGRESS] {i + 1}/{max(len(debris), 1)}')

    verb = 'Removed' if apply else 'Would remove'
    log(f'Done. {verb} {removed} store(s), {freed / (1024 * 1024):.1f} MB'
        + (f'; skipped {skipped} still in use.' if skipped else '.'))
    return removed, skipped, freed


# ── run_py entry (Settings → Data patches) ────────────────────────────────────
# Entry point and logic share a file: unlike the centroid patch there is no owning task category to
# co-locate a `*_run.py` with, and the whole sweep is short.

def summarise(root, active_window_s=ACTIVE_WINDOW_S):
    """Counts + bytes of what a sweep WOULD remove, without deleting or logging anything.

    Exists so the Settings storage box can announce leftover bytes using the SAME detector the patch
    uses. A second, Julia-side name-based count would be a duplicate implementation of the thing this
    module just stopped relying on — and it would under-report exactly the cases that matter (a
    cancelled import writes to the final name).
    """
    debris = find_store_debris(root, active_window_s=active_window_s)
    inactive = [d for d in debris if not d['active']]
    by_why = {}
    for d in inactive:
        by_why[d['why']] = by_why.get(d['why'], 0) + 1
    return {
        'count': len(inactive),
        'bytes': sum(d['bytes'] for d in inactive),
        'activeSkipped': len(debris) - len(inactive),
        'byWhy': by_why,
    }


def run(params):
    root = script_utils.get_param(params, 'root', default=None)
    apply = bool(script_utils.get_param(params, 'apply', default=False))
    # report-only mode: write the totals to `resultPath` and delete nothing (the storage box)
    result_path = script_utils.get_param(params, 'resultPath', default=None)
    if not root:
        print('[ERROR] no root project directory in params', flush=True)
        return
    if result_path:
        from cecelia.utils.atomic_io import write_json_atomic
        write_json_atomic(result_path, summarise(root))
        return
    sweep(root, apply=apply, log=lambda m: print(m, flush=True))


def main():
    # Dispatch on the flag, NOT by trying `script_params()` first: it runs its own argparse over the
    # whole command line, so it would reject the CLI's positional `root` and exit before we got here.
    if '--params' in sys.argv:                   # invoked by run_py
        params = script_utils.script_params()
        if params is None:
            print('[ERROR] no --params file', flush=True)
            return
        run(params)
        return
    ap = argparse.ArgumentParser(description='Delete leftover stores: staging debris, import scratch, unregistered and incomplete stores.')
    ap.add_argument('root', help='a project root (or any directory to scan)')
    ap.add_argument('--apply', action='store_true', help='actually delete (default: dry-run)')
    a = ap.parse_args()
    if not os.path.isdir(a.root):
        print(f'[ERROR] not a directory: {a.root}', flush=True)
        sys.exit(1)
    sweep(a.root, apply=a.apply)


if __name__ == '__main__':
    main()
