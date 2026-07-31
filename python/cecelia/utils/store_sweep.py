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
import os
import shutil
import sys
import time

import cecelia.utils.script_utils as script_utils
from cecelia.utils.zarr_utils import STAGING_SUFFIX, SUPERSEDED_SUFFIX

_SUFFIXES = (STAGING_SUFFIX, SUPERSEDED_SUFFIX)

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


def find_store_debris(root, active_window_s=ACTIVE_WINDOW_S, now=None):
    """Every staging/superseded store dir under `root`, as dicts with path/size/active.

    Prunes the walk at each hit — and at every real store — so this doesn't descend into pyramids.
    """
    now = time.time() if now is None else now
    found = []
    for dirpath, dirnames, _ in os.walk(root):
        keep = []
        for d in sorted(dirnames):
            full = os.path.join(dirpath, d)
            if d.endswith(_SUFFIXES):
                found.append({
                    'path': full,
                    'bytes': _dir_bytes(full),
                    'active': (now - _last_touched(full)) < active_window_s,
                })
                continue                          # never descend into debris
            if d.endswith('.zarr'):
                continue                          # a real store holds no debris — don't walk it
            keep.append(d)
        dirnames[:] = keep
    return found


def sweep(root, apply=False, active_window_s=ACTIVE_WINDOW_S, log=print):
    """Report (and with `apply`, delete) the debris under `root`. Returns (removed, skipped, bytes)."""
    debris = find_store_debris(root, active_window_s=active_window_s)
    log(f'Found {len(debris)} leftover staging store(s)'
        + ('' if apply else '  [DRY-RUN — nothing deleted; use Apply to remove]'))
    log(f'[PROGRESS] 0/{max(len(debris), 1)}')

    removed = skipped = freed = 0
    for i, d in enumerate(debris):
        mb = d['bytes'] / (1024 * 1024)
        if d['active']:
            skipped += 1
            log(f'  skipped ({mb:.1f} MB): {d["path"]}')
            log(f'      - changed in the last {active_window_s // 60} min — a task may be writing it')
        elif apply:
            try:
                shutil.rmtree(d['path'])
                removed += 1
                freed += d['bytes']
                log(f'  removed ({mb:.1f} MB): {d["path"]}')
            except OSError as e:
                skipped += 1
                log(f'  !! {d["path"]}: ERROR {e}')
        else:
            removed += 1
            freed += d['bytes']
            log(f'  would remove ({mb:.1f} MB): {d["path"]}')
        log(f'[PROGRESS] {i + 1}/{max(len(debris), 1)}')

    verb = 'Removed' if apply else 'Would remove'
    log(f'Done. {verb} {removed} store(s), {freed / (1024 * 1024):.1f} MB'
        + (f'; skipped {skipped} still in use.' if skipped else '.'))
    return removed, skipped, freed


# ── run_py entry (Settings → Data patches) ────────────────────────────────────
# Entry point and logic share a file: unlike the centroid patch there is no owning task category to
# co-locate a `*_run.py` with, and the whole sweep is short.

def run(params):
    root = script_utils.get_param(params, 'root', default=None)
    apply = bool(script_utils.get_param(params, 'apply', default=False))
    if not root:
        print('[ERROR] no root project directory in params', flush=True)
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
    ap = argparse.ArgumentParser(description='Delete leftover *.partial / *.superseded stores.')
    ap.add_argument('root', help='a project root (or any directory to scan)')
    ap.add_argument('--apply', action='store_true', help='actually delete (default: dry-run)')
    a = ap.parse_args()
    if not os.path.isdir(a.root):
        print(f'[ERROR] not a directory: {a.root}', flush=True)
        sys.exit(1)
    sweep(a.root, apply=a.apply)


if __name__ == '__main__':
    main()
