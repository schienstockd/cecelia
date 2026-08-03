"""
Script parameter utilities for feijoa Python tasks.

Tasks are invoked by Julia as subprocesses.  Julia writes a JSON params file,
passes its path via --params, and the script deletes it after reading.

Differs from the original cecelia script_utils in two ways:
  - Logging goes to stdout (Julia captures and streams it via the WS log).
  - Parameters arrive already flat (no R list-wrapping), so no flatten step.
"""

import argparse
import json
import os


class StdoutLogger:
    """Minimal logger that writes to stdout so Julia can stream every line."""

    def log(self, msg):
        print(str(msg), flush=True)

    def progress(self, n: int, total: int):
        """Emit a structured progress line that Julia parses into a task:progress WS message."""
        print(f'[PROGRESS] {n}/{total}', flush=True)


def get_logfile_utils(params):
    """Return a StdoutLogger regardless of params content."""
    return StdoutLogger()


def get_param(params, key, default=None):
    """
    Retrieve a value from params by key, falling back to default.

    If default is a list and the stored value is a scalar, wraps it in a list
    so callers can always iterate safely.
    """
    v = params[key] if key in params else default
    if type(default) is list and v is not None:
        v = v if isinstance(v, list) else [v]
    return v


def channel_indices(channels, what='channels', translator=None):
    """0-based channel indices from a params value, or a ValueError that says what actually went wrong.

    **Every task that takes channels takes them as INDICES.** The Julia side resolves the channel NAMES
    the frontend stores into indices before the params reach Python — `af_combinations_for_python`
    (af_correct.jl), `cellpose_models_for_python` (cellpose.jl), and inline in drift_correct.jl /
    branching.jl. Python then indexes the channel axis with an integer.

    When that translation has not happened, the raw failure is
    ``ValueError: invalid literal for int() with base 10: 'CH3'`` from somewhere deep in a streaming loop,
    which says nothing about the cause — and the cause is nearly always a **backend running older Julia
    than the Python it is calling**: `app/src` is Revise-tracked and a branch switch or a merge under a
    live server does not always reload it. Observed exactly that way on a worktree whose files carried a
    param rename while its running process still had the previous translator compiled. The tell was a
    stale line in the log; nothing said "your backend is out of date".

    So: name the offender, name the translator that should have run, and name the fix. Pass `translator`
    so the message points at the right one.

    Deliberately no stricter than the bare `int(c)` these call sites used: a digit string still converts,
    so a REPL or chain caller passing ``["0", "1"]`` keeps working. Only a value `int()` cannot read is
    refused — plus `bool`, an `int` subclass that would otherwise index channel 0/1 in silence.
    """
    out = []
    for c in (channels or []):
        if isinstance(c, bool):
            raise ValueError(f'{what} contains {c!r}; a bool would silently index channel {int(c)}')
        try:
            out.append(int(c))
        except (TypeError, ValueError):
            raise ValueError(
                f'{what} contains {c!r}, which is a channel NAME, not a 0-based index. '
                f'{translator or "The Julia param translator"} resolves names before the params get '
                f'here, so it did not run — if the backend was already running when this branch '
                f'changed, restart it (Revise does not always reload app/src).') from None
    return out


def channel_index(channel, what='channel', translator=None, default=0):
    """One 0-based channel index, for a `channelSelection` with `multiple=false`.

    Those still arrive as a one-element list (that is how the widget stores them), so a bare scalar and
    a list are both accepted. Empty/None → `default`. Same diagnosis as `channel_indices`.
    """
    if channel is None:
        return default
    values = channel if isinstance(channel, (list, tuple)) else [channel]
    idx = channel_indices(values, what, translator)
    return idx[0] if idx else default


def get_ccia_params(params):
    """Return the 'ccia' sub-dict of params, or an empty list if absent."""
    if 'ccia' in params:
        return params['ccia']
    return list()


def script_params():
    """
    Read and return the JSON params file passed via --params.

    The file is deleted after reading so temp files don't accumulate.
    Returns None if --params is not provided or the file does not exist.
    """
    cli = argparse.ArgumentParser()
    cli.add_argument('--params', type=str, default=None)
    args = cli.parse_args()

    if args.params is None or not os.path.exists(args.params):
        return None

    with open(args.params, 'r', encoding='utf-8') as f:
        params = json.load(f)

    os.remove(args.params)
    return params
