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
import sys

#: Version of the JULIA<->PYTHON PARAMS CONTRACT, mirroring `PY_CONTRACT_VERSION` in
#: app/src/py_runner.jl. Checked in `script_params`, so every runner is covered without doing anything.
#:
#: The asymmetry this exists for: a runner is spawned FRESH from disk every run, while the Julia process
#: that builds its params can be stale — `app/src` is Revise-tracked and a branch switch or a merge under
#: a live server does not always reload it. So Python can be a version ahead of the Julia calling it,
#: with nothing to say so. Observed once as `invalid literal for int() with base 10: 'CH3'` after a param
#: rename landed on disk while the running backend kept the previous translator.
#:
#: Bump BOTH sides together for a renamed/removed key, a changed type or unit, or a new REQUIRED key —
#: not for an additive optional one. A test asserts the two constants agree.
CONTRACT_VERSION = 1


#: Smallest change in completion worth a line. Every emitted line is a stdout write, a Julia parse and
#: a `task:progress` WS frame to every connected client, so the cost is per line while the value is per
#: VISIBLE change — and a bar cannot show more than ~100 distinct states. At 1% a task emits ~100 lines
#: however finely it counts, which is what lets a task count in its natural unit without the unit
#: deciding the traffic: AF over a 180-frame 4-channel movie counts 900 timepoint-passes and emits 101.
#:
#: **Coalesced at the SINK, not at each call site** — the same rule the frontend's continuous controls
#: follow (`docs/UI.md`). A per-runner throttle would be a different number in every task and a new one
#: to get wrong in the next task; here a runner reports every unit it does and this decides what is
#: worth saying.
PROGRESS_MIN_FRACTION = 0.01


# Every task log line is a stdout write that Julia reads, and Julia strings are UTF-8 — but a Python
# child on Windows gets a cp1252 stdout by default, where writing one non-ASCII character raises
# UnicodeEncodeError. That exception propagates out of `log()` and kills the TASK: an import that had
# already read its file and written its output died on the line announcing it, reported only as
# "[ERROR] Track import failed".
#
# This is not one task's problem. 23 of the 25 task runners contain a non-ASCII log line — em dashes
# in warnings, µm in unit readouts — so each is a crash waiting for that branch to be taken on a
# Windows machine. Fixed once, at the sink, rather than by policing the characters at 25 call sites:
# UTF-8 is what Julia expects on the other end anyway, and `errors='replace'` means even a stream that
# cannot be reconfigured degrades to a visible '?' instead of taking the task down.
def _utf8_stdio():
    for stream in (sys.stdout, sys.stderr):
        try:
            stream.reconfigure(encoding='utf-8', errors='replace')
        except Exception:
            pass          # already wrapped, or not a real stream (captured in tests) — never fatal


_utf8_stdio()


class StdoutLogger:
    """Minimal logger that writes to stdout so Julia can stream every line."""

    def __init__(self):
        self._progress_total = None      # the scale the last emitted line was on
        self._progress_frac = None       # …and how far through it was

    def log(self, msg):
        try:
            print(str(msg), flush=True)
        except UnicodeEncodeError:
            # `_utf8_stdio` normally makes this unreachable; belt and braces because the cost of
            # being wrong is the whole task, not the line. A '?' in a log beats a dead import.
            print(str(msg).encode('ascii', 'replace').decode('ascii'), flush=True)

    def progress(self, n: int, total: int):
        """Emit a structured progress line that Julia parses into a task:progress WS message.

        Coalesced to `PROGRESS_MIN_FRACTION` of the total. The two ends are ALWAYS emitted: the first
        line is what sizes the bar, and the last is what completes it — dropping either would be
        visible, where dropping an intermediate is by definition not.
        """
        n, total = int(n), int(total)
        if total <= 0:
            return
        frac = n / total
        ends = n <= 0 or n >= total
        # a changed total is a new scale, not a step along the old one
        same_scale = self._progress_total == total and self._progress_frac is not None
        if not ends and same_scale and abs(frac - self._progress_frac) < PROGRESS_MIN_FRACTION:
            return
        self._progress_total, self._progress_frac = total, frac
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


def check_contract_version(env=None):
    """Refuse a params file written by a backend running older code than this runner.

    `run_py` passes `CECELIA_PY_CONTRACT`; an ABSENT variable means "not launched by run_py" — a
    developer replaying a saved params file by hand, or an external caller — and is deliberately allowed
    through rather than treated as a failure. Only a PRESENT-and-different value is fatal, because that
    can only mean the two halves disagree about the params' shape.

    Raises `SystemExit` rather than `ValueError`: this is a launch precondition, not a data problem, and
    a runner has no way to recover from it. The message names the fix, because the fix is not obvious
    from anything the params contain.
    """
    want = (env if env is not None else os.environ).get('CECELIA_PY_CONTRACT')
    if want in (None, ''):
        return None
    try:
        want_i = int(want)
    except (TypeError, ValueError):
        return None                     # unparseable → treat as absent, never fail a run over the guard
    if want_i != CONTRACT_VERSION:
        raise SystemExit(
            f'[ERROR] Params contract mismatch: the backend sent version {want_i}, this runner speaks '
            f'{CONTRACT_VERSION}. The Julia and Python halves are running different code — almost '
            f'always a backend that was already running when the branch changed, because app/src is '
            f'Revise-tracked and does not always reload. Restart the backend.')
    return want_i


def script_params():
    """
    Read and return the JSON params file passed via --params.

    The file is deleted after reading so temp files don't accumulate.
    Returns None if --params is not provided or the file does not exist.

    Also verifies the params CONTRACT version (`check_contract_version`) — here rather than in each
    runner, because this is the one function every runner already calls, so the guard cannot be
    forgotten by a new one.
    """
    check_contract_version()

    cli = argparse.ArgumentParser()
    cli.add_argument('--params', type=str, default=None)
    args = cli.parse_args()

    if args.params is None or not os.path.exists(args.params):
        return None

    with open(args.params, 'r', encoding='utf-8') as f:
        params = json.load(f)

    os.remove(args.params)
    return params
