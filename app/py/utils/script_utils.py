"""
Script parameter utilities for pineapple Python tasks.

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


def get_ccia_param(params, key, default=None):
    """Retrieve a value from the nested 'ccia' sub-dict of params."""
    return get_param(get_ccia_params(params), key, default=default)


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

    with open(args.params, 'r') as f:
        params = json.load(f)

    os.remove(args.params)
    return params
