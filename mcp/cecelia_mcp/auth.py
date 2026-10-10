"""The API token every Cecelia request needs (the server's gate: app/src/api_token.jl).

Loopback is shared by every account on the machine, so the server only answers a request carrying
`Authorization: Bearer <token>`, where the token is `<config_dir>/api-token` — a file only the user
who launched Cecelia can read. The in-app observer registration passes its path as
`CECELIA_API_TOKEN_FILE` (the path, not the secret, so the secret never lands in Claude's config);
`CECELIA_API_TOKEN` carries the value itself. Without either, the default config dir is tried.
"""
from __future__ import annotations

import os
import pathlib


_REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]


def _default_token_path() -> str:
    """`<config_dir>/api-token`, resolved in `config_dir()`'s order (app/src/config_dir.jl):
    `CECELIA_DEV_DIR` env, then `CECELIA_DEV_DIR` in the checkout's `.env`, then `~/.cecelia`."""
    base = os.environ.get("CECELIA_DEV_DIR")
    if not base:
        try:
            with open(_REPO_ROOT / ".env", encoding="utf-8") as f:
                for line in f:
                    key, sep, val = line.strip().partition("=")
                    if sep and key == "CECELIA_DEV_DIR":
                        base = val.strip()
        except OSError:
            pass
    base = base or os.path.join("~", ".cecelia")
    return os.path.join(os.path.normpath(os.path.expanduser(base)), "api-token")


def api_token() -> str:
    """The token, or "" when none can be read (the request then fails with a 401 that says why)."""
    tok = os.environ.get("CECELIA_API_TOKEN", "").strip()
    if tok:
        return tok
    try:
        with open(os.environ.get("CECELIA_API_TOKEN_FILE") or _default_token_path(),
                  encoding="utf-8") as f:
            return f.read().strip()
    except OSError:
        return ""


def auth_headers() -> dict[str, str]:
    """`{"Authorization": "Bearer <token>"}`, or {} when there is no token."""
    tok = api_token()
    return {"Authorization": f"Bearer {tok}"} if tok else {}
