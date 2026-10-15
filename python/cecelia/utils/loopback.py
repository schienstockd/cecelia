"""Reaching the app on loopback whichever scheme it serves — the ONE copy of this rule, for every
Python client of the API: the launcher (`app.py`), the MCP (`cecelia_mcp`), `CeceliaClient` and the
agent_eval scripts. Julia clients have their own (`api/task_console.jl`, `api/dev.jl`).

The server picks HTTP or HTTPS itself (`tls_desired`: plain HTTP by default, HTTPS with a self-signed
cert when toggled on in Settings or by `CECELIA_TLS=1`), so a URL a client was given can name the
wrong scheme. For a LOOPBACK base URL only (127.0.0.1 / localhost / ::1):

- HTTPS skips certificate verification. The cert is self-signed by design, so verification would
  always fail. Loopback traffic never leaves the machine.
- On a connection-level failure the caller retries once with the other scheme and `remember`s the one
  that worked, so later calls go straight there.

Any other host is untouched: verified with the default context, never swapped. Stdlib only.
"""
from __future__ import annotations

import os
import pathlib
import ssl
import urllib.error
import urllib.parse
import urllib.request

_LOOPBACK_HOSTS = frozenset({"127.0.0.1", "localhost", "::1"})
_SWAP = {"http": "https", "https": "http", "ws": "wss", "wss": "ws"}
_NOVERIFY = ssl._create_unverified_context()

_REPO_ROOT = pathlib.Path(__file__).resolve().parents[3]   # python/cecelia/utils/ → the checkout

# configured base URL → the one that last worked (only differs after a scheme swap)
_WORKING: dict[str, str] = {}


def port_slot() -> int:
    """The slot of this user's running (or most recent) Cecelia — the Python twin of
    `current_port_slot` (app/src/ports.jl): `CECELIA_PORT_SLOT` (a task inherits it), else the sticky
    `<config_dir>/port-slot` the backend writes, else 0. The file is what a plain shell has to go on."""
    env = os.environ.get("CECELIA_PORT_SLOT", "").strip()
    if env:
        return int(env)
    try:
        with open(os.path.join(config_dir(), "port-slot"), encoding="utf-8") as f:
            return int(f.read().strip())
    except (OSError, ValueError):
        return 0


#: Slot-0 port and pinning env var per service — the Python twin of `SERVICE_PORT_BASE` /
#: `SERVICE_PORT_ENV` (app/src/ports.jl); a slot adds `_PORT_SLOT_STRIDE` × slot to every one.
_SERVICE_PORTS = {
    "backend": (8080, "CECELIA_PORT"),
    "frontend": (5173, "CECELIA_FRONTEND_PORT"),
    "preview": (7656, "CECELIA_PREVIEW_PORT"),
    "runner": (7657, "CECELIA_RUNNER_PORT"),
    "notebooks": (7660, "CECELIA_PLUTO_PORT"),
}
_PORT_SLOT_STRIDE = 10


def service_port(service: str) -> int:
    """This user's port for `service` — the Python twin of `service_port(svc; slot =
    current_port_slot())` (app/src/ports.jl): its env var when pinned (the backend pins it for every
    child it launches), else the slot-0 port + 10 × `port_slot()`. So a standalone run (`pixi run
    preview`) binds the port this user's backend probes, not slot 0's."""
    base, env = _SERVICE_PORTS[service]
    pinned = os.environ.get(env, "").strip()
    return int(pinned) if pinned else base + _PORT_SLOT_STRIDE * port_slot()


def default_base_url() -> str:
    """This user's backend: `CECELIA_API_URL` if given, else 127.0.0.1 on this user's port
    (`CECELIA_PORT`, or 8080 + 10 × `port_slot()` — app/src/ports.jl). Never a bare :8080 — on a
    shared machine that is slot 0's backend, another user's. The scheme here is only a first guess;
    `open_url` finds the one the server speaks."""
    url = os.environ.get("CECELIA_API_URL", "").strip()
    if url:
        return url
    return f"http://127.0.0.1:{service_port('backend')}"


def config_dir(root: str | os.PathLike | None = None) -> str:
    """This user's config dir: the Python twin of `config_dir` (app/src/config_dir.jl), same order —
    `CECELIA_DEV_DIR` env, then `CECELIA_DEV_DIR` in `<root>/.env` (default: this checkout), then
    `~/.cecelia` (the installed app has neither, so it lands there)."""
    val = os.environ.get("CECELIA_DEV_DIR")
    if not val:
        try:
            with open(os.path.join(root or _REPO_ROOT, ".env"), encoding="utf-8") as f:
                for line in f:
                    key, sep, rest = line.strip().partition("=")
                    if sep and key == "CECELIA_DEV_DIR":
                        val = rest.strip()
        except OSError:
            pass
    # normpath: `~/x` expands to `C:\Users\u/x` on Windows; Julia's `expand_user` canonicalises it.
    return os.path.normpath(os.path.expanduser(val or os.path.join("~", ".cecelia")))


def api_token(root: str | os.PathLike | None = None) -> str:
    """The API token every request but /api/health must carry (app/src/api_token.jl): `CECELIA_API_TOKEN`,
    else the file `CECELIA_API_TOKEN_FILE` names, else `<config_dir>/api-token` — so a notebook or script
    gets it without being handed it. "" when none can be read (the request then fails with a 401 that
    says why)."""
    tok = os.environ.get("CECELIA_API_TOKEN", "").strip()
    if tok:
        return tok
    try:
        with open(os.environ.get("CECELIA_API_TOKEN_FILE") or os.path.join(config_dir(root), "api-token"),
                  encoding="utf-8") as f:
            return f.read().strip()
    except OSError:
        return ""


def is_loopback(url: str) -> bool:
    return (urllib.parse.urlsplit(url).hostname or "") in _LOOPBACK_HOSTS


def swap_scheme(url: str) -> str:
    scheme, rest = url.split("://", 1)
    return f"{_SWAP.get(scheme, scheme)}://{rest}"


def candidates(url: str) -> list[str]:
    """`url`, then — on loopback only — the same URL with the other scheme."""
    return [url, swap_scheme(url)] if is_loopback(url) else [url]


def ssl_context(url: str) -> ssl.SSLContext | None:
    """The unverified context for HTTPS/WSS on loopback; None (the verifying default) otherwise."""
    secure = urllib.parse.urlsplit(url).scheme in ("https", "wss")
    return _NOVERIFY if secure and is_loopback(url) else None


def resolve(base_url: str) -> str:
    """The base URL to use for `base_url`: the scheme that last worked, else `base_url` itself."""
    return _WORKING.get(base_url, base_url)


def remember(base_url: str, working: str) -> None:
    _WORKING[base_url] = working


def forget() -> None:
    _WORKING.clear()


def open_url(base_url: str, path: str, *, data: bytes | None = None, headers: dict | None = None,
             method: str | None = None, timeout: float = 60.0):
    """`urllib.request.urlopen` on `base_url + path`, the stale-scheme retry included: the remembered
    working scheme first, then (loopback only) the other one on a connection-level failure, remembering
    whichever answered. An HTTP error status or a timeout is an answer, not a scheme problem — raised
    as-is. Nothing reachable → `ConnectionError` naming every URL tried. Returns the open response."""
    errors = []
    for base in candidates(resolve(base_url)):
        req = urllib.request.Request(base + path, data=data, method=method, headers=headers or {})
        ctx = ssl_context(base)
        try:
            resp = (urllib.request.urlopen(req, timeout=timeout, context=ctx) if ctx is not None
                    else urllib.request.urlopen(req, timeout=timeout))
        except urllib.error.HTTPError:
            remember(base_url, base)   # it answered: this IS the scheme, whatever the status
            raise
        except TimeoutError:
            raise
        except (urllib.error.URLError, OSError) as e:  # refused / reset / TLS mismatch
            errors.append(f"{base}: {getattr(e, 'reason', e)}")
            continue
        remember(base_url, base)
        return resp
    raise ConnectionError("; ".join(errors))
