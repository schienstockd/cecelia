"""Reaching the app on loopback whichever scheme it serves — the ONE copy of this rule, for every
Python client of the API: the launcher (`app.py`), the MCP (`cecelia_mcp`), `CeceliaClient` and the
agent_eval scripts. Julia clients have their own (`api/task_console.jl`, `api/dev.jl`).

The server picks HTTP or HTTPS itself (`tls_desired`: installed apps serve HTTPS with a self-signed
cert, dev serves HTTP, and TLS can be toggled in Settings), so a URL a client was given can name the
wrong scheme. For a LOOPBACK base URL only (127.0.0.1 / localhost / ::1):

- HTTPS skips certificate verification. The cert is self-signed by design, so verification would
  always fail. Loopback traffic never leaves the machine.
- On a connection-level failure the caller retries once with the other scheme and `remember`s the one
  that worked, so later calls go straight there.

Any other host is untouched: verified with the default context, never swapped. Stdlib only.
"""
from __future__ import annotations

import ssl
import urllib.error
import urllib.parse
import urllib.request

_LOOPBACK_HOSTS = frozenset({"127.0.0.1", "localhost", "::1"})
_SWAP = {"http": "https", "https": "http", "ws": "wss", "wss": "ws"}
_NOVERIFY = ssl._create_unverified_context()

# configured base URL → the one that last worked (only differs after a scheme swap)
_WORKING: dict[str, str] = {}


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
