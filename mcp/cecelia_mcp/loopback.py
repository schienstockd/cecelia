"""Reaching the app on loopback whichever scheme it serves.

The registered `CECELIA_API_URL` can name the wrong scheme: installed apps serve HTTPS with a
self-signed cert, dev serves HTTP, and TLS can be toggled in Settings after the MCP was set up. So for
a LOOPBACK base URL only (127.0.0.1 / localhost / ::1):

- HTTPS skips certificate verification. The cert is self-signed by design, so verification would
  always fail; the launcher does the same (`app.py` → `_NOVERIFY`). Loopback traffic never leaves the
  machine.
- On a connection-level failure the caller retries once with the other scheme and `remember`s the one
  that worked, so later calls go straight there.

Any other host is untouched: verified with the default context, never swapped. Stdlib only.
"""
from __future__ import annotations

import ssl
import urllib.parse

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
