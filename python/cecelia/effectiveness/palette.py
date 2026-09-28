"""Loader for the shared CVD-safe console palette (`share/console_palette.json`).

Both consoles — `pixi run console` (Julia, `api/task_console.jl`) and `pixi run
recital-console` (this package's `console.py`) — read the same file so the two dev tools
render as one system and there is no drift between "green" here and "green" there.

The file names swatches (`vermillion`, `sky_blue`, …) — semantic mapping (which swatch is
"running", which is "confirmed") lives in each console because the two have disjoint
concept sets (statuses / pools vs mechanisms / outcomes / markers). Same reason the
frontend's `LANDSCAPE_SWATCHES` inlines the same hexes as a peer to this file: three
runtimes, one palette, mapping is local.

`load_palette()` returns a dict of `name → ANSI-escape-string` ready to prepend to a text
run — the terminal decides where the reset goes; both consoles pair each colour with a
trailing `RESET`. A missing / malformed file raises at import time, on the same principle
as `log.py::UnknownEventError`: a silent fall-through to bare ANSI would silently defeat
the CVD guarantee this file exists to hold.
"""

from __future__ import annotations

import json
import pathlib
import typing as _t


class PaletteError(RuntimeError):
    """Raised when the palette JSON is missing, malformed, or missing an expected swatch."""


#: Resolved path to `share/console_palette.json` — walk up from
#: `python/cecelia/effectiveness/palette.py` → `effectiveness/` → `cecelia/` → `python/`
#: → repo root (parents[3]), then into `share/`.
_PALETTE_PATH = pathlib.Path(__file__).resolve().parents[3] / "share" / "console_palette.json"

#: The swatch keys both consoles depend on. Enforced at load — a missing key would surface
#: as a silent `KeyError` at first paint, which is exactly the drift this file protects
#: against.
_REQUIRED_SWATCHES: tuple[str, ...] = (
    "vermillion", "orange", "yellow", "bluish_green",
    "sky_blue", "blue", "reddish_purple", "grey",
)


def _hex_to_ansi(hex_str: str) -> str:
    """`#RRGGBB` → `\\033[38;2;R;G;Bm` (SGR truecolor foreground). Case-insensitive."""
    hex_str = hex_str.strip()
    if not hex_str.startswith("#") or len(hex_str) != 7:
        raise PaletteError(f"palette hex {hex_str!r} is not `#RRGGBB`")
    try:
        r = int(hex_str[1:3], 16); g = int(hex_str[3:5], 16); b = int(hex_str[5:7], 16)
    except ValueError as e:
        raise PaletteError(f"palette hex {hex_str!r} not parseable: {e}") from e
    return f"\033[38;2;{r};{g};{b}m"


def load_palette(path: pathlib.Path | None = None) -> dict[str, str]:
    """Return `{swatch_name: ansi_escape}` from `share/console_palette.json`.

    `path` overrides the default location — used by tests to point at a fixture.
    """
    p = path or _PALETTE_PATH
    if not p.exists():
        raise PaletteError(
            f"shared palette missing at {p}; the two consoles need it to render CVD-safe"
        )
    try:
        with p.open("r", encoding="utf-8") as fh:
            data = json.load(fh)
    except json.JSONDecodeError as e:
        raise PaletteError(f"palette JSON malformed ({p}): {e}") from e
    swatches = data.get("swatches") or {}
    missing = [k for k in _REQUIRED_SWATCHES if k not in swatches]
    if missing:
        raise PaletteError(
            f"palette {p} missing required swatch(es): {missing}. "
            f"Both consoles depend on the full set; add or restore rather than deleting."
        )
    return {name: _hex_to_ansi(hex_str) for name, hex_str in swatches.items()}


#: Loaded once at import — a palette load failure is a hard startup error, not a first-
#: paint runtime one. Callers get a plain `dict[str, str]` they can index into.
PALETTE: dict[str, str] = load_palette()

# Named exports — each console imports the swatches it uses, so a rename (or a removal)
# in the JSON surfaces here as an import error and the console fails to start rather
# than silently rendering the wrong colour.
VERMILLION: _t.Final[str] = PALETTE["vermillion"]
ORANGE: _t.Final[str] = PALETTE["orange"]
YELLOW: _t.Final[str] = PALETTE["yellow"]
BLUISH_GREEN: _t.Final[str] = PALETTE["bluish_green"]
SKY_BLUE: _t.Final[str] = PALETTE["sky_blue"]
BLUE: _t.Final[str] = PALETTE["blue"]
REDDISH_PURPLE: _t.Final[str] = PALETTE["reddish_purple"]
GREY: _t.Final[str] = PALETTE["grey"]
