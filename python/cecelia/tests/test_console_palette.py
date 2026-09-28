"""Tests for the shared console palette (`share/console_palette.json` + `palette.py` loader).

Pins:

- **The eight required swatches load without error** — a missing swatch is exactly the drift
  the loader exists to catch on behalf of both dev consoles.
- **Hex values parse to well-formed truecolor SGR escapes** — `#RRGGBB` → `\\033[38;2;R;G;Bm`,
  case-insensitive on the hex.
- **A malformed JSON raises `PaletteError`** — silent fall-through would render bare ANSI and
  silently defeat the CVD guarantee this file exists to hold.
- **The shipped palette hasn't drifted from Okabe-Ito** — the two dev consoles rely on the
  eight canonical values (jfly.uni-koeln.de/color/), and a hand-tweaked hue would break the
  CVD guarantee without anyone noticing. Golden values pinned below.
"""

from __future__ import annotations

import json
import pathlib
import tempfile
import unittest

from cecelia.effectiveness.palette import (
    PALETTE,
    PaletteError,
    _hex_to_ansi,
    _REQUIRED_SWATCHES,
    load_palette,
)


class PaletteLoaderTest(unittest.TestCase):
    def test_all_required_swatches_present(self):
        for name in _REQUIRED_SWATCHES:
            self.assertIn(name, PALETTE,
                          f"required swatch {name!r} missing from shipped palette")
            self.assertTrue(PALETTE[name].startswith("\033[38;2;"),
                            f"swatch {name!r} isn't a truecolor SGR escape")

    def test_hex_to_ansi_produces_correct_sgr(self):
        # #D55E00 → RGB(213, 94, 0). The SGR family that renders in every modern terminal
        # is `\e[38;2;R;G;Bm`; anything else silently no-ops.
        self.assertEqual(_hex_to_ansi("#D55E00"), "\033[38;2;213;94;0m")

    def test_hex_parse_is_case_insensitive(self):
        self.assertEqual(_hex_to_ansi("#d55e00"), _hex_to_ansi("#D55E00"))

    def test_bad_hex_raises(self):
        # Load-time validation, not first-paint runtime — a bad hex must fail loud.
        for bad in ("D55E00", "#D55E0", "#GGGGGG", ""):
            with self.assertRaises(PaletteError, msg=f"{bad!r} should raise"):
                _hex_to_ansi(bad)

    def test_missing_file_raises(self):
        with self.assertRaises(PaletteError):
            load_palette(pathlib.Path("/nonexistent/palette.json"))

    def test_malformed_json_raises(self):
        with tempfile.TemporaryDirectory() as tmp:
            p = pathlib.Path(tmp) / "palette.json"
            p.write_text("this is not json", encoding="utf-8")
            with self.assertRaises(PaletteError):
                load_palette(p)

    def test_missing_swatch_raises(self):
        # A partial palette (someone deleted a key) breaks the guarantee both consoles rely
        # on. Loader lists the missing name so the operator can restore it.
        with tempfile.TemporaryDirectory() as tmp:
            p = pathlib.Path(tmp) / "palette.json"
            p.write_text(json.dumps({"swatches": {"vermillion": "#D55E00"}}),
                         encoding="utf-8")
            with self.assertRaises(PaletteError) as ctx:
                load_palette(p)
            self.assertIn("orange", str(ctx.exception))  # names the missing swatch

    def test_shipped_palette_matches_okabe_ito(self):
        # Golden values from Okabe & Ito (2008) — https://jfly.uni-koeln.de/color/. The two
        # consoles inherit CVD-safety from this exact hue set; a hand-tweaked value would
        # look "prettier" and silently defeat the guarantee.
        golden = {
            "vermillion":     "\033[38;2;213;94;0m",
            "orange":         "\033[38;2;230;159;0m",
            "yellow":         "\033[38;2;240;228;66m",
            "bluish_green":   "\033[38;2;0;158;115m",
            "sky_blue":       "\033[38;2;86;180;233m",
            "blue":           "\033[38;2;0;114;178m",
            "reddish_purple": "\033[38;2;204;121;167m",
        }
        for name, want in golden.items():
            self.assertEqual(PALETTE[name], want,
                             f"swatch {name} drifted from Okabe-Ito")


if __name__ == "__main__":
    unittest.main()
