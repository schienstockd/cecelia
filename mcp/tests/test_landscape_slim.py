"""Unit tests for the landscape envelope compressor.

Covers the pure transform in `landscape_slim.py`: fat → slim tile shape, prelude
construction (channelNames / popMap), sparsity carry-through, degenerate inputs, and
a size ratchet on a synthetic 32×32 max-density envelope. The server-level wire test
lives in `test_server.py` — this file exercises the transform in isolation.
"""
from __future__ import annotations

import json
import unittest

from cecelia_mcp.landscape_slim import filter_landscape_tiles, slim_landscape_for_mcp


def _fat_tile(row: int, col: int, category: str = "dark", *, channels=None, seg=None,
              pops=None, tracks=None) -> dict:
    tid = _cell_label(row, col)
    tile = {
        "id": tid, "row": row, "col": col, "category": category,
        "stats": {"intensity": 0.1, "peak": 0.5, "variance": 0.2, "edges": 0.05},
    }
    if channels is not None:
        tile["channels"] = channels
    if seg is not None:
        tile["segCount"] = seg
    if pops is not None:
        tile["pops"] = pops
    if tracks is not None:
        tile["tracks"] = tracks
    return tile


def _cell_label(row: int, col: int) -> str:
    # Mirrors the Julia / TypeScript `cellLabel` — spreadsheet style A1, B1, …, AA1.
    letters = ""
    n = col
    while True:
        letters = chr(ord("A") + (n % 26)) + letters
        n = n // 26 - 1
        if n < 0:
            break
    return f"{letters}{row + 1}"


class SlimTileShapeTest(unittest.TestCase):
    def test_no_landscape_returns_dict_copy(self):
        env = {"captureId": "cap-x", "surface": "viewer_frame"}
        out = slim_landscape_for_mcp(env)
        self.assertEqual(env, out)
        self.assertIsNot(env, out)              # non-mutating: new dict

    def test_non_dict_input_returned_verbatim(self):
        # A None / str envelope shouldn't crash — return it as-is.
        self.assertIsNone(slim_landscape_for_mcp(None))
        self.assertEqual("not-a-dict", slim_landscape_for_mcp("not-a-dict"))

    def test_landscape_without_tiles_is_kept(self):
        env = {"landscape": {"grid": {"cols": 2, "rows": 2}, "schemaVersion": 1}}
        out = slim_landscape_for_mcp(env)
        self.assertEqual(env, out)

    def test_v1_landscape_drops_row_col_stats(self):
        # v1 = category-only. No channels/pops/tracks to compact, but row/col/stats still go.
        env = {"landscape": {
            "grid": {"cols": 2, "rows": 1}, "schemaVersion": 1, "legend": [],
            "tiles": [_fat_tile(0, 0), _fat_tile(0, 1, "mixed")],
        }}
        out = slim_landscape_for_mcp(env)["landscape"]
        self.assertNotIn("channelNames", out)   # sparsity: nothing to name
        self.assertNotIn("popMap", out)
        for t in out["tiles"]:
            self.assertNotIn("row", t)
            self.assertNotIn("col", t)
            self.assertNotIn("stats", t)
            self.assertIn("id", t)
            self.assertIn("category", t)

    def test_v2_channels_packed_positionally(self):
        env = {"landscape": {
            "grid": {"cols": 2, "rows": 1}, "schemaVersion": 2, "legend": [],
            "tiles": [
                _fat_tile(0, 0, channels={"GFP": {"mean": 0.5, "snr": 12.0},
                                          "TOM": {"mean": 0.1, "snr": 3.0}}, seg=3),
                _fat_tile(0, 1, channels={"GFP": {"mean": 0.4, "snr": 8.0}}),
            ],
        }}
        out = slim_landscape_for_mcp(env)["landscape"]
        # channelNames uses first-seen order — GFP appears in tile 0 first, then TOM.
        self.assertEqual(["GFP", "TOM"], out["channelNames"])
        # tile 0's channels array is [mean_gfp, snr_gfp, mean_tom, snr_tom] — positional to
        # the tile's own channel order, which for tile 0 matches channelNames.
        self.assertEqual([0.5, 12.0, 0.1, 3.0], out["tiles"][0]["channels"])
        # tile 1 has only GFP — the tile's array carries just GFP's two numbers.
        self.assertEqual([0.4, 8.0], out["tiles"][1]["channels"])

    def test_pops_prelude_dedups_by_path_name(self):
        env = {"landscape": {
            "grid": {"cols": 2, "rows": 1}, "schemaVersion": 2, "legend": [], "tiles": [
                _fat_tile(0, 0, pops=[
                    {"path": "/live/tnaive", "name": "T naive", "count": 3},
                    {"path": "/live/tmem",   "name": "T mem",   "count": 1}]),
                _fat_tile(0, 1, pops=[
                    {"path": "/live/tnaive", "name": "T naive", "count": 5}]),
            ]}}
        out = slim_landscape_for_mcp(env)["landscape"]
        # Two unique pops across all tiles → popMap has two entries.
        self.assertEqual(2, len(out["popMap"]))
        self.assertEqual({"path": "/live/tnaive", "name": "T naive"}, out["popMap"]["0"])
        # Per-tile pops reference popMap keys by index.
        self.assertEqual([["0", 3], ["1", 1]], out["tiles"][0]["pops"])
        self.assertEqual([["0", 5]], out["tiles"][1]["pops"])

    def test_prelude_fields_preserved_verbatim(self):
        env = {"landscape": {
            "grid": {"cols": 1, "rows": 1}, "schemaVersion": 2, "legend": [{"category": "dark"}],
            "sourceRun": {"segCount": {"valueName": "default", "labelsVersion": "v2"}},
            "viewport": {"renderMode": "plane", "zLo": 8, "zHi": 10},
            "tiles": [_fat_tile(0, 0, seg=1)],
        }}
        out = slim_landscape_for_mcp(env)["landscape"]
        self.assertEqual({"cols": 1, "rows": 1}, out["grid"])
        self.assertEqual(2, out["schemaVersion"])
        self.assertEqual([{"category": "dark"}], out["legend"])
        self.assertEqual({"segCount": {"valueName": "default", "labelsVersion": "v2"}},
                         out["sourceRun"])
        self.assertEqual({"renderMode": "plane", "zLo": 8, "zHi": 10}, out["viewport"])

    def test_empty_pops_or_channels_are_dropped(self):
        # Sparsity: an empty channels/pops container on the fat tile is treated as absent —
        # matches the backend's Decision 3 rule ("no `channels` key" not "channels: {}").
        env = {"landscape": {
            "grid": {"cols": 1, "rows": 1}, "schemaVersion": 2, "legend": [],
            "tiles": [{"id": "A1", "row": 0, "col": 0, "category": "dark",
                       "channels": {}, "pops": []}],
        }}
        out = slim_landscape_for_mcp(env)["landscape"]
        self.assertNotIn("channels", out["tiles"][0])
        self.assertNotIn("pops", out["tiles"][0])
        # Nothing to name / index → no prelude either.
        self.assertNotIn("channelNames", out)
        self.assertNotIn("popMap", out)

    def test_non_mutating(self):
        env = {"landscape": {
            "grid": {"cols": 1, "rows": 1}, "schemaVersion": 2, "legend": [],
            "tiles": [_fat_tile(0, 0, channels={"GFP": {"mean": 0.5, "snr": 12.0}}, seg=3)],
        }}
        _ = slim_landscape_for_mcp(env)
        # Original tile still carries row/col/stats + fat channels dict.
        t = env["landscape"]["tiles"][0]
        self.assertIn("row", t)
        self.assertIn("col", t)
        self.assertIn("stats", t)
        self.assertIsInstance(t["channels"], dict)


class FilterLandscapeTilesTest(unittest.TestCase):
    def _env_4_tiles(self):
        return {"landscape": {
            "grid": {"cols": 2, "rows": 2}, "schemaVersion": 2, "legend": [],
            "tiles": [
                _fat_tile(0, 0), _fat_tile(0, 1),
                _fat_tile(1, 0), _fat_tile(1, 1),
            ],
        }}

    def test_ids_filter_keeps_only_named_tiles(self):
        out = filter_landscape_tiles(self._env_4_tiles(), tile_ids=["A1", "B2"])
        ids = sorted(t["id"] for t in out["landscape"]["tiles"])
        self.assertEqual(["A1", "B2"], ids)

    def test_bbox_filter_left_half(self):
        # Left half of a 2×2 → col 0 only → A1, A2.
        out = filter_landscape_tiles(self._env_4_tiles(), bbox=[0.0, 0.0, 0.4, 1.0])
        ids = sorted(t["id"] for t in out["landscape"]["tiles"])
        self.assertEqual(["A1", "A2"], ids)

    def test_bbox_backwards_swaps(self):
        # p2 before p1 in a drag — the tool swaps rather than returning empty.
        out = filter_landscape_tiles(self._env_4_tiles(), bbox=[0.6, 1.0, 0.0, 0.5])
        # x1 < x2 ⇒ 0..0.6 → col 0 (0..0.3) + col 1 (0.3..0.6). y1..y2 = 0.5..1.0 → row 1 only.
        # So A2, B2.
        ids = sorted(t["id"] for t in out["landscape"]["tiles"])
        self.assertEqual(["A2", "B2"], ids)

    def test_bbox_off_frame_returns_empty(self):
        out = filter_landscape_tiles(self._env_4_tiles(), bbox=[1.5, 1.5, 2.0, 2.0])
        self.assertEqual([], out["landscape"]["tiles"])

    def test_ids_wins_when_both_given(self):
        # Explicit id list wins over a bbox that would pick something else — matches the tool's
        # docstring precedence rule.
        out = filter_landscape_tiles(self._env_4_tiles(),
                                     tile_ids=["A1"], bbox=[0.5, 0.5, 1.0, 1.0])
        ids = [t["id"] for t in out["landscape"]["tiles"]]
        self.assertEqual(["A1"], ids)

    def test_neither_returns_all_tiles(self):
        out = filter_landscape_tiles(self._env_4_tiles())
        self.assertEqual(4, len(out["landscape"]["tiles"]))


class SlimEnvelopeSizeRatchetTest(unittest.TestCase):
    """Synthetic max-density envelope — asserts the compressor makes a meaningful dent on
    the worst-case shape a real capture could ever hit.

    Ratchet numbers: 32×32 grid, 4 channels + segCount + 4 pops + tracks on every tile.
    Measured 2026-09-21: fat ~640 KB → slim ~260 KB (~60% reduction). The dominant cost is
    per-tile pops — even after popMap dedup, `[[popKey, count], ...]` × 4 pops × 1024 tiles
    still runs ~30 KB.

    Realistic captures compress MUCH better. The peer session's 4kS67f capture that
    triggered this work (cap-20260921T092735-bd47d6, 22×22, 4 channels, sparse pops):
    169 KB fat → 62 KB slim (63% reduction) — comfortably under Claude Code's
    tool-result token cap. The pathological 32×32-full-pops case is the escape-hatch
    territory for `get_capture_landscape_tiles`, not something we compress our way out of.

    Ceiling set at 350 KB — above the measured 260 KB with slack for one small future
    field. Floor at 100 KB guards against a compressor that silently drops something
    structural."""

    def test_a_32x32_max_density_envelope_slims_under_the_ceiling(self):
        tiles = []
        for r in range(32):
            for c in range(32):
                tiles.append(_fat_tile(
                    r, c, category="bright-textured",
                    channels={
                        "Channel1": {"mean": 0.5432, "snr": 12.345},
                        "Channel2": {"mean": 0.1234, "snr": 3.456},
                        "Channel3": {"mean": 0.8765, "snr": 45.678},
                        "Channel4": {"mean": 0.2468, "snr": 8.912},
                    },
                    seg=42,
                    pops=[
                        {"path": "/live/tnaive",    "name": "T naive",   "count": 5},
                        {"path": "/live/tmem",      "name": "T mem",     "count": 3},
                        {"path": "/live/treg",      "name": "T reg",     "count": 2},
                        {"path": "/live/dendritic", "name": "Dendritic", "count": 4},
                    ],
                    tracks={"count": 7, "meanDuration": 42.5, "meanSpeed": 1.234},
                ))
        env = {"landscape": {
            "grid": {"cols": 32, "rows": 32}, "schemaVersion": 2, "legend": [],
            "sourceRun": {
                "segCount": {"valueName": "default", "labelsVersion": "v2"},
                "pops":     {"valueName": "default", "popType": "flow",
                             "gatingMtime": "1698765432.123"},
                "tracks":   {"valueName": "default", "labelsVersion": "v2"},
                "channels": {"valueName": "default", "imageVersion": "v1", "level": 0},
            },
            "viewport": {"renderMode": "volume", "zLo": 0, "zHi": 29},
            "tiles": tiles,
        }}
        fat_bytes = len(json.dumps(env).encode("utf-8"))
        slim = slim_landscape_for_mcp(env)
        slim_bytes = len(json.dumps(slim).encode("utf-8"))
        KB = 1024
        # Sanity floor — the fat envelope must be materially bigger than the slim one, or the
        # compressor did nothing. Fat should be > 400 KB on this max-density input.
        self.assertGreater(fat_bytes, 400 * KB)
        # Ceiling — see the class docstring for measured baseline (260 KB) and rationale
        # (350 KB leaves room for one small future field before firing).
        self.assertLess(slim_bytes, 350 * KB)
        # And a hard floor — a max-density envelope compressed to less than 100 KB likely
        # means the compressor dropped something structural.
        self.assertGreater(slim_bytes, 100 * KB)


if __name__ == "__main__":
    unittest.main()
