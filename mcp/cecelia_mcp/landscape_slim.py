"""Landscape envelope compressor for the MCP `get_capture` tool result.

Claude Code caps tool responses at ~25 k tokens by default. A v2 landscape at a 32×32
grid with 4 channels + 4 pops + tracks per tile serialises to ~640 KB — well over the
cap. The peer-session symptom is "the landscape tile map came back truncated" (a real
capture on 4kS67f, 2026-09-21): 484 tiles, 169 KB of landscape alone in a 477 KB
envelope, sliced on delivery.

This module compresses the landscape's tile bag for MCP delivery, at read time, without
touching the stored envelope (Kiwi + frontend keep the fat form). The transform is
lossless for anything a reader needs:

  Prelude:
    channelNames: [str, …]            union of channel names across tiles, first-seen
                                       order. Positional index into per-tile channels.
    popMap: {"<key>": {path, name}}    union of (path, name) tuples across tiles.
                                       Key is a stable stringified sequence id.

  Per-tile:
    id, category, segCount?, tracks?   verbatim
    channels: [m, s, m, s, …]          [mean, snr, …] positional to channelNames
    pops:     [[popKey, count], …]     [popMap key, count] pairs

  Dropped: `row` / `col` (derivable from `id` via cellLabel A1/B2/…), `stats` (frontend-
  computed leftovers from Phase 1, superseded by `channels` + `category` for augmented
  tiles).

Also compressed: `frame` is a data-URL string that lands as an Image content block in
the MCP tool response — the raw base64 in the envelope dict is a duplicate. We drop it
from the envelope side of the response and let the Image block do the frame delivery.

Measured on cap-20260921T092735-bd47d6 (4kS67f, 484 tiles, 4 channels, augmented):
  169 KB fat → ~52 KB slim (69% reduction).
"""

from __future__ import annotations

from typing import Any


def slim_landscape_for_mcp(envelope: dict) -> dict:
    """Return a new envelope with the landscape's tile bag compressed for MCP delivery.

    Non-mutating: `envelope` and every nested dict inside its `landscape` field survive
    unchanged. A v1 envelope (schemaVersion 1 or absent) still gets the size cuts (`row`,
    `col`, `stats`) — the augmentation fields are absent so channelNames/popMap end up
    empty. A capture with no landscape at all (`surface: "plot"`, an early frame-only
    capture) is returned as-is.
    """
    if not isinstance(envelope, dict):
        return envelope
    landscape = envelope.get("landscape")
    if not isinstance(landscape, dict):
        return dict(envelope)
    tiles = landscape.get("tiles")
    if not isinstance(tiles, list):
        return dict(envelope)

    channel_names: list[str] = []
    channel_name_index: dict[str, int] = {}
    pop_map: dict[str, dict[str, str]] = {}
    pop_key_index: dict[tuple[str, str], str] = {}

    def _channel_index(name: str) -> int:
        # Preserve first-seen order across tiles so a downstream reader's channelNames
        # doesn't jitter when a tile-order change would otherwise reshuffle it.
        if name in channel_name_index:
            return channel_name_index[name]
        i = len(channel_names)
        channel_names.append(name)
        channel_name_index[name] = i
        return i

    def _pop_key(path: str, name: str) -> str:
        # Stable string keys (JSON dicts don't guarantee int-key round-trip). Numeric
        # ordering by first-seen sequence keeps the prelude legible.
        key_tuple = (path, name)
        if key_tuple in pop_key_index:
            return pop_key_index[key_tuple]
        k = str(len(pop_map))
        pop_map[k] = {"path": path, "name": name}
        pop_key_index[key_tuple] = k
        return k

    slim_tiles = [_slim_tile(t, _channel_index, _pop_key) for t in tiles]

    new_landscape: dict[str, Any] = {}
    # Preserve prelude fields verbatim — grid/legend/schemaVersion/sourceRun/viewport
    # are already tiny and structurally meaningful; the compression is per-tile only.
    for key in ("grid", "legend", "schemaVersion", "sourceRun", "viewport",
                "createdAt", "imageUid", "valueName", "t", "z"):
        if key in landscape:
            new_landscape[key] = landscape[key]
    # Only emit the preludes when they carry data — sparsity mirrors the tile fields.
    if channel_names:
        new_landscape["channelNames"] = channel_names
    if pop_map:
        new_landscape["popMap"] = pop_map
    new_landscape["tiles"] = slim_tiles

    new_env = dict(envelope)
    new_env["landscape"] = new_landscape
    return new_env


def _slim_tile(tile: Any, channel_index, pop_key) -> dict:
    if not isinstance(tile, dict):
        return tile
    out: dict[str, Any] = {}
    # Verbatim fields — everything the reader needs to identify the tile and its
    # non-augmentation content. `id` is A1 / B2 / … and encodes (row, col) via
    # cellLabel, so `row` and `col` are dropped as derivable.
    for key in ("id", "category", "segCount"):
        if key in tile:
            out[key] = tile[key]
    tracks = tile.get("tracks")
    if isinstance(tracks, dict):
        out["tracks"] = tracks
    channels = tile.get("channels")
    if isinstance(channels, dict) and channels:
        # Positional [mean, snr] pairs indexed by channelNames — self-describing given
        # the prelude and cheaper than repeating every channel name per tile.
        packed: list[float] = []
        for name, stats in channels.items():
            channel_index(str(name))
            if isinstance(stats, dict):
                packed.extend([stats.get("mean"), stats.get("snr")])
            else:
                packed.extend([None, None])
        out["channels"] = packed
    pops = tile.get("pops")
    if isinstance(pops, list) and pops:
        # `[popKey, count]` pairs — the (path, name) live in the prelude once instead
        # of on every tile. Zero-count pops are already stripped upstream (Decision 3
        # sparsity in the compute handler).
        packed_pops: list[list[Any]] = []
        for p in pops:
            if not isinstance(p, dict):
                continue
            path = str(p.get("path", ""))
            name = str(p.get("name", ""))
            count = p.get("count", 0)
            packed_pops.append([pop_key(path, name), count])
        if packed_pops:
            out["pops"] = packed_pops
    return out


def filter_landscape_tiles(envelope: dict, *, tile_ids: list[str] | None = None,
                           bbox: list[float] | None = None) -> dict:
    """Return a new envelope keeping only tiles matching `tile_ids` or `bbox`.

    Exactly one of `tile_ids` (list of `id` strings like "A1", "B2") or `bbox` (four
    floats `[x1, y1, x2, y2]` in 0..1 frame-relative coords — same coord system the
    overlay marks use) picks the tiles. Both absent ⇒ all tiles are kept. Both present
    ⇒ `tile_ids` wins (explicit id list is more specific than a rectangle).

    Applies `slim_landscape_for_mcp` after filtering, so the returned envelope's
    landscape has the same shape as `slim_landscape_for_mcp`'s output — compressed
    prelude + slim tiles — just for a subset.
    """
    if not isinstance(envelope, dict):
        return envelope
    landscape = envelope.get("landscape")
    if not isinstance(landscape, dict) or not isinstance(landscape.get("tiles"), list):
        return slim_landscape_for_mcp(envelope)

    tiles: list[dict] = [t for t in landscape["tiles"] if isinstance(t, dict)]
    kept: list[dict]
    if tile_ids:
        wanted = set(str(x) for x in tile_ids)
        kept = [t for t in tiles if t.get("id") in wanted]
    elif bbox and len(bbox) == 4:
        grid = landscape.get("grid") or {}
        cols = int(grid.get("cols") or 0)
        rows = int(grid.get("rows") or 0)
        if cols <= 0 or rows <= 0:
            kept = []
        else:
            x1, y1, x2, y2 = (float(v) for v in bbox)
            if x1 > x2:
                x1, x2 = x2, x1
            if y1 > y2:
                y1, y2 = y2, y1
            # A bbox with BOTH corners fully outside the frame (x < 0 or > 1 on both sides,
            # same for y) is treated as "no tiles" rather than clamped to the nearest edge —
            # a mark drawn entirely off the frame shouldn't collapse to a single corner tile.
            # A bbox that partially extends past the edge still clamps normally below.
            if x2 < 0 or x1 > 1 or y2 < 0 or y1 > 1:
                kept = []
            else:
                # Inclusive integer col/row ranges for the bbox. `floor(x*cols)` picks the
                # column the bbox left edge sits in; `ceil(x*cols) - 1` picks the column the
                # right edge sits in. Clamp to the grid so a bbox that partially overhangs
                # yields a valid slice rather than an IndexError.
                import math
                col_lo = max(0, min(cols - 1, math.floor(x1 * cols)))
                col_hi = max(0, min(cols - 1, math.ceil(x2 * cols) - 1))
                row_lo = max(0, min(rows - 1, math.floor(y1 * rows)))
                row_hi = max(0, min(rows - 1, math.ceil(y2 * rows) - 1))
                if col_hi < col_lo or row_hi < row_lo:
                    kept = []
                else:
                    kept = [t for t in tiles
                            if (row_lo <= int(t.get("row", -1)) <= row_hi
                                and col_lo <= int(t.get("col", -1)) <= col_hi)]
    else:
        kept = tiles

    filtered_env = dict(envelope)
    filtered_landscape = dict(landscape)
    filtered_landscape["tiles"] = kept
    filtered_env["landscape"] = filtered_landscape
    return slim_landscape_for_mcp(filtered_env)
