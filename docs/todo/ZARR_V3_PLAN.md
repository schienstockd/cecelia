# Zarr v3 + sharding

Read, write and report zarr v3 (OME-NGFF 0.5) stores, and offer **sharding** as a write option.

Status: **Phase 1 (read) COMPLETE** — both languages read v2 and v3 identically, against committed real fixtures of each format; all four suites green. Phase 2 (report) next. Prerequisite #484 (bioformats2raw shuffle spelling) is merged; v3 only exists in bioformats2raw ≥ 0.12.0.

---

## Why now

`bioformats2raw` 0.12.0 (2026-04-22) swapped jzarr → zarr-java and gained v3 (`--ngff-version 0.5`)
and sharding (`--shard-width/height/depth`). We will have to go through this regardless, and the
prize is **sharding**: v3 packs many chunks into one shard file, which is the fix for the
millions-of-tiny-files problem that costs us on network-share imports and on `.ccbundle`
tar-per-store export.

Upgrading the binary is **not** the same as being v3-ready — it only makes v3 reachable. Measured
against a real v3 store written by 0.12.1 (`--ngff-version 0.5`, our own codec flags):

| | | |
|---|---|---|
| Python | `is_zarr_store` | ✅ |
| | `series_base` | ❌ returns the root, not `0/` |
| | `read_axes` / `read_scale` / `read_time_increment` | ❌ `None` |
| | `open_as_zarr` | 💥 `AttributeError: 'GroupInfo' object has no attribute 'obj'` |
| Julia | `open_level0` | ✅ Zarr.jl reads v3 **and** sharding transparently |
| | `read_ngff_axes` | ⚠️ empty, **silently** |
| | `image_geometry` | ⚠️ right **by luck** — `axis_dims` falls back to a by-rank `[t,c,z,y,x]` guess |
| | `store_compression` | ❌ `nothing` |
| | crop preview render | ✅ byte-identical to the v2 render |

Two of those matter more than the rest:

* **`open_as_zarr` crashing is the hard blocker** — every Python task reads pixels through it, so
  segmentation/tracking/measurement stop dead. At least it is loud.
* **`read_scale`/`read_time_increment` → `None` is the quiet one.** That is exactly the trap in
  `CLAUDE.md` → *Calibration — three copies, one stamp*: "we don't know" silently becomes "1 µm,
  1 second per frame" downstream. Combined with Julia *guessing* the axis order rather than reading
  it, a v3 store would appear to work and produce wrong physical numbers. **This is the reason the
  read side ships before anything else, and why v3 writing must not be enabled until it is done.**

## What actually differs (v2 → v3)

Measured, not assumed — from a real 0.12.1 store of each.

| | v2 / NGFF 0.4 | v3 / NGFF 0.5 |
|---|---|---|
| group metadata | `.zgroup` + `.zattrs` | `zarr.json` (`node_type: group`) |
| array metadata | `.zarray` | `zarr.json` (`node_type: array`) |
| NGFF attrs location | `.zattrs` **top level** | `zarr.json` → `attributes` → **`ome`** |
| dtype + byte order | one string, `>u2` / `<u2` | `data_type: uint16` + a **`bytes` codec** carrying `endian` |
| compression | one `compressor` | a **codec pipeline** (`[bytes, blosc]`) |
| sharding | n/a | `sharding_indexed` wrapping the inner codecs — **on by default** from bioformats2raw |
| store layout | `0/` series wrapper, `OME/` group | unchanged |

The multiscales *content* (axes, datasets, coordinateTransformations) is **identical** — only the
container and the one `ome` nesting level move. That is what makes the read side small.

Real v3 level-0 codec chain from bioformats2raw 0.12.1:

```json
{"name": "sharding_indexed", "configuration": {
  "chunk_shape": [1, 1, 1, 512, 512],
  "codecs": [{"name": "bytes",  "configuration": {"endian": "little"}},
             {"name": "blosc",  "configuration": {"cname": "zstd", "shuffle": "shuffle",
                                                  "clevel": 3, "typesize": 2, "blocksize": 0}}],
  "index_codecs": [{"name": "bytes", "configuration": {"endian": "little"}}, {"name": "crc32c"}],
  "index_location": "end"}}
```

## Locked decisions

**D1 — One NGFF-attribute resolver per language; extend the existing one, do not add a variant.**
`CLAUDE.md` → *OME-ZARR dual-format* already names `series_base` as the single resolver per language
for the flat-vs-series question. The v2-vs-v3 question goes in the **same** place, not into a parallel
set of readers. In Python it collapses to unwrapping one key, because `zarr-python`'s `Group.attrs`
already abstracts `.zattrs` vs `zarr.json`:

```python
def _ngff_attrs(attrs):          # NGFF 0.5 nests everything under `ome`; 0.4 is top level
    return attrs.get('ome', attrs)
```

Julia reads the JSON itself, so it needs the file-level branch too (`.zattrs`, else
`zarr.json` → `attributes` → unwrap `ome`).

**D2 — The zarr format is DISCOVERED per store, never assumed or configured for reading.** Both
formats will coexist on disk indefinitely: bioformats2raw never rewrites its output, so every store
imported before the upgrade stays v2 (and big-endian — see `docs/NAPARI.md` → *Byte order*). A
reader that needs to know asks `store_format(path)`; nothing keys off a setting or a suffix. Same
principle as the existing structural flat-vs-series detection.

**D3 — Writing v3 and sharding are VISIBLE settings with stated trade-offs, not hardcoded
constants.** Same call as `[zarr].imageCompressor` (`docs/FUTURE.md`, and the reasoning in
`CLAUDE.md` → *the compressor is a decision, not a default*). Sharding trades "millions of small
files" for "rewriting a shard to touch one chunk", which is a real cost on the correction tasks that
rewrite a store plane by plane — so it must be a choice the user can see and revert, with the
measured numbers next to it like the compressor table.

**D4 — Default stays v2 until the read side is proven.** Flipping the default is a separate,
explicit change at the end of Phase 4, after the fixtures and both suites pass. Writing v3 while we
cannot read scale/axes would silently produce wrong physical numbers.

**D5 — `store_compressor` must answer for BOTH formats, from one place.** v3 wants a codec pipeline
rather than a single compressor, so the helper gains a format (and optional shard shape) argument
instead of a second helper appearing next to it. The `test_store_compressor_convention.py` detector
extends to cover v3 writers.

**D6 — v3 needs NO byte-order handling; `read_native` answering `'|'` (never swap) is correct by
construction.** Originally recorded the other way round, and that was wrong. v3 does not put byte
order in the dtype at all — `metadata.dtype` is a plain `"uint16"` and the order lives in the `bytes`
codec *inside the codec pipeline*, which is Zarr.jl's to execute, and it does. Verified: a v2 and a v3
store written from the same source decode to **identical pixels**. The v2 case is the odd one out —
there the order is metadata Zarr.jl parses for the eltype and then ignores, which is the entire reason
`read_native` exists. Adding a v3 branch would double-swap.

**D7 — No v2 → v3 converter. Existing stores stay v2, permanently.** (Dominik, 2026-08-06.) A
converter would rewrite every byte for no analytical gain, and both formats are readable, so there is
nothing to migrate *for*. This is a **non-goal**, not deferred work — `resync_ome_meta!` (metadata-only
repair) is not a precedent for it. It also means `read_native`'s big-endian path
(`docs/NAPARI.md` → *Byte order*) is load-bearing forever, not transitional.

**D8 — When WE write v3, the shard must equal our write unit. Never inherit bioformats2raw's
default.** A shard is one file holding many chunks plus an index, so changing one chunk rewrites the
whole shard. Sharding is therefore a disadvantage exactly where the write pattern does not align to a
shard boundary:

| scenario | effect |
|---|---|
| partial rewrite not aligned to a shard (correction tasks, streaming segmentation writer) | read-modify-write amplification |
| two workers owning different chunks of one shard | contention, or a corrupt shard |
| cancel/resume | a half-written shard loses more than a half-written chunk |
| random single-chunk read | one extra index indirection (small; usually offset by far fewer file opens) |
| overwriting chunks with smaller ones | space inside the shard is not reclaimed |

Measured on bioformats2raw 0.12.1 output: the default shard is `[1,1,1,1024,1024]` and
`--shard-depth` defaults to 1, so **a shard never spans t/c/z** — it is XY within one plane. Our
plane-by-plane writers therefore write each shard exactly once and pay nothing.

**Therefore: shard write-once-sequential stores (imports); do NOT shard incrementally-written ones.**
An import is written once, in order, by bioformats2raw — the ideal sharding case. A segmentation or
correction output is filled tile-by-tile over minutes, and its write unit is
`SegmentationWriter.block_size` — a **task parameter the user sets**, so no fixed shard shape can be
guaranteed to align with it. That is the honest rule; "shard = write unit" is unachievable when the
write unit is user-variable.

> An earlier draft of this decision said "segmentation writes 512×512 tiles
> (`plane_chunks(..., xy_tile=512)`)". That conflated two independent things and was wrong:
> * `plane_chunks(xy_tile=512)` sets the **zarr chunk shape**, chosen for *napari read* granularity —
>   napari slices per (t,c,z), so a chunk must not span T/C. It is an IO parameter.
> * `SegmentationWriter.block_size` (+ `overlap`) is the **compute tile** for cellpose — memory and
>   boundary artefacts. Unrelated to the chunk.
>
> The 512 in `plane_chunks` is also **not a hard border** (Dominik, 2026-08-06): it dates from
> intravital 2P acquisitions being 512×512, and current acquisitions are 1024×1024. Widening it is a
> real option, but it is a *read-granularity* trade — a small random region read pulls 4× the bytes —
> and it must be decided on its own merits, not as a side effect of a sharding choice. It does not
> change the cellpose tile.

Phase 4 measures the correction-task rewrite to confirm the prediction above.

## Phases

### Phase 1 — Read (the blocker) ⬅ current

* Python `zarr_utils`: `_ngff_attrs` shim; route `series_base` + `read_multiscales_meta` through it
  → `read_axes` / `read_scale` / `read_time_increment` / `read_axis_units` inherit it. Fix the
  `zgroup.info.obj` crash in `zarr_data_to_list`. Add `store_format(path)`.
* Julia `api/src/image_geometry.jl`: same shim for `read_ngff_axes`; `zarr.json` branch in
  `store_compression`; `_zarr_byte_order` reads the v3 `bytes` codec (D6).
* Julia `app/src/tasks/importImages/omezarr.jl`: `series_base`, `read_ome_metadata`,
  `update_ome_scale!` / `sync_zarr_calibration!` — the calibration stamp is the risky one, and
  `app/test/runtests.jl` → *"calibration writers agree across languages"* is its contract.
* Fixtures — **done**: `test-data/projects/ZARRFMT/` holds the same real pixels as a v2 store and a
  v3 **sharded** store (a 64×64 crop of `M2b-CD8-GFP-CD20-Tom.tif`, 3t×4c×3z, real calibration,
  259 KB total against the 8 MB tree cap). Cropped from real data rather than a `.fake` gradient so
  the codec and intensity distribution are representative, calibration deliberately ≠ 1.0 so a correct
  read is distinguishable from the "unknown" fallback, and shard ≠ chunk so the report cannot pass
  vacuously. Documented in `test-data/README.md`.
* Tests — **done**: `test-py` (523), `test-api`, `test-pkg` all green, each asserting the two formats
  AGREE rather than hardcoding expectations twice.

### Phase 2 — Report (the metadata modal)

Surface, per image version and label set: **zarr format**, **NGFF version**, **chunk shape**,
**shard shape** (or "none"), and the codec chain. Extend the existing
`store_compression` → `GET /api/images/stores` path that the modal already renders; do not add a
route. Keep the copy to values, not prose (`docs/UI.md` → *UI copy*).

### Phase 3 — Write

* `store_compressor(kind, zarr_format, shard_shape)` (D5); `create_multiscales` /
  `open_multiscales_for_writing` / `create_zarr_from_ndarray` take a format.
* `write_calibration` writes `zarr.json` attributes for v3 — **both** copies from the one derivation,
  per `CLAUDE.md` → *Calibration*.
* `set_ngff_axes`, `write_valid_box` / `read_valid_box`, `multiscales_metadata` (NGFF 0.5 needs a
  `version` key and the `ome` wrapper).
* Verify `staged_store` / `promote_store` are format-agnostic (they rename directories, so expected
  to be — assert it).
* Import task: `ngffVersion` + shard params → `--ngff-version` / `--shard-*`.
* Settings → Storage: format + sharding controls next to the compressor, with measured numbers.

### Phase 4 — Adopt

Measure sharded vs unsharded on a real timecourse (store size, file count, import time, warm plane
read, and the **correction-task rewrite** cost, which is where sharding should hurt). Publish the
numbers on the constants like the compressor table. Then decide the default (D4).

## Open questions

* Phase 4 must still **measure** a correction task rewriting a sharded store end to end. D8 argues
  from the format and from the observed default shard shape that per-plane writers pay nothing; that
  is a prediction until the rewrite is timed.
* zarr-java's `byteshuffle` bug (see #484) is unreported upstream. If we adopt v3 writing through
  bioformats2raw we depend on `shuffle` staying the working spelling.
