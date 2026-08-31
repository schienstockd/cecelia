## Brick renderer integration — auto-select, close the last gaps, retire flat

**Status:** planning (2026-08-29) · branch `feat/brick-followup`
**Supersedes:** [`KILN_BRICK_PLAN.md`](KILN_BRICK_PLAN.md) (P0–P5 shipped through PR #691–#706; P5d perf and
P6/P7 rolled forward here on the up-to-date data).

**Post-#706 note (2026-08-29):** `writeBrick` collapsed from N `writeTexture` calls per brick to ONE.
MAP_WRITE path removed after the fair one-call comparison went the wrong way. Doesn't shift the
auto-select math (write cost sits inside `kickFetch`, not `draw()`), and does NOT fix f8gzA2's 200 ms
`drawP95` (that cost is in `tickScheduler` + `pageTableCpu` upload — B4 still valid). Bench blobs
taken pre-#706 have a `writes[]` array that's populated per-channel; post-#706 it's one entry per
brick. Diffing needs to normalise on that.

## Goal

Bring the brick renderer out of `?bricks=1` dev-only into the default 3D path, without regressing the
cases the flat renderer serves fine. Ground every decision in the bench blobs saved 2026-08-29
(`~/Downloads/TMP/bench/*.json`) and the code state after #704 (LRU-clobber, residency-map filter and
canvas-invalid chip all landed).

## What we know now (that the old plan didn't)

Five images benchmarked flat vs brick 2026-08-29 02:14-02:33, **before** the LRU-clobber fix (#702)
landed at 14:15. Headline read-outs:

| Image | nLev | L0 MB | flat firstMs | brick firstMs | flat drawP95 | brick drawP95 | flat MB | brick MB |
|---|---|---|---|---|---|---|---|---|
| fXgbTl | 1 | 45 | 279 | **12** | 0.1 | 2.3 | 1153 | **44** |
| Dml3RG | 1 | 311 | 1505 | **10** | 0.2 | 12.1 | 2176 | 7883¹ |
| FtGoJO | 4 | 1219 | 294 | **50** | 0 | 0.7 | 12 | 395 |
| SispLk | 6 | 8297 | 896 | **28** | 0.2 | 4.5 | 898 | 2669 |
| f8gzA2 | 6 | 16380 | 1435 | 379 | 0.2 | **200.9** ² | 16 | 2720 |

¹ pre-#702 LRU-clobber thrash; expected to drop toward the 512 MB atlas budget after re-bench.
² this is the shader-scaling failure — nZ=1 × 343 Mpx plane produces a huge viewport intersect list.

Three things follow from this that Sonnet's earlier read (2026-08-27) didn't have:

1. **Brick wins time-to-first-frame everywhere except f8gzA2.** The old "single-level = brick loses"
   heuristic (`nLevels > 1`) excludes fXgbTl / Dml3RG where brick's TTFF is 23-150× better and
   bandwidth is dramatically lower (44 MB vs 1153 MB on fXgbTl). TTFF is the UX metric users feel.
2. **Flat's drawP95 = 0.1-0.2 ms across the board.** That's CPU-side `r.draw()` submission only (per
   the bench harness comment, no GPU timestamp query); brick's higher numbers aren't render cost —
   they're `tickScheduler` + intersect-list + `pageTableCpu` upload work happening inside `draw()`.
   Real cost, but not the 10× gap the raw ratio suggests.
3. **f8gzA2 is a shader-scaling failure, not an auto-select miscall.** 200 ms drawP95 says the brick
   renderer literally cannot render f8gzA2-shape stores at interactive rate today. Auto-select has
   to exclude it; it's a bug, not a preference.

## Locked decisions

1. **Auto-select on a store-side predicate — pure function on `ViewerMeta`.** Not `?bricks=`. The
   URL flag stays as a **dev override** (`?bricks=0` forces flat, `?bricks=1` forces brick) so we can
   A/B any image. User-facing UI does not surface the choice.

2. **Predicate: whole-movie bytes vs a cache budget.** `nT * bytesPerT < CACHE_BUDGET_BYTES` → flat,
   else brick. Reshaped from the earlier `nX * nY < 200 Mpx` gate after bench5 (2026-08-29) showed
   the intersect guard dropped f8gzA2's drawP95 200 ms → 2.8 ms — the shader-scaling exclusion
   disappeared. bench6 then showed the honest axis is "does flat's cache hold the whole movie":
   fXgbTl (1.4 GB total, cached 31/31, playback 0.10 ms, zero re-fetch) → **flat**; Dml3RG (8.5 GB,
   cached 4/181) → **brick**; f8gzA2 (17 GB per t) → **brick** (flat can't upload one volume).
   Budget starts at 1.5 GB. `CACHE_BUDGET_BYTES` in `volumeViewer.ts` locked by unit tests.

3. **Selection is reactive on `meta`, evaluated once meta lands.** `bricksEnabled` is a `computed`
   that reads `meta.value` under a null guard. Consumers snapshot the value inside `ensureRenderer`
   after `reallocate` confirmed `meta.value !== null`, so mid-session flips don't happen and the
   renderer is picked once per image.

4. **The user dropdown is a FLOOR, not a pin. SSE reloads finer on zoom-in.** Reversal of the
   8b780fd `setLevelOverride` pin. Dominik screenshot 2026-08-29 (SispLk zoomed IN, still L5,
   one L5 voxel covering ~30 device px): the pin blocks the exact reason the pyramid exists. New
   rule:
   - "Auto" (dropdown = -1) → floor = `n-1` (coarsest); SSE freely picks finer as viewport shrinks.
   - Explicit level (dropdown = k) → floor = k; SSE never coarser than k, always finer if warranted.
   - **Over-fetch guard** — if the SSE-desired level's viewport intersect list exceeds
     `MAX_INTERSECT_BRICKS`, coarsen one level at a time until it fits (or floor is reached). This
     is the honest fix for f8gzA2 fit-distance: 168× over-fetch was SSE picking L0-L1 for a wide
     viewport, not SSE being wrong — bounding the intersect count at BOTH ends is what the pin
     approximated poorly.
   - `pickVolumeLevel` **stays** in the flat path unchanged; it defines the FLOOR the brick path
     clamps against, not the render level.
   - Rename `setLevelOverride` → `setLevelFloor` (behaviour change, name change follows).

5. **Perf ratchet applies to the case brick REPLACES flat.** The 5.3 ms MIP budget from
   `NAPARI_WEBGPU_AUDIT.md` is a bar for stores the predicate says "use brick" (i.e. mostly the
   multi-level 3D case). Not a bar for fXgbTl, where flat's 0.1 ms is a straw man (CPU submission,
   not real GPU cost).

6. **No new server route.** Brick fetch still goes through `/api/viewer/slab?cTo=nC-1` from #700
   (P0.5 in the old plan). Nothing here needs a server change.

## Phased build sequence

**B0 ✅ shipped 2026-08-29** — Unpin LOD, wire the floor + SSE + over-fetch guard. `setLevelOverride`
renamed to `setLevelFloor`; `MAX_INTERSECT_BRICKS = 256`; scheduler applies pan; hold-going-finer
gate; prev-level touch bias; `rebuildPageTableForDisplayT` on level swap; `needsRedraw` nudges on
all fetch completions; bootstrap-to-floor on first swap; URL knobs `?brickThr`/`?brickBias`/
`?brickHold`; bench chip diagnostics (`missing@dis`/`missing@bnd`, `displayT → boundT`). PR-scale
sequence across ~15 commits.

**B0.5 ✅ shipped 2026-08-29** — Frankenstein hole-fill, default on. `show(t)` and the auto-advance
path snap `displayT` to `boundT` immediately; `rebuildPageTableForDisplayT`'s second pass fills any
grid cell still EMPTY with the same brick position at the previous displayT (LRU-touched with plain
`frameNow` — NOT the `PREV_TOUCH_BIAS` used for prev-level, which was ruinous for prev-t; it evicted
freshly-landed boundT bricks and produced the constant-refresh loop from screenshot #49). Under
playback the readiness probe returns true so scrub never stalls. Hold-on-cold path retired in B8.

**B1 — SKIPPED** by Dominik 2026-08-29. Bench6 blobs (fXgbTl, Dml3RG, f8gzA2) enough evidence.

**B2 ✅ shipped 2026-08-29** — `shouldUseBricks(meta)` + tests. Cache-budget shape:
```ts
export const CACHE_BUDGET_BYTES = 1_500_000_000
export function shouldUseBricks(meta: ViewerMeta): boolean {
  const bytesPerT = meta.nX * meta.nY * meta.nZ * meta.nC * meta.bytesPerVoxel
  if (!Number.isFinite(bytesPerT) || bytesPerT <= 0) return false
  return bytesPerT * Math.max(meta.nT, 1) >= CACHE_BUDGET_BYTES
}
```
Landed pre-bench6 with the earlier `nX * nY < 200 Mpx` gate; reshaped to movie-fits-cache after
bench6 showed fXgbTl (1.4 GB) fits flat cleanly and f8gzA2 needs bricks because flat can't upload
a 17 GB volume at all.

**B3 ✅ shipped 2026-08-29** — Wire the predicate. `bricksEnabled` is a `computed<boolean>` that
consults `meta.value` reactively, with `?bricks=0|1` as a two-way dev override. Script-side reads
use `.value`; template auto-unwraps. `ensureRenderer` snapshots `bricksEnabled.value` inside
`reallocate`, which guards on `meta.value !== null` — so the mount-time semantic is preserved.
- Classification with the 1.5 GB budget:
  - fXgbTl (1.4 GB total) → **flat**
  - Dml3RG (8.5 GB), FtGoJO, SispLk, f8gzA2 → **brick**

**B4 — DIAGNOSIS FOLDED INTO B0.** The intersect guard (`MAX_INTERSECT_BRICKS = 256`, coarsen-until-
fits) that shipped in B0 was outcome (a) from B4's hypothesis, and bench5 confirmed it: f8gzA2's
drawP95 dropped from 200 ms → 2.8 ms without any further shader-scaling work. The "keep f8gzA2 on
flat" outcome is off the table anyway — flat can't render it. No separate B4 write-up needed;
Decision 2 already reflects the fact.

**B5 ✅ shipped 2026-08-31** — Perf blobs recorded on the reference set (bench7). fXgbTl (flat),
Dml3RG (brick L0), f8gzA2 (brick L1); all under the 5 ms `r.draw()` p95 threshold. Details in Perf
ledger below.

**B6 — overlay parity check.** Static-analysis parity confirmed 2026-08-31: brick renderer exposes
the same `setOverlayPoints`/`setOverlaySegments`/`setLabelStyle` interface as flat; label path fetches
a `u32` brick alongside every intensity brick and writes into a 3D `r32uint` atlas texture with
identical slot geometry, silent fallback to intensity-only on miss. Full side-by-side visual sweep
(open the same image in two popups, one `?bricks=0`, one `?bricks=1`, sub-pixel position match on
zolIMa and fXgbTl) still owed and needs Dominik's eyes.

**B7 — STRUCK.** Not a real task after B2+B3+the toggle: auto-select routes small movies to flat
(fXgbTl) and everything else to brick, and the Bricks Auto/Brick/Flat toggle lets the user override
per session for images the predicate gets wrong (Dml3RG 2D is the recorded case). There is nothing
left to "retire" — flat is a legitimate renderer for real cases. The `bricksEnabled === false`
branch stays load-bearing.

**B8 ✅ shipped 2026-08-31** — Hold-on-cold code path deleted. Frankenstein is now the only mode:
`frankensteinEnabled` variable removed; `show(t)` unconditionally snaps `displayT` on any `t` change
(still reports `coreBricksResident(t)` so the caller can distinguish complete vs holey frames);
`tickScheduler` auto-advance always snaps; `rebuildPageTableForDisplayT`'s second pass keeps the
`prevDisplayT >= 0` sanity check (dropping it would build a `brickKey({t:-1, …})` on the initial
frame). `setFrankensteinEnabled` removed from the interface; `?brickFrank` URL knob and the chip's
`frank on/off` readout gone. `anyBricksResident` (only referenced by the hold-on-cold branches) also
removed. Frankenstein confirmed stable across daily use (Dominik, 2026-08-31: "frank works fine.
hold on cold is out"). Retired PRs: this branch.

**B9 ✅ shipped 2026-08-29** — Bricks Auto/Brick/Flat toggle in the VIEW panel + reactive
renderer swap. `settings.viewerBricksMode` persisted to localStorage. Watcher explicitly destroys
`renderer.value` before `reallocate` so the toggle actually swaps renderers — without the destroy
step `ensureRenderer`'s `if (renderer.value) return` short-circuit left the OLD renderer running.

**B10 ✅ shipped 2026-08-29** — Fast plane switch via `setZPlane(zLo)` on both renderers. Brick
renderer keeps its ~64 MB atlas texture allocated and clears the page table + inflight (same
discipline as level swap). Flat renderer bumps a `planeVersion` counter and stamps slots; stale
slots miss on `show`/`hasTimepoint`, and `uploadFrame`'s existing `dropSlot(t)` cleans them
lazily as t's are revisited (no upfront 200 ms `dropAll`). ViewerWindow's `zPump` aborts
in-flight slab fetches before firing `setZPlane` — otherwise old-plane bytes could land in a
slot stamped fresh. `useTiles` and volume mode fall through to full reallocate (different
geometry). Together closes the 1-2 s plane-wheel freeze Dominik hit on Dml3RG 2D.

## Perf ledger (B5, bench7 2026-08-31)

| Image | Predicate → mode | nT | Bytes/t | Draw p50 | Draw p95 | Resident | Level |
|---|---|---|---|---|---|---|---|
| fXgbTl | flat (1.4 GB total, fits budget) | 31 | 47 MB | 0.10 ms | 0.20 ms | 31/31 t | — |
| Dml3RG | brick (8.5 GB movie) | 181 | 47 MB | 0.60 ms | 0.90 ms | 214 bricks | L0 |
| f8gzA2 | brick (17 GB/t) | 1 | 17 GB | 1.20 ms | 2.30 ms | 459 bricks | L1 |

All three land under the 5 ms `r.draw()` p95 threshold. Predicate lands each image on the intended
renderer. Bench blobs: `~/Downloads/TMP/bench7/*.json`.

## Open questions

1. **Is the 1.5 GB cache budget the right cut?** bench6 landed fXgbTl exactly on the flat side
   (1.4 GB total → cap 31/31, playback 0.10 ms) and Dml3RG well on the brick side (8.5 GB). Any
   store between 1.5–3 GB will pick brick; if that turns out to feel worse than a stretched flat
   cache we tune the constant.
2. **Do we ever want `?bricks=1` to force-on when the predicate says off?** Yes for A/B testing, no
   for shipping. Decision 1 already handles this — override in both directions.
3. **Does Frankenstein hole-fill leak prev-t bricks?** LRU still evicts them naturally (plain
   `frameNow` touch, no bias). Under continuous slow-network playback, holes will grow as prev-t
   bricks age out. Not observed in Dml3RG playback so far.

## References

- Bench data (2026-08-29): `~/Downloads/TMP/bench/bench-{image}-{mode}-*.json`
- Predecessor plan: [`KILN_BRICK_PLAN.md`](KILN_BRICK_PLAN.md) (P0–P5 shipped; superseded)
- Perf ratchet: [`NAPARI_WEBGPU_AUDIT.md`](NAPARI_WEBGPU_AUDIT.md) → 5.3 ms MIP budget
- Related renderer contract: `frontend/src/lib/webgpu/volumeRenderer.ts` → `VolumeRenderer.brickResidency()`
- Related PRs (post-KILN_BRICK P5): #691–#704
