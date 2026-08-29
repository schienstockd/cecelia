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

2. **Predicate: `nX * nY < HUGE_XY_THRESHOLD`, brick otherwise.** No `nLevels` gate — bench data
   shows brick's TTFF win covers single-level stores too. `HUGE_XY_THRESHOLD` starts at 200 Mpx
   (fXgbTl 0.2, Dml3RG 1.1, FtGoJO 4.1, SispLk 57 — all under; f8gzA2 343 — over). Refined once
   B4 (f8gzA2 scaling) resolves.

3. **Selection is mount-time.** Mid-session flip needs a full renderer teardown + rebuild that isn't
   worth building for what changes about zero times per session. `bricksEnabled` becomes a
   `computed` read once inside the mount closure; `nLevels`/`nX`/`nY` don't move for one image.

4. **`pickVolumeLevel` stays** in the flat path. Kill Decision 8 of KILN_BRICK_PLAN ("fallback is
   the current path") — that reads as a design retreat. It isn't; flat + `pickVolumeLevel` is the
   RIGHT renderer for the small-image case the predicate points at.

5. **Perf ratchet applies to the case brick REPLACES flat.** The 5.3 ms MIP budget from
   `NAPARI_WEBGPU_AUDIT.md` is a bar for stores the predicate says "use brick" (i.e. mostly the
   multi-level 3D case). Not a bar for fXgbTl, where flat's 0.1 ms is a straw man (CPU submission,
   not real GPU cost).

6. **No new server route.** Brick fetch still goes through `/api/viewer/slab?cTo=nC-1` from #700
   (P0.5 in the old plan). Nothing here needs a server change.

## Phased build sequence

**B1 — Re-baseline post-#702.** One browser session on the five reference images. Save fresh bench
blobs, drop into `~/Downloads/TMP/bench/`. The one number that changes the plan: Dml3RG's brick MB
(pre-fix 7883 MB / post-fix expected < 1 GB). If bytes drop as predicted, Decision 2's threshold
holds. If they don't, we have a second scaling problem alongside f8gzA2.
- Cost: ~10 min at the browser.
- Deliverable: five new JSON blobs, one-line changelog inline in this doc under "What we know".

**B2 — Auto-select predicate + tests.** Pure function in `frontend/src/utils/volumeViewer.ts`
(or a new `brickSelect.ts`, whichever slots better):

```ts
export const HUGE_XY_THRESHOLD_PX = 200_000_000
export function shouldUseBricks(meta: ViewerMeta): boolean {
  return meta.nX * meta.nY < HUGE_XY_THRESHOLD_PX
}
```

Unit test the boundary + each reference image's classification. No wiring yet.
- Deliverable: green `pixi run test-frontend`.

**B3 — Wire the predicate in ViewerWindow.** Replace the `const bricksEnabled = String(route.query.bricks ?? '') === '1'` line with:

```ts
const bricksOverride = route.query.bricks
const bricksEnabled = computed(() => {
  const meta_v = meta.value
  if (bricksOverride === '1') return true
  if (bricksOverride === '0') return false
  return meta_v ? shouldUseBricks(meta_v) : false
})
```

Consumers already reactive (`v-if`, template refs) pick it up automatically. `ensureRenderer` reads
it once during construction; mount-time semantics preserved. Verify by opening each reference image
with no `?bricks=` and checking the bench chip reports the right mode.
- Deliverable: default brick on fXgbTl / Dml3RG / FtGoJO / SispLk, default flat on f8gzA2.

**B4 — f8gzA2 shader-scaling investigation.** Not a fix — a diagnosis + a threshold decision.
Instrument how many bricks the viewport intersect list yields at f8gzA2 nZ=1 × 343 Mpx, and where
the 200 ms drawP95 splits between `tickScheduler` + `pageTableCpu` upload + shader dispatch. Two
outcomes possible:
- (a) intersect list is the whole cost (200+ bricks per frame): cap the intersect count in the
  scheduler (draw far bricks at a coarser level, close bricks at target level — brick's original
  point), OR raise `HUGE_XY_THRESHOLD_PX` so f8gzA2 stays on flat forever.
- (b) something else is 200 ms (LUT rebuild? label texture? bind group?): find that first.
Deliverable: an inline `## f8gzA2 diagnosis` section in this doc, and a `?` -> `!` on Decision 2's
threshold.

**B5 — P5d perf pass on the "brick default" cases.** Formal bench blob per reference image AFTER
B3 lands. Record numbers in this doc under a **Perf ledger** section. Threshold: brick's CPU-side
`r.draw()` p95 must not exceed 5 ms on any store where the predicate says "use brick" (Sispk-shape
being the hard case). If it does, either the shader path needs more work (Session E / F) or the
threshold has to exclude that store.

**B6 — P6 overlay parity check.** Points, tracks, tails, masks position-match the flat renderer to
sub-pixel on zolIMa and fXgbTl. Open both renderers side-by-side (two windows, one `?bricks=0`, one
`?bricks=1`, same image + timepoint). Anything that jumps between the two is a bug.

**B7 — Retire the flat 3D path.** Only if B5 + B6 both green.  Delete `pickVolumeLevel` from the 3D
render loop (keep for the 2D level dropdown); drop `?bricks=` override to `?bricks=1` only (force-on
for override images we haven't found yet); flip `bricksEnabled` to `computed(() => shouldUseBricks(m))`
unconditional. **Do not do this until (a) B5 confirms no regression on the "brick default" cases
AND (b) f8gzA2 either passes B4's threshold check OR the predicate is refined to keep it on flat.**

## Perf ledger (populated in B1 + B5)

*Pending B1 re-baseline.*

## Open questions

1. **Does #702 fully close Dml3RG's 7883 MB thrash?** B1 answers this.
2. **Is f8gzA2's shader cost fixable, or a permanent exclusion?** B4 answers this. If permanent, the
   plan doc for a future f8gzA2-shape store change might supersede this one.
3. **Do we ever want `?bricks=1` to force-on when the predicate says off?** Yes for A/B testing, no
   for shipping. Decision 1 already handles this — override in both directions.
4. **Should the predicate also gate on `meta.bytesPerVoxel`?** r16uint stores double L0 bytes for
   the same pixel count. If `HUGE_XY_THRESHOLD_PX` alone starts letting bad stores through post-B4,
   swap to a `l0BytesPerT > MAX_L0_BYTES` predicate instead. Not urgent — none of the reference
   images cross the line.

## References

- Bench data (2026-08-29): `~/Downloads/TMP/bench/bench-{image}-{mode}-*.json`
- Predecessor plan: [`KILN_BRICK_PLAN.md`](KILN_BRICK_PLAN.md) (P0–P5 shipped; superseded)
- Perf ratchet: [`NAPARI_WEBGPU_AUDIT.md`](NAPARI_WEBGPU_AUDIT.md) → 5.3 ms MIP budget
- Related renderer contract: `frontend/src/lib/webgpu/volumeRenderer.ts` → `VolumeRenderer.brickResidency()`
- Related PRs (post-KILN_BRICK P5): #691–#704
