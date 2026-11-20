# LENS_QUERY_PLAN

Status: **in progress** (2026-11-20) — P0 in review (#2392); P2 built on `feat/lens-query` (score column +
Find similar on the chip, similarity score on a fixed 0–1 scale, lens figures in Movies & figures, Show scores, score
colouring on Population positions), first run seen live; lens figures and plot colouring not yet.

## Goal

Pin the channel lens on a patch of tissue and ask **"find regions like this one"**. The answer comes
from an existing region clustering run, not a new scoring pipeline, and the whole flow happens on the
viewer: pin, ask, see the matches highlighted, adjust one threshold, move on.

From the archived brief `docs/archive/CHANNEL_LENS_POPULATIONS_PROMPT.md` (four ideas). This plan
keeps idea 3 (lens as region query) and idea 1 (lens as population, later phase). Ideas 2 and 4 are
not built — see *Not building*.

## What already exists (2026-11-20)

- **Channel lens** (#2369): `frontend/src/utils/viewer/channelLens.ts`, `lens.wgsl`, state inline in
  `ViewerWindow.vue`. Pin in level-0 px, radius in µm (`settings.viewerLensRadiusUm`), per-population
  counts in the chip, Kiwi ref re-pins it. `docs/UI.md` → *Channel lens*.
- **Region runs store each cell's neighbourhood composition** as obs columns next to
  `regions.{suffix}` (`app/src/tasks/clustRegions/cluster_run.py`, `comp_obs`; names from Julia's
  `compCols`/`otherCol`). Per-frame, per-track (pooled over a whole track) and fixed-window runs all
  write them (#2367, #2380). So the signature of any set of cells is a mean over columns that are
  already on disk, and it is comparable across every image in the run (one shared basis).
- **Transient populations**: the viewer's pick selection is mirrored as a transient
  explicit-label population (`api/src/gating_api.jl` → *Pick-selection registry*), drawn and counted by
  the existing machinery without being saved.
- **Find ROIs** (#1793): `rois/{suffix}.json`, reviewed in the viewer's ROI list.

## Decisions (2026-11-20)

### Decision 1 — the signature is a region run's composition, nothing new

The query signature = the mean composition vector (the run's composition columns) of the cells
inside the pinned lens. No new features in v1: marker intensity, contact stats and density beyond what
the run already carries are out. Behaviour comes in only through a track- or window-pooled region run,
which already folds it into the composition.

### Decision 2 — which cells are "in the lens"

Centroid inside the circle (level-0 px), at the frame on screen for a time-lapse, inside the viewer's
z plane filter for a stack. Fewer than **10** cells → refuse with "Too few cells in the lens (n). Make
it bigger." (`docs/ui/COPY.md`: short = problem, long = action).

### Decision 3 — score and threshold (threshold REOPENED 2026-11-20, see *P1 findings*)

Per cell: cosine similarity between its composition and the lens signature (compositions are
non-negative, so 0–1). A cell is a match when its score ≥ the threshold. Default threshold **Auto** =
the 10th percentile of the lens cells' own scores — the lens defines how tight "like this" is — and the
chip shows the number it picked ("Auto · 0.82"). One slider overrides it. Normalised fractions keep the
score from tracking cell count; a window run's density column is excluded from the signature.

### Decision 4 — the result is a per-cell score column (REVISED 2026-11-20)

Not a transient population, not a pop type, not a region run. A query writes one numeric obs column,
`spatial.lens.<name>` (`lens_score_col`), on every image of the region run: the cosine to the lens's
signature above the run's average cell, 0–1 (`lens_similarity_scores`; a per-cell z against random
lenses until 2026-11-20 — see *Scoring*). The viewer's colour-by shows
it (the score map); an existing filter population on it saves "places like this"; plots and MCP read
it like any cell column. A pop type was rejected (a score is not a membership; ~10 touchpoints for
less), and so were regions (a lens query does not recluster; it would muddle the region heatmap).
New names are `lens1`, `lens2`, … — a query never overwrites another.

### Decision 5 — UX lives on the lens chip

Pinned lens → the chip gains **Find similar**. One region run on this segmentation → use it; several →
a small run picker in the chip (newest first); none → the button explains "Needs a region clustering
run" and links the Spatial page. Results: highlight on the viewer, count + threshold in the chip, a
"Clear" to drop them. No new panel, no new window.

### Decision 6 — scope: this image, then the set

v1 scores the image on screen. Because the basis is shared across a run's images, the set-wide step is
cheap: per-image match counts listed in the chip popover, click to open that image with the matches
shown. Pixel geometry never crosses images; only the signature does.

### Decision 7 — lens as a population (idea 1) is a spatial ROI gate, per image

Stored as parameters (`x`, `y`, `r` in level-0 px, `frame` or `null` for all frames, plus the
segmentation), re-evaluated on demand, owned by one image — not a reusable gate. Combines with marker
gates through the existing boolean pops. Channels belong to the view, not the population.

## Phases

### P0 — prerequisites (hardening, one PR) — BUILT on `lens-pops-prompt`, not seen live

- Lens state → `composables/useVwLens.ts`; chip → `components/viewerWindow/VwLensChip.vue`. No
  behaviour change. (Owed since #2369, convention finding conv-6b11b765.)
- Persist the pin in the view state so a reload keeps it.
- Reconcile radius bounds: `lens_state` (`r > 0`) vs the Kiwi schema (`integer, r >= 1`).

### P1 — validate, no UI (kill test)

Pure Julia `lens_signature` / `lens_similarity` (+ tests on a region fixture) and a REPL call on a
project with a region run. Dominik picks 2–3 lenses he'd call distinct and judges the top matches.

- **Kill**: matches he wouldn't call similar; or scores just reproduce the lens's dominant
  `regions.{suffix}` label (then the feature is "select this region" — a one-click pop, no scoring);
  or scores follow cell density.
- **Pass**: matches he agrees with that cut across region labels. Time it at 154k cells.

**P1 findings (2026-11-20, HuMAP set xxHqjQ, image 2dR91B, region run `default`, 11 phenotypes, 4 regions):**
- ~1% of cells have no composition (NaN, outside the graph); one NaN made every score NaN — fixed
  (`composition_rows`).
- The percentile Auto threshold is rejected: the 10th percentile matched 60–85% of an image (a whole
  region); the median picked by eye is tuning, not a rule.
- Controls (Sonnet review): a per-cell null from 200 random lenses of the same radius; matches overlap a
  local-density baseline by ≤ 0.08 and the dominant region by ≤ 0.14; the same places come back at
  half and double the radius. Caveat: that null is a per-cell calibration, not a significance test (a
  random lens would also pass ~5% of cells); edge-of-tissue truncation not fully excluded. Next: a
  score map + ranking with a per-cell z against the null, Dominik judging retrieval by eye.

### P2 — Find similar (one PR) — BUILT on `feat/lens-query`, not seen live

Task `clustRegions.lensQuery` (hidden, set-scope; `app/src/tasks/clustRegions/lensQuery.jl`) writes
the column (Decision 4) across the run's images (`_clustfeatures_part_of`), banks QC (`nScored`,
`nStrong` ≥ 0.8, `coherence`; warn `lensQuery.average_mix` / `lensQuery.mixed_lens`). The chip's **Find similar** (`composables/useVwLensQuery.ts`)
sends it over the task rail, then colours the view by the new column. Region run picker only when the
image has more than one.

Seen live once (2026-11-20): the min–max ramp put the lens's own cells (the highest z) at the pink top
and most of a matching follicle at yellow — read as "the lens is not coloured". Hence the **fixed scale**
(z 0–5, `fixed_colour_range`, viewer and movie). And "where are my lenses?" had no answer — the colour-by
list is every column, nobody finds a lens there. A viewer Lenses section was built and dropped the same
day for **lens figures**: "in the end they really are figures" (the maintainer) — Find similar saves a
still of the lens to Movies & figures carrying the lens, which gives rename / tags / star / the board for
free; Open re-pins it with its scores *offered* (Show scores, off until asked — locating a lens must not
recolour the view); Delete takes the score column with it. And **Population positions → Colour** (the
score on every image of the run, one scale) covers most of P3's "which images have matches".

**Scoring — z dropped for similarity (2026-11-20, the maintainer's call).** A lens with a real lift (0.17)
in the dominant tissue scored 0 cells at z ≥ 3, and an intravital lens topped out at z 1.13. The z
has a ceiling of about √((1−p)/p) for a lens whose tissue covers a share p of the run (measured: 0.97
VNvvA8, 1.94 lens5, 2.94 lens6; only rare-tissue lenses reached 5), and it ranked the lens's own cells
at the 64th–77th percentile — it measured *distinctive*, not *similar*. Compared on three real images
(six lenses): a stratified null (random lenses in the lens's own region) inverted lens5 (Spearman
−0.43); "% random lenses beaten" was bimodal; a centred cosine tracked the region label (η² 0.70). Plain
cosine above the average cell ranked the lens's cells at the 90th–93rd percentile, passed the region
kill test (η² 0.27 / 0.12), and on lens6 lit two lymphoid aggregates at 0.95–1.0 and nothing else. The
z-era lens columns were deleted, not migrated (unreleased). One guard: coherence (the lens cells' median
cosine to their signature) < 0.8 warns `mixed_lens` — set from six lenses, five at 0.97–1.00 and one
straddling three regions at 0.65.

### Queued — chip readiness line (after #2392 merges and the maintainer has used the lens live)

From LENS_CONTEXT_LAYERS_PROMPT.md (archived on `docs/lens-context-layers-archive`), cut to one item.
`lensChipCounts` counts the layers DRAWN, not what was run — so "no regions" is usually "the Regions
layer is off". When a layer adds nothing to the count, the chip shows one dim line: *not drawn*
("Regions not shown · Show" — switches the existing layer on) or *not run* ("No regions · Cluster
regions" — opens the task form, never starts it). Only layers that apply to the image (no track
clusters on a still). Logic in `useVwLens`, one row type in `VwLensChip`; a unit test per state + a
browser test; the same two states on the lens's Kiwi/MCP surface. Mock: `~/Downloads/TMP/lens_readiness_mock.png`
panel 1. Out of scope: a (?) popover, schematic, readiness endpoint, missing≠zero / provenance /
comparisons.

### P3 — set-wide

Mostly covered by Population positions → Colour (above). Left: per-image match counts as a sort order
on those tiles; open an image with its matches. Consider writing the result
as an ROI set so the Find ROIs list reviews it.

### P4 — Kiwi + MCP

A tool that runs the query from a lens (`set_viewer_view` already pins one); the Kiwi ref carries the
query.

### P5 — save (Decision 7)

Lens as a spatial ROI population; a saved query result as a population.

## Not building

- **Multiple simultaneous lenses**: shader, schema, render payload and UI change for a comparison that
  one lens plus saved lenses (P5) gives.
- **A separate signature/scoring pipeline**: region runs already compute and store the vectors.
- **A lens plot type**: the region composition plot and behaviour cards already show what it would;
  if wanted later, the lens becomes a data source for those.

## Open questions

1. Which project and region run to validate on (P1) — needs Dominik.
2. Cosine vs Jensen–Shannon: cosine first; switch only if P1 shows it misranks.
3. Track-pooled runs: is "the frame on screen" right for picking lens cells, or every frame a track is
   in the lens?
