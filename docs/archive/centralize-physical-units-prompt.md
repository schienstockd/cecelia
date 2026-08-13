> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

# Centralize pixel→µm conversion for pop_df centroids

## Context

PR #491 fixed tracking to scale centroids to µm before btrack, because it's
the only spatially-aware task that wasn't calling `img_physical_sizes`. That
PR considered and rejected a `physical=true` option on `pop_df`, reasoning
that only 1 of 10 call sites needed it.

That reasoning under-weighted two things:
- REPL/notebook use of `pop_df` requires manual µm conversion every time,
  with no guardrail against forgetting or getting it wrong.
- The PR itself flagged that 8 `pop_df` consumers use pixel centroids today,
  and 2 of them pool across images with different pixel sizes — the same bug
  class PR #491 just fixed for tracking, latent elsewhere.

Goal now: one canonical conversion, reused everywhere, not five
implementations of the same math.

## Requirements

1. A single function that scales whichever of `centroid_x/y/z` are present
   on a DataFrame, given one `CciaImage`, using `img_physical_sizes(img)`.
   No-op for any centroid column that's absent.

2. `pop_df` gets a `physical::Bool=false` kwarg. Per the current usage
   pattern (`pop_df(img, pop_type, pops; value_name=vn)`), one call is
   always scoped to one image — so scaling can happen once in-place before
   returning, using that same `img`. No per-row image lookup should be
   needed for this path.

3. Every existing production consumer that already does its own pixel→µm
   conversion (`track_measures`, `cellNeighbours`, both mesh tasks,
   tracking's centroid scaling from PR #491) should be refactored to call
   this same function instead of their own inline math.

4. Any consumer that pools centroids across multiple images (different
   pixel sizes per image) must scale each image's contribution individually
   — before concatenation, never after — since a pooled table has no single
   physical size to apply. This is not limited to the 2 sites named in
   PR #491: check every path that assembles a population data frame across
   images, and across pop types (`flow`, `live`, and any others), for the
   same pattern.

5. The setting must cover every existing access path that creates
   population data frames, not just `pop_df`/`_pop_df` — including any
   write-back path (population data or derived columns saved back to disk,
   e.g. H5AD/label_props) that persists centroids. A write-back that saves
   unscaled pixel centroids while `physical=true` reads return µm would
   reintroduce the exact two-coordinate-systems bug PR #491 fixed.

6. Audit both language boundaries for consistency, not just Julia. Cecelia
   is Julia + Python (skimage/napari side, e.g. `tracking_utils.py`,
   `napari_bridge`). Wherever the Python side reads or writes centroids —
   including the still-open `napari_bridge._tracks_matrix` issue noted in
   PR #491 — confirm it agrees with the Julia side on which coordinate
   system (px vs µm) it's using at each handoff. A fix that's centralized
   in Julia but silently diverges from what Python assumes reintroduces
   the same class of bug across the language boundary instead of within
   one language.

7. Check that any GUI/form unit labels (Vue side) match whatever the
   centralized function actually does — PR #491 already found mismatched
   labels once (`distThresh` said "(physical units)" while
   `maxSearchRadius` said "(px)"); a new shared flag is a fresh chance to
   introduce the same kind of mismatch if labels aren't audited alongside
   the code.

8. Confirm uncalibrated-image fallback behaviour (PR #491: falls back to
   unscaled pixels rather than assuming 1 px = 1 µm) is preserved
   consistently across every access and write-back path this change
   touches, not just the ones PR #491 fixed.

## Ask

- Find the current per-consumer conversion code. Confirm they're all doing
  the same `[sz, sy, sx] × centroid` math, or flag drift/bugs between them.
- Locate `pop_df`'s public wrapper and confirm `img` is in scope at the
  point `_pop_df` returns, so `physical` can be added cleanly.
- Enumerate every access path that builds a population data frame —
  including ones outside `pop_df` — and every path that pools across
  images and/or across pop types. For each, check whether centroids are
  scaled before or after pooling.
- Enumerate every write-back path that persists population data
  (centroids or anything derived from them) and confirm what coordinate
  system it writes in, so reads and writes stay consistent.
- Trace every point where centroids or derived distances cross the
  Julia/Python boundary and confirm both sides agree on units at that
  handoff.
- Check GUI/form labels and tooltips for any parameter this change
  touches, so units stated in the UI match what the code does.
- Judge where the shared function should live — next to
  `img_physical_sizes` in `population_manager.jl`, or a separate
  units/physical-conversion module — based on existing module structure.
- Do not assume signatures, column names, or the full list of access/
  write-back paths beyond what's confirmed in the actual code; read before
  proposing a diff.
