# Motility metrics from the Pizzagalli / Cabini / Wortel review — plan

> **Status: parked (2026-10-31) — plan only, nothing built.** Branch `docs/motility-metrics-plan`.
> Source: Pizzagalli, Cabini & Wortel (2025), *Quantifying Biological Processes in Motion and Across
> Scales*, in *Bioimaging Modalities in Bioengineering*, pp. 627–656, doi:10.1007/978-3-031-99410-4_29
> (CC BY; open PDF at `susi.usi.ch/documents/335519/files/978-3-031-99410-4_29.pdf`). A review — its
> metric set is celltrackR's and Beltman et al. (2009)'s, which we already ported. This plan is the
> delta, and where each piece fits.

## Goal

Add the measures and guards the review recommends that Cecelia does not yet have, each on an
existing surface (track measures task, track table, `track_diagnostics` battery, a population-input
behaviour task) — no new engine, no new page.

## What we already have (grep-verified on `origin/main` 93a95574)

| Review item | Where it lives |
|---|---|
| speed, duration, path length, displacement, straightness, displacement/outreach ratio, turning angle, overall angle, asphericity | `_TRACK_AGG_COLS`, `app/src/tasks/tracking/track_measures.jl` → `{vn}__tracks.h5ad` |
| step speed / turning angle per cell | `live.cell.speed`, `live.cell.angle` (cell obs) |
| MSD + log-log slope, directional autocorrelation + persistence lag (1/e) | `app/src/tracking/track_diagnostics.jl` (`track_msd`, `track_autocorrelation`, `persistence_lag`) |
| Hotelling T² (bias, unknown direction), with step spacing | `drift_test` — wired as a **drift QC**, not a biology readout |
| pair angle-vs-distance (local alignment) | `analyze_cell_pairs` / `track_pair_drift` in the battery |
| volume-edge artefact (angle to plane, 32.7°) | `plane_angle_profile` / `plane_artefact` |
| HMM states over time, motif sub-tracks, whole-track clustering | `behaviour.hmm_states`, `behaviour.motif_discovery`, `clustTracks` |
| per-frame cell–cell contact | `<popType>.cell.contact#<target>`, `.min_distance#<target>`, `.contact_id#<target>` (`contactsMeshes`, `cellContacts`) |

## Tracklets — the old R had them, the port dropped them

The old R stack ran celltrackR on the fly, so `subtracks()` was an argument away:
`tracks.aggregate.fun(subtracks.i=)` / `tracks.measure.fun(steps.subtracks=)` in
`old-R-shiny-version/R/trackHelpers.R`, and user-facing on HMM as `skipTimesteps` + `subtrackOverlap`
(`tracks.build.hmm.dt`, same file — recomputes `live.cell.speed`/`angle` over `1 + skipTimesteps`
frames). The track-measures GUI call with `steps.subtracks = 10` is commented out
(`inst/app/modules/server/trackingImagesServer.R:244`).

The port precomputes at native resolution, so the HMM knobs became no-ops and were dropped —
recorded in `docs/FUTURE.md` → *Temporal downsampling / overlapping tracklets for behaviour*, which
names the blocker as a **storage decision**. Decision 2 below is that decision.

## Contact time — old-R prior art (analysis Rmds, not the package)

`old-R-shiny-version/vignettes/behaviourCD11c.Rmd:1044-1135` and
`behaviourLymphaticVessels.Rmd:1000-1025` computed, by hand per analysis:
- contact time (min) = frame interval × frames in contact (`contact#….TRUE.n`, from `tracksInfo`);
- unique partners per cell: `contact_id#` → the partner's `track_id`, then `n_distinct`;
- time per partner = total contact time ÷ unique partners — the "time spent scanning on individual
  DCs?" question.

No bouts anywhere. Decision 1b makes these first-class.

## Decisions (proposed — numbered for citation; ✋ = Dominik's call, see *Open questions*)

1. **Arrest is "time in a population", not a speed threshold in a task.** A per-cell filter
   population (`live.cell.speed < 2 µm/min`, gated by the user) carries the threshold. The measure is
   the generic **dwell** summary of a per-cell flag or state per track: fraction of time, number of
   bouts, mean and longest bout (minutes), and censoring counts. The same summary gives **contact
   bouts** (`contact#<target>`), and **HMM / motif state dwell** (the state column) — three users,
   so one implementation (rule of three). Follows the "question in the population" rule; folds in
   the BEHAV3D audit's run-length takeaway; supersedes `BEHAVIOUR_READOUT_PLAN.md` P2's arrest item
   and its open question 4. The fraction alone already exists: `track_props` gives
   `<pt>.cell.contact#<t>.mean` and per-state frequencies `{m}.{cat}`. What is new is the bouts.
1b. **Partner-aware contacts, when the target population is tracked.** A flag on `contact#<target>`
   gives partner-blind bouts: a T cell sliding DC1→DC2 without letting go is one bout, and per
   partner it is two. Partner identity = `contact_id#<target>` (a label) → the partner's `track_id`.
   Add `nPartners` and `timePerPartner` (min) next to the dwell columns, and leave them out when the
   target is untracked. Caveat to document: `contact_id` holds only the NEAREST partner, so
   simultaneous contacts flicker and fragment per-partner bouts. Decision 5 has the same caveat.
2. ✋ **Tracklets = per-cell centred-window columns, window in MINUTES.** `trackMeasures` gains
   `windowMinutes` (0 = off). It writes `live.cell.window.{speed,straightness,displacement}` to cell
   obs, each computed over the centred window of ⌈windowMinutes / frame interval⌉ frames (celltrackR
   `subtracks(i, overlap = i-1)` — maximal overlap). One window per `valueName` run, recorded in the
   track table `uns` and shown in the form (*Auto shows what was picked*). Per-track: median of each
   windowed measure → `live.track.window.{speed,straightness}` (NaN for tracks shorter than the
   window). This resolves all three routes in the FUTURE.md entry at once: minutes, not frames, make
   different frame rates comparable (old `skipTimesteps`); centred windows are the overlapping
   tracklets (old `subtrackOverlap`); and the per-cell columns feed HMM / motif / clustering as
   features with no change to those tasks.
3. **Duration bias is a QC finding, not a footnote.** Fast, straight cells leave the volume sooner, so
   whole-track measures partly measure track length. The battery gains a Spearman ρ of
   `live.track.speed` and `live.track.straightness` against `live.track.duration`. Above a threshold
   (constant in `track_diagnostics.jl`, alongside the others) it banks a `tracking.duration_bias`
   finding: short text "Speed tracks track length (ρ = …)", long text pointing at the windowed
   measures from Decision 2. The `trackDiagnostics` plot adds the scatter. Advisory only, like every
   finding in the battery.
4. **Fürth fit, with a noise term, joins the MSD.** Fit
   `MSD(τ) = 2·N·M·(τ − P·(1 − e^(−τ/P))) + 2·N·σ²` over lags ≤ `max_lag`, which gives the motility
   coefficient M, the persistence time P and the localisation noise σ. M and P are frame-rate- and
   duration-independent, so they join `COHORT_METRICS`. σ is the **noise floor** the review asks for
   (detection jitter inflates speed and turning and deflates arrest, and differs by cell type). If σ
   is large relative to the step length, that is the trigger for track smoothing (see *Not doing*).
   Verify the noise-term form against Thomas et al. 2020 before building.
5. **Directed motion toward a TARGET population, from columns we already write.** Per cell:
   `live.cell.approach#<target>` = −Δ`min_distance#<target>` / Δt (µm/min). Per track:
   `live.track.approachIndex#<target>` = (d_first − d_last) / trackLength, the McCutcheon index with
   "toward the nearest surface of the target population" as the direction (−1 to 1). Generic: the
   target is whatever population the contacts task was pointed at (cells, a segmented vessel, the
   TBM). This replaces a chemotactic index toward a fixed drawn direction, which would be a
   task-specific cue. Caveat to document: the nearest target cell can change between frames
   (`contact_id`), so this is approach to the population, not to one partner.
6. **Dwell is computed on read, in `track_props`, and nothing is stored.**
   `app/src/tracking/track_props.jl` already aggregates any per-cell column per track at read time
   (`{m}.mean/.median/…` for numeric, `{m}.{cat}` frequency for categorical). It is the source for
   track gating and per-track plots, and it never goes stale. Dwell becomes a third aggregate family
   there: for a 0/1 flag (`contact#`, `is.aggregate`) and for each category of a categorical column
   (`hmm.state`, `motif.class`) it adds `{m}[.{cat}].nBouts / .boutMean / .boutMax / .nCensored`.
   Bout lengths are in minutes, so the frame interval has to reach `track_props`. The suffixes are
   registered next to `_TRACK_NUM_AGG_SUFFIXES` so `track_aggregate_base` keeps inverting the
   names. **Arrest** needs population membership as a per-cell flag. ✋ Either `track_props` gains a
   `pops` argument that turns membership into flag columns before aggregating (P0 costs it), or arrest
   rides on an HMM speed state, which the dwell family already covers. Approach (Decision 5) is
   numeric per cell, so its per-track mean comes free once the per-cell column exists. The index
   itself (first/last distance over path length) is one more motility column in `trackMeasures`, and
   only when `min_distance#*` already exists at measure time.
7. **Dwell is censored data and is summarised that way.** A bout that touches its track's first or
   last frame is censored: the cell left the volume, the movie ended, or tracking broke, and all three
   look the same in the data. ✋ The gap rule is open (question 5). Per-track columns carry `nBouts` and `nCensored`. The
   **population-level** bout duration (plot, MCP) is a Kaplan–Meier median, never a mean of observed
   bouts. Check Beltman et al. 2009a's truncation correction before building, in case KM alone is not
   enough.
8. **Order index φ goes in the battery, per frame.** |Σvᵢ| / Σ|vᵢ| over each frame's steps, drawn as a
   curve over time — the review's global-alignment readout, and a direct detector for a field-wide
   event (chemokine release, damage). Cheap, no storage. A per-tile φ in the landscape is optional and
   comes later.

## Phases

Each phase is independently shippable, ordered by value per cost.

### P0 — audit (½ day, no code)
- Where are short tracks dropped today (track measures, plots' min-duration filters, gating
  defaults, HMM `drop_na`)? The review says dropping them makes the selection bias worse; we need the
  real list before P1's text recommends anything.
- Cost of feeding population membership into `track_props` as flag columns (Decision 6's arrest
  route): is there a cheap per-cell membership read (`pop_membership_fetch`?), and does the
  track-gating path call `track_props` once per plot or per gate?
- How the frame interval reaches `track_props` (bout lengths in minutes).
- Confirm `Track` / `_step_dists` treat frame gaps the way Decision 2's windows assume.

### P1 — guards in the battery (no storage change)
Decisions 3, 4 and 8. Touchpoints: `track_diagnostics.jl` (three functions plus the summary fields),
`qc/text.jl` (`tracking.duration_bias`), `COHORT_METRICS`, `api/src/tracking_api.jl` (wire fields),
the `trackDiagnostics` panel (scatter, Fürth line, φ curve), `docs/TRACKING.md` table, golden tests
in `app/test/suite.jl` (Fürth on a simulated PRW with known M/P/σ). About 7 files.

### P2 — windowed (tracklet) measures
Decision 2. Touchpoints: `TrackMeasuresParams` + its JSON spec (`windowMinutes`), the per-cell block
and `_TRACK_AGG_COLS` in `track_measures.jl`, `plotDefinitions/track_measures.json`, the
`cell_cards.jl` list, `docs/TRACKING.md`, and the `docs/FUTURE.md` entry (point it here, mark it
resolved). Tests: the window matches celltrackR `subtracks` + `straightness` on the golden fixture. About 7 files.

### P3 — dwell / bouts (arrest, contact, state)
Decisions 1, 6 and 7. Touchpoints: `track_props.jl` (dwell family, suffix constant, the
`track_aggregate_base` naming rule), its testsets in `app/test/suite.jl` (bouts, censoring, gaps), a
KM dwell plot definition (population-level, per `docs/PLOTS.md`), the MCP summary picks the columns up
(check `mcp/cecelia_mcp/guidance.py`), `docs/TRACKING.md` / `docs/ANALYSIS.md`, and a pointer in
`BEHAVIOUR_READOUT_PLAN.md`. About 6 files, plus the `pops` route for arrest if chosen.

### P4 — approach toward a target
Decision 5. `trackMeasures` writes per-cell `approach#<target>` and the per-track index for each
`min_distance#*` column present at measure time (re-run it after contacts). Per-track means then come
free from `track_props`. About 3 files.

## Open questions (Dominik)

1. **Decision 2 storage.** One window per `valueName` run (proposed), or several windows side by side
   (`live.cell.window5m.speed`, `…window15m…`)? Side by side lets HMM mix scales, at the cost of a
   column per window per measure.
2. **Arrest route (Decision 6).** Population membership into `track_props` (most general; costs a
   membership read on every track plot), or arrest as an HMM speed state's dwell (free once P3
   lands, but needs an HMM run first)?
3. **Smooth arrest.** The review recommends a smoothed arrest coefficient for ML (Pizzagalli et al.
   2019b). Its formula has not been read yet. Worth adding as a column next to Decision 1's fraction,
   or do the windowed speed columns (Decision 2) already cover it?
4. **Column naming by population** (only if the `pops` route wins). Dwell columns keyed by
   population path break when a population is renamed. Key by a stable population id plus a display
   name, as clustering does?

5. **Gap rule inside a bout.** (a) A frame gap ends the bout, censored on both sides — consistent
   with the battery's "a time gap is not a lag". (b) Bridge missing detections when the cell is in
   contact on both sides (nothing shows it let go), and only a measured non-contact frame breaks the
   bout — this stops tracker gaps (`maxLost`) from fragmenting contacts. Proposed by the BEHAV3D
   audit session.

## Not doing (and why)

- **Optical-flow histograms (HOF/HOOF), video action recognition** — for data that cannot be
  tracked; ours can.
- **Kalman smoothing of track positions** — not until P1's σ shows detection noise is material
  ("measure before calling it a bug"). If it does, it becomes a `trackMeasures` option.
- **Chemotactic index toward a fixed, user-drawn direction** — covered generically by Decision 5 with
  the structure segmented as a population.
- **Rayleigh test** — Hotelling already covers unknown-direction bias; Rayleigh adds a 2D-only
  variant.
- **Per-image internal-control normalisation** (the review's fix for movie-to-movie variation) — an
  analysis-board feature rather than a measure; belongs in `docs/FUTURE.md` if wanted.
- **Simulation-based robustness checks** — celltrackR's simulators were not ported; out of scope.
