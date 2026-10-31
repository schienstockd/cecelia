# Motility metrics from the Pizzagalli / Cabini / Wortel review — plan

> **Status: in progress (2026-10-31).** P0 done; P1 (#1862), P4 (#1865) and P2 (#1868) merged;
> P3 not started — scope grew, see *P3 — what building it actually takes*. Plan branch history:
> `docs/motility-metrics-plan`.

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

**P0 findings (2026-10-31)** — answered by reading the code:
- **The tracker itself drops short tracks.** `bayesianTracking` removes every track under
  `minTimepoints` (default 5) before writing (`python/cecelia/utils/tracking_utils.py`, the "filter
  short tracks" step); those cells come back untracked. That is the short-track removal the review
  says amplifies selection bias, applied before any measure runs. Nothing downstream (track measures,
  `track_props`, HMM) drops by length — they return NaN for too-short inputs. Worth an option and a
  form tip once P1's `tracking.duration_bias` finding shows real data is affected; not changed here.
- **btrack bridges gaps** (`max_lost = 5` in `cell_config.json`), so tracks DO contain missing
  frames. `step_speeds` divides by the real Δt, so per-cell speed is gap-safe; P2's windows must be
  windows in TIME (the Δt between positions), never a count of positions. Same for Decision 7's
  bout lengths.
- **Frame interval reaches `track_props`** for free: it holds the image, and `img_physical_sizes(img)`
  returns `(pixel_res, time_step)`.
- **Membership read for arrest**: `pop_membership_fetch` (`gating/popmanager/pop_df.jl`) is the
  canonical reader; costing it per track plot remains for P3.

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

#### P3 — what building it actually takes (found 2026-10-31, before starting)

The plan treated P3 as "a suffix family in `track_props`". Reading the read path, it is three pieces:
1. **Pure bouts** (`tracking/track_bouts.jl`): run-lengths of a per-cell 0/1 or categorical series in
   TIME, with the answered gap rule (bridge a missing detection when in on both sides; a measured
   out frame ends the bout) and start/end censoring; plus a Kaplan–Meier median over bouts.
   Unit-testable alone.
2. **`track_props` family**: `{m}[.{cat}].nBouts/.boutMean/.boutMax/.nCensored`, registered beside
   `_TRACK_NUM_AGG_SUFFIXES` so `track_aggregate_base` inverts them (with `cell_cols` the longest-base
   rule already handles `{m}.{cat}.boutMax`; the name-only rule needs the suffixes stripped first).
   Needs the frame interval (`img_physical_sizes`) inside `track_props`.
3. **Reachability — the part the plan missed.** Nothing computes unless a plot or gate NAMES the
   column (`pop_df` → `track_aggregate_measures`), and the track gate-axis picker today offers only the
   motility table (`/api/gating/channels` `columns`); per-track aggregates of cell columns are offered
   only by the clustering feature picker (`cellObsMeasures` + `trackAggregates`). So P3 needs the
   channels endpoint to list dwell columns for flag/categorical obs, and the axis picker to show them
   (`frontend/src/utils/measureGroups.ts` grouping). Arrest via a gated population (answered Q2) adds
   population membership as flag columns (`pop_membership_fetch`) and a picker entry per population.
Split into P3a (1 + 2 + clustering-picker exposure, contacts/HMM/motif) and P3b (gate-axis picker +
population-membership flags for arrest). P3b changes the gating UI — needs Dominik's eyes.

### P4 — approach toward a target
Decision 5. `trackMeasures` writes per-cell `approach#<target>` and the per-track index for each
`min_distance#*` column present at measure time (re-run it after contacts). Per-track means then come
free from `track_props`. About 3 files.

## Answered (Dominik, 2026-10-31)

- **Q1 → one window per run.** `windowMinutes` on `trackMeasures`; a second time scale is a second
  segmentation run. Decision 2 locked as written.
- **Q2 → arrest via a gated population.** `track_props` takes population membership as per-cell flags
  (through `pop_membership_fetch`) and the dwell family aggregates them; no speed threshold anywhere.
- **Q3 → no smoothed arrest column.** The windowed per-cell speed (P2) is the smooth stop/go signal.
- **Q5 → bridge gaps.** A missing detection with contact (or membership) on both sides continues the
  bout; only a MEASURED out-of-population frame ends it. Censoring at track start/end is unchanged.
- **Q4 shrinks:** dwell is compute-on-read, so no column is stored under a population's name — a
  rename can only break a saved track gate drawn on a dwell column. Key the columns by population
  path (the same rule as `contact#<target>`); revisit only if a renamed gate actually breaks.

## Open questions (Dominik) — as first asked

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
