# Centroid axes: explicit `centroid_x/_y/_z/_t` everywhere (rename + read + gate)

**Status:** planned, not started. Branch `feat/centroid-axes`.

## Problem

Segmentation measurement stores centroids in `obsm['spatial']` labelled with skimage's raw
positional names `centroid-0/1/2` (z,y,x order) and time in `obsm['temporal']` labelled `t`. Two
faults follow:

1. **Positional reading is a latent 2D bug.** Every spatial/tracking consumer maps the centroid
   columns onto a physical-resolution vector (or a z/y/x split) by *trailing-position alignment* —
   it assumes "3 cols = z,y,x, 2 cols = y,x" and zips against the tail of `[sz,sy,sx]`. All current
   project images happen to be 3D, so it's never bitten — but a 2D image would silently produce
   wrong track speeds / distances / neighbour graphs. celltrackR in the old R version proved the
   danger: it wants coordinates in **x,y,z** order, the *opposite* of skimage's z,y,x, and it
   assembled them **by explicit column name** (`cciaImage.R:914-920`:
   `which(colnames == "centroid_x")` …). Explicit-by-name is the only correct contract.
2. **Opaque names + no gating parity.** `centroid-0` doesn't say which axis it is; `t` is
   over-generic. And the gating plots don't expose the spatial/temporal axes at all — in R you could
   *visualise* `centroid_x` vs `centroid_y` on a gating scatter and *gate* on spatial position
   (`createFlowFrame.R` put the whole label-props table, centroids included, into the flow frame;
   `flowPlotManager.R` special-cased spatial scatters). Also: the cookbook Claude reads had a wrong
   example (`centroid_t`), so a Claude-generated notebook copied a nonexistent column and crashed —
   the trigger for this work.

The fix everyone already expects (Claude, the R version, intuition): name the columns
`centroid_x`, `centroid_y`, `centroid_z`, `centroid_t`, and make **every** consumer select by axis.

## Locked decisions

1. **Canonical on-disk names** (the labels in `uns/spatial_cols` / `uns/temporal_cols`):
   - `centroid_x`, `centroid_y`, always present.
   - `centroid_z` — present only for 3D.
   - `centroid_t` — present only for a timecourse.
2. **Keep the `obsm` structure.** Only the `uns` *labels* change; `obsm['spatial']`/`['temporal']`
   matrices stay (still skimage z,y,x column order internally). We are relabelling, not restructuring.
   Not flattening centroids back into flat obs columns (that abandons the deliberate obsm layout).
3. **Keystone: `centroid_columns(order=…)` is the explicit-axis selector; no consumer touches
   positions; `centroid-N` is a hard error.** Following the old R `LabelPropsView.centroid_columns`
   (reference, not gospel): the `order` arg selects **by axis name** — `order=[:x,:y,:z]` → the present
   `centroid_{axis}` columns in that order (z dropped for 2D), *not* a positional `centroid-N` slice.
   A consumer that needs coordinates writes `centroid_columns(lp; order=[:x,:y,:z])` and gets explicit,
   correctly-ordered, present-only axes; `axis_of("centroid_x")==:x` + `physical_size_for_axis(img,:x)`
   map each column to its resolution — never by tail position. **NO legacy bridge, NO fallback.**
   **Strict:** `as_df` validates the centroid names on **every** read (both langs), so a file carrying
   `centroid-N`/bare-`t` is rejected outright — those names can never reach a returned DataFrame, even
   via a non-centroid read. The remedy is the converter (decision 6), not silent normalisation. The
   positional `spatial_cols[k]` / `sp[:,0]=z` pattern is deleted everywhere. One home for the axis
   convention (per the divergent-reimplementation rule); the skimage `centroid-N`→axis mapping exists
   ONLY in the writer/converter namer (`skimage_centroid_axis_names`), never on the read path.
4. **Physical resolution maps by axis name, never by position.** `centroid_x → PhysicalSizeX`, etc.,
   via a helper (`physical_size_for_axis(img, :x)` or a `Dict`), replacing every
   `res = phys[end-n+1:end]` tail-slice.
5. **Gating exposes spatial + temporal axes** for both visualise and gate. The obsm read path and
   gate-eval already handle named centroid columns (verified: `as_df` materialises any
   `uns/spatial_cols`/`temporal_cols` name; the gate-eval and plotdata fetches use the same
   `select_cols → as_df` chain). So this is publish-the-columns + UI, not data plumbing. Port R plot
   behaviour: when both axes are `centroid_x`/`centroid_y`, **fixed aspect ratio + reversed Y**;
   default transform **linear** (not logicle) for spatial/temporal axes.
6. **Generic h5ad converter** (idempotent, dry-runnable) over `projects_dir()/**/labelProps/*.h5ad`.
   Handles *any* prior format so it also covers anyone who used the old R version: (a) Pineapple
   `centroid-N` labels in `uns/spatial_cols` + `t` → explicit names (via `skimage_centroid_axis_names`);
   (b) old-R flat `centroid_x/_y/_z/_t` var columns → lifted into `obsm` with explicit `uns` (generalise
   `legacy_migrate`). This is the remedy the read-time guard points at. Dominik runs it; we don't touch
   his files.

## Blast radius (from discovery — file:line)

### Canonical readers (define the contract; become the accessor's home)
- `app/src/label_props.jl` — `centroid_columns`/`temporal_columns` (name-agnostic), `as_df` `cent_map`
  (name→matrix-offset, fine). **`centroid_columns(; order=)` L239 hardcodes `centroid-N`** — the
  positional `order=` filter; drop it (no caller passes `order=`).
- `python/cecelia/utils/label_props_utils.py` — `centroid_columns`/`temporal_columns` L43-47, `as_df`
  L100-107 (positional against the matrix, authoritative — fine).

### Positional consumers — MUST switch to explicit axis (the correctness fix)
Physical-resolution / coordinate-order sensitive (all trailing-align today):
- `app/src/tasks/tracking/track_measures.jl` L194-214 — `coords = row[spatial_cols[k]] * res[k]`.
- `app/src/tasks/spatialAnalysis/detectAggregates.jl` L54-67 — `scols[j] ↔ scale[j]`; `tcols[1]`.
- `app/src/tasks/spatialAnalysis/cellContacts.jl` L21-26 — `scols[j] ↔ scale[j]`.
- `python/cecelia/tasks/spatialAnalysis/cell_neighbours_run.py` L52-64 — `phys[-n:]` tail align.
- `python/cecelia/utils/spatial_utils.py` L112-123 — `phys[-len(ccols):]`; `tcols[0]`.
- `python/cecelia/utils/tracking_utils.py` L113-135 — positional z/y/x split + hardcoded `"t"` (L104,
  L115, L122); builds the btrack `(x,y,z,t)` frame.
- `napari/napari_bridge.py` L395-423 `_centroid_matrix` (`zip(spatial_axes, centroid_cols)`), L523-557
  `_tracks_matrix` (hardcoded `"t"` display-axis).
- `app/src/tasks/tracking/track_measures.jl` L195 + `hmm_states.jl:38` + `detectAggregates.jl:55`
  temporal-first assumptions.

### Producer + migration (mint the names)
- `python/cecelia/utils/measure_utils.py` `_to_anndata` L357-379 (+ `morph_df['t']` L132) — write
  `centroid_{axis}` + `centroid_t` into `uns`.
- `python/cecelia/tasks/importImages/legacy_migrate.py` L122-130 — already knows the explicit source
  names; stop collapsing to `centroid-N`/`t`, emit the explicit names.

### Gating exposure
- `api/src/gating_api.jl` `api_gating_channels` flow/live branch L333-349 — add `spatialColumns =
  centroid_columns(lp)`; `temporalColumns` already served (L340). No change to gate-eval
  (`gating_engine.jl:35,55`), `gates.jl`, or plotdata (`gating_api.jl:151`) — obsm read already works.
- `frontend/src/stores/gating.ts` L194-221 — read `spatialColumns` + the already-served
  `temporalColumns` (currently dropped); export them.
- `frontend/src/modules/gate/GatePlotPanel.vue` (X/Y selects L333/339, default-axis L276-279, default
  transform L49-54) + `GatePairsPanel.vue` (checklist L146, `syncChannels` L84-89) — add a spatial/
  time `<optgroup>`, accept spatial cols as valid persisted axes, default their transform to linear,
  fixed-aspect + reversed-Y when both axes are `centroid_x`/`centroid_y`.

### Docs
- `docs/DATAMODEL.md` L22-25, L84, L98 (centroid tables), `docs/POPULATION.md` L344, `docs/REPL.md`
  L105 (the wrong `centroid_t` example — becomes correct after rename), `docs/TRACKING.md`,
  `label_props.jl` + `tracking_utils.py` docstrings.

## Phases

**Phase 0 — accessor contract (foundation) — DONE (both langs).** `centroid_columns(order=…)` is the
R-style explicit-axis selector (verbatim `uns` names + order-by-axis); `temporal_columns` verbatim;
`axis_of` parses an axis from a name; `_check_centroid_names` fails loudly on `centroid-N`/bare-`t` on
every read (accessors + `as_df`); the skimage→axis namer lives writer-side only
(`skimage_centroid_axis_names`, Python). Python helper unit-check passing; Julia to be run via
`test-pkg` with Phase 3. *(`physical_size_for_axis` lands in Phase 3 with the readers.)*

**Phase 1 — writer + legacy migrate.** `measure_utils._to_anndata` and `legacy_migrate` emit
`centroid_x/_y/_z` + `centroid_t`. Python unit tests: 2D and 3D produce the right `uns` names.

**Phase 2 — generic converter + fixtures.** A dry-runnable, idempotent converter over
`projects_dir()/**/labelProps/*.h5ad` (excl. `*__tracks.h5ad`) handling both prior formats (decision
6), via anndata (sanctioned structural edit, like `legacy_migrate`). Convert the committed test fixture
and update the Julia/Python assertions (`runtests.jl:2422-2456`, `:2565-2569`). Dominik runs it on his
data.

**Phase 3 — explicit-axis readers (all spatial + tracking modules).** Rewrite every Section-B/C
consumer to select by axis via the Phase-0 accessor and map resolution by axis name. Covers
track_measures, detectAggregates, cellContacts, cell_neighbours, spatial_utils, tracking_utils,
napari bridge. Add a **2D** test path so the positional bug can't regress.

**Phase 4 — gating spatial/temporal axes — core DONE.** Backend serves `spatialColumns`
(`centroid_columns(order=[:x,:y,:z])`) + `temporalColumns` (`["centroid_t"]`). Store carries them +
`spatialAxes`/`isSpatialAxis`. `GatePlotPanel` X/Y pickers get a "Spatial / Time" optgroup, spatial
axes default to the **linear** transform (`axisDefaultTransform`), and persisted spatial axes are
accepted as valid (`ensureChannels`). `GatePairsPanel` checklist + `syncChannels` include them too.
Gate-eval + plotdata need no change (obsm read already works). **Deferred (cosmetic):** the R
fixed-aspect-ratio + reversed-Y for a `centroid_x`×`centroid_y` scatter — touches the regl-scatterplot
renderer; capability (visualise + gate on spatial axes) works without it.

**Phase 5 — docs.** Update DATAMODEL/POPULATION/REPL/TRACKING + docstrings; regenerate the REPL.md
generated API section (`Cecelia.write_repl_doc()`); the golden test guards it.

## Reservations / risks
- The migration mutates every existing label-props h5ad — cheap `uns`-label rewrite, but back up
  first; dry-run shows every change; idempotent.
- Tracking / spatial / napari code paths can't be exercised fully headless — lean on the Phase-0/1/3
  unit tests (incl. the 2D case) + a live check.
- **No legacy fallback (by design):** reads of a pre-migration `centroid-N` file **fail** rather than
  degrade — so the converter MUST be run when adopting this change, or existing projects won't read.
  The fail-fast message names the converter. This is the deliberate "centroid-N not acceptable
  anywhere" contract, not an oversight.
