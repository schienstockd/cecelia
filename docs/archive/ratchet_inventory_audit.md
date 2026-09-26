**ARCHIVED — audit findings.** Not authoritative. Point-in-time inventory of ratchet-shaped
enforcement mechanisms in the Cecelia repo, cross-checked against the size-cap meta-ratchet added
to four baselines in `c7d7a752` (branch `docs/drift-prevention-archive`). Companion to
[`drift-prevention-audit`](drift_prevention_mechanism_audit.md) and
[`DRIFT_PREVENTION_ASSESSMENT.md`](../todo/DRIFT_PREVENTION_ASSESSMENT.md). If a grep leads here,
the current design lives with the ratchets themselves — read the tests, not this file.

# Ratchet inventory audit

**Scope.** Every ratchet-shaped enforcement (a `@testset` / `unittest.TestCase` / vitest `describe`
that fails when the codebase moves away from a canonical helper or grows a baseline it wasn't
supposed to grow). Column "cap?" is the `_BASELINE_MAX` / `*_BASELINE_MAX` meta-ratchet from PR
`c7d7a752`. Column "coverage?" is a `@test coverage >= N` / `test_scan_reaches_known_*` /
equivalent sanity assertion that the scan actually walked the intended surface. Git state: audit
branch is off `origin/main` at `580f87df`, which pre-dates the size-cap PR (on
`docs/drift-prevention-archive`, unmerged) — "cap? YES" reflects that PR's diff, not HEAD.

## Table of every ratchet found

| file:line | testset / method | guards | canonical helper it points at | cap? | coverage? |
|---|---|---|---|---|---|
| `app/test/suite/ratchets.jl:19` | `label_props boundary types` | `add_obs` refuses String/Bool cols by name; `CategoricalObsColumn` constructors error on missing `name` | `Cecelia._assert_float_convertible`, `CategoricalObsColumn`, `write_categorical_obs` | n/a (behavioural, not a baseline) | n/a |
| `app/test/suite/ratchets.jl:90` | `typed params ratchet — _run_task reads params through parse_*_params` | every task's `_run_task` reads through `parse_<task>_params(::AbstractDict)` — `TYPED_PARAMS_MIGRATION_BASELINE` (empty) + `STRUCTURAL_EXEMPTIONS` (5) | `parse_<task>_params` / `Base.@kwdef` structs | **YES** (`TYPED_PARAMS_MIGRATION_BASELINE_MAX = 0` in c7d7a752) | YES — `@test length(canonical) >= 41` at line 267 |
| `app/test/suite/ratchets.jl:282` | `cohort-metrics ratchet — write_qc callers are in COHORT_METRICS or marked exempt` | `write_qc` callers registered in `COHORT_METRICS` or carry `# COHORT-EXEMPT:` — `COHORT_METRICS_BASELINE` (6) | `COHORT_METRICS` in `app/src/qc_cohort.jl` | **YES** (`COHORT_METRICS_BASELINE_MAX = 6` in c7d7a752) | YES — `@test coverage >= 20` at line 356 |
| `app/test/suite/ratchets.jl:366` | `zarr-access ratchet — Julia files don't 'using Zarr' outside the sanctioned reader` | exact exempt set (1: `api/src/image_render.jl`) for `using Zarr` / `using EzXML` / `using LightXML` | `api/src/image_render.jl` (narrow display reader) | n/a — exempt set has fixed size 1 by construction | YES — `@test isfile(exempt_path) && occursin(r"\busing\s+Zarr\b", …)` at line 402 |
| `app/test/suite/task_validation.jl:19` | `channelSelection params resolve through channel_indices` | every task with a `channelSelection` param calls `channel_indices` in its handler family | `Cecelia.channel_indices` (`app/src/model/image.jl`) | n/a (walks task registry, no baseline) | YES — `@test length(checked) >= 8` at line 66 |
| `app/test/suite/task_validation.jl:69` | `zarr access routes through the canonical helpers` | no bare `zopen`/`Zarr.open` or literal `.zattrs`/`.zarray`/`zarr.json` reads outside `importImages/omezarr/` and `image_geometry.jl` | `ngff_attrs` / `ngff_multiscales` / `zarr_array_meta` / `open_level` | n/a (closed exempt paths, 1 file + 1 dir family) | YES — `saw_literal_owner` + `saw_open_owner` at lines 153-154 |
| `app/test/suite/task_validation.jl:157` | `a process exit check also checks termsignal` | any `.exitcode` reference has a `termsignal` check within 6 lines | canonical: `proc.exitcode == 0 && proc.termsignal == 0` | n/a (no baseline) | NO (no coverage floor — but the check is line-by-line) |
| `app/test/suite/task_validation.jl:185` | `dirPath param validation` | rejects file-as-dir / non-string dirPath | `_validate_params_against_spec` | n/a (behavioural) | n/a |
| `app/test/suite/task_validation.jl:219` | `units written into OME-XML are schema-valid symbols` | every OME-unit writer routes through `ome_xml_unit_name` (per-directory) | `ome_xml_unit_name` | n/a (no baseline, dir-aggregate) | implicit (walks both `_app_src` and `_api_src`) |
| `app/test/suite/task_spec_ratchets.jl:12` | `numeric param ranges are plausible` | max/default sanity + max/default ratio ≤ `RATIO_MAX=50`; `ALLOWED_WIDE = []` | `each_spec_param` (spec walker) | NO — `ALLOWED_WIDE` starts empty, no size cap | YES — `@test length(nums) > 20` |
| `app/test/suite/task_spec_ratchets.jl:54` | `task spec tips stay short` | every tip ≤ `COPY_MAX=90` chars; `ALLOWED = []` (allow-list of long survivors) | `each_spec_param` | NO — `ALLOWED` starts empty | YES — `@test nspecs > 20 && length(tips) > 100` |
| `app/test/suite/task_spec_ratchets.jl:117` | `a handler fallback never contradicts its spec default` | text-scan `get(params, "k", default)` fallbacks match JSON spec default | `_validate_params_against_spec` (implicit) | n/a (no baseline) | implicit (walks tasks tree) |
| `app/test/suite/task_spec_ratchets.jl:170` | `every task spec field is declared and documented` | every param-field key is in `frontend/…/types.ts` OR Julia src, AND in `docs/MODULES.md` | `types.ts` `ParamDef`, `docs/MODULES.md` | n/a (structural: read-both-sides) | n/a |
| `app/test/suite/task_spec_ratchets.jl:219` | `optionsFrom fills a picker from a named source` | `optionsFrom`/`options` merge is unique-per-value; `list_cellpose_models`/`list_coastal_models` match | `_apply_options_from!` | n/a (targeted assertions) | n/a |
| `app/test/suite/task_spec_ratchets.jl:282` | `showIf conditions name a param that exists` | showIf keys point at declared params (bounded across sections) | walker | n/a | n/a |
| `app/test/suite/task_spec_ratchets.jl:315` | `a param that says segmentation reads SEGMENTATIONS` | `valueNameSelection` labelled "segmentation"/"label set" uses `field: labels` | walker | n/a | n/a |
| `app/test/suite/task_spec_ratchets.jl:345` | `a picker gates on labels only when the task needs the MASK` | `field: labels` requires MASK access (`img_labels_path`/`labelsPath`/…); `ALLOWED = Set(["bayesian_tracking.json"])` | grep on task .jl + `_run.py` | NO — 1 explicit deliberate exemption | n/a (walks spec_dirs) |
| `app/test/suite/task_spec_ratchets.jl:398` | `every task param carries a tip` | every settable param has non-empty `tip`; `ALLOWED_NO_TIP = []` | `each_spec_param` | NO — `ALLOWED_NO_TIP` empty | YES — `@test length(params) > 150` |
| `app/test/suite/task_spec_ratchets.jl:454` | `task spec copy follows the house style` | no Title Case labels, no trailing-period tips, no banned verbs; `ALLOWED_TITLE_CASE = []`, `ALLOWED_TRAILING_PERIOD = []` | `each_spec` / `each_spec_param` | NO — both empty | YES — `@test length(labels) > 150` |
| `app/test/suite/model.jl:567` | `no hand-rolled state writes` | write-mode `open(…, "w")` restricted to 5 exact literal-key entries in `allowed::Dict` | `write_atomic` / `write_json_atomic` | NO — `allowed` dict of 5 exact call-site strings, growable | implicit (walks `app/src`, `api/src`) |
| `app/test/suite/tracking.jl:924` | `every run_py call forwards on_progress` | every `run_py("<script>", …)` mentions `on_progress`; `EXEMPT = Set` (2 metadata-only scripts) | `run_py` | NO — `EXEMPT = Set` of 2 (small structural) | YES — `@test checked >= 20` |
| `app/test/suite/tracking.jl:986` | `task callbacks are bound where they are used` | `on_log`/`on_progress`/`on_process` are in scope where used (parses each file) | AST walk | n/a | YES — `@test scanned >= 40` at line 1132 |
| `app/test/suite/config.jl:84` | `cecelia_version agrees with CITATION.cff and package.json` | four files agree on version string | `Cecelia.version()` | n/a | n/a |
| `app/test/suite/config.jl:114` | `fixtures stay small` | every file ≤ 1 MB; total ≤ 8 MB | fixture policy in `docs/DEV.md` | YES — hard 1 MB per-file and 8 MB total cap (`CAP = 1024*1024`) | implicit (walks `test-data/`) |
| `app/test/suite/config.jl:403` | `release bundle ships every runtime path` | every runtime-required file is in the bundle list; `@test length(required) >= 10` | `_release_bundle_paths` | n/a | YES — `>= 10` floor |
| `app/test/suite/lablog_and_validation.jl` (`Param validation — bounds sweep`) | sweeps every registered task; `checked_bounds >= 25`, `checked_selects >= 8`, `checked_required >= 2` | `_validate_params_against_spec` | n/a | YES — 3 loose floors |
| `api/test/suite/api_src_ratchets_and_chainsave.jl:20` | `API: response bodies go through write_http_body!` | no `write(stream, …)` in `server.jl`; ≥4 uses of `write_http_body!` | `write_http_body!` (`Cecelia/utils.jl`) | n/a | YES — `count(...) >= 4` |
| `api/test/suite/api_src_ratchets_and_chainsave.jl:37` | `API: no bare nrow in api/src` | no `nrow(` outside comments in `api/src/**` | `length(df.<col>)` | n/a (0-tolerance) | implicit |
| `api/test/suite/api_src_ratchets_and_chainsave.jl:56` | `API: chain save validates and repairs the start dot` | dot repair + rejects invalid fn / cycles / dangling / bad-param / ghost-start | `_prune_to_start`, `api_chains_save` | n/a (behavioural) | n/a |
| `python/cecelia/tests/test_zarr_access_convention.py:127` | `ZarrAccessConventionTest` | no bare `import zarr`/`tifffile`/`xml.etree`/`lxml`/`*.from_zarr` outside `_BASELINE` (5 files) | `zarr_utils.open_as_zarr` / `read_axes` / `staged_store`, `ome_xml_utils.load_ome_xml` | **YES** (`_BASELINE_MAX = 5` in c7d7a752) | YES — `ZarrAccessScanCoverageTest.test_scan_reaches_known_zarr_util_call_sites` |
| `python/cecelia/tests/test_zarr_access_convention.py:176` | `DaskInRunnersConventionTest` | no `import dask*` in `app/src/tasks/**` without a `# DASK-OK:` marker | `zarr_utils.open_as_zarr(..., as_dask=True)` / `zarr_utils.read_timepoint` | n/a (marker-based, no baseline) | shares scan coverage above |
| `python/cecelia/tests/test_zarr_access_convention.py:157` | `test_baseline_still_needed` | every entry in `_BASELINE` still uses a banned import (list may only shrink) | n/a (baseline sanity) | n/a | n/a |
| `python/cecelia/tests/test_task_json_convention.py:62` | `TaskJsonCollapseConventionTest` | task JSONs with > 6 top-level params must have a `type: "section"`; `_BASELINE` (5 files) | `segment.cellpose`/`segment.coastal`/`segment.ridges` shape | **YES** (`_BASELINE_MAX = 5` in c7d7a752) | YES — `TaskJsonScanCoverageTest` |
| `python/cecelia/tests/test_task_json_convention.py:89` | `test_baseline_still_needed` | each `_BASELINE` entry still exceeds `_MAX_TOPLEVEL` and has no section | n/a | n/a | n/a |
| `python/cecelia/tests/test_store_staging_convention.py:82` | `StoreStagingConventionTest` | `create_multiscales`/`open_multiscales_for_writing` / write-mode `open_group`/`open_array` route through `staged_store`; `_EXEMPT` (2 files) | `zarr_utils.staged_store` | NO — `_EXEMPT` is a set of 2 files, uncapped | YES — `test_the_scan_actually_reaches_the_writers` (`>= 6` staged callers) |
| `python/cecelia/tests/test_store_compressor_convention.py:97` | `StoreCompressorConventionTest` | every `create_array`/`create_dataset`/write-mode `open_array` declares `compressor=`/`compressors=`/`**_codec_kwargs(...)`; no Blosc/Zstd/LZ4/GZip constructor outside `zarr_utils.py` | `store_compressor` / `store_codecs` / `_codec_kwargs` in `zarr_utils` | n/a (0-tolerance, no baseline) | n/a |
| `python/cecelia/tests/test_streaming_convention.py:153` | `StreamingReadConventionTest` | per-timepoint reads from a tainted zarr level go through `read_timepoint`, or the file carries `# STREAMING-READ-EXEMPT: <why>` | `zarr_utils.read_timepoint` | n/a (marker-based) | YES — `StreamingConventionScanCoverageTest` |
| `python/cecelia/tests/test_streaming_convention.py:182` | `StreamingWriteConventionTest` | no `create_multiscales(np.stack(…), …)` — writes stream through `open_multiscales_for_writing` (or `# STREAMING-WRITE-EXEMPT:`) | `zarr_utils.open_multiscales_for_writing` | n/a (marker-based) | shares coverage above |
| `python/cecelia/tests/test_measure_pass_column.py:252` | `test_the_preview_still_numbers_passes_the_same_way` | cross-language ratchet: TS `passLabel` still adds `n + 1` (parity with Python `pass_display_name`) | `zarr_utils.pass_display_name` / `taskPreview.ts` `passLabel` | n/a (paired-source assertion) | n/a |
| `python/cecelia/tests/test_norm_cache.py:40` | `test_the_name_is_not_a_json_sidecar` | norm-cache sidecar filename must not end in `.json` (sidecar discovery convention) | `norm_cache.path_for` | n/a | n/a |
| `python/cecelia/tests/test_doc_index_convention.py:82` | `TodoIndexIsComplete` + siblings | `docs/todo/README.md` names every plan; every row points at a real file; status labels agree | index conventions | n/a | n/a |
| `python/cecelia/tests/test_doc_pointer_convention.py:96` | `DocPointerConventionTest` | every markdown link resolves; code citations of doc paths resolve; nested `CLAUDE.md` files are named in the root | pointer conventions | n/a | n/a |
| `frontend/src/utils/cssScenarios.test.ts:210` | `hand-rolled UX scenarios` | `findReimplementedScenarios` returns []; `ALLOWED_SCENARIOS = []` | `.cc-muted` / `.cc-empty` / `.cc-eyebrow` / `.cc-readout` utilities | NO — empty allow-list; adding entries visible in diff but no `_MAX` | YES — `expect(sources.length).toBeGreaterThan(100)` |
| `frontend/src/utils/cssScenarios.test.ts:248` | `scoped overrides of a global utility` | no scoped rule shadows a `.cc-*` utility | `utilityRules` | n/a (0-tolerance) | YES — `Object.keys(utils).length > 10` |
| `frontend/src/utils/cssScenarios.test.ts:269` | `the shared size ladder` | `.cc-fs-*` declared after `.cc-muted`/`.cc-empty`/`.cc-readout`/`.cc-eyebrow` in `style.css` | source-order assertion | n/a (structural) | n/a |
| `frontend/src/utils/cssScenarios.test.ts:284` | `shadowed utilities` | `findShadowedUtilities` matches `ALLOWED` (3 items, per-selector deliberate) | `utilityRules` | NO — `ALLOWED` uncapped (3 items today) | shared |
| `frontend/src/utils/cssScenarios.test.ts:323` | `icon-only buttons` | `findHandRolledIconButtons` matches `SEG_BUTTONS` (4 items) exactly | `.cc-btn-icon` / `.cc-btn-bare` / `.cc-btn-ghost` | NO — `SEG_BUTTONS` uncapped (4) | n/a (exact-list) |
| `frontend/src/utils/cssScenarios.test.ts:353` | `raw colours` | no scoped hex value equals a declared token | `colourTokens` | n/a (0-tolerance) | YES — `Object.keys(tokens).length > 10` |
| `frontend/src/utils/cssScenarios.test.ts:376` | `form controls` | no restated-input-base decl outside `ALLOWED` (2 items) | `inputBase` | NO — `ALLOWED` uncapped (2) | n/a |
| `frontend/src/utils/cssScenarios.test.ts:410` | `raw sizes and radii` | no raw scale value outside `ALLOWED` (1 item) | `findRawValues` | NO — `ALLOWED` uncapped (1) | n/a |
| `frontend/src/utils/cssScenarios.test.ts:425` | `tooltip sizing` (continues) | (behavioural; pins `.p-tooltip` container width vs `p-tooltip-text`) | PrimeVue Aura tokens | n/a | n/a |
| `frontend/src/utils/uiCopy.test.ts:307` | `UI copy stays short` | `ALLOWED_LONG = []`, `ALLOWED_MULTI = [1]` | `tooltipStrings`/`hintStrings`, `isTooLong`, `isMultiSentence` | NO — both uncapped; `ALLOWED_MULTI` has 1 entry today | YES — `expect(sfcs.length).toBeGreaterThan(50)` |
| `frontend/src/utils/uiCopy.test.ts:349` | `UI copy is written the house way` | `ALLOWED_TITLE_CASE = []`, `ALLOWED_TRAILING_PERIOD = []`, banned verbs zero-tolerance | `attrStrings`, `tooltipStrings` | NO — both uncapped (empty) | YES |
| `frontend/src/utils/uiCopy.test.ts:405` | `every settable control has a tooltip` | `ALLOWED_NO_TOOLTIP = []`; `ALLOWED_NESTED = []`; `ALLOWED_SIDEWAYS = []`; unnamed `CcToggle` = zero; duplicate tooltips = zero | `uncoveredControls`, `unnamedToggles`, `nestedTooltips`, `misplacedTooltips`, `duplicateTooltips` | NO — three uncapped allow-lists (all empty today) | YES |
| `frontend/src/utils/continuousControls.test.ts:73` | `DECLARED_SINKS` — every side-effecting slider handler names sink | `DECLARED_SINKS` (5) named-and-justified per file | `debouncedLatest`/`rafCoalesce`/`debouncedSave` | NO — 5 entries, uncapped; stale-honesty check exists at line 100 | YES — `expect(sources.length).toBeGreaterThan(50)` |
| `frontend/src/utils/continuousControls.test.ts:174` | `DECLARED_TIMERS` — nobody hand-rolls a fourth debounce | `DECLARED_TIMERS` (13) named-and-justified per file | canonical schedulers above | NO — 13 entries, uncapped; stale-honesty check exists at line 210 | YES |
| `frontend/src/utils/continuousControls.test.ts:160` | `no text field lets the DOM drift from its binding` | `driftingTextFields()` returns [] | `useFieldDraft` | n/a (0-tolerance) | n/a |
| `frontend/src/utils/continuousControls.test.ts:223` | `live viewer view-property endpoints have exactly one owner` | only `utils/viewerOverlays.ts` POSTs to `/api/viewer/(set-z-view|apply-view-state)` | `utils/viewerOverlays` | n/a (0-tolerance) | YES — `all.length > sources.length` |
| `frontend/src/utils/continuousControls.test.ts:249` | `RO_EXEMPT` — no ResizeObserver re-renders into observed elt | `RO_EXEMPT` (10) files exempt, `RO_SCHEDULED` (4) require `schedule` on the observer line | `usePlotResize` (`rafCoalesce`) | NO — `RO_EXEMPT` uncapped (10) | n/a (stale-honesty check at line 318) |
| `frontend/src/composables/dataRefreshCoverage.test.ts:42` | `task-refresh coverage` | `MUST_REFRESH` (13, positive) + `EXEMPT` (10) files | `useDataRefresh` | NO — `EXEMPT` uncapped (10); `MUST_REFRESH` is a positive list (adding is OK) | YES — `expect(sources.length).toBeGreaterThan(100)` + fork-of-toggle check |
| `frontend/src/plots/export.test.ts:156` | `svg roots all come from svgDoc` | only `plots/export.ts` (owner + 2 documented exemptions in `ROOT_EXEMPT`) may hand-roll an `<svg>` root | `svgDoc` (owns `xmlns:xlink`) | NO — `ROOT_EXEMPT` uncapped (2) | YES — `Object.keys(SRC).length > 100` |
| `frontend/src/utils/landscape.test.ts:326` | `envelope size ratchet (LANDSCAPE_COMPLEMENTARY_PLAN Phase 5)` | 32×32 v2 envelope serialises < 700 KB (measured 640 KB, +60 KB slack); floor > 400 KB | schema | YES — hard byte cap `700 * 1024` | n/a (structural — deterministic input) |
| `frontend/src/utils/setupOrder.test.ts:136` | `no setup-order hazards in the app` | `setupOrderHazards()` returns []; no watch source names a const declared below it | `setupOrderHazards` | n/a (0-tolerance) | YES — `Object.keys(RAW).length > 50` |
| `frontend/src/utils/booleanProps.test.ts:37` | `no SFC compares a bare optional boolean prop to undefined` | `booleanUndefinedChecks()` returns [] | `optionalBooleanProps`, `booleanUndefinedChecks` | n/a (0-tolerance) | n/a |
| `frontend/src/utils/webgpuBindings.test.ts` | `every pipeline uses the ONE shared bind group layout` | 0-tolerance scan of viewer WebGPU pipelines | shared BGL | n/a (0-tolerance) | n/a |
| `frontend/src/utils/taskModule.test.ts:109` | `the module tint has one derivation` | no site reading `taskModule` palette appends an alpha hex itself | `moduleTagStyle` | n/a (0-tolerance) | YES — self-test at line 120 + `Object.keys(SOURCES).length > 100` |
| `frontend/src/components/kiwi/kiwiNamingRatchet.test.ts:33` | `kiwi/ naming ratchet` | no `\bClaude\b` in `components/kiwi/*` (after stripping comments and `// ratchet: cite-technical` lines) | provider-neutral wording rule | n/a (marker-based, 0-tolerance) | YES — glob-sanity at line 44 |

## Gaps — baselines without a size cap that plausibly need one

Criterion: a growable exemption list on a ratchet that gates real correctness, matching the shape
of the four already-capped baselines. Empty allow-lists (`[]` is its own cap) are only listed if
the surrounding file is regularly patched.

1. **`test_store_staging_convention.py:29` — `_EXEMPT` (2 files).** Same family as the two Python
   `_BASELINE`s just capped; per-file exemption on a data-integrity convention. `_BASELINE_MAX = 2`
   closes the whole `test_*_convention.py` set.
2. **`app/test/suite/model.jl:576` — `allowed::Dict` (5 exact call-site literals).** Governs
   hand-rolled `open(_, "w")` outside `write_atomic`; the failure mode (silent state truncation) is
   the class that motivated the four caps. Cap on dict size or `sum(length, values(allowed))`.
3. **`continuousControls.test.ts:174` — `DECLARED_TIMERS` (13).** The "no fourth debounce" rule was
   the audit case that made drift expensive; every entry is a named exemption. A cap at 13 makes an
   added exemption a visible bump.
4. **`continuousControls.test.ts:249` — `RO_EXEMPT` (10).** Same shape; ResizeObserver-into-
   observed-element is a real-bugs-not-style ratchet.
5. **`dataRefreshCoverage.test.ts:42` — `EXEMPT` (10).** Governs the "everything refreshes on task
   completion" chokepoint. Same shape.
6. **`cssScenarios.test.ts:301` (`ALLOWED`, 3), `:339` (`SEG_BUTTONS`, 4), `:385` (`ALLOWED`, 2),
   `:414` (`ALLOWED`, 1).** Per-selector carve-outs on the UI-primitives ratchet. `SEG_BUTTONS`
   ranks highest — icon-button drift is exactly what the whole check exists to catch.

## Non-gaps — baselines that legitimately don't need a cap

- **`ratchets.jl:100` — `STRUCTURAL_EXEMPTIONS` (5).** Closed set: `CciaTask`/`CompositeTask`
  dispatcher plus 3 fixture tasks under `testTasks/`. Growing it requires a code re-org that is
  itself visible in review.
- **`tracking.jl:960` — `EXEMPT` (2).** Two `run_py` calls reading one metadata header value each;
  criterion is stated inline ("a user will never wait on it") and the class is bounded.
- **`task_spec_ratchets.jl:369` — `ALLOWED = Set(["bayesian_tracking.json"])`.** Single deliberate
  carve-out; adding a second requires the same inline argument.
- **`api_src_ratchets_and_chainsave.jl:20-25`** — numeric floors on canonical-helper usage, not a
  growable baseline.
- **Every `ALLOWED_* = []` in `task_spec_ratchets.jl` and `uiCopy.test.ts`.** Empty list IS the
  cap; the equality assertion rejects any added entry unless the list is also edited.
- **`plots/export.test.ts:149` — `ROOT_EXEMPT` (2).** Exemption is on two keyed BUILDERS inside
  `plots/export.ts` itself, not on files elsewhere — bounded by that file's own builders.
- **The four already capped in `c7d7a752`** (Python `_BASELINE`s at 5, Julia
  `TYPED_PARAMS_MIGRATION_BASELINE` at 0, Julia `COHORT_METRICS_BASELINE` at 6).

## Overreach flags

- **`app/test/suite/model.jl:567` (`no hand-rolled state writes`)** — per-file `Dict` keyed by
  BASENAME, not full path. Two `utils.jl` files in different dirs would silently share the
  exemption. Not a bug today (only one `utils.jl` in `app/src`), but not robust to a rename.
- **`task_spec_ratchets.jl:117`** — regex text-matches `get(params, "k", <literal>)`; a computed
  fallback (`something(x, y)`) is silently skipped. Comment acknowledges this; it did catch 5 real
  divergences, so noted as a coverage bound, not overreach.
- **`frontend/src/utils/uiCopy.test.ts` has SEVEN `ALLOWED_* = []` lists in one describe group.**
  Empty-list-is-the-cap holds only if reviewers catch the `[]`→`["…"]` transition; the risk is
  additive across seven near-identical lines.
- **`test_measure_pass_column.py:252`** — scans one specific TS file for the substrings `function
  passLabel` and `n + 1`. Renaming `passLabel` fires it (good); refactoring `n + 1` to `n + ONE`
  false-fires. Narrow-scope cross-language coupling; noted for maintenance cost.
- **No ratchet found for**: (a) `.h5ad` bypass rule on the Python side — root `CLAUDE.md` bans
  `h5py`/`anndata` on cell data but no `test_*_convention.py` enforces it (Julia has
  `label_props boundary types` for the type side only); (b) `expand_user` vs `Base.expanduser`,
  which `CLAUDE.md` calls out as "already caused a real bug". Both worth a convention test —
  but `DRIFT_PREVENTION_ASSESSMENT` explicitly says only add per-case, evidence-driven ratchets.

## Reservations

- **Branch mismatch.** Audit branch is off `origin/main` at `580f87df`; the size-cap PR
  (`c7d7a752`) is on `docs/drift-prevention-archive`, unmerged. All "cap? YES" rows reflect that
  PR's diff (verified via `git show`), not HEAD. If it does not land, the four rows move from
  "capped" to "capping recommended (already designed)".
- **`api/test/suite/*.jl`** outside `api_src_ratchets_and_chainsave.jl`, and
  `frontend/src/utils/webgpuBindings.test.ts`, were not read in full — trusted self-description
  from the ratchet-shape grep hits. A testset that enforces a convention without naming itself as
  a ratchet would be missed.
- **Cross-language ratchets** (`test_measure_pass_column.py:252` is the one found) were classified
  singly. A targeted grep for `frontend/src/` string literals inside `python/cecelia/tests/*.py`
  would surface any siblings; only one hit surfaced here.
