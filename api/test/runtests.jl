# API-layer tests. The package (`app/test/runtests.jl`) covers Cecelia headless; this covers the thin
# HTTP adapters in `api/src`. We load the server module WITHOUT binding a socket (`CECELIA_NO_SERVE`)
# and call the handler functions directly — no live server, no ports, so it runs in CI headless.
#
# Run: `julia --project=api api/test/runtests.jl`  (or `pixi run test-api`).
ENV["CECELIA_NO_SERVE"] = "1"

# HERMETIC BY DEFAULT — same guard as `app/test/runtests.jl`, and here it fixes a real leak, not just
# CI. Testsets redirect `dirs["projects"]` to a temp dir individually and restore it in a `finally`;
# anything that forgets, or any `create_project!` on a path between one restore and the next redirect,
# writes into the DEVELOPER'S REAL projects dir and shows up in their project list. (we have been
# seeing these: `apiqc-7602` was still sitting there, from a testset since renamed.) Pointing config at
# a throwaway dir for the whole run makes the whole class impossible instead of per-testset diligence.
#
# Set `CECELIA_DEV_DIR` yourself to run against a specific config. Julia deletes both temp dirs at exit.
# Paths go in TOML *literal* strings (single quotes) so Windows backslashes are not escapes.
if !haskey(ENV, "CECELIA_DEV_DIR")
    let cfg = mktempdir(), proj = mktempdir()
        write(joinpath(cfg, "custom.toml"), "[dirs]\nprojects = '" * proj * "'\n")
        ENV["CECELIA_DEV_DIR"] = cfg
    end
end

using Test
include(joinpath(@__DIR__, "..", "src", "server.jl"))   # defines handlers + shared state; does not start
using JSON3

# ── Test data fixtures ────────────────────────────────────────────────────────
# Same committed fixtures the package suite uses (see test-data/README.md); resolved here too because
# the Julia OME-ZARR readers under test live in `api/src/image_geometry.jl`. Override with
# CECELIA_TEST_DATA. (@__DIR__ = api/test → ../../.. = workspace root.)
api_test_projects_dir() = get(ENV, "CECELIA_TEST_DATA",
    normpath(joinpath(@__DIR__, "..", "..", "test-data", "projects")))
api_fixture(relparts...) = joinpath(api_test_projects_dir(), relparts...)
# A store is a DIRECTORY; warn once and let the caller @test_skip rather than fail a partial checkout.
const _API_WARNED_FIXTURES = Set{String}()
function api_have_fixture(path::AbstractString)::Bool
    (isfile(path) || isdir(path)) && return true
    if !(path in _API_WARNED_FIXTURES)
        push!(_API_WARNED_FIXTURES, path)
        @warn "TEST FIXTURE MISSING — dependent tests SKIPPED. Expected: $path (restore with `git checkout -- test-data`)"
    end
    false
end

# call a POST handler the way the router does: JSON body → Vector{UInt8}
_post(f, obj) = f(Vector{UInt8}(JSON3.write(obj)))
_repl(code) = _post(api_repl, Dict("code" => code))

# Extracted suite files live in `api/test/suite/`. Path expressions that used to walk out one
# level (`joinpath(@__DIR__, "..", "src", …)`) would need `..`, `..` from there — hoist the
# api/test directory to a single constant so extracted files stay legible and future extracts
# can `joinpath(API_TEST_DIR, …)` without redefining anything. `@__DIR__` here still resolves
# to api/test, since runtests.jl itself did not move.
const API_TEST_DIR = @__DIR__

# ── Image payload + narrow-query API testsets ─────────────────────
# Five testsets covering payload shape (points-only value_name), run-log enrichment
# (image-writing tasks only), register-legacy REPAIRS-not-clobbers, /api/tasks/funparams/sources
# per-image (image, valueName) pairs, and /api/plots/populations valueName narrowing. Extracted
# from this file so runtests.jl contains only include lines + section-header comments (same
# pattern as app/test/suite/*.jl). The extracted file loads at top level here, so any helpers
# defined earlier (_post, api_fixture, api_have_fixture) are still in scope (lexical include).
include(joinpath(@__DIR__, "suite", "payload_and_query.jl"))

# ── Admin + install/update + app-lifecycle API testsets ─────────
# 13 testsets covering the server-admin / install / update / lifecycle surface: diagnostics,
# pool-limit guards, task thread budget, maintenance patches, system envs, running version,
# _find_pixi, update scope + version ordering + guard rails, setup wizard, app lifecycle, and
# the incremental console log ring. Extracted from this file so runtests.jl contains only
# include lines + section-header comments (same pattern as app/test/suite/*.jl). The extracted
# file loads at top level here, so helpers defined earlier (_post) stay in scope (lexical).
include(joinpath(@__DIR__, "suite", "admin_and_lifecycle.jl"))

# ── App shutdown / stop testsets ─────────────────────────────
# Two large source-level testsets pinning the shutdown surface: shutdown stops EVERY resident
# child (freeing every port api_diagnostics lists, Quit-vs-Restart asymmetry, PROD/dev
# supervisor mirror) and stopping the app actually stops it (Julia-1.12 exit/signal hazards).
# Source-level on purpose: exercising them would kill the developer’s own napari / preview
# worker / task runner. Extracted from this file so runtests.jl contains only include lines +
# section-header comments (same pattern as app/test/suite/*.jl). Path expressions rewritten to
# use the shared API_TEST_DIR constant defined above.
include(joinpath(@__DIR__, "suite", "shutdown.jl"))

# ── Runner + debug console + packages testsets ────────────────
# Five testsets covering the server-internal control surface: runner relaunch semantics
# (unavailable-vs-refused, re-check inside relaunch lock, one-shot death announcement),
# packages diagnostics, debug-console gating (loopback + toggle), debug-console eval, and
# repl config toggle. Extracted from this file so runtests.jl contains only include lines +
# section-header comments (same pattern as app/test/suite/*.jl). Path expressions use the
# shared API_TEST_DIR constant defined above.
include(joinpath(@__DIR__, "suite", "runner_console.jl"))

# ── Notebooks registry + write testsets ───────────────────────
# Two testsets covering the Notebooks Playground API surface: registry + versioning
# (name sanitisation, list/snapshot round-trip, retention, delete) and write (cells payload
# → .jl body + nb:updated broadcast). Extracted from this file so runtests.jl contains only
# include lines + section-header comments (same pattern as app/test/suite/*.jl).
include(joinpath(@__DIR__, "suite", "notebooks_rw.jl"))

# ── Viewer marks + capture + labels/ids testsets ──────────────
# Three testsets covering the viewer-adjacent HTTP surface: viewer/marks (BIDIR point-out —
# HTTP write → bag → WS broadcast, expiry drops), viewer/capture (BIDIR share-in — write /
# list / read round-trip), and labels/ids (cells + tracks enumeration + stride sampling).
# Extracted so runtests.jl contains only include lines + section-header comments.
include(joinpath(@__DIR__, "suite", "viewer_marks_capture.jl"))
include(joinpath(@__DIR__, "suite", "viewer_nav.jl"))
include(joinpath(@__DIR__, "suite", "labels_by_category.jl"))

# ── Notebooks sysimage status testset ─────────────────────────
# /api/notebooks/status response contract + _classify_sysimage staleness classifier
# (Julia version + Manifest hash). Tested without touching disk.
include(joinpath(@__DIR__, "suite", "notebooks_sysimage.jl"))

# ── Task form + preview + image geometry testsets ─────────────
# Eight testsets covering the task-form/preview/geometry surface: task previewable trait,
# served form resolves optionsFrom + defaultFrom, task preview never guesses image, preview-labels
# slab resolves UNREGISTERED vn, preview stop sweeps scratch labels, built preview request always
# sent, flow sheet centred crop, image geometry (axes + version resolution). Extracted so
# runtests.jl contains only include lines + section-header comments (same pattern as
# app/test/suite/*.jl). Path expressions use API_TEST_DIR (defined above).
include(joinpath(@__DIR__, "suite", "task_form_and_preview.jl"))
# ── VN version routing + rendering + storage codec testsets ───
# Five testsets covering inner-axis (per-value_name) version routing (VN P1c) and the pixel
# pipeline that reads through it: resolve_image_version, store compression, image render composite,
# sRGB encode matches browser canvas gamma, zarr byte order. Extracted so runtests.jl contains
# only include lines + section-header comments (same pattern as app/test/suite/*.jl).
include(joinpath(@__DIR__, "suite", "version_and_render.jl"))
# ── Module canvas + image stores + plot-spec / plotmeta testsets 
# Six testsets covering the module-canvas / stores / plot-spec surface: module-canvas
# persistence, image stores (codec + on-disk size per version), plot-spec per-page popType
# narrowing, interaction matrix (no population selection), cluster/region run resolution
# (family-aware), plotmeta gate-autoscale helpers.
include(joinpath(@__DIR__, "suite", "canvas_stores_and_plots.jl"))
# ── Project-ops testsets (lab log, chain, rename, delete, object find) ──
# Six testsets covering the mutation/discovery API a project owner reaches for: lab log,
# chain create (create-only + validated) + rename, set rename, project delete, delete-label-set
# sweeps its tracks/branch/cluster companions, object find (uid without a project in hand).
include(joinpath(@__DIR__, "suite", "project_ops.jl"))
# ── Task log + history + attribute normalisation testsets ────
# Two testsets: task log + history (per-run slicing + history endpoint) and attribute
# normalisation on write (coerces types + strips empties so consumers do not guard).
include(joinpath(@__DIR__, "suite", "task_log_and_attrs.jl"))
# ── Movie naming + version comparison + comparison grid testsets 
# Six testsets covering the movies API: batch + single-image naming, 3D detail level from
# a movie config, filename-fragment sanitisation, movie version comparison, comparison grid.
include(joinpath(@__DIR__, "suite", "movies.jl"))
# ── Observer WS broadcasts + task-frame instrumentation testsets 
# Four testsets: observer event broadcasts (mcp/ Slice B), bad-param launch emits [ERROR]
# + terminal failed frame, status frames carry the task timing, task log sliced between
# two runs bounds.
include(joinpath(@__DIR__, "suite", "observer_taskframes.jl"))

# ── Custom modules + plugins + observer status testsets ──────
# Four testsets: custom modules status/reload, plugin task options depend on the form,
# plugin task gets a form and nav entry, observer status + feedback validation.
include(joinpath(@__DIR__, "suite", "plugins_and_observer.jl"))
# ── Cohort + Analysis board API testsets ─────────────────────
# Seven testsets covering /api/cohort/* and /api/analysis/*: cohort QC, cohort runs (per
# clustering run selector), analysis lineage, analysis populations, analysis measures,
# analysis behaviour + clusters, analysis chains.
include(joinpath(@__DIR__, "suite", "cohort_and_analysis.jl"))

# ── Misc read-side API testsets (briefing / storage / versions / fs / range / funparams) ─
# Seven testsets covering /api/observer/briefing, /api/repl-api, /api/storage, /api/versions/*
# (VN P5 inventory/prune), /api/fs/browse, /api/movies/parse-range, /api/tasks/funparams.
include(joinpath(@__DIR__, "suite", "briefing_and_reads.jl"))

# ── Movie registry + config + frame range testsets ────────────
# Four testsets covering /api/movies/* configuration: movie registry (settings/movies.json —
# save/list/delete/rename/dedup/default), movie config banks what edit page reads (contract),
# MovieConfig JSON round-trip preserves on-disk shape, movie frame range (_t_range parser).
# Path expression uses API_TEST_DIR.
include(joinpath(@__DIR__, "suite", "movie_registry_and_config.jl"))

# ── Napari branch-labels + task-console (snapshot / project / log-frames / chain-node) ──
# Five testsets: napari branch-labels payload (allBranchLabels dict), task console reconciles
# snapshot removals (stale-running row regression), task console picks up project uid, task
# console ignores post-mortem log frames (finished-task no-resurrection), task console
# attributes chain-node outcomes.
include(joinpath(@__DIR__, "suite", "napari_and_taskconsole_a.jl"))

# ── Task console (times + counts) + chain-bridge + ws_status testsets ─
# Six testsets: task console times each task (elapsed clock semantics), task console counts
# outcomes without the WS frame, /api/tasks/recent, ws_status banks every producer outcome,
# chain bridge taskId degradation, chain bridge frames. TaskConsoleUT module comes from
# napari_and_taskconsole_a.jl (loaded before this include).
include(joinpath(@__DIR__, "suite", "taskconsole_b_and_chainbridge.jl"))

# ── End-to-end producers, sysimage stamp + HTTP router testsets ─
# Five testsets covering server plumbing: real producers reach WS sinks (E2E), Cecelia fns
# called unqualified from api/src are exported (namespace hygiene), sysimage stamp records
# recipe, sysimage stamp format has one implementation, HTTP router — full route table
# dispatches. Six path expressions use API_TEST_DIR.
include(joinpath(@__DIR__, "suite", "e2e_sysimage_router.jl"))

# ── Movie output params + zarr fmt + offline renderer frame + CPU overlays ─
# Five testsets: movie output size (blank = canvas size), movie filename suffix (two movies of
# one image), zarr v2/v3 read identically, render_view_frame (offline renderer movie frame),
# frame_overlays (CPU point + segment drawing).
include(joinpath(@__DIR__, "suite", "movie_frames_and_overlays.jl"))

# ── overlay_author trio testsets ──────────────────────────────
# Three testsets covering overlay_author (shared between the live viewer and the offline
# movie renderer): hex + pixel transform (colour parse + world→pixel maths), build_overlays_for
# (labelProps → per-cell overlays), build_mask_for guard + id_colours dict.
include(joinpath(@__DIR__, "suite", "overlay_author.jl"))

# ── Offline renderer plumbing testsets ────────────────────────
# Four testsets: _resolve_movie_overlays_mask honours String-keyed ov_raw (Symbol-vs-String
# keying bug), render_view_frame (points/segments overlays), record_view_movie (raw frame
# hand-off to the encoder), interpolate_keyframes (offline tween).
include(joinpath(@__DIR__, "suite", "offline_renderer_plumbing.jl"))
# ── Storage layout + VN versioning pilot + versions inventory + _json_safe testsets ─
# Four testsets: store layout defaults (ZARR_V3_PLAN D10), keep-previous-version toggle (VN
# pilot), /api/versions unions vN across images (VN P3 chain-designer picker), _json_safe
# reaches into NamedTuples.
include(joinpath(@__DIR__, "suite", "storage_and_versions.jl"))

# ── Gating mutation + undo/redo testsets ──────────────────────
# Four testsets: pop/move re-parents + pop/delete childrenOnly, gating plotdata carries a
# colour-by measure (COLOUR BY a third measure), boolean populations combine/update/block
# orphan delete (Decision 16), gating undo/redo steps through the population tree.
include(joinpath(@__DIR__, "suite", "gating_mutations.jl"))
# ── Viewer overlays + label stores + meta + props testsets ────
# Five testsets: viewer overlays (one request for the whole movie, in µm — P3 payload
# contract), viewer label stores (P4 — masks through the same reader), viewer meta names
# the versions and which one it resolved, viewer overlays on an image with no cell table,
# viewer props round-trip (save then load).
include(joinpath(@__DIR__, "suite", "viewer_overlays_and_meta.jl"))

# ── Viewer pick / overlay-legend / record-test / thumbnail guards + tracks pick-selection ─
# Seven testsets: viewer pick-cell / overlay-legend / pick-clear / pick-set 404 guards +
# input validation, /api/viewer/record-test + thumbnail input guards, a pick selection
# resolves to TRACKS.
include(joinpath(@__DIR__, "suite", "viewer_picks_and_guards.jl"))

# ── api-source ratchets + chain-save testsets ─────────────────
# Three testsets: response bodies routed through write_http_body! (call-site ratchet), no
# bare nrow in api/src (DataFrames is not imported there), chain save validates + repairs
# the start dot. Path expressions use API_TEST_DIR.
include(joinpath(@__DIR__, "suite", "api_src_ratchets_and_chainsave.jl"))

# ── Viewer slab + viewer meta (spatial audit LOD + labelDims) testsets ─
# Four testsets: viewer slab (voxels for GPU upload without a transform), viewer slab XY
# tile + pyramid level (spatial audit Phase 2), viewer meta per-level shapes (spatial audit
# LOD), viewer meta labelDims (picker flags oversized masks).
include(joinpath(@__DIR__, "suite", "viewer_slab_and_meta.jl"))

# ── Movie-rail input translators + keyframe helpers testsets ─
# Seven testsets covering the movie-rail input contract (viewer look/batch config → smoke
# route render args) and its per-frame helpers: offline overlay-config translator,
# viewstate → render args (keyframe rendering), crop_from_view_state, z_from_view_state,
# _max_px_from_view_state, movie overlays (clock + scale bar), interpolate_keyframes
# reaches the incoming keyframe exactly.
include(joinpath(@__DIR__, "suite", "movie_rail_translators.jl"))

# ── overlay_author 3D + movie-rail 3D + VIEWER_PARITY testsets ─
# Six testsets covering the 3D half of overlay_author + movie-rail 3D pipeline: build_overlays3d_for,
# movie rail overlay context resolver + JSON, overlay_author colourBy + colourOverrides,
# overlay_author rotation_matrix_from_angles, movie rail 2D↔3D overlay projection, palette +
# track-mode JSON as the shared source of truth (VIEWER_PARITY 1+2). Path expression uses API_TEST_DIR.
include(joinpath(@__DIR__, "suite", "overlay_3d_and_rail_3d.jl"))

# ── /api/tasks/validate + /api/correction-plan + /api/cell_cards + _require_ids testsets ─
# Four testsets: /api/tasks/validate wiring (form-time advisory), /api/correction-plan wiring
# (CORRECTION_QC_PLAN slice 3a), /api/cell_cards metadata + sidecar cache (synthetic fixture),
# _require_ids returns 400 on missing/empty ids (guard helper).
include(joinpath(@__DIR__, "suite", "validate_correction_cards.jl"))
# ── BIDIR Part 5 push writer + Kiwi target endpoints testsets ─
# Six testsets: format_capture_message (locked-format one-liner), push_capture_notification
# (:not_paired silent, Unix socket round-trip, stale-target fallback), POST /api/push/target/clear
# (Kiwi PR #3), POST /api/push/target/probe (Kiwi follow-up).
include(joinpath(@__DIR__, "suite", "bidir_push_writer.jl"))

# ── BIDIR Part 4 blackboard CRUD testset ──────────────────────
# One large testset covering the Blackboard storage discipline end-to-end (BIDIR_CONTEXT_PLAN.md
# Part 4): Markdown entries with snapshot-per-revise history + attached captureIds.
include(joinpath(@__DIR__, "suite", "bidir_blackboard.jl"))

# ── VN P3b popmap + BIDIR landscape testsets ──────────────────
# Five testsets: /api/gating/popmap breadcrumb + labelsVersion pin (VN P3b — drift banner),
# _bin_centroids_to_tiles (BIDIR landscape P2a) + its Z filter (P6 — viewer plane/volume),
# _pop_counts_from_label_map (BIDIR landscape P2b), _track_summary_from_binned (BIDIR
# landscape P3 — per-tile track summary).
include(joinpath(@__DIR__, "suite", "popmap_and_landscape.jl"))

# ── BIDIR plot registry testset (PR #8) ───────────────────────────────────────
# One testset covering `api/src/plots_registry_api.jl`: register/deregister/list, last-writer-wins
# on same-plotId different-clientId, WS-disconnect hook drops only this clientId's entries. Fires
# the hook directly (rather than through a live socket) — the enum round-trip of `viewer:hello`
# is covered by observer_taskframes.jl.
include(joinpath(@__DIR__, "suite", "plots_registry.jl"))

# ── Kiwi refs (KIWI_ASSISTANT_PLAN Phase 2) ──────────────────────
# The KiwiRef shape check against the shared schema, and the per-kind resolver against the testpr
# fixture — each kind pinned both ways (real object resolves, near-miss fails), plus the route.
include(joinpath(@__DIR__, "suite", "kiwi_refs.jl"))
include(joinpath(@__DIR__, "suite", "kiwi_turn.jl"))    # Phase 3: schema, seen-this-turn, validate + re-ask (scripted fake engine)
include(joinpath(@__DIR__, "suite", "kiwi_profile.jl")) # LOGIN_CREDENTIAL_ISOLATION_PLAN P3 + P6: profile roster + terminal launcher

# ── Project ownership (USER_PROFILE_PLAN Phase 5) ────────────────
# Create-time stamps active profile as owner + claim/unclaim round-trip.
include(joinpath(@__DIR__, "suite", "project_ownership.jl"))
