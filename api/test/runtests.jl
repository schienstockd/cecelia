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
@testset "API: notebooks sysimage status" begin
    # status always carries a `sysimage` field, one of the valid states (machine-independent: deps.so
    # may or may not exist here). Pins the response contract the frontend's first-run build reads.
    d = JSON3.read(api_notebooks_status(HTTP.Request("GET", "/api/notebooks/status"))[2])
    @test haskey(d, :sysimage)
    @test String(d.sysimage) in ("ready", "building", "error", "absent", "stale")

    # Pure staleness classifier — the update-safety logic, tested without touching disk.
    stamp(j, m) = "{\"julia\":\"$j\",\"manifest\":\"$m\"}"
    @test _classify_sysimage(false, nothing, false, false, "1.11", "abc") == "absent"
    @test _classify_sysimage(false, nothing, true,  false, "1.11", "abc") == "building"
    @test _classify_sysimage(false, nothing, false, true,  "1.11", "abc") == "error"
    @test _classify_sysimage(true,  nothing, false, false, "1.11", "abc") == "stale"     # unstamped ⇒ rebuild
    @test _classify_sysimage(true,  stamp("1.11","abc"), false, false, "1.11", "abc") == "ready"
    @test _classify_sysimage(true,  stamp("1.10","abc"), false, false, "1.11", "abc") == "stale"  # Julia bumped
    @test _classify_sysimage(true,  stamp("1.11","zzz"), false, false, "1.11", "abc") == "stale"  # Manifest changed
    @test _classify_sysimage(true,  stamp("1.11","abc"), true,  false, "1.11", "abc") == "ready"  # fresh wins over building
    @test _classify_sysimage(true,  stamp("1.10","abc"), true,  false, "1.11", "abc") == "building" # stale + rebuilding

    # status wiring reads the right paths: "ready" on disk iff the image exists AND its stamp matches
    # this Julia + Manifest (no build running in tests).
    onstamp = isfile(_sysimage_stamp()) ? read(_sysimage_stamp(), String) : nothing
    @test (_sysimage_status() == "ready") ==
          (isfile(_sysimage_path()) && stamp_matches(onstamp, string(VERSION), _manifest_hash()))
end

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

@testset "API: overlay_author — build_overlays3d_for on the labelProps fixture" begin
    # 3D analogue of build_overlays_for. Same fixture, same wide-open pop, but NATIVE VOXEL coords
    # (no `PixelTransform`) and a `z` field on both points AND segments. Bug this catches: the 3D
    # author silently drops the z column on a 2D-segmented image, OR emits drawn-pixel coords by
    # accident — either would visually work in the smoke script but render wrong under a rotation.
    h5 = api_fixture("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !api_have_fixture(h5)
        @test_skip "labelProps fixture missing"
    else
        dir = mktempdir()
        proj = joinpath(dir, "testpr")
        cp(api_fixture("testpr"), proj)
        old = Cecelia.cecelia_conf()["dirs"]["projects"]
        try
            Cecelia.cecelia_conf()["dirs"]["projects"] = dir
            img, err = _gating_image("testpr", "KDIeEm")
            @test err === nothing

            chb = JSON3.read(api_gating_channels(HTTP.Request("GET",
                "/api/gating/channels?projectUid=testpr&imageUid=KDIeEm&valueName=B&popType=flow"))[2])
            xchan, ychan = String(chb.columns[1]), String(chb.columns[2])
            base = Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                    "valueName" => "B", "popType" => "flow")
            gate = Dict{String,Any}("kind" => "rectangle",
                                    "x_channel" => xchan, "y_channel" => ychan,
                                    "x_min" => -1e9, "x_max" => 1e9,
                                    "y_min" => -1e9, "y_max" => 1e9)
            api_gating_pop_add(Vector{UInt8}(JSON3.write(merge(base,
                Dict{String,Any}("name" => "all3d", "colour" => "#00ff00", "gate" => gate)))))

            per_t = build_overlays3d_for(img; value_name = "B", pop_type = "flow",
                                          include_tracks = false)
            # Identity view: R = I, cx/cy/cz = 0, zoom scale = 1 → projected (u, v) = native (x + 0.5, y + 0.5).
            # This is the "drift guarantee" from a caller's POV: no rotation, no offset, dots land
            # where the cell is.
            R0 = rotation_matrix_from_angles((0.0, 0.0, 0.0))
            canvas_h, canvas_w = 100, 100
            pts0, segs0 = per_t(0, R0, 0.0, 0.0, 0.0, 1.0, canvas_h, canvas_w, 1.0)
            @test pts0 !== nothing
            @test length(pts0.u) > 0
            @test length(pts0.u) == length(pts0.v)
            @test length(pts0.colour) == length(pts0.u)
            # Every point paints in the pop's colour — the resolver honoured the gate.
            @test all(c -> c == RGB{N0f8}(0, 1, 0), pts0.colour)
            # No tracks requested → no segments even if the fixture has `track_id`.
            @test segs0 === nothing
        finally
            Cecelia.cecelia_conf()["dirs"]["projects"] = old
        end
    end
end

@testset "API: movie rail — overlay context resolver + JSON serialisation" begin
    # `_resolve_keyframe_overlay_builders` gates the whole overlay pipeline; `_overlays2d_state`
    # is the JSON contract the Python renderer reads. Julia projects; Python rasterises. Pins the
    # four decision points that could drift.

    # No image → no builders (channels-only movie).
    b2d, b3d = _resolve_keyframe_overlay_builders(nothing, nothing)
    @test b2d === nothing
    @test b3d === nothing
    # Image but empty config → still nothing (no draw-request flags).
    b2d2, b3d2 = _resolve_keyframe_overlay_builders(nothing,
        Dict{String,Any}("valueName" => "B", "popType" => "flow"))
    @test b2d2 === nothing && b3d2 === nothing
    # Serialisation: a `nothing` closure → nothing, so the state dict stays terse.
    @test _overlays2d_state(nothing, 0, (0.0, 0.0, 0.0), nothing, 1.0,
                              100, 100, 10, 1.0, 100, 100, 30, 6, 2) === nothing
    # Empty points-and-segments → nothing (skip the frame's overlay pass).
    empty_closure = (t, R, cx, cy, cz, wpp, ch, cw, za) -> (nothing, nothing)
    @test _overlays2d_state(empty_closure, 5, (0.0, 0.0, 0.0), nothing, 1.0,
                              100, 100, 10, 1.0, 100, 100, 30, 6, 2) === nothing
    # A non-empty payload → JSON-safe primitives (Vector{Float64}, no RGB objects at rest).
    pts = (; u = [50.5, 60.0], v = [40.0, 45.0],
             colour = [RGB{N0f8}(1, 0, 0), RGB{N0f8}(0, 1, 0)])
    segs = (; u0 = [50.5], v0 = [40.0], u1 = [60.0], v1 = [45.0],
              colour = [RGB{N0f8}(1, 0, 0)], alpha = [0.8])
    non_empty = (t, R, cx, cy, cz, wpp, ch, cw, za) -> (pts, segs)
    dct = _overlays2d_state(non_empty, 5, (0.0, 0.0, 0.0), nothing, 1.0,
                              100, 100, 10, 1.0, 100, 100, 30, 6, 2)
    @test dct isa AbstractDict
    @test dct["pointSize"] == 6
    @test dct["segmentWidth"] == 2
    @test dct["tailLength"] == 30
    @test dct["points"]["u"] == [50.5, 60.0]
    @test dct["points"]["v"] == [40.0, 45.0]
    @test dct["points"]["colour"][1] == Float64[1.0, 0.0, 0.0]
    @test dct["segments"]["u0"] == [50.5]
    @test dct["segments"]["alpha"] == [0.8]
    # Round-trips through JSON3 — the actual over-the-wire test.
    j = JSON3.read(JSON3.write(dct))
    @test j.pointSize == 6
    @test collect(j.points.u) == [50.5, 60.0]
    @test collect(j.segments.alpha) == [0.8]
end

@testset "API: overlay_author — colourBy + colourOverrides recolour via shared state" begin
    # The drift-guarantee payoff: `colour_by` + `colour_overrides` plug in ONCE at
    # `_build_overlay_state`, so pointing 2D and 3D authors at the same column produces the same
    # per-vertex colours. Fixture is `testpr`/`KDIeEm` with a wide-open pop; we override the
    # `centroid_t` column so every cell falls to a known value → override wins uniformly.
    h5 = api_fixture("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !api_have_fixture(h5)
        @test_skip "labelProps fixture missing"
    else
        dir = mktempdir()
        proj = joinpath(dir, "testpr")
        cp(api_fixture("testpr"), proj)
        old = Cecelia.cecelia_conf()["dirs"]["projects"]
        try
            Cecelia.cecelia_conf()["dirs"]["projects"] = dir
            img, err = _gating_image("testpr", "KDIeEm")
            @test err === nothing

            chb = JSON3.read(api_gating_channels(HTTP.Request("GET",
                "/api/gating/channels?projectUid=testpr&imageUid=KDIeEm&valueName=B&popType=flow"))[2])
            xchan, ychan = String(chb.columns[1]), String(chb.columns[2])
            base = Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                    "valueName" => "B", "popType" => "flow")
            gate = Dict{String,Any}("kind" => "rectangle",
                                    "x_channel" => xchan, "y_channel" => ychan,
                                    "x_min" => -1e9, "x_max" => 1e9,
                                    "y_min" => -1e9, "y_max" => 1e9)
            api_gating_pop_add(Vector{UInt8}(JSON3.write(merge(base,
                Dict{String,Any}("name" => "cb", "colour" => "#000000", "gate" => gate)))))

            # Which column to colour by — pick something guaranteed present, `centroid_t`. Discover
            # its values to build a total-override map, so EVERY dot gets a known colour.
            lp = label_props(img; value_name = "B")
            view_centroid_cols(lp; order = [:x, :y, :z])
            df = as_df(lp)
            ts = unique(Int[Int(round(Float64(v))) for v in df.centroid_t
                             if v isa Real && isfinite(Float64(v))])
            overrides = Dict{String,String}(string(t) => "#00ff00" for t in ts)

            # 2D author with colourBy = centroid_t + total overrides → all points paint green.
            H = ceil(Int, maximum(Float64.(df.centroid_y))) + 8
            W = ceil(Int, maximum(Float64.(df.centroid_x))) + 8
            tf = pixel_transform(H, W)
            per_t_2d = build_overlays_for(img; value_name = "B", pop_type = "flow", transform = tf,
                                           colour_by = "centroid_t",
                                           colour_overrides = overrides)
            pts, _ = per_t_2d(0)
            @test pts !== nothing
            @test length(pts.colour) > 0
            @test all(c -> c == RGB{N0f8}(0, 1, 0), pts.colour)

            # 3D author, same colourBy + overrides — same colours per vertex.
            per_t_3d = build_overlays3d_for(img; value_name = "B", pop_type = "flow",
                                             colour_by = "centroid_t",
                                             colour_overrides = overrides)
            R0 = rotation_matrix_from_angles((0.0, 0.0, 0.0))
            pts3d, _ = per_t_3d(0, R0, 0.0, 0.0, 0.0, 1.0, 100, 100, 1.0)
            @test pts3d !== nothing
            @test length(pts3d.colour) > 0
            @test all(c -> c == RGB{N0f8}(0, 1, 0), pts3d.colour)

            # Partial override — one value overridden, the rest fall to Okabe-Ito. The overridden
            # value's colour matches; some non-overridden values differ.
            partial = Dict{String,String}(string(first(ts)) => "#0000ff")
            per_t_2d_p = build_overlays_for(img; value_name = "B", pop_type = "flow", transform = tf,
                                             colour_by = "centroid_t",
                                             colour_overrides = partial)
            pts_p, _ = per_t_2d_p(first(ts))
            @test pts_p !== nothing
            @test all(c -> c == RGB{N0f8}(0, 0, 1), pts_p.colour)
        finally
            Cecelia.cecelia_conf()["dirs"]["projects"] = old
        end
    end
end

@testset "API: overlay_author — rotation_matrix_from_angles matches vispy convention" begin
    # The rotation-matrix convention is REPLICATED in three places: `render_view_frame_3d` (Julia
    # CPU fallback), `render_animation_run.py::_rotation_matrix` (GPU raycast), and
    # `rotation_matrix_from_angles` (overlay projection). ALL THREE must agree — if the overlay
    # matrix drifts from the ray one, dots and the volume rotate in different directions.
    R0 = rotation_matrix_from_angles((0.0, 0.0, 0.0))
    @test isapprox(R0, [1.0 0.0 0.0; 0.0 1.0 0.0; 0.0 0.0 1.0]; atol = 1e-9)
    R90y = rotation_matrix_from_angles((0.0, 90.0, 0.0))
    # Ry(90°) sends x → -z, z → x, y → y (standard right-hand rule). Check three column vectors.
    @test isapprox(R90y * [1.0, 0.0, 0.0], [0.0, 0.0, -1.0]; atol = 1e-9)
    @test isapprox(R90y * [0.0, 1.0, 0.0], [0.0, 1.0,  0.0]; atol = 1e-9)
    @test isapprox(R90y * [0.0, 0.0, 1.0], [1.0, 0.0,  0.0]; atol = 1e-9)
end

@testset "API: movie rail — 2D↔3D overlay projection agrees at identity view" begin
    # The DRIFT GUARANTEE. At angles=(0,0,0), zoom=1, the 3D projection reduces to an axial
    # projection: (x, y, z) → (u, v) = (x - cx + (W+1)/2 * something, y - cy + ...). We test that
    # a point at native voxel (cx, cy, cz) projects to the CANVAS CENTRE, and that swapping angles
    # for the same identity view produces the SAME screen coords whether we go through the 2D
    # `pixel_transform` path (which draws at native pixel + offset) or the 3D projection. The two
    # authors read from ONE `_build_overlay_state` and use the same collection; the projection
    # math must round-trip to the same drawn pixel for identity views.
    R0 = rotation_matrix_from_angles((0.0, 0.0, 0.0))
    canvas_h, canvas_w = 100, 100
    # Volume extents matching a 100×100×20 image (so ext_x = ext_y and z_aniso = 1 collapses to
    # canvas coordinates that equal the world coordinates plus a centre offset).
    native_w, native_h, nZ = 100, 100, 20
    z_aniso = 1.0
    cx, cy, cz = 49.5, 49.5, 9.5
    wpp = _world_per_px_3d(native_w, native_h, nZ, z_aniso, 1.0, canvas_w)
    @test isapprox(wpp, 1.0; atol = 1e-9)   # canvas span == native extent → 1 world unit / pixel
    u, v = _project_3d_point(R0, cx, cy, cz, z_aniso, wpp,
                                       canvas_h, canvas_w, cx, cy, cz)
    # Centre of volume projects to centre of canvas ((W + 1) / 2, (H + 1) / 2 — 0-based).
    @test isapprox(u, (canvas_w + 1) / 2; atol = 1e-9)
    @test isapprox(v, (canvas_h + 1) / 2; atol = 1e-9)
    # Same world point projected at Ry(90°) rotation — the point at the volume centre should STILL
    # project to the canvas centre (rotation about a point through the centre leaves the centre
    # fixed). This proves the projection actually uses cx/cy/cz as the rotation origin.
    R90 = rotation_matrix_from_angles((0.0, 90.0, 0.0))
    u90, v90 = _project_3d_point(R90, cx, cy, cz, z_aniso, wpp,
                                           canvas_h, canvas_w, cx, cy, cz)
    @test isapprox(u90, (canvas_w + 1) / 2; atol = 1e-9)
    @test isapprox(v90, (canvas_h + 1) / 2; atol = 1e-9)
end
# ── VIEWER_PARITY phases 1 + 2: overlay_author reads the same JSON the browser reads ─────────────
# The house palette, the three track-colour-mode names, and the heat-ramp anchors used by the
# offline movie renderer all live in one JSON asset (`frontend/src/plots/palettes.json`) that the
# browser look ALSO reads. This testset pins that: the Julia constants must equal the JSON we would
# see the browser using — otherwise the two paths draw the same experiment differently.
# See docs/todo/VIEWER_PARITY_PLAN.md phases 1 + 2.
@testset "API: palette + track-mode JSON is the shared source of truth for overlay_author" begin
    palette_json = normpath(joinpath(@__DIR__, "..", "..", "frontend", "src", "plots", "palettes.json"))
    @assert isfile(palette_json) "palettes.json missing — this test needs the checked-in file"
    doc = JSON3.read(read(palette_json, String))

    # Browser hex → Julia RGB the way overlay_author parses it (matches hex_to_rgb).
    function _parity_rgb(hex::AbstractString)
        h = strip(String(hex))
        startswith(h, "#") && (h = h[2:end])
        length(h) == 3 && (h = string(h[1], h[1], h[2], h[2], h[3], h[3]))
        r = parse(Int, h[1:2]; base = 16) / 255
        g = parse(Int, h[3:4]; base = 16) / 255
        b = parse(Int, h[5:6]; base = 16) / 255
        RGB{N0f8}(r, g, b)
    end

    # Palette equality: the JSON's `palettes.cecelia` block, parsed to RGB, is the Julia constant.
    json_palette = [_parity_rgb(String(h)) for h in doc.palettes.cecelia]
    @test length(CECELIA_TRACK_PALETTE) == length(json_palette) == 12
    @test CECELIA_TRACK_PALETTE == json_palette

    # Heat ramp: the JSON's five anchors are the Julia `_heat_stops()` — same order, same colours.
    json_heat = [_parity_rgb(String(h)) for h in doc.heatRamp]
    @test length(json_heat) == 5
    @test collect(_heat_stops()) == json_heat

    # Track-mode acceptance: every mode name the browser knows is accepted by build_overlays_for
    # (no fall-through to the `"track"` default warning inside the function).
    json_modes = [String(m) for m in doc.trackColorModes]
    @test Set(json_modes) == Set(TRACK_COLOR_MODES)
    for m in json_modes
        @test m in TRACK_COLOR_MODES
    end
end

# ── POST /api/tasks/validate — the generic form-time advisory endpoint ─────────────────
#
# The handler resolves images and hands off to `Cecelia.validate_param`; the validator's own logic
# is pinned in the PACKAGE suite (`_support_temporal_window_advisory`). Here we test the WIRING
# only: bad shapes → 4xx, unknown validator → 200 null, a registered validator round-trips.
@testset "API: /api/tasks/validate — wiring" begin
    _valpost(body) = api_task_validate(HTTP.Request("POST", "/api/tasks/validate"),
                                       Vector{UInt8}(JSON3.write(body)))

    # Bad JSON → 400.
    st, _ = api_task_validate(HTTP.Request("POST", "/api/tasks/validate"), Vector{UInt8}("{not json"))
    @test st == 400

    # Missing funName / paramKey → 400 each.
    st, _ = _valpost(Dict("paramKey" => "x", "value" => 1))
    @test st == 400
    st, _ = _valpost(Dict("funName" => "t", "value" => 1))
    @test st == 400

    # Unknown validator → 200, body is literal "null" (frontend renders nothing).
    st, body = _valpost(Dict("funName" => "no.such.task", "paramKey" => "no.such.key",
                             "value" => 1, "projectUid" => "", "imageUids" => []))
    @test st == 200
    @test body == "null"

    # Register a test validator that ignores images so we can round-trip without a project fixture,
    # then clean up. Same shape a real task file would register.
    Cecelia.register_param_validator!("test.validate.echo", "x",
        (v, _imgs, _sibs) -> (severity = "ok", message = "value=$v", tip = "echoed"))
    try
        st, body = _valpost(Dict("funName" => "test.validate.echo", "paramKey" => "x",
                                 "value" => 42, "projectUid" => "", "imageUids" => []))
        @test st == 200
        obj = JSON3.read(body)
        @test String(obj.severity) == "ok"
        @test occursin("42", String(obj.message))

        # `siblingValues` reaches the validator as an AbstractDict (the shape validators receive).
        Cecelia.register_param_validator!("test.validate.echo_siblings", "x",
            (_, _, sibs) -> (severity = "ok", message = "sib=" * String(get(sibs, "lr", "?")),
                             tip = "sib"))
        st, body = _valpost(Dict("funName" => "test.validate.echo_siblings", "paramKey" => "x",
                                 "value" => 1, "projectUid" => "", "imageUids" => [],
                                 "siblingValues" => Dict("lr" => "0.001")))
        @test st == 200
        @test occursin("sib=0.001", String(JSON3.read(body).message))

        # A throwing validator is swallowed by validate_param — 200 null, no 500.
        Cecelia.register_param_validator!("test.validate.throws", "x",
            (_, _, _) -> error("boom"))
        st, body = _valpost(Dict("funName" => "test.validate.throws", "paramKey" => "x",
                                 "value" => 1, "projectUid" => "", "imageUids" => []))
        @test st == 200
        @test body == "null"
    finally
        delete!(Cecelia.PARAM_VALIDATORS, ("test.validate.echo", "x"))
        delete!(Cecelia.PARAM_VALIDATORS, ("test.validate.echo_siblings", "x"))
        delete!(Cecelia.PARAM_VALIDATORS, ("test.validate.throws", "x"))
    end
end

# ── /api/correction-plan/* — slice 3a of docs/todo/CORRECTION_QC_PLAN.md ─────────────────────────
#
# Wiring only. `recommend_plan` / `_plan_to_dict` are pinned in the package suite; here we test that
# the HTTP adapter (a) shapes errors correctly, (b) hands the wizard/card through as the right Julia
# types, and (c) returns the plan dict shape the frontend types are built against.
@testset "API: /api/correction-plan — wiring" begin
    # GET presets is fixture-free: it enumerates a constant registry.
    st, body = api_correction_plan_presets(HTTP.Request("GET", "/api/correction-plan/presets"))
    @test st == 200
    presets = JSON3.read(body)
    ids = Set(String(p.id) for p in presets)
    @test issubset(Set(["resonance", "galvo", "spinning_disk", "deep_3d", "custom"]), ids)
    # each row has the frontend-facing keys
    p1 = presets[1]
    @test hasproperty(p1, :name) && hasproperty(p1, :description)
    @test hasproperty(p1, :orderHints) && hasproperty(p1, :validationStatus)

    _recpost(body) = api_correction_plan_recommend(
        HTTP.Request("POST", "/api/correction-plan/recommend"), Vector{UInt8}(JSON3.write(body)))

    # Bad JSON → 400.
    st, _ = api_correction_plan_recommend(
        HTTP.Request("POST", "/api/correction-plan/recommend"), Vector{UInt8}("{not json"))
    @test st == 400
    # Missing projectUid → 400 (from _gating_image).
    st, _ = _recpost(Dict("imageUid" => "x"))
    @test st == 400
    # Unknown project → 404.
    st, _ = _recpost(Dict("projectUid" => "no-such", "imageUid" => "no-such"))
    @test st == 404

    # Round-trip against the standard testpr fixture (KDIeEm has T axis → driftCorrect included).
    proj_dir = api_fixture("testpr")
    if !api_have_fixture(proj_dir)
        @test_skip "testpr fixture missing"
    else
        dir = mktempdir()
        cp(proj_dir, joinpath(dir, "testpr"))
        old = Cecelia.cecelia_conf()["dirs"]["projects"]
        try
            Cecelia.cecelia_conf()["dirs"]["projects"] = dir

            # Default: no cardId → auto-picked via recommend_card (empty wizard → :custom).
            st, body = _recpost(Dict("projectUid" => "testpr", "imageUid" => "KDIeEm"))
            @test st == 200
            plan = JSON3.read(body)
            @test plan.planVersion == 1
            @test String(plan.imageUid) == "KDIeEm"
            @test String(plan.presetId) == "custom"
            @test hasproperty(plan, :included) && hasproperty(plan, :excluded)
            @test hasproperty(plan, :qcScores) && hasproperty(plan, :saturationFingerprint)
            # Each step carries the plan.json field names, not the Julia struct names.
            if !isempty(plan.included)
                s = plan.included[1]
                @test hasproperty(s, :funName) && hasproperty(s, :orderWeight)
                @test hasproperty(s, :source) && hasproperty(s, :params)
            end

            # cardId explicit → wins over auto-pick.
            st, body = _recpost(Dict("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                     "cardId" => "resonance"))
            @test st == 200
            @test String(JSON3.read(body).presetId) == "resonance"

            # Wizard W5=yes → recommend_card returns :deep_3d when no cardId.
            st, body = _recpost(Dict("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                     "wizard" => Dict("W5" => "yes")))
            @test st == 200
            @test String(JSON3.read(body).presetId) == "deep_3d"

            # ── slice 3b: /get and /save (round-trip through plan.json on disk) ─────────
            _get(qs) = api_correction_plan_get(HTTP.Request("GET", "/api/correction-plan/get?$qs"))
            _save(body) = api_correction_plan_save(HTTP.Request("POST", "/api/correction-plan/save"),
                                                   Vector{UInt8}(JSON3.write(body)))

            # Fresh fixture — no plan.json yet.
            st, body = _get("projectUid=testpr&imageUid=KDIeEm")
            @test st == 200
            got = JSON3.read(body)
            @test got.exists === false
            @test got.plan === nothing
            @test got.stale === false

            # Save with a card, then GET reads it back.
            st, saved_body = _save(Dict("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                         "cardId" => "resonance"))
            @test st == 200
            saved = JSON3.read(saved_body)
            @test String(saved.presetId) == "resonance"

            st, body = _get("projectUid=testpr&imageUid=KDIeEm")
            @test st == 200
            got = JSON3.read(body)
            @test got.exists === true
            @test got.stale === false                                     # same meta ⇒ fingerprint matches
            @test String(got.plan.presetId) == "resonance"

            # Save with a different card overwrites plan.json.
            _save(Dict("projectUid" => "testpr", "imageUid" => "KDIeEm", "cardId" => "custom"))
            st, body = _get("projectUid=testpr&imageUid=KDIeEm")
            @test String(JSON3.read(body).plan.presetId) == "custom"

            # /get errors mirror /recommend errors (missing / unknown project).
            st, _ = _get("imageUid=x")
            @test st == 400
            st, _ = _get("projectUid=no-such&imageUid=no-such")
            @test st == 404

            # Bad JSON on /save → 400 (mirrors /recommend).
            st, _ = api_correction_plan_save(
                HTTP.Request("POST", "/api/correction-plan/save"), Vector{UInt8}("{nope"))
            @test st == 400

            # ── slice 3d: /mount (plan.json → chain template on disk) ──────────────────
            _mount(body) = api_correction_plan_mount(HTTP.Request("POST", "/api/correction-plan/mount"),
                                                     Vector{UInt8}(JSON3.write(body)))

            # KDIeEm has no SizeT in its shipped meta → axis.T_present = 0 → driftCorrect excluded →
            # empty plan (nothing to mount). Bump SizeT on-disk so mount has real work to do; re-save
            # the plan so the new fingerprint matches. This is scoped to the mount half of the
            # testset — the /get/save assertions above already used the unmodified fixture.
            ccid_path = joinpath(dir, "testpr", "1", "KDIeEm", "ccid.json")
            let raw = JSON3.read(read(ccid_path, String), Dict{String,Any})
                raw["meta"] = merge(get(raw, "meta", Dict{String,Any}()), Dict("SizeT" => 100))
                open(io -> JSON3.pretty(io, raw), ccid_path, "w")
            end
            _save(Dict("projectUid" => "testpr", "imageUid" => "KDIeEm", "cardId" => "resonance"))

            # First mount → creates a new chain. Chain name is fixed per-image.
            st, body = _mount(Dict("projectUid" => "testpr", "imageUid" => "KDIeEm"))
            @test st == 200
            mounted = JSON3.read(body)
            @test mounted.ok === true
            @test String(mounted.name) == "correction-plan-KDIeEm"
            @test mounted.nodeCount >= 1                     # T-present → at least driftCorrect
            @test mounted.created === true

            # Chain landed on disk in the project's chains dir.
            chains_dir = joinpath(dir, "testpr", "settings", "chains")
            chain_path = joinpath(chains_dir, "correction-plan-KDIeEm.json")
            @test isfile(chain_path)

            # Second mount without overwrite → 409 conflict.
            st, body = _mount(Dict("projectUid" => "testpr", "imageUid" => "KDIeEm"))
            @test st == 409
            conflict = JSON3.read(body)
            @test conflict.existed === true
            @test String(conflict.name) == "correction-plan-KDIeEm"

            # With overwrite: true → replaces, created=false.
            st, body = _mount(Dict("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                    "overwrite" => true))
            @test st == 200
            @test JSON3.read(body).created === false

            # No saved plan → 409 with an actionable message (delete plan.json to prove it).
            plan_path = joinpath(dir, "testpr", "1", "KDIeEm", "plan.json")
            rm(plan_path)
            rm(chain_path)                                   # so the conflict path can't mask the missing-plan error
            st, body = _mount(Dict("projectUid" => "testpr", "imageUid" => "KDIeEm"))
            @test st == 409
            @test occursin("Save the plan first", String(JSON3.read(body).error))

            # Bad JSON, missing project, unknown project → mirror /save wiring.
            st, _ = api_correction_plan_mount(
                HTTP.Request("POST", "/api/correction-plan/mount"), Vector{UInt8}("{nope"))
            @test st == 400
            st, _ = _mount(Dict("imageUid" => "x"))
            @test st == 400
            st, _ = _mount(Dict("projectUid" => "no-such", "imageUid" => "no-such"))
            @test st == 404
        finally
            Cecelia.cecelia_conf()["dirs"]["projects"] = old
        end
    end
end

# ── /api/cell_cards — metadata + sidecar cache ──────────────────────────────────
# Exercises the pool-first pipeline on the synthetic clustering fixture (`docs/todo/CELL_CARDS_PLAN.md`
# Decision 0). No OME-Zarr is on disk for `KDIeEm`, so the filmstrip PNGs deliberately come back
# empty — the card metadata (medoid triple, stats, pop name/colour/n) is what this pins.
@testset "API: /api/cell_cards — metadata + sidecar cache on the synthetic fixture" begin
    h5    = api_fixture("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    sidec = api_fixture("testpr", "1", "KDIeEm", "labelProps", "B__tracks.clustfeatures.json")
    gate  = api_fixture("testpr", "1", "KDIeEm", "gating", "B__trackclust.json")
    if !(api_have_fixture(h5) && api_have_fixture(sidec) && api_have_fixture(gate))
        @test_skip "cell-cards fixture missing (see test-data/README.md)"
    else
        dir = mktempdir()
        cp(api_fixture("testpr"), joinpath(dir, "testpr"))
        old = Cecelia.cecelia_conf()["dirs"]["projects"]
        try
            Cecelia.cecelia_conf()["dirs"]["projects"] = dir

            call(body) = api_cell_cards(Vector{UInt8}(JSON3.write(body)))
            req = Dict{String,Any}("projectUid" => "testpr", "rootUid" => "KDIeEm",
                                    "valueName" => "B", "suffix" => "movement",
                                    "pops" => [
                                        Dict("path"=>"/Scanning", "clusterIds"=>[0]),
                                        Dict("path"=>"/Directed", "clusterIds"=>[1]),
                                        Dict("path"=>"/Meandering", "clusterIds"=>[2])])
            st, body = call(req)
            @test st == 200
            resp = JSON3.read(body)

            # Pool is the single-image pool of one (fixture has partOf=["KDIeEm"]).
            @test length(resp.pool) == 1
            @test String(resp.pool[1].uid) == "KDIeEm"
            @test String(resp.pool[1].value_name) == "B"

            # Three cards in request order; each carries a medoid triple pinning (uid, vn, track_id).
            @test length(resp.cards) == 3
            names   = [String(c.name)   for c in resp.cards]
            colours = [String(c.colour) for c in resp.cards]
            @test names   == ["Scanning", "Directed", "Meandering"]
            @test colours == ["#4c78a8", "#f58518", "#54a24b"]
            @test all(c -> Int(c.n) > 0, resp.cards)             # every pop has rows in the pool
            @test all(c -> haskey(c.medoid, :uid) && haskey(c.medoid, :value_name)
                        && haskey(c.medoid, :track_id), resp.cards)
            @test all(c -> String(c.medoid.uid) == "KDIeEm", resp.cards)
            @test all(c -> String(c.medoid.value_name) == "B", resp.cards)
            # Different pops must pick different medoid tracks — the medoid picker collapsed cluster
            # separation once during Phase 1 development (a subset that copied by ref); pin it.
            tids = Set(Int(c.medoid.track_id) for c in resp.cards)
            @test length(tids) == 3

            # Stats footer carries median + IQR for every canonical measure the tracks table has.
            @test all(c -> length(c.stats) >= 10, resp.cards)
            first_stat = resp.cards[1].stats[1]
            @test String(first_stat.name) == "live.track.speed"
            @test first_stat.q25 <= first_stat.median <= first_stat.q75

            # Filmstrip is EMPTY on this fixture (no OME-Zarr on disk) — the metadata still lands.
            @test all(c -> isempty(c.filmstrip), resp.cards)

            # Sidecar written under analysis/cell_cards/{value_name}__{suffix}.json.
            sidecar_path = joinpath(dir, "testpr", "1", "KDIeEm", "analysis", "cell_cards",
                                     "B__movement.json")
            @test isfile(sidecar_path)
            side = JSON3.read(read(sidecar_path, String), Dict{String,Any})
            @test haskey(side, "pool") && haskey(side, "cards") && haskey(side, "clusterMtime")
            @test length(side["cards"]) == 3

            # valueName omitted → server derives from co_clustered_value_names(suffix). Same medoid
            # triples land — a Phase 2 change that lets the cluster panel skip plumbing valueName.
            no_vn = Dict{String,Any}("projectUid" => "testpr", "rootUid" => "KDIeEm",
                                     "suffix" => "movement",
                                     "pops" => [
                                         Dict("path"=>"/Scanning", "clusterIds"=>[0]),
                                         Dict("path"=>"/Directed", "clusterIds"=>[1]),
                                         Dict("path"=>"/Meandering", "clusterIds"=>[2])])
            st_dv, body_dv = call(no_vn)
            @test st_dv == 200
            rdv = JSON3.read(body_dv)
            @test length(rdv.cards) == 3
            @test [Int(c.medoid.track_id) for c in rdv.cards] ==
                  [Int(c.medoid.track_id) for c in JSON3.read(body).cards]

            # Cache hit — second call with unchanged mtime returns the same PARSED content.
            # Byte comparison would drift with Julia Dict key order + int/float encoding; parse first.
            st2, body2 = call(req)
            @test st2 == 200
            r1 = JSON3.read(body);  r2 = JSON3.read(body2)
            @test length(r1.cards) == length(r2.cards)
            @test [String(c.name) for c in r1.cards] == [String(c.name) for c in r2.cards]
            @test [Int(c.medoid.track_id) for c in r1.cards] ==
                  [Int(c.medoid.track_id) for c in r2.cards]

            # Bad body → 400.
            st3, _ = api_cell_cards(Vector{UInt8}("{not json"))
            @test st3 == 400
            st4, _ = call(Dict{String,Any}("projectUid" => "testpr"))
            @test st4 == 400
            # Unknown suffix → 404.
            st5, _ = call(merge(req, Dict{String,Any}("suffix" => "no-such")))
            @test st5 == 404
        finally
            Cecelia.cecelia_conf()["dirs"]["projects"] = old
        end
    end
end

# gating_api._require_ids — audit task #33. Sites that used to hand-roll `body["projectUid"]`
# now go through this helper so a missing id is a 400 with the field name, never a bare
# KeyError. Both empty AND absent should behave the same.
@testset "API: _require_ids returns 400 on missing/empty ids" begin
    # gating_api.jl is already included by server.jl at the top of this file, so `_require_ids`
    # is available at top level.
    let body = Dict{String,Any}("projectUid" => "p", "imageUid" => "i")
        pu, iu, err = _require_ids(body)
        @test err === nothing && pu == "p" && iu == "i"
    end
    let body = Dict{String,Any}("imageUid" => "i")   # projectUid absent
        _, _, err = _require_ids(body)
        @test err !== nothing
        st, msg = err
        @test st == 400
        @test occursin("projectUid", msg)             # field name in the message
    end
    let body = Dict{String,Any}("projectUid" => "", "imageUid" => "i")   # empty
        _, _, err = _require_ids(body)
        @test err !== nothing && err[1] == 400 && occursin("projectUid", err[2])
    end
    let body = Dict{String,Any}("projectUid" => "p", "imageUid" => "")   # empty
        _, _, err = _require_ids(body)
        @test err !== nothing && err[1] == 400 && occursin("imageUid", err[2])
    end
end

# ── BIDIR Part 5 push writer (BIDIR_PUSH_PLAN PR #2) ────────────────────────
@testset "API: format_capture_message builds the locked-format one-liner" begin
    # The exact wording is load-bearing — a chat-side grep for `"[cecelia] shared capture"`
    # must find every incident, past and future. Change the wording only if you're changing
    # the plan's Decision 5.
    m1 = format_capture_message("zolIMa", "cap-20260919T140000-abcdef",
        Dict{String,Any}("surface" => "viewer_frame", "imageUid" => "1SqevM",
                         "t" => 3, "z" => 7))
    @test m1 == "[cecelia] shared capture cap-20260919T140000-abcdef from project zolIMa " *
                "(viewer_frame, image 1SqevM, t=3, z=7). Read it with " *
                "get_capture(\"zolIMa\", \"cap-20260919T140000-abcdef\")."

    # A UI-surface capture with no image / t / z reads cleanly — parts that are empty are
    # dropped rather than showing "()" or "image ".
    m2 = format_capture_message("p", "cap-x", Dict{String,Any}("surface" => "ui"))
    @test m2 == "[cecelia] shared capture cap-x from project p (ui). Read it with " *
                "get_capture(\"p\", \"cap-x\")."

    # Zero-address capture (address stripped or absent) still names project + id.
    m3 = format_capture_message("p", "cap-y", nothing)
    @test occursin("[cecelia] shared capture cap-y from project p", m3)
    @test occursin("get_capture(\"p\", \"cap-y\")", m3)
end

@testset "API: push_capture_notification without pairing ⇒ :not_paired, silent" begin
    # No push_target.json on disk ⇒ we skip the socket work and return :not_paired. This is
    # the common "user hasn't paired a session yet" case; the frontend renders it the same as
    # :fallback (both trigger the clipboard/toast path).
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name = "api-push-unpaired")
        outcome, msg = push_capture_notification(proj.uid, "cap-x",
            Dict{String,Any}("surface" => "viewer_frame"))
        @test outcome === :not_paired
        @test msg === nothing
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: push_capture_notification round-trips to a local Unix socket" begin
    # The full wire test: bind a Unix socket in the test process, listen, then have the writer
    # connect + send both lines. Assert the receiver saw the exact bytes documented above.
    # Skips on Windows — the named-pipe branch would need a different listener setup and this
    # PR doesn't have a Windows box to verify against.
    Sys.iswindows() && (@test_skip "push writer round-trip (Windows named pipes)"; return)

    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    sock_path = joinpath(tmp, "probe.sock")
    server = Sockets.listen(sock_path)
    received = String[]
    receiver = @async begin
        try
            s = Sockets.accept(server)
            while !eof(s)
                line = readline(s; keep = false)
                isempty(line) || push!(received, line)
            end
            close(s)
        catch  # server closed during test teardown
        end
    end
    try
        proj = create_project!(name = "api-push-uds")
        # Write a pairing record ourselves — mirrors what /api/push/target POST would do.
        target_dir = joinpath(tmp, proj.uid, "settings")
        mkpath(target_dir)
        write_json_atomic(joinpath(target_dir, "push_target.json"), Dict{String,Any}(
            "socketPath" => sock_path, "token" => "test-token-abc",
            "sessionLabel" => "probe", "pairedAt" => "2026-09-19T14:00:00",
            "pairedFromPid" => "0",
        ))
        outcome, sent_content = push_capture_notification(proj.uid, "cap-y",
            Dict{String,Any}("surface" => "viewer_frame", "imageUid" => "IMG1",
                             "t" => 2, "z" => 5))
        @test outcome === :sent
        @test sent_content !== nothing
        @test occursin("cap-y", sent_content::String)
        # Give the receiver a moment to drain both lines from the socket buffer.
        for _ in 1:20
            length(received) >= 2 && break
            sleep(0.05)
        end
        @test length(received) == 2
        auth = JSON3.read(received[1], Dict{String,Any})
        msg  = JSON3.read(received[2], Dict{String,Any})
        @test auth["type"]  == "auth"
        @test auth["token"] == "test-token-abc"
        @test msg["type"]   == "user"
        @test msg["message"]["role"]    == "user"
        @test msg["message"]["content"] == sent_content
    finally
        try; close(server); catch; end
        # The @async receiver exits on eof/close; give it a moment.
        try; wait(receiver); catch; end
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: push writer clears stale target on connect failure ⇒ :fallback" begin
    # A pairing record pointing at a dead socket path ⇒ the writer catches the connect
    # exception, removes the record, and returns :fallback. Next auto-pair (from any MCP
    # tool call) rewrites it; until then GET /api/push/target reads "not paired".
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name = "api-push-stale")
        target_dir = joinpath(tmp, proj.uid, "settings")
        mkpath(target_dir)
        stale_path = joinpath(tmp, "definitely-not-a-socket.sock")
        target_json = joinpath(target_dir, "push_target.json")
        write_json_atomic(target_json, Dict{String,Any}(
            "socketPath" => stale_path, "token" => "t",
        ))
        outcome, _ = push_capture_notification(proj.uid, "cap-z", nothing)
        @test outcome === :fallback
        # Stale record cleared silently — no leftover file to re-attempt next time.
        @test !isfile(target_json)
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: POST /api/push/target/clear removes the pairing record (Kiwi PR #3)" begin
    # Manual unpair — the Kiwi cockpit's "Clear pairing" button. Deletes the file if present;
    # succeeds silently when it wasn't there (idempotent).
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name = "api-push-clear")
        target_dir = joinpath(tmp, proj.uid, "settings")
        mkpath(target_dir)
        target_json = joinpath(target_dir, "push_target.json")
        write_json_atomic(target_json, Dict{String,Any}(
            "socketPath" => "/tmp/nowhere.sock", "token" => "t",
        ))
        @test isfile(target_json)

        # Clear it via the HTTP handler. Rebuild the body each call — `_parse_body` moves the
        # bytes into a String, so a reused Vector{UInt8} reads empty on the second dispatch.
        make_body() = Vector{UInt8}(JSON3.write(Dict("projectUid" => proj.uid)))
        status, resp = api_push_target_clear(make_body())
        @test status == 200
        parsed = JSON3.read(resp, Dict{String,Any})
        @test parsed["ok"] == true
        @test parsed["cleared"] == true
        @test !isfile(target_json)

        # Second clear on an already-gone file — still 200, cleared: false.
        status2, resp2 = api_push_target_clear(make_body())
        @test status2 == 200
        @test JSON3.read(resp2, Dict{String,Any})["cleared"] == false

        # Bad inputs — 400 on empty projectUid, 404 on unknown project.
        st400, _ = api_push_target_clear(Vector{UInt8}(JSON3.write(Dict("projectUid" => ""))))
        @test st400 == 400
        st404, _ = api_push_target_clear(Vector{UInt8}(JSON3.write(Dict("projectUid" => "does-not-exist"))))
        @test st404 == 404
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: POST /api/push/target/probe (Kiwi follow-up)" begin
    # Liveness probe. Three shapes:
    #   1. Unpaired ⇒ {paired:false, alive:false} with no side effect.
    #   2. Paired at a live listener ⇒ {paired:true, alive:true}.
    #   3. Paired at a dead socket ⇒ {paired:false, alive:false, reason:...} + record cleared.
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name = "api-push-probe")
        target_dir = joinpath(tmp, proj.uid, "settings")
        mkpath(target_dir)
        target_json = joinpath(target_dir, "push_target.json")
        make_body() = Vector{UInt8}(JSON3.write(Dict("projectUid" => proj.uid)))

        # (1) unpaired — no file yet.
        st1, r1 = api_push_target_probe(make_body())
        @test st1 == 200
        p1 = JSON3.read(r1, Dict{String,Any})
        @test p1["paired"] == false && p1["alive"] == false

        # (2) alive — bind a local UDS in a background task, probe it. Skipped on native
        # Windows: `Sockets.listen(<file path>)` requires a `\\.\pipe\...` name there rather
        # than a plain filesystem path, so a temp-dir socket file is a portable server we can't
        # spin up. The probe under test itself IS Windows-safe (Julia's `Sockets.connect`
        # dispatches on the transport transparently — see push_writer.jl); the dead-socket
        # branch below still exercises the code path on every platform.
        if !Sys.iswindows()
            live_path = joinpath(tmp, "live.sock")
            server = Sockets.listen(live_path)
            accept_task = @async try Sockets.accept(server) catch _ end
            try
                write_json_atomic(target_json, Dict{String,Any}(
                    "socketPath" => live_path, "token" => "t",
                ))
                st2, r2 = api_push_target_probe(make_body())
                @test st2 == 200
                p2 = JSON3.read(r2, Dict{String,Any})
                @test p2["paired"] == true && p2["alive"] == true
                @test isfile(target_json)   # alive ⇒ record preserved
            finally
                close(server)
                try wait(accept_task) catch _ end
            end
        end

        # (3) dead — point at a socket path nobody is listening on.
        dead_path = joinpath(tmp, "definitely-not-a-socket.sock")
        write_json_atomic(target_json, Dict{String,Any}(
            "socketPath" => dead_path, "token" => "t",
        ))
        st3, r3 = api_push_target_probe(make_body())
        @test st3 == 200
        p3 = JSON3.read(r3, Dict{String,Any})
        @test p3["paired"] == false && p3["alive"] == false
        @test haskey(p3, "reason")
        @test !isfile(target_json)   # dead ⇒ record cleared server-side

        # Bad inputs mirror the clear route.
        st400, _ = api_push_target_probe(Vector{UInt8}(JSON3.write(Dict("projectUid" => ""))))
        @test st400 == 400
        st404, _ = api_push_target_probe(Vector{UInt8}(JSON3.write(Dict("projectUid" => "does-not-exist"))))
        @test st404 == 404
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: blackboard CRUD + versioning + attachments (BIDIR Part 4)" begin
    # docs/todo/BIDIR_CONTEXT_PLAN.md Part 4. Backend of the Blackboard: Markdown entries with a
    # snapshot-per-revise history and attached captureIds. Frontend page + MCP tools are separate
    # follow-ups; this testset covers the storage discipline end-to-end.
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        uid = "TESTBB"; mkpath(joinpath(tmp, uid))
        # A captured frame lives on disk so an attachment reference resolves against something real.
        # `_clean_attachments` reads the captures dir directly; we just need the folder shape.
        cap_id  = "cap-20260101T000000-aaaaaa"
        cap_id2 = "cap-20260101T000001-bbbbbb"      # a second, for versioned-attachments tests
        mkpath(joinpath(tmp, uid, "captures", cap_id))
        mkpath(joinpath(tmp, uid, "captures", cap_id2))
        w(path, b) = _post(path, b)

        # ── Guards on create ─────────────────────────────────────────────────
        @test w(api_blackboard_create, Dict("title"=>"t"))[1] == 400
        @test w(api_blackboard_create, Dict("projectUid"=>"NOPE", "title"=>"t"))[1] == 404
        @test w(api_blackboard_create, Dict("projectUid"=>uid, "title"=>""))[1] == 400
        # Content size cap — anything past 100 KiB is rejected before write.
        oversized = repeat("x", 100 * 1024 + 1)
        @test w(api_blackboard_create, Dict("projectUid"=>uid, "title"=>"t", "content"=>oversized))[1] == 400

        # ── Create ───────────────────────────────────────────────────────────
        # Attachments mix: one valid captureId (kept), one made-up (dropped because the dir is
        # missing), one malformed (dropped by the id regex before path resolution). Duplicate is
        # collapsed. The stored list must reflect what actually exists.
        st_c, body_c = w(api_blackboard_create, Dict("projectUid"=>uid,
            "title"=>"Chain design for MERTK sample",
            "content"=>"# Working notes\n\nDiscuss segmentation w/ Claude.",
            "attachments"=>[cap_id, "cap-19700101T000000-000000", "../../../etc/passwd", cap_id]))
        @test st_c == 200
        eid = String(JSON3.read(body_c).entryId)
        @test occursin(r"^bb-[0-9]{8}T[0-9]{6}-[0-9a-f]{6}$", eid)
        entry_dir = joinpath(tmp, uid, "blackboard", eid)
        @test isdir(entry_dir) && isfile(joinpath(entry_dir, "entry.md"))
        @test isfile(joinpath(entry_dir, "meta.json"))
        # Registry now knows about it.
        reg_path = joinpath(tmp, uid, "settings", "blackboard.json")
        reg = JSON3.read(read(reg_path, String), Dict{String,Any})
        @test haskey(reg, eid) && String(reg[eid]["title"]) == "Chain design for MERTK sample"

        # ── List — newest-first, attachmentsCount surfaced ───────────────────
        st_l, body_l = api_blackboard_list(HTTP.Request("GET",
            "/api/blackboard?projectUid=$uid"))
        @test st_l == 200
        entries = JSON3.read(body_l).entries
        @test length(entries) == 1
        @test String(entries[1].entryId) == eid
        @test entries[1].current == 0                # never snapshotted yet
        @test entries[1].attachmentsCount == 1       # dedup + validation kept one of four

        # ── Read live ────────────────────────────────────────────────────────
        st_e, body_e = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test st_e == 200
        e = JSON3.read(body_e).entry
        @test occursin("Working notes", String(e.content))
        @test isempty(e.versions)                    # no snapshots yet
        @test String(e.attachments[1]) == cap_id

        # ── Revise → v1 snapshot of prior content, live now = new content ──
        # Second revise flips attachments only (from [cap_id] to [cap_id2]) — this verifies both
        # halves of the state (markdown AND attachments) are versioned, so a later read at v2 must
        # see the OLD attachments (cap_id), not the current (cap_id2).
        st_r1, body_r1 = w(api_blackboard_revise, Dict("projectUid"=>uid, "entryId"=>eid,
            "content"=>"# Working notes (v2)\n\nNow with a Mermaid diagram."))
        @test st_r1 == 200
        @test JSON3.read(body_r1).version == 1
        # Snapshot v1 is the OLD content; live is the new. Attachments unchanged so v1's recorded
        # atts equal the live atts (still [cap_id]).
        st_v1, body_v1 = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid&version=1"))
        @test st_v1 == 200
        @test occursin("Working notes\n", String(JSON3.read(body_v1).entry.content))
        @test String(JSON3.read(body_v1).entry.attachments[1]) == cap_id
        st_live, body_live = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test occursin("(v2)", String(JSON3.read(body_live).entry.content))
        @test JSON3.read(body_live).entry.current == 1

        # Second revise: same content is fine, DIFFERENT attachments. Must NOT be a no-op — the
        # attachment change is a real diff — and v2's recorded attachments must be the OLD set
        # ([cap_id]), not the new one.
        st_r2, body_r2 = w(api_blackboard_revise, Dict("projectUid"=>uid, "entryId"=>eid,
            "content"=>"# Working notes (v3)", "attachments"=>[cap_id2]))
        @test st_r2 == 200
        @test JSON3.read(body_r2).version == 2
        st_l2, body_l2 = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test JSON3.read(body_l2).entry.current == 2
        @test sort(collect(JSON3.read(body_l2).entry.versions)) == [1, 2]
        @test String(JSON3.read(body_l2).entry.attachments[1]) == cap_id2  # current = new
        st_v2, body_v2 = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid&version=2"))
        @test String(JSON3.read(body_v2).entry.attachments[1]) == cap_id   # v2 record = OLD

        # ── No-op revise: same content, same attachments ⇒ unchanged:true, no new snapshot ─
        st_no, body_no = w(api_blackboard_revise, Dict("projectUid"=>uid, "entryId"=>eid,
            "content"=>"# Working notes (v3)", "attachments"=>[cap_id2]))
        @test st_no == 200
        @test JSON3.read(body_no).unchanged == true
        @test JSON3.read(body_no).version == 2
        st_l_noop, body_l_noop = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        # Version list did NOT grow — a repeat-with-same-payload doesn't spend a snapshot.
        @test sort(collect(JSON3.read(body_l_noop).entry.versions)) == [1, 2]

        # ── Restore v1 → snapshots current first (as v3), then restores v1 ──
        # Restore brings back BOTH the markdown AND the attachment set recorded for v1 (cap_id).
        st_re, body_re = w(api_blackboard_restore, Dict("projectUid"=>uid, "entryId"=>eid,
            "version"=>"1"))
        @test st_re == 200
        @test JSON3.read(body_re).version == 1
        st_after, body_after = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        e_after = JSON3.read(body_after).entry
        @test occursin("Working notes\n", String(e_after.content))    # matches v1's content
        @test e_after.current == 1
        @test String(e_after.attachments[1]) == cap_id                 # matches v1's atts
        # The un-snapshotted-before-restore content is now v3 — critical: we can undo the restore.
        @test sort(collect(e_after.versions)) == [1, 2, 3]
        # And v3 records the STATE THAT WAS LIVE right before this restore fired — cap_id2.
        st_v3, body_v3 = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid&version=3"))
        @test String(JSON3.read(body_v3).entry.attachments[1]) == cap_id2

        # Restore unknown version ⇒ 404
        @test w(api_blackboard_restore, Dict("projectUid"=>uid, "entryId"=>eid,
            "version"=>"99"))[1] == 404

        # ── Prune to keep 2 most recent ─────────────────────────────────────
        st_p, body_p = w(api_blackboard_prune, Dict("projectUid"=>uid, "entryId"=>eid, "keep"=>"2"))
        @test st_p == 200 && JSON3.read(body_p).pruned == 1
        st_prune, body_prune = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test sort(collect(JSON3.read(body_prune).entry.versions)) == [2, 3]

        # ── Delete ───────────────────────────────────────────────────────────
        st_d, body_d = w(api_blackboard_delete, Dict("projectUid"=>uid, "entryId"=>eid))
        @test st_d == 200 && JSON3.read(body_d).deleted == true
        @test !isdir(entry_dir)
        st_d2, body_d2 = w(api_blackboard_delete, Dict("projectUid"=>uid, "entryId"=>eid))
        @test st_d2 == 200 && JSON3.read(body_d2).deleted == false  # idempotent

        # ── Read guards ─────────────────────────────────────────────────────
        @test api_blackboard_entry_get(HTTP.Request("GET", "/api/blackboard/entry"))[1] == 400
        @test api_blackboard_entry_get(HTTP.Request("GET", "/api/blackboard/entry?projectUid=$uid"))[1] == 400
        @test api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=nope"))[1] == 400
        @test api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=bb-20260101T000000-abcdef"))[1] == 404
        @test api_blackboard_list(HTTP.Request("GET", "/api/blackboard"))[1] == 400
        # Traversal via entryId is rejected by the regex before path composition.
        @test w(api_blackboard_delete, Dict("projectUid"=>uid, "entryId"=>"../../etc/passwd"))[1] == 400
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

# ── P3b — /api/gating/popmap returns breadcrumb pair; labelsVersion pin threads ──
# docs/todo/VN_VERSIONING_PLAN.md → P3b. Guardrail so the drift banner is fed a reliable pair:
#   • authoredLabelsVersion       — the map's persisted breadcrumb (`nothing` on legacy/blank)
#   • currentLatestLabelsVersion  — the image's current `_latest` labels vN for this value_name
# The `labelsVersion` query param is only asserted at the routing level here (the endpoint accepts
# it and doesn't error); its effect on the underlying `label_props(img; version=…)` read is unit-
# tested where the reader lives (`app/test/suite/labelprops.jl`) — a positive-value assertion here
# needs a fixture with two labels versions on disk, which the smoke fixture doesn't carry.
@testset "API: /api/gating/popmap breadcrumb + labelsVersion pin (VN P3b)" begin
  if !api_have_fixture(api_fixture("testpr"))
    @test_skip "testpr fixture missing"
  else
    dir = mktempdir(); cp(api_fixture("testpr"), joinpath(dir, "testpr"))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    empty!(_GATING_HISTORY)
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        common = "projectUid=testpr&imageUid=KDIeEm&valueName=B&popType=flow"

        # Fresh fixture: `gating/B.json` may not exist yet → popmap returns an empty tree with no
        # breadcrumb (authored = nothing). The image *does* have label_props for B (implicit v1),
        # so currentLatestLabelsVersion resolves to "v1" (bare-scalar entries → LATEST_DEFAULT_VAL).
        st, body = api_gating_popmap(HTTP.Request("GET", "/api/gating/popmap?" * common))
        @test st == 200
        d = JSON3.read(body, Dict{String,Any})
        @test isnothing(get(d, "authoredLabelsVersion", nothing))
        @test get(d, "currentLatestLabelsVersion", nothing) == "v1"

        # A save (via any gating mutation → save_pop_map!(m, img)) stamps the breadcrumb. Add a pop
        # so the save path fires — then re-read: authored = "v1", drift = false (matches current).
        base = Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                "valueName" => "B", "popType" => "flow")
        gate = Dict{String,Any}("kind" => "rectangle", "x_channel" => "c1", "y_channel" => "c2",
                                "x_min" => 0.0, "x_max" => 1.0, "y_min" => 0.0, "y_max" => 1.0)
        api_gating_pop_add(Vector{UInt8}(JSON3.write(merge(base,
            Dict{String,Any}("name" => "qc", "gate" => gate)))))

        st, body = api_gating_popmap(HTTP.Request("GET", "/api/gating/popmap?" * common))
        d = JSON3.read(body, Dict{String,Any})
        @test d["authoredLabelsVersion"] == "v1"
        @test d["currentLatestLabelsVersion"] == "v1"

        # Route-level: the endpoint accepts the pin without erroring. A vN that doesn't exist on the
        # image still returns 200 — the pin is a read directive, not a validation gate (a missing
        # inner version resolves via the composer to `nothing`, the caller handles empty data). We
        # assert 200 and shape here; the reader-level pin behaviour is pinned in the pkg suite.
        st, body = api_gating_popmap(HTTP.Request("GET",
            "/api/gating/popmap?" * common * "&labelsVersion=v1"))
        @test st == 200
        st, _ = api_gating_membership(HTTP.Request("GET",
            "/api/gating/membership?" * common * "&pops=/qc&labelsVersion=v1"))
        @test st == 200
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
        empty!(_GATING_HISTORY)
    end
  end
end

@testset "API: _bin_centroids_to_tiles (BIDIR landscape Phase 2a)" begin
    # LANDSCAPE_COMPLEMENTARY_PLAN.md Phase 2a — the pure binning helper behind segCount.
    # Bins level-0 centroids into a ncols×nrows grid over the full frame; row-major flat output;
    # sparse-friendly (NaN centroids drop; off-frame timepoints drop; not throw).
    # 100×100 frame, 4×4 grid ⇒ each tile is 25 px wide. tileId row-major so (r=0,c=0)="A1".
    xs = Float64[10.0, 12.0, 60.0,  99.0, 0.0, NaN,  50.0]
    ys = Float64[10.0, 20.0, 60.0,  99.0, 0.0, 50.0, NaN]
    ts = Int[    0,    0,    0,     0,    0,   0,    0]
    counts = _bin_centroids_to_tiles(xs, ys, ts, 0, 4, 4, 100, 100)
    @test length(counts) == 16
    @test counts[1] == 3         # (10,10), (12,20), (0,0) all land in top-left tile
    @test counts[11] == 1        # (60,60) → row 2 col 2 → idx 11
    @test counts[16] == 1        # (99,99) → row 3 col 3 → idx 16 (clamp keeps the edge tile in bounds)
    @test sum(counts) == 5       # 7 rows in, 2 NaN-drops

    # Temporal filter: only rows with centroid_t == t are counted
    xs_t = Float64[10.0, 10.0, 10.0]; ys_t = Float64[10.0, 10.0, 10.0]; ts_t = Int[0, 1, 2]
    @test _bin_centroids_to_tiles(xs_t, ys_t, ts_t, 1, 2, 2, 100, 100)[1] == 1
    @test sum(_bin_centroids_to_tiles(xs_t, ys_t, ts_t, 5, 2, 2, 100, 100)) == 0   # no matching t

    # `ts === nothing` (still image, no centroid_t column) counts every centroid against any t
    counts_still = _bin_centroids_to_tiles(Float64[10.0, 50.0], Float64[10.0, 50.0],
                                            nothing, 0, 2, 2, 100, 100)
    @test sum(counts_still) == 2

    # Length mismatch throws — a caller-side bug we want loud, not silent
    @test_throws ArgumentError _bin_centroids_to_tiles(Float64[1.0], Float64[1.0, 2.0],
                                                       nothing, 0, 2, 2, 100, 100)
end

@testset "API: _pop_counts_from_label_map (BIDIR landscape Phase 2b)" begin
    # LANDSCAPE_COMPLEMENTARY_PLAN.md Phase 2b — the pure per-tile pop aggregator.
    # Sparsity discipline (Decision 3): hidden pops drop; pops with no member cells in the
    # visible frame drop; zero-count tiles have no entry in their inner vector.
    # 4-tile grid, label→tile map: label 1→tile 1, 2→tile 1, 3→tile 2, 4→tile 4.
    label_map = Dict(1 => 1, 2 => 1, 3 => 2, 4 => 4)
    pops = [
        (path = "/live/tnaive", name = "T naive", show = true,  labels = [1, 2, 3]),   # 2 in T1, 1 in T2
        (path = "/live/tmem",   name = "T mem",   show = true,  labels = [3, 4, 99]),  # 1 in T2, 1 in T4, 99 absent
        (path = "/live/hidden", name = "Hidden",  show = false, labels = [1, 2, 3, 4]),# entirely skipped
        (path = "/live/empty",  name = "Empty",   show = true,  labels = Int[]),       # no labels, no entries
        (path = "/live/offmap", name = "Off",     show = true,  labels = [77, 88]),    # no labels land in any tile
    ]
    per_tile = _pop_counts_from_label_map(pops, label_map, 4)
    @test length(per_tile) == 4
    # tile 1: T naive count 2, no T mem
    @test length(per_tile[1]) == 1
    @test per_tile[1][1].path == "/live/tnaive" && per_tile[1][1].count == 2
    # tile 2: T naive count 1, T mem count 1
    paths_t2 = sort([p.path for p in per_tile[2]])
    counts_t2 = Dict(p.path => p.count for p in per_tile[2])
    @test paths_t2 == ["/live/tmem", "/live/tnaive"]
    @test counts_t2["/live/tnaive"] == 1 && counts_t2["/live/tmem"] == 1
    # tile 3: no visible pops occupy it
    @test isempty(per_tile[3])
    # tile 4: only T mem
    @test length(per_tile[4]) == 1
    @test per_tile[4][1].path == "/live/tmem" && per_tile[4][1].count == 1
    # Hidden pop never appears anywhere (Decision 3 sparsity)
    @test all(all(p.path != "/live/hidden" for p in bag) for bag in per_tile)
    # Empty / off-map pops likewise
    @test all(all(p.path != "/live/empty" && p.path != "/live/offmap" for p in bag) for bag in per_tile)
end

@testset "API: _track_summary_from_binned (BIDIR landscape Phase 3)" begin
    # LANDSCAPE_COMPLEMENTARY_PLAN.md Phase 3 — the pure per-tile tracks aggregator.
    # 4-tile grid: tile 1 has tracks {10, 11} with 2 cells contributing speeds; tile 2 has
    # only track 12; tile 3 is empty; tile 4 has track 10 again (a long track passing through).
    # duration_by_track (whole-lifetime frame counts): 10 → 100, 11 → 25, 12 → 8.
    dur = Dict(10 => 100, 11 => 25, 12 => 8)
    tile_ids = [Set([10, 11]), Set([12]), Set{Int}(), Set([10])]
    speed_sum = Float64[3.0, 1.5, 0.0, 4.0]     # tile 1 sum 3.0 across 2 cells → mean 1.5
    speed_n   = Int[2, 1, 0, 1]
    out = _track_summary_from_binned(tile_ids, speed_sum, speed_n, dur)
    @test length(out) == 4
    @test out[1] !== nothing
    @test out[1].count == 2
    @test out[1].meanDuration == (100 + 25) / 2
    @test out[1].meanSpeed ≈ 1.5
    @test out[2].count == 1
    @test out[2].meanDuration == 8
    @test out[2].meanSpeed ≈ 1.5
    # Empty tile → nothing (sparsity — caller emits no `tracks` key)
    @test out[3] === nothing
    @test out[4].count == 1
    @test out[4].meanDuration == 100
    @test out[4].meanSpeed ≈ 4.0

    # No speed observations at all (tracked segmentation without `live.cell.speed`) →
    # meanSpeed NaN; caller emits `count`/`meanDuration` and omits `meanSpeed`.
    out2 = _track_summary_from_binned([Set([10])], Float64[0.0], Int[0], dur)
    @test out2[1].count == 1
    @test isnan(out2[1].meanSpeed)
    @test out2[1].meanDuration == 100

    # A track with no known duration (stale h5ad) gets skipped in the meanDuration numerator
    # but still counts toward `count` — a defensive drop, not a lie about the tile.
    out3 = _track_summary_from_binned([Set([99, 10])], Float64[0.0], Int[0],
                                       Dict(10 => 50))    # 99 absent
    @test out3[1].count == 2                              # both tracks visible in tile
    @test out3[1].meanDuration == 50                      # only 10 contributes a real duration

    # Length mismatch throws
    @test_throws ArgumentError _track_summary_from_binned([Set([1])], Float64[0.0, 0.0],
                                                          Int[1], Dict{Int,Int}())
end
