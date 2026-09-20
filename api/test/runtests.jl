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
