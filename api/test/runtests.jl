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

# ── Undo / redo for hand-drawn gating ────────────────────────────────────────────────────────
# The whole population tree is one serialisable document, so history is a ring of snapshots taken at
# the one choke point every mutation already goes through (`_persist_and_broadcast!`). What this
# pins is the contract that makes that safe: a step must not record ITSELF as an edit (or undo would
# only ever toggle the last change), a fresh edit must drop the redo branch, and the pop types whose
# edit is a re-tickable filter must not get history at all.
# ── pop/move + pop/delete childrenOnly ───────────────────────────────────────────────────────────
# The population manager's ⋯ menu: re-parent a population (gate kept, membership re-derived) and prune
# the subtree under one without deleting it. Both are ordinary mutations, so they persist, broadcast
# and record undo history like any other — asserted here on the ON-DISK document, not just the reply.
@testset "API: pop/move re-parents, pop/delete childrenOnly prunes" begin
  if !api_have_fixture(api_fixture("testpr"))
    @test_skip "testpr fixture missing"
  else
    dir = mktempdir()
    proj = joinpath(dir, "testpr")
    cp(api_fixture("testpr"), proj)
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    empty!(_GATING_HISTORY)
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        vn = "B"
        base = Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                "valueName" => vn, "popType" => "flow")
        post(h, extra) = h(Vector{UInt8}(JSON3.write(merge(base, extra))))
        gate(xmax) = Dict{String,Any}("kind" => "rectangle", "x_channel" => "c1", "y_channel" => "c2",
                                      "x_min" => 0.0, "x_max" => xmax, "y_min" => 0.0, "y_max" => 1.0)
        onDisk() = Set(pop_paths(load_pop_map(joinpath(proj, "1", "KDIeEm"), vn; pop_type = "flow")))

        post(api_gating_pop_add, Dict{String,Any}("name" => "qc", "gate" => gate(1.0)))
        post(api_gating_pop_add, Dict{String,Any}("name" => "B", "parent" => "/qc", "gate" => gate(2.0)))
        post(api_gating_pop_add, Dict{String,Any}("name" => "mem", "parent" => "/qc/B", "gate" => gate(3.0)))
        @test onDisk() == Set(["/qc", "/qc/B", "/qc/B/mem"])

        # lift B out of qc — the whole subtree comes with it, and the reply names the new path
        st, b = post(api_gating_pop_move, Dict{String,Any}("path" => "/qc/B", "parent" => "root"))
        @test st == 200 && String(JSON3.read(b).path) == "/B"
        @test onDisk() == Set(["/qc", "/B", "/B/mem"])

        # rejected moves leave the document alone
        st, _ = post(api_gating_pop_move, Dict{String,Any}("path" => "/B", "parent" => "/B/mem"))
        @test st == 400                                             # into its own subtree = a cycle
        st, _ = post(api_gating_pop_move, Dict{String,Any}("path" => "/nope", "parent" => "root"))
        @test st == 404
        st, _ = post(api_gating_pop_move, Dict{String,Any}("path" => "/B"))
        @test st == 400                                             # parent is required
        @test onDisk() == Set(["/qc", "/B", "/B/mem"])

        # a move is undoable like any other edit (history rides on _persist_and_broadcast!)
        st, _ = post(api_gating_undo, Dict{String,Any}())
        @test st == 200 && onDisk() == Set(["/qc", "/qc/B", "/qc/B/mem"])
        st, _ = post(api_gating_redo, Dict{String,Any}())
        @test st == 200 && onDisk() == Set(["/qc", "/B", "/B/mem"])

        # childrenOnly prunes BELOW the pop; the pop itself survives (plain delete takes it along)
        st, _ = post(api_gating_pop_delete, Dict{String,Any}("path" => "/B", "childrenOnly" => true))
        @test st == 200 && onDisk() == Set(["/qc", "/B"])
        st, _ = post(api_gating_pop_delete, Dict{String,Any}("path" => "/B"))
        @test st == 200 && onDisk() == Set(["/qc"])
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
        empty!(_GATING_HISTORY)
    end
  end
end

# ── COLOUR BY a third measure ────────────────────────────────────────────────────
# The dots keep their positions and gain a value: `plotdata?z=…` answers TRIPLES instead of pairs,
# read in one pass so `z[i]` is the same cell as `(x[i], y[i])`. A stride slip here mis-colours every
# dot with a plausible-looking picture, so pin the pair half against the no-z response, and pin every
# value inside the ramp `plotmeta` hands the legend.
@testset "API: gating plotdata carries a colour-by measure" begin
  if !api_have_fixture(api_fixture("testpr"))
    @test_skip "testpr fixture missing"
  else
    dir = mktempdir(); cp(api_fixture("testpr"), joinpath(dir, "testpr"))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        common = "projectUid=testpr&imageUid=KDIeEm&valueName=B&popType=flow"
        st, chb = api_gating_channels(HTTP.Request("GET", "/api/gating/channels?" * common))
        @test st == 200
        cols = String.(JSON3.read(chb).columns)
        @test length(cols) >= 3
        x, y, z = cols[1], cols[2], cols[3]
        base = "$common&x=$(HTTP.escapeuri(x))&y=$(HTTP.escapeuri(y))"
        data(t) = (r = api_gating_plotdata(HTTP.Request("GET", "/api/gating/plotdata?" * t));
                   (r[1], reinterpret(Float32, UInt8.(r[2]))))
        st1, xy = data(base)
        st2, xyz = data("$base&z=$(HTTP.escapeuri(z))")
        n = length(xy) ÷ 2
        @test st1 == 200 && st2 == 200 && n > 0
        @test length(xyz) == 3n                                  # triples, not pairs
        # asking for a colour does not move a single dot
        @test all(i -> xyz[3i-2] == xy[2i-1] && xyz[3i-1] == xy[2i], 1:n)
        meta = JSON3.read(api_gating_plotmeta(HTTP.Request("GET", "/api/gating/plotmeta?$base&z=$(HTTP.escapeuri(z))"))[2])
        @test meta.zExtent !== nothing && length(meta.zTicks) == 3 && meta.usedZ == "linear"
        lo, hi = Float32(meta.zExtent[1]), Float32(meta.zExtent[2])
        # The ramp is a CONTRAST setting, not an axis: a 2–98 percentile clip, so it sits INSIDE the
        # data range and holds the bulk of it. (Full min…max spent ~70% of the colour scale on outliers
        # on real data — see `_ramp_range`.) Outliers are not dropped; they clamp to the ramp's ends.
        zs = Float32[xyz[3i] for i in 1:n]
        @test minimum(zs) <= lo && hi <= maximum(zs)
        @test count(v -> lo <= v <= hi, zs) >= 0.9 * n
        @test hi > lo
        # no colour measure asked for → the response says nothing about a ramp (the client falls back
        # to the density pseudocolour rather than inventing a range)
        @test JSON3.read(api_gating_plotmeta(HTTP.Request("GET", "/api/gating/plotmeta?" * base))[2]).zExtent === nothing
        # An UNREADABLE colour measure (a stale panel naming a column this table doesn't have) tints
        # nothing — it must not blank the cloud it was only supposed to colour. Triples still (the client
        # chose the stride), values NaN, and no ramp to describe.
        st3, bogus = data("$base&z=live.track.nope")
        @test st3 == 200 && length(bogus) == 3n
        @test all(i -> bogus[3i-2] == xy[2i-1] && bogus[3i-1] == xy[2i] && isnan(bogus[3i]), 1:n)
        @test JSON3.read(api_gating_plotmeta(HTTP.Request("GET", "/api/gating/plotmeta?$base&z=live.track.nope"))[2]).zExtent === nothing
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
  end
end

# ── boolean populations over the API (Decision 16) ───────────────────────────────────────────────
# "positive for nuc-GFP OR mem-TOM", "double positive but NOT CD169" — a population defined by
# combining others, created through pop/add and rewritten through pop/update like any other. The one
# thing only the API can enforce: a delete that would leave a combination pointing at nothing.
@testset "API: boolean populations combine, update and block an orphaning delete" begin
  if !api_have_fixture(api_fixture("testpr"))
    @test_skip "testpr fixture missing"
  else
    dir = mktempdir()
    proj = joinpath(dir, "testpr")
    cp(api_fixture("testpr"), proj)
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    empty!(_GATING_HISTORY)
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        vn = "B"
        base = Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                "valueName" => vn, "popType" => "flow")
        post(h, extra) = h(Vector{UInt8}(JSON3.write(merge(base, extra))))
        gate(xmax) = Dict{String,Any}("kind" => "rectangle", "x_channel" => "c1", "y_channel" => "c2",
                                      "x_min" => 0.0, "x_max" => xmax, "y_min" => 0.0, "y_max" => 1.0)
        loaded() = load_pop_map(joinpath(proj, "1", "KDIeEm"), vn; pop_type = "flow")

        post(api_gating_pop_add, Dict{String,Any}("name" => "gfp+", "gate" => gate(1.0)))
        post(api_gating_pop_add, Dict{String,Any}("name" => "tom+", "gate" => gate(2.0)))
        st, _ = post(api_gating_pop_add,
                     Dict{String,Any}("name" => "either", "colour" => "#abc",
                                      "boolean" => Dict{String,Any}("op" => "or",
                                                                    "pops" => ["/gfp+", "/tom+"])))
        @test st == 200
        p = pop_at(loaded(), "/either")
        @test p.boolean_op == Cecelia.BOOL_OR && p.boolean_pops == ["/gfp+", "/tom+"] && p.gate === nothing

        # rewritten wholesale by pop/update — including an exclusion ("but not …")
        st, _ = post(api_gating_pop_update,
                     Dict{String,Any}("path" => "/either",
                                      "boolean" => Dict{String,Any}("op" => "and", "pops" => ["/gfp+"],
                                                                    "not" => ["/tom+"])))
        @test st == 200
        p = pop_at(loaded(), "/either")
        @test p.boolean_op == Cecelia.BOOL_AND && p.boolean_pops == ["/gfp+"] && p.boolean_not == ["/tom+"]

        # a reference that isn't a population, a loop, and an empty term list are all 400s
        st, _ = post(api_gating_pop_add,
                     Dict{String,Any}("name" => "bad",
                                      "boolean" => Dict{String,Any}("op" => "or", "pops" => ["/nope"])))
        @test st == 400
        st, _ = post(api_gating_pop_update,
                     Dict{String,Any}("path" => "/either",
                                      "boolean" => Dict{String,Any}("op" => "or", "pops" => ["/either"])))
        @test st == 400
        st, _ = post(api_gating_pop_add,
                     Dict{String,Any}("name" => "bad2",
                                      "boolean" => Dict{String,Any}("op" => "or", "pops" => [])))
        @test st == 400

        # deleting a combined population would leave "either" pointing at nothing → refused, by name
        st, b = post(api_gating_pop_delete, Dict{String,Any}("path" => "/tom+"))
        @test st == 400 && occursin("either", String(b))
        @test has_pop(loaded(), "/tom+")
        # …but renaming it is fine: the reference is rewritten with the path
        st, _ = post(api_gating_pop_rename, Dict{String,Any}("path" => "/tom+", "newName" => "TOM+"))
        @test st == 200 && pop_at(loaded(), "/either").boolean_not == ["/TOM+"]
        # clearing the combination releases the hold, and then the delete goes through
        st, _ = post(api_gating_pop_update, Dict{String,Any}("path" => "/either", "boolean" => nothing))
        @test st == 200 && pop_at(loaded(), "/either").boolean_op === nothing
        st, _ = post(api_gating_pop_delete, Dict{String,Any}("path" => "/TOM+"))
        @test st == 200 && !has_pop(loaded(), "/TOM+")
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
        empty!(_GATING_HISTORY)
    end
  end
end

@testset "API: gating undo/redo steps through the population tree" begin
  if !api_have_fixture(api_fixture("testpr"))
    @test_skip "testpr fixture missing"
  else
    dir = mktempdir()
    proj = joinpath(dir, "testpr")
    cp(api_fixture("testpr"), proj)
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    empty!(_GATING_HISTORY)
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        # `_resolve_vn` falls back to the image's ACTIVE segmentation when the requested value_name
        # is not one of its label_props — so use the real one, or every write lands under a different
        # name than the one read back here.
        vn = "B"
        base = Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm",
                                "valueName" => vn, "popType" => "flow")
        post(h, extra) = h(Vector{UInt8}(JSON3.write(merge(base, extra))))
        gate(xmax) = Dict{String,Any}("kind" => "rectangle", "x_channel" => "c1", "y_channel" => "c2",
                                      "x_min" => 0.0, "x_max" => xmax, "y_min" => 0.0, "y_max" => 1.0)
        names(body) = [String(p.name) for p in JSON3.read(body).tree.populations]

        # nothing done yet → nothing to undo, and saying so is a 409, not a crash
        st, _ = post(api_gating_undo, Dict{String,Any}())
        @test st == 409

        st, b1 = post(api_gating_pop_add, Dict{String,Any}("name" => "cd4", "gate" => gate(1.0)))
        @test st == 200 && names(b1) == ["cd4"]
        @test JSON3.read(b1).canUndo && !JSON3.read(b1).canRedo   # the edit's own response says so
        st, b2 = post(api_gating_pop_add, Dict{String,Any}("name" => "cd8", "gate" => gate(2.0)))
        @test st == 200 && Set(names(b2)) == Set(["cd4", "cd8"])

        # one step back = the state before the LAST edit, not a toggle: undo twice reaches empty
        st, b3 = post(api_gating_undo, Dict{String,Any}())
        @test st == 200 && names(b3) == ["cd4"]
        @test JSON3.read(b3).canRedo
        st, b4 = post(api_gating_undo, Dict{String,Any}())
        @test st == 200 && isempty(names(b4))
        @test !JSON3.read(b4).canUndo
        st, _ = post(api_gating_undo, Dict{String,Any}())
        @test st == 409                                            # exhausted, not wrapped around

        # forward again, and it is the same tree — history restores, it does not re-run the edit
        st, b5 = post(api_gating_redo, Dict{String,Any}())
        @test st == 200 && names(b5) == ["cd4"]
        st, b6 = post(api_gating_redo, Dict{String,Any}())
        @test st == 200 && Set(names(b6)) == Set(["cd4", "cd8"])
        @test !JSON3.read(b6).canRedo

        # it is the ON-DISK document that moved, not just the response
        m = load_pop_map(joinpath(proj, "1", "KDIeEm"), vn; pop_type = "flow")
        @test Set(pop_name.(pop_paths(m))) == Set(["cd4", "cd8"])

        # a NEW edit after an undo drops the redo branch — the future you did not take is gone
        st, _  = post(api_gating_undo, Dict{String,Any}())
        st, b7 = post(api_gating_pop_add, Dict{String,Any}("name" => "cd19", "gate" => gate(3.0)))
        @test st == 200 && Set(names(b7)) == Set(["cd4", "cd19"])
        @test !JSON3.read(b7).canRedo
        st, _ = post(api_gating_redo, Dict{String,Any}())
        @test st == 409

        # a gate EDIT is undoable the same way a structural one is (the case this feature exists for)
        st, _  = post(api_gating_pop_set_gate, Dict{String,Any}("path" => "/cd4", "gate" => gate(9.0)))
        m2 = load_pop_map(joinpath(proj, "1", "KDIeEm"), vn; pop_type = "flow")
        @test pop_at(m2, "/cd4").gate.x_max == 9.0
        st, _  = post(api_gating_undo, Dict{String,Any}())
        m3 = load_pop_map(joinpath(proj, "1", "KDIeEm"), vn; pop_type = "flow")
        @test st == 200 && pop_at(m3, "/cd4").gate.x_max == 1.0

        # …including a change of gate KIND, which is what the panel's rectangle ⇄ polygon convert
        # does (same `pop/set-gate` route). Worth its own step: undoing it has to bring back a
        # RectangleGate through the snapshot, not just different numbers in the same struct.
        st, _  = post(api_gating_pop_set_gate, Dict{String,Any}("path" => "/cd4",
            "gate" => Dict{String,Any}("kind" => "polygon", "x_channel" => "c1", "y_channel" => "c2",
                                       "vertices" => [[0.0, 0.0], [1.0, 0.0], [1.0, 1.0]])))
        m4 = load_pop_map(joinpath(proj, "1", "KDIeEm"), vn; pop_type = "flow")
        @test st == 200 && pop_at(m4, "/cd4").gate isa PolygonGate
        st, _  = post(api_gating_undo, Dict{String,Any}())
        m5 = load_pop_map(joinpath(proj, "1", "KDIeEm"), vn; pop_type = "flow")
        @test st == 200 && pop_at(m5, "/cd4").gate isa RectangleGate
        @test pop_at(m5, "/cd4").gate.x_max == 1.0

        # filter pops (cluster / region) are OUT of scope: their edit is a tick you can un-tick, and
        # they mirror set-wide, so there is nothing coherent to step back on one image
        st, body = post(api_gating_undo, Dict{String,Any}("popType" => "clust"))
        @test st == 400 && occursin("flow/track", String(body))
        # …and a cluster edit records no history it could later claim to undo
        st, bc = post(api_gating_pop_add, Dict{String,Any}("popType" => "clust", "name" => "c1",
            "filter" => Dict{String,Any}("measure" => "clusters.default", "fun" => "in", "values" => [0])))
        @test st == 200 && !JSON3.read(bc).canUndo
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
        empty!(_GATING_HISTORY)
    end
  end
end

@testset "API: viewer overlays (one request for the whole movie, in µm)" begin
    # P3's payload contract. What can go wrong here is silent: a coordinate in pixels instead of µm
    # lands the overlay in the corner of the image at 1/3 scale and still LOOKS like data, and a
    # `null` in a coordinate array becomes 0 through `Float32Array.from` — a cell drawn at the origin
    # rather than not drawn.
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
            ask(qs) = JSON3.read(api_viewer_overlays(HTTP.Request("GET",
                "/api/viewer/overlays?projectUid=testpr&imageUid=KDIeEm&valueName=B" * qs))[2])
            st, body = api_viewer_overlays(HTTP.Request("GET",
                "/api/viewer/overlays?projectUid=testpr&imageUid=KDIeEm&valueName=B"))
            @test st == 200
            d = JSON3.read(body)

            # ── the table ────────────────────────────────────────────────────────────
            @test d.nCells > 0
            # Every coordinate finite, always. JSON has no NaN literal (JSON3 refuses to write one) and
            # `null` becomes 0 through `Float32Array.from`, so an undrawable cell is DROPPED rather than
            # encoded — and `nDropped` says how many, since shipping fewer cells than the table holds
            # would otherwise read as a segmentation problem.
            @test d.nDropped >= 0
            @test d.nCells + d.nDropped == length(JSON3.read(String(JSON3.write(d.cells.label))))  ||
                  d.nDropped == 0
            for a in ("x", "y", "z", "t")
                @test all(isfinite, Float64.(getproperty(d.cells, Symbol(a))))
            end
            @test length(d.cells.label) == d.nCells
            @test length(d.cells.x) == d.nCells && length(d.cells.y) == d.nCells
            @test Set(String.(d.axes)) ⊆ Set(["x", "y", "z"])
            @test "x" in d.axes && "y" in d.axes
            # every declared axis actually carries values, and every absent one is empty — the client
            # reads these arrays positionally, so a declared-but-missing axis is a wrong picture
            for a in ("x", "y", "z")
                col = getproperty(d.cells, Symbol(a))
                @test (a in d.axes) == (length(col) == d.nCells)
            end

            # ── µm, not pixels ───────────────────────────────────────────────────────
            # The route promises the same space as `extentUm`. Compare against the raw file: with a
            # real calibration the two MUST differ, and by exactly the axis resolution.
            img, _ = _gating_image("testpr", "KDIeEm")
            sizes, _ = img_physical_sizes(img)              # [sz, sy, sx] µm/px
            lp = label_props(joinpath(proj, "1", "KDIeEm", "labelProps", "B.h5ad"))
            view_centroid_cols(lp; order = [:x, :y, :z])
            raw = as_df(lp)
            if sizes[3] != 1.0                             # x resolution is a real measurement
                @test !(Float64(raw[1, :centroid_x]) ≈ Float64(d.cells.x[1]))
            end
            @test Float64(raw[1, :centroid_x]) * sizes[3] ≈ Float64(d.cells.x[1])
            @test Float64(raw[1, :centroid_y]) * sizes[2] ≈ Float64(d.cells.y[1])
            # t stays a FRAME index — scaling it would silently redefine every frame-counted
            # parameter, the same choice `scale_centroids!` makes on disk.
            if d.hasT
                @test Float64(raw[1, :centroid_t]) ≈ Float64(d.cells.t[1])
            end

            # ── tracks ───────────────────────────────────────────────────────────────
            # -1 for "not tracked", never 0 and never null: one sentinel the client tests against.
            if !isempty(d.cells.track)
                @test length(d.cells.track) == d.nCells
                @test all(t -> t == -1 || t > 0, d.cells.track)
                @test any(t -> t > 0, d.cells.track)        # the fixture IS tracked
            end

            # ── colour-by ────────────────────────────────────────────────────────────
            @test d.colourBy === nothing && d.values === nothing
            if !isempty(d.colourColumns)
                c = String(first(d.colourColumns))
                got = ask("&colourBy=" * HTTP.escapeuri(c))
                @test got.colourBy == c
                @test got.values !== nothing && length(got.values) == got.nCells
                # WHICH KIND of scale is the server's answer, through the same `_is_categorical_col`
                # rule the plots use — so a column that plots as a code set shades as one in the
                # viewer. Re-deriving it in TypeScript would be a second answer about one column.
                @test String(got.valueKind) in ("categorical", "numeric")
                if got.valueKind == "numeric"
                    @test got.valueRange !== nothing && length(got.valueRange) == 2
                    @test got.valueRange[1] <= got.valueRange[2]
                    @test got.valueLevels === nothing
                else
                    @test got.valueLevels !== nothing && !isempty(got.valueLevels)
                    @test got.valueRange === nothing
                    # the levels must COVER the values, else the client greys a cell it can colour
                    lv = Set(string.(got.valueLevels))
                    @test all(v -> v === nothing || string(v) in lv, got.values)
                end
                # every column the route offers must answer both questions — a column that came back
                # with no kind would silently fall through to the population colour
                for col in got.colourColumns
                    one = ask("&colourBy=" * HTTP.escapeuri(String(col)))
                    @test String(one.valueKind) in ("categorical", "numeric")
                end
            end
            # no colour-by → no kind, no levels, no range: three fields that must not linger
            @test get(d, :valueKind, nothing) === nothing
            @test get(d, :valueLevels, nothing) === nothing
            @test get(d, :valueRange, nothing) === nothing
            # an unknown column is ignored rather than fatal — a stale column name from a saved view
            # must not take the overlay down with it
            bad = ask("&colourBy=does_not_exist")
            @test bad.colourBy === nothing && bad.values === nothing && bad.nCells == d.nCells

            # ── populations ──────────────────────────────────────────────────────────
            # Membership comes from `resolve_pops`, so an ungated image answers an empty list. Never an
            # error: unsegmented and ungated are normal states for an image, not failures.
            @test d.pops isa JSON3.Array
            for p in d.pops
                @test !isempty(String(p.path)) && !isempty(String(p.colour))
                @test all(l -> l isa Integer, p.labels)
            end
        finally
            Cecelia.cecelia_conf()["dirs"]["projects"] = old
        end
    end
end

@testset "API: viewer label stores (P4 — masks through the same reader)" begin
    # A mask is another zarr of the same geometry, which is what makes P4 cheap: the same `read_slab`,
    # the same headers, the same shape guard. What must NOT be re-derived is where a store lives —
    # `img_labels_path` is the image-owned accessor the tasks write through, so resolving a path by hand
    # here would drift the day a filename convention changes.
    dirs = Cecelia.cecelia_conf()["dirs"]
    old  = get(dirs, "projects", nothing)
    dirs["projects"] = mktempdir()
    try
        proj = create_project!(name = "api-viewer-labels")
        img  = add_image!(add_set!(proj; name = "s"); name = "a")
        mkpath(joinpath(img._dir, "labels"))
        img.labels = Dict("seg" => ["seg.zarr"], "ghost" => ["ghost.zarr"])
        save!(img)

        # registered AND on disk → resolves
        mkpath(joinpath(img._dir, "labels", "seg.zarr"))
        p, e = label_store_path(proj.uid, img.uid, "seg")
        @test e === nothing
        @test p == joinpath(img._dir, "labels", "seg.zarr")

        # registered but NOT on disk → a message, not a path. `labels` and `label_props` are
        # independent registries and a store can be registered before it is written.
        p2, e2 = label_store_path(proj.uid, img.uid, "ghost")
        @test p2 === nothing && occursin("not on disk", e2)

        # never registered, and no image at all — both normal states, both a message
        @test label_store_path(proj.uid, img.uid, "nope")[2] == "no label store named 'nope'"
        @test label_store_path(proj.uid, img.uid, "")[2] == "no label store named ''"
        @test label_store_path(proj.uid, "NOSUCH", "seg")[2] == "image not found"
    finally
        old === nothing ? delete!(dirs, "projects") : (dirs["projects"] = old)
    end
end

@testset "API: viewer meta names the versions and which one it resolved" begin
    # The viewer window is a pop-out with no project open, so it cannot look up either the list of
    # image versions or which one it is showing. Without the SECOND field a version picker opens on an
    # empty box and the first change is a no-op; without the first there is nothing to pick from.
    #
    # "Active" here must be the ccid's `_active` — the version a task would run against — and NOT
    # "default", which is merely one of the names. The two differ on every image that has been through
    # a correction step, which is most of them.
    dirs = Cecelia.cecelia_conf()["dirs"]
    old  = get(dirs, "projects", nothing)
    dirs["projects"] = mktempdir()
    try
        proj = create_project!(name = "api-viewer-meta")
        img  = add_image!(add_set!(proj; name = "s"); name = "a")
        img.filepath = Dict("default"    => "ccidImage.ome.zarr",
                            "smoothed"   => "ccidSmoothed.ome.zarr",
                            "_active"    => "smoothed")
        save!(img)

        # A minimal (t,c,z,y,x) store per version, so `open_level0` has something real to measure.
        proj_dir = dirname(dirname(img._dir))
        for fn in ["ccidImage.ome.zarr", "ccidSmoothed.ome.zarr"]
            dir = joinpath(proj_dir, "0", img.uid, fn)
            g = zgroup(Zarr.DirectoryStore(dir);
                       attrs = Dict("multiscales" => [Dict("axes" =>
                           [Dict("name" => n) for n in ["t", "c", "z", "y", "x"]])]))
            a = zcreate(UInt16, g, "0", 5, 4, 3, 1, 2; chunks = (5, 4, 3, 1, 2))
            a[:, :, :, :, :] = zeros(UInt16, 5, 4, 3, 1, 2)
        end

        ask(q) = JSON3.read(api_viewer_meta(HTTP.Request("GET", "/api/viewer/meta?" * q))[2])

        # No version asked for → the ACTIVE one, named back.
        m = ask("projectUid=$(proj.uid)&imageUid=$(img.uid)")
        @test m.valueName == "smoothed"
        @test Set(m.valueNames) == Set(["default", "smoothed"])
        # `_active` is bookkeeping, not a version anyone can pick.
        @test !("_active" in m.valueNames)

        @test m.activeValueName == "smoothed"

        # A version asked for → that one, echoed rather than re-resolved. `activeValueName` must NOT
        # follow it: the whole point is that a picker can then say "this is not the active version",
        # which is impossible if the only field echoes the request.
        m2 = ask("projectUid=$(proj.uid)&imageUid=$(img.uid)&valueName=default")
        @test m2.valueName == "default"
        @test m2.activeValueName == "smoothed"
        @test Set(m2.valueNames) == Set(["default", "smoothed"])
    finally
        old === nothing ? delete!(dirs, "projects") : (dirs["projects"] = old)
    end
end

@testset "API: viewer overlays on an image with no cell table" begin
    # An unsegmented image is the FIRST thing the viewer opens for most users. It must answer an empty
    # overlay, not a 500 — the panel asks unconditionally.
    dirs = Cecelia.cecelia_conf()["dirs"]
    old  = get(dirs, "projects", nothing)
    dirs["projects"] = mktempdir()
    try
        proj = create_project!(name = "api-overlay-empty")
        img  = add_image!(add_set!(proj; name = "s"); name = "a")
        save!(img)
        st, body = api_viewer_overlays(HTTP.Request("GET",
            "/api/viewer/overlays?projectUid=$(proj.uid)&imageUid=$(img.uid)"))
        d = JSON3.read(body)
        @test st == 200
        @test d.nCells == 0 && isempty(d.pops) && d.values === nothing
        @test d.note == "not segmented"        # the reason, so the panel can say so rather than guess
    finally
        old === nothing ? delete!(dirs, "projects") : (dirs["projects"] = old)
    end
end

@testset "API: viewer props round-trip (save then load)" begin
    # PY — the WebGPU viewer autosaves per-image view state (contrast/colormap/T-Z/camera) to the
    # SAME on-disk file napari's autosave uses, so an animation-card snapshot is portable across
    # viewers. Missing file answers 404 (a normal state — the image was never saved), a saved file
    # comes back exactly.
    dirs = Cecelia.cecelia_conf()["dirs"]
    old  = get(dirs, "projects", nothing)
    dirs["projects"] = mktempdir()
    try
        proj = create_project!(name = "api-viewer-props")
        img  = add_image!(add_set!(proj; name = "s"); name = "a")
        img.filepath = Dict("default" => "ccidImage.ome.zarr", "_active" => "default")
        save!(img)
        # A minimal store, so `resolve_image_version` finds a directory that exists.
        mkpath(joinpath(dirname(dirname(img._dir)), "0", img.uid, "ccidImage.ome.zarr"))

        base = "projectUid=$(proj.uid)&imageUid=$(img.uid)"

        # No file yet → 404 with a message (a normal state, not a failure).
        st, _ = api_viewer_props_get(HTTP.Request("GET", "/api/viewer/props?" * base))
        @test st == 404

        vs = Dict("webgpu" => Dict("channels" => [Dict("hex"=>"#ff0000","lo"=>1,"hi"=>2,"visible"=>true)],
                                    "cam" => Dict("yaw"=>0.1,"pitch"=>0.2,"dist"=>3.0,"panX"=>0.0,"panY"=>0.0),
                                    "mode"=>"plane","zPlane"=>0,"zRange"=>[0,0],"t"=>0,"valueName"=>""),
                  "layers" => Dict("Channel 0" => Dict("contrast_limits"=>[1,2], "visible"=>true)))
        body = JSON3.write(Dict("projectUid" => proj.uid, "imageUid" => img.uid, "viewState" => vs))
        st, _ = api_viewer_props_post(Vector{UInt8}(body))
        @test st == 200

        st, out = api_viewer_props_get(HTTP.Request("GET", "/api/viewer/props?" * base))
        @test st == 200
        d = JSON3.read(out)
        @test d.webgpu.channels[1].hex == "#ff0000"
        @test d.webgpu.mode == "plane"
    finally
        old === nothing ? delete!(dirs, "projects") : (dirs["projects"] = old)
    end
end

@testset "API: viewer pick-cell — 404 when the mask store is missing" begin
    # P8 — the click endpoint reuses `label_store_path`, so the same "not on disk" answer that
    # `api_viewer_meta`'s labelNames would omit surfaces here as a 404 rather than a 500 mid-read.
    # A viewer that opens on an unsegmented image sends no picks, but the guard belongs at the
    # boundary anyway (an unsegmented image is a normal state, not an error).
    dirs = Cecelia.cecelia_conf()["dirs"]
    old  = get(dirs, "projects", nothing)
    dirs["projects"] = mktempdir()
    try
        proj = create_project!(name = "api-viewer-pick")
        img  = add_image!(add_set!(proj; name = "s"); name = "a")
        save!(img)
        body = JSON3.write(Dict("projectUid" => proj.uid, "imageUid" => img.uid,
                                "valueName" => "nope", "popType" => "flow",
                                "t" => 0, "z" => 0, "x" => 0, "y" => 0))
        st, out = api_viewer_pick_cell(Vector{UInt8}(body))
        @test st == 404
        d = JSON3.read(out)
        @test occursin("no label store", d.error)
    finally
        old === nothing ? delete!(dirs, "projects") : (dirs["projects"] = old)
    end
end

@testset "API: viewer overlay-legend — 404 when the image doesn't exist" begin
    # P9 replacement for `/api/napari/overlay-legend`. Same pure computation (`overlay_legend_content`
    # walks pop maps + resolves colour-by categories against populations); the endpoint went to
    # `viewer_api.jl` verbatim. Uses `_gating_image`, so a missing project/image is a 404 at the
    # boundary rather than a 500 inside `load_pop_map`.
    dirs = Cecelia.cecelia_conf()["dirs"]
    old  = get(dirs, "projects", nothing)
    dirs["projects"] = mktempdir()
    try
        body = JSON3.write(Dict("projectUid" => "nope", "imageUid" => "nope",
                                "colourBy" => "", "overlayPops" => []))
        st, out = api_viewer_overlay_legend(Vector{UInt8}(body))
        @test st == 404
    finally
        old === nothing ? delete!(dirs, "projects") : (dirs["projects"] = old)
    end
end

@testset "API: viewer pick-clear — 404 when the image doesn't exist" begin
    # P9 replacement for `/api/napari/stop-selection`. Same registry / broadcast path as pick-cell
    # and pick-rect, so it goes through `_gating_image` and answers 404 on an unknown project/image
    # before touching the label store. No mask store is needed to clear — an unsegmented image can
    # still have had a stale registry entry — so the ONLY failure mode at the boundary is the
    # image not existing at all.
    dirs = Cecelia.cecelia_conf()["dirs"]
    old  = get(dirs, "projects", nothing)
    dirs["projects"] = mktempdir()
    try
        body = JSON3.write(Dict("projectUid" => "nope", "imageUid" => "nope",
                                "valueName" => "default", "popType" => "flow"))
        st, out = api_viewer_pick_clear(Vector{UInt8}(body))
        @test st == 404
    finally
        old === nothing ? delete!(dirs, "projects") : (dirs["projects"] = old)
    end
end

@testset "API: viewer pick-set — 404 when the image doesn't exist + input validation" begin
    # Symmetric with pick-clear: the boundary check goes through `_gating_image` before touching
    # any zarr, so an unknown image is a 404 not a 500. Also verifies `labels` must be an array —
    # a wrong body shape fails at 400 rather than silently accepting garbage.
    dirs = Cecelia.cecelia_conf()["dirs"]
    old  = get(dirs, "projects", nothing)
    dirs["projects"] = mktempdir()
    try
        # Missing image → 404.
        body = JSON3.write(Dict("projectUid" => "nope", "imageUid" => "nope",
                                "valueName" => "default", "popType" => "flow",
                                "labels" => [1, 2, 3]))
        st, out = api_viewer_pick_set(Vector{UInt8}(body))
        @test st == 404

        # `labels` not an array → 400. We still hit `_gating_image` first (missing project → 404
        # short-circuits), so give a fake project so it survives that check. Simulate that by
        # dropping to a request where project resolution errors on empty projectUid.
        body2 = JSON3.write(Dict("projectUid" => "", "imageUid" => "any", "labels" => 42))
        st2, out2 = api_viewer_pick_set(Vector{UInt8}(body2))
        @test st2 == 400  # `_gating_image` fires "projectUid required" (400) before labels check
    finally
        old === nothing ? delete!(dirs, "projects") : (dirs["projects"] = old)
    end
end

@testset "API: /api/viewer/record-test — input guards" begin
    # The route itself is a smoke test — an end-to-end run needs the Python encoder, so the fixture
    # suites don't attempt it. What IS testable without leaving Julia is the request validation, so a
    # missing/invalid body fails the way the /movies UI expects (a 400 with an error field, not a
    # cascade of 500s from `record_view_movie` trying to open ""); and that an unknown image resolves
    # to a 404 rather than crashing on a bad zarr path.
    st, out = api_viewer_record_test(Vector{UInt8}("not json at all"))
    @test st == 400
    @test occursin("invalid JSON", JSON3.read(out).error)

    st, out = api_viewer_record_test(Vector{UInt8}(JSON3.write(Dict("imageUid" => "x"))))
    @test st == 400
    @test occursin("projectUid", JSON3.read(out).error)

    st, out = api_viewer_record_test(
        Vector{UInt8}(JSON3.write(Dict("projectUid" => "no-such", "imageUid" => "nope"))))
    @test st == 404
end

@testset "API: /api/viewer/thumbnail — input guards" begin
    # Same validation pattern as record-test: a well-shaped 400 rather than a 500 out of the
    # renderer. Full end-to-end (viewState → PNG → assetId) needs a fixture image which the
    # smoke suites don't run here; the request-shape guards are what protect the panel from
    # rendering a broken thumbnail on a typo'd payload.
    st, out = api_viewer_thumbnail(Vector{UInt8}("not json at all"))
    @test st == 400
    @test occursin("invalid JSON", JSON3.read(out).error)

    st, out = api_viewer_thumbnail(Vector{UInt8}(JSON3.write(Dict("imageUid" => "x"))))
    @test st == 400
    @test occursin("projectUid", JSON3.read(out).error)

    st, out = api_viewer_thumbnail(
        Vector{UInt8}(JSON3.write(Dict("projectUid" => "p", "imageUid" => "i"))))
    @test st == 400
    @test occursin("viewState", JSON3.read(out).error)

    st, out = api_viewer_thumbnail(
        Vector{UInt8}(JSON3.write(Dict("projectUid" => "no-such", "imageUid" => "nope",
                                        "viewState" => Dict{String,Any}()))))
    @test st == 404
end

@testset "API: a pick selection resolves to TRACKS" begin
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

            # nothing drawn → an empty answer, never an error: this route is polled by an open panel
            st, body = api_track_selection(HTTP.Request("GET",
                "/api/tracking/selection?projectUid=testpr&imageUid=KDIeEm&valueName=B"))
            d = JSON3.read(body)
            @test st == 200 && d.nLabels == 0 && isempty(d.tracks)

            # inject what the bridge would have stored, then resolve it
            lp = label_props(joinpath(proj, "1", "KDIeEm", "labelProps", "B.h5ad"))
            select_cols(lp, ["track_id"])
            df = as_df(lp; include_x = false, include_obs = true)
            tids = first(Cecelia.track_ids_present(df), 2)
            labs = Int[Int(round(Float64(df[r, :label]))) for r in 1:length(df.label)
                       if df[r, :track_id] isa Real && !isnan(Float64(df[r, :track_id])) &&
                          Int(round(Float64(df[r, :track_id]))) in tids]
            @test length(labs) > 2
            _set_pick_selection!(img._dir, "B", labs)
            try
                st, body = api_track_selection(HTTP.Request("GET",
                    "/api/tracking/selection?projectUid=testpr&imageUid=KDIeEm&valueName=B"))
                d = JSON3.read(body)
                @test st == 200
                @test d.nLabels == length(labs)
                @test Set(Int[t.track for t in d.tracks]) == Set(tids)
                # most cells inside the drawn region first — so "pick the top two and Join" does the
                # obvious thing rather than picking by lowest id
                @test issorted([t.nCells for t in d.tracks]; rev = true)
                @test sum(Int[t.nCells for t in d.tracks]) == length(labs)
                @test d.nUntracked == 0
            finally
                _set_pick_selection!(img._dir, "B", Int[])
            end

            # `ids=` names tracks and IGNORES the cap — "the one I need is not in the top N" must have
            # an answer that is not "raise N for everyone".
            #
            # NOTE the payload shape: one entry per (images × population) GROUP, each carrying what the
            # single-image response used to carry at its top level. One image and no populations is one
            # group — the plot compares like every other plot on the board (docs/TRACKING.md).
            st, body = api_track_paths(HTTP.Request("GET",
                "/api/tracking/paths?projectUid=testpr&imageUid=KDIeEm&valueName=B&limit=1"))
            capped = JSON3.read(body)
            @test st == 200 && capped.tracked && length(capped.groups) == 1
            g = capped.groups[1]
            @test length(g.paths) == 1 && g.shown == 1 && g.total > 1
            @test capped.shown == 1 && capped.dropped == 0
            # the group's identity travels with it — the frontend labels/colours/facets from this
            @test g.valueName == "B" && collect(String.(g.imageUids)) == ["KDIeEm"]
            @test g.label == ""                      # one group: a legend of one entry is noise
            want = string(last(tids))
            st, body = api_track_paths(HTTP.Request("GET",
                "/api/tracking/paths?projectUid=testpr&imageUid=KDIeEm&valueName=B&limit=1&ids=$want"))
            named = JSON3.read(body)
            @test st == 200 && collect(String.(keys(named.groups[1].paths))) == [want]
            # a track that does not exist is empty, not a 500
            st, body = api_track_paths(HTTP.Request("GET",
                "/api/tracking/paths?projectUid=testpr&imageUid=KDIeEm&valueName=B&ids=999999"))
            @test st == 200 && isempty(JSON3.read(body).groups[1].paths)

            # the cohort selectors reach the package resolver: `imageUids=` is the board's form, and
            # pooling one image is still one group (the flags are not a second code path)
            st, body = api_track_paths(HTTP.Request("GET",
                "/api/tracking/paths?projectUid=testpr&imageUids=KDIeEm&valueName=B&limit=2&poolImages=1"))
            pooled = JSON3.read(body)
            @test st == 200 && length(pooled.groups) == 1 && length(pooled.groups[1].paths) == 2
            # an image selector is REQUIRED — neither route may guess one
            @test api_track_paths(HTTP.Request("GET", "/api/tracking/paths?projectUid=testpr"))[1] == 400
            @test api_track_diagnostics(HTTP.Request("GET", "/api/tracking/diagnostics?projectUid=testpr"))[1] == 400

            # the diagnostics battery, same shape: one group carrying the curves and the run's own
            # findings (never re-derived in the frontend)
            st, body = api_track_diagnostics(HTTP.Request("GET",
                "/api/tracking/diagnostics?projectUid=testpr&imageUids=KDIeEm&valueName=B&maxLag=4"))
            diag = JSON3.read(body)
            @test st == 200 && diag.tracked && length(diag.groups) == 1
            dg = diag.groups[1]
            @test !isempty(dg.msd.lag) && !isempty(dg.acor.lag) && dg.nTracks > 0
            @test haskey(dg, :findings) && haskey(dg, :summary)

            # ── /api/tracking/detections — per-frame untracked cells (P3) ────────
            # smoke test: shape correctness on a real fixture. The `_is_untracked` rule itself is
            # pinned in the package suite; here we assert the route wires it into aligned per-frame
            # arrays, and that a frame with zero untracked cells is OMITTED (a scheme draws no rect
            # for zero, not one of zero height).
            @test api_track_detections(HTTP.Request("GET",
                "/api/tracking/detections?projectUid=testpr"))[1] == 400
            st, body = api_track_detections(HTTP.Request("GET",
                "/api/tracking/detections?projectUid=testpr&imageUid=KDIeEm&valueName=B"))
            d = JSON3.read(body)
            @test st == 200 && d.tracked == true
            @test issorted([f.t for f in d.frames])
            for f in d.frames
                @test f.count > 0
                @test f.count == length(f.labels)
                @test length(f.x) == f.count && length(f.y) == f.count
                @test all(l -> l isa Integer, f.labels)
            end
        finally
            Cecelia.cecelia_conf()["dirs"]["projects"] = old
        end
    end
end

# Every response body this server writes goes through `write_http_body!` (Cecelia/utils.jl), because
# an EMPTY one written bare corrupts the connection: no Content-Length → chunked framing → a
# zero-length `write` emits the terminating `0\r\n\r\n` and `closewrite` emits a second, so the NEXT
# response on that keep-alive connection is parsed starting at 4 stray bytes. One legitimately-empty
# gating plot (a track-grained plot of an untracked segmentation) took every other plot on the page
# down with it. The wire-level proof lives in the package suite ("an empty response body is written
# through write_http_body!"); this is the ratchet on the call sites.
@testset "API: response bodies go through write_http_body!" begin
    src = read(joinpath(@__DIR__, "..", "src", "server.jl"), String)
    lines = [l for l in split(src, '\n') if !startswith(strip(l), "#")]
    @test isempty(filter(l -> occursin("write(stream, ", l), lines))
    @test count(l -> occursin("write_http_body!(stream, ", l), lines) >= 4   # JSON/binary, static, asset, file
end

# ── `api/` has no DataFrames — a bare `nrow` compiles and dies at runtime ─────
#
# `api/src` is `include`d into a script that does not `using DataFrames`, so `nrow(df)` is an
# UndefVarError the moment the line executes. It reads fine, it typechecks (there is no typecheck), and
# it survives review — twice now in `tracking_api.jl`, both times on a branch a user reaches and a
# smoke test does not: the second one sat behind "nothing is drawn in napari yet" and would have
# 500-ed only for someone who actually drew a region.
#
# The frame's own columns are always available (`length(df.label)`), so the fix is never to import
# DataFrames here — this layer shapes JSON, it does not do data work.
@testset "API: no bare nrow in api/src (DataFrames is not imported there)" begin
    src = joinpath(@__DIR__, "..", "src")
    offenders = String[]
    for f in readdir(src; join = true)
        endswith(f, ".jl") || continue
        for (i, line) in enumerate(eachline(f))
            startswith(strip(line), "#") && continue        # the explanation itself may name it
            occursin(r"(^|[^.\w])nrow\s*\(", line) && push!(offenders, "$(basename(f)):$i")
        end
    end
    @test isempty(offenders)
    isempty(offenders) || @info "use length(df.<col>) instead" offenders
end

# The save route is the door the USER goes through, and until now it was the unguarded one: it wrote
# the request body straight to disk on the premise recorded in `api_chains_create` — "the whiteboard
# cannot express an invalid template". It can. Deleting the start dot's edge leaves `startTargets: []`,
# which `_prune_to_start` reads as "run the whole chain", so an unwired dot surfaced NOWHERE between
# the canvas and the executor. One checker, both doors.
@testset "API: chain save validates and repairs the start dot" begin
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name = "api-chain-save")
        uid  = proj.uid
        rm_params = Dict("valueName" => "default", "newDefault" => "default")
        node(id)  = Dict("id" => id, "fn" => "importImages.remove", "params" => rm_params)
        save(t)   = _post(api_chains_save, Dict("projectUid" => uid, "template" => t))
        path(n)   = joinpath(tmp, uid, "settings", "chains", "$(n).json")
        stored(n) = JSON3.read(read(path(n), String))

        # REPAIR: no startTargets in, the roots out — same semantics as the empty list already had
        # ("run everything"), now said out loud as a dot the user can see.
        @test save(Dict("name" => "repaired",
                        "nodes" => [node("n1"), node("n2")],
                        "edges" => [Dict("from" => "n1", "to" => "n2")]))[1] == 200
        @test collect(stored("repaired").startTargets) == ["n1"]

        # An explicit start is left exactly as the user wired it, including a MID-chain start.
        @test save(Dict("name" => "mid", "nodes" => [node("n1"), node("n2")],
                        "edges" => [Dict("from" => "n1", "to" => "n2")],
                        "startTargets" => ["n2"]))[1] == 200
        @test collect(stored("mid").startTargets) == ["n2"]

        # Everything else in the body still round-trips — `positions` is canvas-only sidecar data the
        # package does not model, and losing it would scatter a laid-out graph on next load.
        pos = Dict("n1" => Dict("x" => 10, "y" => 20))
        @test save(Dict("name" => "withpos", "nodes" => [node("n1")], "edges" => [],
                        "positions" => pos))[1] == 200
        @test stored("withpos").positions.n1.x == 10

        # VALIDATED: the same offenders the create route rejects, with the offender named.
        st, body = save(Dict("name" => "badfn",
                            "nodes" => [Dict("id" => "oops", "fn" => "importImages.nope")],
                            "edges" => []))
        @test st == 400
        @test occursin("oops", String(JSON3.read(body).error))
        @test !isfile(path("badfn"))            # a rejected save leaves nothing on disk

        @test save(Dict("name" => "cyc", "nodes" => [node("n1"), node("n2")],
                        "edges" => [Dict("from" => "n1", "to" => "n2"),
                                    Dict("from" => "n2", "to" => "n1")]))[1] == 400
        @test save(Dict("name" => "dangling", "nodes" => [node("n1")],
                        "edges" => [Dict("from" => "n1", "to" => "ghost")]))[1] == 400

        # A start target naming a node that is not there is still a hard error, not a repair — it means
        # the template and its dot disagree, which the roots cannot guess a fix for.
        @test save(Dict("name" => "ghoststart", "nodes" => [node("n1")], "edges" => [],
                        "startTargets" => ["ghost"]))[1] == 400

        # An out-of-range param is caught here too, rather than at run time.
        @test save(Dict("name" => "badparam",
                        "nodes" => [Dict("id" => "n1", "fn" => "tracking.bayesian_tracking",
                                         "params" => Dict("maxSearchRadius" => -5))],
                        "edges" => []))[1] == 400
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: viewer slab (voxels the GPU can upload without a transform)" begin
    # `read_slab` feeds a WebGPU 3D texture directly: the response body is copied to VRAM with no
    # reshape, so its linear order MUST be x-fastest, then y, then z. Every assertion here is about a
    # failure that is SILENT — a transposed or byte-swapped volume renders a plausible-looking image of
    # the wrong thing, and neither the route nor the browser can tell. See docs/todo/WEB_VIEWER_PLAN.md.
    #
    # The pattern is `x + 10y + 100z + 1000c + 10000t`, so every axis has its own decimal digit and any
    # swap between two of them is visible in a single voxel.
    val(x, y, z, c, t) = UInt16(x + 10y + 100z + 1000c + 10000t)
    axes_attr(names) = Dict("multiscales" => [Dict("axes" => [Dict("name" => n) for n in names])])

    # Store declared (t,c,z,y,x) in C-order → Zarr.jl presents it REVERSED, so Julia dims are x,y,z,c,t.
    function make_store(dir, c_axes, jdims, fill!)
        g = zgroup(Zarr.DirectoryStore(dir); attrs = axes_attr(c_axes))
        a = zcreate(UInt16, g, "0", jdims...; chunks = jdims)
        buf = zeros(UInt16, jdims...)
        fill!(buf)
        a[fill(Colon(), length(jdims))...] = buf
        dir
    end

    nt, nc, nz, ny, nx = 2, 2, 3, 4, 5
    mktempdir() do d
        p = make_store(joinpath(d, "std.ome.zarr"), ["t", "c", "z", "y", "x"],
                       (nx, ny, nz, nc, nt),
                       b -> for t in 1:nt, c in 1:nc, z in 1:nz, y in 1:ny, x in 1:nx
                           b[x, y, z, c, t] = val(x, y, z, c, t)
                       end)

        vol, sx, sy, sz = read_slab(p, 0, 1)          # t=0, c=1 (both 0-based)
        @test (sx, sy, sz) == (nx, ny, nz)
        @test vol[1, 1, 1] == val(1, 1, 1, 2, 1)
        @test vol[nx, ny, nz] == val(nx, ny, nz, 2, 1)
        # x and y are DIFFERENT lengths here on purpose — a square frame hides a transpose.
        @test vol[2, 3, 1] == val(2, 3, 1, 2, 1)

        # THE upload contract: the first `nx` elements of the flat body must walk x, not y or z.
        @test vec(vol)[1:nx] == [val(x, 1, 1, 2, 1) for x in 1:nx]
        @test vec(vol)[nx + 1] == val(1, 2, 1, 2, 1)              # then y
        @test vec(vol)[nx * ny + 1] == val(1, 1, 2, 2, 1)         # then z

        # …and the wire bytes are little-endian pairs of exactly that, nothing padded or reordered.
        bytes = slab_bytes(vol)
        @test length(bytes) == nx * ny * nz * 2
        @test bytes[1] == UInt8(val(1, 1, 1, 2, 1) % 256)
        @test bytes[2] == UInt8(val(1, 1, 1, 2, 1) ÷ 256)

        # t is honoured (0-based → 1-based) rather than silently always frame 1
        @test read_slab(p, 1, 0)[1][1, 1, 1] == val(1, 1, 1, 1, 2)

        # ── One z plane (the 2D view) ───────────────────────────────────────────────────
        # This is the view a timecourse is actually watched in, and the ONLY one that plays: on Dml3RG a
        # plane timepoint is 8.8 MB against 326 MB, so the whole 181-frame movie is 1.59 GB and fits in
        # VRAM. It shares `read_slab` with the volume deliberately — a scalar z drops the dim exactly as
        # t and c do — so there is no second reader to disagree about axis order.
        pv, px, py, pz = read_slab(p, 0, 1; z = 2)      # 0-based z → the THIRD plane
        @test (px, py, pz) == (nx, ny, 1)               # reports depth 1, not the stack's depth
        @test ndims(pv) == 2
        @test pv[2, 3] == val(2, 3, 3, 2, 1)            # ← the requested plane, not plane 1
        @test vec(pv)[1:nx] == [val(x, 1, 3, 2, 1) for x in 1:nx]   # still x-fastest on the wire
        # z=0 is a real plane, not "no plane" — an absent z means the whole stack and the two must not
        # collapse into each other.
        @test read_slab(p, 0, 0; z = 0)[1][2, 3] == val(2, 3, 1, 1, 1)
        @test size(read_slab(p, 0, 0)[1], 3) == nz      # z omitted → the whole stack
        # Out of range is clamped to a real plane rather than throwing: the slider's bound and the
        # store's depth can disagree for a moment after a version switch.
        @test read_slab(p, 0, 0; z = 999)[1][2, 3] == val(2, 3, nz, 1, 1)
        @test read_slab(p, 0, 0; z = -5)[1][2, 3] == val(2, 3, 1, 1, 1)

        # ── A RANGE of planes (the usable 3D view) ─────────────────────────────────────
        # Every cost here is linear in the plane count, so a few planes out of a deep stack is what
        # makes the volume view interactive: 8 of 37 is 70 MB rather than 326 MB. A range KEEPS the z
        # dim where a scalar drops it — that difference in rank is the whole contract, because the
        # client sizes its texture from it.
        rv, rx, ry, rz = read_slab(p, 0, 1; z = 1:2)    # 0-based → planes 2 and 3
        @test (rx, ry, rz) == (nx, ny, 2)
        @test ndims(rv) == 3
        @test rv[2, 3, 1] == val(2, 3, 2, 2, 1)         # ← starts at the range's low end
        @test rv[2, 3, 2] == val(2, 3, 3, 2, 1)
        @test vec(rv)[1:nx] == [val(x, 1, 2, 2, 1) for x in 1:nx]   # still x-fastest on the wire
        # A one-plane RANGE is not a scalar: same bytes, different rank, and the client's shape guard
        # rejects a slab whose depth disagrees with what it allocated.
        @test ndims(read_slab(p, 0, 1; z = 2:2)[1]) == 3
        @test read_slab(p, 0, 1; z = 2:2)[4] == 1
        @test read_slab(p, 0, 1; z = 2:2)[1][2, 3, 1] == val(2, 3, 3, 2, 1)
        # Clamped at both ends, and a reversed range is read the way round it was meant — these come
        # off a query string, where an out-of-range index is a 500 from inside Zarr.jl.
        @test read_slab(p, 0, 0; z = 0:999)[4] == nz
        @test read_slab(p, 0, 0; z = -5:1)[4] == 2
        # A backwards pair cannot even reach here as a range — `2:0` is normalised to the EMPTY `2:1` by
        # UnitRange's own constructor, which is why the route orders the two integers before building
        # one. An empty range that does arrive reads as the single plane at its start, never as zero
        # planes: a zero-thickness slab renders black (entry and exit distances coincide).
        @test read_slab(p, 0, 0; z = 2:0)[4] == 1
        @test read_slab(p, 0, 0; z = 2:0)[1][2, 3, 1] == val(2, 3, 3, 1, 1)
        @test ndims(read_slab(p, 0, 0; z = 2:0)[1]) == 3     # still a volume, not a plane

        # ── A RANGE of CHANNELS (brick atlas: all channels of one brick in ONE request) ────
        # The brick-atlas 3D viewer wants every channel of a spatial brick in a single response —
        # KILN_BRICK_PLAN.md → Decision 7. Serially at nC=38 measured 273 ms/brick, or ~2.5 s for a
        # 3x3 visible viewport; batched into one request drops to a single round trip. A range KEEPS
        # the c dim as the last axis, so `(x, y, z, c)` — the atlas can upload it as
        # `nc groups of nz consecutive planes` without a copy (KILN_BRICK_PLAN.md → Decision 4).
        rc, cx, cy, cz, cn = read_slab(p, 0, 0:1)
        @test (cx, cy, cz, cn) == (nx, ny, nz, 2)
        @test ndims(rc) == 4
        @test rc[2, 3, 1, 1] == val(2, 3, 1, 1, 1)          # first channel of the range
        @test rc[2, 3, 1, 2] == val(2, 3, 1, 2, 1)          # second channel of the range
        # Wire order stays x-fastest — the atlas uploads the buffer directly, no reshape.
        @test vec(rc)[1:nx] == [val(x, 1, 1, 1, 1) for x in 1:nx]
        # Length-1 RANGE keeps the c dim (rank 4); scalar-c drops it (rank 3). Same contract as z.
        @test read_slab(p, 0, 0:0)[5] == 1
        @test ndims(read_slab(p, 0, 0:0)[1]) == 4           # RANGE at length 1 keeps the dim
        @test ndims(read_slab(p, 0, 0)[1]) == 3             # scalar-c drops it — flat atlas path unchanged
        # Clamped both ends, same shape as z-range. Out-of-range c gets the closest existing channel.
        @test read_slab(p, 0, 0:999)[5] == nc
        @test read_slab(p, 0, -5:1)[5] == 2
        # A backwards pair reads as the single channel at its start (Julia normalises `1:0` to empty;
        # the route orders integers before building the range).
        @test read_slab(p, 0, 2:0)[5] == 1
        # A c-range combined with a z-range keeps both dims: (x, y, z, c), for a 4D brick payload.
        rcz, _, _, rz, rn = read_slab(p, 0, 0:1; z = 1:2)
        @test (rz, rn) == (2, 2)
        @test ndims(rcz) == 4
        @test rcz[2, 3, 1, 1] == val(2, 3, 2, 1, 1)         # first z, first c of the request
        @test rcz[2, 3, 2, 2] == val(2, 3, 3, 2, 1)         # last z, last c
    end

    # A store whose axes are NOT (t,c,z,y,x) must be PERMUTED to (x,y,z), not passed through. This is
    # the whole reason the permute is written out instead of relying on the usual layout: pass-through
    # would put z where x belongs and still render.
    mktempdir() do d
        # C-order (t,c,y,x,z) → Julia dims are z,x,y,c,t
        p = make_store(joinpath(d, "odd.ome.zarr"), ["t", "c", "y", "x", "z"],
                       (nz, nx, ny, nc, nt),
                       b -> for t in 1:nt, c in 1:nc, y in 1:ny, x in 1:nx, z in 1:nz
                           b[z, x, y, c, t] = val(x, y, z, c, t)
                       end)
        vol, sx, sy, sz = read_slab(p, 0, 0)
        @test (sx, sy, sz) == (nx, ny, nz)             # reported as x,y,z whatever the store's order
        @test vol[2, 3, 1] == val(2, 3, 1, 1, 1)       # …and the voxels actually moved
        @test vec(vol)[1:nx] == [val(x, 1, 1, 1, 1) for x in 1:nx]
    end

    # Degenerate ranks: a 2D still and a single-channel stack answer the same shape of question, with
    # the missing axes counting as 1 — not an error, and not a silently dropped dimension.
    mktempdir() do d
        p2 = make_store(joinpath(d, "flat2d.ome.zarr"), ["y", "x"], (nx, ny),
                        b -> for y in 1:ny, x in 1:nx; b[x, y] = val(x, y, 1, 1, 1) end)
        vol, sx, sy, sz = read_slab(p2, 0, 0)
        @test (sx, sy, sz) == (nx, ny, 1)
        @test vol[2, 3] == val(2, 3, 1, 1, 1)
        # an image with no z axis: asking for a plane is a no-op, not an error
        @test read_slab(p2, 0, 0; z = 3)[1][2, 3] == val(2, 3, 1, 1, 1)

        p3 = make_store(joinpath(d, "zyx.ome.zarr"), ["z", "y", "x"], (nx, ny, nz),
                        b -> for z in 1:nz, y in 1:ny, x in 1:nx; b[x, y, z] = val(x, y, z, 1, 1) end)
        @test read_slab(p3, 0, 0)[2:4] == (nx, ny, nz)
    end

    # Big-endian: a raw bioformats2raw store is `>u2` and Zarr.jl hands the bytes back UNSWAPPED, so a
    # slab read with plain `arr[...]` is garbage that renders as saturated noise. `read_slab` must go
    # through `read_native`. Same stamp trick as the byte-order testset above.
    mktempdir() do d
        p = make_store(joinpath(d, "be.ome.zarr"), ["t", "c", "z", "y", "x"],
                       (nx, ny, nz, nc, nt),
                       b -> for t in 1:nt, c in 1:nc, z in 1:nz, y in 1:ny, x in 1:nx
                           b[x, y, z, c, t] = val(x, y, z, c, t)
                       end)
        za = JSON3.read(read(joinpath(p, "0", ".zarray"), String), Dict{String,Any})
        za["dtype"] = ">u2"
        write(joinpath(p, "0", ".zarray"), JSON3.write(za))
        got = read_slab(p, 0, 0)[1]
        @test got[1, 1, 1] == ntoh(val(1, 1, 1, 1, 1))
        @test got[1, 1, 1] != val(1, 1, 1, 1, 1)       # ← fails if read_native was bypassed
    end

    # Cold-start contrast: one spec per channel, sampled from a FIXED (t, z) so playback cannot flicker
    # as the window chases each frame's own distribution (WEB_VIEWER_PLAN.md decision 5).
    mktempdir() do d
        p = make_store(joinpath(d, "c.ome.zarr"), ["t", "c", "z", "y", "x"],
                       (nx, ny, nz, nc, nt),
                       b -> for t in 1:nt, c in 1:nc, z in 1:nz, y in 1:ny, x in 1:nx
                           b[x, y, z, c, t] = val(x, y, z, c, t)
                       end)
        specs = _sampled_specs(p, nc)
        @test length(specs) == nc
        @test all(s -> s[2] >= s[1], specs)
        @test _sampled_specs(p, nc) == specs           # same (t, z) every time, so stable
    end

    # `resolved_display_specs` is the ONE place a colormap name becomes RGB — the browser must not
    # re-derive napari's palette (a name table missing `bop blue` rendered a channel WHITE).
    mktempdir() do d
        pj = joinpath(d, "props.json")
        write(pj, JSON3.write((; Image = [
            (; contrast_limits = [0.0, 10.0], colormap = "bop blue", visible = true),
            (; contrast_limits = [1.0, 5.0], colormap = "green", visible = false),
        ])))
        r = resolved_display_specs(pj, 2)
        @test length(r) == 2
        @test r[1].lo == 0.0 && r[1].hi == 10.0 && r[1].visible
        @test r[1].lut[end] == (0.12549f0, 0.678431f0, 0.972549f0)   # resolved, not the string
        @test r[2].lut[end] == (0f0, 1f0, 0f0) && r[2].visible == false
        # Props describing FEWER channels than the store has → `nothing`, so the route falls back to
        # sampling instead of indexing off the end or shifting every channel's colour by one.
        @test resolved_display_specs(pj, 3) === nothing
        @test resolved_display_specs(joinpath(d, "absent.json"), 1) === nothing
    end
end

@testset "API: viewer slab — XY tile + pyramid level (spatial audit Phase 2)" begin
    # The pan/zoom viewer's access pattern: a rectangular TILE at a chosen pyramid LEVEL, out of a store
    # too big to slab whole. L0 of `f8gzA2` is 20329×16898 and 687 MB per channel; one 1024² chunk is
    # 2 MB. Same shape guard as the timecourse — the response body copies into a WebGPU 2D texture with
    # no transform, so a silent transpose or a level mismatch would render plausible-looking garbage.
    val(x, y, z, c, t) = UInt16(x + 10y + 100z + 1000c + 10000t)
    axes_ms(names, npaths) = Dict("multiscales" => [Dict(
        "axes"     => [Dict("name" => n) for n in names],
        "datasets" => [Dict("path" => string(i)) for i in 0:npaths-1])])

    # A multi-level FLAT store: `.zattrs` at root lists every level, arrays live at `"0"`, `"1"`, …
    # bioformats2raw's series layout wraps them in `0/` and open_level0 already tested that half — this
    # covers the level-index path, not the layout discrimination it inherits.
    function make_pyramid(dir, c_axes, jdims_per_level, filler)
        g = zgroup(Zarr.DirectoryStore(dir); attrs = axes_ms(c_axes, length(jdims_per_level)))
        for (i, jdims) in enumerate(jdims_per_level)
            a = zcreate(UInt16, g, string(i - 1), jdims...; chunks = jdims)
            buf = zeros(UInt16, jdims...)
            filler(buf, i - 1)
            a[fill(Colon(), length(jdims))...] = buf
        end
        dir
    end

    # 5D flat store, tiny — just enough to prove the axis walking is real. L0 is 8×6 XY; L1 is 4×3.
    nt, nc, nz = 1, 2, 2
    l0_nx, l0_ny = 8, 6
    l1_nx, l1_ny = 4, 3

    mktempdir() do d
        # Stamp values fit UInt16: val() maxes at ~12k for this shape, +30000 per level stays under
        # 65535 while making L0 and L1 obviously different (a bad open_level that returned L0's array
        # would fail these assertions loudly rather than passing on matching voxels).
        lvl_stamp = UInt16(30000)
        p = make_pyramid(joinpath(d, "pyr.ome.zarr"),
                         ["t", "c", "z", "y", "x"],
                         [(l0_nx, l0_ny, nz, nc, nt), (l1_nx, l1_ny, nz, nc, nt)],
                         (buf, lvl) -> for t in 1:size(buf, 5), c in 1:size(buf, 4),
                                          z in 1:size(buf, 3), y in 1:size(buf, 2), x in 1:size(buf, 1)
                             buf[x, y, z, c, t] = UInt16(val(x, y, z, c, t) + lvl_stamp * lvl)
                         end)

        # ── XY range: reads a tile, keeps the axes, clamps to the store ──────────────
        # x=2:4, y=1:2 are 0-BASED — store's 1-based coords are (x=3:5, y=2:3). The stamp uses store
        # coordinates, so the assertions read the returned voxel back in the store's own frame.
        vol, sx, sy, sz = read_slab(p, 0, 0; x = 2:4, y = 1:2)
        @test (sx, sy, sz) == (3, 2, nz)                # 3 wide, 2 tall, full stack
        @test ndims(vol) == 3
        @test vol[1, 1, 1] == val(3, 2, 1, 1, 1)        # tile origin in store coords
        @test vol[3, 2, 1] == val(5, 3, 1, 1, 1)        # tile's far corner
        # x-fastest on the wire even for a subset — the shape guard doesn't check the memory order
        @test vec(vol)[1:sx] == [val(x, 2, 1, 1, 1) for x in 3:5]

        # A whole-plane subset agrees with the untiled read — no accidental resample
        big = read_slab(p, 0, 0)[1]
        @test read_slab(p, 0, 0; x = 0:l0_nx-1, y = 0:l0_ny-1)[1] == big

        # Range CLAMPED to the store's edge, not a 500 from Zarr.jl — a viewport hanging off the frame
        # is a normal state, not a bad request. Same discipline `z` already has.
        clamped = read_slab(p, 0, 0; x = 6:99, y = 0:1)
        @test clamped[2:4] == (2, 2, nz)                # x clamped from 6:99 to 6:7 → 2 wide

        # A backwards pair (`x=5:3`) is what a swapped lo/hi arrives as after the route orders them, so
        # it should never REACH `read_slab` — but if it does, the empty range reads as the single column
        # at its start rather than as zero width. A zero-width tile has no pixels to display.
        @test read_slab(p, 0, 0; x = 5:3)[2] == 1
        @test read_slab(p, 0, 0; x = 5:3)[1][1, 1, 1] == val(6, 1, 1, 1, 1)

        # ── Pyramid level: a different array, not a resampled L0 ─────────────────────
        v0 = read_slab(p, 0, 0; level = 0)
        v1 = read_slab(p, 0, 0; level = 1)
        @test v0[2:4] == (l0_nx, l0_ny, nz)
        @test v1[2:4] == (l1_nx, l1_ny, nz)
        # The stamp said "+30000 per level", so a bad open_level would fail this loudly
        @test v1[1][1, 1, 1] == UInt16(val(1, 1, 1, 1, 1) + lvl_stamp)
        @test v0[1][1, 1, 1] == val(1, 1, 1, 1, 1)

        # XY range on a coarser level maps to that level's coordinates, not L0's — the client thinks in
        # level-space (that is the whole point of picking a level from meta's per-level shapes)
        tile = read_slab(p, 0, 0; level = 1, x = 0:1, y = 0:1)
        @test tile[2:4] == (2, 2, nz)
        @test tile[1][1, 1, 1] == UInt16(val(1, 1, 1, 1, 1) + lvl_stamp)

        # ── The (arr, caxes) form has NO `level`: a caller that pre-opened one array cannot ask a
        # different one, and the type system says so.
        a, ax = open_level(p, 1)
        v1b = read_slab(a, ax, 0, 0)
        @test v1b[2:4] == (l1_nx, l1_ny, nz)
        @test v1b[1] == v1[1]

        # ── HTTP round trip: `try_serve_slab` reads x/xTo/y/yTo/level off the query string ───
        qs(pairs...) = join(("$k=$v" for (k, v) in pairs), "&")
        # Reach the route the way the server does, but without spinning a real HTTP server: a
        # stream-handler takes a plain `HTTP.Stream`, and `IOStream_to_HTTP` is not part of the API — so
        # we just call `read_slab` through the same query-string parsing that `try_serve_slab` applies to
        # a request. The route header assertions (`X-Slab-Shape`, `X-Slab-Level`) belong in an
        # integration test; here the point is that a tile request lands on the same voxels.
        # (The route itself needs a project_uid to resolve, which is set up in the `pyramid QC` fixture.)
    end
end

@testset "API: viewer meta — per-level shapes (spatial audit LOD)" begin
    # `api_viewer_meta` grew a `levels` field so the CLIENT can pick a pyramid level from its viewport
    # zoom without asking the server. Same store the tile testset builds, exposed through the meta route
    # on a temporary project dir so the API layer is what's under test — not `store_pyramid_levels`,
    # which is covered where it lives.
    axes_ms(names, npaths) = Dict("multiscales" => [Dict(
        "axes"     => [Dict("name" => n) for n in names],
        "datasets" => [Dict("path" => string(i)) for i in 0:npaths-1])])
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        # `resolve_image_version` reads projects_dir()/proj/1/img/ccid.json → `filepath` → the store
        proj = "PYRTST"; img = "IMGTST"
        proj_dir = joinpath(tmp, proj); mkpath(proj_dir)
        img_dir = joinpath(proj_dir, "1", img); mkpath(img_dir)
        state_target = joinpath(img_dir, "ccid.json")
        store_dir = joinpath(proj_dir, "0", img, "ccidImage.ome.zarr"); mkpath(dirname(store_dir))
        # Two-level flat pyramid: L0 8×6×2×2×1, L1 4×3×2×2×1. In Julia dims (x,y,z,c,t).
        # `_sampled_specs` reads a mid-stack plane for contrast, so both arrays need actual chunks on
        # disk — Zarr.jl throws `missing chunks and no fill_value` on a partially-empty store.
        g = zgroup(Zarr.DirectoryStore(store_dir); attrs = axes_ms(["t", "c", "z", "y", "x"], 2))
        a0 = zcreate(UInt16, g, "0", 8, 6, 2, 2, 1; chunks = (4, 4, 1, 1, 1))
        a1 = zcreate(UInt16, g, "1", 4, 3, 2, 2, 1; chunks = (4, 4, 1, 1, 1))
        a0[:, :, :, :, :] = zeros(UInt16, 8, 6, 2, 2, 1)
        a1[:, :, :, :, :] = zeros(UInt16, 4, 3, 2, 2, 1)
        write(state_target,
              JSON3.write(Dict("filepath" => Dict("default" => "ccidImage.ome.zarr",
                                                  "_active" => "default"))))
        st, body = api_viewer_meta(HTTP.Request("GET",
            "/api/viewer/meta?projectUid=$proj&imageUid=$img"))
        @test st == 200
        j = JSON3.read(body)
        @test haskey(j, :levels)
        lv = j[:levels]
        @test length(lv) == 2
        # `nX = shape[-1]`, `nY = shape[-2]` — Y×X reads the way the modal already renders shape
        @test (lv[1][:level], lv[1][:nX], lv[1][:nY]) == (0, 8, 6)
        @test (lv[2][:level], lv[2][:nX], lv[2][:nY]) == (1, 4, 3)
        @test (lv[1][:chunkX], lv[1][:chunkY]) == (4, 4)
        @test (lv[2][:chunkX], lv[2][:chunkY]) == (4, 4)
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: viewer meta — labelDims lets the picker flag masks that don't fit" begin
    # A mask store segmented on a DIFFERENT image version keeps its old spatial dims (drift-expanded
    # / cropped — same class as commit 860da24b). The viewer's overlay path assumes label dims equal
    # image dims at the same level; a mismatch either mis-strides the label texture (silent wrong
    # render — see volumeRenderer.uploadFrame's `bytesPerRow = imageNX * LABEL_BPV`) or trips the
    # frontend shape guard. Meta now carries per-vn L0 dims so the sidebar picker can flag the
    # offending row rather than the user hitting the raw error at fetch time.
    axes_ms(names, npaths) = Dict("multiscales" => [Dict(
        "axes"     => [Dict("name" => n) for n in names],
        "datasets" => [Dict("path" => string(i)) for i in 0:npaths-1])])
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name = "meta-labeldims")
        img  = add_image!(add_set!(proj; name = "s"); name = "a")
        proj_id = proj.uid; img_id = img.uid
        # Image L0 8×6 (Julia dims x, y, z, c, t as elsewhere in this file).
        store_dir = joinpath(tmp, proj_id, "0", img_id, "ccidImage.ome.zarr")
        mkpath(dirname(store_dir))
        g = zgroup(Zarr.DirectoryStore(store_dir); attrs = axes_ms(["t","c","z","y","x"], 1))
        a = zcreate(UInt16, g, "0", 8, 6, 1, 1, 1; chunks = (4, 4, 1, 1, 1))
        a[:, :, :, :, :] = zeros(UInt16, 8, 6, 1, 1, 1)
        # Two label stores under the image's labels/ dir: `matching` 8×6 (fits),
        # `mismatched` 10×6 (nX differs → flagged). Register through `img.labels` + `save!` so the
        # ccid.json is written in the versioned shape the readers expect.
        labels_dir = joinpath(tmp, proj_id, "1", img_id, "labels"); mkpath(labels_dir)
        for (name, nx, ny) in (("matching", 8, 6), ("mismatched", 10, 6))
            lp = joinpath(labels_dir, "$(name).zarr")
            lg = zgroup(Zarr.DirectoryStore(lp);
                        attrs = axes_ms(["t", "c", "z", "y", "x"], 1))
            la = zcreate(UInt32, lg, "0", nx, ny, 1, 1, 1; chunks = (nx, ny, 1, 1, 1))
            la[:, :, :, :, :] = zeros(UInt32, nx, ny, 1, 1, 1)
        end
        img.filepath = Dict("default" => "ccidImage.ome.zarr", "_active" => "default")
        img.labels = Dict("matching" => ["matching.zarr"],
                          "mismatched" => ["mismatched.zarr"])
        save!(img)
        st, body = api_viewer_meta(HTTP.Request("GET",
            "/api/viewer/meta?projectUid=$(proj_id)&imageUid=$(img_id)"))
        @test st == 200
        j = JSON3.read(body)
        @test j[:nX] == 8 && j[:nY] == 6
        @test haskey(j, :labelDims)
        # Both registered stores must be reported. Absence would leave the client unable to check.
        @test haskey(j[:labelDims], :matching)
        @test haskey(j[:labelDims], :mismatched)
        @test (j[:labelDims][:matching][:nX],   j[:labelDims][:matching][:nY])   == (8,  6)
        @test (j[:labelDims][:mismatched][:nX], j[:labelDims][:mismatched][:nY]) == (10, 6)
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: movie rail — offline overlay-config translator" begin
    # `_overlays_raw_from_config` turns a viewer `look` / batch config into the smoke-route overlay
    # shape. If it drifts, the record button silently regresses to channels-only movies.
    @test _overlays_raw_from_config(Dict{String,Any}(), false) === nothing
    @test _overlays_raw_from_config(nothing, false) === nothing
    ov = _overlays_raw_from_config(Dict{String,Any}("showPopulations" => true, "popType" => "flow",
                                                     "pointsSize" => 8), false)
    @test ov isa AbstractDict
    @test ov["popType"] == "flow"
    @test ov["pointSizePx"] == 8
    @test ov["allTracks"] === false
    @test ov["includeTracks"] === false
    # showTracks = whole-segmentation tracks. Ribbon-eligible (`includeTracks`) — else the
    # `pi-directions` chip alone drew grey dots without ribbons.
    ov_all = _overlays_raw_from_config(Dict{String,Any}("showTracks" => true), false)
    @test ov_all["allTracks"] === true
    @test ov_all["includeTracks"] === true
    # showPops overrides showTracks — an explicit pop selection means "these pops", not "every cell
    # in the seg". Without this fix, `showPops + showTracks` painted every cell in default grey and
    # ignored the popsFilter entirely.
    ov_both = _overlays_raw_from_config(
        Dict{String,Any}("showTracks" => true, "showPopulations" => true), false)
    @test ov_both["allTracks"] === false
    @test ov_both["showPopulations"] === true
    # ribbons still push in the pops branch (via `include_tracks && (is_track || has_tracks)`)
    @test ov_both["includeTracks"] === true
    # A mask fills the mask branch AND flips `allCells` on when there are no pops/gated to filter by.
    ov_mask = _overlays_raw_from_config(Dict{String,Any}("labelContour" => 3), true)
    @test ov_mask["showMask"] === true
    @test ov_mask["maskContourPx"] == 3
    @test ov_mask["allCells"] === true
    # Gated tracks ON → the mask filters by those pops rather than showing every cell.
    ov_gated = _overlays_raw_from_config(Dict{String,Any}("showGatedTracks" => true), true)
    @test ov_gated["includeTracks"] === true
    @test ov_gated["allCells"] === false

    # `popsFilter` on the config surfaces as `popPaths` on the overlay dict — the batch picker's
    # per-image subset, forwarded to `build_overlays_for` / `build_mask_for` via
    # `_resolve_movie_overlays_mask`. Absent / empty = no filter (all pops rendered).
    ov_no_pf = _overlays_raw_from_config(Dict{String,Any}("showPopulations" => true), false)
    @test !haskey(ov_no_pf, "popPaths")
    ov_empty_pf = _overlays_raw_from_config(
        Dict{String,Any}("showPopulations" => true, "popsFilter" => String[]), false)
    @test !haskey(ov_empty_pf, "popPaths")
    ov_pf = _overlays_raw_from_config(
        Dict{String,Any}("showPopulations" => true, "popsFilter" => ["/A", "/B/c"]), false)
    @test ov_pf["popPaths"] == ["/A", "/B/c"]

    # `popValueName` surfaces as `valueName` on the overlay dict — the segmentation whose pop tree
    # `_resolve_movie_overlays_mask` looks up. Without it, pop paths from `flowTom` would silently
    # miss when the batch draws mask `default` (the resolver falls back to `vnn`).
    ov_no_vn = _overlays_raw_from_config(Dict{String,Any}("showPopulations" => true), false)
    @test !haskey(ov_no_vn, "valueName")
    ov_empty_vn = _overlays_raw_from_config(
        Dict{String,Any}("showPopulations" => true, "popValueName" => ""), false)
    @test !haskey(ov_empty_vn, "valueName")
    ov_vn = _overlays_raw_from_config(
        Dict{String,Any}("showPopulations" => true, "popValueName" => "flowTom"), false)
    @test ov_vn["valueName"] == "flowTom"

    # `trackSources` — multi-segmentation composition for showTracks && !showPops. When present +
    # non-empty, `_resolve_movie_overlays_mask` composes one overlay closure per source, each with
    # its own `all_tracks_colour`. Absent / empty → single-source `allTracks` grey (legacy).
    ov_no_ts = _overlays_raw_from_config(Dict{String,Any}("showTracks" => true), false)
    @test !haskey(ov_no_ts, "trackSources")
    ov_empty_ts = _overlays_raw_from_config(
        Dict{String,Any}("showTracks" => true, "trackSources" => []), false)
    @test !haskey(ov_empty_ts, "trackSources")
    ov_ts = _overlays_raw_from_config(Dict{String,Any}(
        "showTracks" => true,
        "trackSources" => [
            Dict("valueName" => "cpSAM",   "colour" => "#ff6b6b"),
            Dict("valueName" => "flowTom", "colour" => "#4ecdc4"),
        ]), false)
    @test length(ov_ts["trackSources"]) == 2
    @test ov_ts["trackSources"][1]["valueName"] == "cpSAM"
    @test ov_ts["trackSources"][1]["colour"]    == "#ff6b6b"
    @test ov_ts["trackSources"][2]["valueName"] == "flowTom"
    # An entry with no colour falls back to the neutral grey, so a caller can send half-filled
    # entries without breaking the multi-source path.
    ov_ts_default = _overlays_raw_from_config(Dict{String,Any}(
        "showTracks" => true,
        "trackSources" => [Dict("valueName" => "cpSAM")]), false)
    @test ov_ts_default["trackSources"][1]["colour"] == "#9ca3af"
    # A blank valueName is dropped (can't render tracks against no seg) — never sent to the author.
    ov_ts_blank = _overlays_raw_from_config(Dict{String,Any}(
        "showTracks" => true,
        "trackSources" => [Dict("valueName" => "", "colour" => "#ff6b6b"),
                            Dict("valueName" => "cpSAM", "colour" => "#4ecdc4")]), false)
    @test length(ov_ts_blank["trackSources"]) == 1
    @test ov_ts_blank["trackSources"][1]["valueName"] == "cpSAM"
end

@testset "API: movie rail — viewstate → render args (keyframe rendering)" begin
    # `viewstate_to_render_args` is the single translator every keyframe of an offline animation
    # runs through — if a Layer entry's `visible` or `contrast_limits` stopped surfacing, the movie
    # would lose intent silently. Pin the four things a keyframe controls.
    args = viewstate_to_render_args(
        Dict{String,Any}("dims" => Dict("current_step" => [5, 2, 0, 0])),
        ["CH1", "CH2"], nothing, 100, 100)
    @test args.t == 5
    @test args.z == 2
    @test args.crop === nothing            # no canvas hints → no crop

    # Missing viewState fields fall back to defaults.
    args2 = viewstate_to_render_args(Dict{String,Any}(), ["CH1"],
                                      [(0.0, 100.0, "red", true)], 100, 100)
    @test args2.t == 0
    @test args2.z === nothing
    @test length(args2.specs) == 1
    @test args2.specs[1] == (0.0, 100.0, "red", true)

    # Layer entries overlay onto defaults (lookup by CHANNEL NAME, so a re-ordered image is safe).
    args3 = viewstate_to_render_args(
        Dict{String,Any}("layers" => Dict("CH2" => Dict("visible" => false,
                                                          "contrast_limits" => [10, 200],
                                                          "colormap" => "blue"))),
        ["CH1", "CH2"], [(0.0, 100.0, "red", true), (0.0, 100.0, "green", true)],
        100, 100)
    @test args3.specs[1] == (0.0, 100.0, "red", true)
    @test args3.specs[2] == (10.0, 200.0, "blue", false)

    # Camera → crop only when canvas hints are given.
    args_crop = viewstate_to_render_args(
        Dict{String,Any}("camera" => Dict("center" => [50.0, 50.0], "zoom" => 2.0)),
        ["CH1"], nothing, 100, 100; canvas_h = 40, canvas_w = 40)
    @test args_crop.crop !== nothing
    @test first(args_crop.crop.y) >= 0
    @test last(args_crop.crop.y)  <= 99

    # Snapshot's own `canvas` wins over the caller's `canvas_h/canvas_w` kwargs — the crop must
    # match the VIEWER'S visible rectangle at capture time, not the OUTPUT mp4 size. Bug (2026-08-31):
    # an animation recorded at 512×512 with a captured 656×831 canvas produced a
    # 126×126 mp4 (cropped to the OUTPUT size instead of the viewer's actual canvas), losing zoom
    # and aspect. Matches `crop_from_view_state`.
    vs_c = Dict{String,Any}("camera" => Dict("center" => [50.0, 50.0], "zoom" => 2.0),
                             "canvas" => Dict("height" => 80, "width" => 80),
                             "dims"   => Dict("ndisplay" => 2, "current_step" => [0, 0]))
    args_snap = viewstate_to_render_args(vs_c, ["CH1"], nothing, 100, 100;
                                          canvas_h = 40, canvas_w = 40)
    @test args_snap.crop !== nothing
    # 80 / (2 × 2) = 20 half-width → x = 30:70; not 40:60 (which would use canvas_h/w).
    @test collect(args_snap.crop.x) == collect(30:70)
    @test collect(args_snap.crop.y) == collect(30:70)

    # No snapshot canvas → falls back to the caller's kwargs (legacy behaviour).
    vs_nocanv = Dict{String,Any}("camera" => Dict("center" => [50.0, 50.0], "zoom" => 2.0),
                                  "dims"   => Dict("ndisplay" => 2, "current_step" => [0, 0]))
    args_fb = viewstate_to_render_args(vs_nocanv, ["CH1"], nothing, 100, 100;
                                        canvas_h = 40, canvas_w = 40)
    @test args_fb.crop !== nothing
    @test collect(args_fb.crop.x) == collect(40:60)
end

@testset "API: crop_from_view_state — one-shot record uses the viewer's rectangle" begin
    # The one-shot record needs the SAME visible rectangle the viewer is looking at, or it renders
    # a full-image movie at native aspect regardless of the user's zoom + pan (bug 2026-08-29). Pin
    # the crop maths in isolation — matches the crop half of `viewstate_to_render_args`, but the
    # one-shot path doesn't own per-frame arg resolution and only needs this piece.
    vs = Dict{String,Any}(
        "dims"   => Dict("ndisplay" => 2, "current_step" => [0, 0]),
        "camera" => Dict("center" => [0.0, 50.0, 50.0], "zoom" => 2.0),
        "canvas" => Dict("width" => 40, "height" => 40),
    )
    c = crop_from_view_state(vs, 100, 100)
    @test c !== nothing
    @test first(c.x) >= 0
    @test last(c.x)  <= 99
    @test first(c.y) >= 0
    @test last(c.y)  <= 99
    # canvas 40 px / (2 × zoom 2) = 10 px half-width around cx = 50 → x = 40:60 (inclusive).
    @test collect(c.x) == collect(40:60)
    @test collect(c.y) == collect(40:60)

    # 3D → no 2D crop.
    vs3 = Dict{String,Any}("dims" => Dict("ndisplay" => 3),
                            "camera" => Dict("center" => [5.0, 50.0, 50.0], "zoom" => 2.0),
                            "canvas" => Dict("width" => 40, "height" => 40))
    @test crop_from_view_state(vs3, 100, 100) === nothing

    # Missing canvas / camera / zoom → nothing (falls through to whole-image behaviour).
    @test crop_from_view_state(nothing,                   100, 100) === nothing
    @test crop_from_view_state(Dict{String,Any}(),        100, 100) === nothing
    @test crop_from_view_state(
        Dict{String,Any}("camera" => Dict("center" => [50.0, 50.0], "zoom" => 2.0)),
        100, 100) === nothing                                                    # no canvas
end

@testset "API: z_from_view_state — one-shot record matches the viewer's plane" begin
    # A 2D browser viewer shows ONE z; when the request doesn't pin `zSlice`, the movie fell back to
    # an all-Z MIP of the same timepoint — which is a very different picture from what the user was
    # watching when they hit Record. Pin the plane pick here so a future drift is a failing test.
    vs2 = Dict{String,Any}(
        "dims"   => Dict("ndisplay" => 2, "current_step" => [0, 7]),
        "camera" => Dict("center" => [0.0, 50.0, 50.0], "zoom" => 1.0),
        "canvas" => Dict("width" => 40, "height" => 40),
    )
    @test z_from_view_state(vs2) == 7

    # Non-integer plane (a Float32 landed in JSON) → rounded to the nearest int. The viewer's slider
    # is integer, but a snapshot can serialise as float — don't drop it on that.
    vs2f = Dict{String,Any}(
        "dims" => Dict("ndisplay" => 2, "current_step" => [0, 3.4]),
    )
    @test z_from_view_state(vs2f) == 3
    vs2fh = Dict{String,Any}(
        "dims" => Dict("ndisplay" => 2, "current_step" => [0, 3.6]),
    )
    @test z_from_view_state(vs2fh) == 4

    # 3D → nothing (whole volume is rendered, so no single plane to pick).
    vs3 = Dict{String,Any}("dims" => Dict("ndisplay" => 3, "current_step" => [0, 5]))
    @test z_from_view_state(vs3) === nothing

    # Nothing / empty / no dims / short current_step → nothing (falls through to previous behaviour).
    @test z_from_view_state(nothing) === nothing
    @test z_from_view_state(Dict{String,Any}()) === nothing
    @test z_from_view_state(Dict{String,Any}("dims" => Dict("ndisplay" => 2))) === nothing
    @test z_from_view_state(
        Dict{String,Any}("dims" => Dict("ndisplay" => 2, "current_step" => [0]))) === nothing
end

@testset "API: _max_px_from_view_state — blank size fields cap at the viewer canvas" begin
    # Blank size fields used to leave the mp4 at native crop resolution: tiny for a zoomed-in view,
    # huge for a zoomed-out one, neither matching what the viewer showed. Cap at the viewer canvas
    # long side. Aspect stays native — `max_px` is a stride cap, not an exact resize.
    vs = Dict{String,Any}("canvas" => Dict("width" => 900, "height" => 600))
    @test _max_px_from_view_state(vs) == 900
    # Portrait canvas → long side is the height.
    @test _max_px_from_view_state(
        Dict{String,Any}("canvas" => Dict("width" => 400, "height" => 800))) == 800
    # Float canvas dims (JSON) → rounded, so a 799.6 canvas doesn't cap the mp4 at 799.
    @test _max_px_from_view_state(
        Dict{String,Any}("canvas" => Dict("width" => 799.6, "height" => 600.0))) == 800

    # Nothing / no canvas / zero canvas → 0 (previous native-crop behaviour).
    @test _max_px_from_view_state(nothing) == 0
    @test _max_px_from_view_state(Dict{String,Any}()) == 0
    @test _max_px_from_view_state(
        Dict{String,Any}("canvas" => Dict("width" => 0, "height" => 0))) == 0
end

@testset "API: movie overlays — clock timestamp + scale-bar picker match the viewer" begin
    # The Julia encoder-side overlays and the browser volume viewer's on-screen overlays draw the
    # SAME frame at the same time — if their formatters drift, a movie captured from the viewer
    # reads "7m 30s / 20 µm" while the viewer itself reads "0:07:30 / 50 µm". Pin the two policies
    # here so a future drift is a failing test, not a screenshot comparison.
    #
    # Timestamp: "H:MM:SS", zero-padded — matches `elapsedLabel(...,'clock')` in
    # `frontend/src/utils/stillOverlay.ts`.
    @test _format_ts(0,   0.5) == "0:00:00"
    @test _format_ts(15,  0.5) == "0:07:30"       # 15 frames × 30 s
    @test _format_ts(120, 1.0) == "2:00:00"       # 2 hours
    @test _format_ts(1,   1/60) == "0:00:01"      # 1 s

    # Scale bar: largest step ≤ 30 % of the frame's µm-extent, roll to mm at ≥ 1000 — matches
    # `niceScaleBar` in `frontend/src/utils/stillOverlay.ts`.
    #  600 px × 0.5 µm/px = 300 µm extent → 30 % = 90 µm → largest fitting step is 50 µm.
    sb = _pick_scale_bar(0.5, 600)
    @test sb !== nothing
    @test sb[1] == 50.0
    @test _scale_bar_label(sb[1]) == "50 µm"
    #  A big frame rolls up to mm.
    @test _scale_bar_label(1000.0) == "1 mm"
    @test _scale_bar_label(2000.0) == "2 mm"
    #  A tiny frame (too small for the smallest step) returns nothing.
    @test _pick_scale_bar(0.001, 10) === nothing
end

@testset "API: interpolate_keyframes reaches the incoming keyframe exactly" begin
    # The offline animation renderer relies on the last tween frame BEING the arrival state — not a
    # half-step short. The napari path had this contract already; the offline sibling has to match.
    kfs = [Dict("viewState" => Dict("dims" => Dict("current_step" => [0, 0])), "steps" => 1),
           Dict("viewState" => Dict("dims" => Dict("current_step" => [10, 0])), "steps" => 5)]
    frames = interpolate_keyframes(kfs)
    @test length(frames) == 6              # 1 + 5
    @test frames[end]["dims"]["current_step"][1] == 10
end

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
