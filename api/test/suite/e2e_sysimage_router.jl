# End-to-end producers, sysimage stamp + HTTP router testsets — extracted from api/test/runtests.jl.
#
# Five testsets covering server plumbing (real WS producers, sysimage-recipe stamp,
# exported-symbol contract, HTTP router route table):
#  - `API: real producers reach the WS sinks` (E2E: run_chain fires → API sinks).
#  - `API: Cecelia functions called unqualified from api/src are exported` (namespace
#    hygiene: qualified fn call or exported one).
#  - `sysimage stamp records which recipe built the image`.
#  - `the sysimage stamp format has exactly one implementation`.
#  - `HTTP router — the full route table still dispatches` (the router literal table).
#
# Six path expressions rewritten to use API_TEST_DIR. Extracted so runtests.jl contains
# only include lines + section-header comments — same shape as app/test/suite/*.jl.

# ── END-TO-END: a real producer reaches the API layer's sinks ─────────────────
# THE SEAM BOTH SUITES USED TO STUB, from both directions. `app/test` runs a real `run_chain` and asserts
# the fired events carry a real `task_id` (producer → event bus) but has no API layer attached; the
# testsets above exercise the real bridge, `ws_status` and console with HAND-BUILT payloads (sink side)
# but nothing ever ran. So every claim of the form "when a real task finishes, the API layer does Y" was
# unasserted — and that is exactly where a regression hid: moving the outcome bank to `ws_status` silently
# un-banked every chain node, because a chain run passes no `on_status_change` and so never reaches
# `ws_status` at all. Both suites stayed green. These tests run the REAL producers with nothing mocked.
#
# Cheap because `server.jl` gates only `start()` on `CECELIA_NO_SERVE` — the `subscribe_chain_events!`
# handlers ran at include time, so the bridge is live in this process.
@testset "API: real producers reach the WS sinks" begin
    # capture client: broadcast_ws enqueues a serialised frame per registered client (as above)
    cap = Channel{String}(512)
    key = gensym("test-e2e")
    lock(_ws_clients_lock) do; _ws_clients[key] = cap; end
    frames() = (fs = []; while isready(cap); push!(fs, JSON3.read(take!(cap))); end; fs)
    banked(id) = filter(r -> r.id == id, recent_tasks())
    empty!(Cecelia._OUTCOMES)

    try
        proj = create_project!(name="api-e2e")
        s    = add_set!(proj; name="set-A")
        imgs = [add_image!(s; name="img-$i") for i in 1:2]
        for img in imgs; img.status = IMAGE_DONE; save!(img); end

        # ── 1. chain node: producer → event bus → bridge → bank + broadcast ──
        # The regression this pins. A chain node's outcome travels ONLY as chain:node:done, so the bank
        # has to be fed from the bridge; keyed by the node's real scheduler task id, which is what a
        # client correlates its (synthetically-keyed) chain row against.
        save_chain_template!(proj, ChainTemplate(
            "e2e-chain",
            [ChainNode(id="n1", fn="testTasks.image_task",
                       params=Dict{String,Any}("message"=>"e2e"))],
            ChainEdge[]))
        frames()
        run = run_chain(proj, [i.uid for i in imgs]; chain="e2e-chain", on_log=_->nothing)

        node_ids = [run.image_states[i.uid]["n1"].task_id for i in imgs]
        @test all(!isnothing, node_ids) && all(!isempty, node_ids)
        for tid in node_ids
            @test only(banked(tid)).status   == "done"      # ← was empty: nothing fed the bank
            @test only(banked(tid)).fun_name == "testTasks.image_task"
        end
        let fs = frames()
            @test count(f -> String(f.type) == "chain:node:done", fs) == 2   # …and still broadcast
            # every terminal frame the client saw is replayable from the log — the whole invariant
            for f in fs
                String(f.type) == "chain:node:done" || continue
                @test !isempty(banked(String(f.taskId)))
            end
        end
        # a chain node emits NO task:status (handle_chain_run passes no on_status_change) — the premise
        # the bridge-side bank exists for. If this ever fails, the second bank may be redundant.
        @test !any(f -> String(f.type) == "task:status", frames())

        # ── 2. module task: handle_task_run → run_task → ws_status → bank ──
        empty!(Cecelia._OUTCOMES); frames()
        tid = "e2e-image-task"
        handle_task_run(nothing, JSON3.read(JSON3.write((;
            taskId=tid, funName="testTasks.image_task", params=Dict("message"=>"e2e"),
            imageUid=imgs[1].uid, projectUid=proj.uid, setUid=s.uid, poolName=""))))
        @test timedwait(() -> !isempty(banked(tid)), 30.0) === :ok
        @test only(banked(tid)).status    == "done"
        @test only(banked(tid)).image_uid == imgs[1].uid
        let fs = filter(f -> String(f.type) == "task:status", frames())
            @test any(f -> String(f.status) == "done", fs)          # the live frame went out too
            @test String(last(fs).taskId) == tid
        end

        # ── 3. set-scope task: the real frame's image_uids, unfabricated ──
        # Every other test hands `image_uids` in by hand, so nothing checked that a real set run actually
        # reports all its members — and a replayed frame missing them refreshes the representative
        # image's plots only, leaving every other member's stale.
        empty!(Cecelia._OUTCOMES); frames()
        stid = "e2e-set-task"
        handle_task_run(nothing, JSON3.read(JSON3.write((;
            taskId=stid, funName="testTasks.set_task", params=Dict("message"=>"e2e-set"),
            imageUid="", imageUids=[i.uid for i in imgs], projectUid=proj.uid,
            setUid=s.uid, poolName=""))))
        @test timedwait(() -> !isempty(banked(stid)), 30.0) === :ok
        @test only(banked(stid)).status == "done"
        @test Set(only(banked(stid)).image_uids) == Set(i.uid for i in imgs)   # ← all members, banked
        @test only(banked(stid)).image_uid == first(imgs).uid                   # …plus the representative
        let f = last(filter(x -> String(x.type) == "task:status", frames()))
            @test Set(String.(f.imageUids)) == Set(i.uid for i in imgs)         # …and on the live frame
        end

        rm(proj.root; recursive=true)
    finally
        lock(_ws_clients_lock) do; delete!(_ws_clients, key); end
        close(cap)
        empty!(Cecelia._OUTCOMES)
    end
end

# api/src runs in `Main` with `using Cecelia`, so it can only call Cecelia functions UNQUALIFIED if
# they are EXPORTED. Miss an export and the code loads fine, the route registers fine, and the call
# dies at runtime with `UndefVarError: f not defined in Main` — which is exactly how the live-preview
# `refresh_labels!` shipped broken: defined next to `show_labels!`, but not added to the export list
# beside it. Nothing about that is specific to napari, so this checks the whole class rather than a
# hand-kept list: every function Cecelia OWNS and api/src calls unqualified must be exported.
@testset "API: Cecelia functions called unqualified from api/src are exported" begin
    src_dir = joinpath(API_TEST_DIR, "..", "src")
    # comments stripped so a mention in prose can't trip the scan (crude, but `#` inside a string
    # would only ever cause a FALSE ALARM here, never a miss)
    sources = join([replace(read(joinpath(src_dir, f), String), r"#[^\n]*" => "")
                    for f in readdir(src_dir) if endswith(f, ".jl")], "\n")

    missing_exports = Symbol[]
    for sym in names(Cecelia; all = true)
        s = string(sym)
        (occursin("#", s) || startswith(s, "@")) && continue
        isdefined(Cecelia, sym) || continue
        f = getfield(Cecelia, sym)
        f isa Function || continue
        # only names Cecelia itself defines — `parentmodule` keeps Base/stdlib re-exports (length,
        # get, …) out, which would otherwise all look unexported
        parentmodule(f) === Cecelia || continue
        Base.isexported(Cecelia, sym) && continue
        # called unqualified: not preceded by a dot (`Cecelia.f(`, `obj.f(`) or another word char
        occursin(Regex("(?<![.\\w])" * escape_string(s) * "\\s*\\("), sources) || continue
        # …and not a same-named function the api layer defines for itself
        occursin(Regex("function\\s+" * escape_string(s) * "\\s*\\("), sources) && continue
        push!(missing_exports, sym)
    end

    @test isempty(missing_exports)
    isempty(missing_exports) ||
        @info "not exported from Cecelia but called unqualified in api/src" missing_exports
end

# ── Notebook sysimage: which recipe built the image ───────────────────────────────
# Deliberately at the END of this file rather than beside the `API: notebooks sysimage status`
# testset it belongs with: #437 inserts ~104 lines at line 486, exactly there, and a flat testset
# suite does not care about order. Move it back up once that has landed.
@testset "sysimage stamp records which recipe built the image" begin
    # Both builders write the SAME pluto/deps.so by design, but the deps-only one EXCLUDES Cecelia so
    # Revise can hot-reload it while -full bakes Cecelia in. Without a variant they are identical on
    # disk, so a -full build on a dev machine silently froze Cecelia in notebook workers while
    # launch.jl still logged "deps sysimage". These are the pure halves of that fix.
    # No include needed: notebooks_api.jl (loaded via server.jl above) now includes the shared
    # pluto/sysimage_stamp.jl itself, so these functions are already in scope. That IS the fix —
    # if this testset ever needs its own include again, the duplication has come back.

    d = mktempdir()
    write(joinpath(d, "Manifest.toml"), "dummy")
    touch(joinpath(d, "deps.so"))

    write_sysimage_stamp(d, "deps")
    @test sysimage_variant(d) == "deps"
    @test sysimage_fresh(d)
    write_sysimage_stamp(d, "full")
    @test sysimage_variant(d) == "full"
    @test sysimage_fresh(d)                        # variant must not affect staleness
    write_sysimage_stamp(d)                        # default is the safe one
    @test sysimage_variant(d) == "deps"
    @test_throws ArgumentError write_sysimage_stamp(d, "bogus")

    # An image stamped BEFORE `variant` existed must keep working — reported honestly as unknown
    # rather than mislabelled "deps", and crucially still FRESH (no forced ~10 min rebuild).
    write(joinpath(d, "deps.so.stamp"),
          "{\"julia\":\"$(VERSION)\",\"manifest\":\"$(string(hash("dummy")))\"}")
    @test sysimage_variant(d) == "unknown"
    @test sysimage_fresh(d)

    rm(joinpath(d, "deps.so.stamp"))
    @test sysimage_variant(d) == "unknown"
    @test !sysimage_fresh(d)

    # Writer and readers are now ONE implementation (pluto/sysimage_stamp.jl, included by the API
    # server rather than copied). These pin the round trip end to end: what the builder writes is
    # what the classifier and the rebuild path read back.
    full = "{\"julia\":\"1.11\",\"manifest\":\"abc\",\"variant\":\"full\"}"
    @test _classify_sysimage(true, full, false, false, "1.11", "abc") == "ready"
    @test _classify_sysimage(true, full, false, false, "1.10", "abc") == "stale"

    # The API reads the variant to rebuild LIKE FOR LIKE. Getting "unknown" wrong in the unreadable /
    # pre-variant / absent cases is what would silently downgrade a release's full image to deps.
    @test stamp_variant(full) == "full"
    @test stamp_variant("{\"julia\":\"1.11\",\"manifest\":\"abc\",\"variant\":\"deps\"}") == "deps"
    @test stamp_variant("{\"julia\":\"1.11\",\"manifest\":\"abc\"}") == "unknown"   # pre-variant stamp
    @test stamp_variant("{\"variant\":\"bogus\"}") == "unknown"                     # unrecognised
    @test stamp_variant("not json at all")         == "unknown"
    @test stamp_variant(nothing)                   == "unknown"                     # absent → first run

    # Round trip through the real writer: what a build stamps is what the readers report.
    for v in ("deps", "full")
        d2 = mktempdir(); write(joinpath(d2, "Manifest.toml"), "x"); touch(_sysimage_file(d2))
        write_sysimage_stamp(d2, v)
        @test stamp_variant(read_sysimage_stamp(d2)) == sysimage_variant(d2) == v
    end
end

@testset "the sysimage stamp format has exactly one implementation" begin
    # REPLACES a "both readers agree" assertion that went vacuous the moment they became the same
    # function. The live risk is no longer drift between two copies — it is someone re-deriving the
    # format a third time, which is exactly how the copy this consolidation deleted came to exist
    # (with a comment noting the two were "kept trivially in sync"). So detect that instead.
    canonical = normpath(joinpath(API_TEST_DIR, "..", "..", "pluto", "sysimage_stamp.jl"))
    @test isfile(canonical)

    roots = [normpath(joinpath(API_TEST_DIR, "..", "src")),
             normpath(joinpath(API_TEST_DIR, "..", "..", "app", "src")),
             normpath(joinpath(API_TEST_DIR, "..", "..", "pluto"))]

    # Knowledge belonging to the canonical file alone: the artefact filenames, the stamp's JSON field
    # spellings, and the Manifest fingerprint. Anything else deriving these is a second source of
    # truth, whether or not it happens to agree today.
    banned = [("image/stamp filename", r"\"deps\.so"),
              ("stamp field literal",  r"\\\"(julia|manifest|variant)\\\""),
              ("manifest fingerprint", r"hash\(read\(.*Manifest\.toml")]

    offenders, scanned = String[], 0
    for root in roots, (dir, _, files) in walkdir(root), f in files
        endswith(f, ".jl") || continue
        path = joinpath(dir, f)
        normpath(path) == canonical && continue
        scanned += 1
        for (i, line) in enumerate(eachline(path))
            startswith(strip(line), "#") && continue        # prose may name them freely
            for (what, re) in banned
                occursin(re, line) && push!(offenders, "$(basename(path))#$i — $what: $(strip(line))")
            end
        end
    end

    @test isempty(offenders)
    isempty(offenders) || @info "sysimage stamp format re-derived outside pluto/sysimage_stamp.jl — use its helpers (_sysimage_file / _sysimage_stamp / _manifest_fingerprint / stamp_matches / stamp_variant)" offenders

    # Anti-vacuity: a walk over nothing reports a clean bill of health, so pin that we really looked.
    @test scanned > 100
end

# ── HTTP router: the full route table dispatches ─────────────────────────────────
# SAFETY NET for turning the router from a 156-branch if/elseif chain into lookup tables. That chain
# was ONE method costing 42s of a 53s server boot (--trace-compile-timing); a table compiles only the
# handler you actually hit. Nothing tested ROUTING before this — the suite calls handlers directly —
# so the refactor could have dropped a route into a 404 that only shows up in the browser.
#
#  * DISPATCH — every (method, path) must reach a handler. `handle_http` is a plain function, so no
#    socket is needed. A router miss is 404 with body "Not found: <path>"; a handler's own 404 says
#    something else, so the two are distinguishable. Reaching a handler and throwing still counts —
#    the point is that routing happened.
#  * INVENTORY — the `/api/...` literals in server.jl must equal this table exactly, so adding or
#    removing a route without updating it fails here rather than in production.
@testset "HTTP router — the full route table still dispatches" begin
    GET_ROUTES = [
        "/api/analysis/behaviour", "/api/analysis/boards",
        "/api/analysis/chains",
        "/api/analysis/clusters", "/api/analysis/lineage",
        "/api/analysis/measures", "/api/analysis/populations",
        "/api/analysis/spatial", "/api/app/worktrees",
        "/api/chains", "/api/chains/get",
        "/api/chains/run", "/api/chains/runs",
        "/api/correction-plan/get",
        "/api/correction-plan/presets",
        "/api/crop/frame", "/api/crop/info",
        "/api/viewer/marks",   # bidir point-out list (PR #4); the two POSTs at /api/viewer/marks/{tracks,cells} are below
        "/api/viewer/meta",
        "/api/viewer/overlays",
        "/api/viewer/props",   # GET; the POST at the same path is the autosave, listed below
        "/api/viewer/captures",   # bidir share-in list; POST /api/viewer/capture at the same singular path
        "/api/viewer/capture",    # bidir share-in read one; POST at same path writes (below)
        "/api/labels/ids",        # bidir follow-up: enumerate cell/track ids for mark_cells / mark_tracks
        "/api/diagnostics", "/api/diagnostics/packages",
        "/api/fs/list", "/api/gating/channels",
        "/api/gating/density", "/api/gating/membership",
        "/api/gating/plotdata", "/api/gating/plotmeta",
        "/api/gating/popmap", "/api/gating/stats",
        "/api/health", "/api/images",
        "/api/images/geometry", "/api/images/meta",
        "/api/images/stores",
        "/api/images/tasklog", "/api/lablog",
        "/api/logs/recent", "/api/maintenance/patches",
        "/api/mcp/connections",
        "/api/movies", "/api/movies/meta",
        "/api/notebooks",
        "/api/notebooks/content", "/api/notebooks/snapshots",
        "/api/blackboard", "/api/blackboard/entry",
        "/api/notebooks/status", "/api/observer/briefing",
        "/api/observer/labarchives",
        "/api/objects/find",
        "/api/optical-flow/models",
        "/api/denoise/models",
        "/api/observer/status", "/api/plots/attrs",
        "/api/plots/definitions", "/api/plots/populations",
        "/api/plots/umap", "/api/pools", "/api/tasks/threads", "/api/runner/status",
        "/api/preview/status", "/api/projects",
        "/api/projects/boards",   # GET; the POST at the same path is the autosave, listed below
        "/api/projects/bundle-info", "/api/projects/bundles",
        "/api/qc/cohort", "/api/qc/cohort/runs",
        "/api/repl/api", "/api/setup/defaults",
        "/api/setup/validate", "/api/config/tls",
        "/api/storage/compressor", "/api/storage/layout", "/api/storage/keep-previous-version",
        "/api/storage/summary",
        "/api/versions",   # VN P3 chain-designer picker — union of vN across images
        "/api/versions/inventory",   # VN P5 prune surface — per-project inner-version listing

        "/api/profiles",
        "/api/tasks", "/api/tasks/custom-modules",
        "/api/tasks/definitions", "/api/tasks/funparams", "/api/tasks/funparams/sources",
        "/api/tasks/history", "/api/tasks/recent",
        "/api/tracking/motion-dims", "/api/tracking/issues", "/api/tracking/paths",
        "/api/tracking/diagnostics", "/api/tracking/selection", "/api/tracking/detections",
        "/api/system/envs",
        "/api/update/check",
        "/api/version",
        "/api/push/target",   # bidir push (PR #1048) — GET reads the pairing record (never returns token)
        "/api/viewer/landscape",   # bidir landscape read (GET); POST at same path is the publish handler
    ]
    POST_ROUTES = [
        "/api/app/restart", "/api/app/shutdown",
        "/api/app/switch-worktree", "/api/board-assets/copy",
        "/api/board-assets/delete", "/api/board-assets/save",
        "/api/boards/add",   # create-only board authoring (MCP write 6/6); NOT /api/projects/boards
        "/api/cell_cards",   # docs/todo/CELL_CARDS_PLAN.md — snapshot cards on the offline renderer
        "/api/motif_cards",  # docs/todo/BEHAVIOUR_CARDS_PLAN.md Phase 2 — motif-class snapshot cards
        "/api/chains/create", "/api/chains/delete",
        "/api/chains/rename", "/api/chains/save",
        "/api/correction-plan/mount",
        "/api/correction-plan/recommend",
        "/api/correction-plan/save",
        "/api/gating/copy", "/api/gating/pop/add",
        "/api/gating/pop/delete", "/api/gating/pop/move", "/api/gating/pop/rename",
        "/api/gating/pop/set-gate", "/api/gating/pop/update",
        "/api/gating/redo", "/api/gating/undo",
        "/api/images/attr/create", "/api/images/attr/delete",
        "/api/images/analysis/reset", "/api/images/attr/set",
        "/api/images/channelnames",
        "/api/images/delete", "/api/images/inclusion/set",
        "/api/images/labels/delete", "/api/images/labels/rename",
        "/api/images/meta/resync",
        "/api/images/meta/set", "/api/images/move",
        "/api/images/register", "/api/images/value-name-check",
        "/api/images/version/remove",
        "/api/import/peek-pyramid",
        "/api/import/register-legacy", "/api/import/scan-legacy",
        "/api/import/series/probe",
        "/api/lablog/append", "/api/lablog/capture",
        "/api/lablog/dismiss",
        "/api/movies/delete", "/api/movies/meta",
        "/api/notebooks/build-sysimage",
        "/api/blackboard/create", "/api/blackboard/revise", "/api/blackboard/restore",
        "/api/blackboard/prune", "/api/blackboard/delete",
        "/api/blackboard/status",   # PROJECT_MEMORY_PLAN P1 — status flip (open/resolved/parked)
        "/api/blackboard/outcome",  # PROJECT_MEMORY_PLAN P4 — outcome tag (good/bad with required note)
        "/api/blackboard/search",   # PROJECT_MEMORY_PLAN P2 — substring search over titles+bodies
        "/api/notebooks/create", "/api/notebooks/delete",
        "/api/notebooks/describe", "/api/notebooks/duplicate",
        "/api/notebooks/launch", "/api/notebooks/prune",
        "/api/notebooks/restart", "/api/notebooks/restore",
        "/api/notebooks/revise", "/api/notebooks/shutdown",
        "/api/notebooks/snapshot", "/api/notebooks/write",
        "/api/viewer/marks/tracks", "/api/viewer/marks/cells",   # bidir point-out write (PR #4)
        "/api/viewer/marks/ui", "/api/viewer/marks/freeform",    # bidir point-out UI + freeform (PR #5)
        "/api/viewer/marks/tile",    # bidir landscape tile mark (PR #6, Decision 14 reframe)
        "/api/viewer/landscape",     # bidir landscape publish (POST); GET at same path is the read handler
        "/api/viewer/landscape/compute",   # bidir landscape complementary compute — per-channel per-tile stats
        "/api/viewer/capture",   # bidir share-in write (POST); GET at same path is the read handler

        "/api/optical-flow/delete", "/api/optical-flow/inspect",
        "/api/optical-flow/rename",
        "/api/denoise/delete", "/api/denoise/rename",
        "/api/observer/clear", "/api/observer/feedback",
        "/api/observer/labarchives/set",
        "/api/observer/register", "/api/plot_data",
        "/api/pools/set", "/api/tasks/threads/set", "/api/preview/run", "/api/runner/restart", "/api/runner/enabled",
        "/api/preview/start", "/api/preview/stop",
        "/api/projects/animations", "/api/projects/boards",
        "/api/projects/canvases", "/api/projects/create",
        "/api/projects/delete", "/api/projects/list",
        "/api/projects/load", "/api/projects/rename",
        "/api/qc/cohort/check", "/api/repl",
        "/api/repl/config", "/api/sets/create",
        "/api/sets/rename", "/api/sets/delete", "/api/setup/init",
        "/api/config/tls/set",
        "/api/storage/compressor/set", "/api/storage/layout/set",
        "/api/storage/keep-previous-version/set", "/api/storage/reclaim",
        "/api/versions/prune",   # VN P5 prune surface — dry-run first, then destructive
        "/api/profiles/save", "/api/profiles/delete",
        "/api/tasks/custom-modules/reload", "/api/tasks/validate",
        "/api/plugins/install", "/api/plugins/install-local", "/api/plugins/remove",
        "/api/system/envs/install",
        "/api/update/apply",
        "/api/update/revert",
        "/api/viewer/props",   # POST; the GET at the same path is the load, listed above
        "/api/viewer/pick-cell",
        "/api/viewer/pick-rect",
        "/api/viewer/pick-clear",
        "/api/viewer/pick-set",
        "/api/viewer/overlay-legend",
        "/api/viewer/record-test",
        "/api/viewer/thumbnail",
        "/api/push/target",   # bidir push (PR #1048) — POST writes/refreshes the per-project pairing record
        "/api/push/target/clear",   # Kiwi PR #3 — manual unpair; deletes the pairing record
        "/api/push/target/probe",   # Kiwi post-PR #3 — connect-only liveness probe; clears if dead
        "/api/viewer/capture/delete",   # Kiwi post-PR #3 — user-driven single-capture delete
        "/api/viewer/captures/clear",   # Kiwi post-PR #3 — user-driven bulk capture clear
    ]
    UNSAFE = [
        "/api/app/restart", "/api/app/shutdown",
        "/api/import/register-legacy", "/api/import/scan-legacy",
        "/api/import/series/probe",
        "/api/notebooks/build-sysimage", "/api/notebooks/launch",
        "/api/notebooks/restart", "/api/notebooks/shutdown",
        "/api/preview/run", "/api/preview/start",
        "/api/preview/stop", "/api/storage/reclaim",
        "/api/update/apply", "/api/update/revert",
    ]
    # counts pinned below: 67 GET, 92 POST, 17 not live-called

    # Served in handle_stream BEFORE handle_http (binary/Range responses), not part of the tables.
    STREAM_ROUTES = ["/api/board-assets", "/api/movies/file", "/api/viewer/slab"]

    # Would genuinely restart/shut down/spawn a worker if called with an empty body. Their PRESENCE is
    # still pinned by the inventory half; only the live call is skipped.
    unsafe = Set(UNSAFE)

    function dispatched(method, path)
        try
            st, body = handle_http(HTTP.Request(method, path), UInt8[])
            !(st == 404 && occursin("Not found: $path", String(body)))
        catch
            true    # reached a handler and it threw — routing still happened
        end
    end

    missed, checked = String[], 0
    for (m, routes) in (("GET", GET_ROUTES), ("POST", POST_ROUTES)), p in routes
        p in unsafe && continue
        checked += 1
        dispatched(m, p) || push!(missed, "$m $p")
    end
    @test isempty(missed)
    isempty(missed) || @info "routes that no longer dispatch" missed

    # Anti-vacuity: a loop over nothing passes trivially.
    @test checked >= 130
    @test length(GET_ROUTES) == 101 && length(POST_ROUTES) == 139

    # A path nobody registered must still 404, else "dispatched" means nothing.
    @test !dispatched("GET",  "/api/definitely-not-a-route")
    @test !dispatched("POST", "/api/definitely-not-a-route")
    # …and an unrouted METHOD falls through to 405, so method association is real.
    @test first(handle_http(HTTP.Request("DELETE", "/api/health"), UInt8[])) == 405

    # INVENTORY — every /api literal in server.jl is accounted for, and vice versa. Structure-agnostic
    # on purpose: it keeps working whatever shape the router takes next.
    src = read(joinpath(API_TEST_DIR, "..", "src", "server.jl"), String)
    literals = Set(strip(m.match, '"') for m in eachmatch(r"\"/api/[^\"]+\"", src))
    @test literals == Set(vcat(GET_ROUTES, POST_ROUTES, STREAM_ROUTES))
end
