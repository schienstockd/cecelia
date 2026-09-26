# ── Model: create project and image ───────────────────────────────────────
@testset "Model round-trip" begin
    proj = create_project!(name="smoke-test-$(rand(1000:9999))")
    @test isdir(proj.root)
    @test isfile(joinpath(proj.root, "project.json"))

    s = add_set!(proj; name="set-A")
    @test isdir(s._dir)

    img = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path" => "/tmp/fake.tif"))
    @test isdir(img._dir)
    @test isfile(joinpath(img._dir, "ccid.json"))

    # Round-trip via init_object
    loaded = init_object(proj.uid, img.uid)
    @test loaded isa CciaImage
    @test loaded.name == "img-1"
    @test get(loaded.meta, "ori_path", "") == "/tmp/fake.tif"

    # image_by_uid(...; uid) convenience accessor (REPL/notebook lookup)
    @test image_by_uid(proj; uid = img.uid) === img
    @test image_by_uid(s; uid = img.uid) === img
    @test image_by_uid(proj; uid = "nope") === nothing
    @test image_by_uid(s; uid = "nope") === nothing

    # Cleanup
    rm(proj.root; recursive=true)
end

# ── Starred: a plain per-image bookmark ───────────────────────────────────────────────────────────
# ANY number of images can be starred, and nothing downstream reads it — it drives the Starred row
# filter and nothing else. It replaced a SET-level single "reference image" nomination, from which
# the 8-bit import used to derive one intensity window for the whole set; that coupling is gone, so
# the load-bearing property here is just that the flag round-trips and is independent per image.
@testset "starred images" begin
    proj = create_project!(name = "star-$(rand(1000:9999))")
    s    = add_set!(proj; name = "set")
    img1 = add_image!(s; name = "a")
    img2 = add_image!(s; name = "b")

    @test img1.starred == false                                 # not starred is the default
    @test img2.starred == false

    img1.starred = true
    img2.starred = true                                         # multi-select: not one per set
    save!(img1); save!(img2)

    rs = load_project(proj.uid)._sets[1]
    @test all(i -> i.starred, rs._images)                       # persisted through ccid.json

    img1.starred = false
    save!(img1)
    reloaded = load_project(proj.uid)._sets[1]._images
    @test count(i -> i.starred, reloaded) == 1                  # independent per image

    rm(proj.root; recursive=true)
end
@testset "add_image! carries attr for derived images" begin
    # editImages tasks (copy/crop/z-proj/t-proj/bin/resample) mint a new image via `add_image!`,
    # and the user's cohort/condition/mouse tags — `img.attr` — must follow that derivation.
    # Silent DROP made a copy invisible to attr_value_counts / compare-by-attribute and forced the
    # user to re-tag every crop; the fix is one kwarg into add_image!.
    proj = create_project!(name = "attr-carry-$(rand(1000:9999))")
    s    = add_set!(proj; name = "set")
    src_attr = Dict{String,String}("cohort" => "WT", "mouse" => "m3")
    derived  = add_image!(s; name = "cropped", attr = src_attr)
    @test derived.attr == src_attr
    # copy defensively — mutating the passed dict later must not leak into the new image's attr
    src_attr["cohort"] = "MUT"
    @test derived.attr["cohort"] == "WT"
    # round-trips through save! / load_project
    reloaded = load_project(proj.uid)._sets[1]._images[1]
    @test reloaded.attr == Dict{String,String}("cohort" => "WT", "mouse" => "m3")

    # Default is empty (the fresh-import path — routes.jl — does not pass `attr`)
    fresh = add_image!(s; name = "import")
    @test fresh.attr == Dict{String,String}()
    rm(proj.root; recursive = true)
end
@testset "move_image! (manifest-only, no data moved)" begin
    proj = create_project!(name="move-test-$(rand(1000:9999))")
    a = add_set!(proj; name="set-A")
    b = add_set!(proj; name="set-B")
    img1 = add_image!(a; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))
    img2 = add_image!(a; name="img-2", meta=Dict{String,Any}("ori_path"=>"/tmp/b.tif"))
    data_dir = joinpath(proj.root, "0", img1.uid)   # image data + metadata dirs are UID-keyed
    meta_dir = joinpath(proj.root, "1", img1.uid)

    move_image!(proj, img1.uid, a.uid, b.uid)

    # membership moved …
    @test a.image_uids == [img2.uid]
    @test b.image_uids == [img1.uid]
    @test image_by_uid(a; uid=img1.uid) === nothing
    @test image_by_uid(b; uid=img1.uid) !== nothing
    # … but NO data moved on disk (dirs are UID-keyed, independent of the set)
    @test isdir(data_dir) && isdir(meta_dir)
    @test !isdir(joinpath(b._dir, img1.uid))   # sets never nest image dirs

    # persists: reload the project fresh and the manifests reflect the move
    reloaded = load_project(proj.uid)
    ra = reloaded._sets[findfirst(s -> s.uid == a.uid, reloaded._sets)]
    rb = reloaded._sets[findfirst(s -> s.uid == b.uid, reloaded._sets)]
    @test ra.image_uids == [img2.uid]
    @test rb.image_uids == [img1.uid]
    @test image_by_uid(reloaded; uid=img1.uid) !== nothing   # still findable project-wide

    # idempotent / no-op guards
    @test move_image!(proj, img1.uid, b.uid, b.uid) === proj        # same set → no-op
    move_image!(proj, img1.uid, a.uid, b.uid)                        # already in dest → no-op
    @test b.image_uids == [img1.uid]                                 # not duplicated
    # error cases
    @test_throws ErrorException move_image!(proj, "nope", a.uid, b.uid)   # image not in source
    @test_throws ErrorException move_image!(proj, img2.uid, a.uid, "gone") # dest missing

    rm(proj.root; recursive=true)
end

# ── REPL / notebook data-access surface (Observer Phase 2 foundation) ─────────
@testset "REPL API surface + generated doc" begin
    # every allow-listed accessor is defined, exported, and documented — the notebook-facing
    # surface must be complete (a rename/removal or a missing docstring fails here).
    ref = repl_api_reference()
    @test !isempty(ref)
    for e in ref
        @test isdefined(Cecelia, Symbol(e.name))
        @test e.exported
        @test e.documented          # has a real docstring (undocumented accessors are a bug)
        @test !occursin("value*name", e.doc)   # raw docstring, not the mangled re-render
    end

    # GOLDEN: docs/REPL.md's generated section is in sync with the live docstrings. If this fails,
    # someone changed a listed function's docstring (or the list) without regenerating — run
    # `Cecelia.write_repl_doc()`. This is the drift-guard that keeps REPL.md honest.
    p = Cecelia.repl_doc_path()
    if isfile(p)
        committed = read(p, String)
        # Compare line-ending agnostically: this is a CONTENT drift-guard, not a byte-exactness
        # check. On Windows git checks the file out with CRLF while `repl_api_section()` emits LF,
        # so the splice mismatched on every line and the test failed for a reason that has nothing
        # to do with docstring drift.
        _lf(s) = replace(s, "\r\n" => "\n")
        @test _lf(committed) == _lf(Cecelia.render_repl_doc(committed))
        @test occursin(Cecelia.REPL_DOC_BEGIN, committed)
    else
        @test_skip "docs/REPL.md not found at $p"
    end
end

# ── Run log (automatic per-image provenance) ─────────────────────────────────
@testset "Run log" begin
    proj = create_project!(name="runlog-test-$(rand(1000:9999))")
    s = add_set!(proj; name="set-A")
    img = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path" => "/tmp/fake.tif"))

    @test read_run_log(img) == Any[]                       # empty before any run
    append_run_log!(img, "segment.cellpose", "default")
    append_run_log!(img, "behaviour.hmm")
    log = read_run_log(img)
    @test length(log) == 2
    @test log[1]["fun"] == "segment.cellpose"
    @test log[1]["valueName"] == "default"
    @test haskey(log[1], "at") && !isempty(log[1]["at"])
    @test log[2]["fun"] == "behaviour.hmm"

    # persists across reload (init_object)
    loaded = init_object(proj.uid, img.uid)
    @test length(read_run_log(loaded)) == 2

    # capped to the most recent RUN_LOG_CAP entries
    for i in 1:(Cecelia.RUN_LOG_CAP + 10); append_run_log!(img, "x.$i"); end
    capped = read_run_log(img)
    @test length(capped) == Cecelia.RUN_LOG_CAP
    @test capped[end]["fun"] == "x.$(Cecelia.RUN_LOG_CAP + 10)"   # newest kept

    # params trail: entry carries the sanitised task params; internal `_…` keys and the redundant
    # `valueName` are dropped, real tuning knobs kept (Observer Phase 2 §1 — see docs/ai-assist/OBSERVER.md).
    img2 = add_image!(s; name="img-2", meta=Dict{String,Any}("ori_path" => "/tmp/fake2.tif"))
    append_run_log!(img2, "tracking.bayesian_tracking", "default", "done",
                    Dict{String,Any}("search_radius" => 5.0, "max_lost" => 3,
                                     "valueName" => "default", "_task_id" => "abc123"))
    e = read_run_log(img2)[end]
    @test e["params"]["search_radius"] == 5.0
    @test e["params"]["max_lost"] == 3
    @test !haskey(e["params"], "valueName")    # redundant with its own field
    @test !haskey(e["params"], "_task_id")      # internal, dropped
    # default (no params) → shape-stable empty dict, and it survives reload
    append_run_log!(img2, "behaviour.hmm")
    @test read_run_log(img2)[end]["params"] == Dict{String,Any}()
    @test read_run_log(init_object(proj.uid, img2.uid))[end-1]["params"]["search_radius"] == 5.0
    # sanitiser handles nothing/empty directly
    @test Cecelia._run_log_params(nothing) == Dict{String,Any}()
    @test Cecelia._run_log_params(Dict("_x" => 1, "keep" => 2)) == Dict{String,Any}("keep" => 2)

    rm(proj.root; recursive=true)
end

# ── Run log: open → close, and reaping runs whose process died ───────────────
# The log is written TWICE per run (open at :running, close at the terminal status) for one reason:
# an append-on-finish log cannot record a run that never reaches its finish. A killed runner takes its
# in-flight tasks with it, so 22 minutes of segmentation left NOTHING on disk — no entry, no outcome.
# See run_log.jl's header.
@testset "Run log open/close and reap" begin
    proj = create_project!(name="runlog-reap-$(rand(1000:9999))")
    s    = add_set!(proj; name="set-A")
    img  = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path" => "/tmp/fake.tif"))

    # open writes a non-terminal entry immediately — before any work happens
    open_run_log!(img, "segment.cellposeMeasure", "afCorrected",
                  Dict{String,Any}("cellDiameter" => 10, "_task_id" => "zzz"); task_id = "T1")
    e = read_run_log(img)[end]
    @test e["status"] == Cecelia.RUN_LOG_RUNNING
    @test e["taskId"] == "T1"
    @test e["params"]["cellDiameter"] == 10        # params are on the OPEN entry, not held until close
    @test !haskey(e["params"], "_task_id")

    # close patches that entry in place — it does not append a second one
    close_run_log!(img, "T1", "done")
    log = read_run_log(img)
    @test length(log) == 1
    @test log[end]["status"] == "done"
    @test log[end]["params"]["cellDiameter"] == 10   # params survive the patch
    @test !isempty(log[end]["finishedAt"])

    # :cancelled is RECORDED, not skipped. This is the regression that made a killed segmentation
    # untraceable: it used to be dropped as "the user aborted, not an outcome worth logging".
    open_run_log!(img, "segment.cellposeMeasure", "afCorrected"; task_id = "T2")
    close_run_log!(img, "T2", "cancelled")
    @test read_run_log(img)[end]["status"] == "cancelled"
    @test length(read_run_log(img)) == 2

    # closing an id that was never opened still records the outcome rather than dropping it
    close_run_log!(img, "T404", "failed"; fun_name = "tracking.bayesian_tracking")
    @test read_run_log(img)[end]["status"] == "failed"
    @test read_run_log(img)[end]["fun"] == "tracking.bayesian_tracking"

    # ── the reap ────────────────────────────────────────────────────────────
    # T3 is still executing somewhere, T4's process died. Only T4 is interrupted — reaping a task the
    # detached runner is still running would report LIVE work as lost, which is the whole risk here.
    open_run_log!(img, "segment.cellposeMeasure", "Neutrophil"; task_id = "T3")
    open_run_log!(img, "segment.cellposeMeasure", "Tcell";      task_id = "T4")
    @test reap_run_log!(img, ["T3"]) == 1
    byid = Dict(e["taskId"] => e for e in read_run_log(img) if !isempty(get(e, "taskId", "")))
    @test byid["T3"]["status"] == Cecelia.RUN_LOG_RUNNING          # untouched — still live
    @test byid["T4"]["status"] == Cecelia.RUN_LOG_INTERRUPTED
    @test byid["T4"]["valueName"] == "Tcell"                       # the reap preserves what it recorded

    # reaping is idempotent, and a closed entry is never re-opened by it
    @test reap_run_log!(img, ["T3"]) == 0
    @test reap_run_log!(img, String[]) == 1                        # now T3 too — nothing is live
    @test read_run_log(img)[1]["status"] == "done"                 # T1 stays done

    # survives reload — this is a durable record, which is the entire point
    @test read_run_log(init_object(proj.uid, img.uid))[end]["status"] == Cecelia.RUN_LOG_INTERRUPTED

    rm(proj.root; recursive=true)
end

# ── Session briefing + all_qc_docs (Observer Phase 2 §2) ─────────────────────
@testset "Session briefing + all_qc_docs" begin
    proj = create_project!(name="brief-$(rand(1000:9999))")
    s = add_set!(proj; name="set-A")
    img1 = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))
    img2 = add_image!(s; name="img-2", meta=Dict{String,Any}("ori_path"=>"/tmp/b.tif"))
    # suppress the calibration fallback (these fixtures have no PhysicalSize) by persisting an
    # empty omezarr QC doc — so the only flag is the one we add explicitly. Also tests "persisted wins".
    for im in (img1, img2)
        write_qc(im, "importImages.omezarr", "default", Dict{String,Any}[])
    end
    write_qc(img1, "tracking.bayesian_tracking", "default",
             [qc_finding("warn", "few_tracks", "Few tracks", "Only 5 tracks")])
    append_lab_log!(proj, "User", ["started tracking run"])

    b = session_briefing(proj)
    @test b.projectUid == proj.uid && b.projectName == proj.name && b.imageCount == 2
    uids = [f.uid for f in b.flagged]
    @test img1.uid in uids && !(img2.uid in uids)     # only the warn image flags; clean stays clean
    f1 = b.flagged[findfirst(f -> f.uid == img1.uid, b.flagged)]
    @test f1.worst == "warn" && f1.findings[1].short == "Few tracks"
    # WHICH task banked it. Without the fun, a probe's hardcoded threshold is indistinguishable from a
    # real pipeline finding — that cost a session chasing "0 cells" no segmentation had produced.
    @test f1.findings[1].fun == "tracking.bayesian_tracking"
    # an image the user has NOT excluded says so positively (absent would read as "unknown")
    @test f1.included == true && b.excludedCount == 0

    # EXCLUDED images: still listed (a warn on one is information, and dropping it would make the count
    # disagree with the image table) but LABELLED, so a session leads with the ones that still count.
    # The first real session opened on a drift anomaly for an image its owner had dropped weeks earlier.
    img1.included = false
    save!(img1)
    b2 = session_briefing(proj)
    @test b2.imageCount == 2 && b2.excludedCount == 1      # cohort size is imageCount - excludedCount
    f1b = b2.flagged[findfirst(f -> f.uid == img1.uid, b2.flagged)]
    @test f1b.included == false && f1b.findings[1].short == "Few tracks"   # labelled, not hidden
    img1.included = true
    save!(img1)
    @test length(b.recentLabLog) == 1 && b.recentLabLog[1].author == "User"
    @test occursin("tracking", b.recentLabLog[1].summary)

    # all_qc_docs: a fresh image (no persisted omezarr) gets the computed calibration fallback
    img3 = add_image!(s; name="img-3", meta=Dict{String,Any}("ori_path"=>"/tmp/c.tif"))
    @test haskey(all_qc_docs(img3), "importImages.omezarr/default")
    @test haskey(all_qc_docs(img1), "importImages.omezarr/default")   # persisted present too

    rm(proj.root; recursive=true)
end

# ── Lockfile (naive guard) ──────────────────────────────────────────────────
@testset "with_transaction" begin
    proj     = create_project!(name="lock-test-$(rand(1000:9999))")
    # the lockfile is DERIVED from the object's state file (the R `getStateFile` + ".lock" shape),
    # not a hardcoded name — that's what makes the per-image form below possible at all
    lockfile = Cecelia.state_file(proj) * ".lock"
    @test lockfile == Cecelia._lock_path(proj)

    # happy path: returns the body value and releases the lock
    @test (with_transaction(proj) do; 42; end) == 42
    @test !isfile(lockfile)

    # lock is released even when the body throws (no leaked lock)
    @test_throws ErrorException with_transaction(proj) do; error("boom"); end
    @test !isfile(lockfile)

    # works for an IMAGE too, locking that image alone — different images never block each other
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="i")
    img2 = add_image!(s; name="i2")
    @test Cecelia._lock_path(img) == Cecelia.state_file(img) * ".lock"
    @test Cecelia._lock_path(img) != Cecelia._lock_path(proj)
    @test Cecelia._lock_path(img) != Cecelia._lock_path(img2)
    @test (with_transaction(img) do; 7; end) == 7
    @test !isfile(Cecelia._lock_path(img))

    # REENTRANT: a commit reached from inside another transaction on the SAME object must not
    # deadlock. Before the in-process lock this sat on its own lockfile until the timeout.
    @test (with_transaction(img) do
               with_transaction(img) do; 9; end
           end) == 9
    @test !isfile(Cecelia._lock_path(img))

    # ANOTHER PROCESS's fresh lockfile is respected — we wait, then fail naming the file
    touch(Cecelia._lock_path(img))
    err = try; with_transaction(img; timeout = 1) do; 1; end; "" catch e; sprint(showerror, e) end
    @test occursin(Cecelia._lock_path(img), err)
    @test occursin("stale lockfile", err)
    rm(Cecelia._lock_path(img); force = true)

    # ...but an ABANDONED one (process died mid-commit) is reclaimed rather than blocking every
    # later task on that image behind a hidden file. Safe only because a transaction now wraps the
    # short commit, never the computation — so nothing legitimate is ever this old.
    @test !Cecelia._lock_abandoned(time())                                   # fresh → keep waiting
    @test  Cecelia._lock_abandoned(time() - Cecelia._LOCK_STALE_AFTER - 1)   # abandoned → reclaim

    rm(proj.root; recursive=true)
end

# ── commit_state!: registering an output is atomic against a concurrent registration ────────
# THE lost-update bug. Every task used to hand-roll re-read → poke → write, so
# two tasks finishing on one image both read the old dict and the second write dropped the first's
# field. `write_json_atomic` alone does NOT fix this — each write is individually intact; the
# second is simply built on stale data.
@testset "commit_state! wraps the read-modify-write in the transaction" begin
    proj = create_project!(name="commit-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="i")

    # basic read-modify-write, persisted
    commit_state!(img) do raw
        raw["status"] = "done"
    end
    @test read_ccid_raw(Cecelia.state_file(img))["status"] == "done"

    # The RMW must happen INSIDE the transaction — that is the whole mechanism, and it is what
    # makes a concurrent registration wait instead of reading stale data. Asserted directly and
    # deterministically: while the body runs, this object's lockfile is held.
    #
    # This replaces a thread-interleaving test that did not work. It spawned two tasks and relied on
    # the scheduler to interleave them so one would read stale `labels`; standalone it did (one
    # registration was lost), but inside the suite the second task simply wasn't scheduled during the
    # first's sleep, so it passed with the lock REMOVED. A concurrency test whose failure depends on
    # scheduler luck is worse than none — it reads as coverage. Mutual exclusion itself is covered
    # deterministically by the `with_transaction` testset above (lockfile held, foreign lock
    # respected, reentrancy, abandoned-lock reclaim).
    held = false
    commit_state!(img) do raw
        held = isfile(Cecelia._lock_path(img))     # false if the RMW isn't wrapped
        raw["labels"] = Dict{String,Any}("segA" => ["segA.zarr"])
    end
    @test held
    @test !isfile(Cecelia._lock_path(img))         # ...and released afterwards
    @test haskey(read_ccid_raw(Cecelia.state_file(img))["labels"], "segA")

    # a nested commit on the same object still completes (reentrancy through commit_state!, not just
    # with_transaction) and both mutations land
    commit_state!(img) do raw
        raw["note"] = "outer"
        commit_state!(img) do inner
            inner["status"] = "inner-done"
        end
    end
    fresh = read_ccid_raw(Cecelia.state_file(img))
    @test fresh["note"] == "outer"

    # the lock is released on a throwing body, and the file keeps its previous content
    @test_throws ErrorException commit_state!(img) do raw
        raw["status"] = "clobbered"
        error("boom")
    end
    @test !isfile(Cecelia._lock_path(img))
    @test read_ccid_raw(Cecelia.state_file(img))["status"] == "done"

    # a metadata-dir form for the API layer, which commits without loading the object
    commit_state!(img._dir) do raw
        raw["note"] = "by dir"
    end
    @test read_ccid_raw(Cecelia.state_file(img))["note"] == "by dir"

    rm(proj.root; recursive=true)
end

# ── Durable state writes are atomic ─────────────────────────────────────────
# Every state file (ccid.json, project.json, sidecars, custom.toml, the lab log) is written
# tmp-then-rename via `write_atomic`. The failure this prevents: `open(path, "w")` truncates
# first, so a kill in that window (the Quit button SIGKILLs) left a half-written file — and since
# `_load_set` has no per-image guard, ONE truncated image ccid.json failed the WHOLE project load.
@testset "durable state writes are atomic" begin
    td = mktempdir()

    # a failed write leaves the PREVIOUS content intact, not a truncated file
    p = joinpath(td, "state.json")
    write_json_atomic(p, Dict("a" => 1))
    before = read(p, String)
    @test_throws ErrorException write_atomic(p) do io
        print(io, "{\"partial\":")
        error("killed mid-write")
    end
    @test read(p, String) == before          # untouched, NOT truncated
    @test JSON3.read(read(p, String))[:a] == 1

    # no temp files left behind, on success or on failure
    @test isempty(filter(f -> occursin(".tmp.", f), readdir(td)))

    # a leftover temp (from a process killed between write and rename) must NOT be picked up by
    # sidecar discovery, which is `readdir` + `endswith(f, ".json")` in several places
    @test !endswith(Cecelia.write_atomic(io -> print(io, "x"), joinpath(td, "probe.json")), ".tmp")
    tmpname = "state.json.tmp.abc123"
    @test !endswith(tmpname, ".json")

    # creates a missing parent dir rather than throwing
    deep = joinpath(td, "a", "b", "c.json")
    write_json_atomic(deep, Dict("ok" => true))
    @test JSON3.read(read(deep, String))[:ok] == true

    # write_atomic handles non-JSON content too (TOML config, the lab-log markdown)
    t = joinpath(td, "notes.md")
    write_atomic(io -> print(io, "# hello"), t)
    @test read(t, String) == "# hello"

    # keys and scalars with `"` inside round-trip. `JSON3.pretty` (the previous serialiser) silently
    # emitted invalid JSON here — see the block comment on `write_json_atomic` for the story. The
    # KIWI_CAPTURE_AND_BLACKBOARD_PLAN sidecar `kiwiRefs` was keyed by a JSON-encoded refKey, so
    # every entry with one on it became unreadable and vanished from the blackboard list.
    tricky = joinpath(td, "tricky.json")
    payload = Dict{String,Any}(
        "kiwiRefs" => Dict{String,Any}("[[\"a\",\"b\"]]" => 1, "plain" => 2),
        "quoted"   => "hello \"world\"",
        "empty"    => Any[],
    )
    write_json_atomic(tricky, payload)
    back = JSON3.read(read(tricky, String), Dict{String,Any})
    @test back["kiwiRefs"]["[[\"a\",\"b\"]]"] == 1
    @test back["kiwiRefs"]["plain"] == 2
    @test back["quoted"] == "hello \"world\""
    @test back["empty"] == Any[]

    rm(td; recursive=true)
end

# ── state_file: one derivation, whatever the caller holds ───────────────────
# The R original kept this private to the object (`getStateFile`); the port had 20+ call sites
# re-deriving `joinpath(obj._dir, "ccid.json")` and the API layer additionally re-spelling the
# `1/` metadata segment. All forms must agree.
# ── resolve_value_name: the `defaultOnly` half of R's cciaImage$valueNames ──────────────────
# Nine call sites hand-rolled `something(value_name, get(img.label_props, "_active", "default"))`,
# hardcoding the `_active` key and the `"default"` fallback that VERSIONED_ACTIVE_KEY /
# VERSIONED_DEFAULT_VAL exist to name. One resolver now, so the fallback rule lives in one place.
@testset "resolve_value_name" begin
    proj = create_project!(name="rvn-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="i")

    # no label_props at all → the versioned default, never an error
    @test resolve_value_name(img) == VERSIONED_DEFAULT_VAL

    # an explicit value_name always wins, even when an active one exists
    img.label_props = Dict{String,String}("A" => "A.h5ad", "B" => "B.h5ad",
                                         VERSIONED_ACTIVE_KEY => "B")
    @test resolve_value_name(img, "A") == "A"
    @test resolve_value_name(img)      == "B"        # else the active one

    # falls back to the versioned default when nothing is marked active
    img.label_props = Dict{String,String}("A" => "A.h5ad")
    @test resolve_value_name(img) == VERSIONED_DEFAULT_VAL

    # agrees with the underlying helper it replaced, and with the list accessor's view
    img.label_props = Dict{String,String}("A" => "A.h5ad", VERSIONED_ACTIVE_KEY => "A")
    @test resolve_value_name(img) == versioned_active(img.label_props)
    @test resolve_value_name(img) in img_value_names(img)

    rm(proj.root; recursive=true)
end

# Migration QC: only the silent-failure case is a finding — an image that migrates with no cell
# table looks successful and leaves every downstream page empty.
@testset "migrate_qc_findings" begin
    @test isempty(migrate_qc_findings(["A"]))
    @test isempty(migrate_qc_findings(["A", "B"]))
    f = migrate_qc_findings(String[])
    @test length(f) == 1
    @test f[1]["level"] == "warn"
    @test f[1]["code"]  == "migrate.no_segmentation"
    @test occursin("re-run", lowercase(f[1]["long"]))   # the long text says what to DO
end

@testset "state_file resolution" begin
    proj = create_project!(name="statefile-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="i")

    @test Cecelia.state_file(proj) == joinpath(proj.root, "project.json")
    @test basename(Cecelia.state_file(img)) == Cecelia.STATE_FILENAME
    @test basename(Cecelia.state_file(s))   == Cecelia.STATE_FILENAME

    # object form == metadata-dir form == (project dir + uid) form
    @test Cecelia.state_file(img) == Cecelia.state_file(img._dir)
    @test Cecelia.state_file(img) == Cecelia.state_file(proj.root, img.uid)
    @test Cecelia.state_file(s)   == Cecelia.state_file(proj.root, s.uid)
    @test Cecelia.obj_meta_dir(proj.root, img.uid) == img._dir

    # and the file the accessor names is the one save! actually wrote
    @test isfile(Cecelia.state_file(img))

    rm(proj.root; recursive=true)
end

# An unreadable state file must name the FILE. JSON3 alone says only "invalid JSON at byte
# position N" — raised from inside a project load, that told the user nothing actionable.
@testset "unreadable state file names the file" begin
    proj = create_project!(name="corrupt-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="i")
    path = Cecelia.state_file(img)

    full = read(path, String)
    write(path, full[1:cld(length(full), 2)])          # truncate, as an interrupted write would
    err = try; load_project(proj.uid); "" catch e; sprint(showerror, e) end
    @test occursin(path, err)                          # says WHICH file
    @test occursin("not valid JSON", err)
    @test occursin(".ccbundle", err)                    # and what to do about it

    rm(proj.root; recursive=true)
end

# Detector, not advisory: a NEW bare `open(<state file>, "w")` fails here. This is how the
# truncating form spread to ~30 sites in the first place — the atomic pattern existed (the gating
# sidecar) but nothing stopped the next writer hand-rolling the unsafe one.
@testset "no hand-rolled state writes" begin
    # Anchor via `pathof(Cecelia)` (= app/src/Cecelia.jl) rather than @__DIR__. `@__DIR__` here is
    # `app/test/suite/`; the old paths (`../src` and `../../api/src`) would resolve to `app/test/src`
    # and `app/api/src` — neither exists — and the scanner would silently pass with zero files read.
    _repo = dirname(dirname(dirname(pathof(Cecelia))))
    roots = [joinpath(_repo, "app", "src"), joinpath(_repo, "api", "src")]
    # Every write-mode `open` is an offender unless listed here WITH a reason. Deliberately an
    # allow-list of exact call sites, not of whole files: exempting a file would let the next
    # state write in that file slip through, which is precisely how this spread. Keyed by full
    # repo-relative path (not basename) — a second file named e.g. `utils.jl` in another dir
    # would otherwise silently inherit the exemption.
    # Meta-ratchet: growing the allow-list requires bumping `allowed_max` in the same PR, so a
    # reviewer sees "weaken the check" attempts. See docs/todo/DRIFT_PREVENTION_ASSESSMENT.md.
    allowed_max = 5
    allowed = Dict(
        # the atomic writer itself — this IS the tmp-then-rename implementation
        joinpath("app", "src", "utils.jl")      => [raw"""open(tmp, "w") do io"""],
        # transient per-run params blob handed to a Python subprocess, in the run's task dir
        joinpath("app", "src", "py_runner.jl")  => [raw"""open(params_file, "w") do io"""],
        # bundle manifest, written INTO the export staging dir that is then tarred and deleted
        joinpath("app", "src", "project_io.jl") => [raw"""open(joinpath(tmp, BUNDLE_MANIFEST), "w") do io"""],
        # bulk image-data copy (multi-GB, chunked); not state, and the import task owns cleanup
        # (moved from `omezarr.jl` when it was split into `omezarr/`; `staging.jl` is the new home)
        joinpath("app", "src", "tasks", "importImages", "omezarr", "staging.jl")
                                                => [raw"""open(dst, "w") do d"""],
        # raw RGB24 frames streamed to the run's task dir and handed straight to the encoder, then
        # deleted (the offline renderer, docs/todo/WEB_VIEWER_PLAN.md P5). Multi-GB and transient: staging a
        # copy to rename would double the disk for a file nothing ever reads back.
        joinpath("api", "src", "movie_render.jl") => [raw"""open(raw, "w") do io"""],
    )
    offenders = String[]
    for root in roots, (dir, _, files) in walkdir(root), f in files
        endswith(f, ".jl") || continue
        rel = relpath(joinpath(dir, f), _repo)
        ok = get(allowed, rel, String[])
        for (i, line) in enumerate(eachline(joinpath(dir, f)))
            occursin(r"""open\([^)]*,\s*"w"\)""", line) || continue
            startswith(strip(line), "#") && continue
            any(a -> occursin(a, line), ok) && continue
            push!(offenders, "$rel:$i: $(strip(line))")
        end
    end
    if !isempty(offenders)
        @warn "Hand-rolled write-mode open — use write_atomic/write_json_atomic, or add an " *
              "allow-list entry with a reason if it genuinely isn't durable state" offenders
    end
    @test isempty(offenders)

    # Meta-ratchet: allow-list size hasn't grown.
    if length(allowed) > allowed_max
        @error "no hand-rolled state writes: allow-list grew to $(length(allowed)) " *
               "(cap $allowed_max). Either fix the new write to use write_atomic, or bump " *
               "`allowed_max` and justify in the PR body. See docs/todo/DRIFT_PREVENTION_ASSESSMENT.md."
    end
    @test length(allowed) <= allowed_max
end

