# Napari branch-labels + task console snapshot/project/log-frames/chain-node testsets —
# extracted from api/test/runtests.jl.
#
# Five testsets covering the napari-branch-labels handler + the first half of the task
# console (task_console.jl) reconciliation surface:
#  - `API: napari branch-labels payload` (allBranchLabels dict → ({vn}) Branches layer;
#    BRANCHING_PLAN Decision 6).
#  - `API: task console reconciles snapshot removals` (the stale-"running" regression).
#  - `API: task console picks up the project uid`.
#  - `API: task console ignores post-mortem log frames` (finished-task no-resurrection).
#  - `API: task console attributes chain-node outcomes`.
#
# One path expression rewritten to use API_TEST_DIR. Extracted so runtests.jl contains
# only include lines + section-header comments — same shape as app/test/suite/*.jl.

# ── Napari: branch-labels payload parsing ─────────────────────────────────────
# The napari open + show-labels handlers accept an `allBranchLabels` dict in parallel to `allLabels`
# so skeleton labels from segment.branching are shown as a distinct layer type (`({vn}) Branches`),
# without leaking into the generic labels picker (BRANCHING_PLAN Decision 6). The full round-trip
# needs a live napari process, but the request parsing is pure and worth pinning: missing key →
# empty dict (legacy image / no branching run), well-formed dict → the same shape as _parse_all_labels.
@testset "API: napari branch-labels payload" begin
    # missing → empty (legacy image / no branching run)
    empty_data = JSON3.read(JSON3.write(Dict{String,Any}()))
    @test _parse_all_branch_labels(empty_data) == Dict{String,Vector{String}}()

    # well-formed
    data = JSON3.read(JSON3.write(Dict("allBranchLabels" =>
        Dict("default" => ["default.zarr"], "shg" => ["shg.zarr"]))))
    parsed = _parse_all_branch_labels(data)
    @test parsed["default"] == ["default.zarr"]
    @test parsed["shg"]     == ["shg.zarr"]

    # a scalar (non-array) filename is coerced to a single-element list — same
    # forgiving contract as _parse_all_labels
    scalar_data = JSON3.read(JSON3.write(Dict("allBranchLabels" => Dict("default" => "default.zarr"))))
    @test _parse_all_branch_labels(scalar_data)["default"] == ["default.zarr"]

    # non-dict payload → empty
    bad = JSON3.read(JSON3.write(Dict("allBranchLabels" => "nope")))
    @test _parse_all_branch_labels(bad) == Dict{String,Vector{String}}()
end

# ── Task console: snapshot reconciliation (the stale-"running"-row regression) ──
# `api/task_console.jl` is run by path (`pixi run console`), never imported, so this is the only
# automated coverage it can have: its entrypoint is guarded by `PROGRAM_FILE`, and the reconciliation
# half is split out as the socket-free `_reconcile_snapshot!(rows)` we drive with synthetic snapshots.
# Wrapped in a module because the script defines top-level consts (TASKS, LOCK, TALLY, …).
#
# The bug this pins: the console only ever ADDED rows from GET /api/tasks, and dropped one solely on a
# terminal task:status frame — which is lossy by design (per-client drop-on-full queue in server.jl,
# and nothing at all on a half-open socket). One missed frame stranded the row as "running" forever:
# six tasks listed as running while every pool read idle and the scheduler held none.
module TaskConsoleUT
    # `module ... end` here creates a fresh scope where `API_TEST_DIR` (defined in Main
    # by runtests.jl) is not visible — reach through Main explicitly.
    include(joinpath(Main.API_TEST_DIR, "..", "task_console.jl"))
end

@testset "API: task console reconciles snapshot removals" begin
    C = TaskConsoleUT
    row(id; status="running", fun="segment.branching", pool="cpu", img="EaMaVq") =
        (; id=id, status=status, fun_name=fun, pool_name=pool, image_uid=img, chain_run_id="")
    reset_console!() = (empty!(C.TASKS); empty!(C.SEEN_TERM); empty!(C.EVENTS); empty!(C.ENDED_IDS);
                        for k in keys(C.TALLY); C.TALLY[k] = 0; end)
    # retiring pushes an activity line, which STREAM_MODE prints — keep it out of the test output
    reconcile(rows) = redirect_stdout(devnull) do; C._reconcile_snapshot!(rows) end

    # a scheduler task appears, then vanishes with NO terminal frame ever delivered
    reset_console!()
    reconcile([row("t1")])
    @test haskey(C.TASKS, "t1") && C.TASKS["t1"].status == "running"
    @test C.TASKS["t1"].in_snapshot                       # eligible for retiring
    reconcile([])                            # miss 1 — not yet (poll/registration race)
    @test haskey(C.TASKS, "t1")
    reconcile([])                            # miss 2 — retire
    @test !haskey(C.TASKS, "t1")                          # ← the row used to live here forever
    @test C.TALLY["ended"] == 1                           # counted, and NOT guessed as done/failed
    @test C.TALLY["done"] == 0 && C.TALLY["failed"] == 0

    # a retired task must not be resurrected by a later snapshot (SEEN_TERM)
    reconcile([row("t1")])
    @test !haskey(C.TASKS, "t1") && C.TALLY["ended"] == 1

    # a WS-only producer (job / batch movie) never appears in the snapshot → never retired by it
    reset_console!()
    t = C._task!("job1"); t.fun_name = "project.export"; t.pool_name = "job"; t.status = "running"
    for _ in 1:5
        reconcile([])
    end
    @test haskey(C.TASKS, "job1") && C.TALLY["ended"] == 0

    # a terminal status seen IN the snapshot is counted for real, not as "ended"
    reset_console!()
    reconcile([row("t2")])
    reconcile([row("t2"; status="failed")])
    @test !haskey(C.TASKS, "t2")
    @test C.TALLY["failed"] == 1 && C.TALLY["ended"] == 0

    # a task still listed keeps its row and its miss counter resets (no drift toward retirement)
    reset_console!()
    reconcile([row("t3")])
    reconcile([])                            # one miss
    reconcile([row("t3")])                   # back in the snapshot → counter cleared
    @test C.TASKS["t3"].misses == 0
    reconcile([])
    @test haskey(C.TASKS, "t3")                           # would have been retired if it hadn't reset

    # an UNATTRIBUTED row (no fun, no pool — only ever log/progress frames) is prunable even though
    # the snapshot never listed it: nothing else could ever remove it, so it sat there forever.
    reset_console!()
    C._task!("ghost")                        # blank fun + pool, default status "queued"
    reconcile([])                            # miss 1
    @test haskey(C.TASKS, "ghost")
    reconcile([])                            # miss 2 → dropped
    @test !haskey(C.TASKS, "ghost")
    @test sum(values(C.TALLY)) == 0                       # no outcome claimed for a task we can't name
    @test !("ghost" in C.SEEN_TERM)                       # …and not suppressed, so a real task returns
end

# ── Task console: the PROJECT column ──────────────────────────────────────────
# The console watches the whole server, which serves every project under `projects_dir()` — so a row's
# image uid alone doesn't say which project it belongs to. `project_uid` rides the snapshot
# (`list_tasks()`); a WS-only producer names no project and must render blank rather than inherit one.
@testset "API: task console picks up the project uid" begin
    C = TaskConsoleUT
    recon(rows) = redirect_stdout(devnull) do; C._reconcile_snapshot!(rows) end
    reset_console!() = (empty!(C.TASKS); empty!(C.SEEN_TERM); empty!(C.EVENTS); empty!(C.ENDED_IDS);
                        for k in keys(C.TALLY); C.TALLY[k] = 0; end)

    reset_console!()
    recon([(; id="p1", status="running", fun_name="segment.cellpose", pool_name="gpu",
              image_uid="EaMaVq", project_uid="NRUBxU", chain_run_id="")])
    @test C.TASKS["p1"].project_uid == "NRUBxU"

    # an older server (no project_uid on the snapshot) must not error, just leave it blank
    reset_console!()
    recon([(; id="p2", status="running", fun_name="segment.cellpose", pool_name="gpu",
              image_uid="EaMaVq", chain_run_id="")])
    @test C.TASKS["p2"].project_uid == ""

    # …and neither does a WS-only row invent one
    reset_console!()
    @test C._task!("job1").project_uid == ""
end

# ── Task console: post-mortem log frames must not resurrect a finished task ────
# The zombie-queued-row regression. Cancelling a running task broadcasts the terminal `task:status`
# at once (cancel_task! → on_status_change), then the killed subprocess's reader flushes whatever was
# still in its pipe as `task:log` frames. Those carry no fun / pool / status, so each one minted a
# fresh blank row stuck at the default "queued" — and the snapshot could never retire it, because the
# scheduler had already deregistered the task. Six cancels, six immortal "queued / waiting" rows with
# every pool reading idle and GET /api/tasks returning [].
@testset "API: task console ignores post-mortem log frames" begin
    C = TaskConsoleUT
    row(id; status="running") = (; id=id, status=status, fun_name="spatialAnalysis.aggregatesMeshes",
                                  pool_name="cpu", image_uid="EaMaVq", chain_run_id="")
    feed(frame) = redirect_stdout(devnull) do; C.handle_ws(JSON3.write(frame)) end
    recon(rows)  = redirect_stdout(devnull) do; C._reconcile_snapshot!(rows) end
    reset_console!() = (empty!(C.TASKS); empty!(C.SEEN_TERM); empty!(C.EVENTS); empty!(C.LOGS);
                        empty!(C.ENDED_IDS); for k in keys(C.TALLY); C.TALLY[k] = 0; end)

    reset_console!()
    recon([row("k1")])
    feed((; type="task:status", taskId="k1", status="cancelled", imageUid="EaMaVq",
           fun="spatialAnalysis.aggregatesMeshes"))
    @test !haskey(C.TASKS, "k1") && C.TALLY["cancelled"] == 1

    # the dying subprocess's remaining stdout arrives AFTER the terminal frame
    nlogs = length(C.LOGS)
    feed((; type="task:log", taskId="k1", line=">> t74: 13 meshes, 0 aggregate(s)"))
    feed((; type="task:log", taskId="k1", line="[QC] mesh aggregates: 31, 18% of cells."))
    @test !haskey(C.TASKS, "k1")                          # ← used to reappear as a blank "queued" row
    @test length(C.LOGS) == nlogs + 2                     # still SHOWN, just not resurrected as a row
    @test C.TALLY["cancelled"] == 1                        # and not re-counted

    # a progress frame after the fact is likewise ignored (this half was already guarded — pin it)
    feed((; type="task:progress", taskId="k1", progress=0.9))
    @test !haskey(C.TASKS, "k1")

    # the row STAYS while the task is alive — a log frame for a live task is the normal case
    reset_console!()
    recon([row("k2")])
    feed((; type="task:log", taskId="k2", line=">> t01: 4 meshes"))
    @test haskey(C.TASKS, "k2") && C.TASKS["k2"].last_log == ">> t01: 4 meshes"
end

# ── Task console: a chain node's real outcome ─────────────────────────────────
# A chain run emits NO `task:status` frames (`handle_chain_run` passes no `on_status_change`), so a
# chain node's row can only leave the table via the snapshot-retire path — i.e. always
# "ended / outcome unseen", never done or failed. The `taskId` now carried on every `chain:node:*`
# frame is the correlation handle; these pin that the console uses it, and that a frame WITHOUT one
# (skipped before submission, set-scope node, hand-fired REPL event → JSON `null`) is harmless.
@testset "API: task console attributes chain-node outcomes" begin
    C = TaskConsoleUT
    row(id; status="running") = (; id=id, status=status, fun_name="segment.branching",
                                  pool_name="cpu", image_uid="EaMaVq", chain_run_id="run1")
    feed(frame) = redirect_stdout(devnull) do        # STREAM_MODE prints; keep test output clean
        C.handle_ws(JSON3.write(frame))
    end
    reset_console!() = (empty!(C.TASKS); empty!(C.SEEN_TERM); empty!(C.EVENTS); empty!(C.ENDED_IDS);
                        for k in keys(C.TALLY); C.TALLY[k] = 0; end)
    recon(rows) = redirect_stdout(devnull) do; C._reconcile_snapshot!(rows) end

    # node finishes → counted as DONE (not "ended"), row dropped at once
    reset_console!()
    recon([row("c1")])
    feed((; type="chain:node:done", runId="run1", chainName="ch", projectUid="p",
           imageUid="EaMaVq", nodeId="n1", fn="segment.branching", taskId="c1"))
    @test !haskey(C.TASKS, "c1")
    @test C.TALLY["done"] == 1 && C.TALLY["ended"] == 0
    # …and the snapshot's retire path must not then double-count it as ended
    recon([]); recon([])
    @test C.TALLY["ended"] == 0 && C.TALLY["done"] == 1

    # node:failed carries WHICH terminal it was — cancelled must not be counted as failed
    reset_console!()
    recon([row("c2")])
    feed((; type="chain:node:failed", runId="run1", imageUid="EaMaVq", nodeId="n1",
           fn="segment.branching", status="cancelled", taskId="c2"))
    @test C.TALLY["cancelled"] == 1 && C.TALLY["failed"] == 0
    reset_console!()
    recon([row("c3")])
    feed((; type="chain:node:failed", runId="run1", imageUid="EaMaVq", nodeId="n1",
           fn="segment.branching", status="failed", taskId="c3"))
    @test C.TALLY["failed"] == 1

    # taskId absent / JSON null (skipped node, set-scope node, hand-fired event) → no crash, no tally,
    # and the row is left for the snapshot to retire as before
    reset_console!()
    recon([row("c4")])
    feed((; type="chain:node:done", runId="run1", imageUid="EaMaVq", nodeId="n1", fn="f"))
    feed((; type="chain:node:done", runId="run1", imageUid="EaMaVq", nodeId="n1", fn="f",
           taskId=nothing))
    feed((; type="chain:node:failed", runId="run1", imageUid="EaMaVq", nodeId="n2", fn="f",
           status="skipped", taskId=""))
    @test haskey(C.TASKS, "c4")                       # untouched — nothing to correlate
    @test sum(values(C.TALLY)) == 0
    recon([]); recon([])
    @test !haskey(C.TASKS, "c4") && C.TALLY["ended"] == 1   # falls back to the retire path

    # a LATE terminal frame corrects an `ended` tally rather than leaving a number we know is wrong
    # (chain frame delayed past the 2-poll retire window). It must move the count, not add one.
    reset_console!()
    recon([row("c6")]); recon([]); recon([])
    @test C.TALLY["ended"] == 1
    feed((; type="chain:node:done", runId="run1", imageUid="EaMaVq", nodeId="n1", fn="f", taskId="c6"))
    @test C.TALLY["ended"] == 0 && C.TALLY["done"] == 1        # moved, not added
    # …and a further repeat of that frame changes nothing (no double count)
    feed((; type="chain:node:done", runId="run1", imageUid="EaMaVq", nodeId="n1", fn="f", taskId="c6"))
    @test C.TALLY["done"] == 1 && sum(values(C.TALLY)) == 1
    # a real outcome is NOT correctable by a later, different one — first sighting wins
    reset_console!()
    recon([row("c7")])
    feed((; type="chain:node:done", runId="run1", imageUid="EaMaVq", nodeId="n1", fn="f", taskId="c7"))
    feed((; type="chain:node:failed", runId="run1", imageUid="EaMaVq", nodeId="n1", fn="f",
           status="failed", taskId="c7"))
    @test C.TALLY["done"] == 1 && C.TALLY["failed"] == 0

    # a terminal chain frame for a task the console never saw still counts + blocks resurrection
    reset_console!()
    feed((; type="chain:node:done", runId="run1", imageUid="EaMaVq", nodeId="n1", fn="f", taskId="c5"))
    @test C.TALLY["done"] == 1
    recon([row("c5")])
    @test !haskey(C.TASKS, "c5") && C.TALLY["done"] == 1
end
