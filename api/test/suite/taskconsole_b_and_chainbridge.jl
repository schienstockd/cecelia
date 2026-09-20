# Task console (times + counts) + chain-bridge + ws_status testsets — extracted from
# api/test/runtests.jl.
#
# Six testsets covering the second half of the task-console reconciliation surface plus
# the WS-bridge and terminal-frame guarantees:
#  - `API: task console times each task` (elapsed clock semantics)
#  - `API: task console counts outcomes without the WS frame`
#  - `API: /api/tasks/recent`
#  - `API: ws_status banks every producer outcome`
#  - `API: chain bridge taskId degradation`
#  - `API: chain bridge frames`
#
# The task-console testsets reach through `Main.TaskConsoleUT` — the module defined by
# `suite/napari_and_taskconsole_a.jl` (loaded earlier in runtests.jl). No path expressions
# to rewrite in this extract. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

# ── Task console: the elapsed clock ───────────────────────────────────────────
# Elapsed is measured client-side — nothing on the wire carries a start timestamp (the scheduler keeps
# none, and a record is deregistered the instant it finishes). So the two things worth pinning are the
# formatter and WHEN the clock restarts: only on a real status change, because the snapshot re-asserts
# the same status every 2s and resetting per poll would peg every row at "0s". Plus the honesty marker:
# a run whose start we didn't witness reads `≥` rather than passing a floor off as a measurement.
@testset "API: task console times each task" begin
    C = TaskConsoleUT
    row(id; status="running") = (; id=id, status=status, fun_name="segment.cellpose",
                                  pool_name="gpu", image_uid="EaMaVq", chain_run_id="")
    feed(frame) = redirect_stdout(devnull) do; C.handle_ws(JSON3.write(frame)) end
    recon(rows)  = redirect_stdout(devnull) do; C._reconcile_snapshot!(rows) end
    reset_console!() = (empty!(C.TASKS); empty!(C.SEEN_TERM); empty!(C.EVENTS); empty!(C.ENDED_IDS);
                        for k in keys(C.TALLY); C.TALLY[k] = 0; end)

    # formatter: seconds → minutes → hours, zero-padded so the column doesn't jitter. Same spelling as
    # the GUI's `formatTaskDuration` — a duration must not read two ways depending on where you look.
    @test C.dur_str(0)          == "0s"
    @test C.dur_str(42_400)     == "42s"
    @test C.dur_str(59_400)     == "59s"          # rounds to the second, no early rollover
    @test C.dur_str(60_000)     == "1m 00s"
    @test C.dur_str(252_000)    == "4m 12s"
    @test C.dur_str(3_600_000)  == "1h 00m"
    @test C.dur_str(5_430_000)  == "1h 30m"
    @test C.dur_str(-1)         == "0s"           # clock skew must not print a negative
    # a start we didn't witness is a FLOOR, and says so
    @test C.dur_str(252_000; exact = false) == "≥4m 12s"

    # a witnessed queued → running transition is exact and restarts the clock
    reset_console!()
    recon([row("e1"; status="queued")])
    waited = C.TASKS["e1"].since
    @test !C.TASKS["e1"].exact                    # the snapshot found it already queued
    recon([row("e1"; status="queued")])           # re-asserted, not changed…
    @test C.TASKS["e1"].since == waited           # …so the queue-wait clock keeps running
    feed((; type="task:status", taskId="e1", status="running", fun="segment.cellpose"))
    @test C.TASKS["e1"].status == "running"
    @test C.TASKS["e1"].exact                     # we saw it start
    @test C.TASKS["e1"].since >= waited           # and the clock restarted on the run
    since_run = C.TASKS["e1"].since
    recon([row("e1")])                            # snapshot agrees it is running — no reset
    @test C.TASKS["e1"].since == since_run && C.TASKS["e1"].exact

    # a task ALREADY running when the console connects: clocked from now, marked as a floor
    reset_console!()
    recon([row("e2")])
    @test C.TASKS["e2"].status == "running" && !C.TASKS["e2"].exact
    @test startswith(C.dur_since(C.TASKS["e2"]), "≥")

    # …and one whose first frame is the live `running` transition is exact even with no queued sighting
    reset_console!()
    feed((; type="task:status", taskId="e3", status="running", fun="segment.cellpose", pool="gpu"))
    @test C.TASKS["e3"].exact && !startswith(C.dur_since(C.TASKS["e3"]), "≥")

    # the outcome line reports the run time — the only place a finished task's elapsed can appear,
    # since the row is collapsed to a count. Read before the row is dropped, so it must be non-empty.
    reset_console!()
    recon([row("e4")])
    ran = C._ran_for("e4")
    @test occursin("in ", ran)
    feed((; type="task:status", taskId="e4", status="done", fun="segment.cellpose"))
    @test !haskey(C.TASKS, "e4") && C.TALLY["done"] == 1
    @test occursin("in ", last(C.EVENTS))         # …and it made it onto the announced line
    @test C._ran_for("e4") == ""                  # gone with the row

    # a task cancelled while still QUEUED never ran — no run time is claimed for it
    reset_console!()
    recon([row("e5"; status="queued")])
    @test C._ran_for("e5") == ""

    # ── the server's own timestamps, which is what makes it a measurement rather than an estimate ──
    iso(dt) = Dates.format(dt, C.TS_FORMAT)
    stamped(id; status="running", started="", queued="") =
        (; id=id, status=status, fun_name="segment.cellpose", pool_name="gpu",
           image_uid="EaMaVq", chain_run_id="", started_at=started, queued_at=queued)

    # a task that has been running for 20 minutes, first seen NOW: the console used to be able to say
    # only "≥0s" here — the whole point of `started_at` on the snapshot
    reset_console!()
    began = Dates.now(UTC) - Dates.Minute(20)
    recon([stamped("s1"; started = iso(began))])
    @test C.TASKS["s1"].exact
    @test C.TASKS["s1"].since == began
    @test C.dur_since(C.TASKS["s1"]) == "20m 00s"           # not "≥0s"

    # …re-asserted every poll without drifting or resetting
    recon([stamped("s1"; started = iso(began))])
    @test C.TASKS["s1"].since == began && C.TASKS["s1"].exact

    # a row the console had been timing ITSELF is upgraded the first time the rail supplies a real start
    reset_console!()
    recon([row("s2")])                                       # no timestamps (older server)
    @test !C.TASKS["s2"].exact
    recon([stamped("s2"; started = iso(began))])
    @test C.TASKS["s2"].exact && C.TASKS["s2"].since == began

    # queued rows are timed from `queued_at`, so the wait is real too
    reset_console!()
    enq = Dates.now(UTC) - Dates.Second(90)
    recon([stamped("s3"; status="queued", queued = iso(enq))])
    @test C.TASKS["s3"].exact && C.dur_since(C.TASKS["s3"]) == "1m 30s"

    # a garbage or empty timestamp must not take the reader down — it just means "not known"
    reset_console!()
    recon([stamped("s4"; started = "not a date")])
    @test haskey(C.TASKS, "s4") && !C.TASKS["s4"].exact      # fell back to the local clock

    # a live terminal frame carries both ends → the announced duration is exact
    reset_console!()
    recon([stamped("s5"; started = iso(began))])
    feed((; type="task:status", taskId="s5", status="done", fun="segment.cellpose",
           startedAt=iso(began), finishedAt=iso(began + Dates.Minute(25))))
    @test occursin("in 25m 00s", last(C.EVENTS))
    @test C.TALLY["done"] == 1

    # …and so is a RECOVERED one, for a task this console never even held a row for. Timing it locally
    # would have measured the poll delay, not the task.
    reset_console!()
    redirect_stdout(devnull) do
        C._apply_recent!([(; id="s6", status="done", image_uid="EaMaVq", image_uids=String[],
                            started_at=iso(began), finished_at=iso(began + Dates.Minute(3)))])
    end
    @test !haskey(C.TASKS, "s6") && C.TALLY["done"] == 1
    @test occursin("in 3m 00s", last(C.EVENTS))

    # an outcome row with no start (older server / never ran) still counts, just without a duration
    reset_console!()
    redirect_stdout(devnull) do
        C._apply_recent!([(; id="s7", status="failed", image_uid="", image_uids=String[],
                            started_at="", finished_at=iso(Dates.now(UTC)))])
    end
    @test C.TALLY["failed"] == 1 && !occursin("in ", last(C.EVENTS))
end

# ── Task console: the done counter must not depend on the WS stream ───────────
# The reported bug: nine images ran and finished, and the console read "0 done · 17 ended". The
# terminal `task:status` frame is the ONE frame per task that carries the outcome, and the server
# drops frames for a slow client by design (per-client drop-on-full queue in server.jl) — so every
# lost or late frame became a permanent "finished, outcome unseen".
#
# Two independent halves, one per failure mode:
#   1. a LATE frame (it did arrive, after the snapshot had already retired the row) was DISCARDED —
#      `handle_ws` returned on `id in SEEN_TERM` before reaching the ended→outcome correction, which
#      made that correction unreachable from the task:status path (only chain frames could use it).
#   2. a LOST frame can now be recovered at all: the outcome is polled from GET /api/tasks/recent.
@testset "API: task console counts outcomes without the WS frame" begin
    C = TaskConsoleUT
    row(id; status="running") = (; id=id, status=status, fun_name="cleanupImages.driftCorrect",
                                  pool_name="io", image_uid="EaMaVq", chain_run_id="")
    rec(id, status, ts) = (; id=id, status=status, finished_at=ts,
                             fun_name="cleanupImages.driftCorrect", pool_name="io",
                             image_uid="EaMaVq", image_uids=String[])
    feed(frame)  = redirect_stdout(devnull) do; C.handle_ws(JSON3.write(frame)) end
    recon(rows)  = redirect_stdout(devnull) do; C._reconcile_snapshot!(rows) end
    recent(rows; prime=false) = redirect_stdout(devnull) do
        C._apply_recent!(JSON3.read(JSON3.write(rows)); prime = prime)
    end
    reset_console!() = (empty!(C.TASKS); empty!(C.SEEN_TERM); empty!(C.EVENTS); empty!(C.LOGS);
                        empty!(C.ENDED_IDS); C.RECENT_SINCE[] = "";
                        for k in keys(C.TALLY); C.TALLY[k] = 0; end)

    # ── 1. a late terminal task:status frame corrects the "ended" it was retired as ──
    reset_console!()
    recon([row("t1")]); recon([]); recon([])
    @test C.TALLY["ended"] == 1 && C.TALLY["done"] == 0
    feed((; type="task:status", taskId="t1", status="done", imageUid="EaMaVq",
           fun="cleanupImages.driftCorrect"))
    @test C.TALLY["done"] == 1 && C.TALLY["ended"] == 0     # ← was silently discarded
    @test sum(values(C.TALLY)) == 1                          # moved, not added
    feed((; type="task:status", taskId="t1", status="done", imageUid="EaMaVq"))
    @test C.TALLY["done"] == 1                               # repeat frame changes nothing
    # a real outcome still can't be overwritten by a later, different one
    feed((; type="task:status", taskId="t1", status="failed", imageUid="EaMaVq"))
    @test C.TALLY["done"] == 1 && C.TALLY["failed"] == 0

    # ── 2. the frame never arrives at all → the outcome poll supplies it ──
    reset_console!()
    recon([row("t2")])
    recent([rec("t2", "done", "2026-07-31T04:50:20.100Z")])
    @test !haskey(C.TASKS, "t2") && C.TALLY["done"] == 1     # counted with no WS frame at all
    recon([]); recon([])
    @test C.TALLY["ended"] == 0 && C.TALLY["done"] == 1      # …and not re-retired as unseen

    # …and it corrects a row already retired as "ended" (poll landed after the retire)
    reset_console!()
    recon([row("t3")]); recon([]); recon([])
    @test C.TALLY["ended"] == 1
    recent([rec("t3", "failed", "2026-07-31T04:51:00.000Z")])
    @test C.TALLY["failed"] == 1 && C.TALLY["ended"] == 0

    # a whole batch: every task finishes, not one terminal frame gets through → all counted
    reset_console!()
    ids = ["b$i" for i in 1:9]
    recon([row(i) for i in ids])
    @test length(C.TASKS) == 9
    recent([rec(i, "done", "2026-07-31T04:5$(n):00.000Z") for (n, i) in enumerate(ids)])
    @test C.TALLY["done"] == 9 && C.TALLY["ended"] == 0 && isempty(C.TASKS)

    # ── `since` bookkeeping: newest wins, and a re-served row is not double-counted ──
    reset_console!()
    recent([rec("s1", "done", "2026-07-31T04:00:00.000Z"),
            rec("s2", "done", "2026-07-31T05:00:00.000Z")])
    @test C.RECENT_SINCE[] == "2026-07-31T05:00:00.000Z"
    recent([rec("s2", "done", "2026-07-31T05:00:00.000Z")])   # inclusive bound re-serves it
    @test C.TALLY["done"] == 2

    # ── the prime pass: outcomes that predate this console session are NOT counted ──
    # (the ring holds up to 500; crediting the session with work it never watched would be a lie)
    reset_console!()
    recent([rec("old1", "done", "2026-07-30T01:00:00.000Z"),
            rec("old2", "failed", "2026-07-30T02:00:00.000Z")]; prime = true)
    @test sum(values(C.TALLY)) == 0
    @test C.RECENT_SINCE[] == "2026-07-30T02:00:00.000Z"      # …but we resume from after them
    recent([rec("old2", "failed", "2026-07-30T02:00:00.000Z")])
    @test sum(values(C.TALLY)) == 0                           # primed ids stay uncounted
    recent([rec("new1", "done", "2026-07-30T03:00:00.000Z")])
    @test C.TALLY["done"] == 1                                # anything after it counts normally

    # a task still IN FLIGHT is never touched by the poll (nothing to report yet)
    reset_console!()
    recon([row("live")])
    recent(Any[])
    @test haskey(C.TASKS, "live") && C.TASKS["live"].status == "running"
end

# The route the poll above reads. `since` must reach `recent_tasks` (an unparsed one would re-serve
# the whole ring every 2s), and a missing param must mean "everything", not an error.
@testset "API: /api/tasks/recent" begin
    get_recent(q = "") = JSON3.read(api_tasks_recent(HTTP.Request("GET", "/api/tasks/recent$q"))[2])
    @test api_tasks_recent(HTTP.Request("GET", "/api/tasks/recent"))[1] == 200
    @test get_recent() isa JSON3.Array                       # no `since` → the whole ring
    @test isempty(get_recent("?since=9999-01-01T00:00:00.000Z"))
    @test get_recent("?since=") == get_recent()              # blank is "everything", not a filter
end

# ── Every terminal frame is banked, whoever emitted it ────────────────────────
# `ws_status` is the rail's ONE status sink, so banking the outcome there (rather than in the scheduler,
# where it started) is what makes recovery universal: background jobs (project export/import, data
# patches — `pool="job"`) and batch movies (`pool="viewer"`) never enter the scheduler's registry at all,
# so a dropped `done` frame used to strand their row with nothing able to correct it. These pin that the
# bank is fed by the sink and not by the producer.
@testset "API: ws_status banks every producer's outcome" begin
    empty!(Cecelia._OUTCOMES)
    banked(id) = filter(r -> r.id == id, recent_tasks())

    # a background job — the case that was previously uncoverable
    ws_status(nothing, "job-1", "done", "EaMaVq"; fun="project:export", pool="job")
    @test only(banked("job-1")).status    == "done"
    @test only(banked("job-1")).pool_name == "job"
    @test only(banked("job-1")).fun_name  == "project:export"

    # a batch movie (napari/viewer producer)
    ws_status(nothing, "movie-1", "failed", "EaMaVq"; fun="movie:batch", pool="viewer")
    @test only(banked("movie-1")).status == "failed"

    # a scheduler task, incl. a set-scope run's full member list (only ever present on this frame)
    ws_status(nothing, "task-9", "done", "a"; image_uids=["a", "b"], fun="behaviour.hmm")
    @test only(banked("task-9")).image_uids == ["a", "b"]

    # in-flight statuses are not outcomes — the sink hands over every frame, terminal or not
    ws_status(nothing, "task-live", "queued", "EaMaVq"; fun="segment.cellpose")
    ws_status(nothing, "task-live", "running", "EaMaVq"; fun="segment.cellpose")
    @test isempty(banked("task-live"))
    ws_status(nothing, "task-live", "cancelled", "EaMaVq"; fun="segment.cellpose")
    @test only(banked("task-live")).status == "cancelled"     # …until it ends

    # the frame still goes out to clients — banking must not replace broadcasting
    cap = Channel{String}(8)
    key = gensym("test-outcome")
    lock(_ws_clients_lock) do; _ws_clients[key] = cap; end
    try
        ws_status(nothing, "task-bc", "done", "EaMaVq"; fun="segment.cellpose")
        @test isready(cap)
        let f = JSON3.read(take!(cap))
            @test f.type == "task:status" && f.taskId == "task-bc" && f.status == "done"
        end
        @test only(banked("task-bc")).status == "done"
    finally
        lock(_ws_clients_lock) do; delete!(_ws_clients, key); end
    end
    empty!(Cecelia._OUTCOMES)
end

# ── Chain event → WS bridge: taskId degradation ───────────────────────────────
# The bridge reads `task_id` through `chain_event_task_id`, not `p.task_id`, because two real payloads lack a
# usable one: a node with no task id yet (skipped before submission, set-scope/incremental nodes that
# bypass `run_task`) carries `nothing`, and a hand-fired REPL/test event may omit the field entirely.
# Either must degrade to "" — a bridge handler that throws would take down chain telemetry for every
# connected client.
@testset "API: chain bridge taskId degradation" begin
    @test chain_event_task_id((; task_id = "abc123")) == "abc123"
    @test chain_event_task_id((; task_id = nothing))  == ""       # node had no task id yet
    @test chain_event_task_id((; run_id  = "r1"))     == ""       # field absent (hand-fired event)
    @test chain_event_task_id(NamedTuple())            == ""
    @test chain_event_task_id((; task_id = "x")) isa String
end

# ── Chain event → WS bridge: the frames that actually go out ───────────────────
# The four `subscribe_chain_events!` handlers were only covered through `_ev_task_id`, so a mistyped
# key ("taskID", "task_id") would have passed every other test. No socket needed: this harness already
# include-d server.jl, so the subscriptions are live and `broadcast_ws` writes a pre-serialised frame
# into each client's queue — register a Channel as a fake client and read the frame back.
@testset "API: chain bridge frames" begin
    q = Channel{String}(32)
    lock(_ws_clients_lock) do; _ws_clients[:probe] = q; end
    try
        base = (; run_id="r1", chain_name="ch", project_uid="p", image_uid="EaMaVq",
                 node_id="n1", fn="segment.branching", params=Dict{String,Any}("a"=>1),
                 task_id="tid123")
        fire(t, p) = (Cecelia._fire_chain_event!(t, p); JSON3.read(take!(q)))

        for (ev, wire) in (("node:queued", "chain:node:queued"), ("node:running", "chain:node:running"))
            f = fire(ev, base)
            @test String(f.type)       == wire
            @test String(f.taskId)     == "tid123"          # ← the correlation handle, right key name
            @test String(f.runId)      == "r1"
            @test String(f.chainName)  == "ch"
            @test String(f.projectUid) == "p"
            @test String(f.imageUid)   == "EaMaVq"
            @test String(f.nodeId)     == "n1"
            @test String(f.fn)         == "segment.branching"
            @test haskey(f, :params)
        end

        f = fire("node:done", (; base..., result=Dict{String,Any}("valueName"=>"B")))
        @test String(f.type) == "chain:node:done" && String(f.taskId) == "tid123"
        @test String(f.result.valueName) == "B"

        f = fire("node:failed", (; base..., status="cancelled"))
        @test String(f.type) == "chain:node:failed" && String(f.taskId) == "tid123"
        @test String(f.status) == "cancelled"               # console needs WHICH terminal it was

        # a node with no task id yet (skipped/set-scope) must broadcast "" — not null, not a throw
        f = fire("node:failed", (; run_id="r1", chain_name="ch", project_uid="p", image_uid="EaMaVq",
                                  node_id="n2", fn="f", status="skipped", task_id=nothing))
        @test String(f.taskId) == ""
        # …and a hand-fired REPL event that omits the field entirely must not take the bridge down
        f = fire("node:queued", (; run_id="r1", chain_name="ch", project_uid="p", image_uid="EaMaVq",
                                  node_id="n3", fn="f", params=Dict{String,Any}()))
        @test String(f.taskId) == ""

        # ── the bridge is the SECOND carrier of a terminal outcome, and banks it too ──
        # A chain run emits no `task:status` at all, so `ws_status` never sees a chain node: banking only
        # there left every chain node unrecoverable (a dropped `chain:node:done` = a row stuck at running
        # with nothing able to correct it, and the console back to "outcome unseen"). Keyed by the node's
        # scheduler task id — what a consumer correlates a chain row against.
        empty!(Cecelia._OUTCOMES)
        banked(id) = filter(r -> r.id == id, recent_tasks())
        fire("node:queued", base); fire("node:running", base)
        @test isempty(banked("tid123"))                       # in-flight is not an outcome
        fire("node:done", (; base..., result=nothing))
        @test only(banked("tid123")).status   == "done"
        @test only(banked("tid123")).fun_name == "segment.branching"
        @test only(banked("tid123")).image_uid == "EaMaVq"

        fire("node:failed", (; base..., task_id="tid456", status="cancelled"))
        @test only(banked("tid456")).status == "cancelled"     # not flattened to "failed"

        # a SKIPPED node never ran: no task id, and "skipped" is not a terminal task status
        fire("node:failed", (; base..., task_id=nothing, node_id="n9", status="skipped"))
        @test isempty(filter(r -> r.status == "skipped", recent_tasks()))
        @test isempty(banked(""))
        empty!(Cecelia._OUTCOMES)
    finally
        lock(_ws_clients_lock) do; delete!(_ws_clients, :probe); end
        close(q)
    end
end
