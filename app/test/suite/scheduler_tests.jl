# ── TaskJob target sum type (SingleImage / MultiImage) ─────────────────────
#
# The scheduler used to carry the scope as `img::CciaImage` + `imgs::Union{Nothing,Vector{CciaImage}}`
# — `nothing` meant "single image, use img". Every read branched on `isnothing`, and a bare read of
# `job.imgs` looked like it might return the representative. The `TaskJobTarget` ADT (SingleImage |
# MultiImage) makes the two shapes distinct types and moves the branch into `all_images` /
# `run_task_target` / `representative_image` — one dispatch each, no `isnothing`.
@testset "TaskJobTarget: SingleImage | MultiImage" begin
    proj = create_project!(name="tjt-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    a    = add_image!(s; name="a")
    b    = add_image!(s; name="b")

    st = Cecelia.SingleImage(a)
    @test Cecelia.all_images(st) == CciaImage[a]
    @test Cecelia.run_task_target(st) === a
    @test Cecelia.representative_image(st) === a

    mt = Cecelia.MultiImage(CciaImage[a, b])
    @test Cecelia.all_images(mt) == CciaImage[a, b]
    @test Cecelia.run_task_target(mt) == CciaImage[a, b]       # the vector, contents unchanged
    @test Cecelia.representative_image(mt) === a               # first is the representative

    # An empty set-scope job is a bug at THIS layer — the empty vector is caught earlier in
    # `run_task(::CciaTask, ::Vector{CciaImage}, …)`. Refusing at construction closes the "queued
    # a job that will crash on the worker" path.
    @test_throws ArgumentError Cecelia.MultiImage(CciaImage[])
end

# ── A task crash is recorded in the per-image log, not just the console ──────
# Regression: a Julia-side failure (caught in _execute_job!) used to only @warn to the console —
# it never reached {img._dir}/logs/{fun}.log, so a crashed task looked like it just stopped
# mid-run with no error (invisible to get_task_log + on-disk debugging).
@testset "Task crash is teed into the per-image log" begin
    proj = create_project!(name="crash-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")

    logs = String[]
    result = run_task(_CrashTask(), img, Dict{String,Any}(); on_log = l -> push!(logs, l))

    @test result === nothing                                       # crash → nil result
    @test any(l -> occursin("Task crashed", l) && occursin("boom", l), logs)  # reached on_log

    fun_name = Cecelia._fun_name_from_task(_CrashTask())
    logfile  = joinpath(img._dir, "logs", fun_name * ".log")
    @test isfile(logfile)                                          # ...and the on-disk log
    @test occursin("boom", read(logfile, String))

    # ...AND the run log records the FAILED run (so history / the observer can see repeats, not
    # just successes). This is the fix for tasks that silently failed invisibly (broken run_py, HMM).
    rlog = read_run_log(img)
    @test length(rlog) == 1
    @test String(rlog[end]["fun"]) == fun_name && String(rlog[end]["status"]) == "failed"
    rm(proj.root; recursive=true)
end

# ── A cancelled run is RECORDED, and it is recorded before the work starts ───
# Regression, and the expensive one. The run log used to be appended on finish and only for
# :done/:failed — `:cancelled` was skipped as "the user aborted, not an outcome worth logging". So a
# segmentation killed 22 minutes in left NOTHING behind: no run-log entry, no outcome, and a task log
# that simply stopped mid-stream. "I started six, three are running, what happened to the other three?"
# had no answer anywhere in the project. Two things are pinned here:
#   1. the entry exists WHILE the task runs (an append-on-finish log cannot record a run whose process
#      is killed, which is how the detached runner loses its queue — docs/RUNNER.md), and
#   2. the cancel is stamped onto that same entry rather than dropped or duplicated.
@testset "A cancelled run is banked in the run log" begin
    proj = create_project!(name="rl-cancel-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name="s"); name="img")

    _HOLD_TASK_GO[] = Channel{Nothing}(1)
    tid  = "cancelhold$(rand(1000:9999))"
    logs = String[]
    th = Threads.@spawn run_task(_HoldTask(), img, Dict{String,Any}("modelType" => "cyto3",
                                                                    "diameter"  => 17);
                                 task_id = tid, on_log = l -> push!(logs, l))
    try
        @test timedwait(() -> any(r -> r.id == tid && r.status == "running", list_tasks()), 30.0) === :ok
        # (1) OPEN — the run is on disk, as "running", while it is still going
        open_entry = only(filter(e -> String(get(e, "taskId", "")) == tid, read_run_log(img)))
        @test String(open_entry["status"]) == Cecelia.RUN_LOG_RUNNING
        @test String(open_entry["fun"]) == Cecelia._fun_name_from_task(_HoldTask())
        cancel_task!(tid)
    finally
        put!(_HOLD_TASK_GO[], nothing)
        timedwait(() -> istaskdone(th), 30.0)
    end

    # (2) CLOSE — one entry, now terminal. Not skipped (the bug) and not appended twice.
    rlog = filter(e -> String(get(e, "taskId", "")) == tid, read_run_log(img))
    @test length(rlog) == 1
    @test String(rlog[end]["status"]) == "cancelled"
    @test !isempty(String(rlog[end]["finishedAt"]))
    # …and the task log says so, instead of just stopping — which is indistinguishable from a crash
    @test any(l -> occursin("cancelled", lowercase(l)), logs)
    @test occursin("cancelled", lowercase(read(joinpath(img._dir, "logs",
                                                        Cecelia._fun_name_from_task(_HoldTask()) * ".log"),
                                               String)))
    forget_task_start!(tid)
    rm(proj.root; recursive=true)
end

# ── A job ALWAYS releases its submitter, even if the error path itself throws ──
# Regression: `_execute_job!` posted to `job.done` as its last statement, so any throw before that
# (here: the crash `@warn` itself failing) escaped into the dispatcher's fire-and-forget
# `Threads.@spawn` — silently. `run_task` then blocked in `take!(job.done)` FOREVER and never ran
# `_deregister_task!`, leaving the TaskRecord stranded at `:running`: `list_tasks()`, the GUI and
# the task console all keep listing a task that finished, while every pool correctly reads idle
# (the dispatcher's `finally` had released the slot). The post now lives in a `finally`.
@testset "Job posts its result even when the error path throws" begin
    proj = create_project!(name="job-post-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name="s"); name="img")

    tid = "jobpost$(rand(1000:9999))"
    old = global_logger()
    t = try
        global_logger(_ThrowingLogger(old))
        th = Threads.@spawn run_task(_CrashTask(), img, Dict{String,Any}(); task_id = tid)
        timedwait(() -> istaskdone(th), 30.0)        # wait BEFORE restoring, so the throw is injected
        th
    finally
        global_logger(old)                            # a failing @test must not log through it
    end

    @test istaskdone(t)                               # the whole point — this used to hang forever
    # guarded: on a regression the task is still blocked, and a bare fetch would hang the SUITE
    istaskdone(t) && @test fetch(t) === nothing       # aborted job → nil result, like any failure
    # ...and the record is gone, not stranded at :running for the console/GUI to keep showing
    @test !any(r -> r.id == tid, list_tasks())
    rm(proj.root; recursive=true)
end

# ── A terminal task-rail frame is banked for replay ────────────────────────
# Regression (the "0 done · 17 ended" console, and a project export stuck at "running"): the frame
# announcing HOW a unit of work ended is its only carrier and the server drops frames for a slow
# client BY DESIGN, so a client that missed it could never find out. `record_task_outcome!` keeps it.
# Banked from `ws_status` (see api/test) — the rail's one status sink, so background jobs and batch
# movies are covered too, not just scheduler tasks. Here: the log's own contract.
@testset "Task outcome log" begin
    empty!(Cecelia._OUTCOMES)
    rec!(id, status; kw...) = record_task_outcome!(id, status; kw...)

    rec!("o1", "done"; image_uid="img1", fun="segment.cellpose", pool="gpu")
    rec!("o2", "failed"; image_uid="img2", fun="project:export", pool="job")
    rec!("o3", "cancelled"; image_uid="img3")
    rows = recent_tasks()
    @test [r.id for r in rows] == ["o1", "o2", "o3"]              # oldest → newest
    @test [r.status for r in rows] == ["done", "failed", "cancelled"]
    let o1 = first(rows)
        @test o1.fun_name == "segment.cellpose" && o1.pool_name == "gpu"
        @test o1.image_uid == "img1" && o1.image_uids == String[]
        @test occursin(r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$", o1.finished_at)
    end

    # a non-terminal status is NOT an outcome — the one call site hands over every status frame, so
    # banking "running" here would report work that is still going as finished.
    rec!("live", "queued"); rec!("live", "running")
    @test !any(r -> r.id == "live", recent_tasks())

    # a set-scope task's FULL member list survives: it exists only on this frame, and a replay
    # without it invalidates the representative image's plots only (docs/API.md).
    rec!("set1", "done"; image_uid="a", image_uids=["a", "b", "c"])
    @test only(filter(r -> r.id == "set1", recent_tasks())).image_uids == ["a", "b", "c"]

    # ONE row per task id. Both repeats are real: a cancel is announced twice (immediately, then as
    # the final status), and task:restart reuses the id for a new run that must supersede the old.
    rec!("o1", "done"); rec!("o1", "done")
    @test count(r -> r.id == "o1", recent_tasks()) == 1
    rec!("o2", "done")                                            # restarted → new outcome wins
    @test only(filter(r -> r.id == "o2", recent_tasks())).status == "done"
    @test last(recent_tasks()).id == "o2"                         # …and re-appended, still in order

    # `since` is INCLUSIVE, so a poll always re-reads its own newest entry (two units finishing in
    # the same millisecond must not let the second fall through the gap).
    newest = last(recent_tasks()).finished_at
    @test any(r -> r.finished_at == newest, recent_tasks(; since = newest))
    @test isempty(recent_tasks(; since = "9999-01-01T00:00:00.000Z"))
    @test length(recent_tasks(; since = "")) == length(recent_tasks())

    # bounded: a reporting tail for live clients, never run history (that's the on-disk run log)
    for i in 1:(Cecelia._OUTCOME_CAP + 50); rec!("cap$i", "done"); end
    @test length(Cecelia._OUTCOMES) == Cecelia._OUTCOME_CAP
    @test last(recent_tasks()).id == "cap$(Cecelia._OUTCOME_CAP + 50)"   # newest kept
    @test !any(r -> r.id == "o1", recent_tasks())                        # oldest evicted
    empty!(Cecelia._OUTCOMES)
end

# ── The scheduler stamps a task's own timing ───────────────────────────────
# `list_tasks()` is what a client polls to answer "how long has this been going?". Without these
# fields it can only be answered from when the client first SAW the row, so a console or a browser
# tab that attached mid-run could report a lower bound and nothing better (the task console printed
# `≥0s` for a task that had been running for 20 minutes).
@testset "Scheduler records queued/started timestamps" begin
    proj = create_project!(name="tasktime-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name="s"); name="img")

    _HOLD_TASK_GO[] = Channel{Nothing}(1)
    tid  = "hold$(rand(1000:9999))"
    seen = TaskRecord[]
    th = Threads.@spawn run_task(_HoldTask(), img, Dict{String,Any}("modelType" => "cyto3",
                                                                    "diameter"  => 17);
                                 task_id = tid, on_status_change = rec -> push!(seen, rec))
    try
        @test timedwait(() -> any(r -> r.id == tid && r.status == "running", list_tasks()), 30.0) === :ok
        row = only(filter(r -> r.id == tid, list_tasks()))
        # The snapshot's FIELD NAMES are a contract with two independent consumers that share no runtime:
        # `_reconcile_snapshot!` (api/task_console.jl) and `adoptableTasks`
        # (frontend/src/utils/runningTasks.ts), each of which silently blanks a column if one is renamed.
        # Pinned here so a rename fails a test instead — the frontend pins its own half in
        # runningTasks.test.ts.
        @test issubset(Set([:id, :fun_name, :pool_name, :image_uid, :project_uid, :chain_run_id,
                            :chain_node_id, :status, :queued_at, :started_at, :params]), Set(keys(row)))
        # …and WHICH project the image belongs to: one server serves every project under
        # `projects_dir()`, so the task console (which watches the whole rail, not one project) can't
        # tell two images apart by uid alone. Read off the image's path, never stored — the project's
        # identity IS its directory name (docs/OBJECTMODEL.md).
        @test row.project_uid == proj.uid == img_project_uid(img)
        # …and the params it was SUBMITTED with, which is what lets a client that didn't launch the task
        # offer Re-run: with only the fun_name it would relaunch on the JSON spec's defaults instead.
        @test row.params == Dict{String,Any}("modelType" => "cyto3", "diameter" => 17)
        # both are ISO-8601 UTC to the millisecond — one wire format for the whole rail
        @test occursin(r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$", row.queued_at)
        @test occursin(r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$", row.started_at)
        @test row.started_at >= row.queued_at            # a slot is acquired after submission
        # …and the SAME start is on the rail, so it still answers once this record is gone
        @test iso_utc(task_started_at(tid)) == row.started_at
        # the status frames the API sends carry it too (that's what `on_status_change` feeds)
        running = last(filter(r -> r.status === TASK_RUNNING, seen))
        @test !isnothing(running.started_at) && iso_utc(running.started_at) == row.started_at
    finally
        put!(_HOLD_TASK_GO[], nothing)                    # let the task finish even if a @test failed
        timedwait(() -> istaskdone(th), 30.0)
    end
    @test istaskdone(th) && fetch(th) === true
    @test !any(r -> r.id == tid, list_tasks())            # record gone…
    @test !isnothing(task_started_at(tid))                # …but the start outlived it
    forget_task_start!(tid)

    # A chain node reports WHICH node it is, not just which run — the GUI keys a chain row
    # `runId::nodeId::imageUid`, so a snapshot row without the node id can't be matched to one and the
    # same work would be listed twice (once adopted, once from the chain events).
    rec = Cecelia._register_task!("cn$(rand(1000:9999))", "segment.cellpose", "gpu", img.uid,
                                 "run1", _ -> nothing; chain_node_id = "n3")
    let row = only(filter(r -> r.id == rec.id, list_tasks()))
        @test row.chain_run_id == "run1" && row.chain_node_id == "n3"
    end
    Cecelia._deregister_task!(rec.id)

    # a task still QUEUED has a queue time and NO start — so a client shows a wait, not a run of 0s
    rec = Cecelia._register_task!("q$(rand(1000:9999))", "f", "cpu", img.uid, "", _ -> nothing)
    @test isnothing(rec.started_at)
    @test only(filter(r -> r.id == rec.id, list_tasks())).started_at == ""
    # …and a task registered with no params reports an EMPTY set, never a missing field: the frontend
    # reads an absent `params` as "unknown, withhold Re-run" and an empty one as "this task takes none".
    @test only(filter(r -> r.id == rec.id, list_tasks())).params == Dict{String,Any}()
    Cecelia._deregister_task!(rec.id)

    # The whole snapshot is written in ONE JSON3.write, so an unserialisable param value would throw and
    # take `/api/tasks` down for every row — no adoption, no console reconcile, and a quit busy-check
    # that reads idle. Params from the GUI are parsed JSON and always fine; a REPL-dispatched run can
    # put anything in the dict. Published as `null` instead, which the client reads as "unknown".
    rec = Cecelia._register_task!("np$(rand(1000:9999))", "f", "cpu", img.uid, "", _ -> nothing;
                                  params = Dict{String,Any}("fn" => sin, "diameter" => 17))
    let row = only(filter(r -> r.id == rec.id, list_tasks()))
        @test isnothing(row.params)                      # all-or-nothing — NOT a partial dict
        @test JSON3.write(row) isa String                # the endpoint still answers
    end
    Cecelia._deregister_task!(rec.id)

    # …and the nested shapes params actually take DO survive (a group param is a dict of dicts), plus the
    # ones Julia code writes naturally, so a REPL-dispatched run isn't denied Re-run over a tuple.
    nested = Dict{String,Any}("models" => Any["cyto3", "nuclei"], "opts" => Dict("d" => 17, "gpu" => true),
                              "unset" => nothing, "name" => :cellpose,
                              "range" => (1, 10), "shape" => (w = 5, h = 6))
    rec = Cecelia._register_task!("ok$(rand(1000:9999))", "f", "cpu", img.uid, "", _ -> nothing;
                                  params = nested)
    let row = only(filter(r -> r.id == rec.id, list_tasks()))
        @test row.params == nested
        @test JSON3.write(row) isa String
    end
    Cecelia._deregister_task!(rec.id)

    # A whitelist, NOT a `try JSON3.write` probe: JSON3 throws on a Function but happily serialises a
    # plain struct INTO AN OBJECT, so a probe would publish that and a client would Re-run on a value
    # that is not what the task ran with. Anything whose JSON form isn't the value it came from must
    # read as unknown — which is why this is a predicate over shapes, not an attempted write.
    @test !Cecelia._json_writable(img)
    rec = Cecelia._register_task!("st$(rand(1000:9999))", "f", "cpu", img.uid, "", _ -> nothing;
                                  params = Dict{String,Any}("img" => img))
    @test isnothing(only(filter(r -> r.id == rec.id, list_tasks())).params)
    Cecelia._deregister_task!(rec.id)
    rm(proj.root; recursive=true)
end

# ── When a unit of work started ────────────────────────────────────────────
# The other half of the same problem as the outcome log: the scheduler's record is deregistered the
# instant a task finishes, and the consumers that want a DURATION mostly ask afterwards (the chain
# bridge fires node:done once run_task has returned; a dropped terminal frame is recovered minutes
# later). So the start is noted on the rail, and the banked outcome row carries it from then on —
# without which every client has to time tasks off when it first happened to see them.
@testset "Task start timing" begin
    empty!(Cecelia._OUTCOMES); empty!(Cecelia._STARTED)

    # first note wins, so a repeated `running` announcement does not restart the clock
    began = Dates.now(UTC) - Dates.Minute(5)
    @test note_task_started!("t1", began) == began
    @test note_task_started!("t1", Dates.now(UTC)) == began       # ← would have reset the elapsed
    @test task_started_at("t1") == began

    # not started / not noted is `nothing`, never a zero date — a client must be able to tell
    @test isnothing(task_started_at("never-seen"))
    @test isnothing(task_started_at(""))
    @test iso_utc(nothing) == ""                                   # …and it serialises as "", not epoch 0

    # the banked outcome carries the start, and the in-flight note is then dropped: one home for the
    # fact at a time, so nothing can report two different starts for the same task.
    row = record_task_outcome!("t1", "done"; image_uid="img1")
    @test row.started_at == iso_utc(began)
    @test isnothing(task_started_at("t1"))
    @test only(filter(r -> r.id == "t1", recent_tasks())).started_at == iso_utc(began)
    @test row.finished_at >= row.started_at

    # a task that never ran banks an empty start rather than a made-up one
    @test record_task_outcome!("t2", "cancelled").started_at == ""

    # a non-terminal status still returns nothing — the caller uses that to tell live from finished
    @test isnothing(record_task_outcome!("t3", "running"))

    # a reused id (task:restart) is timed from its own beginning, not the previous run's
    note_task_started!("t4", began)
    forget_task_start!("t4")
    @test isnothing(task_started_at("t4"))

    # bounded, like the outcome log: a producer that never announces an outcome must not accumulate
    # forever. The OLDEST starts are evicted — a long-running task is the one whose elapsed matters.
    empty!(Cecelia._STARTED)
    base = Dates.now(UTC) - Dates.Hour(1)
    for i in 1:(Cecelia._STARTED_CAP + 10)
        note_task_started!("s$i", base + Dates.Millisecond(i))
    end
    @test length(Cecelia._STARTED) <= Cecelia._STARTED_CAP
    @test !isnothing(task_started_at("s$(Cecelia._STARTED_CAP + 10)"))   # newest kept
    @test isnothing(task_started_at("s1"))                              # oldest evicted
    empty!(Cecelia._STARTED); empty!(Cecelia._OUTCOMES)
end

@testset "Run log records status (done + failed)" begin
    proj = create_project!(name="rl-status-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name="s"); name="img")
    append_run_log!(img, "segment.cellpose", "default")              # default status = done
    append_run_log!(img, "behaviour.hmm", "", "failed")
    rl = read_run_log(img)
    @test String(rl[1]["status"]) == "done"
    @test String(rl[2]["fun"]) == "behaviour.hmm" && String(rl[2]["status"]) == "failed"
    rm(proj.root; recursive=true)
end

# ── Set expansion — a set resolves to its correct member UIDs ───────────────
# run_tasks and the batch accessors depend on this everywhere.
@testset "Set expansion" begin
    proj = create_project!(name="se-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    a = add_image!(s; name="a"); b = add_image!(s; name="b"); c = add_image!(s; name="c")
    expected = Set([a.uid, b.uid, c.uid])

    @test Set(i.uid for i in images(s))    == expected   # in-memory
    @test Set(i.uid for i in images(proj)) == expected
    reloaded = init_object(proj.uid, s.uid)              # reload from disk
    @test reloaded isa CciaSet
    @test Set(reloaded.image_uids)            == expected
    @test Set(i.uid for i in images(reloaded)) == expected
    rm(proj.root; recursive=true)
end

# ── Renaming a set is a metadata edit, and must not disturb its membership ─────
# A set's identity is its uid (the dir under {proj}/1/), so the name is display-only. The thing worth
# pinning is that it lands ON DISK and that nothing else moves: the images stay attached, and the
# project manifest — which stores uids — is untouched.
@testset "Set rename" begin
    proj = create_project!(name="sr-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="before")
    a = add_image!(s; name="a"); b = add_image!(s; name="b")

    out = rename_set!(proj, s.uid, "after")
    @test out.uid == s.uid && out.name == "after"        # same object, new name
    @test s.name == "after"                              # the loaded set is in step with disk
    reloaded = init_object(proj.uid, s.uid)
    @test reloaded.name == "after"                        # persisted, not just in memory
    @test Set(reloaded.image_uids) == Set([a.uid, b.uid]) # membership survived the one-field commit
    @test proj.set_uids == [s.uid]                        # the manifest keys off the uid, so untouched

    # The duplicate-name GUARD lives at this layer on purpose, so it holds for a REPL caller — which is
    # where a rename is most likely to be scripted across a cohort and least likely to be eyeballed.
    # `add_set!` carries the same guard, so it covers all three creation paths (create route,
    # `newSetName` on move, the copyImage task) at once.
    @test_throws ErrorException add_set!(proj; name="after")
    @test length(proj._sets) == 1                             # refused before anything was written
    other = add_set!(proj; name="other")
    @test set_name_taken(proj, "after")                       # taken by `s`
    @test !set_name_taken(proj, "after"; except = s.uid)      # …but not by anyone ELSE
    @test !set_name_taken(proj, "unused")
    @test_throws ErrorException rename_set!(proj, other.uid, "after")
    @test init_object(proj.uid, other.uid).name == "other"    # refused, and nothing was written

    # Renaming to its OWN name is a no-op rather than a self-conflict, so a re-run is idempotent.
    @test rename_set!(proj, s.uid, "after").name == "after"

    # …and `force` is the escape hatch for a caller who means it (two sets CAN share a name — they are
    # still distinct objects, keyed by uid). Both mutators take it, spelled the same way.
    forced = add_set!(proj; name="after", force = true)
    @test forced.name == "after" && forced.uid != s.uid
    delete_set!(proj, forced.uid)
    rename_set!(proj, other.uid, "after"; force = true)
    @test init_object(proj.uid, other.uid).name == "after"
    @test other.uid != s.uid

    # the set-object form, which is what a REPL caller holding `sets(proj)[i]` reaches for
    @test rename_set!(proj, other, "by object").name == "by object"
    @test init_object(proj.uid, other.uid).name == "by object"

    @test_throws ErrorException rename_set!(proj, "nosuchset", "x")
    rm(proj.root; recursive=true)
end

# ── Sink-agnostic task execution (runner/execute.jl) ──────────────────────────
# `execute_task` is the body `handle_task_run` used to inline. It is tested HERE, in the package, with
# no server and no socket — which is the whole point: the API server and the detached runner drive the
# same function, so its contract (scope dispatch, terminal frame on every path, result-before-status)
# must hold without either of them. See docs/todo/TASK_RUNNER_PLAN.md.

# Collect every announcement in call order, so ORDERING can be asserted and not just membership.
function _collect_exec(req)
    logs, sts, res = String[], Tuple{String,String,Vector{String}}[], Any[]
    final = execute_task(req;
        on_log      = l -> push!(logs, l),
        on_status   = (s, uid, uids) -> push!(sts, (s, uid, uids)),
        on_result   = (uid, meta) -> push!(res, (uid, meta)))
    (; final, logs, statuses = sts, results = res)
end

@testset "execute_task — image scope" begin
    proj = create_project!(name="exec-img-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name="s"); name="img")

    r = _collect_exec(TaskRequest(; task_id = "exec$(rand(1000:9999))",
                                   fun_name = "testTasks.image_task",
                                   project_uid = proj.uid, image_uid = img.uid,
                                   params = Dict{String,Any}("message" => "hello runner")))

    @test r.final == :done
    @test any(l -> occursin("hello runner", l), r.logs)          # on_log reached the caller
    @test last(r.statuses) == ("done", img.uid, String[])        # terminal frame, image-scope shape
    @test "running" in [s for (s, _, _) in r.statuses]           # live transitions forwarded
    @test length(r.results) == 1                                 # ...and the result arrived
    # ORDERING: the result must precede the terminal status — the frontend keys off that, and
    # reversing them silently drops the result.
    @test r.statuses[end][1] == "done" && !isempty(r.results)
    rm(proj.root; recursive=true)
end

@testset "execute_task — set scope names every member" begin
    proj = create_project!(name="exec-set-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    a, b = add_image!(s; name="a"), add_image!(s; name="b")

    r = _collect_exec(TaskRequest(; task_id = "execset$(rand(1000:9999))",
                                   fun_name = "testTasks.set_task",
                                   project_uid = proj.uid,
                                   image_uids = [a.uid, b.uid]))

    @test r.final == :done
    st, uid, uids = last(r.statuses)
    @test st == "done"
    @test uid == a.uid                       # representative = first image
    # The terminal frame carries ALL members. The representative alone leaves the other members'
    # plots stale — the non-rep-member gap this list closes.
    @test Set(uids) == Set([a.uid, b.uid])
    rm(proj.root; recursive=true)
end

# Every exit path must emit a terminal status, including the ones that never reach the scheduler.
# A failure that logs nothing and announces nothing is how a task pins at `running` forever in the
# GUI, and how anything keyed on the terminal frame (the observer's Watch trigger) never fires.
@testset "execute_task — failure paths still announce" begin
    proj = create_project!(name="exec-fail-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name="s"); name="img")

    unknown = _collect_exec(TaskRequest(; task_id = "execunk", fun_name = "nope.notATask",
                                          project_uid = proj.uid, image_uid = img.uid))
    @test unknown.final == :failed
    @test last(unknown.statuses)[1] == "failed"
    @test any(l -> occursin("Unknown task", l), unknown.logs)

    missing_img = _collect_exec(TaskRequest(; task_id = "execmiss",
                                             fun_name = "testTasks.image_task",
                                             project_uid = proj.uid, image_uid = "nosuchuid"))
    @test missing_img.final == :failed
    @test last(missing_img.statuses)[1] == "failed"

    # A bad param throws in `run_task` BEFORE any job is queued, so `on_status_change` never fires —
    # the terminal frame here can only come from execute_task's own outer catch.
    badparam = _collect_exec(TaskRequest(; task_id = "execbad",
                                          fun_name = "testTasks.image_task",
                                          project_uid = proj.uid, image_uid = img.uid,
                                          params = Dict{String,Any}("waitMs" => "not-an-int")))
    @test badparam.final == :failed
    @test last(badparam.statuses)[1] == "failed"
    @test any(l -> occursin("[ERROR]", l), badparam.logs)

    rm(proj.root; recursive=true)
end

# The request IS the wire format, so it round-trips through JSON unchanged — one encoder, one decoder.
# Each transport shaping its own dict is how a param key goes missing on one path only.
@testset "TaskRequest survives the wire" begin
    req = TaskRequest(; task_id = "t1", fun_name = "segment.cellpose", project_uid = "p1",
                        image_uid = "i1", image_uids = ["i1", "i2"], pool_name = "gpu",
                        params = Dict{String,Any}("blockSize" => 512, "name" => "x"))
    back = task_request(JSON3.read(JSON3.write(task_request_dict(req)), Dict{String,Any}))

    @test back.task_id == req.task_id && back.fun_name == req.fun_name
    @test back.project_uid == req.project_uid && back.image_uid == req.image_uid
    @test back.image_uids == req.image_uids && back.pool_name == req.pool_name
    @test back.params["blockSize"] == 512 && back.params["name"] == "x"
    @test back.target == "local"          # the field a second target lands in, defaulted not omitted

    # A request off the wire with only the required keys must not throw — a sender on older code
    # omitting an optional field is a compatibility case, not an error.
    minimal = task_request(Dict{String,Any}("taskId" => "t2", "funName" => "f", "projectUid" => "p"))
    @test minimal.image_uids == String[] && minimal.params == Dict{String,Any}()
    @test minimal.target == "local"
end

# ── The detached task runner ──────────────────────────────────────────────────
# In-process: the server's routes and fan-out, driven directly. A REAL second process is exercised by
# `pixi run test-runner` (app/test/runner_e2e.jl) — it costs a Julia start-up + precompile, which does
# not belong in the suite everyone runs.
@testset "runner protocol identity" begin
    # `/ping` must carry what tells an adopted runner apart from one we started, and WHICH CODE it is
    # running — a runner that deliberately survives your edits is only safe if it says so (Decision 5).
    id = runner_identity()
    @test id["protocol"] == RUNNER_PROTOCOL
    @test id["pid"] == getpid()
    @test haskey(id, "commit") && haskey(id, "startedAt") && haskey(id, "projectsDir")
end

@testset "runner_emit never blocks or throws without subscribers" begin
    # Emission happens on a POOL WORKER thread. If it could block or throw it would wedge a slot, so
    # the no-subscriber case has to be a silent no-op rather than an error path nobody tests.
    @test runner_emit(Dict{String,Any}("type" => "task:log", "taskId" => "x", "line" => "hi")) === nothing
end

# The opt-in is a SETTING, not just an env var — a packaged install has no way to set an env var, so
# "opt-in" would have meant "dev-only". The override still wins, which is what `pixi run dev-runner`
# and CI use; the test pins that precedence, because getting it backwards makes the toggle a lie.
# The runner is DEV-ONLY. A prod install has no Restart button, so its whole benefit is unreachable
# there while every failure mode (an idle process with no window, no cancel, nothing to find it by)
# lands on the user — and a prod user does not need it: they leave the app running, and closing the
# browser tab was never what stopped a task. The gate is asserted, not remembered, because "it is only
# a setting, what harm" is exactly how it would drift back on.
@testset "runner is dev-only, then a setting, then an env override" begin
    # `init_cecelia!()` mutates PROCESS-WIDE config, so this testset must put it back — leaving it
    # pointed at a throwaway config dir broke 37 later testsets (every one that touches
    # projects_dir()) the first time this was written, while passing itself.
    cfg = mktempdir()
    try
        # NOT dev: off, whatever the file says. Written first because it is the load-bearing one.
        withenv("CECELIA_DEV_DIR" => cfg, "CECELIA_DEV" => nothing, "CECELIA_RUNNER" => nothing) do
            init_cecelia!()
            @test is_dev_session() == false
            @test set_runner_enabled!(true) == false      # persisted, but not in EFFECT here
            @test runner_enabled() == false
        end
        # …and not even an explicit env override turns it on outside dev.
        withenv("CECELIA_DEV_DIR" => cfg, "CECELIA_DEV" => nothing, "CECELIA_RUNNER" => "1") do
            init_cecelia!()
            @test runner_enabled() == false
        end

        withenv("CECELIA_DEV_DIR" => cfg, "CECELIA_DEV" => "1", "CECELIA_RUNNER" => nothing) do
            init_cecelia!()
            @test runner_enabled() == true                # the value written above, now in effect
            @test occursin("[runner]", read(custom_toml_path(), String))
            @test set_runner_enabled!(false) == false
            init_cecelia!()
            @test runner_enabled() == false               # …and it survives a config reload
        end
        withenv("CECELIA_DEV_DIR" => cfg, "CECELIA_DEV" => "1", "CECELIA_RUNNER" => "1") do
            init_cecelia!()
            @test runner_enabled() == true                # env wins over a file that says false
        end
        withenv("CECELIA_DEV_DIR" => cfg, "CECELIA_DEV" => "1", "CECELIA_RUNNER" => "0") do
            init_cecelia!()
            @test runner_enabled() == false               # …in both directions
        end
    finally
        init_cecelia!()                                   # restore the suite's own config
    end
end

@testset "TLS desired: env overrides toml overrides default (prod on / dev off)" begin
    # Parallel to the runner testset above — same resolution shape. The default IS the
    # feature that changed (opt-in → prod-on), so it is the one pinned by every branch here.
    cfg = mktempdir()
    try
        # PROD default (no dev, no env, no toml key): TLS on. The whole point of the flip.
        withenv("CECELIA_DEV_DIR" => cfg, "CECELIA_DEV" => nothing, "CECELIA_TLS" => nothing) do
            init_cecelia!()
            @test tls_desired(is_dev = false) == true
        end
        # DEV default: TLS off. Vite proxy is HTTP/1.1-only, TLS earns nothing.
        withenv("CECELIA_DEV_DIR" => cfg, "CECELIA_DEV" => "1", "CECELIA_TLS" => nothing) do
            init_cecelia!()
            @test tls_desired(is_dev = true) == false
        end
        # Settings toggle persists in both modes and takes precedence over the default.
        withenv("CECELIA_DEV_DIR" => cfg, "CECELIA_DEV" => nothing, "CECELIA_TLS" => nothing) do
            init_cecelia!()
            @test set_tls_desired!(false; is_dev = false) == false
            @test occursin("[tls]", read(custom_toml_path(), String))
            init_cecelia!()
            @test tls_desired(is_dev = false) == false     # …survives a reload
        end
        # …and env still wins over the file, in both directions.
        withenv("CECELIA_DEV_DIR" => cfg, "CECELIA_DEV" => nothing, "CECELIA_TLS" => "1") do
            init_cecelia!()
            @test tls_desired(is_dev = false) == true       # env-on beats toml-off
        end
        withenv("CECELIA_DEV_DIR" => cfg, "CECELIA_DEV" => nothing, "CECELIA_TLS" => "0") do
            init_cecelia!()
            @test set_tls_desired!(true; is_dev = false) == false   # written but env forces off
            @test tls_desired(is_dev = false) == false
        end
    finally
        init_cecelia!()
    end
end

@testset "runner client refuses a dead port cleanly" begin
    # Every one of these runs on the API server's request path. A runner that is simply not there must
    # read as absent — never a throw that takes a route down, and never a hang.
    h = RunnerHandle(; port = 7699)          # nothing listens here
    @test runner_ping(h) === nothing
    @test runner_alive(h) == false
end

