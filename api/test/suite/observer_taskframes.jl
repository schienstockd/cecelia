# Observer WS broadcasts + task-frame instrumentation testsets — extracted from api/test/runtests.jl.
#
# Four testsets covering the mcp/observer + task-frame surface:
#  - `API: observer event broadcasts` (mcp/ Slice B — WS frames drive observer patterns)
#  - `API: bad-param launch still emits [ERROR] + terminal failed frame`
#  - `API: status frames carry the task task timing`
#  - `API: task log sliced between two runs bounds`
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

@testset "API: observer event broadcasts" begin
    # register a capture client; drain returns the parsed frames seen since the last drain
    cap = Channel{String}(64)
    key = gensym("test-observer")
    lock(_ws_clients_lock) do; _ws_clients[key] = cap; end
    drain() = (frames = []; while isready(cap); push!(frames, JSON3.read(take!(cap))); end; frames)

    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir()
    dirs["projects"] = tmp
    try
        proj = create_project!(name="api-observer")
        uid  = proj.uid
        s    = add_set!(proj; name="set-A")
        img  = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))

        # ── ws_status carries `fun` so a module-page run is attributable to a function ──
        drain()
        ws_status(nothing, "task-1", "done", img.uid; fun="segment.cellpose")
        let f = drain()
            @test length(f) == 1
            @test f[1].type == "task:status" && f[1].fun == "segment.cellpose"
            @test f[1].status == "done" && f[1].imageUid == img.uid
        end

        # ── image_note_added fires when a note is set ──
        drain()
        @test _post(api_images_inclusion_set,
                    Dict("projectUid"=>uid, "values"=>Dict(img.uid=>Dict("note"=>"odd cells"))))[1] == 200
        let f = drain()
            note = filter(x -> x.type == "image_note_added", f)
            @test length(note) == 1
            @test note[1].imageUid == img.uid && note[1].note == "odd cells" && note[1].projectUid == uid
        end
        # setting only `included` (no note) does NOT broadcast a note event
        drain()
        @test _post(api_images_inclusion_set,
                    Dict("projectUid"=>uid, "values"=>Dict(img.uid=>Dict("included"=>false))))[1] == 200
        @test isempty(filter(x -> x.type == "image_note_added", drain()))

        # ── lab_log_entry_added fires for USER entries only (anti-loop); lab_log_updated (the panel-
        # reload signal) fires for EVERY append so an external Chat-to-Claude append still refreshes ──
        drain()
        @test _post(api_lablog_append, Dict("projectUid"=>uid, "author"=>"User", "lines"=>["switched to diam 30"]))[1] == 200
        let f = drain()
            ea = filter(x -> x.type == "lab_log_entry_added", f)
            @test length(ea) == 1 && occursin("diam 30", ea[1].summary) && ea[1].projectUid == uid
            @test length(filter(x -> x.type == "lab_log_updated" && x.projectUid == uid, f)) == 1
        end
        # the observer's own [Claude] append must NOT re-broadcast entry_added (would loop) — but it
        # STILL emits lab_log_updated so an open panel reloads (the external-append bug fix)
        drain()
        @test _post(api_lablog_append, Dict("projectUid"=>uid, "author"=>"Claude", "lines"=>["noted"]))[1] == 200
        let f = drain()
            @test isempty(filter(x -> x.type == "lab_log_entry_added", f))
            @test length(filter(x -> x.type == "lab_log_updated", f)) == 1
        end
        # [Cecelia] auto-digests: no entry_added either, but still a panel reload
        drain()
        @test _post(api_lablog_append, Dict("projectUid"=>uid, "author"=>"Cecelia", "lines"=>["digest"]))[1] == 200
        let f = drain()
            @test isempty(filter(x -> x.type == "lab_log_entry_added", f))
            @test length(filter(x -> x.type == "lab_log_updated", f)) == 1
        end
    finally
        lock(_ws_clients_lock) do; delete!(_ws_clients, key); end
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive=true, force=true)
    end
end

@testset "API: bad-param launch still emits [ERROR] + terminal failed frame" begin
    # An explicit JSON `null` is ordinary for "I have no value" — a canvas panel legitimately has no
    # set — and `String(get(data, :setUid, ""))` did NOT tolerate it: `get`'s default only fires for a
    # MISSING key, so JSON3's `nothing` reached `String(::Nothing)` and aborted the whole handler.
    # It shipped as `WS message error — MethodError: no method matching String(::Nothing)` on a track
    # correction that then silently never ran.
    @testset "_wstr absorbs an explicit null at the WS boundary" begin
        d = JSON3.read(JSON3.write((; setUid = nothing, projectUid = "abc", mode = nothing)))
        @test _wstr(d, :setUid) == ""
        @test _wstr(d, :projectUid) == "abc"
        @test _wstr(d, :missingKey) == ""
        # a non-empty default still applies to both absent AND null
        @test _wstr(d, :mode, "error") == "error"
        @test _wstr(d, :alsoMissing, "error") == "error"
    end

    # `_parse_body` is the ONE boundary every route setter goes through — malformed JSON
    # returns a uniform 400 rather than a stack-tracing 500. Before this helper, some setters
    # had a copy of the same try/catch and some had none (raw `JSON3.read(body_bytes)`), so a
    # `{` was a server crash for one endpoint and a clean 400 for its neighbour. Pinning the
    # tuple shape here matches how handlers dispatch: `data = _parse_body(...); data isa Tuple
    # && return data`.
    @testset "_parse_body: malformed JSON is a uniform 400, empty body is opt-in" begin
        # a normal parse
        body = _parse_body(Vector{UInt8}("{\"name\":\"raw\"}"))
        @test !(body isa Tuple)
        @test _wstr(body, :name) == "raw"

        # empty body defaults to 400 unless allow_empty
        res = _parse_body(UInt8[])
        @test res isa Tuple && res[1] == 400
        empty_ok = _parse_body(UInt8[]; allow_empty = true)
        @test empty_ok isa AbstractDict && isempty(empty_ok)

        # a garbage body is always 400 — an "invalid JSON body" from the server, not a stack trace
        for junk in ("{", "not json", "\0")
            r = _parse_body(Vector{UInt8}(junk))
            @test r isa Tuple && r[1] == 400
            @test occursin("Invalid JSON body", r[2])
        end
    end

    # Typed field getters — same null-tolerance contract as `_wstr`/`_wbool`. Absence and
    # explicit JSON `null` both fall back to `default`; anything else is coerced. Before this,
    # `Int(get(data, :limit, 0))` and `[String(u) for u in get(data, :imageUids, [])]` were
    # inline and CRASHED on a client sending `null` (Int(nothing) / iterating nothing) — the
    # bug `_wstr` was written to close, one field type at a time.
    @testset "_wint / _wvec_str null-tolerance" begin
        d = JSON3.read(JSON3.write((; limit = nothing, count = 5, imageUids = nothing,
                                     names = ["a", "b"], other = [1, 2])))
        @test _wint(d, :limit)               == 0        # explicit null → default
        @test _wint(d, :limit, 3)            == 3        # explicit null → given default
        @test _wint(d, :missingKey)          == 0        # absent → default
        @test _wint(d, :count)               == 5        # number → Int
        @test _wvec_str(d, :imageUids)       == String[] # explicit null → empty Vector{String}
        @test _wvec_str(d, :missingKey)      == String[] # absent → empty Vector{String}
        @test _wvec_str(d, :names)           == ["a", "b"]
        @test _wvec_str(d, :other)           == ["1", "2"]  # non-string entries coerced
    end

    # `parse_ws_msg_type` is the ONE place the wire string becomes a typed value. Every dispatch
    # arm in `handle_message` compares an enum — a stray typo like `WS_TASK_CANCLE` is a compile
    # error instead of a silently-dropped Cancel button. Pin the full registry both directions so a
    # rename requires updating the string on this line, and a wire literal added to the dispatch
    # without a matching enum arm cannot smuggle "unknown" into a running handler.
    @testset "WsMsgType parses every wire literal (round-trip)" begin
        expected = Dict(
            WS_PING         => "ping",
            WS_TASK_RUN     => "task:run",
            WS_TASK_RESTART => "task:restart",
            WS_TASK_CANCEL  => "task:cancel",
            WS_MOVIE_BATCH  => "movie:batch",
            WS_MOVIE_RECORD => "movie:record",
            WS_CHAIN_RUN    => "chain:run",
            WS_CHAIN_CANCEL => "chain:cancel",
            WS_MAINT_RUN    => "maintenance:run",
            WS_MAINT_CANCEL => "maintenance:cancel",
            WS_PROJ_EXPORT  => "project:export",
            WS_PROJ_IMPORT  => "project:import",
            WS_VIEWER_HELLO => "viewer:hello",
        )
        for (kind, wire) in expected
            @test parse_ws_msg_type(wire) === kind
            @test string(kind) == wire
        end
        # Unrecognised (including a typo, an empty string, and a null-derived empty) becomes
        # WS_UNKNOWN. `handle_message`'s fallback branch logs + drops on this value, so the
        # server survives garbage from a client.
        for junk in ("", "not:a:type", "task:cancle", "TASK:RUN")
            @test parse_ws_msg_type(junk) === WS_UNKNOWN
        end
    end

    # run_task validates params FIRST and throws before any job runs. handle_task_run must catch that
    # and STILL emit a task log + a terminal task:status:failed frame — otherwise the throw dies in
    # the @spawn silently and the observer's "Watch" auto-trigger (which keys off the terminal frame)
    # never fires. This is the regression the HMM-with-no-params case exposed.
    cap = Channel{String}(64)
    key = gensym("test-taskfail")
    lock(_ws_clients_lock) do; _ws_clients[key] = cap; end
    drain() = (fs = Any[]; while isready(cap); push!(fs, JSON3.read(take!(cap))); end; fs)

    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name="api-taskfail")
        s    = add_set!(proj; name="set-A")
        img  = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))

        # importImages.omezarr with pyramidLevels=99 fails validate_params (min/max) before any job.
        drain()
        handle_task_run(nothing, Dict{Symbol,Any}(
            :taskId => "t-fail", :funName => "importImages.omezarr",
            :projectUid => proj.uid, :imageUid => img.uid,
            :params => Dict{String,Any}("pyramidLevels" => 99)))

        # the handler runs the task on a @spawn — poll until the terminal frame lands (or time out)
        frames = Any[]
        for _ in 1:200
            append!(frames, drain())
            any(f -> f.type == "task:status" && f.status == "failed", frames) && break
            sleep(0.05)
        end
        status = filter(f -> f.type == "task:status", frames)
        @test any(f -> f.status == "failed" && f.fun == "importImages.omezarr", status)
        # the [ERROR] log names the offending param → confirms we reached (and reported) validation
        errs = filter(f -> f.type == "task:log" && occursin("[ERROR]", String(f.line)), frames)
        @test any(f -> occursin("pyramidLevels", String(f.line)), errs)
    finally
        lock(_ws_clients_lock) do; delete!(_ws_clients, key); end
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive=true, force=true)
    end
end

# ── Every status frame carries the task's timing ───────────────────────────────
# `ws_status` is the rail's one status sink, so it is where a client learns WHEN a task ran. Without
# this a client can only time a task from when its own socket happened to receive the frame — which
# restarts at zero on a page reload and overstates by the poll delay on a recovered frame.
#
# It is also the sink that covers the producers with NO scheduler record — background jobs
# (`pool="job"`) and batch movies (`pool="viewer"`) announce themselves only here — so `running`
# notes the start on the rail rather than assuming somebody upstream did.
@testset "API: status frames carry the task's timing" begin
    cap = Channel{String}(64)
    key = gensym("test-tasktime")
    lock(_ws_clients_lock) do; _ws_clients[key] = cap; end
    drain() = (fs = Any[]; while isready(cap); push!(fs, JSON3.read(take!(cap))); end; fs)
    isots   = r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$"
    try
        tid = "wsdur$(rand(1000:9999))"
        forget_task_start!(tid)

        # queued: nothing has started, so the frame says so with "" — never a placeholder date
        drain()
        ws_status(nothing, tid, "queued"; fun="project:export", pool="job")
        let f = only(drain())
            @test f.startedAt == "" && f.finishedAt == ""
        end

        # running: the sink itself notes the start (this producer has no TaskRecord) and publishes it
        ws_status(nothing, tid, "running"; fun="project:export", pool="job")
        started = only(drain()).startedAt
        @test occursin(isots, started)
        @test iso_utc(task_started_at(tid)) == started

        # a re-announced running must not restart the clock — the same start comes back out
        ws_status(nothing, tid, "running"; fun="project:export", pool="job")
        @test only(drain()).startedAt == started

        # terminal: both ends, and they are the SAME values banked for replay — a client that missed
        # this frame and recovers it from /api/tasks/recent must not compute a different duration
        ws_status(nothing, tid, "done"; fun="project:export", pool="job")
        let f = only(drain())
            @test f.startedAt == started
            @test occursin(isots, f.finishedAt)
            row = only(filter(r -> r.id == tid, recent_tasks()))
            @test row.started_at == f.startedAt && row.finished_at == f.finishedAt
        end
        # …and the in-flight note is released once the row owns it
        @test isnothing(task_started_at(tid))
    finally
        lock(_ws_clients_lock) do; delete!(_ws_clients, key); end
    end
end

# ── One run's slice of a cumulative task log ───────────────────────────────────
# `logs/{fun}.log` is appended to by EVERY run of that fun on that image, and its lines are stamped in
# LOCAL time. The GUI backfills a live task's log by that task's UTC `started_at` (`since`), and a
# history row for a past run additionally passes the NEXT same-fun run's start as `until` — otherwise
# an older row's slice runs to EOF and shows every subsequent run's output as its own. The slice
# happens server-side, where the clock that wrote the stamps lives.
@testset "API: task log sliced between two runs' bounds" begin
    off  = _tasklog_local_offset()                       # what the writer's stamps are offset by
    # a stamp N seconds ago, written the way `_wrap_log_with_file` writes them
    stamp(secs) = Dates.format(Dates.now(UTC) + off - Dates.Second(secs), "yyyy-mm-dd HH:MM:SS")
    iso(secs)   = iso_utc(Dates.now(UTC) - Dates.Second(secs))

    log = """
    [$(stamp(600))] old run: starting
    [$(stamp(590))] old run: done
    [$(stamp(60))] this run: starting
    a bare continuation line
    [$(stamp(10))] this run: 5/20
    """
    log = join(lstrip.(split(strip(log), '\n')), '\n') * '\n'

    # `since` only — the live-row / newest-history-row case: no next run, keep to EOF
    kept = _tasklog_slice(log, iso(120))
    @test occursin("this run: starting", kept)
    @test occursin("this run: 5/20", kept)
    @test !occursin("old run", kept)
    # an unstamped line belongs to the line above it, so a multi-line message isn't torn apart
    @test occursin("a bare continuation line", kept)

    # `since` + `until` — the older-history-row case: the OLD run must not drag the newer one in.
    # Half-open bound: a line stamped exactly at `until` belongs to the next run, not this one.
    old = _tasklog_slice(log, iso(700), iso(120))
    @test occursin("old run: starting", old)
    @test occursin("old run: done", old)
    @test !occursin("this run", old)                     # ← the whole point

    # a start BEFORE everything keeps everything; one after everything keeps nothing
    @test occursin("old run: starting", _tasklog_slice(log, iso(9999)))
    @test strip(_tasklog_slice(log, iso(-60))) == ""

    # `until` on its own — no lower bound, keep the preamble too
    @test occursin("old run: starting", _tasklog_slice(log, "", iso(120)))
    @test !occursin("this run", _tasklog_slice(log, "", iso(120)))

    # garbage `since` degrades to no lower bound — showing too much beats showing nothing
    @test _tasklog_slice(log, "not a timestamp") == log
    @test _tasklog_slice("", iso(120)) == ""
end
