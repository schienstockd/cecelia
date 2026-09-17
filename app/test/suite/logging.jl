# ── Log rail testsets ─────────────────────────────────────────────────
# Five sections pinning the pure halves of app/src/log_stream.jl: log_record's exception
# formatting, child log-line reassembly (level + traceback), the LogRing seq that makes a
# dropped WS frame detectable, the discipline that long-lived children reach the log rail
# (never fire-and-forget), and the log-sources ⇔ frontend chip-set contract. Extracted from
# suite.jl to keep it small enough to merge without EOF conflicts on every append. The
# extracted file loads inside this file's aggregating testset scope, so any helpers defined
# earlier in suite.jl are still in scope (lexical include).
#
# The one `@__DIR__ .. .. frontend/src/utils/logFilter.ts` repo-reach (the log-sources
# ratchet reads frontend logFilter.ts to enforce the chip-set contract) is rerouted through
# pathof(Cecelia) so it resolves identically whether the file sits at app/test/ or
# app/test/suite/.

_repo = dirname(dirname(dirname(pathof(Cecelia))))

# ── The log rail (app/src/log_stream.jl) ─────────────────────────────────────
# What the bottom console shows is built here, and every piece of it used to be got wrong somewhere:
# the exception kwarg was interpolated instead of formatted, child stdio was thrown away, and a
# dropped WS frame was undetectable. These pin the pure halves of all three.
@testset "log_record formats an exception instead of interpolating it" begin
    e, bt = try; error("boom"); catch err; (err, catch_backtrace()); end

    rec = log_record("error", "Unhandled error in POST /api/x"; exception = (e, bt))
    @test rec["level"]  == "error"
    @test rec["source"] == LOG_SOURCE_BACKEND
    # THE regression this file exists for: `"$k = $v"` on an (exception, backtrace) tuple rendered
    # 857 characters of `Ptr{Nothing}(0x…)` into the collapsed row. A formatted trace says
    # "Stacktrace:" and names files; a interpolated one is a wall of pointers.
    @test occursin("Stacktrace:", rec["detail"])
    @test !occursin("Ptr{Nothing}(0x", rec["detail"])
    # the exception's first line is promoted into the row — a row reading only "…failed" is why the
    # one word that matters used to be a click away
    @test occursin("boom", rec["message"])
    @test occursin("Unhandled error", rec["message"])

    # an exception with no backtrace (the `exception = e` call sites) still formats
    @test occursin("boom", log_record("warn", "show_labels failed"; exception = e)["detail"])
    # …and is not repeated when the message already carries it
    @test count(_ -> true, findall("boom", log_record("warn", "boom"; exception = e)["message"])) == 1

    # ordinary kwargs go to the DETAIL, not the row: they are what made rows unreadable
    plain = log_record("info", "GL renderer"; renderer = "NVIDIA", vendor = "NVIDIA Corp")
    @test plain["message"] == "GL renderer"
    @test occursin("renderer = NVIDIA", plain["detail"])
    @test !haskey(log_record("info", "plain line"), "detail")

    # `source` is a facet, never text in the body
    @test log_record("info", "x"; source = LOG_SOURCE_PREVIEW)["source"] == LOG_SOURCE_PREVIEW
    @test !haskey(log_record("info", "x"; source = LOG_SOURCE_PREVIEW), "detail")

    # a detail is capped — it rides a WS frame into a browser store
    big = log_record("error", "huge"; detail = repeat("x", LOG_DETAIL_CAP * 2))["detail"]
    @test length(big) < LOG_DETAIL_CAP + 100
    @test occursin("truncated", big)

    # never throws on a value that is neither shape
    @test log_record("error", "odd"; exception = (1, 2))["detail"] isa String
end

@testset "child log lines — level and traceback reassembly" begin
    @test Cecelia.child_line_level("[preview] added labels: shape=(1, 2)") == "info"
    @test Cecelia.child_line_level("[ERROR] $(:x): could not open")       == "error"
    @test Cecelia.child_line_level("ValueError: invalid literal for int") == "error"
    @test Cecelia.child_line_level("cecelia.utils.zarr_utils.StoreError: nope") == "error"
    @test Cecelia.child_line_level("[WARN] could not read back the movie size") == "warn"
    @test Cecelia.child_line_level("UserWarning: deprecated")             == "warn"

    # A Python traceback is ONE event over many lines. Split per line, the frames classify as `info`
    # — which the default view hides — so the console would show a bare header with nothing under it.
    sink = Cecelia.ChildLineSink(LOG_SOURCE_PREVIEW)
    recs = Dict{String,Any}[]
    for ln in ["preview worker ready on ws://127.0.0.1:7656",
               "Traceback (most recent call last):",
               "  File \"preview_worker.py\", line 903, in _serve",
               "    reply = handle(msg)",
               "  File \"preview_worker.py\", line 400, in handle",
               "    raise ValueError(\"bad region\")",
               "ValueError: bad region",
               "preview worker ready again"]
        append!(recs, Cecelia.child_line_records!(sink, ln))
    end
    @test length(recs) == 3                                   # ready · the whole traceback · ready
    tb = recs[2]
    @test tb["level"]   == "error"
    @test tb["source"]  == LOG_SOURCE_PREVIEW
    @test tb["message"] == "ValueError: bad region"           # the row names the failure, not "Traceback"
    @test occursin("line 400", tb["detail"])                  # …and the frames are all in the body
    @test occursin("Traceback (most recent call last):", tb["detail"])
    @test isempty(sink.buf)

    # a traceback cut off by the process dying still gets emitted rather than swallowed
    cut = Cecelia.ChildLineSink(LOG_SOURCE_PREVIEW)
    Cecelia.child_line_records!(cut, "Traceback (most recent call last):")
    Cecelia.child_line_records!(cut, "  File \"preview_worker.py\", line 1, in <module>")
    left = Cecelia.child_line_flush!(cut)
    @test length(left) == 1 && left[1]["level"] == "error"
    @test occursin("preview_worker.py", left[1]["detail"])

    # blank filler is not an event
    @test isempty(Cecelia.child_line_records!(Cecelia.ChildLineSink("x"), "   "))
end

@testset "LogRing stamps a monotonic seq so a dropped line is detectable" begin
    r = LogRing(3)
    a = log_ring_push!(r, log_record("info", "one"))
    b = log_ring_push!(r, log_record("warn", "two"))
    @test a["seq"] == 1 && b["seq"] == 2
    @test !isempty(a["ts"])
    @test log_ring_seq(r) == 2

    # `since` is what repairs a gap — the client asks for what it did not get, not for everything
    @test [x["message"] for x in log_ring_since(r, 1)] == ["two"]
    @test length(log_ring_since(r, 0)) == 2
    @test isempty(log_ring_since(r, 2))

    # the cap evicts, but the seq keeps counting — so eviction reads as a gap, not as a renumbering
    for i in 3:6; log_ring_push!(r, log_record("info", "n$i")); end
    @test length(log_ring_since(r, 0)) == 3
    @test log_ring_seq(r) == 6

    # a relayed record (the runner's) keeps the stamp from where it happened, and is RE-seq'd here so
    # the browser follows exactly one counter
    relayed = log_ring_push!(r, Dict{String,Any}("level" => "info", "message" => "from runner",
                                                 "source" => LOG_SOURCE_RUNNER, "ts" => "2026-01-01T00:00:00.000Z"))
    @test relayed["ts"]  == "2026-01-01T00:00:00.000Z"
    @test relayed["seq"] == 7
end

@testset "long-lived children are spawned onto the log rail, never swallowed" begin
    # `run(cmd; wait = false)` sends BOTH streams to devnull (spawn_opts_swallow), which is how the
    # preview worker's traceback.print_exc() reached nothing at all — not the console, not the
    # terminal. This is a convention test: the two remaining spawn sites must use `spawn_logged`,
    # because the failure it prevents is invisible by construction.
    src = dirname(pathof(Cecelia))
    for (file, what) in (("preview.jl", "preview worker"),)
        body = read(joinpath(src, file), String)
        @test occursin("spawn_logged(", body)
        @test !occursin(r"\.proc = run\(", body)          # the swallowing shape, gone
    end

    # The RUNNER is the deliberate exception and must stay one: it is `detach = true` so it outlives
    # the backend, and a pipe this process owns becomes a broken pipe on the restart it exists to
    # survive. It reports over its event stream instead — assert that carrier still exists, or the
    # exemption silently becomes the old silence again.
    runner_src = read(joinpath(src, "runner", "server.jl"), String)
    @test occursin("install_log_tee!", runner_src)
    @test occursin("runner:log", runner_src)
    @test occursin("/api/logs/recent", runner_src)

    # …and its OTHER carrier. Being exempt from `spawn_logged` is not a licence to go back to devnull:
    # the runner's raw stdout/stderr (a `println`, an unhandled `@spawn` task error, a segfault dump the
    # C runtime writes while the process dies) is not a log record and never reaches `runner:log`. It is
    # explicitly INHERITED instead, which is safe precisely because the fd belongs to the terminal
    # rather than to us — a detached child keeps writing to it long after we exit.
    client_src = read(joinpath(src, "runner", "client.jl"), String)
    @test occursin("detach = true", client_src)                    # the reason for the exemption
    @test occursin(r"stdout\s*=\s*stdout", client_src)             # …and not into a hole
    @test occursin(r"stderr\s*=\s*stderr", client_src)
    # …and NOT via spawn_logged: a pipe we read would break on the restart it exists to survive. The
    # trailing paren matters — the file discusses `spawn_logged` at length in the comment explaining
    # why it is wrong here, and a bare substring test would fail on the explanation.
    @test !occursin("spawn_logged(", client_src)
end

@testset "log sources agree across languages" begin
    # The console renders one filter chip per source, so the set is a CONTRACT with the frontend —
    # a source Julia can emit but the console has no chip for is a message that arrives and is
    # unreachable. Same shape as the protocol-version pairs in docs/ARCHITECTURE.md.
    ts = read(joinpath(_repo, "frontend", "src", "utils", "logFilter.ts"), String)
    for s in LOG_SOURCES
        @test occursin("'$s'", ts)
    end
    @test LOG_SOURCE_BACKEND ∉ CHILD_LOG_SOURCES     # the backend is never hidden by default
    @test length(LOG_SOURCES) == length(CHILD_LOG_SOURCES) + 1
end
