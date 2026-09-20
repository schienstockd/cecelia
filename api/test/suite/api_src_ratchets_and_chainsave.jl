# api-source ratchets + chain-save testsets — extracted from api/test/runtests.jl.
#
# Three testsets that read api/src/*.jl at test time:
#  - `API: response bodies go through write_http_body!` — every HTTP.Response body callsite
#    is routed through the utility (call-site ratchet on the empty-body bug).
#  - `API: no bare nrow in api/src (DataFrames is not imported there)` — a bare `nrow`
#    compiles and dies at runtime; the api/ package is deliberately DataFrames-free.
#  - `API: chain save validates and repairs the start dot` (start-dot repair semantics).
#
# Two path expressions rewritten to use API_TEST_DIR. Extracted so runtests.jl contains
# only include lines + section-header comments — same shape as app/test/suite/*.jl.

# Every response body this server writes goes through `write_http_body!` (Cecelia/utils.jl), because
# an EMPTY one written bare corrupts the connection: no Content-Length → chunked framing → a
# zero-length `write` emits the terminating `0\r\n\r\n` and `closewrite` emits a second, so the NEXT
# response on that keep-alive connection is parsed starting at 4 stray bytes. One legitimately-empty
# gating plot (a track-grained plot of an untracked segmentation) took every other plot on the page
# down with it. The wire-level proof lives in the package suite ("an empty response body is written
# through write_http_body!"); this is the ratchet on the call sites.
@testset "API: response bodies go through write_http_body!" begin
    src = read(joinpath(API_TEST_DIR, "..", "src", "server.jl"), String)
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
    src = joinpath(API_TEST_DIR, "..", "src")
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
