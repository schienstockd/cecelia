# API-layer tests. The package (`app/test/runtests.jl`) covers Cecelia headless; this covers the thin
# HTTP adapters in `api/src`. We load the server module WITHOUT binding a socket (`CECELIA_NO_SERVE`)
# and call the handler functions directly — no live server, no ports, so it runs in CI headless.
#
# Run: `julia --project=api api/test/runtests.jl`  (or `pixi run test-api`).
ENV["CECELIA_NO_SERVE"] = "1"

using Test
include(joinpath(@__DIR__, "..", "src", "server.jl"))   # defines handlers + shared state; does not start
using JSON3

# call a POST handler the way the router does: JSON body → Vector{UInt8}
_post(f, obj) = f(Vector{UInt8}(JSON3.write(obj)))
_repl(code) = _post(api_repl, Dict("code" => code))

@testset "API: diagnostics" begin
    st, body = api_diagnostics(HTTP.Request("GET", "/api/diagnostics"))
    @test st == 200
    d = JSON3.read(body)
    @test d.threads >= 1
    @test !isempty(String(d.julia))
    @test haskey(d, :replAvailable) && haskey(d, :loopback) && haskey(d, :replEnabled)
    # installed-build provenance (.cecelia-version at the install root); a source checkout has no
    # such file → the fallback string. Either way the field must be present and non-empty.
    @test haskey(d, :version) && !isempty(String(d.version))
end

@testset "API: packages" begin
    st, body = api_packages(HTTP.Request("GET", "/api/diagnostics/packages"))
    @test st == 200
    d = JSON3.read(body)
    @test haskey(d, :julia) && haskey(d, :python) && haskey(d, :pythonError)
    # Julia list is in-process (Pkg.dependencies) → always populated & well-formed; the server dep set
    # includes HTTP.
    @test !isempty(d.julia)
    @test all(p -> haskey(p, :name) && haskey(p, :version), d.julia)
    @test any(p -> p.name == "HTTP", d.julia)
    # Python list comes from `pixi list`; it's populated when pixi is on PATH (it is under
    # `pixi run test-api`) and otherwise reports pythonError rather than throwing.
    if d.pythonError === nothing
        @test !isempty(d.python)
        @test all(p -> haskey(p, :name) && haskey(p, :version) && haskey(p, :kind), d.python)
    end
end

@testset "API: debug console gating" begin
    # disabled (default) → refused
    _repl_on[] = false; _BOUND_HOST[] = "127.0.0.1"
    st, _ = _repl("1 + 1"); @test st == 403
    @test !_repl_available()

    # enabled but the server is network-bound → refused (loopback is the hard gate)
    _repl_on[] = true; _BOUND_HOST[] = "0.0.0.0"
    st, body = _repl("1 + 1")
    @test st == 403
    @test occursin("loopback", JSON3.read(body).error)
    @test !_repl_available()

    # enabled AND loopback-bound → available
    _BOUND_HOST[] = "127.0.0.1"
    @test _repl_available()
end

@testset "API: debug console eval" begin
    _repl_on[] = true; _BOUND_HOST[] = "127.0.0.1"

    # value
    st, body = _repl("1 + 1")
    r = JSON3.read(body)
    @test st == 200 && r.ok == true && r.value == "2"

    # captured stdout + last value from a multi-statement block
    r = JSON3.read(_repl("println(\"hi\"); 3 + 4")[2])
    @test r.value == "7" && occursin("hi", r.output)

    # error path: ok=false + message, still HTTP 200
    r = JSON3.read(_repl("sqrt(-1)")[2])
    @test r.ok == false && occursin("DomainError", r.error)

    # empty code → 400
    @test _repl("   ")[1] == 400
end

@testset "API: repl config toggle" begin
    _BOUND_HOST[] = "127.0.0.1"
    st, body = _post(api_repl_config, Dict("enabled" => false))
    @test st == 200 && JSON3.read(body).replEnabled == false
    @test _repl("1+1")[1] == 403                      # now disabled

    st, body = _post(api_repl_config, Dict("enabled" => true))
    @test st == 200 && JSON3.read(body).replEnabled == true
    @test _repl("1+1")[1] == 200                      # enabled again
end

@testset "API: notebooks registry + versioning" begin
    # Pure name sanitisation: reject path-like input + dotfiles, accept plain names.
    @test _safe_nb_file("../evil") === nothing
    @test _safe_nb_file("a/b")     === nothing
    @test _safe_nb_file("a\\b")    === nothing
    @test _safe_nb_file(".hidden") === nothing
    @test _safe_nb_file("my nb")   == "my nb.jl"
    @test _safe_nb_file("a.b.jl")  == "a.b.jl"

    # Redirect projects_dir() → a temp dir so we never touch the real dev projects dir.
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir()
    dirs["projects"] = tmp
    try
        uid = "TESTNB"
        mkpath(joinpath(tmp, uid))
        list()  = JSON3.read(api_notebooks_list(HTTP.Request("GET", "/api/notebooks?projectUid=$uid"))[2]).notebooks
        find(f) = (ns = filter(n -> n.file == f, list()); isempty(ns) ? nothing : ns[1])
        snaps() = JSON3.read(api_notebooks_snapshots(HTTP.Request("GET", "/api/notebooks/snapshots?projectUid=$uid&file=nb1.jl"))[2]).snapshots

        # create (+ duplicate-name 409, bad-name 400)
        @test _post(api_notebooks_create, Dict("projectUid"=>uid, "name"=>"nb1", "description"=>"first"))[1] == 200
        @test _post(api_notebooks_create, Dict("projectUid"=>uid, "name"=>"nb1"))[1] == 409
        @test _post(api_notebooks_create, Dict("projectUid"=>uid, "name"=>"../x"))[1] == 400
        nb = find("nb1.jl")
        @test nb !== nothing && nb.version == 0 && nb.description == "first"   # fresh → v0

        # snapshot advances the current-version pointer; number derived from disk
        @test JSON3.read(_post(api_notebooks_snapshot, Dict("projectUid"=>uid,"file"=>"nb1.jl"))[2]).version == 1
        @test JSON3.read(_post(api_notebooks_snapshot, Dict("projectUid"=>uid,"file"=>"nb1.jl"))[2]).version == 2
        @test find("nb1.jl").version == 2
        @test [s.version for s in snaps()] == [2, 1]

        # restore: pointer back to 1, no new snapshot, repeatable (no churn), bad version 404
        @test JSON3.read(_post(api_notebooks_restore, Dict("projectUid"=>uid,"file"=>"nb1.jl","version"=>1,"force"=>true))[2]).version == 1
        @test find("nb1.jl").version == 1
        @test _post(api_notebooks_restore, Dict("projectUid"=>uid,"file"=>"nb1.jl","version"=>1,"force"=>true))[1] == 200
        @test [s.version for s in snaps()] == [2, 1]
        @test _post(api_notebooks_restore, Dict("projectUid"=>uid,"file"=>"nb1.jl","version"=>99,"force"=>true))[1] == 404

        # next snapshot after restore = max-on-disk + 1 (→ 3, not "current+1")
        @test JSON3.read(_post(api_notebooks_snapshot, Dict("projectUid"=>uid,"file"=>"nb1.jl"))[2]).version == 3

        # describe + duplicate
        @test _post(api_notebooks_describe, Dict("projectUid"=>uid,"file"=>"nb1.jl","description"=>"updated"))[1] == 200
        @test find("nb1.jl").description == "updated"
        @test JSON3.read(_post(api_notebooks_duplicate, Dict("projectUid"=>uid,"file"=>"nb1.jl","scope"=>"project"))[2]).file == "nb1-copy.jl"
        @test find("nb1-copy.jl") !== nothing

        # delete (server not running in tests → guard doesn't require force)
        @test _post(api_notebooks_delete, Dict("projectUid"=>uid,"file"=>"nb1.jl"))[1] == 200
        @test find("nb1.jl") === nothing

        # errors
        @test api_notebooks_list(HTTP.Request("GET", "/api/notebooks?projectUid=NOPE"))[1] == 404
        @test api_notebooks_list(HTTP.Request("GET", "/api/notebooks"))[1] == 400
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: module-canvas persistence" begin
    # Redirect projects_dir() → temp so we don't touch the dev projects dir.
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    try
        uid = "TESTCANVAS"
        mkpath(joinpath(tmp, uid, "1", "IMG1"))   # the object (image) dir must exist
        write(joinpath(tmp, uid, "project.json"),
              JSON3.write((; uid = uid, name = "T", kind = "static", set_uids = String[])))
        entry = Dict("panels" => [], "activeId" => 0, "nextId" => 0, "arrangeSeq" => 0, "shared" => Dict())
        payload = Dict("projectUid" => uid, "objects" => Dict(
            "IMG1" => Dict("entries" => Dict("summary:behaviour:IMG1" => entry), "geom" => Dict())))
        # save writes 1/IMG1/moduleCanvases.json (with the object), verbatim
        @test _post(api_projects_canvases, payload)[1] == 200
        mc_file = joinpath(tmp, uid, "1", "IMG1", "moduleCanvases.json")
        @test isfile(mc_file)
        @test haskey(JSON3.read(read(mc_file, String)).entries, Symbol("summary:behaviour:IMG1"))
        # object dir absent → skipped (no crash, no stray file)
        @test _post(api_projects_canvases,
                    Dict("projectUid" => uid, "objects" => Dict("GHOST" => Dict("entries" => Dict(), "geom" => Dict()))))[1] == 200
        @test !isfile(joinpath(tmp, uid, "1", "GHOST", "moduleCanvases.json"))
        # load reassembles the per-object files into one keyed map
        st, body = api_projects_load(Vector{UInt8}(JSON3.write(Dict("uid" => uid))))
        @test st == 200
        mc = JSON3.read(body).moduleCanvases
        @test mc !== nothing && haskey(mc.entries, Symbol("summary:behaviour:IMG1"))
        # error paths
        @test _post(api_projects_canvases, Dict("objects" => Dict()))[1] == 400          # no projectUid
        @test _post(api_projects_canvases, Dict("projectUid" => "NOPE", "objects" => Dict()))[1] == 404
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end
