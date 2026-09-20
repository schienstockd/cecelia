# Notebooks registry + write testsets — extracted from api/test/runtests.jl.
#
# Two testsets covering the Notebooks Playground API surface:
#  - `API: notebooks registry + versioning` — _safe_nb_file sanitisation, list / snapshot
#    round-trip, versioning + retention, delete flow.
#  - `API: notebooks write (generate from cells)` — write route builds the .jl body from
#    cells payload and broadcasts a nb:updated WS frame.
#
# No path expressions to rewrite (this extract does not reach for files outside the API).
# Extracted so runtests.jl contains only include lines + section-header comments — same
# shape as app/test/suite/*.jl. Helpers defined earlier (_post) stay in scope (lexical).

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

        # revise: SNAPSHOTS the current notebook (freezes it) then overwrites its cells — a real new
        # version, not a "-v2" copy. 409 if the file is absent; 400 without cells.
        let before = length(snaps())
            r = JSON3.read(_post(api_notebooks_revise, Dict("projectUid"=>uid, "file"=>"nb1.jl",
                                  "cells"=>["using Cecelia", "df = 1 + 1"], "description"=>"revised"))[2])
            @test r.ok == true
            @test length(snaps()) == before + 1                 # pre-revision state was frozen as a version
            @test find("nb1.jl").description == "revised"
        end
        @test _post(api_notebooks_revise, Dict("projectUid"=>uid, "file"=>"nope.jl", "cells"=>["x"]))[1] == 409  # must exist
        @test _post(api_notebooks_revise, Dict("projectUid"=>uid, "file"=>"nb1.jl"))[1] == 400                   # cells required

        # description cap: a long blurb is truncated at _NB_DESC_MAX (create/describe/write/revise all cap)
        @test _post(api_notebooks_create, Dict("projectUid"=>uid, "name"=>"nbcap", "description"=>repeat("x", 300)))[1] == 200
        @test length(find("nbcap.jl").description) == _NB_DESC_MAX

        # prune: keep ONLY the current version's snapshot, drop the older ones; description is untouched.
        # State here: several snapshots on disk, current pointer set by the revise above.
        let cur = find("nb1.jl").version
            @test cur > 1 && length(snaps()) > 1              # precondition: history to prune
            @test _post(api_notebooks_describe, Dict("projectUid"=>uid,"file"=>"nb1.jl","description"=>"keep me"))[1] == 200
            pr = JSON3.read(_post(api_notebooks_prune, Dict("projectUid"=>uid,"file"=>"nb1.jl"))[2])
            @test pr.ok == true && pr.kept == cur
            @test [s.version for s in snaps()] == [cur]       # only the current version survives
            @test find("nb1.jl").version == cur               # pointer unchanged
            @test find("nb1.jl").description == "keep me"      # description NOT pruned
            # idempotent: pruning again removes nothing (single snapshot = the current one)
            @test JSON3.read(_post(api_notebooks_prune, Dict("projectUid"=>uid,"file"=>"nb1.jl"))[2]).removed |> length == 0
        end
        # prune with no current version (never snapshotted) aborts rather than wiping — 409, history intact
        @test _post(api_notebooks_create, Dict("projectUid"=>uid, "name"=>"nbfresh"))[1] == 200
        @test _post(api_notebooks_prune, Dict("projectUid"=>uid,"file"=>"nbfresh.jl"))[1] == 409

        # delete — pass force=true so this is deterministic regardless of whether a Pluto server is
        # running locally. The guard 409s on a live server without force (a dev machine with the
        # notebook server up would otherwise fail this + the two asserts below); force is what the
        # UI's confirm supplies anyway.
        @test !isempty(snaps())    # nb1 has snapshots on disk before delete
        @test _post(api_notebooks_delete, Dict("projectUid"=>uid,"file"=>"nb1.jl","force"=>true))[1] == 200
        @test find("nb1.jl") === nothing
        @test isempty(snaps())     # delete also removes the notebook's snapshot history

        # errors
        @test api_notebooks_list(HTTP.Request("GET", "/api/notebooks?projectUid=NOPE"))[1] == 404
        @test api_notebooks_list(HTTP.Request("GET", "/api/notebooks"))[1] == 400
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: notebooks write (generate from cells)" begin
    # Pure serialiser: cells → valid Pluto source (activation prepended, markers + Cell order block).
    src = _pluto_notebook_source(["using Cecelia", "df = pop_df(img, \"flow\", [\"/T\"])"])
    @test occursin("### A Pluto.jl notebook ###", src)
    @test occursin("Pkg.activate", src)                       # env-activation cell prepended
    @test occursin("df = pop_df(img", src)                    # caller cell present, verbatim
    @test occursin("# ╔═╡ Cell order:", src)
    @test count("# ╠═", src) == 3                             # activation + 2 caller cells, listed once each

    # Pluto is one-expression-per-cell: a multi-statement cell must be wrapped in begin…end so it
    # loads (otherwise "Multiple expressions in one cell"). Single expressions stay bare.
    @test _wrap_multi_expr("df = 1") == "df = 1"                          # single expr → untouched
    @test _wrap_multi_expr("# just a comment") == "# just a comment"     # no expr → untouched
    @test !occursin("begin", _wrap_multi_expr("plot(df.x, df.y)"))       # single call → bare
    multi = _wrap_multi_expr("a = 1\nb = 2\na + b")
    @test startswith(multi, "begin\n") && endswith(multi, "\nend")       # multi-statement → wrapped
    @test occursin("a = 1\nb = 2\na + b", multi)                         # body preserved verbatim
    @test _wrap_multi_expr("x = (\n  1 + 2)") == "x = (\n  1 + 2)"       # one expr spanning lines → bare
    # end-to-end through the serialiser: a multi-statement caller cell comes out wrapped
    @test occursin("begin\nusing Cecelia\ndf = pop_df", _pluto_notebook_source(["using Cecelia\ndf = pop_df(img, \"flow\", [\"/T\"])"]))

    # Cell-id preservation across a revise (so Pluto's auto_reload can update an OPEN notebook in place).
    ids_of(src) = (p = tempname() * ".jl"; write(p, src); v = _content_cell_ids(p); rm(p; force = true); v)
    # A fresh serialise → the activation id is pinned + excluded; content cells get distinct v4 uuids.
    src1 = _pluto_notebook_source(["using Cecelia", "df = 1", "plot(df)"])
    ids1 = ids_of(src1)
    @test length(ids1) == 3 && allunique(ids1) && _NB_ACTIVATION_ID ∉ ids1
    # Re-serialising WITH the prior ids reuses them positionally → the same ids come back out.
    src2 = _pluto_notebook_source(["using Cecelia", "df = 2", "plot(df)"]; reuse_ids = ids1)
    @test ids_of(src2) == ids1                                # ids stable ⇒ auto_reload matches cells
    @test occursin("df = 2", src2)                            # …but the code did change
    # Extra cells beyond the reused set get fresh ids; the reused prefix stays put.
    src3 = _pluto_notebook_source(["using Cecelia", "df = 3", "plot(df)", "extra = 4"]; reuse_ids = ids1)
    ids3 = ids_of(src3)
    @test ids3[1:3] == ids1 && length(ids3) == 4 && ids3[4] ∉ ids1

    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    w(b) = _post(api_notebooks_write, b)
    # capture WS frames so we can assert the create nudges an open Notebooks page to refresh
    cap = Channel{String}(64); key = gensym("test-nbwrite")
    lock(_ws_clients_lock) do; _ws_clients[key] = cap; end
    drain() = (fs = []; while isready(cap); push!(fs, JSON3.read(take!(cap))); end; fs)
    try
        uid = "TESTNBW"; mkpath(joinpath(tmp, uid))
        @test w(Dict("projectUid"=>uid, "name"=>"gen"))[1] == 400              # cells required
        @test w(Dict("projectUid"=>"NOPE", "name"=>"gen", "cells"=>["x=1"]))[1] == 404
        drain()   # clear any frames from the failing calls above
        st, body = w(Dict("projectUid"=>uid, "name"=>"gen",
                          "cells"=>["using Cecelia", "df = 1"], "description"=>"speed over time"))
        @test st == 200 && JSON3.read(body).file == "gen.jl"
        # a notebooks_changed frame for this project → an open Notebooks page auto-refreshes
        @test any(f -> String(get(f, :type, "")) == "notebooks_changed" &&
                       String(get(f, :projectUid, "")) == uid, drain())
        dest = joinpath(tmp, uid, "notebooks", "gen.jl")
        @test isfile(dest)
        content = read(dest, String)
        @test occursin("Pkg.activate", content) && occursin("df = 1", content)   # runnable + caller code
        # registered + snapshotted v1 (an immediate restore point)
        nb = JSON3.read(api_notebooks_list(HTTP.Request("GET", "/api/notebooks?projectUid=$uid"))[2]).notebooks
        g  = nb[findfirst(n -> n.file == "gen.jl", nb)]
        @test g.version == 1 && g.description == "speed over time"
        # create-only: never clobbers
        @test w(Dict("projectUid"=>uid, "name"=>"gen", "cells"=>["y=2"]))[1] == 409

        # content read (the "have a look" flow): returns the notebook's current source
        cget(q) = api_notebooks_content(HTTP.Request("GET", "/api/notebooks/content?$q"))
        st2, cbody = cget("projectUid=$uid&file=gen.jl")
        @test st2 == 200
        cd = JSON3.read(cbody)
        @test cd.file == "gen.jl" && cd.scope == "project" && occursin("df = 1", cd.content)
        @test cget("projectUid=$uid&file=nope.jl")[1] == 404      # missing notebook
        @test cget("projectUid=$uid")[1] == 400                    # file required
        @test cget("projectUid=NOPE&file=gen.jl")[1] == 404        # missing project
        @test cget("projectUid=$uid&file=../secret")[1] == 400     # path traversal rejected
    finally
        lock(_ws_clients_lock) do; delete!(_ws_clients, key); end
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end
