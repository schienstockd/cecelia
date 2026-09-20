# Misc read-side API testsets (briefing / storage / versions / fs / movie-range / funparams)
# — extracted from api/test/runtests.jl.
#
# Seven testsets covering the /api/observer/briefing, /api/repl-api, /api/storage,
# /api/versions/*, /api/fs/browse, /api/movies/parse-range and /api/tasks/funparams surface:
#  - `API: observer briefing`
#  - `API: repl api surface`
#  - `API: storage`
#  - `API: /api/versions/inventory + /api/versions/prune (VN P5)`
#  - `API: fs browser`
#  - `API: movie range parsing + name guard`
#  - `API: funparams by output name`
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

@testset "API: observer briefing" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    _b(t) = api_observer_briefing(HTTP.Request("GET", "/api/observer/briefing" * t))
    try
        proj = create_project!(name = "api-brief")
        s    = add_set!(proj; name = "set-A")
        img  = add_image!(s; name = "i1", meta = Dict{String,Any}("ori_path" => "/tmp/x.tif"))
        write_qc(img, "importImages.omezarr", "default", Dict{String,Any}[])   # suppress calibration fallback
        write_qc(img, "segment.measureLabels", "default",
                 [qc_finding("fail", "zero_cells", "No cells", "Segmentation produced 0 cells")])
        @test _b("")[1] == 400                                # projectUid missing
        @test _b("?projectUid=nope")[1] == 404
        st, body = _b("?projectUid=$(proj.uid)")
        @test st == 200
        d = JSON3.read(body)
        @test d.projectUid == proj.uid && d.imageCount == 1
        @test d.flagged[1].uid == img.uid && String(d.flagged[1].worst) == "fail"
        @test String(d.flagged[1].findings[1].short) == "No cells"
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: repl api surface" begin
    # Project-independent: the notebook/REPL data-access surface backing the get_repl_api MCP tool.
    st, body = api_repl_api(HTTP.Request("GET", "/api/repl/api"))
    @test st == 200
    d = JSON3.read(body)
    @test !isempty(d.api)
    names = Set(String(e.name) for e in d.api)
    @test "pop_df" in names && "label_props" in names && "load_project" in names
    @test all(e -> e.documented, d.api)                 # every listed accessor is documented
    # the cookbook rides along (dev checkout ships docs/REPL.md) and carries the write rules
    @test occursin("using Cecelia", d.doc)
    @test occursin("figures", d.doc) && occursin("CSV", d.doc)
end

@testset "API: storage" begin
    # summary requires projectUid
    st, body = api_storage_summary(HTTP.Request("GET", "/api/storage/summary"))
    @test st == 400 && haskey(JSON3.read(body), :error)

    # reclaim requires projectUid + a non-empty imageUids list (a stale/empty request is rejected,
    # never allowed to touch disk)
    st, _ = _post(api_storage_reclaim, Dict("projectUid" => ""))
    @test st == 400
    st, _ = _post(api_storage_reclaim, Dict("projectUid" => "p", "imageUids" => String[]))
    @test st == 400
end

# ── VN P5 — /api/versions/inventory + /api/versions/prune ─────────────────────
# Full plan: docs/todo/VN_VERSIONING_PLAN.md → P5. The route surface is validated at the routing
# level here (guards); the per-image behaviour is unit-tested in the pkg suite where the helpers
# live (`app/test/suite/image_model.jl` → "VN P5 — inner_versions_of + prune_inner_versions!").
@testset "API: /api/versions/inventory + /api/versions/prune (VN P5)" begin
    # inventory requires projectUid; a stale project → 500 (load_project throws)
    st, body = api_versions_inventory(HTTP.Request("GET", "/api/versions/inventory"))
    @test st == 400 && haskey(JSON3.read(body), :error)

    # prune requires projectUid, imageUid, valueName, non-empty versions[] — every gate rejects
    # before touching disk (a mis-shaped request must never proceed to the destructive path).
    st, _ = _post(api_versions_prune, Dict("projectUid" => ""))
    @test st == 400
    st, _ = _post(api_versions_prune, Dict("projectUid" => "p"))
    @test st == 400
    st, _ = _post(api_versions_prune, Dict("projectUid" => "p", "imageUid" => "i"))
    @test st == 400
    st, _ = _post(api_versions_prune,
        Dict("projectUid" => "p", "imageUid" => "i", "valueName" => "default"))
    @test st == 400
    st, _ = _post(api_versions_prune,
        Dict("projectUid" => "p", "imageUid" => "i", "valueName" => "default",
             "versions" => String[]))
    @test st == 400
    # A well-formed request against a non-existent project → 404 (checked BEFORE any file op)
    st, _ = _post(api_versions_prune,
        Dict("projectUid" => "does-not-exist", "imageUid" => "i", "valueName" => "default",
             "versions" => ["v1"]))
    @test st == 404
end

@testset "API: fs browser" begin
    tmp = mktempdir()
    mkdir(joinpath(tmp, "sub"))
    write(joinpath(tmp, "img.tif"), "x")
    write(joinpath(tmp, "notes.txt"), "y")

    st, body = api_fs_list(HTTP.Request("GET", "/api/fs/list?path=" * HTTP.URIs.escapeuri(tmp)))
    @test st == 200
    d = JSON3.read(body)
    @test String(d.current) == tmp
    @test String(d.parent)  == dirname(tmp)          # navigates UP out of tmp — NOT clamped to home
    ents = Dict(String(e.name) => e for e in d.entries)
    @test haskey(ents, "sub") && ents["sub"].isdir
    @test ents["img.tif"].isimage
    @test String(ents["img.tif"].path) == joinpath(tmp, "img.tif")   # absolute path
    @test !ents["notes.txt"].isimage
    @test any(s -> String(s.label) == "Home", d.shortcuts)

    # non-existent dir → 400 (not a 500)
    st2, _ = api_fs_list(HTTP.Request("GET", "/api/fs/list?path=" * HTTP.URIs.escapeuri(joinpath(tmp, "nope"))))
    @test st2 == 400
    rm(tmp; recursive=true)
end

@testset "API: movie range parsing + name guard" begin
    # _parse_range → inclusive (start, stop) clamped to the file, or nothing if unsatisfiable.
    @test _parse_range("bytes=0-99", 1000) == (0, 99)
    @test _parse_range("bytes=500-", 1000) == (500, 999)     # open-ended → to EOF
    @test _parse_range("bytes=0-", 1000)   == (0, 999)
    @test _parse_range("bytes=-100", 1000) == (900, 999)     # suffix: last 100 bytes
    @test _parse_range("bytes=990-100000", 1000) == (990, 999)  # end clamped to file
    @test _parse_range("", 1000)           === nothing       # no header
    @test _parse_range("bytes=1000-1100", 1000) === nothing  # start past EOF → unsatisfiable
    @test _parse_range("bytes=50-10", 1000) === nothing      # stop < start
    @test _parse_range("bytes=-0", 1000)   === nothing       # zero-length suffix
    @test _parse_range("bogus", 1000)      === nothing

    # _movie_plan → (status, start, stop, framing). The FRAMING is the load-bearing half: HTTP.jl
    # buffers the whole body of a `Content-Length` response and streams a chunked one, measured on one
    # file through both — +390 MB peak for a 210 MB file and +1022 MB for a 420 MB one with a
    # Content-Length (~2.4x the file, buffer plus copies) against a flat ~30 MB chunked or clamped, and
    # through the route itself: +506 MB before this, +24 MB after, on one 210 MB movie. So a
    # `:length` plan must never exceed MOVIE_RANGE_MAX, and the unclamped whole-file plan must be
    # `:chunked` — that pair is what stops memory tracking the file size.
    big = 200 * 1024 * 1024
    @test _movie_plan("bytes=0-", big)  == (206, 0, MOVIE_RANGE_MAX - 1, :length)   # the clamp
    @test _movie_plan("bytes=0-", 1000) == (206, 0, 999, :length)                   # under the cap: untouched
    @test _movie_plan("bytes=$(MOVIE_RANGE_MAX)-", big) ==
          (206, MOVIE_RANGE_MAX, 2 * MOVIE_RANGE_MAX - 1, :length)                  # the slice a player asks for next
    @test _movie_plan("bytes=0-99", big)  == (206, 0, 99, :length)                   # a bounded ask is honoured exactly
    @test _movie_plan("bytes=-100", 1000) == (206, 900, 999, :length)                # suffix form
    @test _movie_plan("", big)            == (200, 0, big - 1, :chunked)             # no Range → whole file, streamed
    @test _movie_plan("bogus", 1000)      == (200, 0, 999, :chunked)                 # unparseable → as if absent
    @test _movie_plan("bytes=1000-1100", 1000) == (200, 0, 999, :chunked)            # unsatisfiable → whole file (as before)
    @test _movie_plan("", 0) == (200, 0, -1, :chunked)   # a 0-byte render: n = 0, and write_http_body! frames it

    # the invariant, not just the examples: nothing that carries a Content-Length exceeds the ceiling
    for h in ("bytes=0-", "bytes=0-999999999", "bytes=-$big", "bytes=5-", "bytes=0-$(big - 1)")
        _, a, b, fr = _movie_plan(h, big)
        fr === :length && @test b - a + 1 <= MOVIE_RANGE_MAX
    end

    # _valid_movie_name accepts the sanitised names the recorders write; blocks traversal/other types.
    @test _valid_movie_name("myImage_animation.mp4")
    @test _valid_movie_name("A1_B2_x0f2Kd.mp4")
    @test !_valid_movie_name("../secret.mp4")
    @test !_valid_movie_name("movie.mp4/../../etc")
    @test !_valid_movie_name("note.txt")
    @test !_valid_movie_name("has space.mp4")
end

# ── Movie registry (settings/movies.json) ─────────────────────────────────────
# The registry DECORATES the movies dir; the directory listing is the truth
# (docs/todo/MOVIE_MANAGEMENT_PLAN.md Decision 1). What is worth pinning is the reconciliation, which
# is the part with a wrong answer available: an entry whose file is gone must disappear rather than
# render a row that plays nothing, and an entry older than its file must be flagged rather than offer
# a config that did not produce those bytes.
# Params are remembered PER OUTPUT NAME, so naming `Tcell` again brings back Tcell's settings instead
# of whatever ran last. `matched` is the load-bearing part of the response: it says the params came
# from a by-name record rather than a fallback, and the form only REPLACES what the user is looking at
# when it is true — otherwise switching to a brand-new name would stamp the previous run's params over
# edits the user had just made.
@testset "API: funparams by output name" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name="api-funparams")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="img")
        save!(img)
        fun = "segment.cellpose"

        get_fp(; vn = "", iu = img.uid, ius = "") = begin
            q = "projectUid=$(proj.uid)&fun=$(fun)&imageUid=$(iu)&setUid=$(s.uid)" *
                (isempty(ius) ? "" : "&imageUids=$(ius)") *
                (isempty(vn) ? "" : "&valueName=$(vn)")
            st, body = api_task_fun_params(HTTP.Request("GET", "/api/tasks/funparams?$q"))
            @test st == 200
            JSON3.read(body)
        end

        # nothing banked at all
        r = get_fp()
        @test r.params === nothing && r.matched == false

        write_module_fun_params!(img._dir, fun,
            Dict{String,Any}("outputValueName" => "Tcell", "cellDiameter" => 8); value_name = "Tcell")
        write_module_fun_params!(img._dir, fun,
            Dict{String,Any}("outputValueName" => "Neutrophil", "cellDiameter" => 15);
            value_name = "Neutrophil")

        # each name gets ITS params, and says so
        t = get_fp(vn = "Tcell")
        @test t.matched == true && t.params.cellDiameter == 8
        n = get_fp(vn = "Neutrophil")
        @test n.matched == true && n.params.cellDiameter == 15

        # a NEW name falls back to the last run — useful as a starting point, but `matched` is false so
        # the form knows not to overwrite anything with it
        m = get_fp(vn = "Macrophage")
        @test m.matched == false && m.params.cellDiameter == 15

        # no valueName at all behaves exactly as it did before this existed
        @test get_fp().matched == false && get_fp().params.cellDiameter == 15

        # by-name wins across BOTH levels before either flat blob: a set-level record for the name the
        # user is actually naming beats the image's record of some other run
        # both extra images added BEFORE anything is written to the SET dir: `add_image!` saves the
        # set from its in-memory object, which would overwrite a `funParams` blob written to that
        # file dir-based (that write is deliberately object-free — see `write_module_fun_params!`)
        img2 = add_image!(s; name="img2"); save!(img2)
        img3 = add_image!(s; name="img3"); save!(img3)
        write_module_fun_params!(img2._dir, fun,
            Dict{String,Any}("cellDiameter" => 99))                       # image flat only
        write_module_fun_params!(s._dir, fun,
            Dict{String,Any}("cellDiameter" => 7); value_name = "Tcell")  # set by-name
        r2 = get_fp(vn = "Tcell", iu = img2.uid)
        @test r2.matched == true && r2.params.cellDiameter == 7

        # A BATCH: several images selected, so there is no driving image and `imageUid` is empty. The
        # by-name answer still has to be found, because that is the normal way a segmentation is run —
        # it lives on the images (and, for names predating the record, only in their run logs), never
        # on the set. `imageUids` carries the whole selection for that question alone.
        write_module_fun_params!(img3._dir, fun,
            Dict{String,Any}("outputValueName" => "Bcell", "cellDiameter" => 21); value_name = "Bcell")
        b = get_fp(vn = "Bcell", iu = "", ius = "$(img2.uid),$(img3.uid)")
        @test b.matched == true && b.params.cellDiameter == 21
        # …and it does not become a second way to answer the flat blob: with no name, the resolution is
        # image → set exactly as before, which for a batch means the set
        @test get_fp(iu = "", ius = "$(img2.uid),$(img3.uid)").params.cellDiameter == 7

        @test api_task_fun_params(HTTP.Request("GET", "/api/tasks/funparams?fun=$fun"))[1] == 400
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive=true, force=true)
    end
end
