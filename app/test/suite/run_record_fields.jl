# ── Run record fields: the commit, a plugin task's source, and the methods a task cites ────────────
# docs/todo/GALAXY_LESSONS_PLAN.md (L0); app/src/run_log.jl → *What made it*.
# Reuses `_rl_prov_fixture` / `_rl_find` from run_log_provenance.jl (included before this file).

@testset "cecelia_commit reads a dev install's provenance line and stays blank for a release" begin
    sha = "0123456789abcdef0123456789abcdef01234567"
    rel = mktempdir(); write(joinpath(rel, "VERSION"), "0.1.9\n"); write(joinpath(rel, ".cecelia-version"), "0.1.9\n")
    @test Cecelia._read_commit(rel) == ""                              # a release: the tag names the code
    flip = mktempdir(); write(joinpath(flip, "VERSION"), "dev\n")      # a dev-channel apply writes VERSION = dev
    write(joinpath(flip, ".cecelia-version"), "dev @ main $sha\n")
    @test Cecelia._read_commit(flip) == sha
    back = mktempdir(); write(joinpath(back, "VERSION"), "v0.2.0\n")   # dev → stable leaves the old marker
    write(joinpath(back, ".cecelia-version"), "dev @ main $sha\n")
    @test Cecelia._read_commit(back) == ""
    dev = mktempdir(); write(joinpath(dev, ".cecelia-version"), "dev @ feat/x $sha\n")
    @test Cecelia._read_commit(dev) == sha
    odd = mktempdir(); write(joinpath(odd, ".cecelia-version"), "dev @ main unknown\n")   # install.sh's fallback
    @test Cecelia._read_commit(odd) == ""
    @test Cecelia._read_commit(mktempdir()) == ""                      # no marker, no git: nothing, no throw
    # this checkout: a full SHA (CI and dev both run from a git checkout)
    @test occursin(r"^[0-9a-f]{40}$", Cecelia._read_commit(dirname(pkgdir(Cecelia))))
end

@testset "A run-log entry names the commit and, for a plugin task, the plugin version and ref" begin
    # MAINT-EXEMPT: run-log plumbing; no data property of the image can change what is stamped
    have_fixture(fixture_path("testpr")) || return @test_skip "testpr fixture missing"
    cfg  = mktempdir()
    pdir = joinpath(cfg, "modules", "plugins", "p-cite")
    mkpath(joinpath(pdir, "tracking"))
    write(joinpath(pdir, "plugin.json"), JSON3.write(Dict("name" => "p-cite", "version" => "1.2.3")))
    write(Cecelia.install_record_path(pdir), JSON3.write(Dict("url" => "https://github.com/x/p-cite", "ref" => "v1.2.3")))
    spec = joinpath(pdir, "tracking", "t.json")
    write(spec, JSON3.write(Dict("fun_name" => "x", "label" => "x", "resource_pool" => "cpu",
                                 "scope" => "image", "params" => [])))
    pfn = "plugCite.task$(rand(1000:9999))"; ufn = "userCite.task$(rand(1000:9999))"
    function _as(fn, path, tier, plugin)
        Cecelia._LOADING_SOURCE[] = (; path, tier, plugin)
        try register_task!(fn, _TestCustomTask(); spec = spec) finally Cecelia._LOADING_SOURCE[] = nothing end
    end
    _as(pfn, joinpath(pdir, "tracking", "t.jl"), Cecelia.TIER_PLUGIN, "p-cite")
    _as(ufn, joinpath(cfg, "modules", "tracking", "mine.jl"), Cecelia.TIER_USER, nothing)

    @test Cecelia.task_source_stamp(pfn) ==
          Dict{String,Any}("tier" => "plugin", "plugin" => "p-cite", "version" => "1.2.3", "ref" => "v1.2.3")
    @test Cecelia.task_source_stamp(ufn) == Dict{String,Any}("tier" => "hand-dropped", "file" => "mine.jl")
    @test Cecelia.task_source_stamp("importImages.remove") === nothing    # a built-in: version + commit name it

    dir = _rl_prov_fixture()
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    old_commit = Cecelia._CECELIA_COMMIT[]
    sha = "0123456789abcdef0123456789abcdef01234567"
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir   # in memory only — never set_projects_dir
        Cecelia._CECELIA_COMMIT[] = sha
        img = init_object("testpr", "KDIeEm")
        Cecelia.open_run_log!(img, pfn, "", Dict{String,Any}(); task_id = "cite-p")
        Cecelia.open_run_log!(img, "importImages.remove", "", Dict{String,Any}(); task_id = "cite-b")
        ep = _rl_find(img, "cite-p"); eb = _rl_find(img, "cite-b")
        @test ep["ceceliaCommit"] == sha && eb["ceceliaCommit"] == sha
        @test ep["source"]["plugin"] == "p-cite" && ep["source"]["version"] == "1.2.3"
        @test !haskey(eb, "source")
        Cecelia._CECELIA_COMMIT[] = ""                      # a release: no commit field, not an empty one
        Cecelia.open_run_log!(img, "importImages.remove", "", Dict{String,Any}(); task_id = "cite-r")
        @test !haskey(_rl_find(img, "cite-r"), "ceceliaCommit")
    finally
        Cecelia._CECELIA_COMMIT[] = old_commit
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
        Cecelia._unregister_task!(pfn); Cecelia._unregister_task!(ufn)
    end
end

@testset "task_citations keeps bare DOIs only, and every built-in citation is one" begin
    @test Cecelia.task_citations(Dict{String,Any}("citations" => ["10.1038/s41592-020-01018-x", " 10.7554/eLife.35800 "])) ==
          ["10.1038/s41592-020-01018-x", "10.7554/eLife.35800"]
    @test Cecelia.task_citations(Dict{String,Any}("citations" => ["https://doi.org/10.1/x", "cellpose", 3])) == String[]
    @test Cecelia.task_citations(Dict{String,Any}()) == String[]
    # a method only one setting runs is cited only when the params hold that setting
    cond = Dict{String,Any}("citations" => Any["10.1016/j.celrep.2020.107523",
        Dict{String,Any}("doi" => "10.1038/s41598-019-41695-z", "showIf" => Dict{String,Any}("clusterMethod" => "leiden"))])
    @test Cecelia.task_citations(cond, Dict{String,Any}("clusterMethod" => "leiden")) ==
          ["10.1016/j.celrep.2020.107523", "10.1038/s41598-019-41695-z"]
    @test Cecelia.task_citations(cond, Dict{String,Any}("clusterMethod" => "kmeans")) == ["10.1016/j.celrep.2020.107523"]
    @test Cecelia.task_citations(cond) == ["10.1016/j.celrep.2020.107523"]          # unknown params: not guessed
    # a run whose recorded params omit the setting ran the spec default
    withdflt = merge(cond, Dict{String,Any}("params" => Any[Dict{String,Any}("key" => "clusterMethod", "default" => "leiden")]))
    @test "10.1038/s41598-019-41695-z" in Cecelia.task_citations(withdflt, Dict{String,Any}())
    # the shipped region-clustering spec, as loaded: Leiden only for a Leiden run
    reg = _task_from_fun_name("clustRegions.cluster")
    @test "10.1038/s41598-019-41695-z" in Cecelia.task_citations(reg, Dict{String,Any}("clusterMethod" => "leiden"))
    @test Cecelia.task_citations(reg, Dict{String,Any}("clusterMethod" => "kmeans")) == ["10.1016/j.celrep.2020.107523"]
    # a composite cites its steps: segment + measure names Cellpose, track + measures names btrack
    @test "10.1038/s41592-020-01018-x" in Cecelia.task_citations(_task_from_fun_name("segment.cellposeMeasure"))
    @test "10.3389/fcomp.2021.734559" in Cecelia.task_citations(_task_from_fun_name("tracking.bayesian_track_measures"))
    @test Cecelia.task_citations(nothing) == String[]
    # a typo in a built-in spec would silently drop its citation from every Methods table
    n = 0
    for (d, _, fs) in walkdir(joinpath(pkgdir(Cecelia), "src", "tasks")), f in fs
        endswith(f, ".json") || continue
        raw = JSON3.read(read(joinpath(d, f), String))
        raw isa AbstractDict || continue                  # a param fragment is an array
        spec = Dict{String,Any}(String(k) => v for (k, v) in raw)
        c = get(spec, "citations", nothing)
        c === nothing && continue
        n += 1
        # every entry parses: a bare DOI, or a conditional one whose `when` the spec's own params satisfy
        show_all = Dict{String,Any}(String(k) => (v isa AbstractVector ? first(v) : v)
                                    for e in c if e isa AbstractDict for (k, v) in e["showIf"])
        @test length(Cecelia.task_citations(spec, show_all)) == length(c)
    end
    @test n >= 10
end
