# ── Declared task-spec tests (`tests` block; docs/MODULES.md → *Declared tests*) ──────────────────
#
# A spec says how to check itself: a fixture by properties, params, assertions on the outputs. The
# first testset pins the evaluator on hand-built inputs whose answers follow from the definitions in
# `spec_assertion_observed`; the runner below executes every declared test — built-in specs, the
# shipped examples under docs/examples/, and a plugin author's own modules dir when
# `CECELIA_SPEC_TEST_MODULES` points at one (laid out like <config_dir>/modules).

_sa(type, kind; vn = "vn", value = nothing, delta = 0.0, min = nothing, max = nothing, column = nothing) =
    Cecelia.SpecAssertion(type, kind, vn, value, delta, min, max, column)

@testset "declared spec tests — schema + evaluator" begin
    # tolerance: value ± delta, [min, max], or both (Galaxy's `_assert_number` semantics)
    @test Cecelia.spec_assertion_check(_sa("n_labels", "cells"; value = 6.0), 6)[1]
    @test !Cecelia.spec_assertion_check(_sa("n_labels", "cells"; value = 6.0), 7)[1]
    @test Cecelia.spec_assertion_check(_sa("n_labels", "cells"; value = 6.0, delta = 1.0), 7)[1]
    @test !Cecelia.spec_assertion_check(_sa("n_labels", "cells"; value = 6.0, delta = 1.0), 8)[1]
    @test Cecelia.spec_assertion_check(_sa("mean_measure", "cells"; min = 0.0, max = 1.0, column = "c"), 1.0)[1]
    @test !Cecelia.spec_assertion_check(_sa("mean_measure", "cells"; min = 0.0, max = 1.0, column = "c"), 1.01)[1]
    @test !Cecelia.spec_assertion_check(_sa("mean_measure", "cells"; min = 0.0, column = "c"), -0.5)[1]
    @test !Cecelia.spec_assertion_check(_sa("mean_measure", "cells"; value = 0.5, delta = 0.5, max = 0.8, column = "c"), 0.9)[1]
    @test !Cecelia.spec_assertion_check(_sa("mean_measure", "cells"; min = 0.0, column = "c"), nothing)[1]
    @test !Cecelia.spec_assertion_check(_sa("mean_measure", "cells"; min = 0.0, column = "c"), NaN)[1]
    # a non-finite or non-numeric reading never passes, even a one-sided bound it would satisfy
    @test !Cecelia.spec_assertion_check(_sa("mean_measure", "cells"; min = 0.0, column = "c"), Inf)[1]
    @test !Cecelia.spec_assertion_check(_sa("mean_measure", "cells"; min = 0.0, column = "c"), "0.5")[1]
    # centre of mass: a Euclidean distance over the axes the expectation names
    com = _sa("center_of_mass", "cells"; value = Dict("x" => 0.0, "y" => 0.0), delta = 5.0)
    @test Cecelia.spec_assertion_check(com, Dict("x" => 3.0, "y" => 4.0, "z" => 99.0))[1]   # distance 5
    @test !Cecelia.spec_assertion_check(com, Dict("x" => 3.0, "y" => 4.1))[1]
    @test !Cecelia.spec_assertion_check(com, Dict("x" => 0.0))[1]                           # no y axis

    # shape: the store axis, 1 when the store has no such axis (a label store has no c)
    sizes = Dict("t" => 5, "c" => 3, "z" => 4, "y" => 16, "x" => 32)
    for (type, want) in ("width" => 32, "height" => 16, "channels" => 3, "depth" => 4, "frames" => 5)
        @test Cecelia.spec_assertion_observed(_sa(type, "image"), sizes) == want
    end
    @test Cecelia.spec_assertion_observed(_sa("channels", "labels"), Dict("y" => 2, "x" => 2)) == 1
    @test Cecelia.spec_assertion_observed(_sa("frames", "labels"), Dict("y" => 2, "x" => 2)) == 1
    @test Cecelia.spec_assertion_observed(_sa("width", "image"), Dict{String,Int}()) === nothing

    # cells: two objects of 10 and 30 px at x = 0 and x = 4 → mass centre 3 (not the plain mean 2);
    # mean size 20; a NaN measure is left out of the mean
    df = DataFrame(label = [1, 2], area = [10.0, 30.0], centroid_x = [0.0, 4.0], centroid_y = [1.0, 1.0],
                   CD4 = [100.0, 300.0], m = [NaN, 0.5])
    @test Cecelia.spec_assertion_observed(_sa("n_labels", "cells"), df) == 2
    @test Cecelia.spec_assertion_observed(_sa("mean_object_size", "cells"), df) == 20.0
    @test Cecelia.spec_assertion_observed(_sa("center_of_mass", "cells"), df) == Dict("x" => 3.0, "y" => 1.0)
    @test Cecelia.spec_assertion_observed(_sa("mean_intensity", "cells"; column = "CD4"), df) == 200.0
    @test Cecelia.spec_assertion_observed(_sa("mean_measure", "cells"; column = "m"), df) == 0.5
    @test Cecelia.spec_assertion_observed(_sa("mean_measure", "cells"; column = "nope"), df) === nothing
    # a centre of mass needs `area` (the weights): absent, there is nothing to measure — not an error
    @test Cecelia.spec_assertion_observed(_sa("center_of_mass", "cells"), DataFrame(label = [1], centroid_x = [1.0])) === nothing
    @test Cecelia.spec_assertion_observed(_sa("center_of_mass", "cells"), df[1:0, :]) === nothing

    # the store reader, on a committed label store whose layout its generator states
    # (python/cecelia/tests/make_contacts_mesh_fixture.py: Z, Y, X = 4, 16, 32, no channel axis)
    fx = fixture_for(Symbol("label-store"), Symbol("image-store"); technology = "synthetic")
    img = Cecelia._load_image(fx.dir)
    @test Cecelia.store_axis_sizes(Cecelia.img_labels_path(img, "seg")) == Dict("z" => 4, "y" => 16, "x" => 32)
    @test Cecelia.store_axis_sizes(Cecelia.img_filepath(img, "default")) == Dict("c" => 2, "z" => 4, "y" => 16, "x" => 32)
    # the level is the one `.zattrs` names, and a name that leaves the store falls back to "0": a copy of
    # that label store's own metadata with its level renamed, then pointed outside
    lab = Cecelia.img_labels_path(img, "seg"); td = mktempdir()
    renamed = joinpath(td, "renamed.zarr"); mkpath(joinpath(renamed, "lvl"))
    za = read(joinpath(lab, ".zattrs"), String)
    @test occursin("\"path\": \"0\"", za) || occursin("\"path\":\"0\"", za)
    write(joinpath(renamed, ".zattrs"), replace(za, r"\"path\":\s*\"0\"" => "\"path\": \"lvl\""))
    cp(joinpath(lab, ".zgroup"), joinpath(renamed, ".zgroup"))
    zarray = JSON3.read(read(joinpath(lab, "0", ".zarray"), String), Dict{String,Any})
    zarray["shape"] = [1, 2, 3]                       # differs from level 0, so a fallback would show
    write(joinpath(renamed, "lvl", ".zarray"), JSON3.write(zarray))
    @test Cecelia.store_axis_sizes(renamed) == Dict("z" => 1, "y" => 2, "x" => 3)
    escaped = joinpath(td, "escaped.zarr"); cp(renamed, escaped)
    write(joinpath(escaped, ".zattrs"), replace(za, r"\"path\":\s*\"0\"" => "\"path\": \"../renamed.zarr/lvl\""))
    mkpath(joinpath(escaped, "0")); cp(joinpath(lab, "0", ".zarray"), joinpath(escaped, "0", ".zarray"))
    @test Cecelia.store_axis_sizes(escaped) == Dict("z" => 4, "y" => 16, "x" => 32)
    @test Cecelia.evaluate_spec_assertion(img, _sa("channels", "labels"; vn = "seg", value = 1.0))[1]
    @test !Cecelia.evaluate_spec_assertion(img, _sa("width", "labels"; vn = "seg", value = 31.0))[1]
    @test !Cecelia.evaluate_spec_assertion(img, Cecelia.SpecAssertion("width", "image", "nope", 32.0, 0.0, nothing, nothing, nothing))[1]

    # the schema: what parses, and each refusal names its test
    ok = Dict("tests" => [Dict("name" => "t", "fixture" => Dict("properties" => ["cells"], "need" => "x"),
                                "params" => Dict("a" => 1),
                                "outputs" => [Dict("cells" => "seg", "assert" => [
                                    Dict("type" => "n_labels", "value" => 6),
                                    Dict("type" => "center_of_mass", "value" => Dict("x" => 1, "y" => 2), "delta" => 0.5)]),
                                              Dict("image" => "smoothed", "assert" => [Dict("type" => "width", "min" => 2)])])])
    ts = Cecelia.parse_spec_tests(ok)
    @test length(ts) == 1 && ts[1].properties == [:cells] && ts[1].need == "x" && ts[1].params["a"] == 1
    @test [(a.type, a.kind, a.value_name) for a in ts[1].assertions] ==
          [("n_labels", "cells", "seg"), ("center_of_mass", "cells", "seg"), ("width", "image", "smoothed")]
    @test ts[1].assertions[2].value == Dict("x" => 1.0, "y" => 2.0) && ts[1].assertions[2].delta == 0.5
    @test ts[1].assertions[3].min == 2.0 && ts[1].assertions[3].value === nothing
    @test isempty(Cecelia.parse_spec_tests(Dict("params" => [])))
    # the same spec as JSON3 reads it from disk (Symbol keys)
    @test length(Cecelia.parse_spec_tests(JSON3.read(JSON3.write(ok)))[1].assertions) == 3
    bad(f) = (s = JSON3.read(JSON3.write(ok), Dict{String,Any}); f(s["tests"][1]); s)
    asrt(t) = t["outputs"][1]["assert"][1]
    for (why, mutate) in [
            "unknown type"            => t -> (asrt(t)["type"] = "n_cells"),
            "wrong output kind"       => t -> (asrt(t)["type"] = "width"),
            "no tolerance"            => t -> delete!(asrt(t), "value"),
            "delta without value"     => t -> (delete!(asrt(t), "value"); asrt(t)["delta"] = 1; asrt(t)["min"] = 0),
            "negative delta"          => t -> (asrt(t)["delta"] = -1),
            "unknown assertion field" => t -> (asrt(t)["tol"] = 1),
            "unknown test field"      => t -> (t["expect"] = []),
            "no name"                 => t -> delete!(t, "name"),
            "no properties"           => t -> (t["fixture"]["properties"] = []),
            "two output kinds"        => t -> (t["outputs"][1]["image"] = "x"),
            "no outputs"              => t -> (t["outputs"] = []),
            "channel off mean_intensity" => t -> (asrt(t)["channel"] = "CD4"),
            "mean_intensity w/o channel" => t -> (asrt(t)["type"] = "mean_intensity"),
            "center_of_mass min/max"  => t -> (t["outputs"][1]["assert"][2]["min"] = 0),
            "center_of_mass bad axis" => t -> (t["outputs"][1]["assert"][2]["value"] = Dict("w" => 1))]
        @test_throws ArgumentError Cecelia.parse_spec_tests(bad(mutate))
    end
end

# ── The runner ────────────────────────────────────────────────────────────────

# Install the shipped examples (and the plugin author's own modules dir, if given) into a throwaway
# config dir, the way a user installs them; returns the dir.
function _spec_test_modules_dir()
    root = dirname(dirname(dirname(pathof(Cecelia))))
    cfg  = mktempdir(); mods = joinpath(cfg, "modules"); plug = joinpath(mods, Cecelia.PLUGINS_SUBDIR)
    mkpath(plug)
    # copy every entry of `src` into `dst` (a modules tree or its plugins/ dir), README aside
    put(src, dst) = isdir(src) && for e in readdir(src)
        e == "README.md" || e == Cecelia.PLUGINS_SUBDIR || cp(joinpath(src, e), joinpath(dst, e); force = true)
    end
    put(joinpath(root, "docs", "examples", "custom-modules"), mods)
    put(joinpath(root, "docs", "examples", "plugins"), plug)
    own = get(ENV, "CECELIA_SPEC_TEST_MODULES", "")
    if !isempty(own)
        isdir(own) || error("CECELIA_SPEC_TEST_MODULES=$own is not a directory")
        put(own, mods); put(joinpath(own, Cecelia.PLUGINS_SUBDIR), plug)
    end
    cfg
end

# Run one declared test: fixture → temp copy → run_task → every assertion.
function _run_declared_spec_test(fun::String, t::Cecelia.SpecTest)
    fx = fixture_for(t.properties...; technology = t.technology, need = t.need)
    if fx === nothing
        @test_skip "$fun — $(t.name): waits on test-data/NEEDS.toml → $(t.need)"
        return
    end
    fx.dir === nothing && error("$fun — $(t.name): fixture $(fx.uid) has no ccid.json to run a task on")
    # the task writes into the project: a fresh copy (a derived fixture already is one)
    root = mktempdir(); proj = joinpath(root, basename(fx.project)); cp(fx.project, proj)
    dir  = joinpath(proj, relpath(fx.dir, fx.project))
    conf = Cecelia.cecelia_conf()["dirs"]; old = conf["projects"]
    try
        conf["projects"] = root                    # in memory only — never set_projects_dir
        log = String[]
        task = Cecelia._task_from_fun_name(fun)
        Cecelia.task_scope(task) == "image" ||
            error("$fun — $(t.name): declared tests run image-scoped tasks only")
        img0 = Cecelia._load_image(dir)
        outs = unique((a.kind, a.value_name) for a in t.assertions)
        before = Dict(o => Cecelia.spec_output_stamp(Cecelia.spec_output_path(img0, o...)) for o in outs)
        res = run_task(task, img0, t.params; on_log = l -> push!(log, l))
        res === nothing && @error "declared test: $fun — $(t.name) on $(fx.uid) failed" log = join(log, "\n")
        @test res !== nothing
        res === nothing && return
        img = Cecelia._load_image(dir)
        # an assertion is about what THIS run wrote, never what the fixture already held
        for o in outs
            after = Cecelia.spec_output_stamp(Cecelia.spec_output_path(img, o...))
            written = Cecelia.spec_output_written(before[o], after)
            written || @error "declared test: $fun — $(t.name) on $(fx.uid): the run did not write $(o[1]) $(o[2])"
            @test written
        end
        for a in t.assertions
            ok, msg = Cecelia.evaluate_spec_assertion(img, a)
            ok || @error "declared test: $fun — $(t.name) on $(fx.uid): $msg"
            @test ok
        end
    finally
        conf["projects"] = old
    end
end

@testset "declared spec tests — run every one" begin
    declared(funs) = [(f, Cecelia.parse_spec_tests(something(Cecelia._task_spec(Cecelia._task_from_fun_name(f)), Dict())))
                      for f in sort(collect(funs))]
    builtin = filter(x -> !isempty(x[2]), declared(keys(Cecelia._fun_name_map())))
    # not vacuous: built-ins declare tests, and the plugin example carries one to copy
    @test length(builtin) >= 2
    for (f, ts) in builtin, t in ts
        @testset "$f — $(t.name)" begin _run_declared_spec_test(f, t) end
    end

    cfg = _spec_test_modules_dir()
    res = load_custom_modules!(; dev_dir = cfg)
    @test isempty(res.failed)
    funs = [e.fun_name for e in Cecelia.user_task_specs(; dev_dir = cfg)]
    try
        custom = filter(x -> !isempty(x[2]), declared(funs))
        @test "trackTools.cumulativeChange" in first.(custom)
        for (f, ts) in custom, t in ts
            @testset "$f — $(t.name)" begin _run_declared_spec_test(f, t) end
        end
    finally
        foreach(Cecelia._unregister_task!, funs)   # later testsets enumerate the registry
    end
end
