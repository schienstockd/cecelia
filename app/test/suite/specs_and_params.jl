# ── Task spec + repeatable-group + param-defaults testsets ────────────
# Six sections covering: every slider (int/float param) can reach its own max and default
# (range-input reachable-values fix), a second model group is not born a copy of the first
# (entryDefaults[2] plumbing), coastal forwards every top-level spec param to its runner,
# an int param never declares a fractional step (widget-vs-value type contract),
# a repeatable group's run order is resolved into the group (group flatten), and a group's
# two sets of defaults agree (spec-defaults vs entryDefaults[1] parity). Extracted from
# suite.jl to keep it small enough to merge without EOF conflicts on every append. The
# extracted file loads inside this file's aggregating testset scope, so any helpers defined
# earlier in suite.jl are still in scope (lexical include).
#
# Three `joinpath(@__DIR__, "..", "src", "tasks", "segment", "coastal.*")` scans (the two
# coastal-forwarding testsets read coastal.jl + coastal.json) are rerouted through
# pathof(Cecelia) so they resolve identically whether the file sits at app/test/ or
# app/test/suite/.

_app_src = joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "app", "src")

@testset "every slider can reach its own max and default" begin
    # An `int`/`float` param renders as `<input type="range">` (ParamRenderer.vue), and a range input
    # anchors its stops at `min` — the reachable values are `min + k*step`, NOT the round numbers the
    # three bounds suggest. So `min 1, max 500, step 5` (what `epochs` shipped as) offers 1, 6, 11 …
    # 496: 100 is not selectable, 101 is, the declared max is unreachable, and the declared default of
    # 30 is not even on a stop — the form silently disagrees with its own spec the moment the user
    # touches the control.
    #
    # Only the two unambiguous failures are asserted. That the stops are round *numbers* is taste and
    # sometimes wrong on purpose: `smooth.temporalFrames` is 1–9 step 2 precisely so the window stays
    # odd, and its max and default are both reachable.
    on_grid(v, mn, st) = (n = (v - mn) / st; isapprox(n, round(n); atol = 1e-6))
    checked = 0
    for (fun_name, task) in sort(collect(Cecelia._fun_name_map()); by = first)
        path = try Cecelia._spec_path(task) catch; nothing end
        (isnothing(path) && continue)
        isfile(path) || continue
        each_spec_param(get(JSON3.read(read(path, String)), :params, [])) do p, _
            t = String(something(spec_get(p, "type", ""), ""))
            (t == "int" || t == "float") || return
            key = String(something(spec_get(p, "key", ""), ""))
            mn = Float64(something(spec_get(p, "min", 0), 0))
            mx = Float64(something(spec_get(p, "max", 100), 100))
            st = Float64(something(spec_get(p, "step", t == "int" ? 1 : 0.01), 1))
            st > 0 || return
            checked += 1
            on_grid(mx, mn, st) ||
                @error "slider max is unreachable" task = fun_name param = key min = mn max = mx step = st
            @test on_grid(mx, mn, st)
            dflt = spec_get(p, "default", nothing)
            if dflt isa Real
                on_grid(Float64(dflt), mn, st) ||
                    @error "slider default is not on a stop" task = fun_name param = key default = dflt min = mn step = st
                @test on_grid(Float64(dflt), mn, st)
            end
        end
    end
    @test checked > 20      # the sweep actually found the sliders
end

@testset "a second model group is not born a copy of the first" begin
    # Entries of a `repeatable` group are applied IN RUN ORDER and each fills only the pixels an
    # earlier one left (`fill_unlabelled`), so entry 2 is a FRAGMENT pass over what the cell pass did
    # not claim. Born identical it grows to nearly the same regions, is clipped along the first pass's
    # boundaries, and leaves slivers at twice the compute — measured on zolIMa/fXgbTl, where both
    # passes carried `affinityThreshold` 0.5 and the run took 508 s to produce that.
    #
    # `entryDefaults[2]` is what the form seeds a second entry with. This pins that it actually
    # DIFFERS on the parameters that decide how far a pass grows: identical values here would make the
    # whole feature a no-op and nothing else would notice.
    spec = JSON3.read(read(joinpath(_app_src, "tasks", "segment", "coastal.json"),
                           String))
    models = nothing
    for p in get(spec, :params, [])
        String(get(p, :key, "")) == "models" && (models = p)
    end
    @test !isnothing(models)
    seeds = get(models, :entryDefaults, nothing)
    @test !isnothing(seeds)
    @test length(seeds) >= 2
    second = seeds[2]

    # The sub-param defaults, i.e. what entry 1 starts as.
    firsts = Dict{String,Any}()
    function collect_leaves!(ps)
        for p in ps
            if String(get(p, :type, "")) == "section"
                collect_leaves!(get(p, :params, []))
            else
                haskey(p, :default) && (firsts[String(p[:key])] = p[:default])
            end
        end
    end
    collect_leaves!(get(models, :params, []))

    # These three decide how far a pass grows and how readily its fragments re-merge. If the second
    # pass matches the first on them, it is the first pass again.
    for k in ("affinityThreshold", "seedSize", "seedBlurSigma")
        @test haskey(second, Symbol(k))
        @test second[Symbol(k)] != firsts[k]
    end
    # Directions, not just difference: the fragment pass grows LESS freely and blurs its seeds LESS.
    @test second[:affinityThreshold] > firsts["affinityThreshold"]
    @test second[:seedSize] < firsts["seedSize"]
    @test second[:seedBlurSigma] < firsts["seedBlurSigma"]
    # Every seeded key must be one the group declares, or it reaches the runner as an unknown param.
    for k in keys(second)
        @test haskey(firsts, String(k))
    end
end

@testset "coastal forwards every top-level spec param to its runner" begin
    # `coastal.jl` hands `run_py` an explicit NamedTuple, i.e. a WHITELIST — while `preview_params`
    # forwards the whole param bag. So a spec param missing from that list is honoured in the preview
    # and silently ignored by the run, which is the same class of divergence as the group-order bug
    # and just as quiet. Checked by reading the handler source: the param names are literals there.
    #
    # `valueName`/`outputValueName` are excluded because the handler TRANSFORMS them (into `imPath`
    # and the label store name) rather than forwarding them, and `models` is rebuilt by
    # `coastal_models_for_python`. Sections are excluded here and covered by the flatten tests —
    # their sub-params appear individually.
    raw  = read(joinpath(_app_src, "tasks", "segment", "coastal.jl"), String)
    # COMMENTS STRIPPED, and the key matched QUOTED — i.e. as `get(params, "key", …)` spells it.
    # A bare `occursin(key, raw)` passes on a param that was deleted and left described in a comment,
    # which is exactly the state this is meant to catch.
    src  = join((replace(l, r"#.*$" => "") for l in split(raw, '\n')), "\n")
    spec = JSON3.read(read(joinpath(_app_src, "tasks", "segment", "coastal.json"),
                           String))
    transformed = ["valueName", "outputValueName", "models"]
    checked = 0
    for p in get(spec, :params, [])
        key = String(get(p, :key, ""))
        String(get(p, :type, "")) in ("section", "group") && continue
        key in transformed && continue
        isempty(key) && continue
        checked += 1
        found = occursin("\"" * key * "\"", src)
        found || @error "a coastal spec param never reaches coastal_run.py" param = key
        @test found
    end
    @test checked >= 2      # the sweep found the top-level params, not an empty list
end

@testset "an int param never declares a fractional step" begin
    # `ParamRenderer.vue` runs `parseInt` on an `int` slider's value, so a fractional step makes half
    # the stops DEAD: the control moves and the value does not. Found live on `segment.coastal`, where
    # the two params carrying a PHYSICAL unit were the ones affected — `seedSize` (µm, step 0.5) and
    # `minComponentSize` (µm², step 0.5). At 0.33 µm/px that put coastal's own tuned pass-1 seed window
    # (14 px = 4.6 µm) and pass-2 size floor (6 px = 0.66 µm²) out of reach entirely, so a two-pass
    # config could not be given the values the two passes are supposed to differ by.
    #
    # The fix is the type, not the step: a µm value is continuous and the conversion to pixels rounds
    # at the ONE boundary that owns it (`px_from_um` / `px_area_from_um2`). An int with step 1 is fine
    # and common — this only catches the contradiction.
    checked = 0
    for (fun_name, task) in sort(collect(Cecelia._fun_name_map()); by = first)
        path = try Cecelia._spec_path(task) catch; nothing end
        (isnothing(path) && continue)
        isfile(path) || continue
        each_spec_param(get(JSON3.read(read(path, String)), :params, [])) do p, _
            String(something(spec_get(p, "type", ""), "")) == "int" || return
            st = spec_get(p, "step", nothing)
            st isa Real || return
            checked += 1
            frac = !isapprox(Float64(st), round(Float64(st)); atol = 1e-9)
            frac && @error "int param with a fractional step — half its slider stops do nothing" task = fun_name param = String(something(spec_get(p, "key", ""), "")) step = st
            @test !frac
        end
    end
    @test checked > 5
end

# A repeatable group carries defaults in TWO places: the group's own `default` dict (what entry "0"
# starts as) and each nested param's `default` (what a NEWLY ADDED entry starts as). When they
# disagree, the first entry and the second silently begin on different values — which is exactly the
# shape of a multi-pass segmentation, so the two passes differ by a parameter nobody set.
#
# Found live: `segment.coastal` had `embeddingBlurSigma` at 0.5 in the group default and 1.5 in the
# param spec (whose tip says "Calibrated at 1.5"). A second pass added in the GUI therefore ran at a
# different embedding blur from the first, and on real data that mismatch turned 56% of the second
# pass's objects into rims around the first pass's cells instead of standalone fragments.
# Ordering and switching off entries of a repeatable group is offered for EVERY such group, by the
# renderer, with no spec field — the reason it exists is a property of repeatable groups themselves
# (entries are applied in turn, each filling only what an earlier one left, so the order is
# semantic). It is resolved away here rather than forwarded: `_apply_group_order` rebuilds the group
# so no handler, runner or Python task learns that ordering exists. The first version of this WAS a
# hand-authored `modelsOrder` param plus a passthrough in one task's .jl — i.e. exactly the thing
# every future grouped task would have had to remember.
@testset "a repeatable group's run order is resolved into the group" begin
    task = Cecelia._fun_name_map()["segment.coastal"]
    @test "models" in Cecelia._repeatable_group_keys(task)

    three = Dict{String,Any}("models" => Dict{String,Any}(
        "0" => Dict{String,Any}("model" => "a"),
        "1" => Dict{String,Any}("model" => "b"),
        "2" => Dict{String,Any}("model" => "c")))

    # no order at all: every entry, untouched. A task saved before the control existed, a chain node
    # and a REPL call all look like this.
    kept = Cecelia._apply_group_order(task, copy(three))
    @test sort(collect(keys(kept["models"]))) == ["0", "1", "2"]

    # reordered AND filtered, renumbered so a consumer's ascending walk IS the run order
    ord = merge(copy(three), Dict{String,Any}("modelsOrder" => ["2", "0"]))
    got = Cecelia._apply_group_order(task, ord)
    @test !haskey(got, "modelsOrder")            # resolved away, never forwarded
    @test sort(collect(keys(got["models"]))) == ["0", "1"]
    @test got["models"]["0"]["model"] == "c"
    @test got["models"]["1"]["model"] == "a"

    # an empty list means run NOTHING — otherwise the off switch would be a no-op
    none = Cecelia._apply_group_order(task, merge(copy(three), Dict{String,Any}("modelsOrder" => String[])))
    @test isempty(none["models"])

    # a stale key outlives the group it was saved against; ignore it rather than fail the run
    stale = Cecelia._apply_group_order(task, merge(copy(three), Dict{String,Any}("modelsOrder" => ["1", "9"])))
    @test length(stale["models"]) == 1
    @test stale["models"]["0"]["model"] == "b"

    # the same entry twice would offset its labels against itself and write nothing the second time
    dup = Cecelia._apply_group_order(task, merge(copy(three), Dict{String,Any}("modelsOrder" => ["0", "0", "1"])))
    @test length(dup["models"]) == 2

    # idempotent, because `run_task` is not the only thing that may normalise a bag of params
    @test Cecelia._apply_group_order(task, copy(got))["models"] == got["models"]

    # EVERY repeatable group gets it, not just the one that motivated it
    reps = String[]
    for (fun_name, t) in Cecelia._fun_name_map()
        isempty(Cecelia._repeatable_group_keys(t)) || push!(reps, fun_name)
    end
    @test "segment.cellpose" in reps
    @test length(reps) >= 3
end

@testset "a group's two sets of defaults agree" begin
    checked = 0
    for (fun_name, task) in sort(collect(Cecelia._fun_name_map()); by = first)
        path = try Cecelia._spec_path(task) catch; nothing end
        (isnothing(path) && continue)
        isfile(path) || continue
        each_spec_param(get(JSON3.read(read(path, String)), :params, [])) do p, _
            String(something(spec_get(p, "type", ""), "")) == "group" || return
            entry0 = spec_get(p, "default", nothing)
            entry0 isa AbstractDict || return
            # the group's default is keyed by entry index ("0"); take the first entry's values
            vals = get(entry0, Symbol("0"), get(entry0, "0", nothing))
            vals isa AbstractDict || return
            gkey = String(something(spec_get(p, "key", ""), ""))
            each_spec_param(get(p, :params, get(p, "params", []))) do q, _
                qkey = String(something(spec_get(q, "key", ""), ""))
                isempty(qkey) && return
                qdef = spec_get(q, "default", nothing)
                isnothing(qdef) && return
                gdef = get(vals, Symbol(qkey), get(vals, qkey, nothing))
                isnothing(gdef) && return
                checked += 1
                same = (gdef isa Real && qdef isa Real) ? isapprox(Float64(gdef), Float64(qdef)) :
                                                          string(gdef) == string(qdef)
                same || @error "group default disagrees with the param default — entry 0 and a newly added entry start differently" task = fun_name group = gkey param = qkey group_default = gdef param_default = qdef
                @test same
            end
        end
    end
    @test checked > 10      # the sweep actually found the groups
end
