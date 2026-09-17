# ── Cohort stages + guides + flow + CellposeSegment testsets ──────────
# 9 sections covering: cohort-stages vs cohort-metrics parity (Julia COHORT_METRICS ⇔
# frontend COHORT_STAGES), guide catalogue name check (every guide names a real task),
# composite-half guide declaration, parse_temporal_scales, flow_dropped_metrics,
# flow_model_target, flow_training_qc_findings, and CellposeSegment spec dynamic Model
# options. Extracted from suite.jl to keep it small enough to merge without EOF conflicts
# on every append. The extracted file loads inside this file's aggregating testset scope,
# so any helpers defined earlier in suite.jl are still in scope (lexical include).
#
# Three `@__DIR__ .. ..` repo-root walks (cohort stages reads frontend/src/lib/cohortStages.ts,
# the two guide catalogue testsets walk frontend/src/lib/guides) are rerouted through
# pathof(Cecelia) so they resolve identically whether the file sits at app/test/ or
# app/test/suite/.

_repo = dirname(dirname(dirname(pathof(Cecelia))))

# ── COHORT_METRICS (Julia) vs COHORT_STAGES (frontend) ───────────────────────
# The two lists were kept in step by a comment, and the comment did not work: `segment.coastal` was
# added to COHORT_METRICS and not to COHORT_STAGES, so the Segment page's cohort check silently
# skipped every coastal run. Nothing failed — the button was just quietly less useful than it looked,
# which is the worst shape for a bug to have.
#
# The rule is scoped, not total. A category with NO entry in COHORT_STAGES has deliberately no cohort
# button (Import banks metrics and offers none), so it is exempt. But once a page offers the button,
# it must cover every cohort-bearing fun in its category — that is the case this catches.
@testset "cohort stages cover their category's cohort metrics" begin
    ts_path = joinpath(_repo, "frontend", "src", "lib", "cohortStages.ts")
    if !isfile(ts_path)
        @test_skip "cohortStages.ts not found"
    else
        src = read(ts_path, String)
        body = match(r"COHORT_STAGES:\s*Record<string,\s*string\[\]>\s*=\s*\{(.*?)\n\}"s, src)
        @test !isnothing(body)

        stages = Dict{String,Vector{String}}()
        for m in eachmatch(r"(\w+)\s*:\s*\[([^\]]*)\]", body.captures[1])
            stages[m.captures[1]] = [String(x.captures[1])
                                     for x in eachmatch(r"'([^']+)'", m.captures[2])]
        end
        @test !isempty(stages)

        # every fun the frontend lists must actually bank cohort metrics
        unknown = [f for fs in values(stages) for f in fs if !haskey(COHORT_METRICS, f)]
        @test isempty(unknown)

        # …and every cohort-bearing fun in a category that HAS a button must be listed
        listed = Set(f for fs in values(stages) for f in fs)
        missing_funs = [f for f in keys(COHORT_METRICS)
                        if haskey(stages, first(split(f, "."))) && !(f in listed)]
        @test isempty(missing_funs)
    end
end

# ── Guide catalogue (frontend) vs the task registry (Julia) ──────────────────
# A guide that teaches "run this function" names the task two ways: `taskKey` (what TaskRunner's
# <select> holds, i.e. the spec's `task`) and `funName` (what the task rail reports, i.e. `fun_name`).
# Nothing in the frontend can check either — the specs live here — so a rename or a mismatched pair
# would leave the guide waiting forever on a function that does not exist, with no error anywhere.
#
# This is the structural half of a real bug: the Segment guide taught plain `segment.cellpose`, which
# produces labels with NO measures, so its own "now gate on these" ending could not work. Choosing the
# wrong function is a judgement no test can make; naming one that isn't real is, and that's this.
@testset "guide catalogue names real tasks" begin
    dir = joinpath(_repo, "frontend", "src", "lib", "guides")
    if !isdir(dir)
        @test_skip "frontend guides catalogue not found"
    else
        src = join([read(joinpath(dir, f), String)
                    for f in readdir(dir) if endswith(f, ".ts") && !endswith(f, ".test.ts")], "\n")
        funs = [String(m.captures[1]) for m in eachmatch(r"funName:\s*'([^']+)'", src)]
        keys_ = [String(m.captures[1]) for m in eachmatch(r"taskKey:\s*'([^']+)'", src)]
        @test !isempty(funs)
        @test length(funs) == length(keys_)      # every task-run block names both

        registry = Cecelia._fun_name_map()
        @test isempty([f for f in funs if !haskey(registry, f)])

        # …and the pair must describe the SAME task: spec(funName).task == taskKey. A half-applied
        # rename that leaves the two pointing at different functions passes every other check —
        # the dropdown gate would never match while the rail happily parked on something else.
        mismatched = String[]
        for (f, k) in zip(funs, keys_)
            haskey(registry, f) || continue
            spec = Cecelia._task_spec(registry[f])
            String(get(spec, "task", "")) == k || push!(mismatched, "$f => $k")
        end
        @test isempty(mismatched)
    end
end

# ── Guides teaching a task a COMPOSITE wraps ─────────────────────────────────
# The bug this closes, twice over: the Segment guide taught `segment.cellpose` and the Track guide
# `tracking.bayesian_tracking` — the BARE halves of `…cellposeMeasure` / `…bayesian_track_measures`.
# Labels without measures and tracks without measures leave gating, clustering and the HMM with nothing
# to read, so each guide's own closing promise ("now gate on these") could not be kept. Nothing failed:
# the tasks ran, the guides completed, the next page was just empty.
#
# So whenever a guide teaches a task that some composite CONTAINS, that has to be a decision on record.
# Drift correction is the legitimate case — its composite adds autofluorescence removal, a separate
# scientific step, not the missing half of drift — which is exactly the distinction a human has to make
# and a test cannot. This is the inventory that forces the question, in the same spirit as the
# frontend's DECLARED_TIMERS list.
@testset "a guide teaching a composite's bare half is declared" begin
    dir = joinpath(_repo, "frontend", "src", "lib", "guides")
    if !isdir(dir)
        @test_skip "frontend guides catalogue not found"
    else
        src = join([read(joinpath(dir, f), String)
                    for f in readdir(dir) if endswith(f, ".ts") && !endswith(f, ".test.ts")], "\n")
        taught = unique([String(m.captures[1]) for m in eachmatch(r"funName:\s*'([^']+)'", src)])
        @test !isempty(taught)

        # fun_name => why teaching the bare task is right even though a composite wraps it
        bare_by_design = Dict{String,String}()

        # every composite's constituent steps, from the registry
        wrapped_by = Dict{String,Vector{String}}()
        for (fun, task) in Cecelia._fun_name_map()
            spec = Cecelia._task_spec(task)
            steps = get(spec, "composite", nothing)
            steps isa AbstractVector || continue
            for st in steps
                push!(get!(wrapped_by, String(st), String[]), fun)
            end
        end

        undeclared = [t for t in taught
                      if haskey(wrapped_by, t) && !haskey(bare_by_design, t)]
        @test isempty(undeclared)

        # …and the list stays honest: an entry whose composite is gone, or that no guide teaches
        # any more, is stale rather than protective.
        stale = [k for k in keys(bare_by_design)
                 if !(k in taught) || !haskey(wrapped_by, k)]
        @test isempty(stale)
    end
end

# ── Optical-flow training (opticalFlow.train) ────────────────────────────────
# The scales are the single most consequential parameter of the pipeline AND the one that fails
# silently: the set a model is trained on must be the set inference feeds it, and coastal does not
# check. Rejecting a typo at the form is the only cheap place to catch it.
@testset "parse_temporal_scales" begin
    @test parse_temporal_scales("1,2,4,8") == [1, 2, 4, 8]
    @test parse_temporal_scales(" 8 , 1 ,2 ") == [1, 2, 8]      # sorted
    @test parse_temporal_scales("2 4 4 2") == [2, 4]            # deduped, whitespace-separated
    @test parse_temporal_scales([1, 2]) == [1, 2]               # a REPL caller's vector

    @test_throws ParamValidationError parse_temporal_scales("")
    @test_throws ParamValidationError parse_temporal_scales("   ")
    @test_throws ParamValidationError parse_temporal_scales("1,2,x")
    @test_throws ParamValidationError parse_temporal_scales("1,0")     # a lag of 0 is not a lag
    @test_throws ParamValidationError parse_temporal_scales("1,-2")
    @test_throws ParamValidationError parse_temporal_scales("1.5")
end

# Which metric planes the model reads. Same silent-failure family as the scales above: coastal stacks
# what it is given in sorted-key order and zero-fills the rest, so an inference set that differs from
# the training set shifts every later channel and raises nothing.
@testset "flow_dropped_metrics" begin
    # nothing = a caller from before the picker existed → the shipped default, not "train on all 11"
    @test sort(Cecelia.flow_dropped_metrics(nothing)) ==
          sort(collect(Cecelia.FLAT_FLOW_METRICS))

    # the picker's own default: the three flat planes are the ones left out
    default_pick = ["acceleration", "cell_boundary_likelihood", "cumulative_mag",
                    "direction_stability", "edge_strength", "normal_flow", "strain",
                    "tangential_flow"]
    @test Cecelia.flow_dropped_metrics(default_pick) ==
          ["divergence", "flow_structure_alignment", "vorticity"]

    # an arbitrary subset is allowed — the defaults are a starting point, not a rule
    @test Cecelia.flow_dropped_metrics(["divergence", "vorticity"]) ==
          [m for m in Cecelia.FIXED_FLOW_METRICS if !(m in ("divergence", "vorticity"))]
    @test isempty(Cecelia.flow_dropped_metrics(collect(Cecelia.FIXED_FLOW_METRICS)))

    # per-scale magnitudes are NOT choices (they follow temporalScales), so naming one drops nothing
    @test Cecelia.flow_dropped_metrics(["mag_1", "strain"]) ==
          [m for m in Cecelia.FIXED_FLOW_METRICS if m != "strain"]

    @test_throws ErrorException Cecelia.flow_dropped_metrics(String[])
end

# A model name reaches the filesystem. Not a security boundary — the user owns the machine — but a
# stray separator would write outside the vault and the model would then never appear in the picker.
@testset "flow_model_target" begin
    td = mktempdir()
    dir = joinpath(td, "models", "coastalModels")

    @test flow_model_target("gcMemTom"; dev_dir = td) == joinpath(dir, "gcMemTom.pt")
    @test isdir(dir)                                   # the vault is created on demand
    @test flow_model_target("gcMemTom.pt"; dev_dir = td) == joinpath(dir, "gcMemTom.pt")

    @test_throws ErrorException flow_model_target(""; dev_dir = td)
    @test_throws ErrorException flow_model_target("  "; dev_dir = td)
    @test_throws ErrorException flow_model_target("../escape"; dev_dir = td)
    @test_throws ErrorException flow_model_target("sub/dir"; dev_dir = td)
    @test_throws ErrorException flow_model_target(".."; dev_dir = td)

    # Overwrite is opt-in: a training run is long, and silently replacing the model a segmentation
    # already used would make an earlier run unreproducible with no trace.
    open(io -> write(io, "stub"), joinpath(dir, "gcMemTom.pt"), "w")
    @test_throws ErrorException flow_model_target("gcMemTom"; dev_dir = td)
    @test flow_model_target("gcMemTom"; overwrite = true, dev_dir = td) ==
          joinpath(dir, "gcMemTom.pt")
end

# The one objective signal a training run has. A model whose loss never came down still segments —
# confidently and wrongly — so it is worth a warning rather than being left in the log.
@testset "flow_training_qc_findings" begin
    @test isempty(flow_training_qc_findings(
        Dict{String,Any}("finalLoss" => 0.2, "lossDrop" => 3.4, "epochs" => 30)))

    flat = flow_training_qc_findings(
        Dict{String,Any}("finalLoss" => 0.9, "lossDrop" => 0.98, "epochs" => 30))
    @test length(flat) == 1
    @test flat[1]["level"] == "warn"
    @test flat[1]["detail"]["epochs"] == 30
    # numbers live in `detail`, not in the prose (docs/UI.md → QC copy)
    @test !occursin("0.9", flat[1]["long"])

    # exactly 1.0 = no improvement at all, still a warning
    @test length(flow_training_qc_findings(Dict{String,Any}("lossDrop" => 1.0))) == 1
    # no history parsed → no claim either way
    @test isempty(flow_training_qc_findings(Dict{String,Any}("epochs" => 30)))
    @test isempty(flow_training_qc_findings(Dict{String,Any}("lossDrop" => NaN)))

    # The held-out arm. This is the case the training curve CANNOT see: the loss drops 3.4x on the
    # frames the weights were fitted to while the held-out loss goes nowhere — a model fitting these
    # frames rather than learning what a cell looks like.
    memorised = flow_training_qc_findings(Dict{String,Any}(
        "finalLoss" => 0.2, "lossDrop" => 3.4, "valLossDrop" => 0.99,
        "valFinalLoss" => 0.9, "epochs" => 30))
    @test length(memorised) == 1
    @test memorised[1]["code"] == "opticalFlow.val_loss_flat"
    @test memorised[1]["level"] == "warn"
    @test memorised[1]["detail"]["valLossDrop"] == 0.99
    @test !occursin("0.99", memorised[1]["long"])

    # both flat = both findings, in order
    @test [f["code"] for f in flow_training_qc_findings(
        Dict{String,Any}("lossDrop" => 0.9, "valLossDrop" => 0.9))] ==
        ["opticalFlow.loss_flat", "opticalFlow.val_loss_flat"]

    # a run with no split says nothing about generalising, rather than passing it silently
    @test isempty(flow_training_qc_findings(
        Dict{String,Any}("finalLoss" => 0.2, "lossDrop" => 3.4)))
    @test isempty(flow_training_qc_findings(
        Dict{String,Any}("lossDrop" => 3.4, "valLossDrop" => 2.1)))
end

# `_task_spec` runs `_inject_dynamic_options!` for CellposeSegment on every call, so a
# dropped-in checkpoint under `<repo>/models/cellposeModels/` appears in the Model select's
# options — that's what makes validate_params accept the name. The bundled dir is normally EMPTY
# since the cellpose 4 migration (the `ceceliaModels` set is all v3 checkpoints and is no longer
# fetched), so the picker returns builtins only; the test guards both regimes.
@testset "CellposeSegment spec dynamic Model options" begin
    spec = Cecelia._task_spec(CellposeSegment())
    @test !isnothing(spec)
    models_group = only(p for p in spec["params"]
                        if get(p, "key", "") == "models")
    model_sel    = only(p for p in models_group["params"]
                        if get(p, "key", "") == "model")
    values = [string(o["value"]) for o in model_sel["options"]]
    # Built-ins are always there
    @test issubset(["cpsam_v2", "cpsam"], values)
    # …and exactly ONCE each. `optionsFrom` APPENDS to the spec's literal `options`, so a spec that
    # also declares an option the lister enumerates gets it twice — which is what this picker did,
    # showing "Cellpose-SAM v2" and "v1" twice each in the browser. An
    # `issubset` assertion cannot see that, which is why it survived; this can.
    @test length(values) == length(unique(values))

    # A genuinely-unknown checkpoint name is still rejected — the enumeration is real
    @test_throws ParamValidationError validate_params(CellposeSegment(),
        Dict{String,Any}("models" => Dict{String,Any}(
            "0" => Dict{String,Any}(
                "model" => "__no_such_file__.pt", "matchAs" => "base",
                "cellChannels" => [], "nucChannels" => [],
                "cellDiameter" => 10, "normalise" => 99.9,
                "stitchThreshold" => 0.0, "threshold" => 0,
                "medianFilter" => 0, "gaussianFilter" => 0.0))))
end
