# ── Task machinery testsets ─────────────────────────────────────────────────────────
# Five small task-machinery testsets that lived between larger extracted suite includes:
# params-NOT-declared pass-through, Custom module registry (drop-in tasks), Section params
# flatten (chain nesting), flow_register QC, and fun_name dispatch. Gathered here so
# suite.jl carries only include lines and their comments. The extracted file loads inside
# this file's aggregating testset scope, so any helpers defined earlier in suite.jl are
# still in scope (lexical include).

@testset "params NOT declared in the spec pass through untouched" begin
    # Ranges/types are swept above. What is asserted here is the absence of a rule: a task's
    # runner may receive keys its spec does not declare, and validation must not reject an
    # unknown key — several tasks rely on carrying extra values through to their runner.
    # cropImage's spec now declares one `cropBox` param carrying the whole box (the imagePicker
    # widget's shape); everything else is unknown and passes through.
    box = Dict{String,Any}("x0"=>0, "x1"=>100, "y0"=>0, "y1"=>100,
                           "z0"=>2, "z1"=>8, "t0"=>-1, "t1"=>-1)
    @test validate_params(
        CropImage(), Dict{String,Any}("cropBox" => box, "extraKey" => "unknown")) === nothing
end

@testset "Custom module registry (drop-in tasks)" begin
    # A user drops a task by calling register_task! with an instance + a spec path; it must then
    # resolve through _task_from_fun_name / _spec_path / validate_params exactly like a built-in.
    spec_dir = mktempdir()
    spec = joinpath(spec_dir, "exampleTest.json")
    write(spec, JSON3.write(Dict(
        "fun_name"      => "customTest.exampleTest",
        "label"         => "Example test",
        "resource_pool" => "cpu",
        "scope"         => "image",
        "params"        => [Dict("key" => "n", "label" => "N", "type" => "int",
                                 "min" => 0, "max" => 10)],
    )))

    register_task!("customTest.exampleTest", _TestCustomTask(); spec = spec)

    @test _task_from_fun_name("customTest.exampleTest") isa _TestCustomTask
    @test Cecelia._spec_path(_TestCustomTask()) == spec       # default _spec_path → custom registry
    @test task_scope(_TestCustomTask()) == "image"           # spec read via the registered path
    # param validation uses the dropped spec (n has min=0/max=10)
    @test_throws ParamValidationError validate_params(
        _TestCustomTask(), Dict{String,Any}("n" => 99))
    @test validate_params(_TestCustomTask(), Dict{String,Any}("n" => 3)) === nothing

    # built-ins win on a fun_name clash — registering under an existing name must NOT shadow it
    register_task!("importImages.remove", _TestCustomTask(); spec = spec)
    @test _task_from_fun_name("importImages.remove") isa RemoveImage

    # a missing spec file is rejected up front
    @test_throws ArgumentError register_task!(
        "customTest.bad", _TestCustomTask(); spec = joinpath(spec_dir, "nope.json"))

    # unknown fun_name still errors
    @test_throws Exception _task_from_fun_name("customTest.doesNotExist")
end

# ── Section-param flattening (whiteboard/chain stores section params NESTED) ───
# Regression: a chain node saves `section` params under the section key (e.g.
# measureOptions => {extendedMeasures: true}), but tasks read them flat. run_task must lift them.
@testset "Section params flatten (chain nesting)" begin
    ml = _task_from_fun_name("segment.measureLabels")
    # measureLabels declares `measureOptions` + `imageTiling` sections
    @test "measureOptions" in Cecelia._section_keys(ml)
    @test "imageTiling"    in Cecelia._section_keys(ml)
    nested = Dict{String,Any}(
        "outputValueName" => "T",
        "measureOptions"  => Dict{String,Any}("extendedMeasures" => true),
        "imageTiling"     => Dict{String,Any}("blockSize" => 4096, "overlap" => 0))
    flat = Cecelia._flatten_sections(ml, nested)
    @test flat["extendedMeasures"] == true          # was buried under measureOptions
    @test flat["blockSize"] == 4096                  # was buried under imageTiling
    @test !haskey(flat, "measureOptions")            # section container dropped
    @test flat["outputValueName"] == "T"             # top-level survives
    # composite pulls section keys from its sub-tasks (cellpose + measureLabels)
    comp = _task_from_fun_name("segment.cellposeMeasure")
    @test "measureOptions" in Cecelia._section_keys(comp)
    @test Cecelia._flatten_sections(comp,
        Dict{String,Any}("measureOptions" => Dict{String,Any}("extendedMeasures" => true)))["extendedMeasures"] == true
    # already-flat params are unchanged (idempotent)
    @test Cecelia._flatten_sections(ml, Dict{String,Any}("extendedMeasures" => true))["extendedMeasures"] == true
end

# QC helper for FlowRegister — the "aligner is chronically saturating" case is what a user needs to
# know about (raise the clamp or accept the deformation is beyond dense flow) and the metrics are
# cohort-comparable, so both branches deserve a pin.
@testset "flow_register QC" begin
    quiet = Dict{String,Any}(
        "flowMax"    => [0.0, 3.2, 4.1, 2.8],
        "flowMean"   => [0.0, 0.8, 1.1, 0.7],
        "maxShiftPx" => 16.0,
    )
    @test isempty(Cecelia._flow_register_qc_findings(quiet))
    qm = Cecelia._flow_register_qc_metrics(quiet)
    @test qm["peakFlowPx"] == 4.1
    # 2.525 -> 2.52 under Julia's banker's rounding at digits=2 (ties to even).
    # Assert what actually ships, not the raw mean.
    @test qm["meanPeakFlowPx"] ≈ 2.52 atol=1e-3
    @test qm["meanFlowPx"]  ≈ 0.65  atol=1e-3

    # >= 50% of frames within 85% of the 16 px clamp -> single 'high_shifts' warn.
    saturating = Dict{String,Any}(
        "flowMax"    => [0.0, 15.0, 14.5, 15.2, 14.0],
        "flowMean"   => [0.0, 5.0, 4.8, 5.3, 4.9],
        "maxShiftPx" => 16.0,
    )
    findings = Cecelia._flow_register_qc_findings(saturating)
    @test length(findings) == 1
    @test findings[1]["level"] == "warn"
    @test findings[1]["code"]  == "flow_register.high_shifts"
    @test findings[1]["detail"]["framesNearCap"] == 4
    @test findings[1]["detail"]["nFrames"]       == 5
    # numbers live in detail, not the copy (docs/UI.md → QC copy)
    @test !occursin("14", findings[1]["long"])

    # No flow trajectory at all -> no claim, rather than a spurious warn.
    @test isempty(Cecelia._flow_register_qc_findings(Dict{String,Any}("maxShiftPx" => 16.0)))
    @test isempty(Cecelia._flow_register_qc_findings(
        Dict{String,Any}("flowMax" => Float64[], "maxShiftPx" => 16.0)))

    # meanFrameCorrelation metric (Phase 1) — average of the finite entries,
    # emitted as a per-image cohort-comparable diagnostic (Galene-derived; see
    # Warren et al. 2018, eLife 7:e35800). The metric is banked even when the
    # `high_shifts` warn does NOT fire, so the sidecar has to carry it.
    with_corr = merge(quiet, Dict{String,Any}(
        "frameCorrelation" => [1.0, 0.8, 0.4, 0.2]))
    qm2 = Cecelia._flow_register_qc_metrics(with_corr)
    @test qm2["meanFrameCorrelation"] == 0.6
    # Absent → metric absent (rather than a spurious NaN or 0).
    @test !haskey(Cecelia._flow_register_qc_metrics(quiet), "meanFrameCorrelation")
end

@testset "fun_name dispatch" begin
    @test _task_from_fun_name("importImages.omezarr") isa ImportOmezarr
    @test _task_from_fun_name("importImages.remove")  isa RemoveImage
    @test _task_from_fun_name("cleanupImages.afCorrect")       isa AfCorrect
    @test _task_from_fun_name("cleanupImages.driftCorrect")    isa DriftCorrect
    @test _task_from_fun_name("cleanupImages.stackAlign")      isa StackAlign
    @test _task_from_fun_name("cleanupImages.flowRegister")    isa FlowRegister
    @test _task_from_fun_name("cleanupImages.smooth")          isa Smooth
    @test _task_from_fun_name("segment.cellpose")              isa CellposeSegment
    @test _task_from_fun_name("segment.measureLabels")         isa MeasureLabels
    cp_measure = _task_from_fun_name("segment.cellposeMeasure")
    @test cp_measure isa CompositeTask
    @test cp_measure.fun_name == "segment.cellposeMeasure"
    @test_throws ErrorException _task_from_fun_name("nonexistent.task")
    @test _task_from_fun_name("tracking.bayesian_tracking")    isa BayesianTracking
    @test _task_from_fun_name("tracking.track_measures")       isa TrackMeasures
    bt_measures = _task_from_fun_name("tracking.bayesian_track_measures")
    @test bt_measures isa CompositeTask
    @test bt_measures.fun_name == "tracking.bayesian_track_measures"
end
