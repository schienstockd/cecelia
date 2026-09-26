# ── Boundary types + ratchet testsets ─────────────────────────────────
# Four sections pinning the type / convention boundaries that must hold across the code
# base: label_props boundary types (add_obs type refusals), typed params ratchet (every
# _run_task reads params through parse_*_params), cohort-metrics ratchet (write_qc
# callers are in COHORT_METRICS or marked exempt), and zarr-access ratchet (Julia files
# don't `using Zarr` outside the sanctioned reader). Extracted from suite.jl to keep it
# small enough to merge without EOF conflicts on every append. The extracted file loads
# inside this file's aggregating testset scope, so any helpers defined earlier in
# suite.jl are still in scope (lexical include).
#
# Multiple `@__DIR__` walkdir scans (into app/src, api/src, both) are rerouted through
# pathof(Cecelia) via `_repo` / `_app_src` / `_api_src` so they resolve identically
# whether the file sits at app/test/ or app/test/suite/.

_repo    = dirname(dirname(dirname(pathof(Cecelia))))
_app_src = joinpath(_repo, "app", "src")
_api_src = joinpath(_repo, "api", "src")

@testset "label_props boundary types" begin
    using DataFrames: DataFrame

    # add_obs — numeric passes; String / Bool refused with the column name.
    let df = DataFrame("label" => [1, 2, 3], "live.cell.speed" => [0.1, 0.2, 0.3])
        @test Cecelia._assert_float_convertible(df) === nothing
    end
    let df = DataFrame("label" => [1, 2, 3], "state" => ["A", "B", "A"])
        try
            Cecelia._assert_float_convertible(df); @test false  # should raise
        catch e
            msg = sprint(showerror, e)
            @test occursin("`state`", msg)                      # column name is IN the message
            @test occursin("write_categorical_obs", msg)        # points at the right helper
        end
    end
    let df = DataFrame("label" => [1, 2], "gated" => [true, false])
        # Bool refused deliberately — silently coerces to 0.0/1.0 and misrepresents the semantic.
        @test_throws Exception Cecelia._assert_float_convertible(df)
    end
    let df = DataFrame("label" => [1, 2], "mixed" => [missing, 0.5])
        # `missing` is allowed (maps to NaN in save!).
        @test Cecelia._assert_float_convertible(df) === nothing
    end

    # CategoricalObsColumn — direct construction, dict entry, NamedTuple entry, and error naming.
    let c = Cecelia.CategoricalObsColumn(; name = "state", labels = [1, 2, 3],
                                          values = ["A", "B", missing])
        @test c.name == "state"
        @test c.labels == [1, 2, 3]
        @test c.values[3] === nothing                  # missing → nothing (JSON null → Python)
        @test c.values[1] == "A"
    end
    let c = Cecelia._to_categorical_obs_column(
                Dict("name" => "state", "labels" => [1, 2], "values" => ["A", "B"]))
        @test c isa Cecelia.CategoricalObsColumn
        @test c.name == "state"
        @test c.labels == [1, 2]
    end
    let nt = (; name = "state", labels = [1, 2], values = ["A", "B"])
        @test Cecelia._to_categorical_obs_column(nt).labels == [1, 2]
    end
    let bad = Dict("nmae" => "state", "labels" => [1], "values" => ["A"])
        # missing "name" — error names the missing field
        try
            Cecelia._to_categorical_obs_column(bad); @test false
        catch e
            @test occursin("`name`", sprint(showerror, e))
        end
    end
    let bad_nt = (; labels = [1], values = ["A"])
        try
            Cecelia._to_categorical_obs_column(bad_nt); @test false
        catch e
            @test occursin("`name`", sprint(showerror, e))
        end
    end
end

# ── Typed-params ratchet ──────────────────────────────────────────────────────
# Every task's `_run_task` reads its params bag through a `parse_<task>_params(::AbstractDict)`
# helper that returns a `Base.@kwdef` struct — one authoritative statement of the task's contract
# with its inbound bag, so a spec rename becomes a struct-field error at parse rather than a silent
# default at the read site. This is the mechanical enforcement of what MAINTAINABILITY.md → *Task
# params are typed* names as a standing rule (pattern-4 fix from the maintainability audit).
#
# Ratchet shape: same as `frontend/src/utils/cssScenarios.ts` — an exact per-file baseline of the
# tasks NOT YET on typed params. New offenders outside the list fail immediately; a file that leaves
# the list must be removed from `TYPED_PARAMS_MIGRATION_BASELINE`. When the baseline is empty the
# ratchet becomes "no offenders", the shape docs/ui/PRIMITIVES.md → *Re-implementing a scenario is a
# test failure* describes.
@testset "typed params ratchet — _run_task reads params through parse_*_params" begin
    tasks_root = joinpath(_app_src, "tasks")

    # Structurally exempt — never need typing:
    #   • `task.jl` — the CciaTask / CompositeTask dispatcher; its base `_run_task` methods delegate
    #     to concrete task methods and legitimately pass the raw bag through.
    #   • `task/composite.jl` — the CompositeTask `_run_task` overloads (split out of `task.jl`);
    #     same exemption reason as the aggregator.
    #   • `testTasks/*` — minimal in-tree fixtures used ONLY by the test suite to exercise the
    #     scheduler; typing them would double their surface with zero production value.
    STRUCTURAL_EXEMPTIONS = Set([
        joinpath("tasks", "task.jl"),
        joinpath("tasks", "task", "composite.jl"),
        joinpath("tasks", "testTasks", "image_task.jl"),
        joinpath("tasks", "testTasks", "incremental_plot_task.jl"),
        joinpath("tasks", "testTasks", "set_task.jl"),
    ])

    # Migration baseline — tasks currently on raw `Dict` access, awaiting their family-arc PR. This
    # list MAY SHRINK, MUST NEVER GROW. Remove the entry in the SAME change that lands the parser.
    # Empty list ⇒ the arc is closed; the ratchet then rejects any regression.
    #
    # The arc closed with PRs #906 (cleanupImages), #909 (editImages), #910 (tracking),
    # #913 (segment), #914 (opticalFlow), #915 (tail: importImages, exportImages, clust*,
    # behaviour, spatialAnalysis). See docs/archive/comment-audit-findings.md for the register.
    TYPED_PARAMS_MIGRATION_BASELINE = Set{String}()
    # Meta-ratchet: growing the baseline requires bumping this cap in the same PR, so a reviewer
    # sees "weaken the check" attempts. See docs/todo/DRIFT_PREVENTION_ASSESSMENT.md.
    TYPED_PARAMS_MIGRATION_BASELINE_MAX = 0

    # Walk every _run_task body via Meta.parseall — robust vs regex (docstrings, nested `end`,
    # helper functions that happen to name their positional arg `params`).
    function _walk(f, expr)
        f(expr)
        if expr isa Expr
            for a in expr.args
                _walk(f, a)
            end
        end
    end

    function _is_run_task_def(e::Expr)
        e.head == :function || return false
        sig = e.args[1]
        sig isa Expr || return false
        # Peel `where` clauses
        while sig.head == :where || sig.head == :(::)
            sig = sig.args[1]
        end
        sig.head == :call || return false
        name = sig.args[1]
        # Bare `_run_task` OR `Foo._run_task` (qualified)
        name === :_run_task || (name isa Expr && name.head == :. &&
                                 name.args[2] isa QuoteNode &&
                                 name.args[2].value === :_run_task)
    end

    _sym_starts(sym::Symbol, prefix::AbstractString, suffix::AbstractString) =
        let s = String(sym)
            startswith(s, prefix) && endswith(s, suffix)
        end

    # Returns (has_parser_def, run_task_calls_parser, run_task_reads_bag).
    # An unavoidable pre-parse guard (a shape check that only makes sense on the raw bag) may carry
    # `# ratchet-ok:` on that exact line + a reason — same escape-hatch discipline as the H5AD/zarr
    # readers (CLAUDE.md → deviations need an inline comment on that exact line). Everything else
    # goes through the parser.
    function _classify(src::AbstractString)
        expr = try
            Meta.parseall(src)
        catch
            return (false, false, false, "unparseable")
        end
        src_lines = split(src, '\n'; keepempty = true)
        _exempt(line::Int) = 1 <= line <= length(src_lines) &&
                             occursin("# ratchet-ok", src_lines[line])

        has_parser_def = false
        calls_parser   = false
        reads_bag      = false
        _walk(expr) do e
            e isa Expr || return
            if e.head == :function
                sig = e.args[1]
                while sig isa Expr && (sig.head == :where || sig.head == :(::))
                    sig = sig.args[1]
                end
                if sig isa Expr && sig.head == :call && sig.args[1] isa Symbol &&
                   _sym_starts(sig.args[1], "parse_", "_params")
                    has_parser_def = true
                end
            end
        end
        _walk(expr) do e
            e isa Expr || return
            _is_run_task_def(e) || return
            body = e.args[2]
            last_line = Ref(0)
            _walk(body) do inner
                if inner isa LineNumberNode
                    last_line[] = inner.line
                    return
                end
                inner isa Expr || return
                if inner.head == :call && !isempty(inner.args)
                    callee = inner.args[1]
                    if callee isa Symbol && _sym_starts(callee, "parse_", "_params")
                        length(inner.args) >= 2 && inner.args[2] === :params && (calls_parser = true)
                    end
                    if callee === :get && length(inner.args) >= 2 && inner.args[2] === :params
                        _exempt(last_line[]) || (reads_bag = true)
                    end
                end
                if inner.head == :ref && !isempty(inner.args) && inner.args[1] === :params
                    _exempt(last_line[]) || (reads_bag = true)
                end
            end
        end
        (has_parser_def, calls_parser, reads_bag, "")
    end

    offenders = String[]      # unexpected — not in either list
    stale_baseline = String[] # still in baseline but actually compliant now (list must shrink)
    for (dir, _, files) in walkdir(tasks_root)
        for f in files
            endswith(f, ".jl") || continue
            path = joinpath(dir, f)
            rel_from_src = relpath(path, _app_src)
            src = read(path, String)
            occursin(r"\bfunction\s+_run_task\b", src) || continue
            (has_parser, calls_parser, reads_bag, err) = _classify(src)
            is_offender = !isempty(err) || !has_parser || !calls_parser || reads_bag

            if rel_from_src in STRUCTURAL_EXEMPTIONS
                # Structural exemptions are silent whether compliant or not — they wouldn't be
                # exempt if the intent were to type them.
                continue
            end
            if rel_from_src in TYPED_PARAMS_MIGRATION_BASELINE
                if !is_offender
                    push!(stale_baseline, rel_from_src)
                end
                continue
            end
            if is_offender
                push!(offenders, rel_from_src *
                      (isempty(err) ? "" : "  ($err)"))
            end
        end
    end

    if !isempty(offenders)
        @error "typed-params ratchet: NEW file(s) on raw `get(params,…)` access.\n" *
               "Add a `Base.@kwdef struct <Task>Params` + `parse_<task>_params(::AbstractDict)` and " *
               "call it once at the top of `_run_task`. See app/src/tasks/cleanupImages/smooth.jl for " *
               "the canonical shape.\n" *
               "Offending file(s):\n  " * join(offenders, "\n  ")
    end
    @test isempty(offenders)

    if !isempty(stale_baseline)
        @error "typed-params ratchet: baseline names file(s) that are now compliant. " *
               "Remove them from `TYPED_PARAMS_MIGRATION_BASELINE` in the same change that landed the " *
               "parser (the list may shrink, must never grow):\n  " * join(stale_baseline, "\n  ")
    end
    @test isempty(stale_baseline)

    # Meta-ratchet: baseline size hasn't grown.
    if length(TYPED_PARAMS_MIGRATION_BASELINE) > TYPED_PARAMS_MIGRATION_BASELINE_MAX
        @error "typed-params ratchet: baseline grew to $(length(TYPED_PARAMS_MIGRATION_BASELINE)) " *
               "(cap $TYPED_PARAMS_MIGRATION_BASELINE_MAX). Either fix the new violation, or bump " *
               "`TYPED_PARAMS_MIGRATION_BASELINE_MAX` and justify in the PR body. " *
               "See docs/todo/DRIFT_PREVENTION_ASSESSMENT.md."
    end
    @test length(TYPED_PARAMS_MIGRATION_BASELINE) <= TYPED_PARAMS_MIGRATION_BASELINE_MAX

    # Sanity: the scan must actually find `_run_task` bodies; a wrong root would let real offenders
    # slip through with an empty offender list. cleanupImages + editImages already ship the pattern.
    canonical = String[]
    for (dir, _, files) in walkdir(tasks_root), f in files
        endswith(f, ".jl") || continue
        path = joinpath(dir, f)
        rel = relpath(path, _app_src)
        rel in STRUCTURAL_EXEMPTIONS && continue
        rel in TYPED_PARAMS_MIGRATION_BASELINE && continue
        src = read(path, String)
        occursin(r"\bfunction\s+_run_task\b", src) || continue
        occursin(r"\bfunction\s+parse_\w+_params\b", src) && push!(canonical, rel)
    end
    @test length(canonical) >= 41   # cleanupImages (6) + editImages (9) + tracking (3) + segment (6) + opticalFlow (2) + tail (15)
end

# ── Cohort-metrics ratchet ────────────────────────────────────────────────────
# Every task that BANKS QC (calls `write_qc`) either registers cohort-comparable metrics in
# `COHORT_METRICS` (app/src/qc_cohort.jl) OR carries a `# COHORT-EXEMPT: <reason>` marker in its
# `.jl` file. Same shape as the typed-params ratchet: MAY SHRINK, MUST NEVER GROW.
#
# The ridges post-mortem exposed the discipline gap: `segment.ridges` shipped `write_qc(...,
# metrics = ...)` calls but was absent from `COHORT_METRICS`, so its per-image counts couldn't be
# outlier-checked across the cohort. The `docs/MODULES.md` "adding a task" checklist already lists
# both (bank QC + add to COHORT_METRICS) as boxes; this makes the second box mechanical.
#
# Marker discipline mirrors the H5AD/zarr readers (CLAUDE.md → deviations need an inline comment
# with a reason). `# COHORT-EXEMPT: <reason>` anywhere in the task's `.jl` file exempts it.
@testset "cohort-metrics ratchet — write_qc callers are in COHORT_METRICS or marked exempt" begin
    tasks_root = joinpath(_app_src, "tasks")

    # Baseline: tasks that bank QC today without a COHORT_METRICS entry and no exemption marker.
    # Each entry is a candidate to close — either register cohort metrics OR add an inline
    # `# COHORT-EXEMPT: <reason>` and remove from this list, in one PR. MAY SHRINK, MUST NEVER GROW.
    COHORT_METRICS_BASELINE = Set([
        # `cleanupImages.smooth` — `zeroFracIn` reads acquisition properties (sparsity), not a
        # cohort-comparable process metric. Test at suite.jl also asserts this is deliberately out
        # (`@test !haskey(COHORT_METRICS, "cleanupImages.smooth")`).
        joinpath("tasks", "cleanupImages", "smooth.jl"),
        # `cleanupImages.denoise`, `cleanupImages.stackAlign` — pending review whether these have
        # cohort-comparable output signals; on-list until the owner decides.
        joinpath("tasks", "cleanupImages", "denoise.jl"),
        joinpath("tasks", "cleanupImages", "stack_align.jl"),
        # `opticalFlow.trainSupportDenoise` — companion training task to opticalFlow.train; same
        # decision-pending as the two above.
        joinpath("tasks", "opticalFlow", "train_support_denoise.jl"),
        # `exportImages.ome_tiff` — an EXPORT (writes a downstream file). No per-image processing
        # metric that a cohort comparison would clarify.
        joinpath("tasks", "exportImages", "ome_tiff.jl"),
        # `segment.ridges` — the incident this ratchet exists to catch. Registered by name on the
        # baseline so this PR doesn't couple to a scientific decision about which ridge counts are
        # cohort-comparable. Close in a follow-up.
        joinpath("tasks", "segment", "ridges.jl"),
    ])
    # Meta-ratchet: growing the baseline requires bumping this cap in the same PR, so a reviewer
    # sees "weaken the check" attempts. See docs/todo/DRIFT_PREVENTION_ASSESSMENT.md.
    COHORT_METRICS_BASELINE_MAX = 6

    # Extract the `fun_name` a task's write_qc call names (`write_qc(img, "segment.ridges", …)`).
    _fun_from_write_qc = function (src::AbstractString)
        m = match(r"write_qc\([^,]+,\s*\"([a-zA-Z_.]+)\"", src)
        return isnothing(m) ? nothing : m.captures[1]
    end

    offenders = String[]
    stale_baseline = String[]
    coverage = 0
    for (dir, _, files) in walkdir(tasks_root), f in files
        endswith(f, ".jl") || continue
        path = joinpath(dir, f)
        src = read(path, String)
        occursin("write_qc(", src) || continue
        fun_name = _fun_from_write_qc(src)
        isnothing(fun_name) && continue
        coverage += 1
        rel = relpath(path, _app_src)
        in_cohort = haskey(COHORT_METRICS, fun_name)
        marked = occursin("COHORT-EXEMPT", src)
        if rel in COHORT_METRICS_BASELINE
            if in_cohort || marked
                push!(stale_baseline, "$rel: no longer needs baseline ($(in_cohort ? "in COHORT_METRICS" : "carries COHORT-EXEMPT marker"))")
            end
            continue
        end
        if !in_cohort && !marked
            push!(offenders, "$rel: `write_qc(\"$fun_name\", …)` — add `$fun_name` to `COHORT_METRICS` (app/src/qc_cohort.jl) OR add `# COHORT-EXEMPT: <reason>` in this file")
        end
    end

    if !isempty(offenders)
        @error "cohort-metrics ratchet: NEW task(s) banking QC without a COHORT_METRICS entry.\n" *
               "See docs/MODULES.md → *Cohort metrics*.\n" *
               "Offending file(s):\n  " * join(offenders, "\n  ")
    end
    @test isempty(offenders)

    if !isempty(stale_baseline)
        @error "cohort-metrics ratchet: baseline names file(s) that are now compliant. " *
               "Remove them from `COHORT_METRICS_BASELINE` in the same change that landed the " *
               "registration/marker (the list may shrink, must never grow):\n  " *
               join(stale_baseline, "\n  ")
    end
    @test isempty(stale_baseline)

    # Meta-ratchet: baseline size hasn't grown.
    if length(COHORT_METRICS_BASELINE) > COHORT_METRICS_BASELINE_MAX
        @error "cohort-metrics ratchet: baseline grew to $(length(COHORT_METRICS_BASELINE)) " *
               "(cap $COHORT_METRICS_BASELINE_MAX). Either register/exempt the new violation, or " *
               "bump `COHORT_METRICS_BASELINE_MAX` and justify in the PR body. " *
               "See docs/todo/DRIFT_PREVENTION_ASSESSMENT.md."
    end
    @test length(COHORT_METRICS_BASELINE) <= COHORT_METRICS_BASELINE_MAX

    # Sanity: a wrong scan root would let real offenders slip through with an empty offender list.
    @test coverage >= 20
end

# ── Julia zarr-access ratchet ────────────────────────────────────────────────
# Companion to `python/cecelia/tests/test_zarr_access_convention.py`, for the Julia side. The
# Python side goes through `zarr_utils`; the Julia side has one sanctioned reader —
# `api/src/image_render.jl` (its header declares itself a "SANCTIONED, NARROW carve-out" for
# lightweight preview renders, with "Do NOT grow this into a general image reader"). Any other
# `using`/`import` of `Zarr`, `EzXML`, or `LightXML` in `app/src/**` or `api/src/**` fails this
# ratchet. Empty baseline today.
@testset "zarr-access ratchet — Julia files don't `using Zarr` outside the sanctioned reader" begin
    banned = (r"^\s*(?:using|import)\s+Zarr(?:\s|$|,)",
              r"^\s*(?:using|import)\s+EzXML(?:\s|$|,)",
              r"^\s*(?:using|import)\s+LightXML(?:\s|$|,)")
    # Sanctioned narrow reader. Its module header pins the "do not grow this" contract.
    exempt = Set([joinpath("api", "src", "image_render.jl")])

    roots = [_app_src,
             _api_src]
    offenders = String[]
    for root in roots, (dir, _, files) in walkdir(root), f in files
        endswith(f, ".jl") || continue
        path = joinpath(dir, f)
        rel = relpath(path, _repo)
        rel in exempt && continue
        src = read(path, String)
        for (i, line) in enumerate(split(src, '\n'; keepempty = true))
            for pat in banned
                if occursin(pat, line)
                    push!(offenders, "$rel:$i: `$(strip(line))`")
                end
            end
        end
    end

    if !isempty(offenders)
        @error "zarr-access ratchet: Julia file(s) import Zarr/EzXML/LightXML outside the " *
               "sanctioned reader. On the Julia side, only `api/src/image_render.jl` is allowed to " *
               "hold this — the browser viewer / bricks path goes through the Python `zarr_utils` " *
               "reader indirectly. See CLAUDE.md → *Image / OME-ZARR access*.\n" *
               "Offending line(s):\n  " * join(offenders, "\n  ")
    end
    @test isempty(offenders)

    # Coverage: the sanctioned exempt must still exist and still import Zarr — otherwise we're
    # ratcheting on nothing (a moved file would silently make the scan pass).
    exempt_path = joinpath(_api_src, "image_render.jl")
    @test isfile(exempt_path)
    @test occursin(r"\busing\s+Zarr\b", read(exempt_path, String))
end
