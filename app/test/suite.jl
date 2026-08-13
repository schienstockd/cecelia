# Cecelia package smoke tests — the suite body.
#
# Split out of runtests.jl (which keeps the preamble and wraps this in the one aggregating
# @testset). The split is a PERFORMANCE fix, not organisation: as a single 8k-line `@testset
# begin ... end`, the whole body was one top-level thunk that Julia lowered and compiled in full
# before running a single assertion — ~90s of the suite's ~200s, on top of ~99s of ordinary method
# compilation, for ~11s of actual test work. Behind an include it is ~194 small statements instead.
# Keep it that way: do not re-wrap this file in a `begin` block or a single outer @testset.


# ── Config ────────────────────────────────────────────────────────────────
@testset "Config" begin
    @test !isempty(projects_dir())
    @test !isempty(python_bin_path())
    @test tasks_concurrent_limit() >= 1
    @test napari_discrete_gpu() isa Bool   # [napari].discreteGpu, default false
end

# ── Fixture size ratchet ─────────────────────────────────────────────────────
# Fixtures are committed now, and `.h5ad` is binary: git stores a WHOLE new copy per update and
# history can't be pruned without a rewrite. "Keep fixtures small" was already the rule but nothing
# enforced it, and an in-repo dir is a standing invitation to drop a GB OME-ZARR in. This is the
# enforcement — same shape as the UI-copy ratchets: an exact cap, not a vibe.
#
# 1 MB leaves ~3x headroom over today's largest (B.h5ad, 332 KB). If a fixture genuinely needs more,
# that is a design conversation (regenerate smaller / synthesise / gate the test differently), not a
# number to nudge up.
@testset "fixtures stay small" begin
    root = normpath(joinpath(@__DIR__, "..", "..", "test-data"))
    CAP  = 1024 * 1024
    if isdir(root)
        oversized = Tuple{String,Int}[]
        total = 0
        for (dir, _, files) in walkdir(root), f in files
            n = filesize(joinpath(dir, f)); total += n
            n > CAP && push!(oversized, (relpath(joinpath(dir, f), root), n))
        end
        @test isempty(oversized)          # names the offender if it fires
        isempty(oversized) || @info "oversized fixtures" oversized
        # a whole-tree bound too: many medium files are as bad as one large one
        @test total <= 8 * CAP
    else
        @test_skip "test-data/ not present"
    end
end

# ── Config resolver (dev↔prod coordination) ─────────────────────────────────
# The single resolver both init_cecelia! (reader) and set_projects_dir! (writer) share.
# Order: explicit arg → CECELIA_DEV_DIR env → .env → ~/.cecelia. See docs/todo/ONBOARDING_PLAN.md.
@testset "Config resolver" begin
    # pure resolution order, no env/file reads
    @test Cecelia._resolve_config_dir("/x", "/y", "/z") == "/x"       # explicit wins
    @test Cecelia._resolve_config_dir(nothing, "/y", "/z") == "/y"    # env beats .env
    @test Cecelia._resolve_config_dir(nothing, nothing, "/z") == "/z" # .env beats default
    # `expand_user`, NOT Base.expanduser: the latter is a no-op on Windows, so asserting against
    # it would compare two unexpanded strings and pass vacuously there.
    @test Cecelia._resolve_config_dir(nothing, nothing, nothing) ==   # installed-app default
          expand_user("~/.cecelia")
    @test Cecelia._resolve_config_dir("~/foo", nothing, nothing) == expand_user("~/foo")
    # …and the resolved default must be a real absolute path on EVERY platform — a surviving `~`
    # is the Windows bug that produced `~/.cecelia\observer-mcp.json` in CI.
    @test isabspath(Cecelia._resolve_config_dir(nothing, nothing, nothing))
    @test !startswith(Cecelia._resolve_config_dir(nothing, nothing, nothing), "~")
    # public composition
    @test config_dir("/tmp/ceceliatest") == "/tmp/ceceliatest"
    @test custom_toml_path("/tmp/ceceliatest") == joinpath("/tmp/ceceliatest", "custom.toml")
end

# ── python_bin_path: resolved, not a bare name ───────────────────────────────
# It used to return the config default `"python3"` verbatim. That works for anything JULIA spawns
# (pixi run puts the env first on PATH) but not for the string the observer registers into the
# user's OWN Claude Code config: launched from a plain shell, bare `python3` is the SYSTEM python,
# which has neither `mcp` nor `websockets` — so the observer's tools failed in exactly the sessions
# one-click setup exists to enable. And on Windows `python3` frequently doesn't exist at all.
@testset "python_bin_path resolution" begin
    # candidate order — `iswin` explicit so the Windows list is asserted from any host
    @test Cecelia._python_bin_candidates("", false) == ["python3", "python"]
    @test Cecelia._python_bin_candidates("", true)  == ["python", "python3"]   # conda ships python.exe
    # the SHIPPED default is not a deliberate choice, so it gets the platform fallbacks
    @test Cecelia._python_bin_candidates(Cecelia._PYTHON_BIN_DEFAULT, true) == ["python", "python3"]
    @test Cecelia._python_bin_candidates("  python3  ", false) == ["python3", "python"]  # trimmed
    # a DELIBERATELY configured name is the only candidate — resolve it, never substitute another
    # interpreter (that would run tasks under something lacking the analysis deps, silently)
    @test Cecelia._python_bin_candidates("mypy-thon", false) == ["mypy-thon"]
    @test Cecelia._python_bin_candidates("python", false) == ["python"]
    @test Cecelia._PYTHON_BIN_DEFAULT == "python3"          # must match app/config.toml [dirs]

    # the live resolver: with no explicit path configured it must return something ABSOLUTE that
    # exists — that is the whole point (the old bare name failed `isfile`).
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "python"); old = get(dirs, "python", nothing)
    try
        dirs["python"] = "python3"          # the shipped default → must resolve, not pass through
        let p = python_bin_path()
            @test isabspath(p)
            @test isfile(p)
        end
        # an explicitly configured PATH is honoured verbatim — the user named an exact interpreter
        dirs["python"] = "/opt/custom/bin/python3.11"
        @test python_bin_path() == "/opt/custom/bin/python3.11"
        # …including through a leading ~
        dirs["python"] = joinpath("~", "venv", "bin", "python")
        @test python_bin_path() == joinpath(homedir(), "venv", "bin", "python")
        # an unresolvable bare name degrades to itself rather than to nothing
        dirs["python"] = "cecelia-no-such-interpreter-42"
        @test python_bin_path() == "cecelia-no-such-interpreter-42"
    finally
        had ? (dirs["python"] = old) : delete!(dirs, "python")
    end
end

# ── expand_user: portable leading-~ expansion ────────────────────────────────
# Base.expanduser is documented Unix-only and silently returns the path unchanged on Windows, so
# every stored `~`-prefixed path (custom.toml dirs, .env CECELIA_DEV_DIR) went through unexpanded
# there. These assertions hold on all three platforms.
@testset "expand_user" begin
    @test expand_user("~") == homedir()
    @test expand_user("~/foo") == joinpath(homedir(), "foo")
    @test expand_user("~/foo/bar") == joinpath(homedir(), "foo", "bar")
    # never leaves a tilde behind
    @test !startswith(expand_user("~/foo"), "~")
    # absolute + relative paths pass through untouched
    @test expand_user("/abs/path") == "/abs/path"
    @test expand_user("relative/path") == "relative/path"
    @test expand_user("") == ""
    # a tilde that isn't a leading path component is a legitimate filename character
    @test expand_user("/tmp/a~b") == "/tmp/a~b"
    @test expand_user("~notauser/foo") == "~notauser/foo"
    # Windows accepts either separator after the tilde
    if Sys.iswindows()
        @test expand_user("~\\foo") == joinpath(homedir(), "foo")
        @test expand_user("~\\foo\\bar") == joinpath(homedir(), "foo", "bar")
        # The result must be a CANONICAL path — no mixed separators. Pasting the remainder on
        # verbatim gave `C:\Users\x\foo/bar`, which Windows tolerates but which makes every path
        # comparison unreliable (and this is what CI caught first time it ran on Windows).
        @test !occursin('/', expand_user("~/foo/bar"))
    end
    # collapsing a doubled separator is fine; losing a component is not
    @test expand_user("~//foo") == joinpath(homedir(), "foo")
    @test splitpath(expand_user("~/a/b/c"))[end-2:end] == ["a", "b", "c"]
end

# ── ensure_config_dir: safe to WRITE into ────────────────────────────────────
# config_dir() is a pure path computation, so on a machine that has never run the setup wizard
# the directory does not exist and `open(joinpath(config_dir(), …), "w")` throws. That broke CI
# on all three platforms when the observer began writing its MCP config on every status call.
@testset "ensure_config_dir" begin
    base = mktempdir()
    target = joinpath(base, "never-created")
    @test !isdir(target)
    @test ensure_config_dir(target) == target
    @test isdir(target)                              # created
    @test ensure_config_dir(target) == target        # idempotent on an existing dir
    @test isdir(target)
    # and a file can actually be written into it — the thing the observer needs
    nested = joinpath(base, "a", "b", "c")           # several levels missing
    ensure_config_dir(nested)
    write(joinpath(nested, "observer-mcp.json"), "{}")
    @test isfile(joinpath(nested, "observer-mcp.json"))
end

# ── Release-bundle integrity ─────────────────────────────────────────────────
# `/api/update/apply` hands the downloaded payload to the launcher, which overwrites the app
# with it on the next restart — so a truncated or swapped asset matters, and HTTPS says nothing
# about either. These back the `.sha256` published beside the bundle by `release.yml`.
@testset "_file_sha256 / _sha256_matches" begin
    mktempdir() do d
        f = joinpath(d, "cecelia.tar.gz")
        write(f, "some bundle bytes")
        h = Cecelia._file_sha256(f)

        @test occursin(r"^[0-9a-f]{64}$", h)                         # lowercase hex, 64 chars
        @test h == Cecelia._file_sha256(f)                           # stable
        @test Cecelia._sha256_matches(f, "$h  cecelia.tar.gz")       # GNU `sha256sum` form
        @test Cecelia._sha256_matches(f, h)                          # bare hash
        @test Cecelia._sha256_matches(f, "$h  cecelia.tar.gz\n")     # trailing newline
        @test Cecelia._sha256_matches(f, uppercase(h))               # case-insensitive

        # Every not-a-match must be FALSE, never an exception — the caller decides whether a
        # missing/broken digest is fatal (it is verify-if-present, so it isn't).
        @test !Cecelia._sha256_matches(f, "0"^64)                    # wrong digest
        @test !Cecelia._sha256_matches(f, "")                        # empty file
        @test !Cecelia._sha256_matches(f, "   \n ")                  # whitespace only
        @test !Cecelia._sha256_matches(f, "<!DOCTYPE html>")         # an error page, not a digest
        @test !Cecelia._sha256_matches(f, h[1:40])                   # truncated
        @test !Cecelia._sha256_matches(f, h * "ff")                  # over-long

        # A changed byte must change the verdict — the whole point.
        write(f, "some bundle bytez")
        @test !Cecelia._sha256_matches(f, "$h  cecelia.tar.gz")
    end
end

# ── Custom cellpose model resolver ───────────────────────────────────────────
# A user-placed checkpoint under `<config_dir>/models/cellposeModels/{name}` is picked up
# by the cellpose Julia handler and passed to Python as an absolute file path (which
# `cellpose_utils.py::_get_model` loads via `pretrained_model=…`). No shell-outs, no
# network — pure filesystem resolution.
@testset "cellpose_model_path resolver" begin
    # user-override slot (config_dir): missing file → nothing; empty/blank name → nothing.
    td = mktempdir()
    @test cellpose_model_path("__no_such_model__.pt", td) === nothing
    @test cellpose_model_path("", td) === nothing
    @test cellpose_model_path("   ", td) === nothing

    # place a file in the config-dir override slot, resolver returns its absolute path (the
    # bundled slot at <repo>/models/cellposeModels/ takes precedence when both exist — that
    # path is real in this repo and can't be safely mocked here).
    mkpath(joinpath(td, "models", "cellposeModels"))
    f = joinpath(td, "models", "cellposeModels", "__unique_test_model__.pt")
    open(io -> write(io, "stub"), f, "w")
    @test cellpose_model_path("__unique_test_model__.pt", td) == f

    # cellpose_models_dir is a pure path — no I/O, no side-effects
    @test cellpose_models_dir(td) == joinpath(td, "models", "cellposeModels")
end

# ── Runtime-enumerated cellpose model picker (drop-in convention) ────────────
# `list_cellpose_models` returns builtins + bundled + user drop-ins; the ordering + dedup
# rules are what the picker AND `validate_params` both see (see `_inject_dynamic_options!`
# in cellpose.jl + `_task_spec`'s dynamic hook in task.jl). Guards: builtins always present,
# dedup shadows a bundled file by the same-name user file, a user file that isn't a known
# built-in becomes selectable, and validate_params accepts it.
@testset "list_cellpose_models enumeration" begin
    # NOTE: only the USER dir is td-scoped here; the BUNDLED dir is `<repo>/models/
    # cellposeModels/` (hardcoded via @__DIR__ so it matches the resolver + install layout).
    # A dev running `pixi run models-fetch` populates the bundled dir with real checkpoints,
    # so tests can't assume it's empty — check invariants that hold either way.
    td = mktempdir()

    # Built-ins are always first, in a stable order.
    names = [m.name for m in list_cellpose_models(td)]
    @test names[1:4] == ["cyto3", "cyto2", "cyto", "nuclei"]

    # No user drop-in → nothing tagged "user" for THIS td.
    base = list_cellpose_models(td)
    @test all(m.source != "user" for m in base)

    # Place a user drop-in checkpoint → appears with source="user"
    mkpath(joinpath(td, "models", "cellposeModels"))
    f = joinpath(td, "models", "cellposeModels", "myFluo.pt")
    open(io -> write(io, "stub"), f, "w")
    with_user = list_cellpose_models(td)
    idx = findfirst(m -> m.name == "myFluo.pt", with_user)
    @test !isnothing(idx)
    @test with_user[idx].source == "user"
    @test occursin("user", with_user[idx].label)

    # Dotfiles / subdirectories are skipped when scanning.
    open(io -> write(io, "hidden"), joinpath(td, "models", "cellposeModels", ".DS_Store"), "w")
    mkpath(joinpath(td, "models", "cellposeModels", "subdir"))
    clean = list_cellpose_models(td)
    @test all(m.name != ".DS_Store" for m in clean)
    @test all(m.name != "subdir"    for m in clean)
end

# ── Coastal (optical-flow) model vault ───────────────────────────────────────
# The same drop-in convention as cellpose, one directory over — but with no built-ins and no
# bundled slot, because coastal ships no models: a user trains their own on the Optical Flow
# page. The manifest is the load-bearing part. Inference MUST use the metric set a model was
# trained on, and coastal fails silently when it doesn't (a missing plane shifts every later
# channel), so `CoastalUtils` configures itself from the sidecar rather than from task params.
@testset "coastal model vault" begin
    td = mktempdir()
    @test coastal_models_dir(td) == joinpath(td, "models", "coastalModels")

    # Empty vault → empty picker. No built-in fallback to segment with by accident.
    @test isempty(list_coastal_models(td))
    @test coastal_model_path("anything.pt", td) === nothing
    @test coastal_model_path("", td) === nothing
    @test coastal_model_path("   ", td) === nothing

    dir = joinpath(td, "models", "coastalModels")
    mkpath(dir)
    pt = joinpath(dir, "gcMemTom.pt")
    open(io -> write(io, "stub"), pt, "w")
    @test coastal_model_path("gcMemTom.pt", td) == pt

    # A checkpoint with no manifest still lists — it just falls back to coastal's defaults.
    bare = list_coastal_models(td)
    @test length(bare) == 1
    @test bare[1].name == "gcMemTom.pt"
    @test bare[1].label == "gcMemTom"
    @test isempty(bare[1].manifest)
    @test isempty(coastal_model_manifest("gcMemTom.pt", td))

    # With a manifest, the picker label says what the model was trained on.
    write(joinpath(dir, "gcMemTom.json"),
          """{"channelName":"mem-TOM","temporalScales":[1,2,4,8],"cumulativeWindow":5}""")
    with_manifest = list_coastal_models(td)
    @test with_manifest[1].label == "gcMemTom (mem-TOM)"
    @test with_manifest[1].manifest["cumulativeWindow"] == 5
    @test with_manifest[1].manifest["temporalScales"] == [1, 2, 4, 8]

    # A corrupt manifest must not take the picker down with it.
    write(joinpath(dir, "gcMemTom.json"), "{not json")
    @test isempty(coastal_model_manifest("gcMemTom.pt", td))
    @test length(list_coastal_models(td)) == 1

    # The sidecar is not an entry of its own, and dotfiles/subdirs are skipped.
    open(io -> write(io, "hidden"), joinpath(dir, ".DS_Store"), "w")
    mkpath(joinpath(dir, "subdir.pt"))
    names = [m.name for m in list_coastal_models(td)]
    @test names == ["gcMemTom.pt"]
end

# The coastal picker is ENTIRELY runtime-enumerated — coastal ships no built-in models, so on a
# fresh install the only option is "None". That empty state has to stay a legible choice rather than
# a select that rejects its own default, which is what the first version did.
@testset "CoastalSegment spec dynamic Model options" begin
    spec = Cecelia._task_spec(CoastalSegment())
    @test !isnothing(spec)
    models_group = only(p for p in spec["params"] if get(p, "key", "") == "models")
    model_sel    = only(p for p in models_group["params"] if get(p, "key", "") == "model")
    values = [string(o["value"]) for o in model_sel["options"]]

    @test first(values) == ""                       # "None" is always first and always present
    @test string(first(model_sel["options"])["label"]) == "None"
    @test validate_params(CoastalSegment(),
        Dict{String,Any}("models" => Dict{String,Any}(
            "0" => Dict{String,Any}("model" => "")))) === nothing

    # A name that is not in the vault is rejected — the enumeration is real, and a missing model
    # must never silently fall back to another one.
    @test_throws ParamValidationError validate_params(CoastalSegment(),
        Dict{String,Any}("models" => Dict{String,Any}(
            "0" => Dict{String,Any}("model" => "__not_in_the_vault__.pt"))))
end

# Selecting nothing must fail with an instruction, not with a stack trace deep in Python. The
# missing-model case is the shared one: a config-dir model does not travel with a `.ccbundle`, so
# opening someone else's project WILL name a model this machine does not have.
@testset "coastal_models_for_python resolution" begin
    raw = Dict{String,Any}("imChannelNames" => Dict{String,Any}(
        "default" => ["CH1", "CH2"], "_active" => "default"))

    @test isempty(Cecelia.coastal_models_for_python(Dict{String,Any}(), raw))

    no_model = Dict{String,Any}("models" => Dict{String,Any}(
        "0" => Dict{String,Any}("model" => "", "cellChannels" => ["CH2"])))
    err = try
        Cecelia.coastal_models_for_python(no_model, raw); nothing
    catch e; e end
    @test err isa ErrorException && occursin("Optical Flow page", err.msg)

    missing_model = Dict{String,Any}("models" => Dict{String,Any}(
        "0" => Dict{String,Any}("model" => "__absent__.pt", "cellChannels" => ["CH2"])))
    err2 = try
        Cecelia.coastal_models_for_python(missing_model, raw); nothing
    catch e; e end
    @test err2 isa ErrorException && occursin("not included in a project export", err2.msg)

    # Channel NAMES become 0-based indices — the translation the preview shares with the run.
    abs_model = Dict{String,Any}("models" => Dict{String,Any}(
        "0" => Dict{String,Any}("model" => @__FILE__, "cellChannels" => ["CH2"])))
    out = Cecelia.coastal_models_for_python(abs_model, raw)
    @test out["0"]["cellChannels"] == [1]
    @test out["0"]["model"] == @__FILE__
end

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
    ts_path = joinpath(@__DIR__, "..", "..", "frontend", "src", "lib", "cohortStages.ts")
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
    dir = joinpath(@__DIR__, "..", "..", "frontend", "src", "lib", "guides")
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
    dir = joinpath(@__DIR__, "..", "..", "frontend", "src", "lib", "guides")
    if !isdir(dir)
        @test_skip "frontend guides catalogue not found"
    else
        src = join([read(joinpath(dir, f), String)
                    for f in readdir(dir) if endswith(f, ".ts") && !endswith(f, ".test.ts")], "\n")
        taught = unique([String(m.captures[1]) for m in eachmatch(r"funName:\s*'([^']+)'", src)])
        @test !isempty(taught)

        # fun_name => why teaching the bare task is right even though a composite wraps it
        bare_by_design = Dict(
            "cleanupImages.driftCorrect" =>
                "its composite adds AF correction — a separate scientific step, not drift's missing half",
        )

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
# dropped-in checkpoint under `<repo>/models/cellposeModels/` (this worktree has ccia.fluo
# from `pixi run models-fetch`) appears in the Model select's options — that's what makes
# validate_params accept the name. If the bundled dir is empty (no models-fetch), the picker
# still returns builtins; the test guards both regimes.
@testset "CellposeSegment spec dynamic Model options" begin
    spec = Cecelia._task_spec(CellposeSegment())
    @test !isnothing(spec)
    models_group = only(p for p in spec["params"]
                        if get(p, "key", "") == "models")
    model_sel    = only(p for p in models_group["params"]
                        if get(p, "key", "") == "model")
    values = [string(o["value"]) for o in model_sel["options"]]
    # Built-ins are always there
    @test issubset(["cyto3", "cyto2", "cyto", "nuclei"], values)

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

@testset "run_py custom-modules PYTHONPATH (config_dir not shadowed)" begin
    # Regression: run_py's task-dir parameter was named `config_dir`, which shadowed the
    # config_dir() function, so the custom-modules PYTHONPATH line `joinpath(config_dir(), …)`
    # called the task-dir STRING as a function → every Python task died with
    # `MethodError(<task dir>, (), …)` before Python was ever spawned. The call now lives in a
    # standalone helper with no shadowing param in scope. This asserts it resolves via the real
    # config_dir() function (equality would fail if it ever called anything else).
    # Post-#332 the custom-modules python dir IS the modules ROOT (runners are co-located under
    # modules/<cat>/, launched by absolute path so their own dir is sys.path[0]; the root just makes
    # the wider tree importable) — not the old shared modules/python. See py_runner.jl:_custom_modules_pydir.
    @test Cecelia._custom_modules_pydir() == joinpath(config_dir(), "modules")
    @test endswith(Cecelia._custom_modules_pydir(), "modules")
end

# ── Every Python task inherits a BLAS thread budget ─────────────────────────
#
# A pool limit caps concurrent TASKS, not threads: any numpy/scipy call reaching BLAS takes every
# core, so `cpu` at its default 20 means twenty tasks each asking for 32 threads. `run_py` is the
# only layer that can bound it — `OPENBLAS_NUM_THREADS` is read when the child imports numpy.
#
# Asserted on the SOURCE rather than by spawning, because the value has to be in the env `addenv`
# builds, and a helper nobody is forced to call is exactly how this gets dropped again. See
# docs/SCHEDULER.md → *Thread budgets*.
@testset "run_py bounds the BLAS thread pool" begin
    env = Dict(Cecelia._py_task_env("/tmp/py"))
    @test env["OPENBLAS_NUM_THREADS"] == string(Cecelia.BLAS_THREADS_PER_TASK)
    # A small positive budget: 1 measured SLOWER than 4 (the work is parallel, just not 32-ways),
    # and anything large defeats the point.
    @test 2 <= Cecelia.BLAS_THREADS_PER_TASK <= 8

    # NOT OMP_NUM_THREADS. That also throttles torch's intra-op parallelism, and torch on CPU is
    # the one measured workload that genuinely wants the cores (a cellpose-shaped conv stack goes
    # 0.19s -> 0.34s at 4 threads). Capping OpenBLAS alone leaves torch untouched.
    @test !haskey(env, "OMP_NUM_THREADS")
    @test !haskey(env, "MKL_NUM_THREADS")

    # the rest of the contract this env carries, so a refactor cannot silently drop one
    @test env["PYTHONPATH"] == "/tmp/py"
    @test haskey(env, "CECELIA_PY_CONTRACT") && haskey(env, "CECELIA_IMAGE_COMPRESSOR")

    # The preview worker runs the tasks' OWN compute, so it inherits the same budget. Napari
    # deliberately does not — un-pooled interactive viewer, not BLAS-bound, unmeasured.
    prev = read(joinpath(Cecelia._app_dir(), "src", "preview.jl"), String)
    @test occursin("OPENBLAS_NUM_THREADS", prev)
    @test !occursin("OPENBLAS_NUM_THREADS",
                    read(joinpath(Cecelia._app_dir(), "src", "napari.jl"), String))
end

# ── First-launch setup wizard (isolated temp config dir) ────────────────────
# Uses its own CECELIA_DEV_DIR tempdir so it never touches the real dev/prod config; restores
# global config afterwards. Exercises setup_required + set_projects_dir! (merge + reload).
@testset "Config setup wizard" begin
    prev_env = get(ENV, "CECELIA_DEV_DIR", nothing)
    mktempdir() do tmp
        ENV["CECELIA_DEV_DIR"] = tmp
        try
            init_cecelia!()                            # load the empty temp config
            @test custom_toml_path() == joinpath(tmp, "custom.toml")
            @test setup_required() == true             # no custom.toml yet

            # a pre-existing key must survive the merge
            write(custom_toml_path(), "[dirs]\npython = \"/opt/py\"\n")
            @test setup_required() == true             # projects still unset → placeholder

            proj = joinpath(tmp, "projects"); mkpath(proj)
            stored = set_projects_dir!(proj)
            @test stored == proj
            @test isfile(custom_toml_path())
            @test projects_dir() == proj               # hot-reloaded, no restart
            @test setup_required() == false            # configured + dir exists
            @test python_bin_path() == "/opt/py"       # merge preserved the other key

            # a configured-but-missing dir re-triggers setup
            rm(proj; recursive = true)
            @test setup_required() == true
        finally
            prev_env === nothing ? delete!(ENV, "CECELIA_DEV_DIR") :
                                   (ENV["CECELIA_DEV_DIR"] = prev_env)
            init_cecelia!()                            # restore real dev/prod config
        end
    end
end

# ── Napari discrete-GPU launch env ──────────────────────────────────────────
# The bridge command gains the offload env only when discrete_gpu is on (Linux). DRI_PRIME is
# always applied (safe on single-GPU); the NVIDIA GLX vendor var only when NVIDIA is present.
@testset "Napari discrete-GPU command" begin
    plain = Cecelia._bridge_cmd(false)
    gpu   = Cecelia._bridge_cmd(true)
    for cmd in (plain, gpu)
        @test cmd.env !== nothing
        @test any(==("PYTHONPATH=$(Cecelia._python_dir())"), cmd.env)
    end
    if Sys.islinux()
        @test any(==("DRI_PRIME=1"), gpu.env)             # always safe → always applied
        # nvidia GLX vendor var is gated on detection (forcing it without NVIDIA breaks GL)
        has_nvidia = any(startswith("__GLX_VENDOR_LIBRARY_NAME=nvidia"), gpu.env)
        @test has_nvidia == Cecelia._nvidia_present()
    else
        @test !any(==("DRI_PRIME=1"), gpu.env)            # no GPU offload off Linux
    end
end

# ── Resident Python processes resolve `cecelia` from THIS checkout ──────────
# The bridge and the preview worker are launched by PATH but import `cecelia` by NAME, so without an
# explicit PYTHONPATH they use whatever pip has installed — in dev an editable install pointing at the
# MAIN checkout. A worktree then runs its own `napari_bridge.py`/`preview_worker.py` against another
# checkout's library, and the halves drift with no error until one calls something the other lacks
# (observed: `module 'cecelia.utils.correction_utils' has no attribute 'af_derived_values'`, raised by a
# worker whose own file did have the caller). `run_py` always set PYTHONPATH; these two did not.
@testset "resident python processes pin PYTHONPATH" begin
    pyroot = Cecelia._python_dir()
    @test isdir(joinpath(pyroot, "cecelia"))              # the dir we are pinning really is the package

    for cmd in (Cecelia._bridge_cmd(false), Cecelia._bridge_cmd(true))
        @test any(==("PYTHONPATH=$pyroot"), cmd.env)
    end

    # the worker's launch is inside `launch!` (which spawns), so assert on the source rather than run it
    src = read(joinpath(dirname(pathof(Cecelia)), "preview.jl"), String)
    body = src[findfirst("function launch!(", src)[1]:end]
    @test occursin("PYTHONPATH", body[1:findfirst("\nend", body)[1]])
end

# ── AI observer (in-app assistant) — pure command/result pieces ─────────────
# The live spawn (needs the agent CLI + a running API) isn't tested here; these pin the pure
# builders/parsers that the runner + api route depend on. See docs/todo/OBSERVER_INTEGRATION_PLAN.md.
@testset "AI observer agent runner (pure pieces)" begin
    a   = Cecelia.ClaudeAgent(bin = "claude", model = "")               # explicit empty → no flag
    cmd = Cecelia._build_claude_cmd(a, "hello", "/tmp/mcp.json"; system_prompt = "be brief")
    argv = cmd.exec
    @test argv[1] == "claude"
    @test "-p" in argv && "hello" in argv
    @test "--output-format" in argv && "json" in argv
    @test "--mcp-config" in argv && "/tmp/mcp.json" in argv
    @test "--allowedTools" in argv                                    # observer tools allowed
    @test "--append-system-prompt" in argv && "be brief" in argv
    @test !("--resume" in argv)                                       # no session → no resume
    @test !("--model" in argv)                                        # empty model → no flag

    cmd2 = Cecelia._build_claude_cmd(Cecelia.ClaudeAgent(bin = "claude", model = "claude-opus-4-8"),
                                     "hi", "/tmp/m.json"; session_id = "sess123")
    @test "--resume" in cmd2.exec && "sess123" in cmd2.exec
    @test "--model" in cmd2.exec && "claude-opus-4-8" in cmd2.exec

    # model choice: shipped default is Sonnet (Opus not needed for observer work); the request
    # model is allow-listed — an arbitrary string never reaches --model. (default_model reads
    # config [ai] model, so assert it stays within the allow-list rather than a hard "sonnet".)
    @test Set(Cecelia.OBSERVER_MODELS) == Set(["haiku", "sonnet", "opus"])
    @test Cecelia.observer_default_model() in Cecelia.OBSERVER_MODELS
    @test Cecelia.observer_valid_model("haiku") == "haiku"
    @test Cecelia.observer_valid_model("gpt-4") == Cecelia.observer_default_model()   # unknown → default
    @test Cecelia.observer_valid_model("")     == Cecelia.observer_default_model()
    @test Cecelia.ClaudeAgent(bin = "claude").model == Cecelia.observer_default_model()

    # result parsing — success carries text + usage + session
    r = Cecelia._parse_claude_result(
        """{"is_error":false,"result":"noted a stuck task","session_id":"s1","usage":{"input_tokens":1200,"output_tokens":40}}""")
    @test r.ok && r.text == "noted a stuck task"
    @test r.input_tokens == 1200 && r.output_tokens == 40 && r.session_id == "s1"
    # error result surfaces the message; garbage is a clean failure, not a throw
    e = Cecelia._parse_claude_result("""{"is_error":true,"result":"tool failed"}""")
    @test !e.ok && occursin("tool failed", e.error)
    g = Cecelia._parse_claude_result("not json")
    @test !g.ok && g.input_tokens == 0

    # stale-session detection: a pruned/expired --resume id makes the CLI say "No conversation
    # found with session ID: …" → run_observer_turn drops the id and retries fresh (self-heal).
    @test Cecelia._is_stale_session_error(
        "No conversation found with session ID: 0df65af8-ae13-4ec5-964a-7231cd8bf005")
    @test Cecelia._is_stale_session_error("no conversation found with session id: x")  # case-insensitive
    @test !Cecelia._is_stale_session_error("agent exited 1")                            # other failures don't retry
    @test !Cecelia._is_stale_session_error("tool failed")

    # MCP config points the spawned agent at the same cecelia_mcp server + this API
    cfg = Cecelia.observer_mcp_config("/repo/mcp", "/env/python", "http://127.0.0.1:8080")
    srv = cfg["mcpServers"]["cecelia-observer"]
    @test srv["command"] == "/env/python"
    @test srv["args"] == ["-m", "cecelia_mcp.server"]
    @test srv["env"]["PYTHONPATH"] == "/repo/mcp"
    @test srv["env"]["CECELIA_API_URL"] == "http://127.0.0.1:8080"
    # the wrapper is built FROM the spec — one source of truth for both the --mcp-config file and
    # `claude mcp add-json` (which takes the bare spec)
    @test srv == Cecelia.observer_mcp_spec("/repo/mcp", "/env/python", "http://127.0.0.1:8080")

    # one-click terminal setup: add-json is not idempotent, so registering is remove-then-add at
    # user scope, and both commands must name the SAME server as the config/--allowedTools filter
    reg = Cecelia._build_mcp_register_cmd(a, "{\"command\":\"/env/python\"}")
    @test reg.exec == [a.bin, "mcp", "add-json", Cecelia.OBSERVER_MCP_NAME,
                       "{\"command\":\"/env/python\"}", "-s", "user"]
    rm_cmd = Cecelia._build_mcp_remove_cmd(a)
    @test rm_cmd.exec == [a.bin, "mcp", "remove", Cecelia.OBSERVER_MCP_NAME, "-s", "user"]
    @test Cecelia._build_mcp_register_cmd(a, "{}"; scope = "local").exec[end] == "local"
    # the restore path uses the SAME add command, so a prior entry can be put back verbatim
    @test Cecelia._build_mcp_register_cmd(a, "{\"command\":\"/old/python\"}").exec[5] ==
          "{\"command\":\"/old/python\"}"
    @test Cecelia.OBSERVER_MCP_NAME == "cecelia-observer"   # the name users see in `claude mcp list`

    # ── Windows CLI resolution + spawn wrapping ──────────────────────────────────────────
    # The observer was invisible on Windows: `Sys.which` only tries the bare name plus `.exe`/
    # `.com` (base/sysinfo.jl), never `.cmd`/`.bat`, so an npm-installed `claude.cmd` was never
    # found → available:false → "Set up my terminal" told users WITH Claude Code to install it.
    # And a `.cmd` cannot be spawned directly — CreateProcess refuses batch files.
    # These helpers take `iswin` explicitly so BOTH platforms' behaviour is asserted from any
    # host — the reason this shipped broken is that nobody could exercise the Windows path.
    @test Cecelia._agent_bin_candidates("claude", false) == ["claude"]        # unix: as given
    @test Cecelia._agent_bin_candidates("claude", true) ==
          ["claude", "claude.cmd", "claude.bat"]                             # windows: + batch shims
    # an explicit extension is Sys.which's job already — don't append to it
    @test Cecelia._agent_bin_candidates("claude.exe", true) == ["claude.exe"]
    @test Cecelia._agent_bin_candidates("claude.cmd", true) == ["claude.cmd"]

    # only batch files need the shell, and only on Windows
    @test Cecelia._needs_cmd_shell("C:/n/claude.cmd", true)
    @test Cecelia._needs_cmd_shell("C:/n/claude.BAT", true)                  # extension case-insensitive
    @test !Cecelia._needs_cmd_shell("C:/n/claude.exe", true)
    @test !Cecelia._needs_cmd_shell("/usr/bin/claude", false)
    @test !Cecelia._needs_cmd_shell("/usr/bin/claude.cmd", false)            # never on unix

    # argv rewriting: argv[1] becomes the resolved path, and a batch file gains `cmd /c`
    logical = ["claude", "mcp", "add-json", "cecelia-observer", "{}", "-s", "user"]
    @test Cecelia._agent_spawn_argv(logical, "/usr/bin/claude", false) ==
          ["/usr/bin/claude", "mcp", "add-json", "cecelia-observer", "{}", "-s", "user"]
    @test Cecelia._agent_spawn_argv(logical, "C:/npm/claude.cmd", true) ==
          ["cmd", "/c", "C:/npm/claude.cmd", "mcp", "add-json", "cecelia-observer", "{}", "-s", "user"]
    @test Cecelia._agent_spawn_argv(logical, "C:/p/claude.exe", true) ==
          ["C:/p/claude.exe", "mcp", "add-json", "cecelia-observer", "{}", "-s", "user"]
    # unresolvable → argv untouched, so the spawn fails naming what the user configured
    @test Cecelia._agent_spawn_argv(logical, nothing, true) == logical
    @test Cecelia._agent_spawn_argv(String[], nothing, true) == String[]
    # the spec argument must survive rewriting verbatim — it is the whole payload
    @test Cecelia._agent_spawn_argv(logical, "C:/npm/claude.cmd", true)[7] == "{}"

    # live resolver: an absolute path to a real executable resolves to itself; nonsense is nothing.
    # (Uses this Julia's own binary — no assumption about what is on PATH.)
    let jl = joinpath(Sys.BINDIR, Sys.iswindows() ? "julia.exe" : "julia")
        isfile(jl) && @test Cecelia.agent_bin_path(jl) == jl
    end
    @test Cecelia.agent_bin_path("") === nothing
    @test Cecelia.agent_bin_path("cecelia-definitely-no-such-binary-42") === nothing

    # Is the user's own terminal set up? Drives which button the lab-log toolbar shows, so the
    # three states must be exact. A stale entry (another checkout's python, or no/!matching
    # CECELIA_API_URL) is NOT "set up" — it fails silently in the user's session.
    want = Cecelia.observer_mcp_spec("/repo/mcp", "/env/python", "http://127.0.0.1:8080")
    @test Cecelia.observer_registration_state(nothing, want) === :missing
    registered = JSON3.read(JSON3.write(merge(want, Dict("type" => "stdio"))))  # as Claude stores it
    @test Cecelia.observer_registration_state(registered, want) === :current    # extra keys are fine
    other_py = JSON3.read(JSON3.write(Cecelia.observer_mcp_spec("/repo/mcp", "/OTHER/python",
                                                               "http://127.0.0.1:8080")))
    @test Cecelia.observer_registration_state(other_py, want) === :stale        # moved checkout
    other_port = JSON3.read(JSON3.write(Cecelia.observer_mcp_spec("/repo/mcp", "/env/python",
                                                                 "http://127.0.0.1:9999")))
    @test Cecelia.observer_registration_state(other_port, want) === :stale      # different port
    # the real-world case that bit an early manual registration: PYTHONPATH set, CECELIA_API_URL absent
    no_url = JSON3.read(JSON3.write(Dict("command" => "/env/python", "args" => ["-m", "cecelia_mcp.server"],
                                         "env" => Dict("PYTHONPATH" => "/repo/mcp"))))
    @test Cecelia.observer_registration_state(no_url, want) === :stale
    @test Cecelia.observer_registration_state(Dict{String,Any}("command" => "/env/python"), want) === :stale

    # config path honours Claude Code's own env override; missing file → not set up, never an error
    withenv("CLAUDE_CONFIG_DIR" => "/tmp/cc-cfg") do
        @test Cecelia.claude_config_path() == joinpath("/tmp/cc-cfg", ".claude.json")
    end
    withenv("CLAUDE_CONFIG_DIR" => nothing) do
        @test Cecelia.claude_config_path() == joinpath(homedir(), ".claude.json")
    end
    @test Cecelia.read_registered_observer_spec(joinpath(mktempdir(), "nope.json")) === nothing
    let bad = joinpath(mktempdir(), "bad.json")
        write(bad, "not json at all")
        @test Cecelia.read_registered_observer_spec(bad) === nothing     # another tool's file — tolerate
    end
    let cfgf = joinpath(mktempdir(), ".claude.json")
        write(cfgf, JSON3.write(Dict("mcpServers" => Dict(Cecelia.OBSERVER_MCP_NAME => want))))
        @test Cecelia.observer_registration_state(
            Cecelia.read_registered_observer_spec(cfgf), want) === :current
        # this reader is user-scope ONLY — a local-scope entry is not a registration for it to find
        write(cfgf, JSON3.write(Dict("projects" => Dict("/somewhere" =>
            Dict("mcpServers" => Dict(Cecelia.OBSERVER_MCP_NAME => want))))))
        @test Cecelia.read_registered_observer_spec(cfgf) === nothing
    end

    # ── Local-scope shadowing ────────────────────────────────────────────────────────────
    # The bug: our button writes `-s user`, but Claude Code resolves `local` scope
    # (projects[<dir>].mcpServers) FIRST. A leftover local entry pointing at a DELETED checkout
    # therefore killed the server with ENOENT for every session started in that dir — while the
    # status route, reading only the top level, reported :current and offered "Chat to Claude".
    stale_local = Cecelia.observer_mcp_spec("/gone/mcp", "/gone/python", "http://127.0.0.1:8080")
    let cfgf = joinpath(mktempdir(), ".claude.json")
        write(cfgf, JSON3.write(Dict(
            "mcpServers" => Dict(Cecelia.OBSERVER_MCP_NAME => want),
            "projects"   => Dict(
                "/home/u"       => Dict("mcpServers" => Dict(Cecelia.OBSERVER_MCP_NAME => stale_local)),
                "/home/u/right" => Dict("mcpServers" => Dict(Cecelia.OBSERVER_MCP_NAME => want)),
                "/home/u/none"  => Dict("mcpServers" => Dict{String,Any}()),
                "/home/u/other" => Dict("mcpServers" => Dict("something-else" => want))))))
        locals = Cecelia.read_local_observer_specs(cfgf)
        @test sort(String[d for (d, _) in locals]) == ["/home/u", "/home/u/right"]
        # only the MISMATCHED one is a problem: a local entry equal to `want` resolves to the same
        # server, so it is left alone — we never delete config that isn't breaking anything
        @test Cecelia.observer_shadow_dirs(locals, want) == ["/home/u"]
        # user scope is still read independently of any of this
        @test Cecelia.observer_registration_state(
            Cecelia.read_registered_observer_spec(cfgf), want) === :current
    end
    # tolerant like the user-scope reader — a missing/garbage/odd-shaped config is "no shadows",
    # never an exception (it is another tool's file and this runs on every status poll)
    @test isempty(Cecelia.read_local_observer_specs(joinpath(mktempdir(), "nope.json")))
    let bad = joinpath(mktempdir(), "bad.json")
        write(bad, "not json at all")
        @test isempty(Cecelia.read_local_observer_specs(bad))
    end
    let odd = joinpath(mktempdir(), "odd.json")
        write(odd, JSON3.write(Dict("projects" => "a string, not an object")))
        @test isempty(Cecelia.read_local_observer_specs(odd))
    end
    @test Cecelia.observer_shadow_dirs(Pair{String,Any}[], want) == String[]
    # sorted → the folder list the UI reports (and the removal order) is deterministic
    @test Cecelia.observer_shadow_dirs(
        Pair{String,Any}["/b" => stale_local, "/a" => stale_local], want) == ["/a", "/b"]

    # `claude mcp remove -s local` acts on the process's CWD, so the cleanup must be able to say
    # WHERE it runs — and the spawn wrapper must not drop that when it rewrites argv (it rebuilds
    # the Cmd, which is exactly how a `dir` gets silently lost and the wrong scope edited)
    @test Cecelia._build_mcp_remove_cmd(a; scope = "local", dir = "/home/u").exec[end] == "local"
    @test Cecelia._build_mcp_remove_cmd(a; scope = "local", dir = "/home/u").dir == "/home/u"
    @test isempty(Cecelia._build_mcp_remove_cmd(a).dir)                  # unchanged default
    @test Cecelia._agent_spawn_cmd(
        Cecelia._build_mcp_remove_cmd(a; scope = "local", dir = "/home/u")).dir == "/home/u"
    # a dir that no longer exists is skipped, not attempted (Claude ignores its entry too)
    @test Cecelia.remove_shadowing_observer_mcps(a, ["/no/such/dir/at/all"]) == (String[], String[])

    # the prompt carries the project + the discipline rules
    fp = Cecelia.observer_feedback_prompt("NRUBxU")
    @test occursin("NRUBxU", fp) && occursin("append_lab_log", fp) && occursin("[Claude]", fp)
    # §1 param-suggestion guidance is present: on an outlier, use get_module_params + the trail to
    # suggest a param direction — framed as a suggestion, current-state only (not a prediction).
    @test occursin("get_module_params", fp) && occursin("suggest", fp)
    # The artifacts it can author are named, so the agent knows they exist — the prompt listed
    # neither for a while, and an unmentioned tool is an unused one.
    @test occursin("create_notebook", fp) && occursin("create_chain", fp)
    # the Phase-0 read tools + the discipline that makes them worth having (a figure proposed without
    # checking how the images are annotated throws the experiment's design away)
    @test occursin("get_image_attributes", fp) && occursin("get_analysis_boards", fp)
    # match on unwrapped text — the prompt is hard-wrapped, so a phrase can straddle a newline
    @test occursin("not four replicates", replace(fp, r"\s+" => " "))
    # …and the boundary is stated: authoring a chain is not running it. This is the line that keeps
    # the assistant from telling a user their pipeline has started.
    @test occursin("cannot start", fp) || occursin("cannot rename", fp)
    @test occursin("press Run", fp)
end

@testset "the two observer prompts name the same MCP tools" begin
    # THE recurring bug in this area, twice now: an MCP tool is added, ONE of the two prompts is
    # updated, and the other silently goes stale — an unmentioned tool is an unused one, so the
    # capability just never gets offered. It surfaced both times only because Dominik pasted his
    # Chat-to-Claude prompt and noticed something missing (create_chain the first time,
    # get_analysis_boards/get_image_attributes the second).
    #
    # A warning comment inside one of the files cannot fix this — you only read it if you already knew
    # the other file existed. So the invariant is enforced here instead, across all three sources.
    # Same idea as "calibration writers agree across languages": two implementations that cannot call
    # each other get a test that compares them.
    #
    # The prompts are NOT required to name the same set — they address different surfaces — but every
    # difference must be DELIBERATE and listed below. Adding a tool to one prompt and not the other now
    # fails here, naming the offender.
    root = normpath(joinpath(@__DIR__, "..", ".."))
    server = read(joinpath(root, "mcp", "cecelia_mcp", "server.py"), String)
    jl     = read(joinpath(root, "app", "src", "ai", "observer_prompt.jl"), String)
    ts     = read(joinpath(root, "frontend", "src", "lib", "chatHandoff.ts"), String)

    tools = Set(String[m.captures[1] for m in eachmatch(r"@mcp\.tool\(\)\s*\ndef (\w+)", server)])
    @test length(tools) >= 30                          # anti-vacuity: a bad regex must not pass
    named_jl = Set(t for t in tools if occursin(t, jl))
    named_ts = Set(t for t in tools if occursin(t, ts))

    # In the IN-APP prompt only — it drives the autonomous observer loop, which the user's own session
    # has no use for.
    @test setdiff(named_jl, named_ts) == Set(["poll_observations"])
    # In the CHAT HAND-OFF only — orientation + authoring aids for a session the user starts fresh;
    # the in-app agent is already oriented and does not browse notebooks.
    # (`get_available_plots` left this list when add_analysis_board landed — the in-app observer needs
    # the spec ids and each spec's chart types to author a board, so both prompts now name it.)
    # LabArchives is CHAT-ONLY, and structurally so: the in-app agent is spawned with `--mcp-config`
    # listing ONLY cecelia-observer, so it has no LabArchives connector and could never read an ELN.
    # Mentioning the tools there would advertise a capability that build cannot have.
    @test setdiff(named_ts, named_jl) ==
        Set(["get_session_briefing", "list_notebooks", "set_notebook_description",
             "get_labarchives_context", "set_labarchives_context"])
    # Named by NEITHER: per-image detail an agent reaches for from a summary rather than from the
    # prompt, plus the observer's own bookkeeping.
    @test setdiff(tools, union(named_jl, named_ts)) ==
        Set(["get_image_notes", "get_observer_stats", "get_qc_metrics", "get_spatial_stats",
             "set_observer_active"])
    # Every WRITE must be offered by both: a mutating capability nobody mentions is a capability the
    # assistant never uses, which is how create_chain sat unmentioned in the hand-off.
    # (set_labarchives_context is deliberately absent — see the chat-only note above.)
    for w in ("append_lab_log", "create_notebook", "revise_notebook", "create_chain",
              "add_analysis_board")
        @test w in named_jl && w in named_ts
    end
end

@testset "MCP connections — enumerate whatever is registered" begin
    # Generic on purpose: it lists what's in the config rather than looking for names we know, so a
    # connector added later needs no change here. Backs Settings → MCP connections.
    dir = mktempdir()
    cfg = joinpath(dir, ".claude.json")

    @test isempty(mcp_connections(joinpath(dir, "nope.json")))     # no config → no rows, never throws
    write(cfg, "{not json")
    @test isempty(mcp_connections(cfg))                            # another tool's file: tolerate junk

    write(cfg, """
    {"mcpServers": {"cecelia-observer": {"command": "py"}, "other-tool": {"url": "https://x/mcp"}},
     "projects": {"/tmp/p1": {"mcpServers": {"cecelia-observer": {"command": "old"}}},
                  "/tmp/p2": {}}}
    """)
    rows = mcp_connections(cfg)
    @test length(rows) == 3                                        # 2 user-scope + 1 local-scope
    names = [r["name"] for r in rows]
    @test "other-tool" in names                                    # a server we know nothing about still lists
    obs = [r for r in rows if r["name"] == "cecelia-observer"]
    @test length(obs) == 2 && Set(r["scope"] for r in obs) == Set(["user", "local"])
    @test all(r["ours"] for r in obs)                              # ours is flagged, others are not
    @test !first(r["ours"] for r in rows if r["name"] == "other-tool")
    # transport is inferred so an http connector doesn't render as a stdio one
    @test first(r["transport"] for r in rows if r["name"] == "other-tool") == "http"
    @test first(r["dir"] for r in rows if r["scope"] == "local") == "/tmp/p1"
    @test rows == sort(rows; by = r -> (r["name"], r["scope"], r["dir"]))   # deterministic order
end

@testset "LabArchives context sidecar (round-trip, gaps, briefing)" begin
    proj = create_project!(name = "la-$(rand(1000:9999))")
    s = add_set!(proj; name = "s1")

    # no sidecar → present=false, no gaps, and the briefing OMITS the key entirely (so "not linked"
    # and "linked but empty" stay distinguishable).
    d0 = read_la_doc(proj)
    @test d0["present"] == false && d0["readable"] == true
    @test isempty(la_gaps(proj))
    @test la_briefing(proj) === nothing
    @test !haskey(session_briefing(proj), :labarchives)

    # two images, both Treatment=MERTK — the WT arm the ELN declares has NO images.
    for (nm, mouse) in (("m1", "1"), ("m2", "2"))
        img = add_image!(s; name = nm)
        img.attr = Dict("Treatment" => "MERTK", "Mouse" => mouse)
        save!(img)
    end

    doc = write_la_doc!(proj;
        source   = Dict("notebookId" => "nb1", "notebookName" => "Ailsa",
                        "pageIds" => ["p1"], "url" => "https://example/nb"),
        sections = [Dict("heading" => "Setup", "lines" => ["LHS immunised only", "2 sites/mouse"],
                         "sourceDate" => "2026-02-24")],
        cohort   = [Dict("attr" => "Treatment", "value" => "WT", "n" => 6),
                    Dict("attr" => "Treatment", "value" => "MERTK", "n" => 5)])
    @test doc["source"]["notebookName"] == "Ailsa" && doc["source"]["pageIds"] == ["p1"]
    @test !isempty(doc["syncedAt"])

    # round-trips through disk with String keys intact (JSON3 hands back Symbols — json_native)
    r = read_la_doc(proj)
    @test r["present"] == true && r["readable"] == true
    @test r["sections"][1]["heading"] == "Setup"
    @test r["sections"][1]["lines"] == ["LHS immunised only", "2 sites/mouse"]

    # THE case this feature exists for: the declared WT arm has no images, and nothing in the project
    # would otherwise show it — attribute levels are derived from the images PRESENT.
    @test [v for (v, _) in Dict(attr_value_counts(images(proj)))["Treatment"]] == ["MERTK"]
    g = la_gaps(proj)
    @test length(g) == 1
    @test g[1]["attr"] == "Treatment" && g[1]["value"] == "WT"
    @test g[1]["declared"] == 6 && g[1]["present"] == 0

    # briefing carries HEADINGS + gaps, never the section text
    b = la_briefing(proj)
    @test b.sections == ["Setup"] && length(b.gaps) == 1 && b.notebookName == "Ailsa"
    sb = session_briefing(proj)
    @test haskey(sb, :labarchives) && sb.labarchives.sections == ["Setup"]

    # a full REPLACE, not a merge — a section deleted in the ELN must not linger
    write_la_doc!(proj; source = Dict("notebookName" => "Ailsa"),
                  sections = [Dict("heading" => "Question", "lines" => ["nuclear vs cytoplasmic"])],
                  cohort = [])
    r2 = read_la_doc(proj)
    @test [s["heading"] for s in r2["sections"]] == ["Question"]
    @test isempty(la_gaps(proj))            # cohort cleared → nothing to be missing

    # bounds are the WRITER's call, not the caller's
    r3 = write_la_doc!(proj; sections = [Dict("heading" => "H", "lines" => ["x" for _ in 1:50])])
    @test length(r3["sections"][1]["lines"]) == Cecelia.LA_MAX_LINES
    r4 = write_la_doc!(proj; sections = [Dict("heading" => "H$i", "lines" => ["l"]) for i in 1:40])
    @test length(r4["sections"]) == Cecelia.LA_MAX_SECTIONS

    # a corrupt sidecar reads as present-but-UNREADABLE, never as "no context"
    write(la_doc_path(proj), "{not json")
    rbad = read_la_doc(proj)
    @test rbad["present"] == true && rbad["readable"] == false
    @test la_briefing(proj).readable == false

    # the sidecar must never have touched the lab log — that stays append-only
    @test !isfile(lab_log_path(proj)) || isempty(read_lab_log(proj))
end

@testset "AI observer session sidecar (tokens + clear)" begin
    proj = create_project!(name = "obs-sess-$(rand(1000:9999))")
    # fresh project → zeroed session
    s0 = read_observer_session(proj)
    @test s0["sessionId"] == "" && s0["inputTokens"] == 0 && s0["turns"] == 0

    # a turn adopts the session id + accumulates tokens
    record_observer_turn!(proj, "sessABC", 1000, 40)
    s1 = read_observer_session(proj)                       # re-read from disk (persisted)
    @test s1["sessionId"] == "sessABC" && s1["inputTokens"] == 1000 && s1["outputTokens"] == 40
    @test s1["turns"] == 1
    # a second turn accumulates; an EMPTY session id keeps the existing one
    record_observer_turn!(proj, "", 500, 10)
    s2 = read_observer_session(proj)
    @test s2["sessionId"] == "sessABC"                     # unchanged (empty id kept prior)
    @test s2["inputTokens"] == 1500 && s2["outputTokens"] == 50 && s2["turns"] == 2

    # activity log: every pass is recorded (newest-first), even a silent/failed one
    log_observer_pass!(proj; trigger = "manual", model = "sonnet", ok = true, appended = false,
                       input_tokens = 900, output_tokens = 20, note = "reviewed — nothing to flag")
    log_observer_pass!(proj; trigger = "auto", model = "haiku", ok = true, appended = true,
                       input_tokens = 700, output_tokens = 30, note = "flagged clustTracks failed 4×")
    ps = read_observer_session(proj)["passes"]
    @test length(ps) == 2
    @test ps[1]["trigger"] == "auto" && ps[1]["appended"] == true          # newest-first
    @test ps[1]["model"] == "haiku" && ps[1]["inputTokens"] == 700
    @test ps[2]["trigger"] == "manual" && ps[2]["appended"] == false
    @test occursin("nothing to flag", ps[2]["note"])

    # clear resets everything (next run forks a fresh session), incl. the activity log
    cleared = clear_observer_session!(proj)
    @test cleared["sessionId"] == "" && cleared["inputTokens"] == 0 && cleared["turns"] == 0
    @test isempty(cleared["passes"])
    @test read_observer_session(proj)["inputTokens"] == 0
    rm(proj.root; recursive = true)
end

# ── Model: create project and image ───────────────────────────────────────
@testset "Model round-trip" begin
    proj = create_project!(name="smoke-test-$(rand(1000:9999))")
    @test isdir(proj.root)
    @test isfile(joinpath(proj.root, "project.json"))

    s = add_set!(proj; name="set-A")
    @test isdir(s._dir)

    img = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path" => "/tmp/fake.tif"))
    @test isdir(img._dir)
    @test isfile(joinpath(img._dir, "ccid.json"))

    # Round-trip via init_object
    loaded = init_object(proj.uid, img.uid)
    @test loaded isa CciaImage
    @test loaded.name == "img-1"
    @test get(loaded.meta, "ori_path", "") == "/tmp/fake.tif"

    # image_by_uid(...; uid) convenience accessor (REPL/notebook lookup)
    @test image_by_uid(proj; uid = img.uid) === img
    @test image_by_uid(s; uid = img.uid) === img
    @test image_by_uid(proj; uid = "nope") === nothing
    @test image_by_uid(s; uid = "nope") === nothing

    # Cleanup
    rm(proj.root; recursive=true)
end

# ── Starred: a plain per-image bookmark ───────────────────────────────────────────────────────────
# ANY number of images can be starred, and nothing downstream reads it — it drives the Starred row
# filter and nothing else. It replaced a SET-level single "reference image" nomination, from which
# the 8-bit import used to derive one intensity window for the whole set; that coupling is gone, so
# the load-bearing property here is just that the flag round-trips and is independent per image.
@testset "starred images" begin
    proj = create_project!(name = "star-$(rand(1000:9999))")
    s    = add_set!(proj; name = "set")
    img1 = add_image!(s; name = "a")
    img2 = add_image!(s; name = "b")

    @test img1.starred == false                                 # not starred is the default
    @test img2.starred == false

    img1.starred = true
    img2.starred = true                                         # multi-select: not one per set
    save!(img1); save!(img2)

    rs = load_project(proj.uid)._sets[1]
    @test all(i -> i.starred, rs._images)                       # persisted through ccid.json

    img1.starred = false
    save!(img1)
    reloaded = load_project(proj.uid)._sets[1]._images
    @test count(i -> i.starred, reloaded) == 1                  # independent per image

    rm(proj.root; recursive=true)
end
@testset "move_image! (manifest-only, no data moved)" begin
    proj = create_project!(name="move-test-$(rand(1000:9999))")
    a = add_set!(proj; name="set-A")
    b = add_set!(proj; name="set-B")
    img1 = add_image!(a; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))
    img2 = add_image!(a; name="img-2", meta=Dict{String,Any}("ori_path"=>"/tmp/b.tif"))
    data_dir = joinpath(proj.root, "0", img1.uid)   # image data + metadata dirs are UID-keyed
    meta_dir = joinpath(proj.root, "1", img1.uid)

    move_image!(proj, img1.uid, a.uid, b.uid)

    # membership moved …
    @test a.image_uids == [img2.uid]
    @test b.image_uids == [img1.uid]
    @test image_by_uid(a; uid=img1.uid) === nothing
    @test image_by_uid(b; uid=img1.uid) !== nothing
    # … but NO data moved on disk (dirs are UID-keyed, independent of the set)
    @test isdir(data_dir) && isdir(meta_dir)
    @test !isdir(joinpath(b._dir, img1.uid))   # sets never nest image dirs

    # persists: reload the project fresh and the manifests reflect the move
    reloaded = load_project(proj.uid)
    ra = reloaded._sets[findfirst(s -> s.uid == a.uid, reloaded._sets)]
    rb = reloaded._sets[findfirst(s -> s.uid == b.uid, reloaded._sets)]
    @test ra.image_uids == [img2.uid]
    @test rb.image_uids == [img1.uid]
    @test image_by_uid(reloaded; uid=img1.uid) !== nothing   # still findable project-wide

    # idempotent / no-op guards
    @test move_image!(proj, img1.uid, b.uid, b.uid) === proj        # same set → no-op
    move_image!(proj, img1.uid, a.uid, b.uid)                        # already in dest → no-op
    @test b.image_uids == [img1.uid]                                 # not duplicated
    # error cases
    @test_throws ErrorException move_image!(proj, "nope", a.uid, b.uid)   # image not in source
    @test_throws ErrorException move_image!(proj, img2.uid, a.uid, "gone") # dest missing

    rm(proj.root; recursive=true)
end

# ── REPL / notebook data-access surface (Observer Phase 2 foundation) ─────────
@testset "REPL API surface + generated doc" begin
    # every allow-listed accessor is defined, exported, and documented — the notebook-facing
    # surface must be complete (a rename/removal or a missing docstring fails here).
    ref = repl_api_reference()
    @test !isempty(ref)
    for e in ref
        @test isdefined(Cecelia, Symbol(e.name))
        @test e.exported
        @test e.documented          # has a real docstring (undocumented accessors are a bug)
        @test !occursin("value*name", e.doc)   # raw docstring, not the mangled re-render
    end

    # GOLDEN: docs/REPL.md's generated section is in sync with the live docstrings. If this fails,
    # someone changed a listed function's docstring (or the list) without regenerating — run
    # `Cecelia.write_repl_doc()`. This is the drift-guard that keeps REPL.md honest.
    p = Cecelia.repl_doc_path()
    if isfile(p)
        committed = read(p, String)
        # Compare line-ending agnostically: this is a CONTENT drift-guard, not a byte-exactness
        # check. On Windows git checks the file out with CRLF while `repl_api_section()` emits LF,
        # so the splice mismatched on every line and the test failed for a reason that has nothing
        # to do with docstring drift.
        _lf(s) = replace(s, "\r\n" => "\n")
        @test _lf(committed) == _lf(Cecelia.render_repl_doc(committed))
        @test occursin(Cecelia.REPL_DOC_BEGIN, committed)
    else
        @test_skip "docs/REPL.md not found at $p"
    end
end

# ── Run log (automatic per-image provenance) ─────────────────────────────────
@testset "Run log" begin
    proj = create_project!(name="runlog-test-$(rand(1000:9999))")
    s = add_set!(proj; name="set-A")
    img = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path" => "/tmp/fake.tif"))

    @test read_run_log(img) == Any[]                       # empty before any run
    append_run_log!(img, "segment.cellpose", "default")
    append_run_log!(img, "behaviour.hmm")
    log = read_run_log(img)
    @test length(log) == 2
    @test log[1]["fun"] == "segment.cellpose"
    @test log[1]["valueName"] == "default"
    @test haskey(log[1], "at") && !isempty(log[1]["at"])
    @test log[2]["fun"] == "behaviour.hmm"

    # persists across reload (init_object)
    loaded = init_object(proj.uid, img.uid)
    @test length(read_run_log(loaded)) == 2

    # capped to the most recent RUN_LOG_CAP entries
    for i in 1:(Cecelia.RUN_LOG_CAP + 10); append_run_log!(img, "x.$i"); end
    capped = read_run_log(img)
    @test length(capped) == Cecelia.RUN_LOG_CAP
    @test capped[end]["fun"] == "x.$(Cecelia.RUN_LOG_CAP + 10)"   # newest kept

    # params trail: entry carries the sanitised task params; internal `_…` keys and the redundant
    # `valueName` are dropped, real tuning knobs kept (Observer Phase 2 §1 — see docs/ai-assist/OBSERVER.md).
    img2 = add_image!(s; name="img-2", meta=Dict{String,Any}("ori_path" => "/tmp/fake2.tif"))
    append_run_log!(img2, "tracking.bayesian_tracking", "default", "done",
                    Dict{String,Any}("search_radius" => 5.0, "max_lost" => 3,
                                     "valueName" => "default", "_task_id" => "abc123"))
    e = read_run_log(img2)[end]
    @test e["params"]["search_radius"] == 5.0
    @test e["params"]["max_lost"] == 3
    @test !haskey(e["params"], "valueName")    # redundant with its own field
    @test !haskey(e["params"], "_task_id")      # internal, dropped
    # default (no params) → shape-stable empty dict, and it survives reload
    append_run_log!(img2, "behaviour.hmm")
    @test read_run_log(img2)[end]["params"] == Dict{String,Any}()
    @test read_run_log(init_object(proj.uid, img2.uid))[end-1]["params"]["search_radius"] == 5.0
    # sanitiser handles nothing/empty directly
    @test Cecelia._run_log_params(nothing) == Dict{String,Any}()
    @test Cecelia._run_log_params(Dict("_x" => 1, "keep" => 2)) == Dict{String,Any}("keep" => 2)

    rm(proj.root; recursive=true)
end

# ── Session briefing + all_qc_docs (Observer Phase 2 §2) ─────────────────────
@testset "Session briefing + all_qc_docs" begin
    proj = create_project!(name="brief-$(rand(1000:9999))")
    s = add_set!(proj; name="set-A")
    img1 = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))
    img2 = add_image!(s; name="img-2", meta=Dict{String,Any}("ori_path"=>"/tmp/b.tif"))
    # suppress the calibration fallback (these fixtures have no PhysicalSize) by persisting an
    # empty omezarr QC doc — so the only flag is the one we add explicitly. Also tests "persisted wins".
    for im in (img1, img2)
        write_qc(im, "importImages.omezarr", "default", Dict{String,Any}[])
    end
    write_qc(img1, "tracking.bayesian_tracking", "default",
             [qc_finding("warn", "few_tracks", "Few tracks", "Only 5 tracks")])
    append_lab_log!(proj, "User", ["started tracking run"])

    b = session_briefing(proj)
    @test b.projectUid == proj.uid && b.projectName == proj.name && b.imageCount == 2
    uids = [f.uid for f in b.flagged]
    @test img1.uid in uids && !(img2.uid in uids)     # only the warn image flags; clean stays clean
    f1 = b.flagged[findfirst(f -> f.uid == img1.uid, b.flagged)]
    @test f1.worst == "warn" && f1.findings[1].short == "Few tracks"
    @test length(b.recentLabLog) == 1 && b.recentLabLog[1].author == "User"
    @test occursin("tracking", b.recentLabLog[1].summary)

    # all_qc_docs: a fresh image (no persisted omezarr) gets the computed calibration fallback
    img3 = add_image!(s; name="img-3", meta=Dict{String,Any}("ori_path"=>"/tmp/c.tif"))
    @test haskey(all_qc_docs(img3), "importImages.omezarr/default")
    @test haskey(all_qc_docs(img1), "importImages.omezarr/default")   # persisted present too

    rm(proj.root; recursive=true)
end

# ── Lockfile (naive guard) ──────────────────────────────────────────────────
@testset "with_transaction" begin
    proj     = create_project!(name="lock-test-$(rand(1000:9999))")
    # the lockfile is DERIVED from the object's state file (the R `getStateFile` + ".lock" shape),
    # not a hardcoded name — that's what makes the per-image form below possible at all
    lockfile = Cecelia.state_file(proj) * ".lock"
    @test lockfile == Cecelia._lock_path(proj)

    # happy path: returns the body value and releases the lock
    @test (with_transaction(proj) do; 42; end) == 42
    @test !isfile(lockfile)

    # lock is released even when the body throws (no leaked lock)
    @test_throws ErrorException with_transaction(proj) do; error("boom"); end
    @test !isfile(lockfile)

    # works for an IMAGE too, locking that image alone — different images never block each other
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="i")
    img2 = add_image!(s; name="i2")
    @test Cecelia._lock_path(img) == Cecelia.state_file(img) * ".lock"
    @test Cecelia._lock_path(img) != Cecelia._lock_path(proj)
    @test Cecelia._lock_path(img) != Cecelia._lock_path(img2)
    @test (with_transaction(img) do; 7; end) == 7
    @test !isfile(Cecelia._lock_path(img))

    # REENTRANT: a commit reached from inside another transaction on the SAME object must not
    # deadlock. Before the in-process lock this sat on its own lockfile until the timeout.
    @test (with_transaction(img) do
               with_transaction(img) do; 9; end
           end) == 9
    @test !isfile(Cecelia._lock_path(img))

    # ANOTHER PROCESS's fresh lockfile is respected — we wait, then fail naming the file
    touch(Cecelia._lock_path(img))
    err = try; with_transaction(img; timeout = 1) do; 1; end; "" catch e; sprint(showerror, e) end
    @test occursin(Cecelia._lock_path(img), err)
    @test occursin("stale lockfile", err)
    rm(Cecelia._lock_path(img); force = true)

    # ...but an ABANDONED one (process died mid-commit) is reclaimed rather than blocking every
    # later task on that image behind a hidden file. Safe only because a transaction now wraps the
    # short commit, never the computation — so nothing legitimate is ever this old.
    @test !Cecelia._lock_abandoned(time())                                   # fresh → keep waiting
    @test  Cecelia._lock_abandoned(time() - Cecelia._LOCK_STALE_AFTER - 1)   # abandoned → reclaim

    rm(proj.root; recursive=true)
end

# ── commit_state!: registering an output is atomic against a concurrent registration ────────
# THE lost-update bug. Every task used to hand-roll re-read → poke → write, so
# two tasks finishing on one image both read the old dict and the second write dropped the first's
# field. `write_json_atomic` alone does NOT fix this — each write is individually intact; the
# second is simply built on stale data.
@testset "commit_state! wraps the read-modify-write in the transaction" begin
    proj = create_project!(name="commit-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="i")

    # basic read-modify-write, persisted
    commit_state!(img) do raw
        raw["status"] = "done"
    end
    @test read_ccid_raw(Cecelia.state_file(img))["status"] == "done"

    # The RMW must happen INSIDE the transaction — that is the whole mechanism, and it is what
    # makes a concurrent registration wait instead of reading stale data. Asserted directly and
    # deterministically: while the body runs, this object's lockfile is held.
    #
    # This replaces a thread-interleaving test that did not work. It spawned two tasks and relied on
    # the scheduler to interleave them so one would read stale `labels`; standalone it did (one
    # registration was lost), but inside the suite the second task simply wasn't scheduled during the
    # first's sleep, so it passed with the lock REMOVED. A concurrency test whose failure depends on
    # scheduler luck is worse than none — it reads as coverage. Mutual exclusion itself is covered
    # deterministically by the `with_transaction` testset above (lockfile held, foreign lock
    # respected, reentrancy, abandoned-lock reclaim).
    held = false
    commit_state!(img) do raw
        held = isfile(Cecelia._lock_path(img))     # false if the RMW isn't wrapped
        raw["labels"] = Dict{String,Any}("segA" => ["segA.zarr"])
    end
    @test held
    @test !isfile(Cecelia._lock_path(img))         # ...and released afterwards
    @test haskey(read_ccid_raw(Cecelia.state_file(img))["labels"], "segA")

    # a nested commit on the same object still completes (reentrancy through commit_state!, not just
    # with_transaction) and both mutations land
    commit_state!(img) do raw
        raw["note"] = "outer"
        commit_state!(img) do inner
            inner["status"] = "inner-done"
        end
    end
    fresh = read_ccid_raw(Cecelia.state_file(img))
    @test fresh["note"] == "outer"

    # the lock is released on a throwing body, and the file keeps its previous content
    @test_throws ErrorException commit_state!(img) do raw
        raw["status"] = "clobbered"
        error("boom")
    end
    @test !isfile(Cecelia._lock_path(img))
    @test read_ccid_raw(Cecelia.state_file(img))["status"] == "done"

    # a metadata-dir form for the API layer, which commits without loading the object
    commit_state!(img._dir) do raw
        raw["note"] = "by dir"
    end
    @test read_ccid_raw(Cecelia.state_file(img))["note"] == "by dir"

    rm(proj.root; recursive=true)
end

# ── Durable state writes are atomic ─────────────────────────────────────────
# Every state file (ccid.json, project.json, sidecars, custom.toml, the lab log) is written
# tmp-then-rename via `write_atomic`. The failure this prevents: `open(path, "w")` truncates
# first, so a kill in that window (the Quit button SIGKILLs) left a half-written file — and since
# `_load_set` has no per-image guard, ONE truncated image ccid.json failed the WHOLE project load.
@testset "durable state writes are atomic" begin
    td = mktempdir()

    # a failed write leaves the PREVIOUS content intact, not a truncated file
    p = joinpath(td, "state.json")
    write_json_atomic(p, Dict("a" => 1))
    before = read(p, String)
    @test_throws ErrorException write_atomic(p) do io
        print(io, "{\"partial\":")
        error("killed mid-write")
    end
    @test read(p, String) == before          # untouched, NOT truncated
    @test JSON3.read(read(p, String))[:a] == 1

    # no temp files left behind, on success or on failure
    @test isempty(filter(f -> occursin(".tmp.", f), readdir(td)))

    # a leftover temp (from a process killed between write and rename) must NOT be picked up by
    # sidecar discovery, which is `readdir` + `endswith(f, ".json")` in several places
    @test !endswith(Cecelia.write_atomic(io -> print(io, "x"), joinpath(td, "probe.json")), ".tmp")
    tmpname = "state.json.tmp.abc123"
    @test !endswith(tmpname, ".json")

    # creates a missing parent dir rather than throwing
    deep = joinpath(td, "a", "b", "c.json")
    write_json_atomic(deep, Dict("ok" => true))
    @test JSON3.read(read(deep, String))[:ok] == true

    # write_atomic handles non-JSON content too (TOML config, the lab-log markdown)
    t = joinpath(td, "notes.md")
    write_atomic(io -> print(io, "# hello"), t)
    @test read(t, String) == "# hello"

    rm(td; recursive=true)
end

# ── state_file: one derivation, whatever the caller holds ───────────────────
# The R original kept this private to the object (`getStateFile`); the port had 20+ call sites
# re-deriving `joinpath(obj._dir, "ccid.json")` and the API layer additionally re-spelling the
# `1/` metadata segment. All forms must agree.
# ── resolve_value_name: the `defaultOnly` half of R's cciaImage$valueNames ──────────────────
# Nine call sites hand-rolled `something(value_name, get(img.label_props, "_active", "default"))`,
# hardcoding the `_active` key and the `"default"` fallback that VERSIONED_ACTIVE_KEY /
# VERSIONED_DEFAULT_VAL exist to name. One resolver now, so the fallback rule lives in one place.
@testset "resolve_value_name" begin
    proj = create_project!(name="rvn-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="i")

    # no label_props at all → the versioned default, never an error
    @test resolve_value_name(img) == VERSIONED_DEFAULT_VAL

    # an explicit value_name always wins, even when an active one exists
    img.label_props = Dict{String,String}("A" => "A.h5ad", "B" => "B.h5ad",
                                         VERSIONED_ACTIVE_KEY => "B")
    @test resolve_value_name(img, "A") == "A"
    @test resolve_value_name(img)      == "B"        # else the active one

    # falls back to the versioned default when nothing is marked active
    img.label_props = Dict{String,String}("A" => "A.h5ad")
    @test resolve_value_name(img) == VERSIONED_DEFAULT_VAL

    # agrees with the underlying helper it replaced, and with the list accessor's view
    img.label_props = Dict{String,String}("A" => "A.h5ad", VERSIONED_ACTIVE_KEY => "A")
    @test resolve_value_name(img) == versioned_active(img.label_props)
    @test resolve_value_name(img) in img_value_names(img)

    rm(proj.root; recursive=true)
end

# Migration QC: only the silent-failure case is a finding — an image that migrates with no cell
# table looks successful and leaves every downstream page empty.
@testset "migrate_qc_findings" begin
    @test isempty(migrate_qc_findings(["A"]))
    @test isempty(migrate_qc_findings(["A", "B"]))
    f = migrate_qc_findings(String[])
    @test length(f) == 1
    @test f[1]["level"] == "warn"
    @test f[1]["code"]  == "migrate.no_segmentation"
    @test occursin("re-run", lowercase(f[1]["long"]))   # the long text says what to DO
end

@testset "state_file resolution" begin
    proj = create_project!(name="statefile-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="i")

    @test Cecelia.state_file(proj) == joinpath(proj.root, "project.json")
    @test basename(Cecelia.state_file(img)) == Cecelia.STATE_FILENAME
    @test basename(Cecelia.state_file(s))   == Cecelia.STATE_FILENAME

    # object form == metadata-dir form == (project dir + uid) form
    @test Cecelia.state_file(img) == Cecelia.state_file(img._dir)
    @test Cecelia.state_file(img) == Cecelia.state_file(proj.root, img.uid)
    @test Cecelia.state_file(s)   == Cecelia.state_file(proj.root, s.uid)
    @test Cecelia.obj_meta_dir(proj.root, img.uid) == img._dir

    # and the file the accessor names is the one save! actually wrote
    @test isfile(Cecelia.state_file(img))

    rm(proj.root; recursive=true)
end

# An unreadable state file must name the FILE. JSON3 alone says only "invalid JSON at byte
# position N" — raised from inside a project load, that told the user nothing actionable.
@testset "unreadable state file names the file" begin
    proj = create_project!(name="corrupt-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="i")
    path = Cecelia.state_file(img)

    full = read(path, String)
    write(path, full[1:cld(length(full), 2)])          # truncate, as an interrupted write would
    err = try; load_project(proj.uid); "" catch e; sprint(showerror, e) end
    @test occursin(path, err)                          # says WHICH file
    @test occursin("not valid JSON", err)
    @test occursin(".ccbundle", err)                    # and what to do about it

    rm(proj.root; recursive=true)
end

# Detector, not advisory: a NEW bare `open(<state file>, "w")` fails here. This is how the
# truncating form spread to ~30 sites in the first place — the atomic pattern existed (the gating
# sidecar) but nothing stopped the next writer hand-rolling the unsafe one.
@testset "no hand-rolled state writes" begin
    roots = [joinpath(@__DIR__, "..", "src"), joinpath(@__DIR__, "..", "..", "api", "src")]
    # Every write-mode `open` is an offender unless listed here WITH a reason. Deliberately an
    # allow-list of exact call sites, not of whole files: exempting a file would let the next
    # state write in that file slip through, which is precisely how this spread.
    allowed = Dict(
        # the atomic writer itself — this IS the tmp-then-rename implementation
        "utils.jl"       => [raw"""open(tmp, "w") do io"""],
        # transient per-run params blob handed to a Python subprocess, in the run's task dir
        "py_runner.jl"   => [raw"""open(params_file, "w") do io"""],
        # bundle manifest, written INTO the export staging dir that is then tarred and deleted
        "project_io.jl"  => [raw"""open(joinpath(tmp, BUNDLE_MANIFEST), "w") do io"""],
        # bulk image-data copy (multi-GB, chunked); not state, and the import task owns cleanup
        "omezarr.jl"     => [raw"""open(dst, "w") do d"""],
    )
    offenders = String[]
    for root in roots, (dir, _, files) in walkdir(root), f in files
        endswith(f, ".jl") || continue
        ok = get(allowed, f, String[])
        for (i, line) in enumerate(eachline(joinpath(dir, f)))
            occursin(r"""open\([^)]*,\s*"w"\)""", line) || continue
            startswith(strip(line), "#") && continue
            any(a -> occursin(a, line), ok) && continue
            push!(offenders, "$f:$i: $(strip(line))")
        end
    end
    if !isempty(offenders)
        @warn "Hand-rolled write-mode open — use write_atomic/write_json_atomic, or add an " *
              "allow-list entry with a reason if it genuinely isn't durable state" offenders
    end
    @test isempty(offenders)
end

# ── Lab log (per-project append-only markdown) ──────────────────────────────
@testset "Lab log" begin
    proj = create_project!(name="lablog-test-$(rand(1000:9999))")

    @test read_lab_log(proj) == ""                     # empty before any entry
    @test parse_lab_log("") == Dict{String,Any}[]

    append_lab_log!(proj, "User", "CD4 gate lower bound ~0.25 for this tissue prep")
    append_lab_log!(proj, "Claude", ["Image 7 gated to 23 cells (cohort mean 187)",
                                     "User excluded image"])
    content = read_lab_log(proj)
    @test occursin("[User]", content) && occursin("[Claude]", content)

    # parsed NEWEST-FIRST, date+author injected, bullets captured
    entries = parse_lab_log(content)
    @test length(entries) == 2
    @test entries[1]["author"] == "Claude"             # newest first
    @test entries[1]["lines"] == ["Image 7 gated to 23 cells (cohort mean 187)",
                                  "User excluded image"]
    @test entries[2]["author"] == "User"
    @test occursin(r"^\d{4}-\d{2}-\d{2}$", entries[1]["date"])

    # APPEND-ONLY: a later write never rewrites earlier bytes
    before = read_lab_log(proj)
    append_lab_log!(proj, "User — correction",
                    "Corrects above: image 7 low count is real biology — keep it")
    after = read_lab_log(proj)
    @test startswith(after, before)
    after_entries = parse_lab_log(after)
    @test length(after_entries) == 3
    @test after_entries[1]["author"] == "User — correction"

    # persists across reload
    @test length(parse_lab_log(read_lab_log(load_project(proj.uid)))) == 3

    # a non-entry `## ` header (version boundary) is not parsed as an entry, and doesn't
    # swallow the following real entry
    marked = after * "\n## [Version boundary: v1 → v2, 2026-07-15]\n---\n"
    append_lab_log!(proj, "User", "post-boundary note")
    # (the boundary line lives in the file only if a user adds it; here we assert the parser)
    @test length(parse_lab_log(marked)) == 3           # boundary line adds no entry

    # empty / whitespace-only / no-author entries are rejected
    @test_throws ErrorException append_lab_log!(proj, "User", ["   "])
    @test_throws ErrorException append_lab_log!(proj, "Claude", String[])
    @test_throws ErrorException append_lab_log!(proj, "   ", ["x"])

    # returned block is a well-formed, header-injected markdown block
    blk = append_lab_log!(proj, "Claude", "another note")
    @test startswith(blk, "## ") && occursin("[Claude]", blk) && occursin("- another note", blk)

    rm(proj.root; recursive=true)
end

# ── Lab log context (auto [Cecelia] activity digest) ────────────────────────
# ROLLING DAILY block: one [Cecelia] block per day, regenerated from source and rewritten in place
# as activity accrues (append-only preserved for human entries). Dates are pinned so day rollover is
# deterministic; run-log `at` is stamped explicitly (the `at` kwarg) so a task lands on a given day.
@testset "Lab log context — rolling daily block" begin
    proj = create_project!(name="labctx-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="set-A")
    img1 = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))
    img2 = add_image!(s; name="img-2", meta=Dict{String,Any}("ori_path"=>"/tmp/b.tif"))
    d1, d2 = Date(2026, 7, 20), Date(2026, 7, 21)

    # nothing run yet → no digest, no block on disk
    @test capture_context!(proj; date=d1) === nothing
    @test isempty(parse_lab_log(read_lab_log(proj)))

    # day-1 activity (run logs dated on d1) → ONE [Cecelia] block for the day
    append_run_log!(img1, "segment.cellpose", "default"; at="2026-07-20T09:00:00")
    append_run_log!(img2, "segment.cellpose", "default"; at="2026-07-20T09:05:00")
    append_run_log!(img1, "tracking.bayesian_tracking", ""; at="2026-07-20T10:00:00")

    block = capture_context!(proj; date=d1)
    @test block !== nothing
    @test occursin("## 2026-07-20 [Cecelia]", block)
    @test occursin("Segment — cellpose on 2 images", block)      # grouped by category, prefix dropped
    @test occursin("Tracking — bayesian_tracking on 1 image", block)   # singular
    @test occursin("✅ Segment", block) && occursin("✅ Tracking", block)   # all ok → ✅
    entries = parse_lab_log(read_lab_log(proj))
    @test length(entries) == 1 && entries[1]["author"] == "Cecelia"

    # idempotent: same day, no new activity → block unchanged → nothing, still ONE block
    @test capture_context!(proj; date=d1) === nothing
    @test length(parse_lab_log(read_lab_log(proj))) == 1

    # MORE activity the SAME day → the day's block is rewritten IN PLACE (cumulative), not a 2nd block
    append_run_log!(img2, "behaviour.hmm", ""; at="2026-07-20T14:00:00")
    block1b = capture_context!(proj; date=d1)
    @test block1b !== nothing
    @test occursin("Behaviour — hmm on 1 image", block1b)
    @test occursin("cellpose on 2 images", block1b)              # earlier activity still present
    @test length(parse_lab_log(read_lab_log(proj))) == 1         # ONE block, rewritten — not appended

    # NEXT day → a NEW block; the previous day's block is untouched and holds only its own activity
    append_run_log!(img1, "segment.measureLabels", "default"; at="2026-07-21T08:00:00")
    block2 = capture_context!(proj; date=d2)
    @test block2 !== nothing
    @test occursin("## 2026-07-21 [Cecelia]", block2)
    @test occursin("measureLabels", block2)
    @test !occursin("cellpose", block2)                          # day-2 block ≠ day-1 activity
    @test length(parse_lab_log(read_lab_log(proj))) == 2
    full = read_lab_log(proj)
    @test occursin("cellpose on 2 images", full) && occursin("Behaviour — hmm", full)   # d1 intact

    rm(proj.root; recursive=true)
end

# severity symbols + the COLLAPSED QC-detail lines (per-image and per-channel repetition folded away)
@testset "Lab log context — severity + collapsed QC details" begin
    projS = create_project!(name="labctx-sev-$(rand(1000:9999))")
    sS    = add_set!(projS; name="set-S")
    iS1   = add_image!(sS; name="s-1", meta=Dict{String,Any}("ori_path"=>"/tmp/s1.tif"))
    iS2   = add_image!(sS; name="s-2", meta=Dict{String,Any}("ori_path"=>"/tmp/s2.tif"))
    iS3   = add_image!(sS; name="s-3", meta=Dict{String,Any}("ori_path"=>"/tmp/s3.tif"))
    d = Date(2026, 7, 22)
    append_run_log!(iS1, "segment.measureLabels", "default", "failed"; at="2026-07-22T08:00:00")
    write_qc(iS2, "tracking.track_measures", "default",
             [Dict{String,Any}("level"=>"warn","code"=>"c","short"=>"s","long"=>"l")])
    append_run_log!(iS2, "tracking.track_measures", "default"; at="2026-07-22T09:00:00")
    # a fun run on 3 images with the SAME warn banked on 2 of them
    for im in (iS1, iS2, iS3); append_run_log!(im, "behaviour.hmm_states", "default"; at="2026-07-22T10:00:00"); end
    for im in (iS1, iS2)
        write_qc(im, "behaviour.hmm_states", "default",
                 [Dict{String,Any}("level"=>"warn","code"=>"hmm.collapsed","short"=>"Collapsed to one state","long"=>"l")])
    end

    sev = capture_context!(projS; date=d)
    @test sev !== nothing
    @test occursin("❌ Segment", sev)      # measureLabels failed → worst outcome for the module
    @test occursin("⚠️ Tracking", sev)     # track_measures produced a warn finding
    @test occursin("hmm_states on 3 images — 2 flagged", sev)
    @test !occursin("(3 images)", sev)     # redundant parenthetical dropped for >2 images
    # the SAME finding across 2 images collapses to ONE detail line (few images → listed by stable
    # uid, sorted; the panel resolves uid→name on demand)
    @test occursin("↳ Collapsed to one state ($(join(sort([iS1.uid, iS2.uid]), ", ")))", sev)
    @test count("Collapsed to one state", sev) == 1     # folded to one line, not repeated per image

    # per-CHANNEL collapse: 4 findings differing only by channel → ONE "ch 0-3" detail line
    iH = add_image!(sS; name="h-1", meta=Dict{String,Any}("ori_path"=>"/tmp/h.tif"))
    append_run_log!(iH, "segment.cellpose", "seg"; at="2026-07-22T11:00:00")
    write_qc(iH, "segment.cellpose", "seg",
             [Dict{String,Any}("level"=>"warn","code"=>"cellpose.channel_blank",
                               "short"=>"Channel $i is saturated","long"=>"l",
                               "detail"=>Dict{String,Any}("channel"=>i)) for i in 0:3])
    b2 = capture_context!(projS; date=d)
    @test occursin("↳ is saturated — ch 0-3 ($(iH.uid))", b2)

    rm(projS.root; recursive=true)
end

# gating + exclusion deltas: NET over the day, diffed against a start-of-day baseline
@testset "Lab log context — gating & exclusions (daily net)" begin
    proj = create_project!(name="labctx-gate-$(rand(1000:9999))")
    s    = add_set!(proj; name="set-A")
    img1 = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))
    img2 = add_image!(s; name="img-2", meta=Dict{String,Any}("ori_path"=>"/tmp/b.tif"))
    d1, d2 = Date(2026, 7, 23), Date(2026, 7, 24)

    # nothing yet
    @test capture_context!(proj; date=d1) === nothing

    # add a gated pop on d1 → "added: CD3" (net vs the day baseline)
    m = PopulationMap(; pop_type="flow", value_name="default")
    cd3 = add_pop!(m, "CD3"; gate=RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
    save_pop_map!(m, img1)
    bg = capture_context!(proj; date=d1)
    @test bg !== nothing && occursin("Gating — ", bg) && occursin("added: CD3", bg)

    # editing that gate the SAME day still nets to "added: CD3": the digest line is unchanged, so
    # the block is a no-op rewrite (returns nothing) and the log still reads "added", never "gate changed"
    set_gate!(m, cd3, RectangleGate("c1", "c2", 0.2, 1.0, 0.0, 1.0)); save_pop_map!(m, img1)
    @test capture_context!(proj; date=d1) === nothing
    ll = read_lab_log(proj)
    @test occursin("added: CD3", ll) && !occursin("gate changed", ll)

    # roll into d2: first capture seeds the new day's baseline (CD3 as-is) and reports nothing new
    @test capture_context!(proj; date=d2) === nothing
    # NOW change the gate on d2 → "gate changed: CD3" (CD3 is in the day baseline, so it's an edit)
    set_gate!(m, cd3, RectangleGate("c1", "c2", 0.3, 1.0, 0.0, 1.0)); save_pop_map!(m, img1)
    bg3 = capture_context!(proj; date=d2)
    @test bg3 !== nothing && occursin("gate changed: CD3", bg3) && !occursin("added: CD3", bg3)

    # filter/membership pop (cluster tracks): a DEFINITION change is caught generically, not just gates
    mt = PopulationMap(; pop_type="trackclust", value_name="default")
    add_pop!(mt, "clust_a"; filter_measure="clusters.x", filter_values=[0, 1])
    save_pop_map!(mt, img1)
    bd = capture_context!(proj; date=d2)
    @test bd !== nothing && occursin("added: clust_a", bd)

    # exclusions: net over the day. Images are referenced by stable uid, not name (the panel
    # resolves uid→name on demand), so the line reads "excluded <uid>".
    img2.included = false; save!(img2)
    be = capture_context!(proj; date=d2)
    @test be !== nothing && occursin("excluded $(img2.uid)", be) && !occursin("img-2", be)

    rm(proj.root; recursive=true)
end

@testset "Lab log context — first capture seeds silently" begin
    proj = create_project!(name="labctx-seed-$(rand(1000:9999))")
    s    = add_set!(proj; name="set-A")
    img  = add_image!(s; name="imgB", meta=Dict{String,Any}("ori_path"=>"/tmp/c.tif"))
    m    = PopulationMap(; pop_type="flow", value_name="default")
    add_pop!(m, "preexisting"; gate=RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
    save_pop_map!(m, img)

    # gating already present at first capture → baseline seeded, NOT reported (no retro dump)
    @test capture_context!(proj) === nothing
    @test isempty(parse_lab_log(read_lab_log(proj)))

    # a subsequent addition IS reported
    m2 = load_pop_map(img; value_name="default", pop_type="flow")
    add_pop!(m2, "newpop"; gate=RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
    save_pop_map!(m2, img)
    b = capture_context!(proj)
    @test b !== nothing && occursin("added: newpop", b)

    rm(proj.root; recursive=true)
end

# ── Lab log dismiss (hide a single entry — config sidecar, log stays append-only) ──
@testset "Lab log dismiss" begin
    proj = create_project!(name="dismiss-test-$(rand(1000:9999))")
    @test read_dismissed(proj) == String[]
    set_dismissed!(proj, "e1a2", true)
    set_dismissed!(proj, "b3c4", true)
    @test Set(read_dismissed(proj)) == Set(["e1a2", "b3c4"])
    set_dismissed!(proj, "e1a2", false)                                # un-hide
    @test read_dismissed(proj) == ["b3c4"]
    @test read_dismissed(load_project(proj.uid)) == ["b3c4"]           # persists
    @test_throws ErrorException set_dismissed!(proj, "  ", true)       # empty id rejected

    # hiding NEVER edits the log file (append-only): the entry text is still on disk
    append_lab_log!(proj, "Cecelia", ["a digest line to hide"])
    before = read_lab_log(proj)
    set_dismissed!(proj, "deadbeef", true)
    @test read_lab_log(proj) == before                                 # file untouched
    rm(proj.root; recursive=true)
end

# ── Lab log capture: the daily [Cecelia] digest groups activity by task category ──
@testset "Lab log capture — category digest" begin
    proj = create_project!(name="capture-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="set-A")
    img  = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))
    d    = Date(2026, 7, 25)

    @test capture_context!(proj; date=d) === nothing    # no activity yet → nothing to digest

    # run-log activity across two categories → one bullet per category, in _CATEGORY_ORDER
    append_run_log!(img, "segment.cellpose", "default"; at="2026-07-25T09:00:00")
    append_run_log!(img, "behaviour.hmm", ""; at="2026-07-25T10:00:00")
    b = capture_context!(proj; date=d)
    @test b !== nothing && occursin("Segment — cellpose", b) && occursin("Behaviour — hmm", b)
    # Segment (earlier in _CATEGORY_ORDER) precedes Behaviour in the block
    @test findfirst("Segment", b).start < findfirst("Behaviour", b).start
    # images are referenced by stable uid, never name — the panel resolves uid→name on demand
    @test occursin(img.uid, b) && !occursin("img-1", b)
    rm(proj.root; recursive=true)
end

# ── Param validation ──────────────────────────────────────────────────────
# ONE test over the whole registry, instead of a hand-written testset per task.
#
# `validate_params` is a single generic function driven by the task's JSON spec, so asserting it
# per task tested one function N times, and only for the tasks somebody remembered to write a
# testset for — a new task whose spec declared no bounds, or a malformed spec, passed silently.
# This walks EVERY registered task and EVERY bounded param: ~80 range checks rather than 16, and
# it covers tasks added after this was written. Per-task testsets remain ONLY where they assert
# something task-specific (Branching's µm key rename, NeighbourStats/ClustRegions' moved keys).
#
# Two rules that only a whole-registry sweep can enforce, both of which caught a real defect when
# this landed: `params` must be a JSON ARRAY (testTasks.incremental_plot_task declared an object, so
# `validate_params` threw MethodError instead of validating), and every `type` must be one the
# validator knows (migrateLegacy said "string", which is not a case in `_validate_leaf`, so the
# param silently skipped validation).
@testset "Param validation — every registered task, from its spec" begin
    # Spec param types `_validate_leaf` understands. A type outside this set is a typo that
    # silently disables validation for that param, so the set is asserted, not assumed.
    known_types = Set(["int", "float", "bool", "select", "chipSelect", "text", "dirPath", "section", "group",
                       "channelSelection", "valueNameSelection", "popSelection",
                       "labelPropsColsSelection", "motionDimsSelection"])

    # `field` values a `valueNameSelection` may name — the frontend's CciaImage fields, kept in step
    # with `VALUE_NAME_FIELDS` (frontend/src/tasks/paramValues.ts). Absent is legal and means image
    # versions. NOT the ccid.json spelling (`filepath`, singular) nor the R version's (`imFilepath`).
    known_value_name_fields = Set(["filepaths", "labels", "spatialGraphs"])

    # A value the spec itself calls valid: the declared default, else something in range/options.
    function valid_value(p)
        t = string(get(p, "type", ""))
        haskey(p, "default") && !isnothing(p["default"]) && p["default"] != "" && return p["default"]
        t == "int"    && return Int(get(p, "min", 1))
        t == "float"  && return Float64(get(p, "min", 1.0))
        t == "bool"   && return false
        t == "select" && return get(first(get(p, "options", [Dict("value" => "x")])), "value", "x")
        return "x"
    end

    # Every param at a value the spec accepts — so a rejection below is provably the ONE value we
    # perturbed, not a required param we forgot to supply. Group values live nested under the
    # group's key, keyed by index string, which is what `each_spec_param` hands back as `gk`.
    function baseline(spec_params)
        d = Dict{String,Any}()
        each_spec_param(spec_params) do p, gk
            key = String(something(spec_get(p, "key", ""), ""))
            t   = String(something(spec_get(p, "type", ""), ""))
            isempty(key) && return
            if t == "group"
                d[key] = Dict{String,Any}("0" => Dict{String,Any}())
            elseif t == "section"
                return                        # layout only, holds no value
            elseif isempty(gk)
                d[key] = valid_value(p)
            else
                get!(d, gk, Dict{String,Any}("0" => Dict{String,Any}()))["0"][key] = valid_value(p)
            end
        end
        d
    end

    # Assert the rejection names the offending key — otherwise a throw for an unrelated reason
    # (a missing required param) would let a broken bound pass as "validated".
    function rejects(task, params, key, why)
        err = nothing
        try
            validate_params(task, params)
        catch e
            err = e
        end
        if !(err isa ParamValidationError)
            @error "expected a ParamValidationError" task = typeof(task) param = key case = why got = err
        end
        @test err isa ParamValidationError
        if err isa ParamValidationError && !occursin(key, err.msg)
            @error "rejected, but for a different param" param = key case = why msg = err.msg
        end
        @test !(err isa ParamValidationError) || occursin(key, err.msg)
    end

    checked_bounds   = 0
    checked_selects  = 0
    checked_required = 0
    checked_tasks    = 0
    skipped_tasks    = 0     # registered, but ships no spec / no params (composites, testTasks)

    for (fun_name, registered) in sort(collect(Cecelia._fun_name_map()); by = first)
        @testset "$fun_name" begin
            # dispatch wiring: the registry name resolves back to the same task
            resolved = _task_from_fun_name(fun_name)
            @test typeof(resolved) === typeof(registered)
            @test task_scope(resolved) ∈ ("image", "set")

            spec = Cecelia._task_spec(resolved)
            spec_params = spec === nothing ? [] : get(spec, "params", [])
            # ARRAY, not object — an object makes validate_params throw MethodError
            @test spec_params isa AbstractVector
            # a task may legitimately ship no spec / no params; anything else is walked below
            if !(spec_params isa AbstractVector) || isempty(spec_params)
                skipped_tasks += 1
            end
            if spec_params isa AbstractVector && !isempty(spec_params)
            checked_tasks += 1

            base = baseline(spec_params)
            # the spec's own defaults must satisfy the spec
            @test validate_params(resolved, deepcopy(base)) === nothing

            # every param, carrying the group it is nested in (containers filtered out below)
            flat = Tuple[]
            each_spec_param(spec_params) do p, gk
                push!(flat, (p, gk))
            end

            for (p, group_key) in flat
                key = String(something(spec_get(p, "key", ""), ""))
                t   = String(something(spec_get(p, "type", ""), ""))
                isempty(key) && continue
                t in ("section", "group") && continue   # containers hold no value of their own
                @test t ∈ known_types

                # A `valueNameSelection`'s `field` names a CciaImage field the FRONTEND reads
                # (`VALUE_NAME_FIELDS` in frontend/src/tasks/paramValues.ts). Same failure mode as an
                # unknown `type`: nothing errors, the widget just quietly degrades. Four tasks carried
                # the R version's `imFilepath`, which matched no branch, so they stopped preselecting
                # the image's ACTIVE version — and the form pointed at a version the viewer wasn't
                # showing while cellpose (field absent) pointed at the right one.
                if t == "valueNameSelection"
                    fld = spec_get(p, "field", nothing)
                    fld === nothing || @test String(fld) ∈ known_value_name_fields
                end

                # perturb exactly one value, in place, inside its group entry if nested
                function with(bad)
                    d = deepcopy(base)
                    if isempty(group_key)
                        d[key] = bad
                    else
                        d[group_key]["0"][key] = bad
                    end
                    d
                end

                # a required param that goes missing must be rejected, whatever its type
                if get(p, "required", false) == true && isempty(group_key)
                    d = deepcopy(base)
                    delete!(d, key)
                    rejects(resolved, d, key, "required but missing")
                    checked_required += 1
                end

                if t in ("int", "float")
                    if haskey(p, "min")
                        rejects(resolved, with(p["min"] - 1), key, "below min")
                        checked_bounds += 1
                    end
                    if haskey(p, "max")
                        rejects(resolved, with(p["max"] + 1), key, "above max")
                        checked_bounds += 1
                    end
                    rejects(resolved, with("not-a-number"), key, "wrong type")
                elseif t == "select"
                    rejects(resolved, with("__not_a_valid_option__"), key, "unknown option")
                    checked_selects += 1
                elseif t == "bool"
                    rejects(resolved, with("yes"), key, "non-bool")
                end
            end
            end   # spec_params is a non-empty array
        end
    end

    # Guard against the sweep silently covering nothing. If the walk breaks — a renamed registry,
    # a spec shape this walker doesn't recognise — the loop runs zero times, every assertion in it
    # passes vacuously, and the suite reports green having tested nothing.
    #
    # The task count is EXACT and relative: every registered task is either walked or explicitly
    # counted as spec-less, so it needs no maintenance and catches a task that silently stopped
    # being visited. The rest are deliberately LOOSE floors — their job is "the walk still finds
    # params", not "the tree is currently this big". Pinning them near today's counts (82 bounds,
    # 22 selects) would turn deleting a task into a red build, which is how a guard becomes a
    # chore. Raise one only if it ever fails without the walk being broken.
    @test checked_tasks + skipped_tasks == length(Cecelia._fun_name_map())
    @test checked_bounds   >= 25
    @test checked_selects  >= 8
    @test checked_required >= 2
end

@testset "Param validation — run_task enforces it, not just validate_params" begin
    # The bounds themselves are swept for every task above; what is unique here is WHERE
    # validation happens — the scheduler entry point rejects bad params before `_run_task`
    # is ever reached, so a task body never runs with values its spec forbids.
    proj2 = create_project!(name="val-test-$(rand(1000:9999))")
    s2 = add_set!(proj2; name="s")
    img2 = add_image!(s2; name="img", meta=Dict{String,Any}("ori_path" => "/tmp/fake.tif"))
    @test_throws ParamValidationError run_task(
        ImportOmezarr(), img2, Dict{String,Any}("pyramidScale" => 99))
    rm(proj2.root; recursive=true)
end

# ── Param validation — CellposeCorrect (constraints live inside a `group`) ───

# ── Axis gating (task_applies + img_axes) ────────────────────────────────
@testset "Axis gating — img_axes + task_applies" begin
    # img_axes: SizeT > 1 → :T; TimeIncrement present as fallback for pre-SizeT projects.
    img_static = CciaImage(; uid="a1", name="static", dir="")
    img_static.meta = Dict{String,Any}("SizeC"=>2, "SizeT"=>1, "SizeZ"=>5)
    @test Cecelia.img_axes(img_static) == Set([:X, :Y, :Z, :C])
    @test !Cecelia.img_has_time(img_static)

    img_live = CciaImage(; uid="a2", name="live", dir="")
    img_live.meta = Dict{String,Any}("SizeC"=>4, "SizeT"=>10, "SizeZ"=>1)
    @test Cecelia.img_axes(img_live) == Set([:X, :Y, :T, :C])
    @test Cecelia.img_has_time(img_live)

    # TimeIncrement fallback for pre-SizeT imports (present + parseable → :T)
    img_legacy = CciaImage(; uid="a3", name="legacy", dir="")
    img_legacy.meta = Dict{String,Any}("TimeIncrement"=>"30")
    @test :T ∈ Cecelia.img_axes(img_legacy)
    @test Cecelia.img_has_time(img_legacy)

    # task_requires_axes: reads spec's requires.axes (BayesianTracking → {:T})
    @test Cecelia.task_requires_axes(BayesianTracking()) == Set([:T])
    @test isempty(Cecelia.task_requires_axes(ImportOmezarr()))

    # task_applies: T-requiring task rejects a static image, accepts a live one
    @test !task_applies(BayesianTracking(), img_static)
    @test  task_applies(BayesianTracking(), img_live)
    @test  task_applies(ImportOmezarr(), img_static)   # no requirement → always applies

    # Composite recursion: HMM (states + transitions) inherits T from its steps
    hmm = Cecelia._task_from_fun_name("behaviour.hmm")
    @test :T ∈ Cecelia.task_requires_axes(hmm)
    @test !task_applies(hmm, img_static)
    @test  task_applies(hmm, img_live)

    # run_task raises TaskApplicabilityError before scheduling on a static image
    proj = create_project!(name="axis-gate-$(rand(1000:9999))")
    s   = add_set!(proj; name="s")
    img = add_image!(s; name="static-img",
                     meta=Dict{String,Any}("SizeC"=>1, "SizeT"=>1, "SizeZ"=>1))
    @test_throws Cecelia.TaskApplicabilityError run_task(
        BayesianTracking(), img, Dict{String,Any}(); pool_name="cpu")
    rm(proj.root; recursive=true)
end

# ── Dispatch + param validation — Branching (segment.branching) ──────────────
# docs/todo/BRANCHING_PLAN.md Phase 1. New task registers via _task_from_fun_name and
# validate_params rejects out-of-range dilation sizes + wrong-typed booleans.
# Ranges, types and the unknown-select case are swept for EVERY task above. What survives here is
# specific to branching: the µm key rename, the enumerated anisotropy sources, and the copy budget.
@testset "Branching spec — µm keys, anisotropy sources, copy budget" begin
    # anisotropySource (docs/todo/SPATIAL_ANISOTROPY_PLAN.md Decision 5) — a select with three
    # allowed values. The runner raises on anything else, so an unknown value must not get past
    # validation and reach Python as a subprocess failure.
    for src in ("skeleton", "mask", "channel")
        @test begin
            validate_params(Branching(), Dict{String,Any}("anisotropySource" => src))
            true
        end
    end
    @test_throws ParamValidationError validate_params(
        Branching(), Dict{String,Any}("anisotropySource" => "intensity"))

    # The anisotropy scales are in MICRONS, and the keys carry `Um` for exactly that reason: a
    # project with saved PIXEL params (sigma 2, box 45) must not have them silently reread as µm,
    # which would be ~3 px and ~75 px. New keys mean the stale values simply do not apply.
    let spec = Cecelia._task_spec(Branching())
        bykey = Dict(String(get(p, "key", "")) => p for p in spec["params"])
        @test haskey(bykey, "structureTensorSigmaUm") && haskey(bykey, "anisotropyBoxUm")
        @test !haskey(bykey, "structureTensorSigma") && !haskey(bykey, "anisotropyBoxSize")
        @test bykey["structureTensorSigmaUm"]["default"] == 7.0
        @test bykey["anisotropyBoxUm"]["default"] == 5.0
        @test bykey["anisotropySource"]["default"] == "skeleton"
        # tips: covered repo-wide by "every task param carries a tip" + the copy-budget sweep
    end
end

# ── µm → px for the anisotropy scales ─────────────────────────────────────────────────────────
# The user sets a PHYSICAL scale (a fibre is ~2 µm thick whatever the objective); the compute is
# in pixels. Getting this backwards, or letting a sub-pixel request through, produces a grid that
# resamples noise rather than summarising structure.
@testset "Anisotropy µm→px conversion" begin
    # EaMaVq's real calibration: 0.596 µm/px
    px, clamped = Cecelia._um_to_px(7.0, 0.596; minimum_px = 0.5)
    @test px ≈ 7.0 / 0.596 && !clamped
    @test round(Int, first(Cecelia._um_to_px(5.0, 0.596; minimum_px = 3))) == 8

    # A coarser image needs FEWER pixels for the same physical scale — the whole point: the same
    # setting means the same thing on both, which a pixel setting never did.
    @test first(Cecelia._um_to_px(5.0, 1.0; minimum_px = 3)) <
          first(Cecelia._um_to_px(5.0, 0.5; minimum_px = 3))

    # Sub-minimum requests clamp AND say so, rather than silently running a different setting.
    px2, clamped2 = Cecelia._um_to_px(0.5, 0.596; minimum_px = 3)
    @test px2 == 3.0 && clamped2
    @test_throws ErrorException Cecelia._um_to_px(5.0, 0.0; minimum_px = 3)

    # Stored bytes scale as boxes, and boxes as 1/box² — so halving the spacing is 4x the file.
    # This is the number behind the "what do I actually put there" tip.
    @test Cecelia._aniso_grid_bytes(1296, 201) == 1296 * 40 * 201        # 36x36 over 201 frames
    @test Cecelia._aniso_grid_bytes(4 * 1296, 201) == 4 * Cecelia._aniso_grid_bytes(1296, 201)
    @test Cecelia._aniso_grid_bytes(100, 0) == Cecelia._aniso_grid_bytes(100, 1)  # static image

    # Advisory only, and only past the threshold — a fine grid is a legitimate choice.
    @test isempty(Cecelia._aniso_grid_findings(Cecelia._aniso_grid_bytes(1296, 201), 1296, 5.0))
    big = Cecelia._aniso_grid_bytes(36_000, 201)      # ~290 MB, a ~1 µm grid on this image
    f = Cecelia._aniso_grid_findings(big, 36_000, 1.0)
    @test length(f) == 1 && f[1]["level"] == "warn"
    @test f[1]["code"] == "branching.aniso_grid_large"
    # the number lives in `detail`, per the catalog rule that prose carries no figures
    @test occursin("289 MB", f[1]["detail"]) && occursin("36000 boxes", f[1]["detail"])
    @test !occursin("289", f[1]["short"])

    # Both branching findings come from the QC CATALOG now, not inlined at the call site
    @test haskey(Cecelia.QC_TEXT, "branching.no_branches")
    @test haskey(Cecelia.QC_TEXT, "branching.aniso_grid_large")
    @test haskey(Cecelia.QC_TEXT, "branching.uncalibrated")
    @test Cecelia._branching_qc_findings(0)[1]["short"] == "No branches found"
    @test isempty(Cecelia._branching_qc_findings(5))
end

@testset "Smoothing QC" begin
    # Both findings key off the persisted python stats, so the helper is fed exactly what
    # smooth_run.py writes. Photon-limited input: zeros fall from ~90% to ~5%, no clipping.
    worked = Dict{String,Any}(
        "gain" => 2.4, "clippedVoxels" => 0,
        "zeroFracIn"  => Dict{String,Any}("0" => 0.91, "1" => 0.88),
        "zeroFracOut" => Dict{String,Any}("0" => 0.06, "1" => 0.05))
    @test isempty(Cecelia._smooth_qc_findings(worked))

    # Gain clipping — the bright end of every smoothed channel is now flat.
    clipped = merge(worked, Dict{String,Any}("clippedVoxels" => 1234))
    f = Cecelia._smooth_qc_findings(clipped)
    @test length(f) == 1 && f[1]["code"] == "smooth.gain_clipped" && f[1]["level"] == "warn"
    @test f[1]["short"] == "Dynamic-range gain clipped 1234 voxels"  # the count is IN the message
    @test occursin("Restore dynamic range", f[1]["long"]) # the action, imperative
    @test f[1]["detail"] isa AbstractDict
    # from the catalog, so it re-renders at read time like every other finding
    @test haskey(Cecelia.QC_TEXT, "smooth.gain_clipped")
    @test f[1]["key"] == "smooth.gain_clipped"

    # Dense input — nothing sparse to fill, so the step bought nothing. Info, not warn.
    dense = Dict{String,Any}(
        "gain" => 1.0, "clippedVoxels" => 0,
        "zeroFracIn"  => Dict{String,Any}("0" => 0.02),
        "zeroFracOut" => Dict{String,Any}("0" => 0.00))
    fd = Cecelia._smooth_qc_findings(dense)
    @test length(fd) == 1 && fd[1]["code"] == "smooth.no_effect" && fd[1]["level"] == "info"
    @test haskey(Cecelia.QC_TEXT, "smooth.no_effect")

    # advisory only, per docs/MODULES.md — never an error, never a gate
    @test all(x -> x["level"] in ("info", "warn"),
              vcat(Cecelia._smooth_qc_findings(clipped), fd))

    # Metrics reduce the per-channel dicts to the WORST channel — a step that filled one channel and
    # left another sparse is the case worth seeing.
    m = Cecelia._smooth_metrics(worked)
    @test m["zeroFracInMax"]  == 0.91
    @test m["zeroFracOutMax"] == 0.06
    @test m["gain"] == 2.4 && m["clippedVoxels"] == 0

    # Missing stats must not throw — the helper runs on whatever python managed to write.
    @test Cecelia._smooth_metrics(Dict{String,Any}())["gain"] == 1.0
    @test isempty(Cecelia._smooth_qc_findings(Dict{String,Any}()))

    # Deliberately NOT cohort: the input is the drift-corrected store, whose zero fraction includes
    # the canvas padding drift correction added, so the outlier detector would rank images by shake.
    @test !haskey(COHORT_METRICS, "cleanupImages.smooth")
end

@testset "AF correction QC — the exemption that got retired" begin
    # This task carried a QC-EXEMPT comment calling itself the weakest exemption in the codebase.
    # It now has exactly ONE finding: the correction has no free parameter left to land badly, so the
    # only objective signal is about the INPUT.
    ok = Dict{String,Any}("1" => Dict{String,Any}(
        "saturatedFrac" => 0.0001, "levelsUsed" => 200, "levelsAvailable" => 256))
    @test isempty(Cecelia.af_qc_findings(ok)[1])

    saturated = Dict{String,Any}("1" => Dict{String,Any}(
        "saturatedFrac" => 0.05, "levelsUsed" => 200, "levelsAvailable" => 256))
    f, w = Cecelia.af_qc_findings(saturated)
    @test length(f) == 1 && f[1]["code"] == "af.saturated_input" && f[1]["level"] == "warn"
    @test w.saturated == 0.05

    # THE BUG THIS REPLACED: the finding was hand-rolled with a `detail` STRING and no `long` at all,
    # so the QC panel rendered "Channel 1 saturated → undefined" — visible in the GUI from the day AF
    # QC shipped, because `lib/qc.ts` reads `f.long`. House convention (see drift_correct.jl):
    # short = problem, long = the action, FIGURES in `detail` as a Dict.
    @test f[1]["short"] == "Channel 1 saturated"
    @test !isempty(get(f[1], "long", ""))
    @test occursin("gain", f[1]["long"])                    # the action, imperative
    @test f[1]["detail"] isa AbstractDict                   # figures, NOT a string
    @test f[1]["detail"]["saturatedPct"] == 5.0
    # ...and it comes from the copy catalog, so it re-renders at read time like every other finding
    @test haskey(Cecelia.QC_TEXT, "af.saturated_input")
    @test f[1]["key"] == "af.saturated_input"

    # advisory only, per docs/MODULES.md — never an error, never a gate
    @test all(x -> x["level"] == "warn", f)

    # `af-low-range` IS GONE, and re-tuning it would be wrong. It warned when the output used <20% of
    # the dtype's levels — a real signal under the RATIO, whose output was stretched to fill the range
    # through a derived ceiling. The power weight outputs INPUT COUNTS, so a 16-bit channel with signal
    # in the low thousands legitimately occupies a sliver: measured on real runs, 735-3576 of 65536
    # levels (1.1-5.5%) on EVERY channel of EVERY image. The premise inverted with the mechanism.
    coarse = Dict{String,Any}("2" => Dict{String,Any}(
        "saturatedFrac" => 0.0, "levelsUsed" => 775, "levelsAvailable" => 65536))
    f2, w2 = Cecelia.af_qc_findings(coarse)
    @test isempty(f2)                              # 1.2% of the range is NORMAL now, not a warning
    @test w2.levels < 0.02                         # ...but the metric is still banked
    @test !any(x -> occursin("range", x["code"]), Cecelia.af_qc_findings(coarse)[1])

    # worst-case rollup across channels, since QC banks one number per image
    both = merge(saturated, coarse)
    _, w3 = Cecelia.af_qc_findings(both)
    @test w3.saturated == 0.05         # worst = most saturated
    @test w3.levels < 0.02             # worst = least range used

    # `levelsUsedFrac` stays a COHORT metric: an image far below its peers is informative even when the
    # absolute number is not. `saturatedFrac` describes the acquisition — measured across the nine
    # kSUFux movies it spanned 0.001%-0.018%, a 13x spread at identical settings.
    @test COHORT_METRICS["cleanupImages.afCorrect"] == ["saturatedFrac", "levelsUsedFrac"]
    @test !("ceiling" in COHORT_METRICS["cleanupImages.afCorrect"])
    @test !("clippedFrac" in COHORT_METRICS["cleanupImages.afCorrect"])

    # ratio-era stats files are ignored, not warned on
    ceiling_era = Dict{String,Any}("1" => Dict{String,Any}(
        "clippedFrac" => 0.9, "levelsUsed" => 200, "levelsAvailable" => 256, "ceiling" => 999.0))
    @test isempty(Cecelia.af_qc_findings(ceiling_era)[1])

    # A stats file missing the key must read as 0.0, not throw.
    @test Cecelia.af_qc_findings(Dict{String,Any}("1" => Dict{String,Any}(
        "levelsUsed" => 200, "levelsAvailable" => 256)))[2].saturated == 0.0
end

@testset "every QC finding carries the fields the GUI reads" begin
    # `lib/qc.ts` renders `${f.short}\n→ ${f.long}`, so a finding without `long` displays the literal
    # string "undefined" to the user. AF shipped exactly that for months because it hand-rolled its
    # finding dict instead of calling `qc_finding`. Nothing checked, so nothing caught it.
    #
    # Enforced structurally: no producer may build a finding dict by hand. `qc_finding` is the one
    # constructor, and it cannot omit `long` or put a string in `detail`.
    src = String[]
    for (root, _, files) in walkdir(joinpath(dirname(dirname(pathof(Cecelia))), "src"))
        for f in files
            endswith(f, ".jl") || continue
            push!(src, joinpath(root, f))
        end
    end
    @test !isempty(src)
    offenders = String[]
    for path in src
        endswith(path, "qc.jl") && continue          # the constructor itself
        for (i, line) in enumerate(eachline(path))
            occursin(r"\"level\"\s*=>\s*\"(warn|info)\"", line) &&
                push!(offenders, "$(basename(path)):$i")
        end
    end
    @test offenders == []

    # and the constructor's own contract: `long` always present, `detail` only ever structured
    f = Cecelia.qc_finding("warn", "af.saturated_input"; channel = 2,
                           detail = Dict{String,Any}("saturatedPct" => 1.5))
    @test haskey(f, "long") && !isempty(f["long"])
    @test f["detail"] isa AbstractDict
    @test !(Cecelia.qc_finding("warn", "x.y", "s", "l")["long"] |> isempty)
end

@testset "one resolver turns channel names into indices" begin
    # SIX handlers had hand-rolled `findfirst(==(String(ch)), ch_names)` and drifted into three
    # different behaviours, all silently wrong: an already-resolved index crashed four of them, an
    # unmatched name was dropped by five, and `drift_correct` fell back to index 0 — which on a
    # resonance-scanner movie means registering the whole timelapse against SHG at 99.5% zeros.
    names = ["SHG", "nuc-GFP", "mem-TOM", "CD169-Kat"]

    @test Cecelia.channel_index("mem-TOM", names) == 2          # 0-BASED, for the Python side
    @test Cecelia.channel_index("SHG", names) == 0
    @test Cecelia.channel_indices(["CD169-Kat", "nuc-GFP"], names) == [3, 1]   # order preserved

    # idempotent: an index passes through, so translating a chain dict twice is a no-op
    @test Cecelia.channel_index(2, names) == 2
    @test Cecelia.channel_indices([2, "CD169-Kat"], names) == [2, 3]

    # a single value, not a vector — `channelSelection` with multiple=false still arrives as one
    @test Cecelia.channel_indices("mem-TOM", names) == [2]

    # deduped by default: a channel named twice would square its term into the AF denominator
    @test Cecelia.channel_indices(["mem-TOM", "mem-TOM"], names) == [2]
    @test Cecelia.channel_indices(["mem-TOM", "mem-TOM"], names; unique_only = false) == [2, 2]

    # "nothing selected" is a legitimate state each task judges for itself (branching only needs
    # fibreChannels for anisotropySource="channel") — not an error
    @test Cecelia.channel_indices(nothing, names) == Int[]
    @test Cecelia.channel_indices([], names) == Int[]

    # AN UNMATCHED NAME RAISES, and the message names what was available. This is the deliberate
    # behaviour change from silent-drop: a channel the user named and we cannot find is not a thing
    # to guess about.
    err = try; Cecelia.channel_index("CH3", names); nothing; catch e; e; end
    @test err isa ErrorException
    @test occursin("CH3", err.msg) && occursin("mem-TOM", err.msg)
    @test_throws ErrorException Cecelia.channel_indices(["nuc-GFP", "nope"], names)
    # ...including when the image registered no names at all, rather than silently indexing nothing
    @test_throws ErrorException Cecelia.channel_index("nuc-GFP", String[])

    # A case-only difference is the common real cause: two images from ONE experiment shipped
    # `mem-TOM` (zolIMa/eQRnwU) and `mem-Tom` (zolIMa/fXgbTl), so a chain built on one fails on the
    # other. Still an error — the match stays exact, guessing is what this resolver removes — but the
    # message names the near match so it is a five-second fix.
    cased = try; Cecelia.channel_index("mem-TOM", ["SHG", "nuc-GFP", "mem-Tom"]); nothing
            catch e; e end
    @test cased isa ErrorException
    @test occursin("mem-Tom", cased.msg) && occursin("case", cased.msg)

    # ccid_channel_names reads the versioned field; `nothing` asks for the ACTIVE version
    raw = Dict{String,Any}("imChannelNames" => Dict{String,Any}(
        "default" => names, "corrected" => ["a", "b"], "_active" => "corrected"))
    @test Cecelia.ccid_channel_names(raw) == names                  # default
    @test Cecelia.ccid_channel_names(raw, nothing) == ["a", "b"]    # active
    @test Cecelia.ccid_channel_names(Dict{String,Any}()) == String[]

    # NO SEVENTH COPY. The detector, not just the extraction — this is the second time this file has
    # had to count these sites, and grep-based guesses were wrong both times.
    src_root = dirname(pathof(Cecelia))
    offenders = String[]
    for (root, _, files) in walkdir(joinpath(src_root, "tasks"))
        for f in files
            endswith(f, ".jl") || continue
            body = read(joinpath(root, f), String)
            for line in split(body, '\n')
                startswith(strip(line), "#") && continue
                occursin(r"findfirst\(==\(String\(", line) &&
                    push!(offenders, relpath(joinpath(root, f), src_root))
            end
        end
    end
    @test isempty(offenders)
end

@testset "AF params are just channels" begin
    # The spec grew into a bag of ~20 numbers while fitting individual datasets and was never
    # revisited. A combination is now the two things it is actually about; everything else is derived
    # (`af_weight_stats`) or was a filter that belongs to a filtering task.
    spec = Cecelia._task_spec(Cecelia.AfCorrect())
    keys_top = [string(get(p, "key", "")) for p in get(spec, "params", [])]
    @test keys_top == ["valueName", "afCombinations", "backgroundMethod"]

    combo = only(p for p in get(spec, "params", []) if string(get(p, "key", "")) == "afCombinations")
    @test [string(get(p, "key", "")) for p in get(combo, "params", [])] ==
          ["targetChannel", "competingChannels"]

    # `none` is NOT offered: the weight is a ratio of intensities, so an unsubtracted pedestal makes
    # background voxels split evenly and survive. Measured on kSUFux/Or1L8a: 92.1% of background voxels
    # come out non-zero and cell-to-background contrast collapses to 6.8x.
    bg = only(p for p in get(spec, "params", []) if string(get(p, "key", "")) == "backgroundMethod")
    @test [string(get(o, "value", "")) for o in get(bg, "options", [])] == ["triangle", "otsu"]

    # No exponent param. This task deleted four numbers with no defensible value (channelPercentile,
    # correctionPercentile, correctionMin, correctionMax) and a user-facing sharpness dial is that same
    # thing returning — see `AF_WEIGHT_EXPONENT`.
    @test !("exponent" in keys_top)
    @test !("weightExponent" in keys_top)

    # the deleted ones, named so a future session doesn't reintroduce them one at a time
    gone = ["correctionMin", "correctionMax", "correctionGain", "channelPercentile",
            "correctionPercentile", "correctionMode", "summaryMode", "summaryPercentile",
            "generateInverse", "medianFilter", "topHatRadius", "rollingBallRadius",
            "rollingBallPadding", "denoiseFun", "waveletMethod", "waveletMode", "tvWeight",
            "applyGaussian", "applyGaussianToOthers"]
    flat = Set{String}(keys_top)
    union!(flat, Set(string(get(p, "key", "")) for p in get(combo, "params", [])))
    for k in gone
        @test !(k in flat)
    end
end

# Cohort QC must aggregate the per-image anisotropy readout — it is Figure 4 panel D's x-axis
# (SPATIAL_ANISOTROPY_PLAN Decision 6), so dropping it from COHORT_METRICS silently removes
# the plot's data source.
@testset "Cohort metrics — branching anisotropy" begin
    @test "anisotropy" in COHORT_METRICS["segment.branching"]
    @test "nBranches" in COHORT_METRICS["segment.branching"]

    # `anisotropy` is the first RATIO metric in a cohort list otherwise made of counts, so
    # check the outlier rule behaves on 0–1 values at the magnitudes real data produces
    # (EaMaVq measures ≈ 0.32). The modified-z path is scale-free, but the MAD==0 fallback
    # is a RELATIVE departure, so tiny numbers are where it would misbehave if anywhere.
    r = Cecelia._cohort_outliers(Dict("a" => 0.31, "b" => 0.33, "c" => 0.30, "d" => 0.09))
    @test haskey(r.outliers, "d") && !haskey(r.outliers, "a")
    # …and a cohort that merely spans the normal 0.1–0.4 band must NOT flag anything: real
    # tissue varies this much, and a false "outlier" on every low-anisotropy image is noise.
    @test isempty(Cecelia._cohort_outliers(
        Dict("a" => 0.12, "b" => 0.21, "c" => 0.30, "d" => 0.38)).outliers)
end

# ── Dispatch + param validation — ClustPops (clustPops.cluster, set-scope) ───


@testset "cellNeighbours QC findings (pure helper)" begin
    # objective graph metrics → advisory findings; only the unambiguous problems flag
    @test isempty(Cecelia._neighbours_qc_findings(100, 500, 0.1))        # healthy graph → no finding
    @test only(Cecelia._neighbours_qc_findings(0, 0, 0.0))["code"]   == "spatial.no_cells"
    @test only(Cecelia._neighbours_qc_findings(100, 0, 0.0))["code"] == "spatial.no_edges"
    @test only(Cecelia._neighbours_qc_findings(100, 40, 0.7))["code"] == "spatial.many_isolated"
    @test isempty(Cecelia._neighbours_qc_findings(100, 40, 0.3))         # some isolated, under half → fine
end


@testset "aggregate DBSCAN ids (Clustering.jl)" begin
    # two dense blobs + one far noise point → two aggregates, noise = id 0
    coords = [0.0 0.0; 0.1 0.1; 0.2 0.0; 5.0 5.0; 5.1 5.1; 5.2 5.0; 50.0 50.0]
    ids = Cecelia._aggregate_ids(coords, 0.5, 2)
    @test length(unique(ids[ids .> 0])) == 2                          # two aggregates
    @test ids[end] == 0                                               # far point is noise
    @test count(==(0), ids) == 1                                      # exactly one noise point
    # too-few points → all noise
    @test all(Cecelia._aggregate_ids([0.0 0.0; 0.1 0.1], 0.5, 5) .== 0)
end



@testset "cellContacts target-name sanitisation" begin
    # obs column suffix — nothing to do with param validation, which is swept above
    @test Cecelia._contact_target("flow", ["T/qc"]) == "flow.T_qc"
    @test Cecelia._contact_target("flow", ["B/qc", "T/qc"]) == "flow.B_qc+T_qc"
end

@testset "neighbourStats spec — graph knobs live on the graph, not here" begin
    # The graph parameters (method / radius / k) deliberately do NOT live here any more — they belong
    # to the graph this task consumes (`graphSuffix` → spatialAnalysis.cellNeighbours), so a
    # neighbourhood is defined once. (Ranges for what remains are swept above.)
    ns_spec = JSON3.read(read(Cecelia._spec_path(NeighbourStats()), String))
    ns_keys = Set(String(get(p, :key, "")) for p in get(ns_spec, :params, []))
    @test "graphSuffix" in ns_keys && "nPermutations" in ns_keys
    for gone in ("neighbourRadius", "nNeighbours", "neighbourMethod")
        @test !(gone in ns_keys)
    end
end

@testset "clustRegions spec — graph knobs moved out with the graph" begin
    # regions run ON a neighbour graph and no longer build their own, so the graph knobs moved to
    # cellNeighbours; `perTimepoint` went with them (whether neighbourhoods are per-frame is a
    # property of the graph, so behaviour regions come from choosing a per-timepoint graph).
    cr_keys = Set(String(get(p, :key, ""))
                  for p in get(JSON3.read(read(Cecelia._spec_path(ClustRegions()), String)), :params, []))
    @test "graphSuffix" in cr_keys && "includeOther" in cr_keys
    for gone in ("neighbourRadius", "nNeighbours", "neighbourMethod", "perTimepoint")
        @test !(gone in cr_keys)
    end
end

@testset "plot specs live on the page that EXPLORES, not the one that DEFINES" begin
    # Where a plot lives is a product decision worth pinning, because the drift is invisible: a new
    # pop type arrives, someone adds a `population_summary_<type>.json` pointed at the page that
    # produced it, and every population-DEFINING page slowly grows a summary canvas it has no use
    # for. Populations are DEFINED on gate / track / clust-cells / clust-tracks / regions, and
    # SUMMARISED on the Explore pages. Each summary follows its pop type:
    #     flow → phenotype ·  clust → phenotype ·  live/trackclust → behaviourAnalysis ·  region → spatialAnalysis
    root = joinpath(dirname(dirname(pathof(Cecelia))), "src", "plotDefinitions")
    @test isdir(root)
    specs = Dict{String,Any}()
    for f in readdir(root)
        endswith(f, ".json") || continue
        specs[f] = JSON3.read(read(joinpath(root, f), String), Dict{String,Any})
    end
    @test length(specs) > 5          # the walk found the registry (a floor, not a census)

    # The interaction matrix is a REGISTRY plot now, not a bespoke component + route: it was the one
    # violation of docs/PLOTS.md → *Hosting — ONE way*, which is why it sat in a fixed box below the
    # table and couldn't be duplicated, arranged, exported or put on the Analysis board.
    @test haskey(specs, "spatial_interactions.json")
    @test String(specs["spatial_interactions.json"]["dataSource"]["matrix"]["mode"]) == "interaction"
    @test String(specs["spatial_interactions.json"]["module"]) == "spatialAnalysis"
    # …and the bespoke surface is gone for good
    fe = joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "frontend", "src")
    @test !isfile(joinpath(fe, "modules", "spatial", "SpatialContactHeatmap.vue"))
    @test !isfile(joinpath(fe, "utils", "contactHeatmap.ts"))
    srv = read(joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "api", "src", "server.jl"), String)
    @test !occursin("/api/plots/contact_matrix", srv)

    # the population-DEFINING module pages carry no plot specs at all
    DEFINING = ("clustPops", "clustTracks", "clustRegions")
    stray = ["$f → module=$(get(s, "module", ""))" for (f, s) in specs
             if String(get(s, "module", "")) in DEFINING]
    @test isempty(stray)

    # There is now ONE population-summary spec offering every family, with the per-page curation in
    # its `modules` allow-list — the four per-popType copies are gone. Pin both halves: no copies
    # come back, and each page still offers exactly the families it should.
    for gone in ("population_summary_clust.json", "population_summary_trackclust.json",
                 "population_summary_tracks.json", "population_summary_region.json")
        @test !haskey(specs, gone)
    end
    ps = specs["population_summary.json"]
    @test !haskey(ps, "module")                       # multi-page specs use `modules`, not `module`
    offered = Dict(String(k) => Set(String(x) for x in v) for (k, v) in ps["modules"])
    @test offered["phenotype"]         == Set(["flow", "clust"])
    @test offered["behaviourAnalysis"] == Set(["live", "track", "trackclust"])
    @test offered["spatialAnalysis"]   == Set(["region"])

    # every family a page offers must actually be declared, WITH its own granularity — the one thing
    # that genuinely blocked a shared spec (sending the spec's single granularity asked for cell rows
    # under a track pop type). flow/clust/region are cell-grained, live/track/trackclust track-grained.
    pts = Dict(String(p["popType"]) => String(p["granularity"]) for p in ps["dataSource"]["popTypes"])
    @test Set(keys(pts)) == Set(["flow", "clust", "live", "track", "trackclust", "region"])
    @test pts["flow"] == "cell" && pts["clust"] == "cell" && pts["region"] == "cell"
    @test pts["live"] == "track" && pts["track"] == "track" && pts["trackclust"] == "track"
    for (_, fams) in offered, f in fams
        @test haskey(pts, f)                          # a page can't offer an undeclared family
    end

    # BEHAVIOUR PLOTS ARE NOT LIVE-ONLY. Every one of them shipped the legacy single
    # `popType: "live"`, so a gated-track population or a track cluster could not be plotted at all
    # — the family picker existed but these specs never opted into it. `pop_df` has always
    # supported `track`/`trackclust` at either granularity (`_pop_df_track_gating` expands track
    # membership to its member cells), so this was a spec omission, not a capability gap.
    BEHAVIOUR = ("cell_properties.json", "hmm_state_frequency.json", "state_signature.json",
                 "transition_matrix.json", "track_measures.json")
    for f in BEHAVIOUR
        ds = specs[f]["dataSource"]
        @test !haskey(ds, "popType")            # legacy single-family form is gone
        fams = Dict(String(p["popType"]) => String(p["granularity"]) for p in ds["popTypes"])
        @test Set(keys(fams)) == Set(["live", "track", "trackclust"])
        # granularity is the PLOT's, not the family's: per-track measures are track-grained, the
        # cell/HMM readouts cell-grained — and it must be the same for all three families, or one
        # pick would silently ask for a different table than another.
        want = f == "track_measures.json" ? "track" : "cell"
        @test all(g == want for g in values(fams))
    end

    # A plot's family list is CURATED in its spec (not derived from the data), because "which family
    # can this measure be sliced by" is a judgement the data can't make. The cost of curation is
    # silent drift, and it drifted: the spatial measures plot offered Gated/Cell clusters/Regions/
    # Tracked but not Track clusters — a family every spatial task happily accepts as input. So pin
    # the agreement to the PRODUCING tasks' own `accepts`, via the canonical token mapping
    # (`_accept_pop_types`) rather than a second hand-written list.
    producing = (CellNeighbours(), NeighbourStats(), CellContacts(), ContactsMeshes(),
                 DetectAggregates(), AggregatesMeshes(), ClustRegions())
    accepted = Set{String}()
    for t in producing
        spec = JSON3.read(read(Cecelia._spec_path(t), String))
        for p in get(spec, :params, [])
            String(get(p, :type, "")) == "popSelection" || continue
            acc = Cecelia._normalise_accepts(get(p, :accepts, String[]))
            union!(accepted, Cecelia._accept_pop_types(acc))
        end
    end
    @test accepted == Set(["live", "track", "clust", "trackclust", "region"])
    spat = Dict(String(p["popType"]) => String(p["granularity"])
                for p in specs["spatial_cell_properties.json"]["dataSource"]["popTypes"])
    @test isempty(setdiff(accepted, keys(spat)))   # every accepted family is offered for plotting
    # `flow` is offered ON TOP: _normalise_accepts folds flow→live (same gate map), but the plot
    # keeps them apart — "Gated" slices the cell gates, "Tracked" the derived `_tracked` sets.
    @test haskey(spat, "flow")
    # the spatial readouts are per-CELL columns, so every family is sliced at cell granularity —
    # including the track-grained ones (pop_df expands track membership to its member cells)
    @test all(g == "cell" for g in values(spat))

    # The manager follows the ACTIVE plot's family, which needs both hosts to pass activeSpecId AND
    # activePopType into useSummaryData. If that regresses the picker silently lists the wrong
    # family — invisible, so pin the wiring.
    fe = joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "frontend", "src")
    for host in ("SummaryCanvas.vue", "LayoutCanvas.vue")
        src = read(joinpath(fe, "components", "canvas", host), String)
        @test occursin("activeSpecId", src)
        @test occursin("activePopType", src)
        @test occursin("migrateSpecId", src)          # persisted canvases must not silently empty
    end
end

@testset "interaction matrix aggregates with NO population targets" begin
    # The path `api_plot_data`'s `precomputed` branch now takes. The panel sends no `series` (the
    # matrix's rows/columns come from the neighbourStats run), so the targets vector is EMPTY — and
    # the interception has to fire before anything touches pop_df. Previously the selector guard
    # rejected the body outright ("pops (or series) required" on a plot with no pops to pick), so
    # this dispatch was never exercised.
    td = mktempdir()
    try
        mkpath(joinpath(td, "spatialStats"))
        write(joinpath(td, "spatialStats", "run1.json"), """
            {"basis":["B/qc","T/qc"],"nCells":334,"nEdges":1200,"graphSuffix":"g1",
             "nPermutations":500,"coverage":0.9,"records":[
              {"popA":"B/qc","popB":"B/qc","observed":120,"expected":80,"logOdds":0.48,
               "zScore":15.8,"pValue":0.002,"significant":true,"association":"association"},
              {"popA":"B/qc","popB":"T/qc","observed":10,"expected":33,"logOdds":-1.19,
               "zScore":-30.4,"pValue":0.002,"significant":true,"association":"avoidance"},
              {"popA":"T/qc","popB":"T/qc","observed":90,"expected":60,"logOdds":0.58,
               "zScore":17.7,"pValue":0.002,"significant":true,"association":"association"}]}
            """)
        img = CciaImage(; dir = td)
        r = plot_summary_data(img, "flow", Tuple{String,String}[], "matrix";
                              matrix_mode = "interaction", stats_suffix = "run1")
        @test r["chartType"] == "matrix" && r["matrixMode"] == "interaction"
        @test r["xLabels"] == ["B/qc", "T/qc"] && r["yLabels"] == r["xLabels"]
        @test r["suffixes"] == ["run1"] && r["suffix"] == "run1"
        @test isempty(r["series"])                       # nothing to overlay — it IS the matrix
        # symmetric fill: 2 populations → 4 cells, the off-diagonals sharing one record
        @test length(r["cells"]) == 4
        by = Dict((c["x"], c["y"]) => c for c in r["cells"])
        @test by[("B/qc", "B/qc")]["value"] == 0.48
        @test by[("B/qc", "T/qc")]["value"] == by[("T/qc", "B/qc")]["value"] == -1.19
        # z / p / observed ride along per cell so the renderer needs no second request
        @test by[("B/qc", "T/qc")]["zScore"] == -30.4
        @test by[("B/qc", "T/qc")]["pValue"] == 0.002
        @test by[("B/qc", "T/qc")]["count"] == 10
        # …plus the star ladder, from the SAME function the hypothesis tests use — a second ladder
        # in the renderer would be a fork waiting to disagree
        @test by[("B/qc", "T/qc")]["significance"] == Cecelia._significance(0.002)
        @test by[("B/qc", "T/qc")]["significance"] == "**"
        # the colour encoding is DIVERGING about 0, so the value must keep its sign as sent (the
        # renderer asserts the scale; here we pin that the payload isn't pre-normalised)
        @test by[("B/qc", "T/qc")]["value"] < 0 < by[("B/qc", "B/qc")]["value"]
        @test r["valueLabel"] == "log-odds"
        # an unknown suffix falls back to the first run rather than erroring
        @test plot_summary_data(img, "flow", Tuple{String,String}[], "matrix";
                                matrix_mode = "interaction", stats_suffix = "nope")["suffix"] == "run1"
        # …and with NO run at all it's an empty matrix, not a throw (the panel shows its own hint)
        empty_img = CciaImage(; dir = mktempdir())
        e = plot_summary_data(empty_img, "flow", Tuple{String,String}[], "matrix";
                              matrix_mode = "interaction")
        @test isempty(e["cells"]) && isempty(e["xLabels"])
    finally
        rm(td; recursive = true, force = true)
    end
end

@testset "spatial graph — path accessor + discovery" begin
    # The graph pools ACROSS segmentations, so it is keyed by run suffix under spatialGraph/, not by
    # value_name next to a cell table (which could not represent a cross-segmentation graph).
    # Discovery is a directory listing, like spatialStats/ — nothing in ccid.json.
    td = mktempdir()
    img = CciaImage(; dir = td)
    @test img_spatial_graph_suffixes(img) == String[]        # nothing built yet
    @test endswith(img_spatial_graph_path(img, "run1"), joinpath("spatialGraph", "run1.h5ad"))
    mkpath(img_spatial_graph_dir(img))
    for s in ("run2", "run1")
        touch(img_spatial_graph_path(img, s))
    end
    touch(joinpath(img_spatial_graph_dir(img), "notes.txt"))  # non-h5ad ignored
    @test img_spatial_graph_suffixes(img) == ["run1", "run2"]     # sorted
end

@testset "neighbourStats QC findings" begin
    # pure helper (docs/MODULES.md) — advisory findings only, never gates
    ids(fs) = Set(String(f["code"]) for f in fs)
    @test ids(Cecelia._neighbour_stats_findings(0, 0)) == Set(["spatial.no_cells"])
    @test ids(Cecelia._neighbour_stats_findings(10, 0)) == Set(["spatial.no_edges"])
    @test isempty(Cecelia._neighbour_stats_findings(10, 5, 1.0, 3))
    # a graph built over far more cells than the analysis selects → the counts rest on a slice of it
    @test "spatial.low_coverage" in ids(Cecelia._neighbour_stats_findings(10, 5, 0.02, 3))
    @test !("spatial.low_coverage" in ids(Cecelia._neighbour_stats_findings(10, 5, 0.5, 3)))
    # nothing beat chance → say so; -1 means the test was skipped (permutations = 0), so stay quiet
    @test "spatial.none_significant" in ids(Cecelia._neighbour_stats_findings(10, 5, 1.0, 0))
    @test isempty(Cecelia._neighbour_stats_findings(10, 5, 1.0, -1))
end

@testset "params NOT declared in the spec pass through untouched" begin
    # Ranges/types are swept above. What is asserted here is the absence of a rule: cropImage
    # passes z/t bounds that its spec never declares, and validation must not reject an unknown
    # key — several tasks rely on carrying extra values through to their runner.
    @test validate_params(
        CropImage(), Dict{String,Any}("x0" => 0, "x1" => 100, "y0" => 0, "y1" => 100,
                                      "z0" => 2, "z1" => 8, "t0" => -1, "t1" => -1)) === nothing
end

@testset "CropImage inherits source calibration (pure helper)" begin
    # A crop must carry the source's physical calibration onto the new image (else the metadata
    # dialog shows "—" and the strip timestamp has no Δt) — see cropImage.jl.
    src = Dict{String,Any}(
        "SizeC" => 4, "SizeZ" => 20, "SizeT" => 181,
        "PhysicalSizeX" => 0.33, "PhysicalSizeY" => 0.33, "PhysicalSizeZ" => 2.0,
        "PhysicalSizeUnit" => "micrometer", "TimeIncrement" => 15, "TimeIncrementUnit" => "second",
        "ori_path" => "/should/not/carry")                       # non-calibration keys stay behind
    # Z trimmed [2,8), T kept whole (-1) → SizeZ shrinks, SizeT & the scale/unit carry over unchanged
    m = Cecelia._crop_inherited_meta(src, (; x0=0, x1=100, y0=0, y1=100, z0=2, z1=8, t0=-1, t1=-1))
    @test m["SizeZ"] == 6                     # 8 - 2 (half-open)
    @test m["SizeT"] == 181                   # axis kept → source count
    @test m["SizeC"] == 4                     # channels invariant under crop
    @test m["PhysicalSizeX"] == 0.33 && m["TimeIncrement"] == 15
    @test m["TimeIncrementUnit"] == "second"
    @test !haskey(m, "ori_path")              # only calibration is inherited
    # T also trimmed [10,40); a source missing SizeZ → no SizeZ key invented
    m2 = Cecelia._crop_inherited_meta(Dict{String,Any}("SizeC" => 2),
                                      (; x0=0, x1=50, y0=0, y1=50, z0=-1, z1=-1, t0=10, t1=40))
    @test m2["SizeT"] == 30 && m2["SizeC"] == 2 && !haskey(m2, "SizeZ")
end


@testset "CopyImage carries calibration + provenance (pure helper)" begin
    # A copy is a faithful duplicate of ONE version: every calibration field carries over UNCHANGED
    # (unlike a crop), plus ori_path and a copy_source_* breadcrumb; non-calibration keys stay behind.
    src = Dict{String,Any}(
        "SizeC" => 4, "SizeZ" => 20, "SizeT" => 181,
        "PhysicalSizeX" => 0.33, "PhysicalSizeY" => 0.33, "PhysicalSizeZ" => 2.0,
        "PhysicalSizeUnit" => "micrometer", "TimeIncrement" => 15, "TimeIncrementUnit" => "second",
        "ori_path" => "/data/raw.czi", "crop_box" => Dict("x0" => 0))   # crop_box must NOT carry
    m = Cecelia._copied_meta(src, "srcUID", "driftCorrected")
    @test m["SizeC"] == 4 && m["SizeZ"] == 20 && m["SizeT"] == 181   # unchanged (full copy)
    @test m["PhysicalSizeX"] == 0.33 && m["PhysicalSizeZ"] == 2.0
    @test m["TimeIncrement"] == 15 && m["TimeIncrementUnit"] == "second"
    @test m["ori_path"] == "/data/raw.czi"                           # same acquisition → provenance carried
    @test m["copy_source_uid"] == "srcUID" && m["copy_source_value_name"] == "driftCorrected"
    @test !haskey(m, "crop_box")                                     # only calibration/provenance
end

@testset "CopyImage copy-tree helper (recursive, verbatim)" begin
    # The zarr copy is a byte-for-byte directory copy (preserves layout/levels/OME sidecar), NOT a
    # zarr re-encode — assert nested files land intact and progress reports the true file count.
    src = mktempdir()
    dst = joinpath(mktempdir(), "out.ome.zarr")
    mkpath(joinpath(src, "0", "sub"))
    write(joinpath(src, ".zattrs"), "{\"multiscales\":[]}")
    write(joinpath(src, "0", "chunk"), "abc")
    write(joinpath(src, "0", "sub", "deep"), "xyz")
    last = Ref((0, 0))
    n = Cecelia._copy_tree_with_progress(src, dst; on_progress = (a, b) -> (last[] = (a, b)))
    @test n == 3
    @test last[][2] == 3                                             # total reported = file count
    @test read(joinpath(dst, ".zattrs"), String) == "{\"multiscales\":[]}"
    @test read(joinpath(dst, "0", "chunk"), String) == "abc"
    @test read(joinpath(dst, "0", "sub", "deep"), String) == "xyz"  # nested tree preserved
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

@testset "Resource pool mapping" begin
    # Pins the pool recategorisation (cpu/gpu/io/network). A task's pool comes from its JSON
    # resource_pool via _task_pool_name; a spec-less task falls back to "cpu".
    poolof(fn) = Cecelia._task_pool_name(_task_from_fun_name(fn))
    @test poolof("segment.cellpose")              == "gpu"
    @test poolof("segment.cellposeMeasure")       == "gpu"
    @test poolof("cleanupImages.cellposeCorrect") == "gpu"
    @test poolof("importImages.omezarr")          == "io"
    @test poolof("importImages.migrateLegacy")    == "io"
    @test poolof("editImages.cropImage")          == "io"
    @test poolof("editImages.copyImage")          == "io"
    @test poolof("cleanupImages.afCorrect")       == "cpu"
    @test poolof("tracking.bayesian_tracking")    == "cpu"
    @test poolof("segment.measureLabels")         == "cpu"
    # no strays: every built-in task resolves to one of the four canonical pools
    for fn in keys(Cecelia._fun_name_map())
        @test poolof(fn) in ("cpu", "gpu", "io", "network")
    end
end

@testset "Live pool limit (set_pool_limit!)" begin
    # resize a pool live AND persist to custom.toml. Redirect the config dir to a temp so the
    # real custom.toml is never touched (config_dir reads CECELIA_DEV_DIR live).
    prev = get(ENV, "CECELIA_DEV_DIR", nothing)
    tmp  = mktempdir()
    ENV["CECELIA_DEV_DIR"] = tmp
    try
        @test set_pool_limit!("io", 3) == 3
        @test any(p -> p.name == "io" && p.limit == 3, list_pools())   # applied live
        @test set_pool_limit!("io", 0)   == 1                           # clamped to ≥ 1
        @test set_pool_limit!("io", 999) == Cecelia.POOL_LIMIT_MAX      # clamped to max
        txt = read(joinpath(tmp, "custom.toml"), String)                # persisted under [pools]
        @test occursin("[pools]", txt) && occursin("io", txt)
    finally
        prev === nothing ? delete!(ENV, "CECELIA_DEV_DIR") : (ENV["CECELIA_DEV_DIR"] = prev)
        resize_pool!("io", 8)   # restore default so later tests are unaffected
    end
end

@testset "Pool status snapshot (pool_status)" begin
    # pool_status() joins the pool registry (limit + in-flight slots) with the task registry
    # (queued count). With no tasks in flight during the test, running/queued are 0 and the
    # limit reflects the live pool budget.
    resize_pool!("cpu", 7)
    st = pool_status()
    cpu = only(filter(p -> p.name == "cpu", st))
    @test cpu.limit   == 7          # live budget
    @test cpu.running == 0          # nothing executing in the test
    @test cpu.queued  == 0          # nothing queued
    # every configured pool is present with the same field set the UI consumes
    @test Set(keys(cpu)) == Set((:name, :limit, :running, :queued))
    @test Set(p.name for p in st) == Set(p.name for p in list_pools())
    resize_pool!("cpu", 20)         # restore default so later tests are unaffected
end

@testset "Custom module reload prunes deleted files" begin
    # load a module from a temp config dir, then delete its .jl and reload → the task must be
    # unregistered (no longer dispatchable) and dropped from the load report. Regression: the
    # report used to only accumulate, so a deleted module stayed "loaded"/green forever.
    tmp = mktempdir()
    srcdir = joinpath(tmp, "modules", "sources", "tmpcat");            mkpath(srcdir)
    defdir = joinpath(tmp, "modules", "inputDefinitions", "tmpcat");   mkpath(defdir)
    spec = joinpath(defdir, "pruneMe.json")
    write(spec, JSON3.write(Dict(
        "fun_name" => "tmpcat.pruneMe", "label" => "Prune me",
        "resource_pool" => "cpu", "scope" => "image", "params" => Any[])))
    jl = joinpath(srcdir, "pruneMe.jl")
    write(jl, """
        struct _PruneMeTask <: Cecelia.CciaTask end
        Cecelia.register_task!("tmpcat.pruneMe", _PruneMeTask(); spec = $(repr(spec)))
        """)

    load_custom_modules!(; dev_dir = tmp)
    @test _task_from_fun_name("tmpcat.pruneMe") isa CciaTask
    @test any(m -> m.path == jl, custom_modules_report())

    rm(jl)                                   # user deletes the module file
    res = load_custom_modules!(; dev_dir = tmp)
    @test jl in res.removed                  # reported as unloaded
    @test_throws Exception _task_from_fun_name("tmpcat.pruneMe")   # no longer dispatchable
    @test !any(m -> m.path == jl, custom_modules_report())         # gone from the report
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

# ── Dispatch + param validation — ClustTracks (clustTracks.cluster, set-scope) ───

# ── Legacy `kind` on disk is silently ignored ────────────────────────────────
# Guards the on-disk contract: a pre-existing ccid.json/project.json with a `kind` key must load
# cleanly (no field on the struct) and the next save! must strip it. Project-wide static/live/flow
# distinction was dropped in favour of per-image axis gating (Cecelia.task_applies).
@testset "Legacy `kind` on disk — ignored + stripped" begin
    proj = create_project!(name="legacy-kind-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")

    # Inject legacy `kind` back into every ccid.json / project.json on disk
    for f in (joinpath(proj.root, "project.json"),
              joinpath(s._dir, "ccid.json"),
              joinpath(img._dir, "ccid.json"))
        raw = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(f, String)))
        raw["kind"] = "live"
        open(f, "w") do io; JSON3.pretty(io, raw); end
    end

    # Load — must not error, must not surface `kind` as a struct field
    loaded = load_project(proj.uid)
    @test !hasfield(typeof(loaded), :kind)
    r_img = init_object(proj.uid, img.uid)
    @test r_img isa CciaImage
    @test !hasfield(typeof(r_img), :kind)

    # save! strips `kind` from disk
    save!(loaded)
    for f in (joinpath(proj.root, "project.json"),
              joinpath(s._dir, "ccid.json"),
              joinpath(img._dir, "ccid.json"))
        raw = JSON3.read(read(f, String))
        @test !haskey(raw, :kind)
    end
    rm(proj.root; recursive=true)
end

# ── Image round-trip (status + attr) ────────────────────────────────────────
# Regression guard: save!(img) must persist status and attr, not silently drop them.
@testset "Image status/attr round-trip" begin
    proj = create_project!(name="rt-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    img.status = "done"
    img.attr["condition"] = "treated"
    save!(img)
    r = init_object(proj.uid, img.uid)
    @test r isa CciaImage
    @test r.status == "done"
    @test get(r.attr, "condition", "") == "treated"
    rm(proj.root; recursive=true)
end

# ── Branch labels round-trip (BRANCHING_PLAN.md Decision 6) ──────────────────
# Skeleton (branch) label sets live in a dedicated `branch_labels` field, NOT in the generic
# `labels` dict, so the labels/measure/tracking pickers never see branch labels. Guards: the
# field survives save!/init_object, its accessor resolves the disk path from branchLabels/,
# and a legacy ccid.json without the key still loads (defaults to empty).
@testset "Branch labels round-trip" begin
    proj = create_project!(name="branch-rt-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    @test isempty(img.branch_labels)
    img.branch_labels["stroma"] = ["stroma.zarr"]
    save!(img)
    r = init_object(proj.uid, img.uid)
    @test r isa CciaImage
    @test r.branch_labels["stroma"] == ["stroma.zarr"]
    @test img_branch_labels_dir(r) == joinpath(r._dir, "branchLabels")
    @test img_branch_labels_path(r, "stroma") == joinpath(r._dir, "branchLabels", "stroma.zarr")
    # unregistered value_name falls back to {value_name}.zarr (write path)
    @test img_branch_labels_path(r, "shg") == joinpath(r._dir, "branchLabels", "shg.zarr")

    # legacy ccid.json (no branch_labels key) → empty
    ccid = joinpath(r._dir, "ccid.json")
    raw  = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
    delete!(raw, "branch_labels")
    open(ccid, "w") do io; JSON3.write(io, raw); end
    legacy = init_object(proj.uid, img.uid)
    @test isempty(legacy.branch_labels)
    rm(proj.root; recursive=true)
end

# ── Reserved value_name suffixes ─────────────────────────────────────────────
# __tracks and __branch are companion-table markers, not legal user segmentation names.
@testset "Reserved value_name suffixes" begin
    @test  is_reserved_value_name("stroma__tracks")
    @test  is_reserved_value_name("stroma__branch")
    @test !is_reserved_value_name("stroma")
    @test !is_reserved_value_name("stroma.branch")   # dot-suffix is the old R convention; not reserved
end

# ── Per-image user flags (included / note / starred) round-trip ──────────────
# Guards: new images default to included + unstarred; the flags survive save!/init_object; and a
# legacy ccid.json with none of the keys loads as included (the accessor never sees a missing field).
@testset "Image included/note/starred round-trip" begin
    proj = create_project!(name="incl-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    @test image_included(img)                 # default: included
    @test img.note == ""
    @test img.starred == false                # default: not starred

    img.included = false
    img.note = "bad drift reference channel"
    img.starred = true
    save!(img)
    r = init_object(proj.uid, img.uid)
    @test r isa CciaImage
    @test !image_included(r)
    @test r.note == "bad drift reference channel"
    @test r.starred

    # legacy file (none of the keys) → included, empty note, unstarred
    ccid = joinpath(r._dir, "ccid.json")
    raw  = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
    delete!(raw, "included"); delete!(raw, "note"); delete!(raw, "starred")
    open(ccid, "w") do io; JSON3.write(io, raw); end
    legacy = init_object(proj.uid, img.uid)
    @test image_included(legacy)
    @test legacy.note == ""
    @test legacy.starred == false
    rm(proj.root; recursive=true)
end

# ── Per-task param memory (funParams) — R moduleFunParams parity ─────────────
# Last-used params are remembered in ccid.json under meta["funParams"][fun], per image and per
# set. Guards: round-trips through save!/init_object, per-fun keys don't clobber, set-level too.
@testset "funParams per-object memory" begin
    proj = create_project!(name="fp-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    save!(img)

    @test read_module_fun_params(img._dir, "cleanupImages.driftCorrect") === nothing  # absent

    p = Dict{String,Any}("valueName" => "cpCorrected", "driftChannel" => ["DAPI"])
    write_module_fun_params!(img._dir, "cleanupImages.driftCorrect", p)
    got = read_module_fun_params(img._dir, "cleanupImages.driftCorrect")
    @test !isnothing(got)
    @test got["valueName"] == "cpCorrected"
    @test got["driftChannel"] == ["DAPI"]

    # init_object loads funParams into the object's meta; a load-modify-save then preserves them
    # (the loaded object carries funParams, so save! doesn't drop them — unlike a stale object).
    r = init_object(proj.uid, img.uid)
    @test haskey(r.meta, "funParams")
    r.status = "done"; save!(r)
    r2 = init_object(proj.uid, img.uid)
    @test r2.status == "done"
    @test read_module_fun_params(r2._dir, "cleanupImages.driftCorrect")["valueName"] == "cpCorrected"

    # a second task's params coexist under its own key (no clobber)
    write_module_fun_params!(img._dir, "cleanupImages.cellposeCorrect",
                             Dict{String,Any}("valueName" => "default"))
    @test read_module_fun_params(img._dir, "cleanupImages.driftCorrect")["valueName"] == "cpCorrected"
    @test read_module_fun_params(img._dir, "cleanupImages.cellposeCorrect")["valueName"] == "default"

    # set-level memory uses the same dir-based mechanism on the set's ccid.json
    write_module_fun_params!(s._dir, "cleanupImages.driftCorrect",
                             Dict{String,Any}("valueName" => "setDefault"))
    @test read_module_fun_params(s._dir, "cleanupImages.driftCorrect")["valueName"] == "setDefault"

    rm(proj.root; recursive=true)
end

# ── Channel names use the versioned convention ──────────────────────────────
# Regression guard: channel names were stored unversioned under meta, where the
# task/API readers (which use top-level versioned imChannelNames) never saw them.
@testset "Channel names versioned round-trip" begin
    proj = create_project!(name="cn-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")

    set_channel_names!(img, ["DAPI", "GFP"]; check_length=false)
    save!(img)

    # model accessor round-trips through save!/load
    r = init_object(proj.uid, img.uid)
    @test channel_names(r) == ["DAPI", "GFP"]

    # on-disk: top-level versioned imChannelNames (the shape tasks/API use), not under meta
    raw = JSON3.read(read(joinpath(img._dir, "ccid.json"), String), Dict{String,Any})
    @test haskey(raw, "imChannelNames")
    @test !haskey(Dict{String,Any}(get(raw, "meta", Dict())), "imChannelNames")
    @test versioned_active(raw["imChannelNames"]) == "default"
    # readable via the exact helper tasks/API use
    @test collect(String, versioned_get_field(raw, "imChannelNames")) == ["DAPI", "GFP"]

    rm(proj.root; recursive=true)
end

# read_ccid_raw is the one ccid.json read+Symbol-key-normalize helper (used by the api layer).
# versioned_get is the single active-value accessor for both String→String path dicts and the
# Any/JSON3 raw dicts (replaced the removed image.jl `active`).
@testset "read_ccid_raw + versioned_get on path dicts" begin
    mktempdir() do d
        p = joinpath(d, "ccid.json")
        write(p, """{"filepath":{"default":"x.ome.zarr","_active":"default"},"class":"CciaImage"}""")
        raw = read_ccid_raw(p)
        @test raw isa Dict{String,Any}
        @test all(k -> k isa String, keys(raw))
        @test raw["class"] == "CciaImage"
        # readable via the exact helper the api/tasks use (nothing → active entry)
        @test versioned_get_field(raw, "filepath") == "x.ome.zarr"
    end
    # versioned_get on a concrete String→String versioned dict (the img.filepath / img.label_props shape)
    d = Dict{String,String}("default" => "a.zarr", "v2" => "b.zarr", "_active" => "v2")
    @test versioned_get(d) == "b.zarr"                 # active entry
    @test sort(versioned_keys(d)) == ["default", "v2"] # excludes _active
end

# ── Destructive ops ──────────────────────────────────────────────────────────
@testset "delete_image! / delete_set!" begin
    proj = create_project!(name="del-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    a    = add_image!(s; name="a")
    b    = add_image!(s; name="b")

    delete_image!(s, b.uid)
    @test !(b.uid in s.image_uids)
    @test !isdir(joinpath(proj.root, "0", b.uid))
    @test !isdir(joinpath(proj.root, "1", b.uid))
    @test !(b.uid in init_object(proj.uid, s.uid).image_uids)   # persisted

    set_uid = s.uid
    delete_set!(proj, set_uid)
    @test !(set_uid in proj.set_uids)
    @test !isdir(joinpath(proj.root, "1", set_uid))
    @test !isdir(joinpath(proj.root, "1", a.uid))               # member removed too
    @test !(set_uid in load_project(proj.uid).set_uids)         # persisted

    rm(proj.root; recursive=true)
end

# ── Boundary contract: run a REAL module function end-to-end, no api/ ───────
# The whole suite loads only `using Cecelia` (api/ is not on the path). This runs
# an actual task (RemoveImage — real ccid.json + disk work, no external binary) to
# completion through the public `run_task` entrypoint. Catches coupling creeping back.
@testset "Boundary contract — real module fn end-to-end" begin
    proj = create_project!(name="bc-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")

    # register a real on-disk image version
    zarr = joinpath(img_zero_dir(img), "ccidImage.ome.zarr")
    mkpath(zarr)
    img.filepath["default"] = "ccidImage.ome.zarr"
    img.filepath["_active"] = "default"
    img.status = "done"
    save!(img)

    logs = String[]
    result = run_task(RemoveImage(), img,
        Dict{String,Any}("valueName"=>"default", "newDefault"=>"default");
        on_log = l -> push!(logs, l))

    @test result isa Dict
    @test result["removedValue"] == "default"
    @test result["cleared"] == true          # primary removal clears dims/status
    @test !isdir(zarr)                        # file actually deleted from disk
    @test !isempty(logs)                      # on_log callback fired (no WS needed)

    reloaded = init_object(proj.uid, img.uid)
    @test reloaded.status == "pending"        # primary removal reset status
    @test !haskey(reloaded.filepath, "default")  # version entry gone
    rm(proj.root; recursive=true)
end

# ── Storage reclaim — free every non-active image version, keep the active one ───────
@testset "Storage reclaim" begin
    # _path_bytes is the ONE "how big is this on disk" answer, shared by storage accounting, version
    # removal and the image-metadata modal — a directory is walked, a plain file is stat'd, and
    # anything absent is 0 rather than an error (a caller listing versions must not throw on a store
    # that isn't there).
    mktempdir() do d
        f = joinpath(d, "one.bin"); write(f, rand(UInt8, 4096))
        sub = joinpath(d, "store"); mkpath(joinpath(sub, "0"))
        write(joinpath(sub, "0", "chunk"), rand(UInt8, 8192))
        @test Cecelia._path_bytes(f) >= 4096                     # file: at least its bytes
        @test Cecelia._path_bytes(sub) >= 8192                   # dir: walked, not stat'd (a dir stat is ~4 KB)
        @test Cecelia._path_bytes(joinpath(d, "nope")) == 0      # absent, not an error
    end

    # pure policy: everything except the active version
    @test Set(Cecelia.reclaimable_versions(Dict{String,Any}(
        "default"=>"a", "afCorrected"=>"b", "cpCorrected"=>"c", "_active"=>"cpCorrected"))) ==
        Set(["default", "afCorrected"])
    @test isempty(Cecelia.reclaimable_versions(Dict{String,Any}(  # only the active version present
        "default"=>"a", "_active"=>"default")))
    # active is the original, but a leftover corrected variant is still freeable (NEW vs default-only)
    @test Cecelia.reclaimable_versions(Dict{String,Any}(
        "default"=>"a", "afCorrected"=>"b", "_active"=>"default")) == ["afCorrected"]

    _mk_ver!(img, fn) = (d = joinpath(img_zero_dir(img), fn); mkpath(d);
                         write(joinpath(d, "chunk"), rand(UInt8, 2048)); fn)

    proj = create_project!(name="stor-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")

    # imgA: original + af + cp, cp active → reclaim frees default AND af, keeps cp
    a = add_image!(s; name="a")
    _mk_ver!(a, "import.ome.zarr"); _mk_ver!(a, "af.ome.zarr"); _mk_ver!(a, "cp.ome.zarr")
    a.filepath = Dict("default"=>"import.ome.zarr", "afCorrected"=>"af.ome.zarr",
                      "cpCorrected"=>"cp.ome.zarr", "_active"=>"cpCorrected")
    a.im_channel_names = Dict{String,Any}("default"=>["ch0","ch1"], "_active"=>"default")
    a.meta = Dict{String,Any}("SizeC"=>2, "SizeT"=>1, "SizeZ"=>5)
    a.status = "done"; save!(a)

    # imgB: original only, active default → nothing to reclaim
    b = add_image!(s; name="b")
    _mk_ver!(b, "import.ome.zarr")
    b.filepath = Dict("default"=>"import.ome.zarr", "_active"=>"default")
    b.status = "done"; save!(b)

    # safe-primary unit: removing default while other versions remain must NOT un-import
    freed, cleared = remove_image_version!(a, "default", "cpCorrected")
    @test freed > 0 && cleared == false
    # restore default for the batch reclaim below
    _mk_ver!(a, "import.ome.zarr")
    ra0 = init_object(proj.uid, a.uid); ra0.filepath["default"] = "import.ome.zarr"; save!(ra0)

    # reclaim_inactive! frees ALL non-active (default + af), keeps cp; imgB skipped
    tot, reclaimed = reclaim_inactive!(proj.uid, [a.uid, b.uid])
    @test reclaimed == [a.uid]
    @test tot > 0
    @test !isdir(joinpath(img_zero_dir(a), "import.ome.zarr"))    # original gone
    @test !isdir(joinpath(img_zero_dir(a), "af.ome.zarr"))        # intermediate gone
    @test  isdir(joinpath(img_zero_dir(a), "cp.ome.zarr"))        # active kept
    @test  isdir(joinpath(img_zero_dir(b), "import.ome.zarr"))    # b untouched

    ra = init_object(proj.uid, a.uid)
    @test ra.status == "done"                                     # NOT un-imported
    @test ra.filepath["_active"] == "cpCorrected"
    @test collect(keys(filter(kv -> kv.first != "_active", ra.filepath))) == ["cpCorrected"]
    @test ra.meta["SizeC"] == 2                                   # dims kept
    @test Cecelia.versioned_get(ra.im_channel_names, "default") == ["ch0","ch1"]  # channel names kept
    rm(proj.root; recursive=true)
end

# ── Analysis reset: drop everything derived, keep the image ────────────────────
# The other half of the delete story (docs/todo/IMAGE_DELETE_PLAN.md): `remove_image_version!` sheds
# STORES, `reset_image_analysis!` sheds NUMBERS, and neither may do the other's job. The keep-list is
# asserted by NAME rather than by count, so adding a new analysis dir to the image layout fails here
# until it is deliberately classified (Decision 7 — a delete-list would have leaked it silently).
@testset "Analysis reset keeps the image and drops the numbers" begin
    @test Cecelia.ANALYSIS_KEEP == Set(["ccid.json", "runlog.json", "gating"])

    proj = create_project!(name="reset-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="a")

    # an image store (must survive) …
    zdir = joinpath(img_zero_dir(img), "import.ome.zarr"); mkpath(zdir)
    write(joinpath(zdir, "chunk"), rand(UInt8, 2048))
    img.filepath = Dict("default"=>"import.ome.zarr", "_active"=>"default")
    img.im_channel_names = Dict{String,Any}("default"=>["ch0"], "_active"=>"default")
    img.meta   = Dict{String,Any}("SizeC"=>1, "SizeT"=>1, "SizeZ"=>3)
    img.attr   = Dict{String,Any}("treatment"=>"CTRL")
    img.status = "done"
    img.labels = Dict("A"=>["A.zarr"])          # not versioned — a plain valueName → files map
    img.label_props = Dict("A"=>"A.h5ad")
    save!(img)

    # … and one file in every derived location, plus the things the keep-list protects
    for sub in ("labels", "labelProps", "populations", "stats", "mesh", "qc", "cl",
                "spatialGraph", "spatialStats", "branchLabels")
        mkpath(joinpath(img._dir, sub))
        write(joinpath(img._dir, sub, "x.bin"), rand(UInt8, 1024))
    end
    write(joinpath(img._dir, "runlog.json"), "[]")
    mkpath(joinpath(img._dir, "gating"))
    write(joinpath(img._dir, "gating", "A.json"), "{}")        # hand-drawn gates — must SURVIVE

    # the storage box's number IS what a reset would free — one accounting, so the box can't promise
    # bytes the reset doesn't deliver
    predicted = analysis_bytes_of(img)
    @test predicted > 0

    freed, dropped = reset_image_analysis!(img)

    @test freed == predicted
    @test analysis_bytes_of(img) == 0                              # nothing derived left to free
    @test freed > 0
    @test "labels" in dropped && "qc" in dropped && "spatialGraph" in dropped
    for sub in ("labels", "labelProps", "populations", "stats", "mesh", "qc", "cl",
                "spatialGraph", "spatialStats", "branchLabels")
        @test !ispath(joinpath(img._dir, sub))
    end
    # the keep-list survives, by name
    @test isfile(joinpath(img._dir, "ccid.json"))
    @test isfile(joinpath(img._dir, "runlog.json"))
    # gate polygons are user work, not output: a re-run under the same value_name reuses them
    @test isfile(joinpath(img._dir, "gating", "A.json"))
    @test !("ccid.json" in dropped) && !("runlog.json" in dropped) && !("gating" in dropped)

    # NO store is shed — that is remove_image_version!'s job (Decision 9)
    @test isdir(zdir)

    ri = init_object(proj.uid, img.uid)
    @test ri isa CciaImage
    @test Cecelia.versioned_get(ri.filepath, "default") == "import.ome.zarr"   # version untouched
    @test ri.filepath["_active"] == "default"
    @test ri.status == "done"                                                  # still imported
    @test ri.meta["SizeC"] == 1                                                # calibration/dims kept
    @test ri.attr["treatment"] == "CTRL"                                       # annotations kept
    # the analysis REGISTRATIONS are cleared, so nothing points at a deleted file
    @test isempty(ri.labels)
    @test isempty(ri.label_props)

    # idempotent: a second reset on an already-clean image is a no-op, not an error
    freed2, dropped2 = reset_image_analysis!(ri)
    @test freed2 == 0 && isempty(dropped2)

    rm(proj.root; recursive=true)
end

# ── A task crash is recorded in the per-image log, not just the console ──────
# Regression: a Julia-side failure (caught in _execute_job!) used to only @warn to the console —
# it never reached {img._dir}/logs/{fun}.log, so a crashed task looked like it just stopped
# mid-run with no error (invisible to get_task_log + on-disk debugging).
@testset "Task crash is teed into the per-image log" begin
    proj = create_project!(name="crash-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")

    logs = String[]
    result = run_task(_CrashTask(), img, Dict{String,Any}(); on_log = l -> push!(logs, l))

    @test result === nothing                                       # crash → nil result
    @test any(l -> occursin("Task crashed", l) && occursin("boom", l), logs)  # reached on_log

    fun_name = Cecelia._fun_name_from_task(_CrashTask())
    logfile  = joinpath(img._dir, "logs", fun_name * ".log")
    @test isfile(logfile)                                          # ...and the on-disk log
    @test occursin("boom", read(logfile, String))

    # ...AND the run log records the FAILED run (so history / the observer can see repeats, not
    # just successes). This is the fix for tasks that silently failed invisibly (broken run_py, HMM).
    rlog = read_run_log(img)
    @test length(rlog) == 1
    @test String(rlog[end]["fun"]) == fun_name && String(rlog[end]["status"]) == "failed"
    rm(proj.root; recursive=true)
end

# ── A job ALWAYS releases its submitter, even if the error path itself throws ──
# Regression: `_execute_job!` posted to `job.done` as its last statement, so any throw before that
# (here: the crash `@warn` itself failing) escaped into the dispatcher's fire-and-forget
# `Threads.@spawn` — silently. `run_task` then blocked in `take!(job.done)` FOREVER and never ran
# `_deregister_task!`, leaving the TaskRecord stranded at `:running`: `list_tasks()`, the GUI and
# the task console all keep listing a task that finished, while every pool correctly reads idle
# (the dispatcher's `finally` had released the slot). The post now lives in a `finally`.
@testset "Job posts its result even when the error path throws" begin
    proj = create_project!(name="job-post-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name="s"); name="img")

    tid = "jobpost$(rand(1000:9999))"
    old = global_logger()
    t = try
        global_logger(_ThrowingLogger(old))
        th = Threads.@spawn run_task(_CrashTask(), img, Dict{String,Any}(); task_id = tid)
        timedwait(() -> istaskdone(th), 30.0)        # wait BEFORE restoring, so the throw is injected
        th
    finally
        global_logger(old)                            # a failing @test must not log through it
    end

    @test istaskdone(t)                               # the whole point — this used to hang forever
    # guarded: on a regression the task is still blocked, and a bare fetch would hang the SUITE
    istaskdone(t) && @test fetch(t) === nothing       # aborted job → nil result, like any failure
    # ...and the record is gone, not stranded at :running for the console/GUI to keep showing
    @test !any(r -> r.id == tid, list_tasks())
    rm(proj.root; recursive=true)
end

# ── A terminal task-rail frame is banked for replay ────────────────────────
# Regression (the "0 done · 17 ended" console, and a project export stuck at "running"): the frame
# announcing HOW a unit of work ended is its only carrier and the server drops frames for a slow
# client BY DESIGN, so a client that missed it could never find out. `record_task_outcome!` keeps it.
# Banked from `ws_status` (see api/test) — the rail's one status sink, so background jobs and batch
# movies are covered too, not just scheduler tasks. Here: the log's own contract.
@testset "Task outcome log" begin
    empty!(Cecelia._OUTCOMES)
    rec!(id, status; kw...) = record_task_outcome!(id, status; kw...)

    rec!("o1", "done"; image_uid="img1", fun="segment.cellpose", pool="gpu")
    rec!("o2", "failed"; image_uid="img2", fun="project:export", pool="job")
    rec!("o3", "cancelled"; image_uid="img3")
    rows = recent_tasks()
    @test [r.id for r in rows] == ["o1", "o2", "o3"]              # oldest → newest
    @test [r.status for r in rows] == ["done", "failed", "cancelled"]
    let o1 = first(rows)
        @test o1.fun_name == "segment.cellpose" && o1.pool_name == "gpu"
        @test o1.image_uid == "img1" && o1.image_uids == String[]
        @test occursin(r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$", o1.finished_at)
    end

    # a non-terminal status is NOT an outcome — the one call site hands over every status frame, so
    # banking "running" here would report work that is still going as finished.
    rec!("live", "queued"); rec!("live", "running")
    @test !any(r -> r.id == "live", recent_tasks())

    # a set-scope task's FULL member list survives: it exists only on this frame, and a replay
    # without it invalidates the representative image's plots only (docs/API.md).
    rec!("set1", "done"; image_uid="a", image_uids=["a", "b", "c"])
    @test only(filter(r -> r.id == "set1", recent_tasks())).image_uids == ["a", "b", "c"]

    # ONE row per task id. Both repeats are real: a cancel is announced twice (immediately, then as
    # the final status), and task:restart reuses the id for a new run that must supersede the old.
    rec!("o1", "done"); rec!("o1", "done")
    @test count(r -> r.id == "o1", recent_tasks()) == 1
    rec!("o2", "done")                                            # restarted → new outcome wins
    @test only(filter(r -> r.id == "o2", recent_tasks())).status == "done"
    @test last(recent_tasks()).id == "o2"                         # …and re-appended, still in order

    # `since` is INCLUSIVE, so a poll always re-reads its own newest entry (two units finishing in
    # the same millisecond must not let the second fall through the gap).
    newest = last(recent_tasks()).finished_at
    @test any(r -> r.finished_at == newest, recent_tasks(; since = newest))
    @test isempty(recent_tasks(; since = "9999-01-01T00:00:00.000Z"))
    @test length(recent_tasks(; since = "")) == length(recent_tasks())

    # bounded: a reporting tail for live clients, never run history (that's the on-disk run log)
    for i in 1:(Cecelia._OUTCOME_CAP + 50); rec!("cap$i", "done"); end
    @test length(Cecelia._OUTCOMES) == Cecelia._OUTCOME_CAP
    @test last(recent_tasks()).id == "cap$(Cecelia._OUTCOME_CAP + 50)"   # newest kept
    @test !any(r -> r.id == "o1", recent_tasks())                        # oldest evicted
    empty!(Cecelia._OUTCOMES)
end

# ── The scheduler stamps a task's own timing ───────────────────────────────
# `list_tasks()` is what a client polls to answer "how long has this been going?". Without these
# fields it can only be answered from when the client first SAW the row, so a console or a browser
# tab that attached mid-run could report a lower bound and nothing better (the task console printed
# `≥0s` for a task that had been running for 20 minutes).
@testset "Scheduler records queued/started timestamps" begin
    proj = create_project!(name="tasktime-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name="s"); name="img")

    _HOLD_TASK_GO[] = Channel{Nothing}(1)
    tid  = "hold$(rand(1000:9999))"
    seen = TaskRecord[]
    th = Threads.@spawn run_task(_HoldTask(), img, Dict{String,Any}("modelType" => "cyto3",
                                                                    "diameter"  => 17);
                                 task_id = tid, on_status_change = rec -> push!(seen, rec))
    try
        @test timedwait(() -> any(r -> r.id == tid && r.status == "running", list_tasks()), 30.0) === :ok
        row = only(filter(r -> r.id == tid, list_tasks()))
        # The snapshot's FIELD NAMES are a contract with two independent consumers that share no runtime:
        # `_reconcile_snapshot!` (api/task_console.jl) and `adoptableTasks`
        # (frontend/src/utils/runningTasks.ts), each of which silently blanks a column if one is renamed.
        # Pinned here so a rename fails a test instead — the frontend pins its own half in
        # runningTasks.test.ts.
        @test issubset(Set([:id, :fun_name, :pool_name, :image_uid, :chain_run_id, :chain_node_id,
                            :status, :queued_at, :started_at, :params]), Set(keys(row)))
        # …and the params it was SUBMITTED with, which is what lets a client that didn't launch the task
        # offer Re-run: with only the fun_name it would relaunch on the JSON spec's defaults instead.
        @test row.params == Dict{String,Any}("modelType" => "cyto3", "diameter" => 17)
        # both are ISO-8601 UTC to the millisecond — one wire format for the whole rail
        @test occursin(r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$", row.queued_at)
        @test occursin(r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$", row.started_at)
        @test row.started_at >= row.queued_at            # a slot is acquired after submission
        # …and the SAME start is on the rail, so it still answers once this record is gone
        @test iso_utc(task_started_at(tid)) == row.started_at
        # the status frames the API sends carry it too (that's what `on_status_change` feeds)
        running = last(filter(r -> r.status === :running, seen))
        @test !isnothing(running.started_at) && iso_utc(running.started_at) == row.started_at
    finally
        put!(_HOLD_TASK_GO[], nothing)                    # let the task finish even if a @test failed
        timedwait(() -> istaskdone(th), 30.0)
    end
    @test istaskdone(th) && fetch(th) === true
    @test !any(r -> r.id == tid, list_tasks())            # record gone…
    @test !isnothing(task_started_at(tid))                # …but the start outlived it
    forget_task_start!(tid)

    # A chain node reports WHICH node it is, not just which run — the GUI keys a chain row
    # `runId::nodeId::imageUid`, so a snapshot row without the node id can't be matched to one and the
    # same work would be listed twice (once adopted, once from the chain events).
    rec = Cecelia._register_task!("cn$(rand(1000:9999))", "segment.cellpose", "gpu", img.uid,
                                 "run1", _ -> nothing; chain_node_id = "n3")
    let row = only(filter(r -> r.id == rec.id, list_tasks()))
        @test row.chain_run_id == "run1" && row.chain_node_id == "n3"
    end
    Cecelia._deregister_task!(rec.id)

    # a task still QUEUED has a queue time and NO start — so a client shows a wait, not a run of 0s
    rec = Cecelia._register_task!("q$(rand(1000:9999))", "f", "cpu", img.uid, "", _ -> nothing)
    @test isnothing(rec.started_at)
    @test only(filter(r -> r.id == rec.id, list_tasks())).started_at == ""
    # …and a task registered with no params reports an EMPTY set, never a missing field: the frontend
    # reads an absent `params` as "unknown, withhold Re-run" and an empty one as "this task takes none".
    @test only(filter(r -> r.id == rec.id, list_tasks())).params == Dict{String,Any}()
    Cecelia._deregister_task!(rec.id)

    # The whole snapshot is written in ONE JSON3.write, so an unserialisable param value would throw and
    # take `/api/tasks` down for every row — no adoption, no console reconcile, and a quit busy-check
    # that reads idle. Params from the GUI are parsed JSON and always fine; a REPL-dispatched run can
    # put anything in the dict. Published as `null` instead, which the client reads as "unknown".
    rec = Cecelia._register_task!("np$(rand(1000:9999))", "f", "cpu", img.uid, "", _ -> nothing;
                                  params = Dict{String,Any}("fn" => sin, "diameter" => 17))
    let row = only(filter(r -> r.id == rec.id, list_tasks()))
        @test isnothing(row.params)                      # all-or-nothing — NOT a partial dict
        @test JSON3.write(row) isa String                # the endpoint still answers
    end
    Cecelia._deregister_task!(rec.id)

    # …and the nested shapes params actually take DO survive (a group param is a dict of dicts), plus the
    # ones Julia code writes naturally, so a REPL-dispatched run isn't denied Re-run over a tuple.
    nested = Dict{String,Any}("models" => Any["cyto3", "nuclei"], "opts" => Dict("d" => 17, "gpu" => true),
                              "unset" => nothing, "name" => :cellpose,
                              "range" => (1, 10), "shape" => (w = 5, h = 6))
    rec = Cecelia._register_task!("ok$(rand(1000:9999))", "f", "cpu", img.uid, "", _ -> nothing;
                                  params = nested)
    let row = only(filter(r -> r.id == rec.id, list_tasks()))
        @test row.params == nested
        @test JSON3.write(row) isa String
    end
    Cecelia._deregister_task!(rec.id)

    # A whitelist, NOT a `try JSON3.write` probe: JSON3 throws on a Function but happily serialises a
    # plain struct INTO AN OBJECT, so a probe would publish that and a client would Re-run on a value
    # that is not what the task ran with. Anything whose JSON form isn't the value it came from must
    # read as unknown — which is why this is a predicate over shapes, not an attempted write.
    @test !Cecelia._json_writable(img)
    rec = Cecelia._register_task!("st$(rand(1000:9999))", "f", "cpu", img.uid, "", _ -> nothing;
                                  params = Dict{String,Any}("img" => img))
    @test isnothing(only(filter(r -> r.id == rec.id, list_tasks())).params)
    Cecelia._deregister_task!(rec.id)
    rm(proj.root; recursive=true)
end

# ── When a unit of work started ────────────────────────────────────────────
# The other half of the same problem as the outcome log: the scheduler's record is deregistered the
# instant a task finishes, and the consumers that want a DURATION mostly ask afterwards (the chain
# bridge fires node:done once run_task has returned; a dropped terminal frame is recovered minutes
# later). So the start is noted on the rail, and the banked outcome row carries it from then on —
# without which every client has to time tasks off when it first happened to see them.
@testset "Task start timing" begin
    empty!(Cecelia._OUTCOMES); empty!(Cecelia._STARTED)

    # first note wins, so a repeated `running` announcement does not restart the clock
    began = Dates.now(UTC) - Dates.Minute(5)
    @test note_task_started!("t1", began) == began
    @test note_task_started!("t1", Dates.now(UTC)) == began       # ← would have reset the elapsed
    @test task_started_at("t1") == began

    # not started / not noted is `nothing`, never a zero date — a client must be able to tell
    @test isnothing(task_started_at("never-seen"))
    @test isnothing(task_started_at(""))
    @test iso_utc(nothing) == ""                                   # …and it serialises as "", not epoch 0

    # the banked outcome carries the start, and the in-flight note is then dropped: one home for the
    # fact at a time, so nothing can report two different starts for the same task.
    row = record_task_outcome!("t1", "done"; image_uid="img1")
    @test row.started_at == iso_utc(began)
    @test isnothing(task_started_at("t1"))
    @test only(filter(r -> r.id == "t1", recent_tasks())).started_at == iso_utc(began)
    @test row.finished_at >= row.started_at

    # a task that never ran banks an empty start rather than a made-up one
    @test record_task_outcome!("t2", "cancelled").started_at == ""

    # a non-terminal status still returns nothing — the caller uses that to tell live from finished
    @test isnothing(record_task_outcome!("t3", "running"))

    # a reused id (task:restart) is timed from its own beginning, not the previous run's
    note_task_started!("t4", began)
    forget_task_start!("t4")
    @test isnothing(task_started_at("t4"))

    # bounded, like the outcome log: a producer that never announces an outcome must not accumulate
    # forever. The OLDEST starts are evicted — a long-running task is the one whose elapsed matters.
    empty!(Cecelia._STARTED)
    base = Dates.now(UTC) - Dates.Hour(1)
    for i in 1:(Cecelia._STARTED_CAP + 10)
        note_task_started!("s$i", base + Dates.Millisecond(i))
    end
    @test length(Cecelia._STARTED) <= Cecelia._STARTED_CAP
    @test !isnothing(task_started_at("s$(Cecelia._STARTED_CAP + 10)"))   # newest kept
    @test isnothing(task_started_at("s1"))                              # oldest evicted
    empty!(Cecelia._STARTED); empty!(Cecelia._OUTCOMES)
end

@testset "Run log records status (done + failed)" begin
    proj = create_project!(name="rl-status-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name="s"); name="img")
    append_run_log!(img, "segment.cellpose", "default")              # default status = done
    append_run_log!(img, "behaviour.hmm", "", "failed")
    rl = read_run_log(img)
    @test String(rl[1]["status"]) == "done"
    @test String(rl[2]["fun"]) == "behaviour.hmm" && String(rl[2]["status"]) == "failed"
    rm(proj.root; recursive=true)
end

# ── Set expansion — a set resolves to its correct member UIDs ───────────────
# run_tasks and the batch accessors depend on this everywhere.
@testset "Set expansion" begin
    proj = create_project!(name="se-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    a = add_image!(s; name="a"); b = add_image!(s; name="b"); c = add_image!(s; name="c")
    expected = Set([a.uid, b.uid, c.uid])

    @test Set(i.uid for i in images(s))    == expected   # in-memory
    @test Set(i.uid for i in images(proj)) == expected
    reloaded = init_object(proj.uid, s.uid)              # reload from disk
    @test reloaded isa CciaSet
    @test Set(reloaded.image_uids)            == expected
    @test Set(i.uid for i in images(reloaded)) == expected
    rm(proj.root; recursive=true)
end

# ── Chain template round-trip ─────────────────────────────────────────────
# ── Chain node scope defaults from the task spec (single source of truth) ────
# A node built without an explicit scope inherits the task JSON's "scope": set-scope
# tasks (behaviour.hmm, clustTracks.cluster) become picnic nodes automatically, image
# tasks stay image-scope. An explicit scope always overrides.
@testset "Chain node scope inherits from task spec" begin
    @test Cecelia._task_default_scope("clustTracks.cluster") == "set"
    @test Cecelia._task_default_scope("behaviour.hmm")        == "set"
    @test Cecelia._task_default_scope("importImages.remove")  == "image"
    @test Cecelia._task_default_scope("nonexistent.task")     == "image"   # unknown fn → image
    # EVERY set-scope task declares it in its own spec — including the mock, which used to rely on
    # each chain node passing scope="set" (the one task that contradicted "the spec is the single
    # source of truth", and it's the fixture the barrier tests are built on).
    @test Cecelia._task_default_scope("testTasks.set_task")    == "set"
    @test chain_node("testTasks.set_task").scope               == "set"

    # chain_node / ChainNode with no scope kwarg resolve from the spec …
    @test chain_node("clustTracks.cluster").scope == "set"
    @test chain_node("importImages.remove").scope == "image"
    @test ChainNode(id="x", fn="behaviour.hmm").scope == "set"
    # … and an explicit scope still wins (force a set task to run per-image)
    @test chain_node("clustTracks.cluster"; scope="image").scope == "image"

    # Deserialisation: a node dict with no "scope" key also inherits from the spec
    @test Cecelia._node_from_dict(Dict("id"=>"n", "fn"=>"clustTracks.cluster")).scope == "set"
    # …while a stored scope (frozen template) is honoured verbatim
    @test Cecelia._node_from_dict(Dict("id"=>"n", "fn"=>"clustTracks.cluster",
                                       "scope"=>"image")).scope == "image"
end

# ── Producer output value_name is declared in the JSON spec (introspectable) ──
# The whiteboard reads this to prefill a downstream node's input `valueName`.
@testset "Output value_name from spec" begin
    @test Cecelia._spec_output_value_name(CellposeCorrect(), "fallback") == "cpCorrected"
    @test Cecelia._spec_output_value_name(DriftCorrect(),    "fallback") == "driftCorrected"
    @test Cecelia._spec_output_value_name(AfCorrect(),       "fallback") == "afCorrected"
    @test Cecelia._spec_output_value_name(Smooth(),          "fallback") == "smoothed"
    # A task that declares no top-level outputValueName falls back to the caller's default
    @test Cecelia._spec_output_value_name(RemoveImage(), "fallback") == "fallback"
end

@testset "Chain template round-trip" begin
    proj = create_project!(name="chain-tpl-$(rand(1000:9999))")

    tpl = ChainTemplate(
        "test-chain",
        [ChainNode(id="n1", fn="importImages.remove", params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default")),
         ChainNode(id="n2", fn="importImages.remove", params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default"))],
        [ChainEdge("n1", "n2")],
    )
    save_chain_template!(proj, tpl)

    # Templates must land under settings/chains — the SAME dir the API reads/writes
    # (api/src/routes.jl _chains_dir_for_project). A divergence here made every whiteboard
    # chain run fail with "template not found" (saved via API, loaded via package).
    @test isfile(joinpath(proj.root, "settings", "chains", "test-chain.json"))
    @test !isfile(joinpath(proj.root, "chains", "test-chain.json"))

    loaded = load_chain_template(proj, "test-chain")
    @test loaded.name == "test-chain"
    @test length(loaded.nodes) == 2
    @test loaded.nodes[1].id == "n1"
    @test loaded.nodes[2].id == "n2"
    @test length(loaded.edges) == 1
    @test loaded.edges[1].from == "n1"
    @test loaded.edges[1].to   == "n2"

    rm(proj.root; recursive=true)
end

# ── Template validation (author-time, for every author that isn't the whiteboard) ──
# The whiteboard can't produce an invalid template; the REPL, a hand-edited file and Claude (via the
# MCP `create_chain`) can. Before this, a typo'd fn or a dangling edge surfaced only when the USER
# pressed Run — see the header comment in chain.jl → Template validation.
@testset "validate_chain_template" begin
    node(id, fn; kw...) = ChainNode(; id=id, fn=fn, kw...)
    ok_node(id) = node(id, "importImages.remove";
                       params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default"))
    tpl(nodes, edges; starts=String[]) = ChainTemplate("t", nodes, edges, starts)

    # A well-formed template validates, and returns nothing (not a value to test against)
    @test validate_chain_template(
        tpl([ok_node("n1"), ok_node("n2")], [ChainEdge("n1", "n2")])) === nothing

    # SPARSE params must pass — this is how a non-GUI author writes a node: set only what you mean
    # to change and let the whiteboard fill the rest from the spec defaults on load (applyTemplate).
    # If this ever fails, Claude is forced to restate every default, which is the bug.
    @test validate_chain_template(
        tpl([node("n1", "tracking.bayesian_tracking";
                  params=Dict{String,Any}("maxSearchRadius"=>35))], ChainEdge[])) === nothing
    @test validate_chain_template(tpl([node("n1", "importImages.remove")], ChainEdge[])) === nothing

    bad(t) = @test_throws ChainTemplateError validate_chain_template(t)

    bad(tpl(ChainNode[], ChainEdge[]))                              # nothing to run
    bad(tpl([node("", "importImages.remove")], ChainEdge[]))        # empty id
    bad(tpl([ok_node("n1"), ok_node("n1")], ChainEdge[]))           # duplicate id
    bad(tpl([node("n1", "importImages.nope")], ChainEdge[]))        # unknown fn
    bad(tpl([node("n1", "importImages.remove"; scope="picnic")], ChainEdge[]))
    bad(tpl([node("n1", "importImages.remove"; barrier_policy="maybe")], ChainEdge[]))
    bad(tpl([node("n1", "importImages.remove"; resource_pool="gpu-light")], ChainEdge[]))

    # Both edge endpoints. A dangling `from` is a run-time KeyError in _topo_sort; a dangling `to`
    # silently never runs (in-degree never reaches 0) — the worse of the two, so both are errors.
    bad(tpl([ok_node("n1")], [ChainEdge("ghost", "n1")]))
    bad(tpl([ok_node("n1")], [ChainEdge("n1", "ghost")]))
    bad(tpl([ok_node("n1")], [ChainEdge("n1", "n1")]))              # self-dependency
    bad(tpl([ok_node("n1"), ok_node("n2")],                         # cycle
            [ChainEdge("n1", "n2"), ChainEdge("n2", "n1")]))
    bad(tpl([ok_node("n1")], ChainEdge[]; starts=["ghost"]))        # startTargets must be a node
    bad(tpl([node("n1", "tracking.bayesian_tracking";               # param out of spec range
                  params=Dict{String,Any}("maxSearchRadius"=>9999))], ChainEdge[]))

    # A configured pool name and the inherit-from-spec empty string are both fine
    @test validate_chain_template(
        tpl([node("n1", "importImages.remove"; resource_pool="gpu")], ChainEdge[])) === nothing
    @test validate_chain_template(
        tpl([node("n1", "importImages.remove"; resource_pool="")], ChainEdge[])) === nothing

    # The message names the offending node, so a rejected author knows what to fix
    err = try validate_chain_template(tpl([node("bad-one", "importImages.nope")], ChainEdge[]))
          catch e; e end
    @test err isa ChainTemplateError
    @test occursin("bad-one", err.msg) && occursin("importImages.nope", err.msg)

    # Roots = where a run begins. The whiteboard draws no start dot for a template with neither a
    # start target nor a saved position, so an authored chain gets its roots filled from this.
    @test chain_root_ids(tpl([ok_node("a"), ok_node("b")], [ChainEdge("a", "b")])) == ["a"]
    @test chain_root_ids(tpl([ok_node("a")], ChainEdge[])) == ["a"]
    # template order is preserved, and a fan-in has one root per unfed branch
    @test chain_root_ids(tpl([ok_node("a"), ok_node("b"), ok_node("c")],
                             [ChainEdge("a", "c"), ChainEdge("b", "c")])) == ["a", "b"]
    @test isempty(chain_root_ids(tpl([ok_node("a"), ok_node("b")],       # a cycle has no root
                                     [ChainEdge("a", "b"), ChainEdge("b", "a")])))
end

# ── Chain run — template frozen, per-image state, pipelining ─────────────
@testset "Chain run — end-to-end with RemoveImage" begin
    proj = create_project!(name="chain-run-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")

    # Two images, each with a registered filepath for RemoveImage to remove
    imgs = map(("img-a", "img-b")) do nm
        img = add_image!(s; name=nm)
        zarr = joinpath(dirname(dirname(img._dir)), "0", img.uid, "ccidImage.ome.zarr")
        mkpath(zarr)
        img.filepath["default"] = "ccidImage.ome.zarr"
        img.filepath["_active"] = "default"
        img.status = "done"
        save!(img)
        img
    end

    tpl = ChainTemplate(
        "remove-chain",
        [ChainNode(id="n1", fn="importImages.remove",
                   params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default"))],
        ChainEdge[],
    )
    save_chain_template!(proj, tpl)

    logs = String[]
    run = run_chain(proj, [i.uid for i in imgs];
                    chain="remove-chain",
                    on_log = line -> push!(logs, line))

    # Run record persisted
    @test isfile(joinpath(run._dir, "run.json"))

    # Template frozen in-memory
    @test run.template_snapshot.name == "remove-chain"
    @test length(run.template_snapshot.nodes) == 1

    # Both images completed node n1
    for img in imgs
        @test run.image_states[img.uid]["n1"].status == :done
    end

    # on_log fired for both images
    @test !isempty(logs)

    # run.json stores hash reference, not embedded template
    raw = JSON3.read(read(joinpath(run._dir, "run.json"), String), Dict{String,Any})
    @test length(raw["image_uids"]) == 2
    @test haskey(raw["image_states"], imgs[1].uid)
    @test haskey(raw["image_states"], imgs[2].uid)
    @test haskey(raw, "template_hash")
    @test !isempty(raw["template_hash"])
    @test !haskey(raw, "template_snapshot")

    # Cache entry exists and round-trips back to the original template
    cached = load_template_from_cache(proj, run.template_hash)
    @test cached.name == "remove-chain"
    @test length(cached.nodes) == 1

    rm(proj.root; recursive=true)
end

# ── Chain resume — explicit start node (re-run from here) ─────────────────
@testset "Chain resume — start node force-restart" begin
    # descendants: pure graph reachability over n1→n2→n3
    tpl = ChainTemplate(
        "restart-chain",
        [ChainNode(id="n1", fn="importImages.remove",
                   params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default")),
         ChainNode(id="n2", fn="importImages.remove",
                   params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default")),
         ChainNode(id="n3", fn="importImages.remove",
                   params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default"))],
        [ChainEdge("n1","n2"), ChainEdge("n2","n3")],
    )
    @test Cecelia._descendants(tpl, "n1") == Set(["n2","n3"])
    @test Cecelia._descendants(tpl, "n2") == Set(["n3"])
    @test isempty(Cecelia._descendants(tpl, "n3"))

    # force-restart from n2 on a run whose nodes are all :done → n2,n3 reset to :pending; n1 kept
    proj = create_project!(name="chain-restart-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")
    img.status = "done"; save!(img)
    states = Dict(img.uid => Dict(
        "n1" => Cecelia.ImageNodeState(), "n2" => Cecelia.ImageNodeState(),
        "n3" => Cecelia.ImageNodeState()))
    for nid in ("n1","n2","n3")
        states[img.uid][nid].status      = :done
        states[img.uid][nid].params_hash = "h"
    end
    run = Cecelia.ChainRun("rid", "restart-chain", proj.uid, [img.uid], tpl,
                           "hash", states, time(), joinpath(Cecelia._runs_dir(proj), "rid"),
                           ReentrantLock(), Dict{String,Channel{Nothing}}(),
                           Dict{String,Channel{Nothing}}())
    mkpath(run._dir)
    Cecelia._force_restart_from!(run, "n2")
    @test run.image_states[img.uid]["n1"].status == :done       # upstream untouched
    @test run.image_states[img.uid]["n2"].status == :pending    # start node reset
    @test run.image_states[img.uid]["n3"].status == :pending    # downstream reset
    @test run.image_states[img.uid]["n2"].params_hash === nothing

    rm(proj.root; recursive=true)
end

# ── Chain start dot — prune to the reachable subgraph ─────────────────────
@testset "Chain start dot — prune to reachable subgraph" begin
    tpl = ChainTemplate(
        "start-chain",
        [ChainNode(id="a", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="b", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="c", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="d", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="x", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="y", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        [ChainEdge("a","b"), ChainEdge("b","c"), ChainEdge("c","d"), ChainEdge("x","y")],
        ["c"],                                          # start dot → c (mid-chain)
    )
    pruned = Cecelia._prune_to_start(tpl)
    @test Set(n.id for n in pruned.nodes) == Set(["c","d"])        # c + downstream only
    @test Set((e.from, e.to) for e in pruned.edges) == Set([("c","d")])
    @test isempty(pruned.start_targets)                            # consumed into the node set
    # disconnected draft branch (x→y) dropped; a→b upstream of the start dot dropped too
    @test !any(n.id in ("a","b","x","y") for n in pruned.nodes)
    # no start targets ⇒ unchanged (run the whole chain)
    @test length(Cecelia._prune_to_start(ChainTemplate("t", tpl.nodes, tpl.edges)).nodes) == 6
    # start dot pointing ONLY at since-deleted nodes ⇒ fall back to run-all, not an empty run
    stale = Cecelia._prune_to_start(ChainTemplate("t", tpl.nodes, tpl.edges, ["ghost"]))
    @test length(stale.nodes) == 6
    @test isempty(stale.start_targets)
end

# ── Chain run — set-scope (picnic) node ──────────────────────────────────
@testset "Chain run — picnic node" begin
    proj = create_project!(name="chain-picnic-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b", "img-c")) do nm
        add_image!(s; name=nm)
    end

    # n1 (image) → n2 (set-scope) → n3 (image)
    tpl = ChainTemplate(
        "picnic-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.set_task",   scope="set",   params=Dict{String,Any}()),
         ChainNode(id="n3", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        [ChainEdge("n1","n2"), ChainEdge("n2","n3")],
    )
    save_chain_template!(proj, tpl)

    logs = String[]
    run = run_chain(proj, [i.uid for i in imgs];
                    chain="picnic-chain",
                    on_log = line -> push!(logs, line))

    # All per-image nodes completed for every image
    for img in imgs
        @test run.image_states[img.uid]["n1"].status == :done
        @test run.image_states[img.uid]["n3"].status == :done
    end

    # Set-scope node: all images show :done, result contains the full image count
    for img in imgs
        @test run.image_states[img.uid]["n2"].status == :done
        @test run.image_states[img.uid]["n2"].result["image_count"] == 3
    end

    # Set-scope task log appeared exactly once (ran once, not once per image)
    set_logs = filter(l -> contains(l, "setTask ran"), logs)
    @test length(set_logs) == 1

    rm(proj.root; recursive=true)
end

# ── Chain start dot — end-to-end run prunes to the reachable subgraph ─────
# Reservation guard: pruning to a start dot must still work when the reachable subgraph contains a
# picnic (set-scope barrier) node. The target becomes a root — its dropped upstream doesn't block
# it — and the barrier still fires once across all images. Also covers the save/load round-trip of
# `startTargets` and that pruned-out nodes never enter the run.
@testset "Chain start dot — run prunes to subgraph (set-scope)" begin
    proj = create_project!(name="chain-startrun-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm); end

    # n1(image) → n2(set) → n3(image); start dot → n2, so n1 is an upstream draft (excluded)
    tpl = ChainTemplate(
        "startrun-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.set_task",   scope="set",   params=Dict{String,Any}()),
         ChainNode(id="n3", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        [ChainEdge("n1","n2"), ChainEdge("n2","n3")],
        ["n2"],                                        # start dot → n2 (a set-scope node)
    )
    save_chain_template!(proj, tpl)

    logs = String[]
    run = run_chain(proj, [i.uid for i in imgs]; chain="startrun-chain",
                    on_log = line -> push!(logs, line))

    # only the reachable subgraph exists in the run — n1 pruned out entirely
    @test Set(keys(run.image_states[imgs[1].uid])) == Set(["n2", "n3"])
    for img in imgs
        @test run.image_states[img.uid]["n2"].status == :done   # set-scope barrier fired as root
        @test run.image_states[img.uid]["n3"].status == :done
    end
    @test run.image_states[imgs[1].uid]["n2"].result["image_count"] == 2
    @test length(filter(l -> contains(l, "setTask ran"), logs)) == 1   # ran once, not per image

    rm(proj.root; recursive=true)
end

# ── Fault isolation is per-predecessor, not global (DAG fan-out) ─────────────
# A failed branch must not skip a SIBLING branch that shares only an upstream ancestor
# (e.g. afDriftCorrect → two independent segmentations). Regression guard for the
# over-broad "any node failed → skip" check that skipped independent branches.
@testset "Chain fault isolation — independent fan-out" begin
    proj = create_project!(name="chain-fanout-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")

    # a → { b (fails), c (ok) } — c is independent of b, must still run
    save_chain_template!(proj, ChainTemplate("fanout",
        [ChainNode(id="a", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="b", fn="nonexistent.task",    scope="image", params=Dict{String,Any}()),
         ChainNode(id="c", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        [ChainEdge("a","b"), ChainEdge("a","c")]))
    st = run_chain(proj, [img.uid]; chain="fanout").image_states[img.uid]
    @test st["a"].status == :done
    @test st["b"].status == :failed
    @test st["c"].status == :done       # independent sibling — NOT skipped by b's failure

    # a (fails) → { b, c } — the shared ancestor failing skips BOTH branches
    save_chain_template!(proj, ChainTemplate("fanout-root-fail",
        [ChainNode(id="a", fn="nonexistent.task",    scope="image", params=Dict{String,Any}()),
         ChainNode(id="b", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="c", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        [ChainEdge("a","b"), ChainEdge("a","c")]))
    st2 = run_chain(proj, [img.uid]; chain="fanout-root-fail").image_states[img.uid]
    @test st2["a"].status == :failed
    @test st2["b"].status == :skipped
    @test st2["c"].status == :skipped

    # transitive: a → b(fail) → c → d — skip propagates down the branch via :skipped
    save_chain_template!(proj, ChainTemplate("chain-transitive",
        [ChainNode(id="a", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="b", fn="nonexistent.task",    scope="image", params=Dict{String,Any}()),
         ChainNode(id="c", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="d", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        [ChainEdge("a","b"), ChainEdge("b","c"), ChainEdge("c","d")]))
    st3 = run_chain(proj, [img.uid]; chain="chain-transitive").image_states[img.uid]
    @test st3["a"].status == :done
    @test st3["b"].status == :failed
    @test st3["c"].status == :skipped   # pred b failed
    @test st3["d"].status == :skipped   # pred c skipped → propagates

    rm(proj.root; recursive=true)
end

# ── Picnic node — require_all aborts if any image failed upstream ────────
@testset "Picnic node — require_all policy" begin
    proj = create_project!(name="picnic-req-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

    # n1 is a bad fn — both images will fail there
    # n2 is require_all — should abort since n1 failed
    tpl = ChainTemplate(
        "req-chain",
        [ChainNode(id="n1", fn="nonexistent.task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.set_task", scope="set",
                   params=Dict{String,Any}(), barrier_policy="require_all")],
        [ChainEdge("n1","n2")],
    )
    save_chain_template!(proj, tpl)

    run = run_chain(proj, [i.uid for i in imgs]; chain="req-chain", on_log=_->nothing)

    for img in imgs
        @test run.image_states[img.uid]["n1"].status == :failed
        @test run.image_states[img.uid]["n2"].status == :failed
    end
    rm(proj.root; recursive=true)
end

# ── Picnic node — successful_only aborts when no images eligible ─────────
@testset "Picnic node — successful_only policy" begin
    proj = create_project!(name="picnic-ok-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

    # n1: bad fn — both images fail upstream
    # n2 (successful_only): zero eligible images → should abort, both :failed
    tpl = ChainTemplate(
        "ok-chain",
        [ChainNode(id="n1", fn="nonexistent.task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.set_task", scope="set",
                   params=Dict{String,Any}(), barrier_policy="successful_only")],
        [ChainEdge("n1","n2")],
    )
    save_chain_template!(proj, tpl)

    run = run_chain(proj, [i.uid for i in imgs]; chain="ok-chain", on_log=_->nothing)

    for img in imgs
        @test run.image_states[img.uid]["n1"].status == :failed
        # no eligible images → set-scope node also fails
        @test run.image_states[img.uid]["n2"].status == :failed
    end
    rm(proj.root; recursive=true)
end

# ── Picnic node — successful_only runs with eligible subset ──────────────
@testset "Picnic node — successful_only with all passing" begin
    proj = create_project!(name="picnic-pass-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

    # n1: always succeeds → both images eligible → task runs with all 2
    tpl = ChainTemplate(
        "pass-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.set_task", scope="set",
                   params=Dict{String,Any}(), barrier_policy="successful_only")],
        [ChainEdge("n1","n2")],
    )
    save_chain_template!(proj, tpl)

    run = run_chain(proj, [i.uid for i in imgs]; chain="pass-chain", on_log=_->nothing)

    for img in imgs
        @test run.image_states[img.uid]["n1"].status == :done
        @test run.image_states[img.uid]["n2"].status == :done
    end
    @test run.image_states[imgs[1].uid]["n2"].result["image_count"] == 2
    rm(proj.root; recursive=true)
end

# ── Chain run — overrides applied, bad fn isolated ────────────────────────
@testset "Chain run — overrides + fault isolation" begin
    proj = create_project!(name="chain-iso-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")

    imgs = map(("img-a", "img-b")) do nm
        img = add_image!(s; name=nm)
        zarr = joinpath(dirname(dirname(img._dir)), "0", img.uid, "ccidImage.ome.zarr")
        mkpath(zarr)
        img.filepath["default"] = "ccidImage.ome.zarr"
        img.filepath["_active"] = "default"
        img.status = "done"
        save!(img)
        img
    end

    # n1: bad fn (will fail for both images)
    # n2: valid fn — should be skipped because n1 failed
    tpl = ChainTemplate(
        "fault-chain",
        [ChainNode(id="n1", fn="nonexistent.task",  params=Dict{String,Any}()),
         ChainNode(id="n2", fn="importImages.remove", params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default"))],
        [ChainEdge("n1", "n2")],
    )
    save_chain_template!(proj, tpl)

    run = run_chain(proj, [i.uid for i in imgs]; chain="fault-chain",
                    on_log=_->nothing)

    for img in imgs
        @test run.image_states[img.uid]["n1"].status == :failed
        @test run.image_states[img.uid]["n2"].status == :skipped
    end

    rm(proj.root; recursive=true)
end

# ── Chain resume — load_chain_run round-trips state ──────────────────────
@testset "Chain resume — load_chain_run round-trip" begin
    proj = create_project!(name="chain-resume-rt-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

    tpl = ChainTemplate(
        "resume-rt-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        ChainEdge[],
    )
    save_chain_template!(proj, tpl)

    run = run_chain(proj, [i.uid for i in imgs];
                    chain="resume-rt-chain", on_log=_->nothing)

    # Verify states are :done with a params_hash stored
    for img in imgs
        @test run.image_states[img.uid]["n1"].status == :done
        @test !isnothing(run.image_states[img.uid]["n1"].params_hash)
    end

    # Load the run from disk and verify state is preserved
    loaded = load_chain_run(proj, run.id)
    @test loaded.id == run.id
    @test loaded.chain_name == "resume-rt-chain"
    @test length(loaded.image_uids) == 2
    for img in imgs
        @test loaded.image_states[img.uid]["n1"].status == :done
        @test loaded.image_states[img.uid]["n1"].params_hash == run.image_states[img.uid]["n1"].params_hash
    end

    rm(proj.root; recursive=true)
end

# ── Chain resume — already-done nodes are skipped ────────────────────────
@testset "Chain resume — skip unchanged done nodes" begin
    proj = create_project!(name="chain-resume-skip-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

    tpl = ChainTemplate(
        "skip-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        ChainEdge[],
    )
    save_chain_template!(proj, tpl)

    # First run — collects logs
    logs1 = String[]
    run1 = run_chain(proj, [i.uid for i in imgs];
                     chain="skip-chain", on_log=line->push!(logs1, line))
    @test !isempty(logs1)

    # Second run via run_id — same params, no work to do
    logs2 = String[]
    run2 = run_chain(proj, String[];
                     run_id=run1.id, on_log=line->push!(logs2, line))

    # No new log lines because all nodes were skipped
    @test isempty(logs2)
    # States still :done
    for img in imgs
        @test run2.image_states[img.uid]["n1"].status == :done
    end

    rm(proj.root; recursive=true)
end

# ── Chain resume — params change triggers re-run ──────────────────────────
@testset "Chain resume — params change re-runs node" begin
    proj = create_project!(name="chain-resume-rerun-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a",)) do nm; add_image!(s; name=nm) end

    tpl = ChainTemplate(
        "rerun-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image",
                   params=Dict{String,Any}("message" => "first"))],
        ChainEdge[],
    )
    save_chain_template!(proj, tpl)

    run1 = run_chain(proj, [i.uid for i in imgs]; chain="rerun-chain", on_log=_->nothing)
    @test run1.image_states[imgs[1].uid]["n1"].result["image"] == imgs[1].name

    # Resume with overridden message param — node must re-run
    logs2 = String[]
    run2 = run_chain(proj, String[];
                     run_id=run1.id,
                     overrides=Dict{String,Any}("n1" => Dict{String,Any}("message" => "second")),
                     on_log=line->push!(logs2, line))

    @test !isempty(logs2)   # node re-ran and produced logs
    @test run2.image_states[imgs[1].uid]["n1"].status == :done

    rm(proj.root; recursive=true)
end

# ── Step 5: Resume from mid-chain failure — only failed/downstream nodes rerun ─
@testset "Resume — failure at node 3 does not redo nodes 1-2" begin
    proj = create_project!(name="resume-fail-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a",)) do nm; add_image!(s; name=nm) end

    # n1 → n2 → n3(bad) → n4(skipped)
    tpl = ChainTemplate(
        "fail-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n3", fn="nonexistent.task",   scope="image", params=Dict{String,Any}()),
         ChainNode(id="n4", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        [ChainEdge("n1","n2"), ChainEdge("n2","n3"), ChainEdge("n3","n4")],
    )
    save_chain_template!(proj, tpl)

    logs1 = String[]
    run1 = run_chain(proj, [i.uid for i in imgs]; chain="fail-chain",
                     on_log=line->push!(logs1, line))

    uid = imgs[1].uid
    @test run1.image_states[uid]["n1"].status == :done
    @test run1.image_states[uid]["n2"].status == :done
    @test run1.image_states[uid]["n3"].status == :failed
    @test run1.image_states[uid]["n4"].status == :skipped

    # Resume — n1/n2 are :done with unchanged params → must be skipped
    logs2 = String[]
    run2 = run_chain(proj, String[]; run_id=run1.id,
                     on_log=line->push!(logs2, line))

    # n1 and n2 produced no new log lines — they were skipped
    n1_logs_run1 = count(l -> contains(l, uid*"/n1"), logs1)
    n2_logs_run1 = count(l -> contains(l, uid*"/n2"), logs1)
    n1_logs_run2 = count(l -> contains(l, uid*"/n1"), logs2)
    n2_logs_run2 = count(l -> contains(l, uid*"/n2"), logs2)
    @test n1_logs_run1 > 0    # ran in first pass
    @test n2_logs_run1 > 0    # ran in first pass
    @test n1_logs_run2 == 0   # skipped on resume
    @test n2_logs_run2 == 0   # skipped on resume
    # n3 still fails (fn still missing), n4 still skipped
    @test run2.image_states[uid]["n1"].status == :done
    @test run2.image_states[uid]["n2"].status == :done
    @test run2.image_states[uid]["n3"].status == :failed
    @test run2.image_states[uid]["n4"].status == :skipped

    rm(proj.root; recursive=true)
end

# ── Step 5: Params change on node 4 — only n4 and downstream rerun ──────
@testset "Resume — params change on n4 reruns only n4 and downstream" begin
    proj = create_project!(name="resume-p4-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a",)) do nm; add_image!(s; name=nm) end

    tpl = ChainTemplate(
        "p4-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n3", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n4", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n5", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        [ChainEdge("n1","n2"), ChainEdge("n2","n3"), ChainEdge("n3","n4"), ChainEdge("n4","n5")],
    )
    save_chain_template!(proj, tpl)

    logs1 = String[]
    run1 = run_chain(proj, [i.uid for i in imgs]; chain="p4-chain",
                     on_log=line->push!(logs1, line))
    uid = imgs[1].uid
    for n in ("n1","n2","n3","n4","n5")
        @test run1.image_states[uid][n].status == :done
    end

    # Resume with n4 params changed via override → n1,n2,n3 skip; n4,n5 rerun
    logs2 = String[]
    run2 = run_chain(proj, String[]; run_id=run1.id,
                     overrides=Dict{String,Any}("n4" => Dict{String,Any}("message" => "changed")),
                     on_log=line->push!(logs2, line))

    for n in ("n1","n2","n3") # not re-run
        @test count(l -> contains(l, uid*"/$n"), logs2) == 0
    end
    for n in ("n4","n5") # re-run
        @test count(l -> contains(l, uid*"/$n"), logs2) > 0
    end
    for n in ("n1","n2","n3","n4","n5")
        @test run2.image_states[uid][n].status == :done
    end

    rm(proj.root; recursive=true)
end

# ── Step 5: Picnic node restarts when per-image upstream input changes ───
@testset "Resume — picnic node restarts when upstream stale" begin
    proj = create_project!(name="resume-picnic-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

    # n1(image) → n2(set) → n3(image)
    tpl = ChainTemplate(
        "picnic-resume-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.set_task",   scope="set",   params=Dict{String,Any}()),
         ChainNode(id="n3", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        [ChainEdge("n1","n2"), ChainEdge("n2","n3")],
    )
    save_chain_template!(proj, tpl)

    logs1 = String[]
    run1 = run_chain(proj, [i.uid for i in imgs]; chain="picnic-resume-chain",
                     on_log=line->push!(logs1, line))
    for img in imgs
        @test run1.image_states[img.uid]["n1"].status == :done
        @test run1.image_states[img.uid]["n2"].status == :done
        @test run1.image_states[img.uid]["n3"].status == :done
    end

    # setTask log appeared once in run 1
    set_logs1 = filter(l -> contains(l, "setTask ran"), logs1)
    @test length(set_logs1) == 1

    # Resume with n1 params changed → n1 stale → n2 (picnic) stale → n3 stale
    logs2 = String[]
    run2 = run_chain(proj, String[]; run_id=run1.id,
                     overrides=Dict{String,Any}("n1" => Dict{String,Any}("message" => "new")),
                     on_log=line->push!(logs2, line))

    # Picnic re-ran (set log appeared again)
    set_logs2 = filter(l -> contains(l, "setTask ran"), logs2)
    @test length(set_logs2) == 1   # ran exactly once in this resume run
    # All nodes redone
    for img in imgs
        @test run2.image_states[img.uid]["n1"].status == :done
        @test run2.image_states[img.uid]["n2"].status == :done
        @test run2.image_states[img.uid]["n3"].status == :done
    end

    rm(proj.root; recursive=true)
end

# ── Step 6: Incremental plot node fires as images complete ───────────────
@testset "Incremental plot node — fires and sets :done state" begin
    proj = create_project!(name="incr-plot-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b", "img-c")) do nm; add_image!(s; name=nm) end

    # n1(image) → n2(incremental plot)
    # debounce_ms=10 so it fires quickly in the test
    tpl = ChainTemplate(
        "incr-chain",
        [ChainNode(id="n1", fn="testTasks.image_task",          scope="image",
                   params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.incremental_plot_task", scope="incremental",
                   params=Dict{String,Any}("debounce_ms" => 10))],
        [ChainEdge("n1", "n2")],
    )
    save_chain_template!(proj, tpl)

    logs = String[]
    run = run_chain(proj, [i.uid for i in imgs];
                    chain="incr-chain", on_log=line->push!(logs, line))

    # All per-image n1 nodes succeeded
    for img in imgs
        @test run.image_states[img.uid]["n1"].status == :done
    end

    # Incremental plot ran — all images' n2 state is :done
    for img in imgs
        @test run.image_states[img.uid]["n2"].status == :done
    end

    # Plot log appeared at least once
    plot_logs = filter(l -> contains(l, "incr/n2"), logs)
    @test !isempty(plot_logs)

    rm(proj.root; recursive=true)
end

# ── Step 6: Incremental node does not block per-image progression ────────
@testset "Incremental plot node — image threads not blocked" begin
    proj = create_project!(name="incr-nob-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

    # n1(image) → n2(incremental) → n3(image) — n3 should still run
    # (incremental nodes don't gate downstream per-image nodes)
    tpl = ChainTemplate(
        "incr-pass-chain",
        [ChainNode(id="n1", fn="testTasks.image_task",          scope="image",
                   params=Dict{String,Any}()),
         ChainNode(id="n3", fn="testTasks.image_task",          scope="image",
                   params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.incremental_plot_task", scope="incremental",
                   params=Dict{String,Any}("debounce_ms" => 10))],
        [ChainEdge("n1", "n2"), ChainEdge("n1", "n3")],
    )
    save_chain_template!(proj, tpl)

    run = run_chain(proj, [i.uid for i in imgs];
                    chain="incr-pass-chain", on_log=_->nothing)

    for img in imgs
        @test run.image_states[img.uid]["n1"].status == :done
        @test run.image_states[img.uid]["n3"].status == :done
        @test run.image_states[img.uid]["n2"].status == :done
    end

    rm(proj.root; recursive=true)
end

# ── Step 6: Event bus subscribe/unsubscribe ───────────────────────────────
@testset "Event bus — subscribe and receive node:done events" begin
    proj = create_project!(name="evbus-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

    tpl = ChainTemplate(
        "evbus-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image",
                   params=Dict{String,Any}())],
        ChainEdge[],
    )
    save_chain_template!(proj, tpl)

    received = String[]
    payloads = Any[]
    handler  = payload -> (push!(received, payload.image_uid); push!(payloads, payload))
    subscribe_chain_events!("node:done", handler)

    run = run_chain(proj, [i.uid for i in imgs];
                    chain="evbus-chain", on_log=_->nothing)

    unsubscribe_chain_events!("node:done", handler)

    # Both images fired node:done events
    @test Set(received) ⊇ Set(i.uid for i in imgs)

    # …and every payload carries the scheduler `task_id` the node ran as, matching the state it
    # was recorded under. This is the correlation handle the task console needs: a chain run emits
    # no `task:status` frames, so without it a finished node can only be reported as "outcome
    # unseen". Never `nothing` — absent ⇒ "" (see _update_node_state!).
    for p in payloads
        @test haskey(p, :task_id)
        @test p.task_id isa String
        @test p.task_id == run.image_states[p.image_uid][p.node_id].task_id
        @test !isempty(p.task_id)                     # this node ran, so it has one
    end

    # After unsubscribe, new events don't reach the handler
    n_before = length(received)
    run2 = run_chain(proj, [imgs[1].uid]; chain="evbus-chain", on_log=_->nothing)
    @test length(received) == n_before  # unchanged

    rm(proj.root; recursive=true)
end

# ── Step 7: Resource pool — concurrency limit respected ──────────────────
# Pool limit = 1 on n1. Three images each sleep 40ms in n1. With one worker the
# node executions serialise, so total wall time ≥ 3×40ms (parallel would be ~40ms).
# (Wall-clock, not event counting: node:running now fires from the pool worker and
# node:done from the image thread, so a size-1 pool has a benign running/done
# handoff overlap — execution is still serial, which the timing assertion proves.)
@testset "Resource pool — concurrency limit respected" begin
    proj = create_project!(name="pool-limit-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b", "img-c")) do nm; add_image!(s; name=nm) end

    tpl = ChainTemplate(
        "pool-limit-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image",
                   params=Dict{String,Any}("waitMs" => 40),
                   resource_pool="slow_pool"),
         ChainNode(id="n2", fn="testTasks.image_task", scope="image",
                   params=Dict{String,Any}())],
        [ChainEdge("n1","n2")],
    )
    save_chain_template!(proj, tpl)

    # Pools are global (scheduler.jl _POOLS); register the test pool at limit 1.
    resize_pool!("slow_pool", 1)
    t0  = time()
    run = run_chain(proj, [i.uid for i in imgs];
                    chain="pool-limit-chain",
                    on_log=_->nothing)
    elapsed = time() - t0

    # Serialised: ≥ 3×40ms of n1 work. Parallel would finish in ~40-60ms.
    @test elapsed >= 0.10
    for img in imgs
        @test run.image_states[img.uid]["n1"].status == :done
        @test run.image_states[img.uid]["n2"].status == :done
    end

    rm(proj.root; recursive=true)
end

# ── Step 7: Resource pool — higher limit allows parallel execution ────────
@testset "Resource pool — limit=3 allows all concurrent" begin
    proj = create_project!(name="pool-par-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b", "img-c")) do nm; add_image!(s; name=nm) end

    tpl = ChainTemplate(
        "pool-par-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image",
                   params=Dict{String,Any}("waitMs" => 40),
                   resource_pool="par_pool")],
        ChainEdge[],
    )
    save_chain_template!(proj, tpl)

    max_concurrent = Threads.Atomic{Int}(0)
    current        = Threads.Atomic{Int}(0)

    sh = payload -> begin
        payload.node_id == "n1" || return
        n = Threads.atomic_add!(current, 1) + 1
        Threads.atomic_max!(max_concurrent, n)
        nothing
    end
    dh = payload -> (payload.node_id == "n1" && Threads.atomic_sub!(current, 1); nothing)

    subscribe_chain_events!("node:running", sh)
    subscribe_chain_events!("node:done",    dh)

    resize_pool!("par_pool", 3)
    run = run_chain(proj, [i.uid for i in imgs];
                    chain="pool-par-chain",
                    on_log=_->nothing)

    unsubscribe_chain_events!("node:running", sh)
    unsubscribe_chain_events!("node:done",    dh)

    # With limit 3 and 3 images all able to run simultaneously, max should be 3
    @test max_concurrent[] == 3

    rm(proj.root; recursive=true)
end

# ── Dynamic resize: throttle UP re-parallelises the already-queued backlog ─────
# One persistent queue per pool (not a swap): grow spawns workers that pick up the backlog.
# Start at limit 1 (1 running, 3 queued), throttle to 4 mid-run → the 3 queued fan out, so the
# observed concurrency rises above 1. A queue-swap (the old bug) would leave them serial at 1.
@testset "Pool throttle-up parallelises the backlog" begin
    proj = create_project!(name="pool-grow-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("a", "b", "c", "d")) do nm; add_image!(s; name=nm) end
    tpl = ChainTemplate("pool-grow-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image",
                   params=Dict{String,Any}("waitMs" => 300), resource_pool="dyngrow")],
        ChainEdge[])
    save_chain_template!(proj, tpl)

    max_concurrent = Threads.Atomic{Int}(0)
    current        = Threads.Atomic{Int}(0)
    sh = payload -> begin
        payload.node_id == "n1" || return
        n = Threads.atomic_add!(current, 1) + 1
        Threads.atomic_max!(max_concurrent, n); nothing
    end
    dh = payload -> (payload.node_id == "n1" && Threads.atomic_sub!(current, 1); nothing)
    subscribe_chain_events!("node:running", sh)
    subscribe_chain_events!("node:done",    dh)

    resize_pool!("dyngrow", 1)                              # throttled to 1
    runner = Threads.@spawn run_chain(proj, [i.uid for i in imgs];
                                      chain="pool-grow-chain", on_log=_->nothing)
    sleep(0.12)                                             # 1 running, 3 queued
    resize_pool!("dyngrow", 4)                              # throttle up mid-run
    wait(runner)

    unsubscribe_chain_events!("node:running", sh)
    unsubscribe_chain_events!("node:done",    dh)

    @test max_concurrent[] >= 3                             # backlog fanned out (was serial at 1)
    rm(proj.root; recursive=true)
end

# ── Dynamic resize: throttle DOWN settles to the new limit (never oversubscribes) ──
# Start wide (4), throttle to 1 just after the first batch is admitted. The first 4 run
# concurrently (~150ms), then the remaining 4 serialise at limit 1 (~4×150ms) → ≳0.6s total.
# If the shrink hadn't taken (stayed at 4), all 8 would finish in ~2×150 = 300ms. Wall-clock
# (per the existing pool tests) — the slot gate is checked at execution time, so the tail
# serialises even though the first 4 were admitted while the limit was still 4.
@testset "Pool throttle-down settles to the new limit" begin
    proj = create_project!(name="pool-shrink-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(i -> add_image!(s; name="img-$i"), 1:8)
    tpl = ChainTemplate("pool-shrink-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image",
                   params=Dict{String,Any}("waitMs" => 150), resource_pool="dynshrink")],
        ChainEdge[])
    save_chain_template!(proj, tpl)

    resize_pool!("dynshrink", 4)
    t0 = time()
    runner = Threads.@spawn run_chain(proj, [i.uid for i in imgs];
                                      chain="pool-shrink-chain", on_log=_->nothing)
    sleep(0.05)                                             # first 4 admitted
    resize_pool!("dynshrink", 1)                            # throttle down mid-run
    wait(runner)
    elapsed = time() - t0

    @test elapsed >= 0.55                                   # tail serialised at 1 (not stuck at 4)
    rm(proj.root; recursive=true)
end

# ── Step 7: Pipelining — image A reaches n2 before image B finishes n1 ───
# Pool limit=1 on n1 (each image sleeps 80ms there). Image A exits n1 first
# and immediately enters n2 (instant). B and C are still queuing for n1.
# Verify: A's n2 completion timestamp < B's n1 completion timestamp.
@testset "Pipelining — n2 of first image before n1 of second image" begin
    proj = create_project!(name="pipeline-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b", "img-c")) do nm; add_image!(s; name=nm) end

    tpl = ChainTemplate(
        "pipeline-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image",
                   params=Dict{String,Any}("waitMs" => 80),
                   resource_pool="serial_pool"),
         ChainNode(id="n2", fn="testTasks.image_task", scope="image",
                   params=Dict{String,Any}("waitMs" => 0))],
        [ChainEdge("n1","n2")],
    )
    save_chain_template!(proj, tpl)

    done_times = Dict{String,Float64}()  # "uid/nid" => time()
    th = payload -> (done_times["$(payload.image_uid)/$(payload.node_id)"] = time(); nothing)
    subscribe_chain_events!("node:done", th)

    resize_pool!("serial_pool", 1)
    run = run_chain(proj, [i.uid for i in imgs];
                    chain="pipeline-chain",
                    on_log=_->nothing)

    unsubscribe_chain_events!("node:done", th)

    # Find the image that finished n1 first (earliest n1 completion)
    first_uid  = argmin(uid -> done_times["$(uid)/n1"], [i.uid for i in imgs])
    other_uids = filter(i -> i.uid != first_uid, imgs)

    # The first image's n2 must have finished before any other image's n1 did
    t_first_n2 = done_times["$(first_uid)/n2"]
    for other in other_uids
        @test t_first_n2 < done_times["$(other.uid)/n1"]
    end

    rm(proj.root; recursive=true)
end

# ── Step 7: Cross-image fault isolation ──────────────────────────────────
# Image A fails at n1. Images B and C proceed through n1→n2 unaffected.
# (Different from Step 5's test which checks downstream skips on the SAME image.)
#
# We make n1 = RemoveImage, which requires a registered zarr to succeed.
# img_b and img_c have a real zarr; img_a does not → img_a fails at n1.
# n2 = testTasks.image_task (always succeeds).
# Expected: img_a fails n1, skips n2. img_b and img_c succeed both nodes.
@testset "Cross-image fault isolation" begin
    proj = create_project!(name="xiso-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img_a = add_image!(s; name="img-a")   # no zarr → RemoveImage will fail
    img_b = add_image!(s; name="img-b")
    img_c = add_image!(s; name="img-c")

    for img in (img_b, img_c)
        zarr = joinpath(dirname(dirname(img._dir)), "0", img.uid, "ccidImage.ome.zarr")
        mkpath(zarr)
        img.filepath["default"] = "ccidImage.ome.zarr"
        img.filepath["_active"] = "default"
        img.status = "done"
        save!(img)
    end

    tpl = ChainTemplate(
        "xiso-chain",
        [ChainNode(id="n1", fn="importImages.remove", scope="image",
                   params=Dict{String,Any}("valueName"=>"default","newDefault"=>"default")),
         ChainNode(id="n2", fn="testTasks.image_task", scope="image",
                   params=Dict{String,Any}())],
        [ChainEdge("n1","n2")],
    )
    save_chain_template!(proj, tpl)

    run = run_chain(proj, [img_a.uid, img_b.uid, img_c.uid];
                    chain="xiso-chain", on_log=_->nothing)

    # img_a: n1 failed (no zarr), n2 skipped
    @test run.image_states[img_a.uid]["n1"].status == :failed
    @test run.image_states[img_a.uid]["n2"].status == :skipped

    # img_b and img_c: both nodes succeeded — not affected by img_a's failure
    @test run.image_states[img_b.uid]["n1"].status == :done
    @test run.image_states[img_b.uid]["n2"].status == :done
    @test run.image_states[img_c.uid]["n1"].status == :done
    @test run.image_states[img_c.uid]["n2"].status == :done

    rm(proj.root; recursive=true)
end

# ── Step 7: run_chain headless — no api/ loaded ───────────────────────────
# All tests in this file run without `using` api/. This testset makes the
# contract explicit: run_chain on a picnic chain produces correct results
# with nothing but `using Cecelia`.
@testset "run_chain headless (no api/ dependency)" begin
    proj = create_project!(name="headless-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    imgs = map(("img-a", "img-b")) do nm; add_image!(s; name=nm) end

    tpl = ChainTemplate(
        "headless-chain",
        [ChainNode(id="n1", fn="testTasks.image_task", scope="image", params=Dict{String,Any}()),
         ChainNode(id="n2", fn="testTasks.set_task",   scope="set",   params=Dict{String,Any}()),
         ChainNode(id="n3", fn="testTasks.image_task", scope="image", params=Dict{String,Any}())],
        [ChainEdge("n1","n2"), ChainEdge("n2","n3")],
    )
    save_chain_template!(proj, tpl)

    run = run_chain(proj, [i.uid for i in imgs]; chain="headless-chain", on_log=_->nothing)

    for img in imgs
        @test run.image_states[img.uid]["n1"].status == :done
        @test run.image_states[img.uid]["n2"].status == :done
        @test run.image_states[img.uid]["n3"].status == :done
    end
    @test run.image_states[imgs[1].uid]["n2"].result["image_count"] == 2

    rm(proj.root; recursive=true)
end

# ── fun_name dispatch ─────────────────────────────────────────────────────
@testset "fun_name dispatch" begin
    @test _task_from_fun_name("importImages.omezarr") isa ImportOmezarr
    @test _task_from_fun_name("importImages.remove")  isa RemoveImage
    @test _task_from_fun_name("cleanupImages.cellposeCorrect") isa CellposeCorrect
    @test _task_from_fun_name("cleanupImages.afCorrect")       isa AfCorrect
    @test _task_from_fun_name("cleanupImages.driftCorrect")    isa DriftCorrect
    @test _task_from_fun_name("cleanupImages.smooth")          isa Smooth
    @test _task_from_fun_name("segment.cellpose")              isa CellposeSegment
    @test _task_from_fun_name("segment.measureLabels")         isa MeasureLabels
    composite = _task_from_fun_name("cleanupImages.afDriftCorrect")
    @test composite isa CompositeTask
    @test composite.fun_name == "cleanupImages.afDriftCorrect"
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

# ── BayesianTracking param validation ─────────────────────────────────────
@testset "BayesianTracking params" begin
    task = BayesianTracking()
    good = Dict{String,Any}(
        "valueName" => "default", "popsToTrack" => "NONE",
        "maxSearchRadius" => 20, "maxLost" => 3, "trackBranching" => false,
        "minTimepoints" => 5, "accuracy" => 0.8, "probToAssign" => 0.8,
        # advanced section params are flattened by the frontend before submit
        "noiseInital" => 300, "distThresh" => 10.0, "segmentationMissRate" => 0.1,
    )
    @test begin validate_params(task, good); true end
    # maxSearchRadius max is 200
    @test_throws ParamValidationError validate_params(
        task, merge(good, Dict{String,Any}("maxSearchRadius" => 500)))
    # segmentationMissRate min is 0.001
    @test_throws ParamValidationError validate_params(
        task, merge(good, Dict{String,Any}("segmentationMissRate" => 0.0)))
end

# ── TrackMeasures param validation ────────────────────────────────────────
@testset "TrackMeasures params" begin
    task = TrackMeasures()
    @test begin validate_params(task,
        Dict{String,Any}("valueName" => "B", "forceRecompute" => false)); true end
    # forceRecompute is bool — a non-bool must be rejected. Also a guard that params use
    # "key" (not "id"): with "id" the spec key resolves empty and validation silently skips.
    @test_throws ParamValidationError validate_params(
        task, Dict{String,Any}("valueName" => "B", "forceRecompute" => "yes"))
end

# ── Labels field round-trip ───────────────────────────────────────────────
# Regression guard: the `labels` Dict written by cellposeSegment must survive
# save!/init_object and land at the agreed location in ccid.json.
@testset "Labels field round-trip" begin
    proj = create_project!(name="labels-rt-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")

    img.labels["default"] = ["default.zarr", "default_nuc.zarr"]
    save!(img)

    # Reloaded value is correct in memory
    r = init_object(proj.uid, img.uid)
    @test r isa CciaImage
    @test haskey(r.labels, "default")
    @test r.labels["default"] == ["default.zarr", "default_nuc.zarr"]

    # On-disk shape: top-level "labels" dict with string-vector values
    raw = JSON3.read(read(joinpath(img._dir, "ccid.json"), String), Dict{String,Any})
    @test haskey(raw, "labels")
    label_val = collect(String, raw["labels"]["default"])
    @test label_val == ["default.zarr", "default_nuc.zarr"]

    rm(proj.root; recursive=true)
end

# ── CompositeTask — spec loads and composite array is correct ─────────────
@testset "CompositeTask spec" begin
    task = CompositeTask("cleanupImages.afDriftCorrect")
    spec = Cecelia._task_spec(task)
    @test !isnothing(spec)
    @test haskey(spec, "composite")
    steps = [string(s) for s in spec["composite"]]
    @test steps == ["cleanupImages.afCorrect", "cleanupImages.driftCorrect"]
    @test get(spec, "fun_name", "") == "cleanupImages.afDriftCorrect"
end

# ── $include fragment resolution ──────────────────────────────────────────
# Verifies that {"$include": "imageTiling"} in cellpose.json is expanded
# to the 4 shared tiling params (blockSize, overlap, blockSizeZ, overlapZ).
@testset "\$include fragment resolution" begin
    task = CellposeSegment()
    spec = Cecelia._task_spec(task)
    @test !isnothing(spec)
    # Find the imageTiling section
    tiling_sec = nothing
    for p in spec["params"]
        p isa AbstractDict && string(get(p, "key", "")) == "imageTiling" &&
            (tiling_sec = p; break)
    end
    @test !isnothing(tiling_sec)
    tiling_params = tiling_sec["params"]
    keys_in_tiling = [string(get(p, "key", "")) for p in tiling_params if p isa AbstractDict]
    # Fragment contributes these 4; cellpose.json adds labelOverlap
    @test "blockSize"  ∈ keys_in_tiling
    @test "overlap"    ∈ keys_in_tiling
    @test "blockSizeZ" ∈ keys_in_tiling
    @test "overlapZ"   ∈ keys_in_tiling
    @test "labelOverlap" ∈ keys_in_tiling
    @test length(keys_in_tiling) == 5   # 4 from fragment + 1 inline
    # No raw $include entries should survive
    @test !any(p isa AbstractDict && haskey(p, "\$include") for p in tiling_params)
end

# ── label_props field round-trip ──────────────────────────────────────────
@testset "label_props field round-trip" begin
    proj = create_project!(name="lp-rt-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="img")

    img.label_props["default"] = "default.h5ad"
    save!(img)

    r = init_object(proj.uid, img.uid)
    @test r isa CciaImage
    @test get(r.label_props, "default", nothing) == "default.h5ad"

    raw = JSON3.read(read(joinpath(img._dir, "ccid.json"), String), Dict{String,Any})
    @test haskey(raw, "label_props")
    @test string(raw["label_props"]["default"]) == "default.h5ad"

    rm(proj.root; recursive=true)
end

# ── Param validation — AfCorrect (group with flat sub-params) ─────────────

# ── Param validation — DriftCorrect ───────────────────────────────────────

# ── Versioned helpers ─────────────────────────────────────────────────────
@testset "Versioned dict helpers" begin
    d = Dict{String,Any}()
    versioned_set_field!(d, "filepath", "ccidImage.ome.zarr")
    @test versioned_get_field(d, "filepath") == "ccidImage.ome.zarr"
    @test versioned_active(d["filepath"]) == "default"

    versioned_set_field!(d, "filepath", "ccidCpCorrected.ome.zarr", "cpCorrected")
    @test versioned_get_field(d, "filepath", "cpCorrected") == "ccidCpCorrected.ome.zarr"
    @test versioned_get_field(d, "filepath") == "ccidCpCorrected.ome.zarr"  # active = cpCorrected

    versioned_set_field!(d, "filepath", nothing, "cpCorrected")
    @test isnothing(get(d["filepath"], "cpCorrected", nothing))
end

# ── LabelProps reader (H5AD via HDF5.jl) ──────────────────────────────────
@testset "LabelProps reader" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "LabelProps reader (fixture missing)"
    else
        # metadata (cheap reads)
        @test length(col_names(label_props(h5); data_type=:vars)) == 27
        @test channel_columns(label_props(h5)) ==
              ["mean_intensity_0", "mean_intensity_1", "mean_intensity_2", "mean_intensity_3"]
        @test centroid_columns(label_props(h5)) == ["centroid_z", "centroid_y", "centroid_x"]
        @test temporal_columns(label_props(h5)) == ["centroid_t"]
        # order= selects BY AXIS (present only), never positionally
        @test centroid_columns(label_props(h5); order=[:x, :y, :z]) == ["centroid_x", "centroid_y", "centroid_z"]
        @test centroid_columns(label_props(h5); order=[:x, :y]) == ["centroid_x", "centroid_y"]

        # channel-name selection: request an intensity column by its CHANNEL name and the reader
        # resolves it to the raw {measure}_intensity_{i} column, returning it under the channel name.
        # This is what lets pop_df(...; pop_cols=["<channel>"]) work. channel_names is positional:
        # index i ↔ chans[i+1], so "chC" == mean_intensity_2.
        let lpc = label_props(h5; channel_names=["chA", "chB", "chC", "chD"])
            d = lpc |> select_cols(["chC"]) |> as_df
            @test "chC" in names(d)                                   # returned under the requested name
            @test !("mean_intensity_2" in names(d))                   # not the raw name
            raw = label_props(h5) |> select_cols(["mean_intensity_2"]) |> as_df
            @test d.chC == raw.mean_intensity_2                       # same underlying column
        end
        # raw names still resolve (gates/clustering pass raw) — unchanged behaviour
        @test names(label_props(h5; channel_names=["chA","chB","chC","chD"]) |> select_cols(["mean_intensity_2"]) |> as_df) == ["label", "mean_intensity_2"]
        # a genuinely unknown name is still ignored (not resolved to anything)
        @test names(label_props(h5; channel_names=["chA","chB","chC","chD"]) |> select_cols(["nope"]) |> as_df) == ["label"]

        # full frame: label + 27 vars + 3 spatial + 1 temporal + 8 obs (track lineage +
        # live.cell.* from tracking.track_measures) = 40 cols, 1377 rows
        df = label_props(h5) |> as_df
        @test size(df) == (1377, 40)
        # n_obs is the cheap dims-only count — must agree with the materialised row count
        @test n_obs(label_props(h5)) == 1377
        @test "label" in names(df)
        @test eltype(df.label) == Int64
        @test df.label[1:5] == [0, 1, 2, 3, 4]

        # X orientation correctness (audited values).
        # NOTE: intentional coupling to the committed fixture state of KDIeEm/B.h5ad — these
        # are the actual bbox values in that file, asserting /X is read with correct row/col
        # orientation (not transposed). If this breaks, it's either (a) the reader regressed,
        # or (b) the fixture was deliberately regenerated (e.g. segmentation rerun) — in which
        # case re-audit and update these constants. A failure here is NOT "the test is wrong".
        @test [df[1, "bbox-$j"] for j in 0:4] == Float32[0, 0, 71, 2, 29]
        @test [df[2, "bbox-$j"] for j in 0:4] == Float32[0, 7, 368, 4, 38]

        # is_tracked's signal: a tracked segmentation carries a track_id obs column (KDIeEm/B is
        # tracked). track_props / the track-grained gating plots key off this to say "track first"
        # (empty) instead of erroring when it's absent.
        obs = col_names(label_props(h5); data_type=:obs)
        @test "track_id" in obs
        @test !("not_a_column" in obs)

        # lazy column selection — only requested columns (+ label) are returned
        @test names(label_props(h5) |> select_cols(["area"]) |> as_df) == ["label", "area"]

        # centroid + intensity selection
        @test Set(names(label_props(h5) |> select_cols(["mean_intensity_0"]) |> view_centroid_cols |> as_df)) ==
              Set(["label", "mean_intensity_0", "centroid_z", "centroid_y", "centroid_x", "centroid_t"])

        # row filter by label
        d4 = label_props(h5) |> filter_rows([0, 1, 2]; by=:label) |> as_df
        @test sort(d4.label) == [0, 1, 2]

        # filter is intersection: nonexistent IDs are silently skipped (≤ requested, no NaN/error)
        d4b = label_props(h5) |> filter_rows([0, 1, 999_999]; by=:label) |> as_df
        @test sort(d4b.label) == [0, 1]

        # sort by area, descending
        d5 = label_props(h5) |> select_cols(["area"]) |> sort_by("area"; rev=true) |> as_df
        @test d5.area[1] == maximum(d5.area)
    end
end

# ── LabelProps writer (add_obs / save! — the chain write path) ─────────────
@testset "LabelProps writer" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "LabelProps writer (fixture missing)"
    else
        tmp = joinpath(mktempdir(), "B.h5ad")
        cp(h5, tmp)

        existing = label_props(tmp) |> as_df
        some = existing.label[1:5]                       # write to a subset of labels
        df = DataFrame("label" => some,
                       "test.measure" => Float64.(1:5),
                       "test.other"   => [10.0, 20.0, 30.0, 40.0, 50.0])
        # the documented chain write idiom
        label_props(tmp) |> add_obs(df) |> save!

        # new columns appear in obs column-order
        obs_cols = col_names(label_props(tmp); data_type=:obs)
        @test "test.measure" in obs_cols
        @test "test.other" in obs_cols

        # read back via the reader, aligned by label; unset labels are NaN
        back = label_props(tmp) |> select_cols(["test.measure", "test.other"]) |> as_df
        byrow = Dict(l => i for (i, l) in enumerate(back.label))
        for (k, lab) in enumerate(some)
            @test back[byrow[lab], "test.measure"] == Float64(k)
        end
        # a label not in df → NaN
        other = first(setdiff(back.label, some))
        @test isnan(back[byrow[other], "test.measure"])

        # original data preserved (var count unchanged — obs append only, no X rewrite)
        @test length(col_names(label_props(tmp); data_type=:vars)) ==
              length(col_names(label_props(h5);  data_type=:vars))

        # idempotent overwrite: re-writing the same column updates, doesn't duplicate
        df2 = DataFrame("label" => some, "test.measure" => fill(99.0, 5))
        label_props(tmp) |> add_obs(df2) |> save!
        @test count(==("test.measure"), col_names(label_props(tmp); data_type=:obs)) == 1
        back2 = label_props(tmp) |> select_cols(["test.measure"]) |> as_df
        @test back2[Dict(l => i for (i, l) in enumerate(back2.label))[some[1]], "test.measure"] == 99.0

        # drop_obs: remove a column; gone from column-order and from as_df
        label_props(tmp) |> drop_obs(["test.measure"]) |> save!
        @test "test.measure" ∉ col_names(label_props(tmp); data_type=:obs)
        @test "test.other"   ∈ col_names(label_props(tmp); data_type=:obs)   # sibling untouched
        @test "test.measure" ∉ names(label_props(tmp) |> as_df)
        # dropping a nonexistent column is a no-op (idempotent)
        @test begin label_props(tmp) |> drop_obs(["never.existed"]) |> save!; true end

        # combined drop + add in one chain (invalidate-then-write, e.g. btrack rerun)
        df3 = DataFrame("label" => some, "test.fresh" => Float64.(1:5))
        label_props(tmp) |> drop_obs(["test.other"]) |>
                            add_obs(df3) |> save!
        cols3 = col_names(label_props(tmp); data_type=:obs)
        @test "test.other" ∉ cols3
        @test "test.fresh" ∈ cols3

        # drop + re-add the SAME column in one chain → the add wins (column survives with new
        # values). Regression: the drop used to de-list and delete the just-written dataset, so
        # e.g. overwriting a categorical hmm.state with a numeric one in one chain lost it.
        df4 = DataFrame("label" => some, "test.fresh" => Float64.(101:105))
        label_props(tmp) |> drop_obs(["test.fresh"]) |>
                            add_obs(df4) |> save!
        @test "test.fresh" ∈ col_names(label_props(tmp); data_type=:obs)
        back4 = label_props(tmp) |> select_cols(["test.fresh"]) |> as_df
        row4  = Dict(l => i for (i, l) in enumerate(back4.label))
        for (k, lab) in enumerate(some)
            @test back4[row4[lab], "test.fresh"] == Float64(100 + k)
        end
    end
end

# ── Julia ↔ Python reader parity (the duplication safety net) ──────────────
# The Julia LabelProps reader and the Python LabelPropsView are two implementations of
# ONE spec (docs/DATAMODEL.md). They can drift — a new encoding type added to one and not
# the other. This runs BOTH against the same fixture and compares. Gated on the napari
# venv + anndata being importable, so headless CI without Python skips rather than fails.
@testset "LabelProps Julia/Python parity" begin
    h5    = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    pybin = python_bin_path()
    py_ok = !isempty(pybin) && isfile(pybin) &&
            success(setenv(`$pybin -c "import anndata, numpy, pandas"`, dir=@__DIR__))
    if !have_fixture(h5) || !py_ok
        @test_skip "LabelProps parity (fixture or python+anndata unavailable)"
    else
        # Python side dumps a comparable summary as JSON via the LabelPropsView reader.
        pyscript = """
            import sys, json
            import cecelia.utils.label_props_utils as lpu
            v = lpu.LabelPropsView(sys.argv[1])
            df  = v.view_cols(["mean_intensity_0"]).as_df().sort_values("label")
            cv  = lpu.LabelPropsView(sys.argv[1]).only_centroid_cols().as_df().sort_values("label")
            print(json.dumps({
                "var_names":      list(v.var_names()),
                "obs_cols":       list(v.adata.obs.columns),
                "centroid_cols":  list(v.centroid_columns()),
                "temporal_cols":  list(v.temporal_columns()),
                "n_obs":          int(len(v.labels())),
                "labels5":        [int(x) for x in df["label"].to_numpy()[:5]],
                "mean_int0_5":    [float(x) for x in df["mean_intensity_0"].to_numpy()[:5]],
                "centroid0_5":    [float(x) for x in cv["centroid_z"].to_numpy()[:5]],
            }))
            """
        # python/ on PYTHONPATH so `import cecelia.utils...` resolves (matches run_py's PYTHONPATH).
        py_dir = joinpath(dirname(dirname(@__DIR__)), "python")   # app/test → app → repo-root/python
        penv = copy(ENV); penv["PYTHONPATH"] = py_dir
        out = read(setenv(`$pybin -c $pyscript $h5`, penv; dir=py_dir), String)
        py = JSON3.read(out)

        lp = label_props(h5)
        @test Set(col_names(lp; data_type=:vars)) == Set(String.(py.var_names))
        @test Set(col_names(lp; data_type=:obs))  == Set(String.(py.obs_cols))
        @test Set(centroid_columns(lp))           == Set(String.(py.centroid_cols))
        @test Set(temporal_columns(lp))           == Set(String.(py.temporal_cols))

        # value parity on the same labels (sorted), one var col + one centroid col
        dj = label_props(h5) |> select_cols(["mean_intensity_0"]) |> sort_by("label") |> as_df
        @test dj.label[1:5] == collect(Int, py.labels5)
        @test dj[1:5, "mean_intensity_0"] ≈ Float64.(py.mean_int0_5)
        cj = label_props(h5) |> view_centroid_cols |> sort_by("label") |> as_df
        @test cj[1:5, "centroid_z"] ≈ Float64.(py.centroid0_5)
    end
end

# ── Track measures: numeric cross-check vs celltrackR ─────────────────────
# Golden values — provenance:
#   Generated from celltrackR 1.2.2 (Wortel et al. 2021, doi:10.1016/j.crmeth.2021.100006)
#   on the track below, via the R package's own functions (trackLength/speed/displacement/
#   straightness/asphericity/overallAngle/meanTurningAngle, degrees=TRUE; per-step via
#   subtracks()). celltrackR is the reference Cecelia ported these from; it is NOT a runtime
#   dependency — these constants pin the port to the original. If a measure here changes,
#   either the port regressed or it was deliberately changed (then re-derive from celltrackR).
@testset "Track measures (celltrackR golden)" begin
    Track = Cecelia.Track
    t  = [0.0, 10, 20, 30, 40]
    c3 = [0.0 0 0; 3 4 0; 7 4 2; 7 8 2; 10 12 5]    # (x,y,z) per position
    tr = Track(1, t, c3)

    @test Cecelia.track_length(tr)            ≈ 19.3030878498 atol=1e-6
    @test Cecelia.track_duration(tr)          == 40.0
    @test Cecelia.track_speed(tr)             ≈ 0.4825771962  atol=1e-6
    @test Cecelia.track_displacement(tr)      ≈ 16.4012194669 atol=1e-6
    @test Cecelia.max_displacement(tr)        ≈ 16.4012194669 atol=1e-6
    @test Cecelia.track_straightness(tr)      ≈ 0.8496681772  atol=1e-6
    @test Cecelia.track_displacement_ratio(tr) ≈ 1.0          atol=1e-6
    @test Cecelia.track_outreach_ratio(tr)    ≈ 0.8496681772  atol=1e-6
    @test Cecelia.track_asphericity(tr)       ≈ 0.8469835416  atol=1e-6
    @test Cecelia.track_overall_angle(tr)     ≈ 30.9637565321 atol=1e-6
    @test Cecelia.track_mean_turning_angle(tr) ≈ 64.7432782933 atol=1e-6

    # per-cell subtracks (celltrackR subtracks(·,1) speed; subtracks(·,2) overallAngle)
    ss = Cecelia.step_speeds(tr)              # cell_id 1 → NaN; i>1 = step speed to endpoint
    @test isnan(ss[1])
    @test ss[2:5] ≈ [0.5, 0.4472135955, 0.4, 0.5830951895] atol=1e-6
    sa = Cecelia.step_turning_angles(tr)      # cell_id 1,2 → NaN; i≥3 = turn angle (deg)
    @test all(isnan, sa[1:2])
    @test sa[3:5] ≈ [57.5436915381, 90.0, 46.6861433417] atol=1e-6

    # 2D path (drop z) — same functions, no call-site branching
    tr2 = Track(1, t, c3[:, 1:2])
    @test Cecelia.track_straightness(tr2)  ≈ 0.8678055195 atol=1e-6
    @test Cecelia.track_asphericity(tr2)   ≈ 0.8287305960 atol=1e-6
    @test Cecelia.track_overall_angle(tr2) ≈ 0.0          atol=1e-6  # first ∥ last step in xy

    # ── edge cases (Step 4 mandates these) ────────────────────────────────
    # single-step track (2 positions): measures needing ≥3 steps → NaN; no crash
    one = Track(1, [0.0, 10], [0.0 0 0; 3 4 0])
    @test Cecelia.track_length(one)            ≈ 5.0
    @test Cecelia.track_speed(one)             ≈ 0.5
    @test Cecelia.track_straightness(one)      ≈ 1.0          # straight by definition
    @test isnan(Cecelia.track_overall_angle(one))            # n<3
    @test isnan(Cecelia.track_mean_turning_angle(one))
    @test Cecelia.track_asphericity(one)       == 1.0         # celltrackR convention for <3

    # single-position track: no div-by-zero, sane fallbacks
    pt = Track(1, [0.0], reshape([0.0, 0, 0], 1, 3))
    @test Cecelia.track_length(pt)        == 0.0
    @test isnan(Cecelia.track_speed(pt))                     # duration 0
    @test Cecelia.track_straightness(pt)  == 1.0             # length 0 → 1
    @test isnan(Cecelia.track_displacement_ratio(pt))        # maxDisplacement 0

    # zero net displacement (returns to origin): straightness 0, no div-by-zero
    loop = Track(1, [0.0, 1, 2], [0.0 0; 1 0; 0 0])
    @test Cecelia.track_displacement(loop)       ≈ 0.0
    @test Cecelia.track_length(loop)             ≈ 2.0
    @test Cecelia.track_straightness(loop)       ≈ 0.0
    @test Cecelia.track_displacement_ratio(loop) ≈ 0.0       # disp 0 / maxDisp 1
    @test Cecelia.track_outreach_ratio(loop)     ≈ 0.5       # maxDisp 1 / length 2
end

# ── Gating engine: transforms ─────────────────────────────────────────────
@testset "Transforms" begin
    # linear is identity
    @test apply_transform(LinearTransform(), 42.0) == 42.0
    @test invert_transform(LinearTransform(), 42.0) == 42.0

    # log / asinh round-trip
    lg = LogTransform()
    @test invert_transform(lg, apply_transform(lg, 1234.0)) ≈ 1234.0
    ah = AsinhTransform(cofactor=150.0)
    for x in (-500.0, 0.0, 37.0, 9000.0)
        @test invert_transform(ah, apply_transform(ah, x)) ≈ x atol=1e-6
    end

    # Logicle golden values — provenance:
    #   Generated once from FlowUtils' reference C implementation (`logicle_c`, the C port
    #   of Moore & Parks 2012), params T=262144 W=0.5 M=4.5 A=0, via:
    #     flowutils.transforms.logicle(x, channel_indices=[0], t=262144, m=4.5, w=0.5, a=0)
    #   flowutils was used transiently only to produce these numbers; it is NOT a runtime
    #   dependency. The values are baked in below so this test needs no Python at runtime.
    #   See app/src/gating/transforms.jl for the full citation.
    lc = LogicleTransform(T=262144, W=0.5, M=4.5, A=0)
    golden = [(-1000.0, -0.2321153540), (-100.0, 0.0090411347), (0.0, 0.1111111111),
              (1.0, 0.1122315321), (10.0, 0.1223042757), (100.0, 0.2131810875),
              (1000.0, 0.4543375762), (10000.0, 0.6838326572),
              (100000.0, 0.9069275915), (262144.0, 1.0)]
    for (x, gy) in golden
        @test apply_transform(lc, x) ≈ gy atol=1e-6
    end
    # round-trip
    for x in (-1000.0, 0.0, 1.0, 1000.0, 262144.0)
        @test invert_transform(lc, apply_transform(lc, x)) ≈ x atol=1e-4
    end
    # vectorised
    @test apply_transform(lc, [0.0, 262144.0]) ≈ [0.1111111111, 1.0] atol=1e-6

    # Range-based auto-linearisation: logicle collapses a bounded 0–1 measure (morphology) but
    # spreads a real intensity range → effective_transform swaps to linear only for the former.
    @test transform_kind(lc) == "logicle"
    @test transform_collapses(lc, 0.02, 1.0)              # solidity ∈ [0,1] → collapses
    @test !transform_collapses(lc, 0.0, 262144.0)         # full intensity range → fine
    @test !transform_collapses(lc, 0.0, 5000.0)           # large-range morphology (area) → keep logicle
    @test effective_transform(lc, 0.02, 1.0) isa LinearTransform
    @test effective_transform(lc, 0.0, 262144.0) === lc   # untouched
    @test effective_transform(LinearTransform(), 0.02, 1.0) isa LinearTransform  # linear never coerces
    @test !transform_collapses(LinearTransform(), 0.02, 1.0)
    # log needs ≥1 decade above the floor; asinh coerces when all data is inside its ~linear core
    @test transform_collapses(LogTransform(floor=1.0), 0.5, 5.0)
    @test !transform_collapses(LogTransform(floor=1.0), 1.0, 1e5)
    @test transform_collapses(AsinhTransform(cofactor=150.0), 0.0, 1.0)
    @test !transform_collapses(AsinhTransform(cofactor=150.0), 0.0, 5000.0)
    # degenerate extent (single value / non-finite) → no coercion, keep requested
    @test effective_transform(lc, 1.0, 1.0) === lc
    @test effective_transform(lc, NaN, 1.0) === lc
end

# ── Gating engine: gates ──────────────────────────────────────────────────
@testset "Gates" begin
    # rectangle (linear): inclusive bounds
    rg = RectangleGate("x", "y", 0.0, 10.0, 0.0, 10.0)
    xin = inside(rg, [5.0, 11.0, -1.0, 0.0], [5.0, 5.0, 5.0, 0.0])
    @test xin == BitVector([true, false, false, true])

    # polygon point-in-polygon (unit square)
    sq = [(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0)]
    @test point_in_polygon(0.5, 0.5, sq)
    @test !point_in_polygon(1.5, 0.5, sq)
    @test !point_in_polygon(-0.1, 0.5, sq)
    pg = PolygonGate("x", "y", sq)
    @test inside(pg, [0.5, 2.0], [0.5, 2.0]) == BitVector([true, false])

    # transformed-space gate: a rectangle in logicle coords selects by raw value
    lc = LogicleTransform(T=262144, W=0.5, M=4.5, A=0)
    # logicle(1000)=0.4543, logicle(100000)=0.9069 → gate [0.45,0.95] keeps 1000 & 100000, not 10
    rgl = RectangleGate("x", "y", 0.45, 0.95, 0.45, 0.95;
                        x_transform=lc, y_transform=lc)
    keep = inside(rgl, [10.0, 1000.0, 100000.0], [1000.0, 1000.0, 100000.0])
    @test keep == BitVector([false, true, true])

    # JSON round-trip preserves membership
    for g in (rg, pg, rgl)
        g2 = gate_from_spec(gate_spec(g))
        @test inside(g2, [5.0, 1000.0], [5.0, 1000.0]) == inside(g, [5.0, 1000.0], [5.0, 1000.0])
    end

    # project_gate: re-express a gate's stored geometry into a DISPLAY transform so its outline
    # aligns with points drawn in that transform (the client has no transform math).
    lcp = LogicleTransform(T=262144, W=0.5, M=4.5, A=0)
    # a rectangle stored in logicle coords, projected onto a LINEAR display → raw bounds
    rgp = RectangleGate("cd4", "cd8", 0.4543375762, 0.9069275915, 0.4543375762, 0.9069275915;
                        x_transform=lcp, y_transform=lcp)
    lin = LinearTransform()
    pj = project_gate(rgp, "cd4", "cd8", lin, lin)
    @test pj["kind"] == "rectangle"
    @test pj["x_min"] ≈ 1000.0 atol=1e-2      # invert(logicle, 0.4543) ≈ 1000
    @test pj["x_max"] ≈ 100000.0 atol=1e-1    # invert(logicle, 0.9069) ≈ 100000
    # projecting onto the SAME transform is (near) identity
    same = project_gate(rgp, "cd4", "cd8", lcp, lcp)
    @test same["x_min"] ≈ 0.4543375762 atol=1e-4
    # swapped channel order → x/y transposed
    sw = project_gate(rgp, "cd8", "cd4", lin, lin)
    @test sw !== nothing
    @test sw["y_min"] ≈ 1000.0 atol=1e-2
    # not on this channel pair → nothing
    @test project_gate(rgp, "cd4", "cd19", lin, lin) === nothing
    # polygon vertices map pointwise
    pgp = PolygonGate("cd4", "cd8", [(0.4543375762, 0.4543375762), (0.9069275915, 0.4543375762),
                                     (0.9069275915, 0.9069275915)]; x_transform=lcp, y_transform=lcp)
    # onto a DIFFERENT display transform (logicle→linear) the edges curve → sampled 12 pts/edge
    pjp = project_gate(pgp, "cd4", "cd8", lin, lin)
    @test pjp["kind"] == "polygon"
    @test length(pjp["vertices"]) == 36                 # 3 edges × 12 samples
    @test pjp["vertices"][1][1] ≈ 1000.0 atol=1e-2       # first sample = corner 1
    # edge 1 is horizontal (y const ≈1000); its midpoint (idx 7 = t=0.5) bows far below the straight
    # chord's linear midpoint (~50500) — that bow is the curve the sampling captures.
    @test pjp["vertices"][7][2] ≈ 1000.0 atol=1e-2
    @test pjp["vertices"][7][1] < 50000
    # onto the SAME transform edges are already straight → corners kept (clean edit round-trip)
    @test length(project_gate(pgp, "cd4", "cd8", lcp, lcp)["vertices"]) == 3
end

# ── Gating engine: density ────────────────────────────────────────────────
@testset "Density" begin
    x = collect(0.0:0.01:1.0)
    d = density_2d(x, x; bins=10)
    @test sum(d.counts) == length(x)          # every point counted once
    @test size(d.counts) == (10, 10)
    @test d.counts[1, 10] == 0                 # off-diagonal empty (x==y data)
    @test all(d.counts[i, i] >= 1 for i in 1:10)

    # NaN/Inf (object/morphology measures on degenerate objects) must be skipped, not throw —
    # extents come from the finite values, and a non-finite point contributes to no bin.
    xn = [0.0, 0.5, 1.0, NaN, Inf, -Inf]
    dn = density_2d(xn, xn; bins=10)
    @test sum(dn.counts) == 3                  # only the 3 finite points counted
    @test (dn.x_min, dn.x_max) == (0.0, 1.0)   # extents ignore NaN/Inf
    # all-non-finite → falls back to the default extent and empty counts (no throw)
    dz = density_2d([NaN, Inf], [NaN, Inf]; bins=4)
    @test sum(dz.counts) == 0
end

# ── Population manager: paths, tree, persistence ──────────────────────────
@testset "Population manager" begin
    @test pop_parent("/a/b") == "/a"
    @test pop_parent("/a") == ROOT
    @test pop_name("/a/b") == "b"
    @test pop_path("/a", "b") == "/a/b"
    @test pop_path(ROOT, "a") == "/a"
    @test is_root(ROOT) && is_root("/") && !is_root("/a")

    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "cd4"; parent=ROOT, gate=RectangleGate("x", "y", 0, 10, 0, 10), colour="#f00")
    add_pop!(m, "cd8"; parent="/cd4", gate=PolygonGate("x", "y", [(0.,0.),(0.,5.),(5.,5.),(5.,0.)]))
    @test pop_paths(m) == ["/cd4", "/cd4/cd8"]
    @test direct_children(m, ROOT) == ["/cd4"]
    @test descendants(m, "/cd4") == ["/cd4/cd8"]

    # save/load round-trip (tree + gate)
    td = mktempdir()
    save_pop_map!(m, td)
    @test isfile(gating_path(td, "B"))
    # atomic write: the temp file is renamed into place, never left behind
    @test !isfile(gating_path(td, "B") * ".tmp")
    m2 = load_pop_map(td, "B")
    @test pop_paths(m2) == pop_paths(m)
    @test pop_at(m2, "/cd4").colour == "#f00"
    @test pop_at(m2, "/cd4").gate isa RectangleGate
    @test pop_at(m2, "/cd4/cd8").gate isa PolygonGate

    # cascade rename
    rename_pop!(m, "/cd4", "tcell")
    @test Set(pop_paths(m)) == Set(["/tcell", "/tcell/cd8"])
    @test pop_at(m, "/tcell/cd8").parent == "/tcell"
    # cascade delete
    del_pop!(m, "/tcell")
    @test isempty(pop_paths(m))
end

# ── clust / trackclust pop types (cluster-membership populations) ─────────────
# A cluster pop is a filter on the `clusters.{suffix}` column (clustPops/clustTracks output):
# filter_fun="in", filter_values=[ticked cluster ids]. Stored in its own sidecar so it never
# collides with flow gates. Headless — membership via a recompute! closure (no fixture).
# gating pop types = the hand-drawn ones (flow=cells, track=tracks); clust/trackclust are filters.
# Drives copy-to-images + the defining-plot view (one abstraction over both, no flow special-casing).
@testset "GATING_POP_TYPES" begin
    @test GATING_POP_TYPES == ("flow", "track")
    @test is_gating_pop_type("flow") && is_gating_pop_type("track")
    @test !is_gating_pop_type("clust") && !is_gating_pop_type("trackclust") && !is_gating_pop_type("live")
end

# generic value_name presence check on an image (drives copy-to-images target filtering).
# `_active` is a bookkeeping key, not a value_name → excluded by versioned_keys.
@testset "img_has_value_name" begin
    proj = create_project!(name="vn-test-$(rand(1000:9999))")
    s    = add_set!(proj; name="s")
    img  = add_image!(s; name="i")
    img.label_props = Dict("A" => "A.h5ad", "B" => "B.h5ad", "_active" => "A")
    @test Set(img_value_names(img)) == Set(["A", "B"])
    @test img_has_value_name(img, "A") && img_has_value_name(img, "B")
    @test !img_has_value_name(img, "C") && !img_has_value_name(img, "_active")
    rm(proj.root; recursive=true)
end

@testset "clust / trackclust pop types" begin
    td = mktempdir()
    # each clustering pop_type routes to its OWN gating sidecar (no collision with flow's {vn}.json)
    @test endswith(gating_path(td, "B"; pop_type="flow"),       joinpath("gating", "B.json"))
    @test endswith(gating_path(td, "B"; pop_type="clust"),      joinpath("gating", "B__clust.json"))
    @test endswith(gating_path(td, "B"; pop_type="trackclust"), joinpath("gating", "B__trackclust.json"))
    @test endswith(gating_path(td, "B"; pop_type="track"),      joinpath("gating", "B__tracks.json"))

    # cluster pop membership = filter "in" over the cluster code column
    m = PopulationMap(pop_type="clust", value_name="B")
    add_pop!(m, "myeloid"; filter_measure="clusters.default", filter_fun="in",
             filter_values=[1, 3], colour="#10b981")
    fetch = _ -> DataFrame("label" => [10, 11, 12, 13, 14],
                           "clusters.default" => [0, 1, 2, 3, 1])
    recompute!(m, fetch)
    @test Set(cells_in_pop(m, "/myeloid")) == Set([11, 13, 14])   # codes ∈ {1,3}

    # save/load round-trip → own file, filter fields preserved, flow file untouched
    save_pop_map!(m, td)
    @test isfile(gating_path(td, "B"; pop_type="clust"))
    @test !isfile(gating_path(td, "B"; pop_type="flow"))
    m2 = load_pop_map(td, "B"; pop_type="clust")
    @test pop_at(m2, "/myeloid").filter_measure == "clusters.default"
    @test pop_at(m2, "/myeloid").filter_fun == "in"
    @test Set(pop_at(m2, "/myeloid").filter_values) == Set([1, 3])
end

@testset "region pop type (spatial regions)" begin
    # region reuses the cluster-pop machinery with its OWN `regions.{suffix}` column prefix
    # (docs/todo/SPATIAL_REGIONS_PLAN.md, Decision 5) — no duplicated logic, one generalisation.
    td = mktempdir()
    @test endswith(gating_path(td, "B"; pop_type="region"), joinpath("gating", "B__region.json"))
    @test Cecelia._is_cluster_pop_type("region")
    @test Cecelia._cluster_measure_prefix("region") == "regions."
    @test Cecelia._cluster_measure_prefix("clust") == "clusters."
    @test is_track_pop("region", "/tumour_zone") == false          # regions are per-cell, not per-track
    @test !is_gating_pop_type("region")                            # filter/membership pop, not a gate

    # region membership = filter "in" over the region code column (same engine path as clust)
    m = PopulationMap(pop_type="region", value_name="B")
    add_pop!(m, "tumour_zone"; filter_measure="regions.niches", filter_fun="in",
             filter_values=[1, 3], colour="#10b981")
    fetch = _ -> DataFrame("label" => [10, 11, 12, 13, 14],
                           "regions.niches" => [0, 1, 2, 3, 1])
    recompute!(m, fetch)
    @test Set(cells_in_pop(m, "/tumour_zone")) == Set([11, 13, 14])   # region codes ∈ {1,3}

    # referenced-suffixes generalisation reads the region prefix from the map's own pop_type
    @test Cecelia._referenced_cluster_suffixes(m) == Set(["niches"])

    # save/load round-trip → own __region file, flow file untouched
    save_pop_map!(m, td)
    @test isfile(gating_path(td, "B"; pop_type="region"))
    @test !isfile(gating_path(td, "B"; pop_type="flow"))
    m2 = load_pop_map(td, "B"; pop_type="region")
    @test pop_at(m2, "/tumour_zone").filter_measure == "regions.niches"
    @test Set(pop_at(m2, "/tumour_zone").filter_values) == Set([1, 3])

    # categorical name-rule: `regions`/`regions.{suffix}` are always a code set, even past the level cap
    @test Cecelia._is_categorical_col(collect(0:50), "regions.niches")   # 51 int levels, name-rule wins
    @test Cecelia._is_categorical_col(collect(0:50), "regions")
    @test Cecelia._is_categorical_col([0.0, 1.5, 2.7], "regions.niches") # decimals irrelevant under name-rule

    # per-region heatmap matrix detection routes through the shared suffix extractor (regions. prefix)
    @test Cecelia._cluster_matrix_suffix("matrix", "regions.niches") == "niches"
    @test Cecelia._cluster_matrix_suffix("matrix", "clusters.default") == "default"
end

@testset "contact_matrix — CODEX log-odds heatmap matrix" begin
    # sidecar spatialStats/{suffix}.json → symmetric pop×pop log-odds matrix for the plot renderer
    td = mktempdir(); mkpath(joinpath(td, "spatialStats"))
    open(joinpath(td, "spatialStats", "default.json"), "w") do f
        write(f, """{"basis":["B/qc","T/qc"],"nCells":100,"nEdges":200,"records":[""" *
                 """{"popA":"B/qc","popB":"B/qc","observed":10,"expected":5,"logOdds":0.7,"association":"associated"},""" *
                 """{"popA":"B/qc","popB":"T/qc","observed":1,"expected":5,"logOdds":-1.1,"association":"avoided"},""" *
                 """{"popA":"T/qc","popB":"T/qc","observed":8,"expected":4,"logOdds":0.6,"association":"associated"}]}""")
    end
    m = contact_matrix(CciaImage(; dir=td))
    @test m.suffixes == ["default"] && m.suffix == "default"
    @test Set(m.basis) == Set(["B/qc", "T/qc"]) && m.nCells == 100 && m.nEdges == 200
    val(x, y) = only(c.value for c in m.cells if c.x == x && c.y == y)
    @test val("B/qc", "T/qc") ≈ -1.1 && val("T/qc", "B/qc") ≈ -1.1   # symmetric fill
    @test val("B/qc", "B/qc") ≈ 0.7 && val("T/qc", "T/qc") ≈ 0.6
    @test length(m.cells) == 4                                       # 2×2 fully filled
    # no sidecar → empty (route returns empty, UI shows "run contact stats first")
    m0 = contact_matrix(CciaImage(; dir=mktempdir()))
    @test isempty(m0.cells) && isempty(m0.suffixes)
end

@testset "region pop auto-share (co-clustered value_names, cell granularity)" begin
    # regions are a per-run column shared across co-clustered segmentations — the identical
    # auto-share/expand machinery as clust, exercised via the `regions.` prefix + cell granularity.
    td = mktempdir()
    lpdir = joinpath(td, "labelProps"); mkpath(lpdir)
    # A & B were region-clustered together (both CELL sidecars carry suffix "niches"); C was not.
    for vn in ("A", "B")
        open(joinpath(lpdir, "$(vn).clustfeatures.json"), "w") do f
            JSON3.write(f, Dict("niches" => Dict("features" => ["flow.region.cd8"], "partOf" => ["u1"])))
        end
    end
    am = PopulationMap(pop_type="region", value_name="A")
    add_pop!(am, "TumourZone"; filter_measure="regions.niches", filter_fun="in", filter_values=[2], colour="#c061cb")
    save_pop_map!(am, td)

    img = CciaImage(; dir=td)
    img.label_props = Dict("A" => "A.h5ad", "B" => "B.h5ad", "C" => "C.h5ad", "_active" => "A")

    @test Set(Cecelia.co_clustered_value_names(img, "niches"; granularity=:cell)) == Set(["A", "B"])

    # B has no sidecar but IS co-clustered → borrows A's region pops, relabeled to B
    mb = load_pop_map(img; value_name="B", pop_type="region")
    @test Set(keys(mb.pops)) == Set(["/TumourZone"]) && mb.value_name == "B"
    @test all(p.value_name == "B" for p in values(mb.pops))
    # C was not in the run → no borrow
    @test isempty(load_pop_map(img; value_name="C", pop_type="region").pops)

    # bare region-pop ref expands across all co-clustered segmentations
    @test Set(Cecelia._expand_cluster_pops(img, ["/TumourZone"], "region", "A")) ==
          Set(["A/TumourZone", "B/TumourZone"])
end

@testset "bare cluster/region pops: run-wide by default, per-segmentation on request" begin
    # A bare cluster-family ref spans every co-clustered segmentation (old-R popDT parity) — right
    # for "show me this run's cluster", WRONG for a plot series, where the picker already offered
    # each (segmentation, population) pair separately. Ticking 3 region pops under B plotted 6.
    td = mktempdir()
    lpdir = joinpath(td, "labelProps"); mkpath(lpdir)
    for vn in ("B", "T")
        Cecelia._write_clust_features!(joinpath(lpdir, "$(vn).h5ad"), "immune",
                                       ["spatial.comp.x.immune"], ["u1"]; family = "regions")
    end
    m = PopulationMap(pop_type="region", value_name="B")
    add_pop!(m, "Population 1"; filter_measure="regions.immune", filter_fun="in", filter_values=[1])
    save_pop_map!(m, td)
    img = CciaImage(; dir=td)
    img.label_props = Dict("B" => "B.h5ad", "T" => "T.h5ad", "_active" => "B")

    # default: bare ref fans out across the run's segmentations
    @test Set(Cecelia._expand_cluster_pops(img, ["/Population 1"], "region", "B")) ==
          Set(["B/Population 1", "T/Population 1"])
    # explicitly value_name-prefixed refs are untouched either way
    @test Cecelia._expand_cluster_pops(img, ["B/Population 1"], "region", "B") == ["B/Population 1"]
    # a non-cluster pop type never expands
    @test Cecelia._expand_cluster_pops(img, ["/gate"], "flow", "B") == ["/gate"]
    # and pop_df exposes the opt-out the series path uses (keyword present, both forms)
    @test :expand_cluster_pops in Base.kwarg_decl(
        only(methods(pop_df, (CciaImage, AbstractString, Any))))
end

@testset "clustfeatures sidecar — families, labels, legacy layouts" begin
    # The sidecar is keyed `{family}.{suffix}` so a cell clustering and a REGION clustering that
    # share a suffix coexist on one segmentation instead of clobbering each other. Three historical
    # layouts must all read back through the ONE shared reader (docs/todo/SPATIAL_REGIONS_PLAN.md).
    @test Cecelia._cluster_measure_family("region") == "regions"
    @test Cecelia._cluster_measure_family("clust")  == "clusters"
    @test Cecelia._cluster_measure_family("trackclust") == "clusters"
    @test Cecelia._clustfeatures_key("immune", "regions") == "regions.immune"
    @test Cecelia._clustfeatures_split_key("regions.immune") == ("immune", "regions")
    @test Cecelia._clustfeatures_split_key("clusters.a.b")   == ("a.b", "clusters")
    @test Cecelia._clustfeatures_split_key("immune")         == ("immune", nothing)   # legacy → any family

    td = mktempdir(); lpdir = joinpath(td, "labelProps"); mkpath(lpdir)
    props = joinpath(lpdir, "B.h5ad")

    # two runs, SAME suffix, different families — the collision that used to silently overwrite
    Cecelia._write_clust_features!(props, "immune", ["mean_intensity_0"], ["u1"]; family="clusters")
    Cecelia._write_clust_features!(props, "immune", ["spatial.comp.B_qc.immune"], ["u1", "u2"];
                                   family="regions",
                                   labels=Dict("spatial.comp.B_qc.immune" => "B/qc"))
    @test Cecelia._clustfeatures_features(props, "immune"; family="clusters") == ["mean_intensity_0"]
    @test Cecelia._clustfeatures_features(props, "immune"; family="regions") == ["spatial.comp.B_qc.immune"]
    @test Cecelia._clustfeatures_suffixes(props; family="clusters") == Set(["immune"])
    @test Cecelia._clustfeatures_suffixes(props; family="regions")  == Set(["immune"])
    # partOf stays per-family (the region run covered one more image)
    e_r = Cecelia._clustfeatures_entry(props, "immune"; family="regions")
    e_c = Cecelia._clustfeatures_entry(props, "immune"; family="clusters")
    @test length(get(e_r, "partOf", [])) == 2 && length(get(e_c, "partOf", [])) == 1
    @test String(get(e_r, "labels", Dict())["spatial.comp.B_qc.immune"]) == "B/qc"

    # LEGACY bare-suffix entry (pre-family) matches every family, so existing data keeps working
    legacy = joinpath(lpdir, "L.h5ad")
    open(replace(legacy, r"\.h5ad$" => ".clustfeatures.json"), "w") do f
        JSON3.write(f, Dict("niches" => Dict("features" => ["x"], "partOf" => ["u1"])))
    end
    @test Cecelia._clustfeatures_suffixes(legacy; family="regions")  == Set(["niches"])
    @test Cecelia._clustfeatures_suffixes(legacy; family="clusters") == Set(["niches"])
    @test Cecelia._clustfeatures_features(legacy, "niches"; family="regions") == ["x"]

    # OLDEST layout: {suffix => [features]} (a bare array, no membership) normalises to the current shape
    oldest = joinpath(lpdir, "O.h5ad")
    open(replace(oldest, r"\.h5ad$" => ".clustfeatures.json"), "w") do f
        JSON3.write(f, Dict("old" => ["f1", "f2"]))
    end
    @test Cecelia._clustfeatures_features(oldest, "old") == ["f1", "f2"]
    @test isempty(get(Cecelia._clustfeatures_entry(oldest, "old"), "partOf", ["nonempty"]))

    # absent run / absent file → empty, never a throw
    @test Cecelia._clustfeatures_features(props, "nosuchrun"; family="regions") == String[]
    @test Cecelia._clustfeatures_entry(joinpath(lpdir, "missing.h5ad"), "x") === nothing
end

@testset "spatial obs measures are NUMERIC, not integer code sets" begin
    # A 0/1 contact/aggregate flag has few integer levels, so the generic heuristic calls it a
    # categorical code set — and the plot panel then offers only count/bar and snaps the chart type
    # to `count`. Commit 16ead1d fixed exactly this for integer morphology by exempting `var`
    # columns; these are `obs`, so they need a name-rule instead.
    flag = [0, 1, 1, 0, 1]
    @test Cecelia._is_categorical_col(flag, "live.cell.contact#live.T_qc__tracked") == false
    @test Cecelia._is_categorical_col(flag, "flow.cell.is.aggregate") == false
    @test Cecelia._is_categorical_col([1, 2, 3], "live.cell.min_distance#live.T_qc") == false
    @test Cecelia._is_categorical_col([0, 0, 1], "spatial.comp.other.immune") == false
    # …while the IDENTIFIERS beside them stay categorical (they are label codes, not quantities)
    @test Cecelia._is_categorical_col([3, 7, 7], "live.cell.contact_id#live.T_qc__tracked") == true
    @test Cecelia._is_categorical_col([1, 2, 2], "live.cell.aggregate.id") == true
    # and the existing rules are untouched
    @test Cecelia._is_categorical_col([0, 1, 2], "regions.immune") == true
    @test Cecelia._is_categorical_col([0, 1, 2], "clusters.default") == true
    @test Cecelia._is_categorical_col([1.5, 2.5], "live.cell.speed") == false
    @test Cecelia._is_categorical_col([1, 2, 3], "live.cell.hmm.state.movement") == true
end

@testset "region 'other' column is skipped when it would be all-zero" begin
    # A graph built over the basis populations themselves contains nothing outside the basis, so the
    # "other" composition column is all-zero — not a measurement, just a flat row in the heatmap.
    # The runner drops it and flags that in the run QC; Julia must then not advertise it in the
    # clustfeatures sidecar, or the heatmap offers a column the table doesn't have.
    d = mktempdir()
    p = joinpath(d, "region_qc.json")
    @test Cecelia._region_other_all_zero(joinpath(d, "absent.json")) == false   # missing → written
    open(p, "w") do f; JSON3.write(f, Dict("otherAllZero" => true)); end
    @test Cecelia._region_other_all_zero(p) == true
    open(p, "w") do f; JSON3.write(f, Dict("otherAllZero" => false)); end
    @test Cecelia._region_other_all_zero(p) == false
    open(p, "w") do f; JSON3.write(f, Dict("nClusters" => 3)); end                # older run, no flag
    @test Cecelia._region_other_all_zero(p) == false
    write(p, "{ not json")                                                        # unreadable → written
    @test Cecelia._region_other_all_zero(p) == false
end

@testset "region composition column naming (one namer, Julia → Python)" begin
    # Julia names the composition columns AND records them in the sidecar; the Python runner is
    # handed the same list. They used to be derived independently and disagreed, so the region
    # composition heatmap asked for columns that did not exist.
    @test Cecelia._comp_col("B/qc/_tracked", "immune") == "spatial.comp.B_qc__tracked.immune"
    @test Cecelia._comp_col("T cells", "x") == "spatial.comp.T_cells.x"
    @test Cecelia._comp_col("plain", "s") == "spatial.comp.plain.s"
end

@testset "compound filter populations (Decision 15 — AND-ed conditions)" begin
    # a user-defined filter pop combining two obs conditions in ONE pop: CD4>0.5 AND speed>5
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "CD4hi_fast"; colour="#8b5cf6", filter_conditions=[
        Dict("measure" => "live.cell.cd4",   "fun" => "gt", "values" => 0.5),
        Dict("measure" => "live.cell.speed", "fun" => "gt", "values" => 5)])
    fetch = _ -> DataFrame("label" => [1, 2, 3, 4],
                           "live.cell.cd4"   => [0.9, 0.9, 0.1, 0.6],
                           "live.cell.speed" => [10,  2,   10,  7])
    recompute!(m, fetch)
    @test Set(cells_in_pop(m, "/CD4hi_fast")) == Set([1, 4])   # BOTH hold: (0.9,10) and (0.6,7)

    # single fields mirror conditions[1] so single-field readers still work
    p = pop_at(m, "/CD4hi_fast")
    @test p.filter_measure == "live.cell.cd4" && p.filter_fun == "gt" && length(p.filter_conditions) == 2

    # round-trip through to_tree/from_tree preserves the conditions + membership
    m2 = from_tree(to_tree(m)); recompute!(m2, fetch)
    @test Set(cells_in_pop(m2, "/CD4hi_fast")) == Set([1, 4])
    @test length(pop_at(m2, "/CD4hi_fast").filter_conditions) == 2

    # a missing condition column → the whole pop degrades to empty (warns), never crashes
    fetch1 = _ -> DataFrame("label" => [1, 2], "live.cell.cd4" => [0.9, 0.1])   # speed absent
    @test_logs (:warn, r"live\.cell\.speed") match_mode=:any recompute!(m, fetch1)
    @test isempty(cells_in_pop(m, "/CD4hi_fast"))
end

@testset "recompute! — a missing filter/gate column degrades to empty (no crash)" begin
    # A cluster pop whose `clusters.{suffix}` column isn't in the fetched frame — e.g. evaluated
    # against a segmentation that didn't take part in that run, so `fetch_cols` silently dropped it
    # — must resolve to NO members, not raise `ArgumentError: column name … not found` and 500 the
    # whole plot. Regression: the trackclust heatmap crash (clusters.default not found).
    m = PopulationMap(pop_type="trackclust", value_name="C")
    add_pop!(m, "present"; filter_measure="clusters.movement", filter_fun="in", filter_values=[1, 2], colour="#10b981")
    add_pop!(m, "absent";  filter_measure="clusters.default",  filter_fun="in", filter_values=[0, 1], colour="#ef4444")
    # frame HAS clusters.movement but NOT clusters.default
    fetch = _ -> DataFrame("label" => [10, 11, 12, 13], "clusters.movement" => [0, 1, 2, 1])
    @test_logs (:warn, r"clusters\.default") match_mode=:any recompute!(m, fetch)  # warns, doesn't throw
    @test Set(cells_in_pop(m, "/present")) == Set([11, 12, 13])   # present column resolves normally
    @test isempty(cells_in_pop(m, "/absent"))                     # missing column → empty membership

    # same guard for a GATE whose axis column is absent from the frame
    mg = PopulationMap(pop_type="flow", value_name="C")
    add_pop!(mg, "g"; gate=RectangleGate("x", "missingY", 0.0, 10.0, 0.0, 10.0), colour="#abcdef")
    fetchg = _ -> DataFrame("label" => [1, 2], "x" => [1.0, 2.0])   # no "missingY"
    @test_logs (:warn, r"missingY") match_mode=:any recompute!(mg, fetchg)
    @test isempty(cells_in_pop(mg, "/g"))
end

@testset "colour_by_palette — pop colour else default" begin
    # a value a user pop FILTERS for on the column → that pop's colour; the rest → OKABE_ITO by
    # sorted position. Generalises "use the population's colour where one exists" (a cluster pop is
    # just a filter on clusters.{suffix}).
    m = PopulationMap(pop_type="clust", value_name="B")
    add_pop!(m, "directed";   filter_measure="clusters.mov", filter_fun="in", filter_values=[2],       colour="#ff1493")
    add_pop!(m, "crawling";   filter_measure="clusters.mov", filter_fun="in", filter_values=[0, 3, 4], colour="#ffd700")
    add_pop!(m, "unrelated";  filter_measure="clusters.other", filter_fun="in", filter_values=[1],     colour="#000001")

    pal = colour_by_palette(m, "clusters.mov", [0, 1, 2, 3, 4])
    @test pal[2] == "#ff1493"                 # user pop colour
    @test pal[0] == "#ffd700" && pal[3] == "#ffd700" && pal[4] == "#ffd700"
    @test pal[1] == OKABE_ITO[1]              # uncovered value 1 → first default
    # a pop filtering a DIFFERENT column never leaks its colour in
    @test pal[1] != "#000001"

    # numeric tolerance: a filter value stored as 2.0 still matches integer column value 2
    m3 = PopulationMap(pop_type="clust", value_name="B")
    add_pop!(m3, "d"; filter_measure="clusters.mov", filter_fun="in", filter_values=[2.0], colour="#abcdef")
    @test colour_by_palette(m3, "clusters.mov", [2])[2] == "#abcdef"

    # no matching pop → all default, by sorted position (stable)
    empty = PopulationMap(pop_type="clust", value_name="B")
    p2 = colour_by_palette(empty, "clusters.mov", [5, 3, 3, 1])
    @test p2[1] == OKABE_ITO[1] && p2[3] == OKABE_ITO[2] && p2[5] == OKABE_ITO[3]

    # pop_colour_overrides: string-keyed {value => hex} for the wire (2.0/2 → "2"); only pops on
    # the column contribute; no default fill (the bridge does that).
    ov = pop_colour_overrides(m, "clusters.mov")
    @test ov == Dict("2" => "#ff1493", "0" => "#ffd700", "3" => "#ffd700", "4" => "#ffd700")
    @test pop_colour_overrides(m3, "clusters.mov") == Dict("2" => "#abcdef")   # 2.0 → "2"
    @test isempty(pop_colour_overrides(m, "clusters.absent"))

    # pop_label_overrides: same keying, value → the POP NAME (so the legend reads "directed", not "2")
    lbl = pop_label_overrides(m, "clusters.mov")
    @test lbl == Dict("2" => "directed", "0" => "crawling", "3" => "crawling", "4" => "crawling")
    @test isempty(pop_label_overrides(m, "clusters.absent"))
end

# ── Summary-canvas population picker (plot_pop_types / plot_population_groups) ──
# The logic the /api/plots/populations route delegates to — pure, so tested here (the route is a
# thin wrapper). Covers granularity→pop_type selection, cross-image + cross-pop_type union/dedup,
# derived-pop injection, and pop_type tagging (the track-pops-in-the-picker fix; docs/POPULATION.md).
@testset "plot population picker" begin
    # pop_type selection by granularity
    @test plot_pop_types("live", "cell") == ["live"]
    @test plot_pop_types("live", "")     == ["live"]
    @test plot_pop_types("live", "track") == ["live", "track"]
    @test plot_pop_types("track", "track") == ["track"]          # no duplicate

    # flatten_pop_tree: pre-order paths + colours
    fm = PopulationMap(pop_type="flow", value_name="C")
    add_pop!(fm, "qc"; gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#ef4444")
    add_pop!(fm, "sub"; parent="/qc", gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#abc")
    flat = flatten_pop_tree(to_tree(fm))
    @test [p for (p, _, _) in flat] == ["/qc", "/qc/sub"]
    @test flat[1][3] == "#ef4444"

    # a track-gated map (pop_type "track") with one pop
    tm = PopulationMap(pop_type="track", value_name="C")
    add_pop!(tm, "TEST"; filter_measure="live.track.speed", filter_fun="gt", filter_values=5, colour="#f59e0b")

    # loaders injected (as the API passes versioned_keys/load_pop_map closures)
    names_for = _ -> ["C"]
    load = (_, vn, pt) -> vn == "C" ? (pt == "track" ? tm : (pt == "live" ? fm : nothing)) : nothing

    # CELL granularity → live pops only, with derived `_tracked` at root AND under each stored pop
    # (so /qc/_tracked is a selectable, indented child); a derived child directly follows its parent.
    cell = plot_population_groups([:img1], names_for, load, plot_pop_types("live", "cell"))
    @test length(cell) == 1 && cell[1].value_name == "C"
    cpops = cell[1].populations
    @test [p.path for p in cpops] ==
          ["/_tracked", "/qc", "/qc/_tracked", "/qc/sub", "/qc/sub/_tracked"]
    @test all(p.pop_type == "live" for p in cpops)
    @test !any(p.path == "/TEST" for p in cpops)
    # the nested derived pop is named by its leaf (indents under its parent in the UI)
    @test only(p for p in cpops if p.path == "/qc/_tracked").name == "_tracked"
    # a derived child inherits its parent pop's colour (so /qc/_tracked pairs with /qc visually —
    # the derived colour is read-only on the behaviour page, the parent's is editable on gating)
    @test only(p for p in cpops if p.path == "/qc/_tracked").colour == "#ef4444"      # = /qc
    @test only(p for p in cpops if p.path == "/qc/sub/_tracked").colour == "#abc"      # = /qc/sub
    @test only(p for p in cpops if p.path == "/_tracked").colour == "#7c93b8"          # root: no parent → grey

    # TRACK granularity → unions live (incl. nested /qc/_tracked) AND track (/TEST), each tagged
    trk = plot_population_groups([:img1], names_for, load, plot_pop_types("live", "track"))
    tp = trk[1].populations
    @test Set(p.path for p in tp) ==
          Set(["/_tracked", "/qc", "/qc/_tracked", "/qc/sub", "/qc/sub/_tracked", "/TEST"])
    test_pop = only(p for p in tp if p.path == "/TEST")
    @test test_pop.pop_type == "track" && test_pop.colour == "#f59e0b"
    @test only(p for p in tp if p.path == "/qc").pop_type == "live"
    @test !any(p.path == "/TEST/_tracked" for p in tp)          # no track-derived pop registered

    # root_derived_ok predicate: hide the root-level /_tracked (the API passes false when tracking
    # was gated → root is a redundant duplicate of /qc/_tracked) while KEEPING the per-pop derived
    # children. Default (no predicate) still offers root /_tracked — asserted by `cell` above.
    gated = plot_population_groups([:img1], names_for, load, plot_pop_types("live", "cell");
                                   root_derived_ok = (_v, _pt, dpath) -> dpath != "/_tracked")
    gpaths = [p.path for p in gated[1].populations]
    @test !("/_tracked" in gpaths)                              # root hidden
    @test "/qc/_tracked" in gpaths && "/qc/sub/_tracked" in gpaths   # per-gate derived kept

    # cross-image UNION + dedup: two images both expose "C" → each (pop_type, path) appears once
    dedup = plot_population_groups([:img1, :img2], names_for, load, ["live"])
    @test length(dedup) == 1
    @test length(dedup[1].populations) == length(cpops)         # no duplicates across images

    # LABELS (gateless): no gating map — one selectable pop per segmentation value_name, named by
    # the value_name, tagged pop_type "labels" (segmentation QC: B/T plot side by side).
    names2 = _ -> ["B", "T"]
    lab = plot_population_groups([:img1], names2, (args...) -> error("must not load a map for labels"),
                                 ["labels"])
    @test [g.value_name for g in lab] == ["B", "T"]
    @test all(g -> length(g.populations) == 1, lab)
    bp = only(lab[1].populations)
    @test bp.path == "/labels" && bp.name == "B" && bp.pop_type == "labels"
    @test only(lab[2].populations).name == "T"
end

@testset "popScope population picker" begin
    # is_track_pop: the sole cell-vs-track test (Julia parity of the R `isTrack` attribute)
    @test is_track_pop("live", "/qc") == false                  # plain cell gate
    @test is_track_pop("flow", "/qc/sub") == false
    @test is_track_pop("clust", "/myeloid") == false            # cell cluster
    @test is_track_pop("live", "/_tracked") == true             # derived tracked set (root)
    @test is_track_pop("live", "/qc/_tracked") == true          # derived tracked subset of a gate
    @test is_track_pop("track", "/TEST") == true                # per-track gate
    @test is_track_pop("trackclust", "/clusterA") == true       # track cluster

    # scope_pop_types: sources loaded per scope; clusters toggleable; unknown scope throws.
    # `cells` also loads `region` (spatial regions) alongside `clust` — both cluster-family.
    @test scope_pop_types("cells", true)  == ["live", "clust", "region"]
    @test scope_pop_types("cells", false) == ["live"]
    @test scope_pop_types("tracks", true)  == ["live", "track", "trackclust"]
    @test scope_pop_types("tracks", false) == ["live", "track"]
    @test_throws ErrorException scope_pop_types("bogus", true)

    # maps: flow gates (/qc, /qc/sub), a per-track gate (/TEST), a cell cluster (/myeloid),
    # a track cluster (/clusterA)
    fm = PopulationMap(pop_type="flow", value_name="C")
    add_pop!(fm, "qc"; gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#ef4444")
    add_pop!(fm, "sub"; parent="/qc", gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#abc")
    tm = PopulationMap(pop_type="track", value_name="C")
    add_pop!(tm, "TEST"; filter_measure="live.track.speed", filter_fun="gt", filter_values=5, colour="#f59e0b")
    cm = PopulationMap(pop_type="clust", value_name="C")
    add_pop!(cm, "myeloid"; filter_measure="clusters.default", filter_fun="in", filter_values=[1, 2])
    tcm = PopulationMap(pop_type="trackclust", value_name="C")
    add_pop!(tcm, "clusterA"; filter_measure="clusters.tracks", filter_fun="in", filter_values=[0])
    names_for = _ -> ["C"]
    load = (_, vn, pt) -> vn != "C" ? nothing :
        pt == "live" ? fm : pt == "track" ? tm : pt == "clust" ? cm : pt == "trackclust" ? tcm : nothing

    # CELLS scope: all-cells root ("/") + plain gates + cell clusters; NO derived _tracked sets
    cells = population_scope_groups([:img1], names_for, load, "cells")
    @test length(cells) == 1 && cells[1].value_name == "C"
    cpaths = [p.path for p in cells[1].populations]
    @test cpaths == ["/", "/qc", "/qc/sub", "/myeloid"]
    @test cells[1].populations[1].name == "all"                 # backend all-cells root
    @test !any(occursin("_tracked", p) for p in cpaths)         # cells never show tracked sets
    @test all(!is_track_pop(p.pop_type, p.path) for p in cells[1].populations if p.path != "/")

    # CELLS, clusters excluded → drops /myeloid
    cells_nc = population_scope_groups([:img1], names_for, load, "cells"; include_clusters=false)
    @test [p.path for p in cells_nc[1].populations] == ["/", "/qc", "/qc/sub"]

    # TRACKS scope: derived _tracked sets (root + per-gate) + per-track gate + track cluster;
    # NO plain cell gates (/qc, /qc/sub) and NO all-cells root ("/")
    trk = population_scope_groups([:img1], names_for, load, "tracks")
    tpaths = Set(p.path for p in trk[1].populations)
    @test tpaths == Set(["/_tracked", "/qc/_tracked", "/qc/sub/_tracked", "/TEST", "/clusterA"])
    @test !("/qc" in tpaths) && !("/qc/sub" in tpaths) && !("/" in tpaths)
    @test all(is_track_pop(p.pop_type, p.path) for p in trk[1].populations)
    # a derived tracked child keeps its parent gate's colour (visual pairing, read-only)
    @test only(p for p in trk[1].populations if p.path == "/qc/_tracked").colour == "#ef4444"

    # TRACKS, gated tracking → root /_tracked hidden (redundant with /qc/_tracked); children kept
    trk_g = population_scope_groups([:img1], names_for, load, "tracks";
                                    root_derived_ok=(_v, _pt, d) -> d != "/_tracked")
    gpaths = Set(p.path for p in trk_g[1].populations)
    @test !("/_tracked" in gpaths) && "/qc/_tracked" in gpaths

    # TRACKS, clusters excluded → drops /clusterA, keeps the per-track gate /TEST
    trk_nc = population_scope_groups([:img1], names_for, load, "tracks"; include_clusters=false)
    tncpaths = Set(p.path for p in trk_nc[1].populations)
    @test !("/clusterA" in tncpaths) && "/TEST" in tncpaths
end

# ── pop_category + population_accept_groups (Decision 14, accepts allow-list) ────────────────
@testset "population accepts allow-list + category tags" begin
    # pop_category: gated / clustered / region / tracked / aggregated from (pop_type, leaf).
    @test pop_category("live", "/qc")               == "gated"
    @test pop_category("track", "/TEST")             == "gated"
    @test pop_category("clust", "/myeloid")          == "clustered"
    @test pop_category("trackclust", "/clusterA")    == "clustered"
    @test pop_category("region", "/r0")              == "region"
    @test pop_category("live", "/qc/_tracked")       == "tracked"
    @test pop_category("live", "/qc/" * Cecelia.AGGREGATED_POP_NAME) == "aggregated"

    # same fixtures as the popScope testset above, plus a region map and an aggregated cell pop.
    fm = PopulationMap(pop_type="flow", value_name="C")
    add_pop!(fm, "qc"; gate=RectangleGate("x", "y", 0, 1, 0, 1), colour="#ef4444")
    add_pop!(fm, Cecelia.AGGREGATED_POP_NAME; parent="/qc", filter_measure="live.cell.is.aggregate",
             filter_fun="gt", filter_values=0, reserved_ok=true)   # auto-created aggregate pop
    tm = PopulationMap(pop_type="track", value_name="C")
    add_pop!(tm, "TEST"; filter_measure="live.track.speed", filter_fun="gt", filter_values=5)
    cm = PopulationMap(pop_type="clust", value_name="C")
    add_pop!(cm, "myeloid"; filter_measure="clusters.default", filter_fun="in", filter_values=[1, 2])
    tcm = PopulationMap(pop_type="trackclust", value_name="C")
    add_pop!(tcm, "clusterA"; filter_measure="clusters.tracks", filter_fun="in", filter_values=[0])
    rm_ = PopulationMap(pop_type="region", value_name="C")
    add_pop!(rm_, "r0"; filter_measure="regions.default", filter_fun="in", filter_values=[0])
    names_for = _ -> ["C"]
    load = (_, vn, pt) -> vn != "C" ? nothing :
        pt == "live" ? fm : pt == "track" ? tm : pt == "clust" ? cm :
        pt == "trackclust" ? tcm : pt == "region" ? rm_ : nothing

    # accepts=["live"] → all-cells root + cell gate + the aggregated cell pop; NO tracked sets,
    # NO clusters/regions. Each population carries granularity/category tags.
    g = population_accept_groups([:img1], names_for, load, ["live"])[1].populations
    @test [p.path for p in g] == ["/", "/qc", "/qc/" * Cecelia.AGGREGATED_POP_NAME]
    @test all(p.granularity == "cell" for p in g)
    @test only(p for p in g if p.path == "/qc").category == "gated"
    @test only(p for p in g if endswith(p.path, Cecelia.AGGREGATED_POP_NAME)).category == "aggregated"

    # "flow" is an alias for "live".
    @test [p.path for p in population_accept_groups([:img1], names_for, load, ["flow"])[1].populations] ==
          [p.path for p in g]

    # region basis: cells (gated+clustered+region) AND tracks (gated+clustered). One picker, both
    # granularities — the case popScope could not express.
    basis = population_accept_groups([:img1], names_for, load,
                ["live", "clust", "region", "track", "trackclust"])[1].populations
    bcats = Set((p.granularity, p.category) for p in basis)
    @test ("cell", "gated") in bcats && ("cell", "clustered") in bcats && ("cell", "region") in bcats
    @test ("track", "tracked") in bcats && ("track", "gated") in bcats && ("track", "clustered") in bcats
    @test "/r0" in [p.path for p in basis] && "/myeloid" in [p.path for p in basis]
    @test "/clusterA" in [p.path for p in basis] && "/TEST" in [p.path for p in basis]

    # accepts=["clust"] alone → only cell clusters, no all-cells root (live not accepted).
    cl = population_accept_groups([:img1], names_for, load, ["clust"])[1].populations
    @test [p.path for p in cl] == ["/myeloid"]

    # popScope shim must still produce identical paths to the direct accept call.
    @test [p.path for p in population_scope_groups([:img1], names_for, load, "cells")[1].populations] ==
          [p.path for p in population_accept_groups([:img1], names_for, load,
                                ["live", "clust", "region"])[1].populations]

    # unknown token / empty list throw loudly.
    @test_throws ErrorException population_accept_groups([:img1], names_for, load, ["bogus"])
    @test_throws ErrorException population_accept_groups([:img1], names_for, load, String[])
end

# ── branch pop_type (BRANCHING_PLAN.md Decision 2) ────────────────────────────
# Adding "branch" to the framework must extend POP_MAP_SUFFIX/ACCEPT_TOKENS/pop_category and
# route via population_accept_groups with granularity="branch". The framework was designed to
# take a third pop_type; this guards the wiring.
@testset "branch pop_type wiring" begin
    # POP_MAP_SUFFIX resolves the gating file suffix.
    @test Cecelia.POP_MAP_SUFFIX["branch"] == BRANCH_PROPS_SUFFIX
    # build the expected tail with joinpath — a literal "gating/..." fails on Windows, where the
    # path is "\\gating\\stroma__branch.json" (the product is fine; the assertion wasn't portable)
    @test endswith(gating_path("/tmp", "stroma"; pop_type="branch"),
                   joinpath("gating", "stroma__branch.json"))

    # ACCEPT_TOKENS + validators.
    @test "branch" in Cecelia.ACCEPT_TOKENS

    # pop_category: branch pops are gated (the ensure_filter_pop! per-branch-type case).
    @test pop_category("branch", "/endpoint-to-endpoint") == "gated"

    # population_accept_groups tags branch pops with granularity="branch" and only surfaces
    # them when "branch" is in accepts. A mixed request keeps cells + branches.
    bm = PopulationMap(pop_type="branch", value_name="C")
    add_pop!(bm, "endpoint-to-endpoint"; filter_measure="branch-type",
             filter_fun="eq", filter_values=0)
    add_pop!(bm, "junction-to-junction"; filter_measure="branch-type",
             filter_fun="eq", filter_values=2)
    fm = PopulationMap(pop_type="flow", value_name="C")
    add_pop!(fm, "qc"; gate=RectangleGate("x", "y", 0, 1, 0, 1))
    names_for = _ -> ["C"]
    load = (_, vn, pt) -> vn != "C" ? nothing :
        pt == "live"   ? fm :
        pt == "branch" ? bm : nothing

    # accepts=["branch"] → only branch pops, no cell root
    br = population_accept_groups([:img1], names_for, load, ["branch"])[1].populations
    @test Set(p.path for p in br) == Set(["/endpoint-to-endpoint", "/junction-to-junction"])
    @test all(p.granularity == "branch" for p in br)
    @test all(p.category    == "gated"  for p in br)
    @test all(p.pop_type    == "branch" for p in br)

    # accepts=["live","branch"] → all-cells root + cell gate + branches
    mix = population_accept_groups([:img1], names_for, load, ["live", "branch"])[1].populations
    gcats = Set((p.granularity, p.category) for p in mix)
    @test ("cell", "gated") in gcats
    @test ("branch", "gated") in gcats

    # accepts=["live"] must NOT include branches.
    only_cells = population_accept_groups([:img1], names_for, load, ["live"])[1].populations
    @test all(p.granularity == "cell" for p in only_cells)
end

# ── ensure_filter_pop! — a cutoff materialised as a reusable filter pop (Decision 14) ────────
@testset "ensure_filter_pop! auto-created population" begin
    td = mktempdir()
    img = CciaImage(; dir=td)
    m = PopulationMap(; pop_type="flow", value_name="B")
    add_pop!(m, "qc"; gate=RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
    save_pop_map!(m, img)

    # a 0/1 flag column → aggregated pop under /qc (the generalisable `> 0`, not a baked TRUE/FALSE)
    created = ensure_filter_pop!(img, "flow", "B", ["/qc"], AGGREGATED_POP_NAME;
                 filter_measure="flow.cell.is.aggregate", filter_fun="gt", filter_values=0)
    @test created == ["/qc/" * AGGREGATED_POP_NAME]
    p = pop_at(load_pop_map(img; value_name="B", pop_type="flow"), "/qc/" * AGGREGATED_POP_NAME)
    @test p.filter_measure == "flow.cell.is.aggregate" && p.filter_fun == "gt" && p.filter_values == 0
    @test pop_category(p.pop_type, p.path) == "aggregated" && !is_track_pop(p.pop_type, p.path)

    # idempotent: re-running REDEFINES (a probability cutoff — measure-agnostic), never duplicates
    ensure_filter_pop!(img, "flow", "B", ["/qc"], AGGREGATED_POP_NAME;
                 filter_measure="flow.cell.aggregate.score", filter_fun="gte", filter_values=0.5)
    m3 = load_pop_map(img; value_name="B", pop_type="flow")
    @test count(pp -> endswith(pp, AGGREGATED_POP_NAME), pop_paths(m3)) == 1
    @test pop_at(m3, "/qc/" * AGGREGATED_POP_NAME).filter_fun == "gte"

    # a parent absent from the map is skipped; the all-cells root ("/") maps to ROOT and is created
    created2 = ensure_filter_pop!(img, "flow", "B", ["/nonexistent", "/"], AGGREGATED_POP_NAME;
                 filter_measure="flow.cell.is.aggregate", filter_fun="gt", filter_values=0)
    @test created2 == ["/" * AGGREGATED_POP_NAME]
    rm(td; recursive=true)
end

# ── Mixed-type pop resolution: resolve_pop_type / pop_namespace / pop_df_multi (module pickers) ──
@testset "resolve_pop_type + pop_namespace (mixed-type pickers)" begin
    td = mktempdir()
    img = CciaImage(; dir=td)
    # one stored map per type on disk (routed by m.pop_type), all under value_name "B"
    fm = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(fm, "qc"; gate=RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0)); save_pop_map!(fm, img)
    cm = PopulationMap(pop_type="clust", value_name="B")
    add_pop!(cm, "myeloid"; filter_measure="clusters.default", filter_fun="in", filter_values=[1, 2]); save_pop_map!(cm, img)
    rmp = PopulationMap(pop_type="region", value_name="B")
    add_pop!(rmp, "r0"; filter_measure="regions.default", filter_fun="in", filter_values=[0]); save_pop_map!(rmp, img)
    tm = PopulationMap(pop_type="track", value_name="B")
    add_pop!(tm, "TEST"; filter_measure="live.track.speed", filter_fun="gt", filter_values=5); save_pop_map!(tm, img)

    # each path resolves to the map that CONTAINS it; _tracked → live; root/unknown → flow
    @test resolve_pop_type(img, "B", "/qc")           == "flow"
    @test resolve_pop_type(img, "B", "/myeloid")      == "clust"
    @test resolve_pop_type(img, "B", "/r0")           == "region"
    @test resolve_pop_type(img, "B", "/TEST")         == "track"
    @test resolve_pop_type(img, "B", "/qc/_tracked")  == "live"   # derived leaf, not stored
    @test resolve_pop_type(img, "B", "/")             == "flow"   # all-cells root → cells
    @test resolve_pop_type(img, "B", "/nonexistent")  == "flow"   # unknown → default (empty downstream)

    # _split_pop_ref: prefix names the value_name; leading-slash/root stays in default
    @test Cecelia._split_pop_ref("B/qc", "default") == ("B", "/qc")
    @test Cecelia._split_pop_ref("/qc", "B")        == ("B", "/qc")
    @test Cecelia._split_pop_ref("qc", "B")         == ("B", "/qc")

    # pops_value_name: the spatial tasks derive their segmentation from the pick (no dropdown).
    # Value_name comes from the first ref's prefix; the all-cells root "B/" carries it too.
    @test pops_value_name(["B/qc"])              == "B"
    @test pops_value_name(["B/qc", "B/myeloid"]) == "B"       # single-segmentation set
    @test pops_value_name(["B/"])                == "B"       # "… all" root pick
    @test pops_value_name(String[])              == "default" # empty → default
    @test pops_value_name(String[]; default="C") == "C"
    # distinct value_names shouldn't reach a single-segmentation picker → warn, first still wins
    @test (@test_logs (:warn,) match_mode=:any pops_value_name(["B/qc", "T/qc"])) == "B"

    # grouping by discovered type preserves first-appearance order
    grp = Cecelia._group_pops_by_type(img, ["/qc", "/myeloid", "/qc/_tracked", "/r0"], "B")
    @test grp == ["flow" => ["/qc"], "clust" => ["/myeloid"], "live" => ["/qc/_tracked"], "region" => ["/r0"]]

    # pop_namespace: any TRACKED source → live, else flow (cluster/region are just cell selections)
    @test pop_namespace(img, ["/qc"]; value_name="B")            == "flow"
    @test pop_namespace(img, ["/r0"]; value_name="B")            == "flow"
    @test pop_namespace(img, ["/myeloid"]; value_name="B")       == "flow"
    @test pop_namespace(img, ["/qc/_tracked"]; value_name="B")   == "live"
    @test pop_namespace(img, ["/TEST"]; value_name="B")          == "live"   # track pop → live namespace
    @test pop_namespace(img, ["/qc", "B/TEST"]; value_name="B")  == "live"   # any tracked → live
    @test pop_namespace(img, String[])                           == "flow"

    # name-uniqueness guard (cross pop_type): a name already used by ANOTHER type in the segmentation
    @test pop_name_conflict(img, "B", "/qc"; pop_type="region")     == "flow"    # flow gate qc exists
    @test pop_name_conflict(img, "B", "/myeloid"; pop_type="flow")  == "clust"   # clust myeloid exists
    @test pop_name_conflict(img, "B", "/TEST"; pop_type="flow")     == "track"
    @test pop_name_conflict(img, "B", "/qc"; pop_type="flow")       === nothing   # same type → not a conflict
    @test pop_name_conflict(img, "B", "/qc"; pop_type="live")       === nothing   # live shares the flow map
    @test pop_name_conflict(img, "B", "/brandnew"; pop_type="flow") === nothing   # unused name → ok
    @test pop_name_conflict(img, "B", "/"; pop_type="clust")        === nothing   # root exempt

    # same-name guard: "/qc" now exists in BOTH the flow map (a gate) and the region map — an
    # ambiguous path. resolve by priority (flow first) AND @warn, never a silent mis-resolve.
    add_pop!(rmp, "qc"; filter_measure="regions.default", filter_fun="in", filter_values=[1]); save_pop_map!(rmp, img)
    @test (@test_logs (:warn,) match_mode=:any resolve_pop_type(img, "B", "/qc")) == "flow"
    rm(td; recursive=true)
end

# ── pop_df_multi membership over real H5AD (equals per-type pop_df; unknown refs skip cleanly) ──
@testset "pop_df_multi integration (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "pop_df_multi integration (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        full = label_props(img; value_name="B") |> select_cols(["mean_intensity_0"]) |> as_df
        thr = sort(full.mean_intensity_0)[cld(nrow(full), 2)]
        truth = sum(full.mean_intensity_0 .>= thr)
        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "pos"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1", thr, 1e12, -1e12, 1e12))
        save_pop_map!(m, img)

        # a flow gate resolves + returns the SAME cells as an explicit pop_df("flow", …)
        direct = pop_df(img, "flow", ["/pos"]; value_name="B", pop_cols=["area"])
        multi  = pop_df_multi(img, ["/pos"]; value_name="B", pop_cols=["area"])
        @test nrow(multi) == truth == nrow(direct)
        @test unique(multi.pop) == ["/pos"]

        # an unknown-type ref (no map contains it) resolves to flow and is skipped → still just /pos
        mixed = pop_df_multi(img, ["/pos", "/nonexistent"]; value_name="B", pop_cols=["area"])
        @test nrow(mixed) == truth

        # dedup: /pos ∪ all-cells root collapses to one row per cell (root = every cell)
        pooled = pop_df_multi(img, ["/pos", "/"]; value_name="B", pop_cols=["area"])
        @test nrow(pooled) == nrow(full)
        @test length(unique(pooled.label)) == nrow(pooled)   # no duplicated cell rows

        # restrict_to guard: keep only the operated-on segmentation's cells (single-seg tasks)
        @test nrow(pop_df_multi(img, ["/pos"]; value_name="B", pop_cols=["area"], restrict_to="B")) == truth
        @test nrow(pop_df_multi(img, ["/pos"]; value_name="B", pop_cols=["area"], restrict_to="OTHER")) == 0

        # END-TO-END on real tracked data (the user's ask): the TRACKED subset of a gate resolves
        # to CELLS via the mixed picker — impossible before (cells picker hid _tracked; the consumer
        # assumed flow and got nothing). B.h5ad carries track_id.
        tid = label_props(img; value_name="B") |> v -> select_cols(v, ["track_id"]) |> as_df
        tracked = Set(tid.label[[x isa Real && isfinite(x) && x > 0 for x in tid.track_id]])
        gate = pop_df(img, "flow", ["/pos"]; value_name="B")
        expected_tracked = count(l -> l in tracked, gate.label)
        @test 0 < expected_tracked < nrow(gate)               # a genuine tracked subset of the gate
        trkd = pop_df_multi(img, ["/pos/_tracked"]; value_name="B", restrict_to="B")
        @test nrow(trkd) == expected_tracked                  # tracked cells now resolve
        @test resolve_pop_type(img, "B", "/pos/_tracked") == "live"
        @test pop_namespace(img, ["/pos/_tracked"]; value_name="B") == "live"
        rm(td; recursive=true)
    end
end

# ── Cluster-pop auto-share across co-clustered segmentations (CLUSTER_POOLING_PLAN.md) ─────
@testset "cluster pop auto-share (co-clustered value_names)" begin
    td = mktempdir()
    lpdir = joinpath(td, "labelProps"); mkpath(lpdir)
    # B & T were clustered together (both track sidecars carry suffix "movement"); C was not.
    for vn in ("B", "T")
        open(joinpath(lpdir, "$(vn)__tracks.clustfeatures.json"), "w") do f
            JSON3.write(f, Dict("movement" => Dict("features" => ["live.track.speed"], "partOf" => ["u1"])))
        end
    end
    # named trackclust pops authored ONLY under B (filter the shared clusters.movement column)
    bm = PopulationMap(pop_type="trackclust", value_name="B")
    add_pop!(bm, "Directed"; filter_measure="clusters.movement", filter_fun="in", filter_values=[3], colour="#c061cb")
    add_pop!(bm, "Scanning"; filter_measure="clusters.movement", filter_fun="in", filter_values=[0], colour="#62a0ea")
    save_pop_map!(bm, td)

    img = CciaImage(; dir=td)
    img.label_props = Dict("B" => "B.h5ad", "T" => "T.h5ad", "C" => "C.h5ad", "_active" => "B")

    # co-clustered segmentations for run "movement" (track granularity) = B and T (not C)
    @test Set(Cecelia.co_clustered_value_names(img, "movement"; granularity=:track)) == Set(["B", "T"])

    # B has its OWN sidecar → loaded verbatim
    mb = load_pop_map(img; value_name="B", pop_type="trackclust")
    @test Set(keys(mb.pops)) == Set(["/Directed", "/Scanning"]) && mb.value_name == "B"

    # T has NO sidecar but IS co-clustered → BORROWS B's named pops, relabeled to T so
    # membership resolves over T's own track table
    mt = load_pop_map(img; value_name="T", pop_type="trackclust")
    @test Set(keys(mt.pops)) == Set(["/Directed", "/Scanning"])
    @test mt.value_name == "T" && all(p.value_name == "T" for p in values(mt.pops))
    @test mt.pops["/Directed"].filter_measure == "clusters.movement"

    # C was NOT part of the run (no clustfeatures suffix) → no borrow (empty map)
    mc = load_pop_map(img; value_name="C", pop_type="trackclust")
    @test isempty(mc.pops)

    # BARE cluster-pop ref expands across ALL co-clustered segmentations (R popDT parity)
    @test Set(Cecelia._expand_cluster_pops(img, ["/Directed"], "trackclust", "B")) ==
          Set(["B/Directed", "T/Directed"])
    # explicit value_name-prefixed ref is untouched (single-segmentation request still works)
    @test Cecelia._expand_cluster_pops(img, ["T/Scanning"], "trackclust", "B") == ["T/Scanning"]
    # unknown pop → left as-is (falls back to default_vn downstream); non-cluster type → no-op
    @test Cecelia._expand_cluster_pops(img, ["/Nope"], "trackclust", "B") == ["/Nope"]
    @test Cecelia._expand_cluster_pops(img, ["/x"], "flow", "B") == ["/x"]

    # per-cluster heatmap detection: a matrix over a clusters.{suffix} column pools co-clustered vns
    @test Cecelia._cluster_matrix_suffix("matrix", "clusters.movement") == "movement"
    @test Cecelia._cluster_matrix_suffix("matrix", "pop") === nothing      # per-population mode
    @test Cecelia._cluster_matrix_suffix("boxplot", "clusters.movement") === nothing
end

# ── Gating engine: recompute, membership, filtered (tracked) pops ─────────
@testset "recompute! + cells_in_pop" begin
    df = DataFrame(label=[1, 2, 3, 4, 5], x=[1.0, 6, 6, 9, 9], track_id=[0, 5, 9, 0, 7])

    # flow: parent (x≥0) ∩ child (x≥5)
    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "p"; gate=RectangleGate("x", "x", 0.0, 1e9, -1e9, 1e9))
    add_pop!(m, "c"; parent="/p", gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
    recompute!(m, _ -> df)
    @test cells_in_pop(m, "/p") == [1, 2, 3, 4, 5]
    @test cells_in_pop(m, "/p/c") == [2, 3, 4, 5]         # x≥5
    @test pop_stats(m, "/p/c").pct_parent == 80.0

    # filtered (tracked) pop: track_id > 0
    mt = PopulationMap(pop_type="live", value_name="T")
    add_pop!(mt, "tracked"; filter_measure="track_id", filter_fun="gt", filter_values=0)
    recompute!(mt, _ -> df)
    @test cells_in_pop(mt, "/tracked") == [2, 3, 5]

    @test_throws ErrorException cells_in_pop(PopulationMap(), "/x")  # not recomputed
end

# ── explicit-label membership (napari selection) + transient not persisted ─
@testset "explicit-label (napari) membership" begin
    df = DataFrame(label=[1, 2, 3, 4, 5], x=[1.0, 6, 6, 9, 9])

    m = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m, "p"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))   # x≥5 → 2,3,4,5
    # transient napari selection of labels {2,4,9} ∩ parent(x≥5) → {2,4}
    add_pop!(m, "napari"; parent="/p", explicit_labels=[2, 4, 9],
             colour="#22d3ee", transient=true)
    recompute!(m, _ -> df)
    @test cells_in_pop(m, "/p/napari") == [2, 4]              # 9 absent, 3/5 not selected

    # root-level selection (no gate parent): exactly the labels present
    m2 = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(m2, "sel"; explicit_labels=[1, 3, 99], transient=true)
    recompute!(m2, _ -> df)
    @test cells_in_pop(m2, "/sel") == [1, 3]

    # transient pops are NOT written to disk, but stay in the in-memory/broadcast tree
    td = mktempdir()
    save_pop_map!(m, td)
    reloaded = load_pop_map(td, "B")
    @test !has_pop(reloaded, "/p/napari")                    # dropped on persist
    @test has_pop(reloaded, "/p")                            # real pop kept
    @test "transient" in keys(Cecelia._node_dict(m, "/p/napari"))  # flagged in broadcast tree

    # explicit-label pops carry a membership signature in the broadcast tree (no gate/filter
    # to diff on) so the client refreshes plots when the selection's cell set changes.
    nd1 = Cecelia._node_dict(m, "/p/napari")
    @test haskey(nd1, "membership_sig")
    del_pop!(m, "/p/napari")
    add_pop!(m, "napari"; parent="/p", explicit_labels=[2, 9], colour="#22d3ee", transient=true)
    @test Cecelia._node_dict(m, "/p/napari")["membership_sig"] != nd1["membership_sig"]
end

# ── scale_centroids!: THE one pixel→µm conversion (pure, no fixture needed) ────────
# The Python mirror (`label_props_utils.scale_centroids`) is asserted on the SAME numbers in
# python/cecelia/tests/test_centroid_migrate.py, so the two languages cannot drift on which axis
# scales by what.
@testset "scale_centroids! maps each axis by name" begin
    phys = [3.0, 0.5, 0.25]        # [sz, sy, sx]
    mk(; with_z=true) = begin
        d = DataFrame("label" => [1, 2], "centroid_x" => [100.0, 200.0],
                      "centroid_y" => [10.0, 20.0], "centroid_t" => [0.0, 1.0],
                      "area" => [5.0, 6.0])
        with_z && (d[!, "centroid_z"] = [4.0, 8.0])
        d
    end

    d = scale_centroids!(mk(), phys)
    @test d.centroid_x == [25.0, 50.0]      # ×sx
    @test d.centroid_y == [5.0, 10.0]       # ×sy
    @test d.centroid_z == [12.0, 24.0]      # ×sz
    # time stays a FRAME index on purpose, and non-centroid columns are untouched
    @test d.centroid_t == [0.0, 1.0]
    @test d.area == [5.0, 6.0]
    @test d.label == [1, 2]

    # 2D: with no centroid_z, x must STILL use sx. A tail-aligned implementation would give x the
    # sy value here — the silent 2D bug the by-name contract exists to prevent.
    d2 = scale_centroids!(mk(with_z=false), phys)
    @test d2.centroid_x == [25.0, 50.0]
    @test d2.centroid_y == [5.0, 10.0]
    @test !("centroid_z" in names(d2))

    # a frame with no centroid columns is a no-op, not an error
    plain = DataFrame("label" => [1], "area" => [5.0])
    @test scale_centroids!(copy(plain), phys) == plain

    # the CciaImage form reads the sizes off `meta` — same numbers, one axis at a time
    with_meta(m) = (i = CciaImage(; uid="c1", name="cal", dir=""); i.meta = Dict{String,Any}(m); i)
    let img = with_meta(Dict("PhysicalSizeZ" => "3.0", "PhysicalSizeY" => "0.5",
                             "PhysicalSizeX" => "0.25"))
        d3 = scale_centroids!(mk(), img)
        @test d3.centroid_x == [25.0, 50.0]
        @test d3.centroid_z == [12.0, 24.0]
    end
    # uncalibrated → img_physical_sizes defaults to 1.0, so the frame comes back unchanged
    let img = with_meta(Dict{String,Any}())
        @test scale_centroids!(mk(), img).centroid_x == [100.0, 200.0]
        @test !img_is_calibrated(img)
    end
    # `_pop_df_finish` is the single conversion point every pop_df branch returns through.
    let cal = with_meta(Dict("PhysicalSizeZ" => "3.0", "PhysicalSizeY" => "0.5",
                             "PhysicalSizeX" => "0.25"))
        # :pixel leaves the values alone; :physical converts
        @test Cecelia._pop_df_finish(mk(), cal, :pixel).centroid_x == [100.0, 200.0]
        @test Cecelia._pop_df_finish(mk(), cal, :physical).centroid_x == [25.0, 50.0]
        @test Cecelia._pop_df_finish(mk(), cal, false).centroid_x == [100.0, 200.0]
        # a frame with NO cell coordinates (a track-grained or branch frame) warns rather than
        # silently ignoring the argument
        trackish = DataFrame("label" => [1], "live.track.speed" => [3.0])
        @test_logs (:warn, r"no centroid_x") Cecelia._pop_df_finish(trackish, cal, :physical)
        # …and an uncalibrated image warns instead of relabelling pixels as µm
        @test_logs (:warn, r"no physical pixel size") Cecelia._pop_df_finish(
            mk(), with_meta(Dict{String,Any}()), :physical)
    end

    # calibrated: X/Y present and > 0 (Z not required — a 2D image legitimately has none)
    @test img_is_calibrated(with_meta(Dict("PhysicalSizeX" => "0.25", "PhysicalSizeY" => "0.5")))
    @test !img_is_calibrated(with_meta(Dict("PhysicalSizeX" => "0.25")))
    @test !img_is_calibrated(with_meta(Dict("PhysicalSizeX" => "0", "PhysicalSizeY" => "0.5")))
    @test !img_is_calibrated(with_meta(Dict("PhysicalSizeX" => "", "PhysicalSizeY" => "0.5")))
end

# ── spatial gates in µm: the stamp, the eval-time scale, the portability predicate ────────
# docs/todo/SPATIAL_GATE_UNITS_PLAN.md. A position gate is stored in µm and compared against data
# scaled with THIS image's µm/px, so one gate means one physical region on every image.
@testset "spatial gate units" begin
    # ── the stamp round-trips, and a legacy file (no stamp) reads as px ──
    @test PopulationMap().spatial_unit == SPATIAL_UNIT_PX
    let m = PopulationMap(; spatial_unit=SPATIAL_UNIT_UM)
        @test to_tree(m)["spatial_unit"] == SPATIAL_UNIT_UM
        @test from_tree(to_tree(m)).spatial_unit == SPATIAL_UNIT_UM
    end
    # no stamp ⇒ px: every gating file written before this change holds pixel coordinates and must
    # keep evaluating as pixels, so the migration is optional rather than required
    @test from_tree(Dict("pop_type" => "flow", "value_name" => "B",
                         "populations" => [])).spatial_unit == SPATIAL_UNIT_PX

    # ── a map ADOPTS µm when there is nothing to reinterpret (this replaces the migration) ──
    # The stamp only constrains a file that already holds position coordinates, so a calibrated image
    # upgrades an intensity-only map — new or long-standing — and the first position gate anyone draws is
    # already physical. A map that DOES carry a position gate keeps its unit, or its numbers would move.
    let td = mktempdir()
        img = CciaImage(uid="CAL", dir=td)
        img.meta = Dict{String,Any}("PhysicalSizeX" => "0.25", "PhysicalSizeY" => "0.5")
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # brand-new map on a calibrated image → µm, with that image's sizes attached
        m0 = load_pop_map(img; value_name="B", pop_type="flow")
        @test m0.spatial_unit == SPATIAL_UNIT_UM
        @test m0.physical_sizes == [1.0, 0.5, 0.25]

        # an EXISTING intensity-only file (stamped px, as every pre-change file is) is upgraded
        mpx = PopulationMap(; pop_type="flow", value_name="B", spatial_unit=SPATIAL_UNIT_PX)
        add_pop!(mpx, "hi"; gate=RectangleGate("area", "mean_intensity_0", 0., 1., 0., 1.))
        save_pop_map!(mpx, td)
        @test load_pop_map(img; value_name="B", pop_type="flow").spatial_unit == SPATIAL_UNIT_UM

        # …but one that ALREADY holds a position gate keeps px — re-stamping would move its coordinates
        msp = PopulationMap(; pop_type="flow", value_name="B", spatial_unit=SPATIAL_UNIT_PX)
        add_pop!(msp, "pos"; gate=RectangleGate("centroid_x", "centroid_y", 0., 500., 0., 400.))
        save_pop_map!(msp, td)
        @test load_pop_map(img; value_name="B", pop_type="flow").spatial_unit == SPATIAL_UNIT_PX

        # an UNCALIBRATED image adopts nothing and carries no sizes (no µm to convert to)
        u = CciaImage(uid="UNCAL", dir=mktempdir())
        u.label_props["B"] = "B.h5ad"; u.label_props["_active"] = "B"
        let mu = load_pop_map(u; value_name="B", pop_type="flow")
            @test mu.spatial_unit == SPATIAL_UNIT_PX
            @test mu.physical_sizes === nothing
        end
    end

    # ── is_spatial_axis: centroid_t is NOT spatial (a frame index carries no pixel size) ──
    @test all(is_spatial_axis, ["centroid_x", "centroid_y", "centroid_z"])
    @test !any(is_spatial_axis, ["centroid_t", "area", "mean_intensity_0", "live.cell.speed"])

    # ── recompute! scales the DATA to the gate's unit, in one place ──
    # 3 cells at x = 100/200/300 px; sx = 0.5 µm/px ⇒ 50/100/150 µm. A gate over 40–110 µm selects
    # the first two; the SAME numbers read as pixels select only the first.
    cells = DataFrame("label" => [1, 2, 3], "centroid_x" => [100.0, 200.0, 300.0],
                      "centroid_y" => [0.0, 0.0, 0.0])
    fetch = _ -> cells
    mk(unit, sizes) = begin
        m = PopulationMap(; pop_type="flow", value_name="B", spatial_unit=unit, physical_sizes=sizes)
        add_pop!(m, "sel"; gate=RectangleGate("centroid_x", "centroid_y", 40.0, 110.0, -1.0, 1.0))
        recompute!(m, fetch)
        m
    end
    @test sort(collect(cells_in_pop(mk(SPATIAL_UNIT_UM, [1.0, 0.5, 0.5]), "/sel"))) == [1, 2]
    @test sort(collect(cells_in_pop(mk(SPATIAL_UNIT_PX, [1.0, 0.5, 0.5]), "/sel"))) == [1]
    # a µm map on an UNCALIBRATED image (no sizes) falls back to pixels rather than inventing a scale
    @test sort(collect(cells_in_pop(mk(SPATIAL_UNIT_UM, nothing), "/sel"))) == [1]
    # the caller's frame is never mutated by the scaling (recompute! copies)
    @test cells.centroid_x == [100.0, 200.0, 300.0]
    # an intensity-only gate is untouched by any of this
    let m = PopulationMap(; spatial_unit=SPATIAL_UNIT_UM, physical_sizes=[1.0, 0.5, 0.5])
        add_pop!(m, "hi"; gate=RectangleGate("area", "perim", 5.0, 15.0, 0.0, 100.0))
        recompute!(m, _ -> DataFrame("label" => [1, 2, 3], "area" => [1.0, 10.0, 20.0],
                                     "perim" => [1.0, 1.0, 1.0]))
        @test sort(collect(cells_in_pop(m, "/hi"))) == [2]
    end

    # ── has_spatial_gate: which strategies need the target image to be calibrated to copy ──
    g(f) = (m = PopulationMap(); f(m); m)
    @test has_spatial_gate(g(m -> add_pop!(m, "s";
        gate=RectangleGate("centroid_x", "centroid_y", 0., 1., 0., 1.))))
    @test has_spatial_gate(g(m -> add_pop!(m, "m";      # y axis alone is enough
        gate=RectangleGate("area", "centroid_z", 0., 1., 0., 1.))))
    @test has_spatial_gate(g(m -> add_pop!(m, "f";      # a filter on a position measure counts
        filter_measure="centroid_x", filter_fun="gt", filter_values=10)))
    @test has_spatial_gate(g(m -> add_pop!(m, "c"; filter_conditions=[
        (; measure="area", fun="gt", values=1), (; measure="centroid_y", fun="lt", values=99)])))
    @test !has_spatial_gate(g(m -> add_pop!(m, "i";
        gate=RectangleGate("area", "mean_intensity_0", 0., 1., 0., 1.))))
    @test !has_spatial_gate(g(m -> add_pop!(m, "t";     # centroid_t is not spatial
        gate=RectangleGate("centroid_t", "area", 0., 1., 0., 1.))))
    @test !has_spatial_gate(g(m -> add_pop!(m, "n";
        filter_measure="flow.cell.is.aggregate", filter_fun="gt", filter_values=0)))
    @test !has_spatial_gate(PopulationMap())
end

# ── pop_df(centroids=…): coordinates without naming the columns ────────────
# `pop_df` is the primary accessor for population data (docs/POPULATION.md) — a caller should never
# have to know which centroid columns exist (they differ per segmentation) or convert units by hand.
@testset "pop_df centroids (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "pop_df centroids (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        cp(fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad"),
           joinpath(td, "labelProps", "B__tracks.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
        cent = ["centroid_z", "centroid_y", "centroid_x", "centroid_t"]

        # a narrowed read (pop_cols) does NOT carry coordinates …
        base = pop_df(img, "labels", String[]; value_name="B", pop_cols=["area"])
        @test !any(c -> c in names(base), cent)
        # … until `centroids` widens the PUSHDOWN to include them
        px = pop_df(img, "labels", String[]; value_name="B", pop_cols=["area"], centroids=:pixel)
        @test all(c -> c in names(px), cent)
        @test "area" in names(px)                       # the requested column is still there
        @test nrow(px) == nrow(base)

        # this fixture is UNCALIBRATED, so :physical must return PIXELS rather than relabel them µm
        @test !img_is_calibrated(img)
        ph = pop_df(img, "labels", String[]; value_name="B", pop_cols=["area"], centroids=:physical)
        @test ph.centroid_x == px.centroid_x

        # :pixel and :physical share ONE cached read (the cache holds the frame as read, in pixels,
        # and the unit conversion happens on the returned copy) — so a :physical call cannot leave
        # scaled values behind for a later :pixel caller.
        @test pop_df(img, "labels", String[]; value_name="B", pop_cols=["area"],
                     centroids=:pixel).centroid_x == px.centroid_x

        # …and `false` is a DIFFERENT read (different columns), so it must not share their entry
        @test !("centroid_x" in names(pop_df(img, "labels", String[]; value_name="B",
                                             pop_cols=["area"])))

        # the no-columns read already returns coordinates, so `centroids` changes nothing there
        wide = pop_df(img, "labels", String[]; value_name="B", centroids=:pixel)
        @test all(c -> c in names(wide), cent)

        # the keyword is validated, and is advertised on the public method
        @test_throws ErrorException pop_df(img, "labels", String[]; value_name="B", centroids=:um)
        @test :centroids in Base.kwarg_decl(
            only(methods(pop_df, (CciaImage, AbstractString, Any))))
    end
end

# ── pop_df: pooling across value_names + dedup to most-specific pop ────────
@testset "pop_df pooling + dedup" begin
    dfA = DataFrame(label=[1, 2, 3], x=[1.0, 6.0, 9.0])
    dfB = DataFrame(label=[10, 11], x=[7.0, 2.0])
    mA = PopulationMap(pop_type="flow", value_name="A")
    add_pop!(mA, "p"; gate=RectangleGate("x", "x", -1e9, 1e9, -1e9, 1e9))
    add_pop!(mA, "c"; parent="/p", gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
    mB = PopulationMap(pop_type="flow", value_name="B")
    add_pop!(mB, "hi"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
    maps = Dict("A" => mA, "B" => mB)
    load_map = vn -> maps[vn]
    fetch = (vn, _) -> (vn == "A" ? dfA : dfB)

    # request /p and /p/c from A, and hi from B (prefixed)
    res = Cecelia._pop_df(load_map, fetch, "flow", ["/p", "/p/c", "B/hi"];
                          default_vn="A", pop_cols=["x"], unique_labels=true)
    @test Set(unique(res.value_name)) == Set(["A", "B"])      # pooled across value_names
    # dedup: label 2 & 3 are in both /p and /p/c → assigned the most specific (/p/c)
    getpop(l, vn) = only(res[(res.label .== l) .& (res.value_name .== vn), :pop])
    @test getpop(1, "A") == "/p"
    @test getpop(2, "A") == "/p/c"
    @test getpop(3, "A") == "/p/c"
    @test getpop(10, "B") == "/hi"                            # x=7 ≥ 5
    @test nrow(res[res.value_name .== "B", :]) == 1           # label 11 (x=2) excluded
end

# ── pop_df: drop_na drops NA/NaN cells in requested pop_cols ──────────────
@testset "pop_df drop_na" begin
    # NaN is in a measure column (x), NOT the gate column (g) — a NaN gate value would
    # fail the gate comparison and never enter the result, masking what drop_na does.
    df = DataFrame(label=[1, 2, 3], g=[0.0, 0.0, 0.0], x=[1.0, NaN, 9.0])
    m = PopulationMap(pop_type="flow", value_name="A")
    add_pop!(m, "p"; gate=RectangleGate("g", "g", -1e9, 1e9, -1e9, 1e9))  # all pass
    load_map = _ -> m
    fetch = (_, _) -> df
    keep = Cecelia._pop_df(load_map, fetch, "flow", ["/p"]; default_vn="A", pop_cols=["x"])
    @test nrow(keep) == 3                                     # NaN row kept by default
    dropped = Cecelia._pop_df(load_map, fetch, "flow", ["/p"];
                              default_vn="A", pop_cols=["x"], drop_na=true)
    @test sort(dropped.label) == [1, 3]                       # label 2 (x=NaN) dropped
end

# ── pop_df: track_id joins the dedup key when present (most-specific pop still wins) ──
@testset "pop_df track_id dedup key" begin
    df = DataFrame(label=[1, 2], x=[9.0, 9.0], track_id=[10.0, 20.0])
    m = PopulationMap(pop_type="flow", value_name="A")
    add_pop!(m, "p"; gate=RectangleGate("x", "x", -1e9, 1e9, -1e9, 1e9))
    add_pop!(m, "c"; parent="/p", gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
    load_map = _ -> m
    fetch = (_, _) -> df
    res = Cecelia._pop_df(load_map, fetch, "flow", ["/p", "/p/c"];
                          default_vn="A", pop_cols=["x"], unique_labels=true)
    @test "track_id" in names(res)                            # track_id carried through
    @test nrow(res) == 2                                      # still one row per cell
    @test Set(res.pop) == Set(["/p/c"])                       # most-specific pop wins
end

# ── pop_df: derived live "_tracked" pop (track_id>0 filter on a gated parent) ──
@testset "pop_df live _tracked (derived filter)" begin
    # label4 fails the qc gate (x=1); label2 is in qc but untracked (track_id=NaN).
    df = DataFrame(label=[1, 2, 3, 4], x=[9.0, 9.0, 9.0, 1.0], track_id=[10.0, NaN, 20.0, 30.0])
    m = PopulationMap(pop_type="flow", value_name="A")
    add_pop!(m, "qc"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))   # qc = {1,2,3}
    # "_tracked" is derived, not stored — injecting it adds a filtered child of /qc
    Cecelia._inject_derived_pops!(m, ["/qc/_tracked"], "live")
    @test has_pop(m, "/qc/_tracked")
    @test m.pops["/qc/_tracked"].filter_measure == "track_id"
    load_map = _ -> m
    fetch = (_, _) -> df
    res = Cecelia._pop_df(load_map, fetch, "live", ["/qc/_tracked"];
                          default_vn="A", pop_cols=["track_id"])
    @test sort(res.label) == [1, 3]                # qc ∩ track_id>0 (label2 NaN, label4 not in qc)
    @test unique(res.pop) == ["/qc/_tracked"]
    # a derived pop is only injected under its registered pop_type (foreign type → skip)
    m2 = PopulationMap(pop_type="flow", value_name="A")
    add_pop!(m2, "qc"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
    Cecelia._inject_derived_pops!(m2, ["/qc/_tracked"], "clust")  # _tracked is a `live` spec
    @test !has_pop(m2, "/qc/_tracked")
    # an unknown `_`-name is not derived either
    Cecelia._inject_derived_pops!(m2, ["/qc/_nope"], "live")
    @test !has_pop(m2, "/qc/_nope")
end

# ── reserved derived-pop namespace: `_`-prefixed names can't be hand-drawn gates ──
@testset "reserved pop names (_ prefix)" begin
    @test is_reserved_pop_name("_tracked")
    @test is_reserved_pop_name("_anything")
    @test !is_reserved_pop_name("qc")
    m = PopulationMap(pop_type="flow", value_name="A")
    add_pop!(m, "qc"; gate=RectangleGate("x", "x", 5.0, 1e9, -1e9, 1e9))
    # a hand-drawn gate may not take a reserved name
    @test_throws ErrorException add_pop!(m, "_tracked"; parent="/qc",
                                         gate=RectangleGate("x", "x", 0.0, 1.0, 0.0, 1.0))
    @test_throws ErrorException rename_pop!(m, "/qc", "_qc")
    # the derived injection (reserved_ok) is allowed to create it
    add_pop!(m, "_tracked"; parent="/qc", filter_measure="track_id", filter_fun="gt",
             filter_values=0, transient=true, reserved_ok=true)
    @test has_pop(m, "/qc/_tracked")
    # round-trips through from_tree (reconstruction bypasses the guard)
    m3 = from_tree(to_tree(m; include_transient=true))
    @test has_pop(m3, "/qc/_tracked")
end

# ── pop_df: cache key folds in file mtimes → auto-invalidates on gate/h5ad change ──
@testset "pop_df cache auto-invalidation" begin
    td = mktempdir(); mkpath(joinpath(td, "gating")); mkpath(joinpath(td, "labelProps"))
    img = CciaImage(uid="X", dir=td)
    img.label_props["A"] = "A.h5ad"
    write(joinpath(td, "gating", "A.json"), "{}")
    write(joinpath(td, "labelProps", "A.h5ad"), "x")
    key() = Cecelia._pop_df_cache_key(img, "flow", "A", ["/qc"], nothing,
                                      false, true, true, false, false, :cell, String[], String[])
    k1 = key()
    sleep(0.05); touch(joinpath(td, "labelProps", "A.h5ad"))   # re-track rewrites h5ad
    k2 = key()
    @test k1 != k2
    sleep(0.05); touch(joinpath(td, "gating", "A.json"))        # gate edit rewrites map
    @test key() != k2
end

# ── pop_df: integration on real KDIeEm (gate eval over real H5AD) ─────────
@testset "pop_df integration (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "pop_df integration (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        full = label_props(img; value_name="B") |> select_cols(["mean_intensity_0"]) |> as_df
        thr = sort(full.mean_intensity_0)[cld(nrow(full), 2)] # ~median → discriminating
        truth = sum(full.mean_intensity_0 .>= thr)
        @test 0 < truth < nrow(full)                          # genuinely partial selection

        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "pos"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1",
                                              thr, 1e12, -1e12, 1e12))
        save_pop_map!(m, img)
        df = pop_df(img, "flow", ["/pos"]; value_name="B", pop_cols=["area", "mean_intensity_0"])
        @test nrow(df) == truth
        @test Set(names(df)) ⊇ Set(["label", "area", "mean_intensity_0", "pop", "value_name"])
        @test all(df.mean_intensity_0 .>= thr)                # every returned cell passes the gate
        @test unique(df.pop) == ["/pos"]

        # channel-name resolution: pop_df renames intensity cols to channel names by default,
        # raw_channel_names=true keeps the {measure}_intensity_{i} names (channel names are
        # stored under the default version, so value_name="B" falls back to it)
        set_channel_names!(img, ["CD4", "CD8", "CD3", "CD19"]; check_length=false)
        named = pop_df(img, "flow", ["/pos"]; value_name="B", include_x=true)
        @test "CD4" in names(named) && !("mean_intensity_0" in names(named))
        raw = pop_df(img, "flow", ["/pos"]; value_name="B", include_x=true, raw_channel_names=true)
        @test "mean_intensity_0" in names(raw) && !("CD4" in names(raw))

        # value_name=nothing resolves to the active segmentation (img.label_props _active="B")
        auto = pop_df(img, "flow", ["/pos"]; pop_cols=["area"])
        @test nrow(auto) == truth

        # cache: a request is stored under its signature key; flush_cache recomputes
        ck = Cecelia._pop_df_cache_key(img, "flow", "B", ["/pos"], ["area"],
                                       false, true, true, false, false, :cell, String[], String[])
        cached = pop_df(img, "flow", ["/pos"]; value_name="B", pop_cols=["area"])
        @test haskey(img._pop_df_cache, ck)
        fresh = pop_df(img, "flow", ["/pos"]; value_name="B", pop_cols=["area"], flush_cache=true)
        @test nrow(cached) == truth && nrow(fresh) == truth
    end
end

# ── resolve_pops: cached, display-ready per-pop membership (napari points overlay) ──
@testset "resolve_pops (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "resolve_pops (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        full = label_props(img; value_name="B") |> select_cols(["mean_intensity_0"]) |> as_df
        thr  = sort(full.mean_intensity_0)[cld(nrow(full), 2)]      # ~median → partial selection
        want = sort(Int.(full.label[full.mean_intensity_0 .>= thr]))

        m = PopulationMap(pop_type="flow", value_name="B")
        add_pop!(m, "pos"; gate=RectangleGate("mean_intensity_0", "mean_intensity_1",
                                              thr, 1e12, -1e12, 1e12), colour="#ef4444")
        save_pop_map!(m, img)

        layers = resolve_pops(img, "flow"; value_name="B")
        @test length(layers) == 1
        L = layers[1]
        @test L.path == "/pos" && L.name == "pos" && L.colour == "#ef4444"
        @test L.show === true && L.is_track === false
        @test sort(L.labels) == want                       # membership == the gate's cells

        # cached: a second call returns the SAME stored object (no recompute), keyed under poplayers:
        again = resolve_pops(img, "flow"; value_name="B")
        @test again === layers
        @test any(k -> startswith(k, "poplayers:"), keys(img._pop_df_cache))
    end
end

# ── Segmentation integrity (QC) plot data (KDIeEm, timecourse) ───────────────
# count per (image, timepoint) via group_by=temporal, + a per-timepoint measure distribution.
# See docs/todo/SEGMENTATION_QC_PLOT_PLAN.md.
# ── labels pop_type + count aggregation (segmentation QC data source, R parity) ──
@testset "labels pop_type + count (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "labels pop_type (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # `labels` = ALL measured cells, ungated, one "labels" pop; pops arg is ignored.
        all = pop_df(img, "labels", String[]; value_name="B", pop_cols=["area"])
        @test nrow(all) > 0
        @test Set(names(all)) ⊇ Set(["label", "area", "pop", "value_name"])
        @test unique(all.pop) == ["/labels"]
        @test unique(all.value_name) == ["B"]

        # cell count via the summary aggregator over labels — one series, value == total.
        whole = plot_summary_data(img, "labels", String[], "count"; value_name="B")
        @test whole["chartType"] == "count"
        @test length(whole["series"]) == 1
        @test whole["series"][1]["value"] == Float64(nrow(all))

        # count per timepoint (group_by the temporal column) → counts partition the total.
        byT = plot_summary_data(img, "labels", String[], "count"; value_name="B", group_by="centroid_t")
        @test byT["groupBy"] == "centroid_t"
        @test length(byT["series"]) > 1
        @test sum(s["value"] for s in byT["series"]) == Float64(nrow(all))

        # a morphology distribution over labels, per timepoint
        area = plot_summary_data(img, "labels", String[], "boxplot"; value_name="B",
                                 measure="area", group_by="centroid_t")
        @test area["measure"] == "area"
        @test length(area["series"]) == length(byT["series"])

        # targets signature (the path the summary canvas + whiteboard QC row use: series =
        # [(value_name, "labels")]) — count over the "labels" pop yields the same total.
        tg = plot_summary_data(img, "labels", [("B", "/labels")], "count")
        @test tg["chartType"] == "count"
        @test length(tg["series"]) == 1
        @test tg["series"][1]["value"] == Float64(nrow(all))
        @test tg["series"][1]["pop"] == "B/labels"    # manager-form id round-trips
    end
end

# ── track table: path/naming helpers + JSON-safety (pure, no fixture) ─────
@testset "track table helpers" begin
    td = mktempdir()
    img = CciaImage(uid="X", dir=td)
    # companion track table sits next to the cell labelProps with the __tracks suffix
    @test img_track_props_path(img, "A") == joinpath(td, "labelProps", "A__tracks.h5ad")
    @test endswith(img_track_props_path(img, "A"), "A__tracks.h5ad")
    @test img_track_props_path(img, "A") != img_label_props_path(img, "A")
    # reserved value-name suffix (a user segmentation may not end in __tracks)
    @test is_reserved_value_name("A__tracks")
    @test is_reserved_value_name("foo__tracks")
    @test !is_reserved_value_name("A")
    @test !is_reserved_value_name("A_tracks")        # single underscore is NOT reserved
    # JSON-safety: NaN floats → nothing (→ JSON null), everything else passes through
    @test Cecelia._jsonsafe(NaN) === nothing
    @test Cecelia._jsonsafe(1.5) === 1.5
    @test Cecelia._jsonsafe(3)   === 3
    # cache key folds granularity → :cell and :track differ; :track also folds the track mtime
    mkpath(joinpath(td, "gating")); mkpath(joinpath(td, "labelProps"))
    img.label_props["B"] = "B.h5ad"
    kc = Cecelia._pop_df_cache_key(img, "live", "B", ["/_tracked"], nothing,
                                   false, true, true, false, false, :cell, String[], String[])
    kt = Cecelia._pop_df_cache_key(img, "live", "B", ["/_tracked"], nothing,
                                   false, true, true, false, false, :track, String[], String[])
    @test kc != kt
end

# ── pop_df granularity=:track on real KDIeEm B (track table read path) ─────
@testset "pop_df :track (KDIeEm B)" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "pop_df :track (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # track table layout: measures in X/var, lineage in obs, one row per track_id
        tvars = col_names(label_props(img_track_props_path(img, "B")); data_type=:vars)
        @test "live.track.speed" in tvars && "live.track.meanTurningAngle" in tvars
        tobs = col_names(label_props(img_track_props_path(img, "B")); data_type=:obs)
        @test "track_root" in tobs

        # one row per track; carries measures + track_id + value_name
        tr = pop_df(img, "live", ["B/_tracked"]; granularity=:track)
        @test nrow(tr) > 0
        @test Set(names(tr)) ⊇ Set(["track_id", "live.track.speed", "pop", "value_name"])
        @test length(unique(tr.track_id)) == nrow(tr)          # exactly one point per track
        @test unique(tr.value_name) == ["B"]

        # :track row count == number of distinct tracks among the :cell members (expand↔collapse)
        ce = pop_df(img, "live", ["B/_tracked"]; granularity=:cell)
        ntracks_cells = length(unique(Int.(filter(!isnan, ce.track_id))))
        @test nrow(tr) == ntracks_cells
        @test nrow(ce) > nrow(tr)                               # many cells collapse to few tracks

        # pop_cols restriction returns just that measure (+ bookkeeping)
        sp = pop_df(img, "live", ["B/_tracked"]; granularity=:track,
                    pop_cols=["live.track.speed"])
        @test "live.track.speed" in names(sp) && !("live.track.duration" in names(sp))

        # cell_measures aggregation (the clustTracks path): a per-cell measure is aggregated to
        # per-track feature column(s) via track_props, alongside motility — this is what lets
        # clustTracks cluster `_tracked` pops on HMM/intensity features, not just motility.
        cvars = col_names(label_props(img; value_name="B"); data_type=:vars)
        if !isempty(cvars)
            base = String(first(cvars))                        # a real per-cell measure
            ag = pop_df(img, "live", ["B/_tracked"]; granularity=:track, cell_measures=[base])
            @test any(startswith(c, base * ".") for c in names(ag))   # aggregated → {base}.…
            @test nrow(ag) == nrow(tr)                          # same tracks, extra feature cols
            @test "live.track.speed" in names(ag)               # motility still present
            @test "num_cells" in names(ag)                      # per-track cell count (minTracklength)
        end
    end
end

# ── summary-plot aggregation (server-side; pop_df → bins / freq counts) ────
@testset "plot_summary_data (KDIeEm B)" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "plot_summary_data (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # histogram of per-track speed (granularity=:track): shared edges, every track binned
        h = plot_summary_data(img, "live", ["B/_tracked"], "histogram";
                              measure="live.track.speed", granularity=:track, nbins=20)
        @test h["chartType"] == "histogram"
        @test length(h["binEdges"]) == 21
        @test length(h["series"]) == 1
        ntr = nrow(pop_df(img, "live", ["B/_tracked"]; granularity=:track))
        @test sum(h["series"][1]["counts"]) == ntr

        # mock a categorical per-cell column, then frequency over the tracked cells
        cells = label_props(img; value_name="B") |> select_cols(["track_id"]) |> as_df
        mock = DataFrame("label" => cells.label,
                         "mock.state" => [Float64((l % 3) + 1) for l in cells.label])
        label_props(img_label_props_path(img, "B")) |> add_obs(mock) |> save!
        f = plot_summary_data(img, "live", ["B/_tracked"], "frequency";
                              measure="mock.state", granularity=:cell, normalize=:fraction)
        @test f["chartType"] == "frequency"
        @test Set(f["categories"]) ⊆ Set(["1", "2", "3"])
        @test length(f["series"]) == 1
        props = f["series"][1]["values"]
        @test all(0 .<= props .<= 1) && isapprox(sum(props), 1.0; atol=1e-9)

        # measureType auto-detection: continuous speed → numeric; integer code set → categorical
        @test h["measureType"] == "numeric"
        @test f["measureType"] == "categorical"

        # bar: mean + all three error metrics (sd, sem = sd/√n, ci95 ≈ 1.96·sem)
        br = plot_summary_data(img, "live", ["B/_tracked"], "bar";
                               measure="live.track.speed", granularity=:track)
        s = br["series"][1]
        @test Set(keys(s)) ⊇ Set(["value", "sd", "sem", "ci95", "n"])
        @test s["sem"] ≈ s["sd"] / sqrt(s["n"])
        @test s["ci95"] ≈ 1.96 * s["sem"]

        # raw points: boxplot with raw_points carries downsampled values (≤ cap); "points" chart
        bx = plot_summary_data(img, "live", ["B/_tracked"], "boxplot";
                               measure="live.track.speed", granularity=:track,
                               raw_points=true, max_points=10)
        @test length(bx["series"][1]["points"]) == min(ntr, 10)
        pts = plot_summary_data(img, "live", ["B/_tracked"], "points";
                                measure="live.track.speed", granularity=:track, max_points=10)
        @test pts["chartType"] == "points" && length(pts["series"][1]["points"]) == min(ntr, 10)
        # without raw_points, boxplot carries no payload of values
        bx0 = plot_summary_data(img, "live", ["B/_tracked"], "boxplot";
                                measure="live.track.speed", granularity=:track)
        @test isempty(bx0["series"][1]["points"])
    end
end

@testset "motion dimensionality detection (2D vs 3D)" begin
    # build a Track by cumulative-summing per-step [dz,dy,dx] vectors (coords are [z,y,x])
    mk(steps) = begin
        P = zeros(length(steps) + 1, 3)
        for k in 1:length(steps); P[k+1, :] = P[k, :] .+ steps[k]; end
        Cecelia.Track(1, Float64.(0:length(steps)), P)
    end
    dy(k) = 2 + 0.5 * cos(k / 3); dx(k) = 2 + 0.5 * sin(k / 3)   # persistent forward heading in xy
    # z either tracks the persistent xy motion (real 3D) or oscillates with large amplitude (jitter)
    real(rng)   = mk([[2 + 0.5*sin(k/3), dy(k), dx(k)] for k in rng])
    jitter(rng) = mk([[(-1.0)^k * 4.0,   dy(k), dx(k)] for k in rng])
    trks_real   = [real(t*20 : t*20+14)   for t in 1:8]
    trks_jitter = [jitter(t*20 : t*20+14) for t in 1:8]

    d3 = Cecelia._detect_motion_dims(trks_real)
    @test d3.dims == 3 && d3.z_used            # persistent z → keep 3D
    d2 = Cecelia._detect_motion_dims(trks_jitter)
    @test d2.dims == 2 && !d2.z_used           # oscillating/anti-persistent z → in-plane 2D
    @test d2.metrics["autocorrZ"] < 0          # jitter signature

    # a 2D-only track set (no z column) is trivially 2D
    P2 = zeros(12, 2); for k in 1:11; P2[k+1, :] = P2[k, :] .+ [dy(k), dx(k)]; end
    @test Cecelia._detect_motion_dims([Cecelia.Track(1, Float64.(0:11), P2)]).dims == 2
end

# ── uns reader: the anisotropy grid on the branch sidecar ─────────────────────────────────────
# The one thing worth pinning here is the DIMENSION REVERSAL. HDF5 stores C-order, Julia reads
# column-major, so a numpy (T, y, x, comp) array arrives as (comp, x, y, T) — every axis flipped,
# INCLUDING the two box axes, which are equal-length and would therefore swap silently. The
# fixture's values encode their own (t, y, x) coordinates precisely so a transposed read fails
# instead of passing on symmetry.
@testset "uns reader (anisotropy grid)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "aniso__branch.h5ad")
    if !have_fixture(h5)
        @test_skip "aniso__branch fixture (missing)"
    else
        lp = label_props(h5)
        @test "orientation_coords" in uns_keys(lp) && "orientation_meta" in uns_keys(lp)

        # producer order = numpy order: (T, y_boxes, x_boxes, component)
        coor = uns_array(lp, "orientation_coords")
        @test size(coor) == (3, 4, 4, 2)
        # value encodes 100t + 10y + x, so this catches an axis swap, not just a shape match
        @test coor[1, 1, 1, 1] ≈ 0.0f0
        @test coor[3, 2, 4, 1] ≈ 100 * 2 + 10 * 1 + 3      # t=2, y=1, x=3 (0-based)
        @test coor[3, 2, 4, 2] ≈ 1000 + 100 * 2 + 10 * 1 + 3
        @test size(uns_array(lp, "orientation_eigvec")) == (3, 4, 4, 2, 2)
        @test size(uns_array(lp, "orientation_box_coherence")) == (3, 4, 4)

        # as_stored hands back the raw (reversed) layout for a caller that wants it
        @test size(uns_array(lp, "orientation_coords"; as_stored=true)) == (2, 4, 4, 3)

        # the self-describing block — strings, scalars and arrays all round-trip
        m = uns_dict(lp, "orientation_meta")
        @test m["box_size_px"] == 15 && m["sigma_px"] ≈ 12.0
        @test m["source"] == "skeleton" && m["fibre_direction"] == "minor"
        @test m["eigval_order"] == "ascending" && m["eigvec_layout"] == "vec_major"
        @test Int.(m["t_index"]) == [0, 1, 2]
        @test length(m["scale_um_per_px"]) == 2

        # absent key, and a group requested as an array (or vice versa) → nothing, not a throw
        @test uns_array(lp, "no_such_key") === nothing
        @test uns_array(lp, "orientation_meta") === nothing
        @test uns_dict(lp, "orientation_coords") === nothing

        # `orientation_summary` is a pandas DataFrame in uns — a third encoding, read by uns_df
        s = uns_df(lp, "orientation_summary")
        @test s isa DataFrame && nrow(s) == 3
        @test "anisotropy" in names(s) && "MF_full_length" in names(s)
        @test Float64.(s.anisotropy) ≈ [0.21, 0.32, 0.43] atol = 1e-6
        @test uns_df(lp, "orientation_coords") === nothing      # a plain array is not a dataframe
        @test uns_df(lp, "no_such_key") === nothing
    end
end

# ── The notebook readouts: quiver_df / branch_segments / anisotropy_df ────────────────────────
# These three are the whole point of the anisotropy pass — the arrows, the branch network and
# the per-image scalar, as tidy frames a Pluto notebook can plot directly (docs/NOTEBOOKS.md).
# The fixture is built so a WRONG read fails: the fibre (minor) eigenvector is a pure +x unit
# vector and the major one is +y, so taking the wrong eigenvector rotates every arrow 90°.
@testset "anisotropy notebook readouts" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "aniso__branch.h5ad")
    if !have_fixture(h5)
        @test_skip "aniso__branch fixture (missing)"
    else
        # EXPLICIT uids. `@testset` reseeds the global RNG per testset, so `gen_uid()` deals
        # every testset the SAME sequence — two testsets that both create a project+set+image
        # land in the same directory. Harmless until one of them, like this pair, asserts on
        # the directory's CONTENTS.
        proj = create_project!(name="aniso-fixture")
        s = add_set!(proj; name="set-A")
        img = add_image!(s; name="img-a", uid="anisoA")
        dir = img_label_props_dir(img); mkpath(dir)
        rm.(joinpath.(dir, readdir(dir)); force=true)     # a previous run's copies
        cp(h5, img_branch_props_path(img, "SHG"))

        @test img_branch_value_names(img) == ["SHG"]

        # ── arrows ────────────────────────────────────────────────────────────────────────
        q = quiver_df(img; value_name="SHG")
        @test nrow(q) == 3 * 4 * 4                       # every frame, every box
        @test sort(unique(q.t)) == [0, 1, 2]
        # the MINOR eigenvector is (y=0, x=1) ⇒ u=1, v=0. If the reader took the major one
        # instead the arrows would come back (0, 1) — a silent 90° rotation.
        @test all(q.u .≈ 1.0) && all(q.v .≈ 0.0)
        # box centres, and that x/y did not swap: coor[...,1] is y, coor[...,2] is x
        r = only(q[(q.t .== 2) .& (q.iy .== 1) .& (q.ix .== 3), :] |> eachrow)
        @test r.y ≈ 100 * 2 + 10 * 1 + 3
        @test r.x ≈ 1000 + 100 * 2 + 10 * 1 + 3
        # the deliberately-empty box carries its zero length through, so it can be filtered out
        @test only(q[(q.t .== 0) .& (q.iy .== 0) .& (q.ix .== 0), :].length) == 0.0
        @test count(>(0.0), q.length) == 3 * (16 - 1)

        @test nrow(quiver_df(img; value_name="SHG", t=1)) == 16
        @test_throws ErrorException quiver_df(img; value_name="SHG", t=99)
        @test_throws ErrorException quiver_df(img; value_name="nope")

        # ── branch segments ───────────────────────────────────────────────────────────────
        b = branch_segments(img; value_name="SHG")
        @test nrow(b) == 6
        # x from axis 1, y from axis 0 — a swap here would mirror the whole network
        @test b.y1 == [0.0, 10, 20, 30, 40, 50] && b.x1 == [1.0, 11, 21, 31, 41, 51]
        @test b.y2 == [4.0, 14, 24, 34, 44, 54] && b.x2 == [5.0, 15, 25, 35, 45, 55]
        @test b.branch_type == [0, 1, 2, 3, 1, 2]
        @test nrow(branch_segments(img; value_name="SHG", t=1)) == 2

        # ── per-image scalar ──────────────────────────────────────────────────────────────
        a = anisotropy_df(img)
        @test nrow(a) == 3 && unique(a.uID) == [img.uid] && unique(a.value_name) == ["SHG"]
        @test a.t == [0, 1, 2]                           # from orientation_meta.t_index, not position
        @test Float64.(a.anisotropy) ≈ [0.21, 0.32, 0.43] atol = 1e-6
        @test "occupancy" in names(a) && "branching_act" in names(a)

        # a second branch table on the same image (SHG collagen + a DCs network) — long format,
        # one block per value_name, which is what makes a cross-image comparison filterable
        cp(h5, img_branch_props_path(img, "DCs"); force=true)
        a2 = anisotropy_df(img)
        @test nrow(a2) == 6 && sort(unique(a2.value_name)) == ["DCs", "SHG"]
        @test nrow(anisotropy_df(img; value_name="SHG")) == 3

        # across images — the cohort frame Figure 4 panel D scatters
        img2 = add_image!(s; name="img-b", uid="anisoB")
        mkpath(img_label_props_dir(img2))
        cp(h5, img_branch_props_path(img2, "SHG"); force=true)
        across = anisotropy_df([img, img2]; value_name="SHG")
        @test nrow(across) == 6 && sort(unique(across.uID)) == sort([img.uid, img2.uid])

        # an image with no branch table contributes nothing — never an error, never a zero row
        img3 = add_image!(s; name="img-c", uid="anisoC")
        @test nrow(anisotropy_df(img3)) == 0
        @test nrow(anisotropy_df([img, img3]; value_name="SHG")) == 3
    end
end

# ── Branch value_names are NOT label_props value_names ────────────────────────────────────────
# Branching runs on a SEGMENTATION, which need not have a per-cell measurement table: an SHG
# collagen mask is skeletonised but never measured, so it lives in `labels`/`branch_labels`
# while `label_props` holds only the measured cell segmentations. Enumerating branch pops from
# `label_props` therefore found NOTHING — it looked for B__branch / T__branch and missed the
# SHG__branch that exists, so the branch picker came back empty. One image can carry several
# (SHG + DCs, per behaviourUbiTom3P.Rmd), so this is the plural case.
@testset "branch value_names come from the sidecars" begin
    proj = create_project!(name="bvn-$(rand(1000:9999))")
    s = add_set!(proj; name="set-A")
    img = add_image!(s; name="img-a")
    dir = img_label_props_dir(img); mkpath(dir)
    @test img_branch_value_names(img) == String[]          # nothing banked yet
    for f in ("SHG__branch.h5ad", "DCs__branch.h5ad", "B.h5ad", "B__tracks.h5ad")
        touch(joinpath(dir, f))
    end
    # only the __branch sidecars, and NOT the cell/track tables that sit beside them
    @test img_branch_value_names(img) == ["DCs", "SHG"]
    @test img_branch_props_path(img, "SHG") == joinpath(dir, "SHG__branch.h5ad")
end

# The same question for tracks, and the reason it needs its own answer: "is this image tracked" was
# being asked of the RUN LOG (does a `tracking.*` entry exist), which a project migrated from the R
# version answers "no" for while its `{vn}__tracks.h5ad` sits on disk — so the guide picker declared
# "needs a tracked image" over a project whose tracks had already been clustered. The sidecar is the
# state; the run log is only the provenance of runs this app happened to execute.
@testset "track value_names come from the sidecars" begin
    proj = create_project!(name="tvn-$(rand(1000:9999))")
    s = add_set!(proj; name="set-A")
    # Explicit uid: `@testset` reseeds the RNG, so `gen_uid` would hand this image the SAME uid — and
    # the same directory — as the branch testset above, whose sidecars are already sitting in it
    # (docs/DEV.md → the `@testset` reseeds the global RNG trap; this test hit it on the first run).
    img = add_image!(s; name="img-a", uid="tvnImgA")
    dir = img_label_props_dir(img); mkpath(dir)
    @test img_track_value_names(img) == String[]           # nothing banked yet
    for f in ("B__tracks.h5ad", "T__tracks.h5ad", "B.h5ad", "T.h5ad", "SHG__branch.h5ad")
        touch(joinpath(dir, f))
    end
    # only the __tracks sidecars, and NOT the cell/branch tables that sit beside them
    @test img_track_value_names(img) == ["B", "T"]
    @test img_track_props_path(img, "B") == joinpath(dir, "B__tracks.h5ad")
    # and it does NOT consult the run log: no tracking entry was ever written above
    @test isempty(read_run_log(img))
end

@testset "plot groupBy (generic categorical sub-axis)" begin
    # split a numeric measure by a categorical column (the hmmPlotParams port): each (pop × level)
    # becomes its own series tagged with `group`. Deterministic synthetic frame — no fixture.
    df = DataFrame("value_name" => fill("A", 7), "pop" => fill("/p", 7),
                   "m"  => [1.0, 2.0, 3.0, 10.0, 11.0, 12.0, 99.0],
                   "st" => [1.0, 1.0, 1.0, 2.0,  2.0,  2.0,  NaN])   # last row: missing group → dropped
    r = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                             normalize=:none, by_image=false, group_by="st")
    @test r["groupBy"] == "st"
    @test length(r["series"]) == 2                                  # two states, NaN-group row dropped
    @test Set(s["group"] for s in r["series"]) == Set(["1", "2"])
    byg = Dict(s["group"] => s for s in r["series"])
    @test byg["1"]["median"] == 2.0 && byg["2"]["median"] == 11.0   # NaN row excluded from state 2
    @test byg["2"]["n"] == 3
    # no group_by → single series, empty group label (back-compat)
    r0 = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=false)
    @test length(r0["series"]) == 1 && r0["series"][1]["group"] == "" && r0["groupBy"] === nothing

    # collapse_series: pool across pops/segmentations/images → series by groupBy level only. Two
    # pops, two value_names, but collapse + group_by="st" still yields exactly two series (1, 2).
    dfc = DataFrame("value_name" => ["A","A","B","B","A","B"], "pop" => ["/p","/q","/p","/q","/p","/q"],
                    "uID" => ["x","x","y","y","x","y"],
                    "m"  => [1.0, 2.0, 3.0, 10.0, 11.0, 12.0],
                    "st" => [1.0, 1.0, 1.0, 2.0,  2.0,  2.0])
    rc = Cecelia._summary_agg(dfc, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true, group_by="st", collapse_series=true)
    @test length(rc["series"]) == 2
    @test Set(s["group"] for s in rc["series"]) == Set(["1", "2"])
    @test all(s["pop"] == "" && s["value_name"] == "" && s["uID"] == "" for s in rc["series"])
    @test Dict(s["group"] => s["n"] for s in rc["series"]) == Dict("1" => 3, "2" => 3)
    # collapse with no group_by → one pooled series over everything
    rc0 = Cecelia._summary_agg(dfc, "bar"; measure="m", granularity=:cell, nbins=10,
                               normalize=:none, by_image=true, collapse_series=true)
    @test length(rc0["series"]) == 1 && rc0["series"][1]["n"] == 6
end

@testset "plot percent (% positive of a 0/1 measure)" begin
    # "% of B cells in contact with a T cell" (`…cell.contact#…`) and "how many T cells are
    # clustered" (`…cell.is.aggregate`) are ONE question: the fraction of a population whose 0/1
    # measure is positive. Both were previously only reachable as a `bar` of the MEAN — an
    # unlabelled 0..1 fraction.

    # ── the detector: a property of the data, not a list of blessed column names ──
    @test Cecelia._is_boolean_measure([0, 1, 1, 0])
    @test Cecelia._is_boolean_measure([0.0, 1.0])
    @test Cecelia._is_boolean_measure([1, missing, NaN, 0])      # missing/non-finite ignored
    @test Cecelia._is_boolean_measure([0, 0, 0])                 # all-negative is still boolean
    @test !Cecelia._is_boolean_measure([0, 1, 2])
    @test !Cecelia._is_boolean_measure([0.0, 0.5, 1.0])
    @test !Cecelia._is_boolean_measure(["a", "b"])               # categorical, not boolean
    @test !Cecelia._is_boolean_measure(Float64[])                # nothing to judge
    @test !Cecelia._is_boolean_measure([missing, NaN])

    # ── Wilson score interval (Wilson 1927), against published values ──
    lo, hi = Cecelia._wilson_ci(5, 10)
    @test isapprox(lo, 0.23659, atol = 1e-4) && isapprox(hi, 0.76341, atol = 1e-4)
    # the case Wald gets WRONG: 0 of 10 → Wald says [0,0] ("certainly never"), Wilson keeps width
    lo0, hi0 = Cecelia._wilson_ci(0, 10)
    @test lo0 == 0.0 && isapprox(hi0, 0.27753, atol = 1e-4)
    # …and at p=1 the interval's exact upper bound IS 1 (float arithmetic lands 1 ulp short)
    lo1, hi1 = Cecelia._wilson_ci(10, 10)
    @test isapprox(lo1, 0.72247, atol = 1e-4) && isapprox(hi1, 1.0)
    @test all(isnan, Cecelia._wilson_ci(0, 0))                   # no data → no interval
    # symmetric about 0.5 (a sanity property of the interval, not of our arithmetic)
    @test isapprox(1 - Cecelia._wilson_ci(3, 10)[2], Cecelia._wilson_ci(7, 10)[1], atol = 1e-12)

    # ── the aggregation ──
    df = DataFrame("value_name" => fill("B", 10), "pop" => fill("/qc", 10),
                   "contact" => [1, 1, 1, 0, 0, 0, 0, 0, 0, 0])
    r = Cecelia._summary_agg(df, "percent"; measure="contact", granularity=:cell, nbins=10,
                             normalize=:none, by_image=false)
    @test r["chartType"] == "percent"
    @test r["valueLabel"] == "% positive"
    @test r["measureBoolean"] === true
    s = only(r["series"])
    @test s["value"] == 30.0 && s["n"] == 10 && s["nPositive"] == 3
    # bounds are the Wilson ones (as percentages) and BRACKET the estimate asymmetrically
    wl, wh = Cecelia._wilson_ci(3, 10)
    @test isapprox(s["lower"], 100wl) && isapprox(s["upper"], 100wh)
    @test s["lower"] < s["value"] < s["upper"]
    @test !isapprox(s["value"] - s["lower"], s["upper"] - s["value"])   # asymmetric — hence 2 bounds
    @test isapprox(s["ci95"], 100 * max(wh - 0.3, 0.3 - wl))            # the wider half-width

    # a percent chart must NOT carry rank/ANOVA comparisons — 0/1 data needs a proportion test
    df2 = vcat(df, DataFrame("value_name" => fill("T", 6), "pop" => fill("/qc", 6),
                             "contact" => [1, 1, 1, 1, 1, 0]))
    r2 = Cecelia._summary_agg(df2, "percent"; measure="contact", granularity=:cell, nbins=10,
                              normalize=:none, by_image=false, stats_enabled=true)
    @test !haskey(r2, "comparisons")
    byvn = Dict(s["value_name"] => s for s in r2["series"])
    @test byvn["B"]["value"] == 30.0
    @test isapprox(byvn["T"]["value"], 500 / 6)

    # an all-missing series reports no percentage rather than a spurious 0%
    dfe = DataFrame("value_name" => fill("T", 3), "pop" => fill("/qc", 3),
                    "contact" => [NaN, NaN, NaN])
    se = only(Cecelia._summary_agg(dfe, "percent"; measure="contact", granularity=:cell, nbins=10,
                                   normalize=:none, by_image=false)["series"])
    @test isnan(se["value"]) && se["n"] == 0

    # ── measureBoolean rides along on the ORDINARY charts, so the panel can offer % positive ──
    rb = Cecelia._summary_agg(df, "bar"; measure="contact", granularity=:cell, nbins=10,
                              normalize=:none, by_image=false)
    @test rb["measureBoolean"] === true
    rn = Cecelia._summary_agg(DataFrame("value_name" => fill("B", 3), "pop" => fill("/qc", 3),
                                        "dist" => [1.5, 20.0, 3.25]), "bar";
                              measure="dist", granularity=:cell, nbins=10,
                              normalize=:none, by_image=false)
    @test rn["measureBoolean"] === false
    # a POPULATION SUMMARY substitutes a synthetic per-image count; counts of 0/1 are not a boolean
    # MEASURE, and offering "% positive" on them would be nonsense.
    dfp = DataFrame("value_name" => ["B", "T"], "pop" => ["/qc", "/qc"], "uID" => ["i1", "i1"])
    rp = Cecelia._summary_agg(dfp, "bar"; measure=nothing, granularity=:cell, nbins=10,
                              normalize=:none, by_image=true)
    @test rp["measureBoolean"] === false
end

@testset "plot count (raw + proportion normalize) — population summary" begin
    # two pops in two images; count → raw row counts; normalize=:fraction → each pop's share of
    # its image's plotted total (the population-summary plot). Deterministic frame — no fixture.
    df = DataFrame("value_name" => fill("A", 10),
                   "pop" => ["/p","/p","/p","/q","/q", "/p","/q","/q","/q","/p"],
                   "uID" => ["x","x","x","x","x",       "y","y","y","y","y"])
    # image x: /p=3, /q=2 (total 5); image y: /p=2, /q=3 (total 5)
    bykey(r) = Dict((s["uID"], s["pop"]) => s["value"] for s in r["series"])
    raw = Cecelia._summary_agg(df, "count"; measure=nothing, granularity=:cell, nbins=0,
                               normalize=:none, by_image=true)
    @test raw["chartType"] == "count"
    rk = bykey(raw)
    @test rk[("x","A/p")] == 3.0 && rk[("x","A/q")] == 2.0
    @test rk[("y","A/p")] == 2.0 && rk[("y","A/q")] == 3.0
    prop = Cecelia._summary_agg(df, "count"; measure=nothing, granularity=:cell, nbins=0,
                                normalize=:fraction, by_image=true)
    @test prop["normalize"] == "fraction"
    pk = bykey(prop)
    @test pk[("x","A/p")] ≈ 0.6 && pk[("x","A/q")] ≈ 0.4     # 3/5, 2/5
    @test pk[("y","A/p")] ≈ 0.4 && pk[("y","A/q")] ≈ 0.6     # 2/5, 3/5
    @test Dict((s["uID"], s["pop"]) => s["n"] for s in prop["series"])[("x","A/p")] == 3

    # no measure + a DISTRIBUTION chart → each IMAGE is a point (its pop count), grouped by pop:
    # boxplot/beeswarm show within-pop variability and compare pops. A/p counts = [3(x),2(y)],
    # A/q = [2(x),3(y)] → each pop has 2 points (images), median 2.5.
    bx = Cecelia._summary_agg(df, "boxplot"; measure=nothing, granularity=:cell, nbins=10,
                              normalize=:none, by_image=true)
    @test bx["chartType"] == "boxplot"
    bybp = Dict(s["pop"] => s for s in bx["series"])
    @test Set(keys(bybp)) == Set(["A/p", "A/q"])
    @test bybp["A/p"]["n"] == 2 && bybp["A/p"]["median"] == 2.5
    # bar over the same per-image counts → mean (A/p mean of [3,2] = 2.5)
    br = Cecelia._summary_agg(df, "bar"; measure=nothing, granularity=:cell, nbins=10,
                              normalize=:none, by_image=true)
    @test Dict(s["pop"] => s["value"] for s in br["series"])["A/p"] == 2.5

    # A SYNTHETIC METRIC IS NUMERIC BY CONSTRUCTION — never sniffed.
    #
    # `_is_categorical_col` guesses from the values, and a per-image count is a handful of small
    # integers, which its integer-level heuristic reads as CATEGORICAL. The panel then intersected
    # the spec's numeric charts with the categorical set and everything except `count` (kept
    # explicitly as measure-independent) disappeared — "population summary always defaults back to
    # count and you cannot select anything else", on the FIRST render, since the spec's first chart
    # is boxplot. The gate is "did the user name this column", so it covers `proportion` and any
    # later synthetic metric with no new name to remember.
    # backend chart names — the frontend's strip/violin both map onto `points` (backendChart)
    for (ct, nrm) in (("boxplot", :none), ("bar", :none), ("points", :none),
                      ("count", :none), ("bar", :fraction))
        r = Cecelia._summary_agg(df, ct; measure=nothing, granularity=:cell, nbins=10,
                                 normalize=nrm, by_image=true)
        @test r["measureType"] == "numeric"
        @test r["measureBoolean"] === false     # counts that happen to be 0/1 are not a boolean measure
    end
    # counts of exactly 1 are the worst case for the heuristic (a single integer level)
    df1 = DataFrame("value_name" => ["A","A"], "pop" => ["/p","/q"], "uID" => ["x","x"])
    @test Cecelia._summary_agg(df1, "boxplot"; measure=nothing, granularity=:cell, nbins=10,
                               normalize=:none, by_image=true)["measureType"] == "numeric"
    # …and a REAL categorical measure is still detected as categorical (the gate must not blanket
    # everything to numeric)
    dfc = DataFrame("value_name" => fill("A", 4), "pop" => fill("/p", 4),
                    "live.cell.hmm.state.movement" => [1.0, 2.0, 1.0, 2.0])
    @test Cecelia._summary_agg(dfc, "frequency"; measure="live.cell.hmm.state.movement",
                               granularity=:cell, nbins=10, normalize=:none,
                               by_image=false)["measureType"] == "categorical"

    # SPLIT BY POPULATION: two tracked pops (value_names B, T) each with clusters — proportion is
    # normalised WITHIN each value_name per image, not pooled across B+T.
    df2 = DataFrame("value_name" => ["B","B","B","T","T", "B","B","T","T","T"],
                    "pop" => ["/Dir","/Dir","/Mea","/Dir","/Mea", "/Dir","/Mea","/Dir","/Mea","/Mea"],
                    "uID" => ["x","x","x","x","x",              "y","y","y","y","y"])
    pr2 = Cecelia._summary_agg(df2, "count"; measure=nothing, granularity=:cell, nbins=0,
                               normalize=:fraction, by_image=true)
    p2 = Dict((s["uID"], s["pop"]) => s["value"] for s in pr2["series"])
    @test p2[("x","B/Dir")] ≈ 2/3 && p2[("x","B/Mea")] ≈ 1/3   # within B (image x: B tot 3)
    @test p2[("x","T/Dir")] ≈ 1/2 && p2[("x","T/Mea")] ≈ 1/2   # within T (image x: T tot 2)
    @test p2[("y","B/Dir")] ≈ 1/2 && p2[("y","T/Mea")] ≈ 2/3   # within B / T (image y)

    # COMPLETE CASES (R tidyr::complete): image y has no /q — it must still contribute a 0 to /q's
    # distribution, not be dropped. Without completion /q would have n=1 (only x) and median 1.
    df3 = DataFrame("value_name" => fill("A", 5),
                    "pop" => ["/p","/p","/q", "/p","/p"],
                    "uID" => ["x","x","x",    "y","y"])          # x: p=2 q=1 ; y: p=2 q=0 (missing)
    bx3 = Cecelia._summary_agg(df3, "boxplot"; measure=nothing, granularity=:cell, nbins=10,
                               normalize=:none, by_image=true)
    by3 = Dict(s["pop"] => s for s in bx3["series"])
    @test by3["A/q"]["n"] == 2 && by3["A/q"]["median"] == 0.5    # /q points [1(x), 0(y)]
    @test by3["A/p"]["n"] == 2 && by3["A/p"]["median"] == 2.0
    # proportion completes too: /q in image y = 0 / (y's A total 2) = 0 → points [1/3, 0]
    pr3 = Cecelia._summary_agg(df3, "boxplot"; measure=nothing, granularity=:cell, nbins=10,
                               normalize=:fraction, by_image=true)
    by3f = Dict(s["pop"] => s for s in pr3["series"])
    @test by3f["A/q"]["n"] == 2 && by3f["A/q"]["median"] ≈ 1/6
end

@testset "plot raw (per-datapoint export)" begin
    # raw=true → one tidy row per datapoint (identity + value) for re-plotting externally, instead of
    # collapsing to box stats. Deterministic frame with label + a groupBy column; last row NaN measure.
    df = DataFrame("value_name" => fill("A", 5), "pop" => fill("/p", 5),
                   "uID" => ["x","x","y","y","y"], "label" => [1, 2, 3, 4, 5],
                   "m"  => [1.0, 2.0, 3.0, 4.0, NaN],
                   "st" => [1.0, 1.0, 2.0, 2.0, 2.0])
    r = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                             normalize=:none, by_image=true, group_by="st", raw=true)
    @test r["chartType"] == "raw" && r["measure"] == "m" && r["groupBy"] == "st"
    @test length(r["rows"]) == 4                       # the NaN-measure row is dropped
    row1 = r["rows"][1]
    @test row1["uID"] == "x" && row1["label"] == "1" && row1["value_name"] == "A"
    @test row1["pop"] == "/p" && row1["value"] == 1.0 && row1["group"] == "1"
    @test [rw["value"] for rw in r["rows"]] == [1.0, 2.0, 3.0, 4.0]
    @test [rw["group"] for rw in r["rows"]] == ["1", "1", "2", "2"]

    # measure-less count chart → raw collapses to per-(image, pop) counts (no label column populated)
    dfc = DataFrame("value_name" => fill("A", 5), "pop" => ["/p","/p","/p","/q","/q"],
                    "uID" => ["x","x","x","x","x"])
    rc = Cecelia._summary_agg(dfc, "count"; measure=nothing, granularity=:cell, nbins=0,
                              normalize=:none, by_image=true, raw=true)
    @test rc["chartType"] == "raw" && rc["measure"] == "count"
    cbyp = Dict(rw["pop"] => rw["value"] for rw in rc["rows"])
    @test cbyp["/p"] == 3.0 && cbyp["/q"] == 2.0 && all(!haskey(rw, "label") for rw in rc["rows"])

    # TRACK granularity: `label` duplicates `track_id` in the track table → drop it, keep track_id.
    dft = DataFrame("value_name" => fill("A", 3), "pop" => fill("/_tracked", 3),
                    "uID" => ["x","x","y"], "label" => [10, 11, 12], "track_id" => [10, 11, 12],
                    "m" => [1.0, 2.0, 3.0])
    rt = Cecelia._summary_agg(dft, "boxplot"; measure="m", granularity=:track, nbins=10,
                              normalize=:none, by_image=true, raw=true)
    @test all(!haskey(rw, "label") for rw in rt["rows"])            # no meaningless label
    @test [rw["track_id"] for rw in rt["rows"]] == ["10", "11", "12"]

    # groupBy that ISN'T applied (its column isn't in the frame) → groupBy null + no group column,
    # so the export never carries an empty, misleading category column.
    rna = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                               normalize=:none, by_image=true, group_by="not_a_column", raw=true)
    @test rna["groupBy"] === nothing && all(!haskey(rw, "group") for rw in rna["rows"])
end

@testset "plot statUnit=image (per-image mean = each dot an image)" begin
    # collapse each image to its mean, then plot those per-image means (n = #images). Deterministic
    # frame: image x cells [1,3,5] (mean 3), image y cells [10,20,30] (mean 20).
    df = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                   "uID" => ["x","x","x","y","y","y"], "m" => [1.0, 3.0, 5.0, 10.0, 20.0, 30.0])
    r = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                             normalize=:none, by_image=true, stat_unit=:image)
    @test length(r["series"]) == 1                     # images pooled into ONE box
    @test r["series"][1]["n"] == 2                      # two datapoints = two images
    @test r["series"][1]["median"] == 11.5 && r["series"][1]["mean"] == 11.5   # of [3, 20]
    # default (individual) + per-image scope → one box PER image, each over its own cells (n = 3);
    # image-mean instead pools those into a single box whose points are the two image means.
    r0 = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true)
    @test length(r0["series"]) == 2 && all(s["n"] == 3 for s in r0["series"])
    # bar over per-image means → mean of the image means
    rb = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true, stat_unit=:image)
    @test rb["series"][1]["value"] == 11.5 && rb["series"][1]["n"] == 2

    # with groupBy: per-image mean WITHIN each level → each level's points are its image means.
    df2 = DataFrame("value_name" => fill("A", 8), "pop" => fill("/p", 8),
                    "uID" => ["x","x","y","y","x","x","y","y"],
                    "m"  => [2.0, 4.0, 6.0, 8.0, 20.0, 20.0, 30.0, 10.0],
                    "st" => [1.0, 1.0, 1.0, 1.0, 2.0,  2.0,  2.0,  2.0])
    r2 = Cecelia._summary_agg(df2, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true, group_by="st", stat_unit=:image)
    byg = Dict(s["group"] => s for s in r2["series"])
    @test byg["1"]["n"] == 2 && byg["1"]["median"] == 5.0     # st1: x[2,4]→3, y[6,8]→7 → [3,7]
    @test byg["2"]["n"] == 2 && byg["2"]["median"] == 20.0    # st2: x[20,20]→20, y[30,10]→20

    # with attr_map: one series PER ATTRIBUTE value, points = the images in it.
    dfa = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                    "uID" => ["x","x","y","y","z","z"], "m" => [2.0, 4.0, 6.0, 8.0, 100.0, 100.0])
    am = Dict("x" => "ctrl", "y" => "ctrl", "z" => "treat")
    ra = Cecelia._summary_agg(dfa, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true, stat_unit=:image, attr_map=am)
    bya = Dict(s["uID"] => s for s in ra["series"])
    @test Set(keys(bya)) == Set(["ctrl", "treat"])
    @test bya["ctrl"]["n"] == 2 && bya["ctrl"]["median"] == 5.0   # images x(3), y(7) → [3,7]
    @test bya["treat"]["n"] == 1                                  # image z only

    # raw export honours it too: rows are the per-image means (label empty, value = the mean)
    rr = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true, stat_unit=:image, raw=true)
    @test [rw["value"] for rw in rr["rows"]] == [3.0, 20.0]
    @test all(!haskey(rw, "label") for rw in rr["rows"]) && Set(rw["uID"] for rw in rr["rows"]) == Set(["x","y"])

    # image_agg=:median collapses each image by MEDIAN, not mean — distinguishable on skewed images:
    # x cells [1,2,9] (mean 4, median 2), y [10,20,90] (mean 40, median 20).
    dfs = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                    "uID" => ["x","x","x","y","y","y"], "m" => [1.0, 2.0, 9.0, 10.0, 20.0, 90.0])
    rmean = Cecelia._summary_agg(dfs, "bar"; measure="m", granularity=:cell, nbins=10,
                                 normalize=:none, by_image=true, stat_unit=:image, image_agg=:mean)
    rmed  = Cecelia._summary_agg(dfs, "bar"; measure="m", granularity=:cell, nbins=10,
                                 normalize=:none, by_image=true, stat_unit=:image, image_agg=:median)
    @test rmean["series"][1]["value"] == 22.0   # mean of image means [4, 40]
    @test rmed["series"][1]["value"] == 11.0    # mean of image medians [2, 20]
    rmedraw = Cecelia._summary_agg(dfs, "boxplot"; measure="m", granularity=:cell, nbins=10,
                                   normalize=:none, by_image=true, stat_unit=:image, image_agg=:median, raw=true)
    @test [rw["value"] for rw in rmedraw["rows"]] == [2.0, 20.0]   # per-image medians
end

@testset "plot matrix (heatmap: profile + crosstab)" begin
    # PROFILE: rows = measures, cols = category levels; cell = mean(measure | level). Pools the whole
    # frame into one grid (no series). Deterministic synthetic frame — no fixture.
    df = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                   "speed" => [1.0, 3.0, 10.0, 12.0, NaN, 5.0],
                   "angle" => [0.1, 0.3, 0.9, 1.1, 0.5, 0.5],
                   "st"    => [1.0, 1.0, 2.0,  2.0,  2.0, NaN])   # last row: NaN level → dropped
    pr = Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                              normalize=:none, by_image=false,
                              matrix_mode="profile", measures=["speed", "angle"], category="st")
    @test pr["matrixMode"] == "profile"
    @test pr["xLabels"] == ["1", "2"] && pr["yLabels"] == ["speed", "angle"]
    cell(r, x, y) = first(c for c in r["cells"] if c["x"] == x && c["y"] == y)
    @test cell(pr, "1", "speed")["value"] == 2.0           # mean(1,3)
    @test cell(pr, "2", "speed")["value"] == 11.0          # mean(10,12); NaN excluded
    @test cell(pr, "2", "speed")["n"] == 2
    @test isempty(pr["series"])

    # z-score standardises each row across its levels (mean 0) — the comparable "signature"
    prz = Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                               normalize=:none, by_image=false, zscore=true,
                               matrix_mode="profile", measures=["speed", "angle"], category="st")
    @test prz["zscore"] == true && prz["valueLabel"] == "z-score"
    zs = [c["value"] for c in prz["cells"] if c["y"] == "speed"]
    @test isapprox(sum(zs), 0.0; atol=1e-9) && all(isfinite, zs)

    # CROSSTAB: a "from_to" categorical → transition matrix; the hybrid uses '.', so the first '_'
    # splits prev|cur ("1.2_3.4" → from "1.2", to "3.4"). Row-normalise → P(to|from).
    dft = DataFrame("value_name" => fill("A", 5), "pop" => fill("/p", 5),
                    "tr" => ["1_1", "1_2", "1_2", "2_1", "x"])   # "x" has no sep → ignored
    ct = Cecelia._summary_agg(dft, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                              normalize=:none, by_image=false,
                              matrix_mode="crosstab", category="tr")
    @test ct["matrixMode"] == "crosstab"
    @test ct["yLabels"] == ["1", "2"] && ct["xLabels"] == ["1", "2"]
    ctc(x, y) = first(c for c in ct["cells"] if c["x"] == x && c["y"] == y)
    @test ctc("1", "1")["value"] == 1.0 && ctc("2", "1")["value"] == 2.0   # counts
    # row-normalised: from state 1 → {1:1, 2:2} → P(2|1) = 2/3
    ctr = Cecelia._summary_agg(dft, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                               normalize=:none, by_image=false,
                               matrix_mode="crosstab", category="tr", matrix_normalize=:row)
    @test ctr["valueLabel"] == "P(to|from)"
    ctrc(x, y) = first(c for c in ctr["cells"] if c["x"] == x && c["y"] == y)
    @test isapprox(ctrc("2", "1")["value"], 2/3; atol=1e-9)
    @test isapprox(ctrc("1", "1")["value"], 1/3; atol=1e-9)

    # error cases: unknown mode, missing category, profile with no present measure column
    @test_throws ErrorException Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell,
        nbins=0, normalize=:none, by_image=false, matrix_mode="bogus", category="st")
    @test_throws ErrorException Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell,
        nbins=0, normalize=:none, by_image=false, matrix_mode="profile", measures=["speed"], category="nope")

    # all-NaN / empty level → JSON-null value (NOT NaN — JSON3 rejects NaN; the renderer skips null)
    dfn = DataFrame("value_name" => fill("A", 3), "pop" => fill("/p", 3),
                    "speed" => [1.0, 2.0, NaN], "st" => [1.0, 1.0, 2.0])
    prn = Cecelia._summary_agg(dfn, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                               normalize=:none, by_image=false, matrix_mode="profile",
                               measures=["speed"], category="st")
    c2 = first(c for c in prn["cells"] if c["x"] == "2")
    @test c2["value"] === nothing && c2["n"] == 0     # state 2 has only a NaN → null cell

    # EMPTY FRAME (a population with no rows in this image) → an empty GRID, never an error. pop_df
    # returns a frame with no columns at all, which used to trip the category check and print
    # "matrix needs a `category` column present in the data" into the panel — the failure a per-image
    # board of cluster pops hits whenever one cluster is absent from one image. Every other chart type
    # answers this with an empty series; the response keys must match the populated ones so the
    # renderer's empty state fires instead of breaking.
    for (mode, extra) in (("crosstab", "normalize"), ("profile", "zscore"))
        e = Cecelia._summary_agg(DataFrame(), "matrix"; measure=nothing, granularity=:cell, nbins=0,
                                 normalize=:none, by_image=false, matrix_mode=mode,
                                 measures=["speed"], category="tr")
        @test e["chartType"] == "matrix" && e["matrixMode"] == mode && e["category"] == "tr"
        @test isempty(e["cells"]) && isempty(e["xLabels"]) && isempty(e["yLabels"])
        @test haskey(e, extra) && haskey(e, "valueLabel")   # same shape as the populated response
    end
    # …but an unknown mode is still an error, empty frame or not
    @test_throws ErrorException Cecelia._summary_agg(DataFrame(), "matrix"; measure=nothing,
        granularity=:cell, nbins=0, normalize=:none, by_image=false, matrix_mode="bogus", category="tr")
end

@testset "plot attribute grouping (compare by attr)" begin
    # group cross-image series by an image attribute: images sharing a value pool into one series
    # labelled by the value; an image with no value falls back to its uID. No fixture.
    df = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                   "uID" => ["x1","x1","x2","x2","y1","y1"],
                   "m"   => [1.0, 2.0, 3.0, 4.0, 10.0, 12.0])
    amap = Dict("x1"=>"T", "x2"=>"T", "y1"=>"C")
    r = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=0,
                             normalize=:none, by_image=true, attr_map=amap)
    @test length(r["series"]) == 2
    @test Set(s["uID"] for s in r["series"]) == Set(["T", "C"])
    byu = Dict(s["uID"] => s for s in r["series"])
    @test byu["T"]["n"] == 4 && byu["C"]["n"] == 2          # x1+x2 pooled under "T"
    # image missing the attribute → falls back to its uID
    r2 = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=0,
                              normalize=:none, by_image=true, attr_map=Dict("x1"=>"T", "x2"=>"T"))
    @test Set(s["uID"] for s in r2["series"]) == Set(["T", "y1"])
    # no attr_map → group by image as before (3 images)
    r3 = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=0,
                              normalize=:none, by_image=true)
    @test length(r3["series"]) == 3
end

# ── cross-image (set-level) aggregation: pool pop_df across images by uID ──
@testset "plot_summary_data cross-image" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "plot_summary_data cross-image (fixture missing)"
    else
        # two "images" from the same fixture (uX, uY) — exercises set-level pooling mechanics
        mk = function (uid)
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
            cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
            img = CciaImage(uid=uid, dir=td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
            img
        end
        imgs = [mk("uX"), mk("uY")]; uids = ["uX", "uY"]

        # pop_df set-level: uID column tags each image; rows = 2× a single image's tracks
        one = pop_df(imgs[1], "live", ["B/_tracked"]; granularity=:track)
        both = pop_df(imgs, uids, "live", ["B/_tracked"]; granularity=:track)
        @test "uID" in names(both)
        @test Set(unique(both.uID)) == Set(uids)
        @test nrow(both) == 2 * nrow(one)

        # boxplot per_image → one box per image (same data → equal stats), + scope field
        sp = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "boxplot";
                               measure="live.track.speed", granularity=:track, scope=:per_image)
        @test sp["scope"] == "per_image" && sp["chartType"] == "boxplot"
        @test length(sp["series"]) == 2
        @test Set(s["uID"] for s in sp["series"]) == Set(uids)
        @test Set(keys(sp["series"][1])) ⊇ Set(["q1","median","q3","lower","upper","mean","n"])
        @test sp["series"][1]["median"] ≈ sp["series"][2]["median"]    # identical fixtures
        @test sp["series"][1]["q1"] <= sp["series"][1]["median"] <= sp["series"][1]["q3"]
        @test sp["series"][1]["n"] == nrow(one)

        # summarised → one pooled box across both images
        ss = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "boxplot";
                               measure="live.track.speed", granularity=:track, scope=:summarised)
        @test length(ss["series"]) == 1 && ss["series"][1]["n"] == 2 * nrow(one)

        # SAME data source, different chart type (bar of mean ± sd) — chart ⊥ data source
        br = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "bar";
                               measure="live.track.speed", granularity=:track, scope=:per_image)
        @test br["chartType"] == "bar" && length(br["series"]) == 2
        @test Set(keys(br["series"][1])) ⊇ Set(["value", "sd", "n"])
        @test br["series"][1]["value"] ≈ br["series"][2]["value"]

        # histogram per_image → 2 overlay series sharing bin edges
        hh = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "histogram";
                               measure="live.track.speed", granularity=:track, scope=:per_image, nbins=10)
        @test length(hh["series"]) == 2 && length(hh["binEdges"]) == 11
        @test all(sum(s["counts"]) == nrow(one) for s in hh["series"])
    end
end

# ── multiple SEGMENTATIONS on one plot: (value_name, pop) targets ─────────
@testset "plot_summary_data multi-segmentation targets" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "plot_summary_data multi-segmentation (fixture missing)"
    else
        # one image with the same data exposed under TWO segmentations (B, B2) — exercises the
        # targets path: a (value_name, pop) per series, vcat across segmentations.
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
        cp(h5,  joinpath(td, "labelProps", "B2.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B2__tracks.h5ad"))
        img = CciaImage(uid="uM", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["B2"] = "B2.h5ad"
        img.label_props["_active"] = "B"

        targets = [("B", "/_tracked"), ("B2", "/_tracked")]
        # single image, two segmentations → one series per (segmentation, pop), distinct vn ids
        bp = plot_summary_data(img, "live", targets, "boxplot";
                               measure="live.track.speed", granularity=:track)
        @test bp["chartType"] == "boxplot" && length(bp["series"]) == 2
        @test Set(s["value_name"] for s in bp["series"]) == Set(["B", "B2"])
        @test Set(s["pop"] for s in bp["series"]) == Set(["B/_tracked", "B2/_tracked"])
        @test bp["series"][1]["median"] ≈ bp["series"][2]["median"]   # identical underlying data

        # cross-image AND cross-segmentation: 2 images × 2 segmentations → 4 per_image series
        img2 = CciaImage(uid="uN", dir=td)
        img2.label_props["B"] = "B.h5ad"; img2.label_props["B2"] = "B2.h5ad"
        img2.label_props["_active"] = "B"
        xp = plot_summary_data([img, img2], ["uM", "uN"], "live", targets, "bar";
                               measure="live.track.speed", granularity=:track, scope=:per_image)
        @test xp["scope"] == "per_image" && length(xp["series"]) == 4
        @test Set((s["uID"], s["value_name"]) for s in xp["series"]) ==
              Set([("uM","B"), ("uM","B2"), ("uN","B"), ("uN","B2")])
    end
end

# ── summary-plot aggregation: pure helpers (no fixture) ───────────────────
@testset "plot_summary_data helpers" begin
    @test Cecelia._hist_edges(Float64[], 10) == Float64[]            # no data → no edges
    @test length(Cecelia._hist_edges([5.0], 4)) == 5                 # single value → 1-wide bin
    let edges = Cecelia._hist_edges([0.0, 10.0], 10)
        @test Cecelia._hist_counts([0.0, 5.0, 9.99, NaN, 10.0], edges) |> sum == 4  # NaN skipped
    end
    @test Cecelia._catkey(2.0) == "2" && Cecelia._catkey(1.5) == "1.5"
    @test Cecelia._sort_cats(["10", "2", "1"]) == ["1", "2", "10"]   # numeric, not lexical
    @test Cecelia._sort_cats(["b", "a"]) == ["a", "b"]               # lexical fallback
    # derived-pop registry is generic: /_tracked is a `live` derived pop, none for `flow`
    @test derived_pop_paths("live") == ["/_tracked"]
    @test isempty(derived_pop_paths("flow"))
end

# ── track_props: per-track aggregation (ports tracksInfo; cell→track properties) ──
@testset "track_props (KDIeEm B)" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "track_props (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # mock a categorical per-cell column to exercise the freq branch
        cells = label_props(img; value_name="B") |> select_cols(["track_id"]) |> as_df
        label_props(img_label_props_path(img, "B")) |>
            add_obs(DataFrame("label" => cells.label,
                                      "st" => [Float64((l % 2) + 1) for l in cells.label])) |> save!

        tp = track_props(img; value_name="B", cell_measures=["area", "st"], categorical=["st"])
        ntr = nrow(pop_df(img, "live", ["B/_tracked"]; granularity=:track))
        @test nrow(tp) == ntr                                  # one row per track
        @test Set(names(tp)) ⊇ Set(["track_id", "label", "num_cells"])
        @test tp.label == tp.track_id                          # engine membership key
        # numeric aggregates present
        @test Set(names(tp)) ⊇ Set(["area.mean", "area.median", "area.sum", "area.qUp", "area.qLow", "area.sd"])
        # categorical → per-category frequency columns
        @test "st.1" in names(tp) && "st.2" in names(tp)
        # motility joined from the track table
        @test "live.track.speed" in names(tp)
        # num_cells totals the tracked cells
        @test sum(tp.num_cells) == sum(c -> c > 0, Int.(filter(x -> x isa Number && !isnan(x), cells.track_id)))

        # AUTO-DETECTION (no config map; replaces R config.yml labelStats). The split is read
        # off the decoded type + values: strings and integer code sets → categorical; continuous
        # floats → numeric. Mirrors the real data: hmm.transitions "1.3", hmm.state 1/2/3, speed 10.12.
        @test Cecelia._is_categorical_col(["1.3", "2.2"])                  # String → categorical (transitions)
        @test Cecelia._is_categorical_col(["a", missing])                  # Missing-union String too
        @test Cecelia._is_categorical_col([1.0, 2.0, 3.0])                 # integer code set → categorical (hmm.state)
        @test Cecelia._is_categorical_col([1, 2, missing])                 # integer codes (Missing-union) too
        @test !Cecelia._is_categorical_col([10.12, 11.3, 9.8])             # continuous floats → numeric (speed)
        @test !Cecelia._is_categorical_col(Float64.(1:100))               # wide-spread integers → numeric (counts/area)
        # name-rule: cluster code columns are categorical regardless of level count (>cap clusters)
        @test Cecelia._is_categorical_col(Float64.(1:100), "clusters")          # exact name
        @test Cecelia._is_categorical_col(Float64.(1:100), "clusters.default")  # clusters.{suffix}
        @test !Cecelia._is_categorical_col(Float64.(1:100), "area")             # other names keep the heuristic
        # `st` is an integer code (1/2) → auto-detected categorical with NO override → freq cols
        auto = track_props(img; value_name="B", cell_measures=["st"])
        @test "st.1" in names(auto) && "st.2" in names(auto) && !("st.mean" in names(auto))
        # `numeric` escape-hatch forces it back to numeric aggregates when desired
        forced = track_props(img; value_name="B", cell_measures=["st"], numeric=["st"])
        @test "st.mean" in names(forced) && !("st.1" in names(forced))
    end
end

# ── track_cell_measures: derive base cell measures from track-property column names ──
@testset "track_cell_measures" begin
    mot = ["live.track.speed", "live.track.meanTurningAngle"]
    # motility axes need no cell aggregation
    @test isempty(track_cell_measures(["live.track.speed"], mot))
    # numeric aggregate columns → their base cell measure (suffix stripped)
    @test track_cell_measures(["mean_intensity_0.mean", "area.qUp"], mot) ==
          ["mean_intensity_0", "area"]
    # categorical frequency column `{base}.{cat}` → base
    @test track_cell_measures(["hmm.state.1"], mot) == ["hmm.state"]
    # bookkeeping + motility skipped; dedup across aggregates of the same base
    @test track_cell_measures(["track_id", "num_cells", "live.track.speed",
                               "area.mean", "area.sd"], mot) == ["area"]
end

# ── pop_df pop_type="track": gate DIRECTLY on per-track properties (3b) ────────
@testset "pop_df pop_type=track (KDIeEm B)" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "pop_df pop_type=track (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # motility-only track props (the common track-gating case: no cell_measures needed)
        tp = track_props(img; value_name="B")
        @test "live.track.speed" in names(tp)
        spd = Float64.(collect(skipmissing(tp[!, "live.track.speed"])))
        thr = sort(spd)[cld(length(spd), 2)]                     # ~median → discriminating
        truth = count(>=(thr), spd)
        @test 0 < truth < length(spd)

        # a TRACK gate (one point per track) on the speed axis, stored under __tracks
        m = PopulationMap(pop_type="track", value_name="B")
        add_pop!(m, "fast"; gate=RectangleGate("live.track.speed", "live.track.speed",
                                               thr, 1e12, -1e12, 1e12))
        save_pop_map!(m, img)
        @test isfile(joinpath(td, "gating", "B__tracks.json"))   # track gate file
        @test !isfile(joinpath(td, "gating", "B.json"))          # NOT the flow file

        # granularity=:track → gated track rows, one point per track, gate genuinely applied
        g = pop_df(img, "track", ["/fast"]; value_name="B", granularity=:track)
        @test nrow(g) == truth
        @test length(unique(g.track_id)) == nrow(g)
        @test unique(g.pop) == ["/fast"]
        @test all(Float64.(g[!, "live.track.speed"]) .>= thr)

        # granularity=:cell → expand gated tracks to their member cells (track pulls its cells)
        gc = pop_df(img, "track", ["/fast"]; value_name="B", granularity=:cell)
        @test Set(names(gc)) ⊇ Set(["label", "track_id", "pop", "value_name"])
        @test Set(unique(gc.track_id)) == Set(Int.(g.track_id))  # same tracks, expanded
        @test nrow(gc) > nrow(g)                                 # many cells per track
        @test all(in(Set(Int.(g.track_id))), Int.(gc.track_id))
    end
end

@testset "HMM states + transitions" begin
    # deterministic two-state tracks across two images; state flips at t=13. Track-start cells
    # carry NaN like real track measures (no speed at t=1; no angle at t=1,2) → must decode to
    # `missing` (per-cell states are undefined where a measurement can't exist).
    uID = String[]; vn = String[]; tid = Int[]; tt = Float64[]; sp = Float64[]; an = Float64[]
    for img in ("X", "Y"), k in 1:3, t in 1:25
        slow = t <= 12
        s = (slow ? 0.5 : 5.0) + 0.05 * sin(t)        # deterministic, non-degenerate
        a = (slow ? 0.2 : 2.5) + 0.05 * cos(t)
        if t == 1; s = NaN; a = NaN; elseif t == 2; a = NaN; end
        push!(uID, img); push!(vn, "A"); push!(tid, k); push!(tt, Float64(t))
        push!(sp, s); push!(an, a)
    end
    df = DataFrame("uID" => uID, "value_name" => vn, "track_id" => tid, "t" => tt,
                   "live.cell.speed" => sp, "live.cell.angle" => an)

    st = hmm_fit_states(df, ["live.cell.speed", "live.cell.angle"]; num_states=2, time_col="t")
    @test length(st) == nrow(df)

    # regression: an EMPTY measure selection from the GUI arrives as `Vector{Union{}}` (not
    # Vector{String}); fit must not MethodError on the normalise/scale step.
    let stu = hmm_fit_states(df, ["live.cell.speed", "live.cell.angle"]; num_states=2, time_col="t",
                             scale_measures=Union{}[], normalise=Dict{String,String}())
        @test count(!ismissing, stu) == count(!ismissing, st)
    end
    @test count(ismissing, st) == 12                  # 2 dropped × 6 tracks (t=1 no speed+angle, t=2 no angle)
    @test Set(skipmissing(st)) == Set([1, 2])
    df[!, "live.cell.hmm.state.default"] = st

    one = st[(df.uID .== "X") .& (df.track_id .== 1)]
    @test all(ismissing, one[1:2]) && !any(ismissing, one[3:end])
    decoded = collect(skipmissing(one))
    @test count(i -> decoded[i] != decoded[i-1], 2:length(decoded)) == 1   # exactly one flip

    tr = hmm_transitions(df, ["live.cell.hmm.state.default"]; time_col="t",
                         include_start=false, include_self=true)
    @test length(tr) == nrow(df)
    nonmiss = collect(skipmissing(tr))
    @test all(occursin("_", x) for x in nonmiss)
    @test Set(nonmiss) ⊆ Set(["1_1", "2_2", "1_2", "2_1"])
    @test ("1_2" in nonmiss) || ("2_1" in nonmiss)    # the flip transition exists

    trn = hmm_transitions(df, ["live.cell.hmm.state.default"]; time_col="t",
                          include_start=false, include_self=false)
    nm2 = Set(skipmissing(trn))
    @test nm2 ⊆ Set(["1_2", "2_1"]) && !isempty(nm2)  # self excluded → only the flip survives

    # cross-model hybrid: two state columns paste into "a.b" before transitions
    df[!, "live.cell.hmm.state.second"] = st
    trh = hmm_transitions(df, ["live.cell.hmm.state.default", "live.cell.hmm.state.second"];
                          time_col="t", include_start=false, include_self=true)
    @test any(x -> occursin(".", split(x, "_")[1]), skipmissing(trh))

    # cross-segmentation pops parsing: prefixed pops name their value_name ("A/_tracked" → "A",
    # the derived tracked pop = track_id>0); placeholders/empties are dropped. This is what lets
    # one run fit tracked A, B, C together (the segmentation is the pop prefix, not a separate
    # param). `_tracked` is the reserved derived-pop convention (leaf names beginning with `_`).
    @test Cecelia._hmm_pops(Dict{String,Any}("pops" => ["A/_tracked", "B/_tracked", "NONE", ""])) ==
          ["A/_tracked", "B/_tracked"]
    @test Cecelia._hmm_pops(Dict{String,Any}("pops" => "A/_tracked")) == ["A/_tracked"]
    @test Set(Cecelia._hmm_pop_value_names(["A/_tracked", "B/_tracked", "C/cd4/_tracked"], "default")) ==
          Set(["A", "B", "C"])

    # task registration + set-scope routing
    @test _task_from_fun_name("behaviour.hmm_states") isa Cecelia.HmmStates
    @test _task_from_fun_name("behaviour.hmm_transitions") isa Cecelia.HmmTransitions
    @test _task_from_fun_name("behaviour.hmm") isa Cecelia.CompositeTask
    @test task_scope(_task_from_fun_name("behaviour.hmm")) == "set"
    @test task_scope(_task_from_fun_name("behaviour.hmm_states")) == "set"
    @test task_scope(_task_from_fun_name("tracking.track_measures")) == "image"
end

# ── Physical-size / timing metadata (import review, edit, resync) ──────────────
# All fixtures are synthetic temp zarrs (no real data) — these functions are pure
# readers/writers over BOTH on-disk layouts (CLAUDE.md → OME-ZARR dual-format).
@testset "OME metadata read/edit/resync" begin
    # Build a minimal zarr in either layout, plus optional `.zarray` (shape → SizeC/T/Z) and
    # `OME/METADATA.ome.xml` (planes).
    #   :series (bioformats2raw) — multiscales in `0/.zattrs`, level-0 array at `0/0`
    #   :flat   (create_multiscales) — multiscales at the ROOT `.zattrs`, level-0 array at `0`,
    #           whose own `.zattrs` is `{}`. That empty file is the point: both layouts have a
    #           `0/` child, so only its CONTENT tells them apart.
    function make_zarr(dir; axes, level_scales, units = Dict{String,String}(),
                       shape = nothing, planes = nothing, layout = :series)
        base = layout === :series ? joinpath(dir, "0") : dir
        mkpath(base)
        ax_objs = map(axes) do a
            o = Dict{String,Any}("name" => a,
                "type" => a in ("x", "y", "z") ? "space" : (a == "t" ? "time" : "channel"))
            haskey(units, a) && (o["unit"] = units[a])
            o
        end
        datasets = [Dict{String,Any}("path" => string(i - 1),
                      "coordinateTransformations" =>
                          [Dict{String,Any}("type" => "scale", "scale" => level_scales[i])])
                    for i in eachindex(level_scales)]
        zattrs = Dict{String,Any}("multiscales" =>
            [Dict{String,Any}("axes" => ax_objs, "datasets" => datasets)])
        open(joinpath(base, ".zattrs"), "w") do io; JSON3.write(io, zattrs); end
        if layout === :flat
            mkpath(joinpath(dir, "0"))
            write(joinpath(dir, "0", ".zattrs"), "{}")   # level-0 ARRAY, no multiscales
        end
        if !isnothing(shape)
            mkpath(joinpath(base, "0"))
            open(joinpath(base, "0", ".zarray"), "w") do io
                JSON3.write(io, Dict{String,Any}("shape" => shape))
            end
        end
        if !isnothing(planes)
            mkpath(joinpath(dir, "OME"))
            body = join([
                "<Plane TheZ=\"$(p.z)\" TheT=\"$(p.t)\" DeltaT=\"$(p.dt)\"" *
                (haskey(p, :unit) ? " DeltaTUnit=\"$(p.unit)\"" : "") * "/>"
                for p in planes], "\n")
            open(joinpath(dir, "OME", "METADATA.ome.xml"), "w") do io
                write(io, "<OME><Image><Pixels>$body</Pixels></Image></OME>")
            end
        end
        dir
    end

    # ── _delta_t_fallback: per-plane DeltaT (TheZ=0, TheT=1), unit-converted to seconds ──
    @testset "_delta_t_fallback" begin
        mktempdir() do d
            make_zarr(d; axes = ["t", "z", "y", "x"], level_scales = [[1.0, 1.0, 0.5, 0.5]],
                      planes = [(z = 0, t = 0, dt = 0.0, unit = "ms"),
                                (z = 0, t = 1, dt = 5000.0, unit = "ms"),
                                (z = 0, t = 2, dt = 10000.0, unit = "ms")])
            @test Cecelia._delta_t_fallback(d) == 5.0            # 5000 ms → 5 s, from TheT=1
        end
        mktempdir() do d
            make_zarr(d; axes = ["t", "y", "x"], level_scales = [[1.0, 0.5, 0.5]],
                      planes = [(z = 0, t = 1, dt = 2.0, unit = "min")])
            @test Cecelia._delta_t_fallback(d) == 120.0          # 2 min → 120 s
        end
        mktempdir() do d
            make_zarr(d; axes = ["t", "y", "x"], level_scales = [[1.0, 0.5, 0.5]],
                      planes = [(z = 0, t = 1, dt = 30.0)])      # no unit → seconds
            @test Cecelia._delta_t_fallback(d) == 30.0
        end
        # non-self-closing <Plane>…</Plane> (some vendors) — DeltaT is on the opening tag
        mktempdir() do d
            mkpath(joinpath(d, "OME"))
            write(joinpath(d, "OME", "METADATA.ome.xml"),
                  "<OME><Image><Pixels>" *
                  "<Plane TheZ=\"0\" TheT=\"1\" DeltaT=\"3\" DeltaTUnit=\"s\"><Annotation/></Plane>" *
                  "</Pixels></Image></OME>")
            @test Cecelia._delta_t_fallback(d) == 3.0
        end
        @test isnothing(Cecelia._delta_t_fallback(joinpath(tempdir(), "nope-$(rand(UInt32))")))
    end

    # ── read_ome_metadata: unit-less-t placeholder is rejected; DeltaT fills the gap ──
    @testset "read_ome_metadata" begin
        # t axis has a scale (1.0) but NO unit → placeholder, must NOT become TimeIncrement=1.0;
        # SizeT>1 so the DeltaT fallback kicks in and supplies the real interval.
        mktempdir() do d
            make_zarr(d; axes = ["t", "z", "y", "x"],
                      level_scales = [[1.0, 0.6, 0.5, 0.5]],
                      units = Dict("x" => "micrometer", "y" => "micrometer", "z" => "micrometer"),
                      shape = [3, 1, 4, 4],
                      planes = [(z = 0, t = 1, dt = 7.0, unit = "s")])
            m = read_ome_metadata(d)
            @test m["SizeT"] == 3
            @test m["PhysicalSizeX"] == 0.5
            @test m["PhysicalSizeZ"] == 0.6
            @test m["PhysicalSizeUnit"] == "micrometer"
            @test m["TimeIncrement"] == 7.0                       # from DeltaT, not the 1.0 placeholder
            @test m["TimeIncrementUnit"] == "second"
        end
        # t axis WITH a unit → trusted verbatim, no fallback needed.
        mktempdir() do d
            make_zarr(d; axes = ["t", "y", "x"], level_scales = [[2.5, 0.5, 0.5]],
                      units = Dict("t" => "second", "x" => "micrometer", "y" => "micrometer"),
                      shape = [4, 8, 8])
            m = read_ome_metadata(d)
            @test m["TimeIncrement"] == 2.5
            @test m["TimeIncrementUnit"] == "second"
        end
    end

    # ── update_ome_scale!: level-0 value set, other levels keep their downsample ratio; units ──
    @testset "update_ome_scale!" begin
        mktempdir() do d
            # z doesn't downsample (0.6, 0.6); x halves per level (0.5, 1.0)
            make_zarr(d; axes = ["z", "y", "x"],
                      level_scales = [[0.6, 0.5, 0.5], [0.6, 1.0, 1.0]])
            update_ome_scale!(d, Dict("z" => 3.0, "x" => 0.65);
                units = Dict("x" => "micrometer", "y" => "micrometer", "z" => "micrometer"))
            z = JSON3.read(read(joinpath(d, "0", ".zattrs"), String))
            dss = z[:multiscales][1][:datasets]
            s0 = dss[1][:coordinateTransformations][1][:scale]
            s1 = dss[2][:coordinateTransformations][1][:scale]
            @test s0[1] == 3.0 && s1[1] == 3.0                   # z ratio 5× applied to both levels
            @test s0[3] == 0.65 && isapprox(s1[3], 1.3)          # x ratio 1.3× preserves downsample
            m = read_ome_metadata(d)
            @test m["PhysicalSizeUnit"] == "micrometer"          # axis unit now round-trips
            @test m["PhysicalSizeZ"] == 3.0
        end
        # unit-only edit (no numeric change) still writes the axis unit
        mktempdir() do d
            make_zarr(d; axes = ["y", "x"], level_scales = [[0.5, 0.5]])
            @test isnothing(get(read_ome_metadata(d), "PhysicalSizeUnit", nothing))
            update_ome_scale!(d, Dict{String,Float64}();
                              units = Dict("x" => "nanometer", "y" => "nanometer"))
            @test read_ome_metadata(d)["PhysicalSizeUnit"] == "nanometer"
        end
    end

    # ── update_ome_xml_pixels!: replace an existing attr, insert a missing one ──
    @testset "update_ome_xml_pixels!" begin
        mktempdir() do d
            mkpath(joinpath(d, "OME"))
            xml_file = joinpath(d, "OME", "METADATA.ome.xml")
            write(xml_file, "<OME><Image><Pixels SizeX=\"4\" PhysicalSizeZ=\"0.6\">" *
                            "<Plane/></Pixels></Image></OME>")
            update_ome_xml_pixels!(d, Dict("PhysicalSizeZ" => "3.0", "TimeIncrement" => "5.0"))
            out = read(xml_file, String)
            @test occursin("PhysicalSizeZ=\"3.0\"", out)         # replaced
            @test !occursin("PhysicalSizeZ=\"0.6\"", out)
            @test occursin("TimeIncrement=\"5.0\"", out)         # inserted
            @test occursin("SizeX=\"4\"", out)                   # untouched
        end
    end

    # ── sync_zarr_calibration!: one translator (meta shape → zarr) for import + editor ──
    @testset "sync_zarr_calibration!" begin
        @test !Cecelia.has_calibration_meta(Dict{String,Any}("SizeC" => 2))
        @test !Cecelia.has_calibration_meta(Dict{String,Any}("PhysicalSizeZ" => nothing))  # null clear
        @test Cecelia.has_calibration_meta(Dict{String,Any}("PhysicalSizeZ" => 3.0))
        mktempdir() do d
            make_zarr(d; axes = ["t", "z", "y", "x"], level_scales = [[1.0, 0.6, 0.5, 0.5]],
                      shape = [3, 1, 4, 4],
                      planes = [(z = 0, t = 1, dt = 0.0, unit = "s")])  # OME/ dir + <Pixels>
            # a meta-shaped correction (as ccid.json / the importer / the editor produce it)
            Cecelia.sync_zarr_calibration!(d, Dict{String,Any}(
                "PhysicalSizeZ" => 3.0, "PhysicalSizeUnit" => "micrometer",
                "TimeIncrement" => 5.0, "TimeIncrementUnit" => "second"))
            # .zattrs now round-trips the corrected spatial value + unit
            m = read_ome_metadata(d)
            @test m["PhysicalSizeZ"] == 3.0
            @test m["PhysicalSizeUnit"] == "micrometer"
            # OME-XML <Pixels> carries the time interval napari reads unconditionally
            xml = read(joinpath(d, "OME", "METADATA.ome.xml"), String)
            @test occursin("TimeIncrement=\"5.0\"", xml)
            @test occursin("TimeIncrementUnit=\"s\"", xml)
        end
    end

    # ── flat (create_multiscales) layout: the 8-bit import + crop write one ──
    # Regression: these readers/writers hardcoded the series `0/.zattrs`. For a flat store that
    # path exists too (the level-0 array's own, empty `.zattrs`), so every one of them found no
    # multiscales and returned silently. `sync_zarr_calibration!` then landed its OME-XML half
    # and dropped its NGFF half — a store claiming `TimeIncrement="10.0"` in XML and
    # `t: {unit: second, scale: 1.0}` in NGFF, which napari renders as 1 s/frame.
    @testset "flat layout (create_multiscales)" begin
        mktempdir() do d
            make_zarr(d; layout = :flat, axes = ["t", "z", "y", "x"],
                      level_scales = [[1.0, 5.0, 0.5, 0.5]], shape = [180, 8, 4, 4],
                      units = Dict("t" => "second", "z" => "micrometer",
                                   "y" => "micrometer", "x" => "micrometer"))
            @test Cecelia.series_base(d) == d                       # not the `0/` array
            m = read_ome_metadata(d)
            @test m["SizeT"] == 180 && m["SizeZ"] == 8              # .zarray found at `0/`
            @test m["PhysicalSizeZ"] == 5.0
            @test m["TimeIncrement"] == 1.0                         # the placeholder, pre-sync

            Cecelia.sync_zarr_calibration!(d, Dict{String,Any}(
                "TimeIncrement" => 10.0, "TimeIncrementUnit" => "second"))
            @test read_ome_metadata(d)["TimeIncrement"] == 10.0     # NGFF half now lands
            z = JSON3.read(read(joinpath(d, ".zattrs"), String))
            @test z[:multiscales][1][:datasets][1][:coordinateTransformations][1][:scale][1] == 10.0
            @test read(joinpath(d, "0", ".zattrs"), String) == "{}" # array attrs untouched
        end
        # series layout still resolves to `0/` — the discriminator is the multiscales attr
        mktempdir() do d
            make_zarr(d; axes = ["t", "y", "x"], level_scales = [[2.5, 0.5, 0.5]],
                      units = Dict("t" => "second"))
            @test Cecelia.series_base(d) == joinpath(d, "0")
        end
        @test Cecelia.series_base(joinpath(tempdir(), "nope-$(rand(UInt32))")) isa String
    end

    # ── Cross-language: the Julia and Python calibration stamps must agree ──
    # Calibration lives in two on-disk copies (NGFF `.zattrs`, OME-XML `<Pixels>`) and has two
    # writers that cannot call each other: Python `zarr_utils.write_calibration` (used by every
    # task runner) and Julia `sync_zarr_calibration!` (the importer + metadata editor). Each
    # therefore carries its own unit table and its own idea of which axis gets what. This is the
    # only thing that stops them drifting: same stale store, one stamped by each, byte-compared.
    @testset "calibration writers agree across languages" begin
        pyroot  = joinpath(dirname(dirname(@__DIR__)), "python")
        haspy   = success(pipeline(addenv(`python -c "import ome_types, zarr, dask, cecelia"`,
                                          "PYTHONPATH" => pyroot);
                                   stdout = devnull, stderr = devnull))
        if !haspy
            @test_skip "analysis-env Python (ome_types/zarr/dask) not importable"
        else
            mktempdir() do d
                a, b = joinpath(d, "py.ome.zarr"), joinpath(d, "jl.ome.zarr")
                script = joinpath(d, "fixture.py")
                write(script, """
import sys, numpy as np, dask.array as da, ome_types, zarr
import cecelia.utils.zarr_utils as zu, cecelia.utils.ome_xml_utils as ox
from cecelia.utils.dim_utils import DimUtils
XML = '''<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="x"><Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16"
    SizeT="3" SizeC="2" SizeZ="5" SizeY="4" SizeX="3"
    PhysicalSizeX="0.5" PhysicalSizeXUnit="\\u00b5m" PhysicalSizeY="0.5" PhysicalSizeYUnit="\\u00b5m"
    PhysicalSizeZ="2.0" PhysicalSizeZUnit="\\u00b5m" TimeIncrement="10.0" TimeIncrementUnit="s">
    <Channel ID="Channel:0:0" SamplesPerPixel="1"/><Channel ID="Channel:0:1" SamplesPerPixel="1"/>
    <MetadataOnly/></Pixels></Image></OME>'''
SHAPE = (3, 2, 5, 4, 3)
du = DimUtils(ome_types.from_xml(XML), use_channel_axis=True)
du.calc_image_dimensions(SHAPE)
for p in sys.argv[1:3]:
    zu.create_multiscales(da.from_array(np.zeros(SHAPE, dtype=np.uint16), chunks=SHAPE),
                          p, dim_utils=du, nscales=2)
    # Reproduce the shipped bug on BOTH stores: NGFF t back to the unit-less 1.0 placeholder and a
    # sidecar carrying someone else's numbers, exactly what a half-landed sync left behind.
    g = zarr.open_group(p, mode='a'); ms = g.attrs['multiscales']
    ms[0]['axes'] = [{k: v for k, v in ax.items() if not (ax['name'] == 't' and k == 'unit')}
                     for ax in ms[0]['axes']]
    for ds in ms[0]['datasets']:
        ds['coordinateTransformations'][0]['scale'][0] = 1.0
    g.attrs['multiscales'] = ms
    stale = ome_types.from_xml(XML)
    stale.images[0].pixels.physical_size_z = 99.0
    stale.images[0].pixels.time_increment  = 99.0
    ox.write_ome_xml(p, stale)
zu.write_calibration(sys.argv[1], du)     # the PYTHON stamp, on the first store only
""")
                @test success(pipeline(addenv(`python $script $a $b`, "PYTHONPATH" => pyroot);
                                       stdout = devnull, stderr = devnull))

                # the JULIA stamp, same calibration, on the second store
                Cecelia.sync_zarr_calibration!(b, Dict{String,Any}(
                    "PhysicalSizeX" => 0.5, "PhysicalSizeY" => 0.5, "PhysicalSizeZ" => 2.0,
                    "PhysicalSizeUnit" => "micrometer",
                    "TimeIncrement" => 10.0, "TimeIncrementUnit" => "second"))

                # NGFF half — identical axes (incl. units) and identical per-level scales.
                # Compared field-by-field, not as raw JSON: the two writers emit the same keys in
                # different ORDER, which is meaningless to every reader.
                za, zb = (JSON3.read(read(joinpath(p, ".zattrs"), String))[:multiscales][1]
                          for p in (a, b))
                axkey(ms) = [(string(get(ax, :name, "")), string(get(ax, :type, "")),
                              string(get(ax, :unit, ""))) for ax in ms[:axes]]
                sckey(ms) = [(string(get(d, :path, "")),
                              collect(Float64, first(d[:coordinateTransformations])[:scale]))
                             for d in ms[:datasets]]
                @test axkey(za) == axkey(zb)
                @test sckey(za) == sckey(zb)
                # …and it is the RIGHT answer, not merely the same wrong one
                @test read_ome_metadata(a)["TimeIncrement"] == 10.0
                @test read_ome_metadata(a)["PhysicalSizeZ"] == 2.0

                # OME-XML half — same <Pixels> calibration attrs from both stamps
                for attr in ("PhysicalSizeX", "PhysicalSizeY", "PhysicalSizeZ",
                             "PhysicalSizeZUnit", "TimeIncrement", "TimeIncrementUnit")
                    vals = map((a, b)) do p
                        tag = match(r"<Pixels\b[^>]*>",
                                    read(joinpath(p, "OME", "METADATA.ome.xml"), String)).match
                        m = match(Regex(attr * "=\"([^\"]*)\""), tag)
                        isnothing(m) ? nothing : m.captures[1]
                    end
                    @test vals[1] == vals[2] != nothing
                end
                @test occursin("TimeIncrement=\"10.0\"",
                               read(joinpath(a, "OME", "METADATA.ome.xml"), String))
            end
        end
    end

    # ── _merge_zarr_meta_into_ccid!: overwrite=true is authoritative; false is fill-only ──
    @testset "merge fill-only vs overwrite" begin
        proj = create_project!(name = "meta-merge-$(rand(1000:9999))")
        s    = add_set!(proj; name = "set")
        # Simulate an ImageJ-corrected image: PhysicalSizeZ + the ccid-only PhysicalSizeZ_raw marker
        img  = add_image!(s; name = "img", meta = Dict{String,Any}(
            "PhysicalSizeZ" => 3.0, "PhysicalSizeZ_raw" => 0.6))

        # Fill-only backfill: existing keys survive, genuinely-missing ones get filled.
        Cecelia._merge_zarr_meta_into_ccid!(img,
            Dict{String,Any}("PhysicalSizeZ" => 0.6, "PhysicalSizeX" => 0.5); overwrite = false)
        r = init_object(proj.uid, img.uid)
        @test r.meta["PhysicalSizeZ"] == 3.0                     # NOT reverted to the raw 0.6
        @test r.meta["PhysicalSizeZ_raw"] == 0.6                 # marker NOT dropped
        @test r.meta["PhysicalSizeX"] == 0.5                     # filled (was absent)

        # Authoritative import merge: clears derived keys, takes the fresh read verbatim.
        Cecelia._merge_zarr_meta_into_ccid!(r,
            Dict{String,Any}("PhysicalSizeZ" => 0.6); overwrite = true)
        r2 = init_object(proj.uid, img.uid)
        @test r2.meta["PhysicalSizeZ"] == 0.6
        @test !haskey(r2.meta, "PhysicalSizeZ_raw")              # zombie marker cleared
        rm(proj.root; recursive = true)
    end

    # ── A re-import must NOT revert renamed channels ────────────────────────────────────────────
    # bioformats2raw always writes the vendor's own CH1..CHn into the store's omero labels, so the
    # fresh read never reproduces a rename — and saved task params reference channels by name.
    @testset "import keeps renamed channel names" begin
        proj = create_project!(name = "meta-chan-$(rand(1000:9999))")
        s    = add_set!(proj; name = "set")
        img  = add_image!(s; name = "img")

        # First import: nothing stored yet → take the fresh read.
        Cecelia._merge_zarr_meta_into_ccid!(img,
            Dict{String,Any}("SizeC" => 4,
                             "channel_names" => ["CH1", "CH2", "CH3", "CH4"]); overwrite = true)
        r = init_object(proj.uid, img.uid)
        @test channel_names(r) == ["CH1", "CH2", "CH3", "CH4"]

        # The user renames them (the API path).
        set_channel_names!(r, ["SHG", "nuc-GFP", "mem-TOM", "CD169-Kat"]; check_length = false)
        save!(r)

        # Re-import of the same source: same channel count → the renames survive, and the task
        # says so rather than reverting silently.
        logged = String[]
        Cecelia._merge_zarr_meta_into_ccid!(init_object(proj.uid, img.uid),
            Dict{String,Any}("SizeC" => 4,
                             "channel_names" => ["CH1", "CH2", "CH3", "CH4"]);
            overwrite = true, on_log = l -> push!(logged, l))
        r2 = init_object(proj.uid, img.uid)
        @test channel_names(r2) == ["SHG", "nuc-GFP", "mem-TOM", "CD169-Kat"]
        @test any(l -> occursin("Kept the existing channel names", l), logged)

        # A source whose channel count changed: the stored list cannot describe it → take the fresh
        # names (and stay silent, nothing was preserved).
        logged2 = String[]
        Cecelia._merge_zarr_meta_into_ccid!(r2,
            Dict{String,Any}("SizeC" => 2, "channel_names" => ["CH1", "CH2"]);
            overwrite = true, on_log = l -> push!(logged2, l))
        r3 = init_object(proj.uid, img.uid)
        @test channel_names(r3) == ["CH1", "CH2"]
        @test isempty(logged2)

        # Fill-only (resync) never touches channel names at all, whatever the count.
        Cecelia._merge_zarr_meta_into_ccid!(r3,
            Dict{String,Any}("channel_names" => ["nope", "nope2"]); overwrite = false)
        @test channel_names(init_object(proj.uid, img.uid)) == ["CH1", "CH2"]

        rm(proj.root; recursive = true)
    end

    # ── resync_ome_meta! end-to-end: fill-only into ccid, then push the merge back to the zarr ──
    @testset "resync_ome_meta! fill-only" begin
        proj = create_project!(name = "meta-resync-$(rand(1000:9999))")
        s    = add_set!(proj; name = "set")
        img  = add_image!(s; name = "img", meta = Dict{String,Any}(
            "PhysicalSizeZ" => 3.0, "PhysicalSizeZ_raw" => 0.6))   # ImageJ-corrected, ccid-only

        # Register a "default" zarr on disk carrying the RAW (pre-correction) calibration.
        zdir = joinpath(img_zero_dir(img), "img.ome.zarr")
        make_zarr(zdir; axes = ["z", "y", "x"], level_scales = [[0.6, 0.5, 0.5]],
                  units = Dict("x" => "micrometer", "y" => "micrometer", "z" => "micrometer"),
                  shape = [1, 8, 8])
        img.filepath["default"]         = "img.ome.zarr"
        img.filepath[VERSIONED_ACTIVE_KEY] = "default"
        save!(img)

        @test resync_ome_meta!(init_object(proj.uid, img.uid))
        r = init_object(proj.uid, img.uid)
        @test r.meta["PhysicalSizeZ"] == 3.0                     # correction survives resync
        @test r.meta["PhysicalSizeZ_raw"] == 0.6                 # marker survives
        @test r.meta["PhysicalSizeUnit"] == "micrometer"         # genuinely-missing field filled
        @test r.meta["PhysicalSizeX"] == 0.5
        # …and the ccid-only correction is pushed BACK into the store, so the two agree
        @test read_ome_metadata(zdir)["PhysicalSizeZ"] == 3.0
        rm(proj.root; recursive = true)
    end

    # ── resync_ome_meta! repairs a flat store whose NGFF calibration never landed ──
    # The shipped case: the 8-bit import wrote a flat store, `sync_zarr_calibration!` silently
    # skipped its NGFF half, and the store ended up disagreeing with its own OME-XML. ccid.json
    # has the right number, so resync is the repair path — no re-import.
    @testset "resync_ome_meta! repairs a stale flat store" begin
        proj = create_project!(name = "meta-flat-$(rand(1000:9999))")
        s    = add_set!(proj; name = "set")
        img  = add_image!(s; name = "img", meta = Dict{String,Any}(
            "TimeIncrement" => 10.0, "TimeIncrementUnit" => "second",
            "PhysicalSizeUnit" => "micrometer"))

        zdir = joinpath(img_zero_dir(img), "img.ome.zarr")
        make_zarr(zdir; layout = :flat, axes = ["t", "y", "x"],
                  level_scales = [[1.0, 0.5, 0.5]], shape = [180, 8, 8],
                  units = Dict("t" => "second", "x" => "micrometer", "y" => "micrometer"),
                  planes = [(z = 0, t = 1, dt = 0.0, unit = "s")])
        img.filepath["default"]            = "img.ome.zarr"
        img.filepath[VERSIONED_ACTIVE_KEY] = "default"
        save!(img)

        @test read_ome_metadata(zdir)["TimeIncrement"] == 1.0     # stale placeholder, pre-repair
        @test resync_ome_meta!(init_object(proj.uid, img.uid))
        @test read_ome_metadata(zdir)["TimeIncrement"] == 10.0    # NGFF now matches ccid
        @test occursin("TimeIncrement=\"10.0\"",
                       read(joinpath(zdir, "OME", "METADATA.ome.xml"), String))
        @test init_object(proj.uid, img.uid).meta["TimeIncrement"] == 10.0  # ccid untouched
        rm(proj.root; recursive = true)
    end
end

@testset "QC framework" begin
    # ── The QC copy catalog (app/src/qc.jl → QC_TEXT) ─────────────────────────────────────────
    #
    # QC prose used to live inline in the analysis functions, which made it the least reviewable
    # copy in the app. It now sits in one table; these pin the contract that table has to keep.
    @testset "copy catalog" begin
        @test length(Cecelia.QC_TEXT) > 15

        # Placeholders are filled from keywords, and the emitted `code` is independent of the
        # catalog key — the two cases the `key` argument exists for.
        f = Cecelia.qc_finding("warn", "drift.canvas_expansion";
                               key = "output.canvas_expansion", pct = 42)
        @test f["code"] == "drift.canvas_expansion"
        @test f["short"] == "Output canvas grew +42% in XY"

        # Loud failures, not a user-visible "{channel}".
        @test_throws ErrorException Cecelia.qc_text("no.such.key")
        @test_throws ErrorException Cecelia.qc_text("output.canvas_expansion")  # missing `pct`

        # House style (docs/UI.md): `short` is a fragment, `long` is a sentence. Checked here
        # because the frontend ratchet cannot see Julia strings.
        bad_short = [k for (k, v) in Cecelia.QC_TEXT if occursin(r"[^.]\.$", v.short)]
        @test isempty(bad_short)
        no_period = [k for (k, v) in Cecelia.QC_TEXT if !endswith(v.long, ".")]
        @test isempty(no_period)

        # Every placeholder the catalog uses must be one a caller actually passes; a typo'd
        # `{metrics}` would otherwise only surface when that finding fires in production.
        KNOWN = Set(["channel", "pct", "unit", "dims", "metric", "value", "dir", "median"])
        unknown = [m.captures[1] for (_, v) in Cecelia.QC_TEXT
                   for m in eachmatch(r"\{(\w+)\}", v.short * " " * v.long)
                   if !(m.captures[1] in KNOWN)]
        @test isempty(unknown)

        # The inputs are persisted, not just the output — that's what read-time rendering needs.
        g = Cecelia.qc_finding("info", "hmm.dominant_state"; pct = 91)
        @test g["key"] == "hmm.dominant_state" && g["subs"]["pct"] == 91
    end

    # ── Read-time rendering ───────────────────────────────────────────────────────────────────
    #
    # The point of the catalog: fixing a wording should reach QC that is ALREADY on disk, without
    # re-running the analysis that produced it. (And, later, a locale switch does the same.)
    @testset "findings re-render on read" begin
        img = CciaImage(; dir = mktempdir())
        write_qc(img, "behaviour.hmmStates", "default",
                 [Cecelia.qc_finding("info", "hmm.dominant_state"; pct = 91)])

        @test read_qc(img, "behaviour.hmmStates", "default")["findings"][1]["short"] ==
              "One state holds 91% of cells"

        # Edit the catalog; the banked file on disk is NOT rewritten.
        orig = Cecelia.QC_TEXT["hmm.dominant_state"]
        try
            Cecelia.QC_TEXT["hmm.dominant_state"] =
                (short = "{pct}% of cells in one state", long = orig.long)
            doc = read_qc(img, "behaviour.hmmStates", "default")
            @test doc["findings"][1]["short"] == "91% of cells in one state"
            # Symbol access is what lab_log_context/qc_cohort use — must survive the rebuild.
            @test String(get(doc["findings"][1], :short, "")) == "91% of cells in one state"
            @test get(doc, :funName, "") == "behaviour.hmmStates"
        finally
            Cecelia.QC_TEXT["hmm.dominant_state"] = orig
        end

        # A catalog entry that disappears must fall back to the stored snapshot, not blow up the
        # read — this is a data path shared by every image in the payload.
        saved = Cecelia.QC_TEXT["hmm.dominant_state"]
        try
            delete!(Cecelia.QC_TEXT, "hmm.dominant_state")
            @test read_qc(img, "behaviour.hmmStates", "default")["findings"][1]["short"] ==
                  "One state holds 91% of cells"
        finally
            Cecelia.QC_TEXT["hmm.dominant_state"] = saved
        end
    end

    @testset "pre-catalog sidecars are read unchanged" begin
        # Findings banked before the catalog carry no `key`; they must pass through verbatim.
        img = CciaImage(; dir = mktempdir())
        write_qc(img, "mycat.myTask", "default",
                 [qc_finding("warn", "legacy.code", "Old short", "Old long.")])
        doc = read_qc(img, "mycat.myTask", "default")
        @test doc["findings"][1]["short"] == "Old short"
        @test doc["findings"][1]["long"] == "Old long."
        @test !haskey(doc["findings"][1], :key)
    end

    @testset "sidecar round-trip" begin
        img = CciaImage(; dir = mktempdir())
        f = qc_finding("warn", "demo.code", "short text", "long text"; detail = Dict("k" => 1))
        @test f["level"] == "warn" && f["code"] == "demo.code"

        p = write_qc(img, "cleanupImages.driftCorrect", "driftCorrected", [f];
                     source = Dict("shape" => [1, 2, 3]))
        @test isfile(p)
        @test occursin(joinpath("qc", "cleanupImages.driftCorrect", "driftCorrected.json"), p)

        doc = read_qc(img, "cleanupImages.driftCorrect", "driftCorrected")
        @test length(doc["findings"]) == 1
        @test doc["findings"][1]["code"] == "demo.code"

        all = read_all_qc(img)
        @test haskey(all, "cleanupImages.driftCorrect/driftCorrected")

        # no-value_name → falls back to the default key
        write_qc(img, "some.task", "", Dict{String,Any}[])
        @test isfile(qc_path(img, "some.task", VERSIONED_DEFAULT_VAL))
    end

    @testset "canvas-expansion check" begin
        order = "TCZYX"
        # fHqhyb: XY +42%/+21% → flagged; Z doubling is ignored
        bad = qc_canvas_expansion([94, 4, 13, 512, 512], [94, 4, 26, 728, 618], order)
        @test bad !== nothing && bad["code"] == "output.canvas_expansion"
        # LUkCpP (normal): XY +6%/+3% → not flagged even though Z grew +46%
        @test qc_canvas_expansion([64, 4, 13, 512, 512], [64, 4, 19, 541, 527], order) === nothing
    end

    @testset "drift findings" begin
        base = Dict{String,Any}("dimOrder" => "TCZYX", "shiftAxes" => ["Z", "Y", "X"])
        smooth = [[0.0, 1.0, 1.0] for _ in 1:20]
        spiky  = copy(smooth); spiky[16] = [0.0, 120.0, 90.0]   # jump at frame 16

        # bad ref: canvas ballooned AND a spike → both findings
        meta_bad = merge(base, Dict("sourceShape" => [20, 4, 13, 512, 512],
                                    "outputShape" => [20, 4, 26, 728, 618], "shifts" => spiky))
        fb, _, _ = Cecelia._drift_qc_findings(meta_bad)
        codes = Set(f["code"] for f in fb)
        @test "drift.canvas_expansion" in codes
        @test "drift.jump" in codes
        jump = first(f for f in fb if f["code"] == "drift.jump")
        @test jump["detail"]["atT"] == 15                       # 0-based frame index of the spike

        # good ref: modest canvas, smooth trajectory → no findings
        meta_ok = merge(base, Dict("sourceShape" => [20, 4, 13, 512, 512],
                                   "outputShape" => [20, 4, 19, 541, 527], "shifts" => smooth))
        fo, _, _ = Cecelia._drift_qc_findings(meta_ok)
        @test isempty(fo)

        # A sidecar written before the residual existed carries no `residualRms`. It must not be
        # read as a perfect registration — no finding, and no metric either (a banked 0 would drag
        # the cohort median toward "everything registered").
        @test !haskey(Cecelia._drift_qc_metrics(meta_ok, [20, 4, 13, 512, 512],
                                                [20, 4, 19, 541, 527]), "residualPx")
    end

    @testset "drift reliability findings" begin
        # The registration disagreeing with ITSELF. This is the one check that can tell a broken
        # registration from a movie that genuinely moved a lot — the other two read the trajectory
        # and cannot. Numbers are the measured ones: every movie that registered on this machine
        # sat at 0.13–0.39 px, `4kS67f/fHqhyb` at 24 px.
        base = Dict{String,Any}("dimOrder" => "TCZYX", "shiftAxes" => ["Z", "Y", "X"],
                                "sourceShape" => [20, 4, 13, 512, 512],
                                "outputShape" => [20, 4, 19, 541, 527],
                                "shifts" => [[0.0, 1.0, 1.0] for _ in 1:20])

        good = merge(base, Dict("residualRms" => 0.39, "residualP90" => 0.5,
                                "nPairs" => 57, "nRejected" => 0))
        @test isempty(Cecelia._drift_qc_findings(good)[1])

        bad = merge(base, Dict("residualRms" => 24.3, "residualP90" => 12.6,
                               "nPairs" => 57, "nRejected" => 8))
        fb = Cecelia._drift_qc_findings(bad)[1]
        unrel = first(f for f in fb if f["code"] == "drift.unreliable")
        @test unrel["level"] == "warn"
        @test unrel["detail"]["nRejected"] == 8
        @test occursin("24.3", unrel["short"])

        # Frames no measurement survived for: their position is predicted, and the sidecar says so.
        gappy = merge(good, Dict("interpolated" => [4, 9]))
        fg = Cecelia._drift_qc_findings(gappy)[1]
        interp = first(f for f in fg if f["code"] == "drift.unregistered_frames")
        @test interp["detail"]["frames"] == [4, 9]
        @test occursin("2 frame", interp["short"])

        # Metrics: the cohort-comparable numbers, and only the ones actually measured.
        m = Cecelia._drift_qc_metrics(bad, [20, 4, 13, 512, 512], [20, 4, 19, 541, 527])
        @test m["residualPx"] == 24.3
        @test m["canvasExpansion"] > 1.0
        @test m["framesInterpolated"] == 0
        # everything the cohort pass is told to aggregate must actually be banked
        for k in Cecelia.COHORT_METRICS["cleanupImages.driftCorrect"]
            @test haskey(m, k)
        end
    end

    @testset "OIR companion-file staging" begin
        # REAL Olympus naming: the registered file already ends in _NNNN.oir and companions are
        # EXTENSIONLESS <mainstem>_00001, _00002, … (this is what shipped broken — only the main
        # matched, so bioformats saw a fraction of the timepoints).
        real = ["M1a-res_0001.oir", "M1a-res_0001_00001", "M1a-res_0001_00002", "M1a-res_0001_00045",
                "M1a-res_0002.oir",           # a DIFFERENT acquisition — must NOT be grabbed
                "M1a-res_0001_notes.txt"]     # non-numeric sibling — excluded
        @test Set(Cecelia._companion_files(real, "M1a-res_0001.oir")) ==
              Set(["M1a-res_0001.oir", "M1a-res_0001_00001", "M1a-res_0001_00002", "M1a-res_0001_00045"])

        # extensioned companions (Img.oir + Img_00001.oir …); sibling Img2 / non-numbered excluded
        names = ["Img.oir", "Img_00001.oir", "Img_00002.oir",
                 "Img2.oir", "Img_processed.oir", "Other.oir", "notes.txt"]
        @test Set(Cecelia._companion_files(names, "Img.oir")) ==
              Set(["Img.oir", "Img_00001.oir", "Img_00002.oir"])
        # regex metacharacters in the stem (the `basal+NECA` bug): literal match, no injection
        plus = ["basal+NECA.oir", "basal+NECA_00001", "basal+NECB.oir"]
        @test Set(Cecelia._companion_files(plus, "basal+NECA.oir")) ==
              Set(["basal+NECA.oir", "basal+NECA_00001"])
        # single self-contained file → just itself
        @test Cecelia._companion_files(["a.tif", "b.tif"], "a.tif") == ["a.tif"]

        # chunked yielding copy is byte-identical (incl. a size that isn't a chunk multiple)
        src = tempname(); dst = tempname()
        data = rand(UInt8, 3 * 1024 * 1024 + 777)
        write(src, data)
        copied = Ref(0)
        Cecelia._copy_file_yielding(src, dst; chunk = 1024 * 1024, on_bytes = n -> (copied[] += n))
        @test read(dst) == data
        @test copied[] == length(data)
        rm(src; force = true); rm(dst; force = true)
    end

    @testset "import metrics" begin
        # base import metric — present for EVERY import (from SizeC/SizeZ/SizeT). An odd channel
        # count or dimensionality vs cohort peers means the wrong file was imported.
        bm = Cecelia.import_metrics(Dict{String,Any}("SizeC" => 4, "SizeZ" => 13, "SizeT" => 20))
        @test bm == Dict{String,Any}("nChannels" => 4, "nZ" => 13, "nT" => 20)
        @test Cecelia.import_metrics(Dict{String,Any}()) === nothing

        # partial metadata: only the keys that are present are banked
        @test Cecelia.import_metrics(Dict{String,Any}("SizeC" => 2)) ==
              Dict{String,Any}("nChannels" => 2)

        # JSON3 round-trip — the real path (meta read back from ccid.json has Symbol keys)
        rt     = JSON3.read(JSON3.write(Dict{String,Any}("SizeC" => 4, "SizeZ" => 13)))
        rtmeta = Dict{String,Any}(String(k) => v for (k, v) in rt)
        @test Cecelia.import_metrics(rtmeta)["nChannels"] == 4

    end

    @testset "clipping-at-acquisition findings" begin
        # `sigfrac` is the one the finding gates on — clipped voxels over SIGNAL voxels. `frac` (over
        # ALL voxels) is still banked, so both are set.
        mk(i, sat; top = 4095, frac = 0.0, n = 0, sigfrac = 0.0) =
            Dict{String,Any}("index" => i, "saturated" => sat, "topValue" => top,
                             "topCount" => n, "topFrac" => frac, "clippedSignalFrac" => sigfrac)
        meta = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [
            mk(0, false; top = 1032, frac = 1.0e-6, sigfrac = 1.0e-5),
            # unmistakably clipped: 2% of this channel's SIGNAL voxels piled at the 12-bit ceiling
            mk(1, true;  top = 4095, frac = 0.00018, n = 534, sigfrac = 0.02),
            mk(2, false; top = 2854, frac = 1.0e-6, sigfrac = 1.0e-5),
        ]))

        fs = Cecelia.saturation_qc_findings(meta)
        @test length(fs) == 1                              # only the clipped channel
        @test fs[1]["code"] == "import.channel_saturated"
        @test fs[1]["level"] == "warn"                     # advisory, never a gate
        @test fs[1]["detail"]["channel"] == 1
        # the effective ceiling is reported because it is NOT the dtype maximum on 12-bit-in-16-bit
        @test fs[1]["detail"]["topValue"] == 4095.0
        # the COUNT is what a reader can judge; the fraction is ~1e-6 and rounds away
        @test fs[1]["detail"]["clippedVoxels"] == 534.0

        @test fs[1]["detail"]["clippedSignalPct"] == 2.0     # reported against SIGNAL, not all voxels

        m = Cecelia.saturation_metrics(meta)
        @test m["nChannelsSaturated"] == 1
        @test m["maxClippedFrac"] == 0.00018
        @test m["maxClippedSignalFrac"] == 0.02

        # the check didn't run (pre-existing image, or a non-integer store) → say nothing at all
        @test isempty(Cecelia.saturation_qc_findings(Dict{String,Any}()))
        @test Cecelia.saturation_metrics(Dict{String,Any}()) === nothing
        @test isempty(Cecelia.saturation_qc_findings(
            Dict{String,Any}("saturation" => Dict{String,Any}())))

        # nothing clipped → no findings, but the metrics still bank (a measured zero is a result, and
        # the cohort needs it to tell "clean" apart from "not checked")
        clean = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [mk(0, false)]))
        @test isempty(Cecelia.saturation_qc_findings(clean))
        @test Cecelia.saturation_metrics(clean)["nChannelsSaturated"] == 0

        # TRACE clipping: structurally detected, but far too small to act on. This is the real measured
        # case — 4 of 36 channels across nine kSUFux movies sit at 1.1-1.4e-6, i.e. ~500 voxels of
        # 377 M. No finding (telling someone to lower the gain over 500 voxels is not actionable), but
        # the metric MUST still record it: the cohort comparison is relative, so it is what surfaces an
        # image clipping far more than its session peers.
        # 7.2e-5 of SIGNAL voxels is the worst real case measured across nine movies — three orders
        # below the smoke-alarm level, so it must not warn while still being banked.
        trace = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [
            mk(0, true; top = 4095, frac = 1.4e-6, n = 534, sigfrac = 7.2e-5),
        ]))
        @test isempty(Cecelia.saturation_qc_findings(trace))
        tm = Cecelia.saturation_metrics(trace)
        @test tm["nChannelsSaturated"] == 1
        @test tm["maxClippedFrac"] == 1.4e-6
        @test tm["maxClippedSignalFrac"] == 7.2e-5

        # …and just above the level it does warn
        material = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [
            mk(0, true; top = 4095, frac = 2.0e-4, n = 75_000, sigfrac = 1.1e-2),
        ]))
        @test length(Cecelia.saturation_qc_findings(material)) == 1

        # the ALL-voxel fraction must not be what decides it: a channel with a large all-voxel fraction
        # but trace signal clipping stays quiet, and vice versa. This is the whole point of the change.
        allvox = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [
            mk(0, true; top = 4095, frac = 0.5, n = 999, sigfrac = 1.0e-6),
        ]))
        @test isempty(Cecelia.saturation_qc_findings(allvox))

        # a channel with no recorded fraction is not guessed at
        noneframe = Dict{String,Any}("saturation" => Dict{String,Any}("channels" => [
            Dict{String,Any}("index" => 0, "saturated" => true, "topValue" => 4095),
        ]))
        @test isempty(Cecelia.saturation_qc_findings(noneframe))

        # JSON3 round-trip — the real path: persisted ccid meta comes back with Symbol keys
        rt   = JSON3.read(JSON3.write(meta))
        rtm  = Dict{String,Any}(String(k) => v for (k, v) in rt)
        @test length(Cecelia.saturation_qc_findings(rtm)) == 1
        @test Cecelia.saturation_metrics(rtm)["nChannelsSaturated"] == 1

        # the finding renders — a `{channel}` placeholder with no substitution throws (see qc_text)
        @test occursin("1", fs[1]["short"])
    end

    # Julia and Python each carry the compressor table — a bioformats2raw command line cannot read a
    # Python constant, and the API serves the list to Settings. Same arrangement as the calibration
    # writers (CLAUDE.md -> *Calibration - three copies, one stamp*): two copies, one contract test.
    @testset "image compressor: the two tables agree" begin
        py = read(joinpath(@__DIR__, "..", "..", "python", "cecelia", "utils", "zarr_utils.py"), String)

        # every Julia choice exists in the Python dict with the SAME cname/clevel/shuffle
        for c in Cecelia.IMAGE_COMPRESSOR_CHOICES
            m = match(Regex("'" * c.name * "': *dict\\(cname='(\\w+)', *clevel=(\\d+), *shuffle='(\\w+)'\\)"), py)
            @test !isnothing(m)
            isnothing(m) && continue
            @test m.captures[1] == c.cname
            @test parse(Int, m.captures[2]) == c.clevel
            @test (m.captures[3] == "shuffle") == c.shuffle
        end

        # ...and neither side has a choice the other lacks
        py_names = Set(String(m.captures[1]) for m in
                       eachmatch(r"'([\w-]+)': +dict\(cname=", py))
        @test py_names == Set(c.name for c in Cecelia.IMAGE_COMPRESSOR_CHOICES)

        # the defaults match, on both sides
        @test occursin("IMAGE_COMPRESSOR_DEFAULT = '$(Cecelia.IMAGE_COMPRESSOR_DEFAULT)'", py)
        @test Cecelia.image_compressor() in [c.name for c in Cecelia.IMAGE_COMPRESSOR_CHOICES]

        # The blosc `shuffle` property is spelled DIFFERENTLY per bioformats2raw version, and each
        # version hard-fails on the other's spelling (0.12.0 swapped jzarr → zarr-java). Detected from
        # the bundled jar; asserted here against synthetic lib dirs so this is hermetic — CI has no
        # bioformats2raw install at all, and that must resolve to the current spelling, not error.
        mktempdir() do d
            legacy = joinpath(d, "legacy"); mkpath(legacy)
            touch(joinpath(legacy, "jzarr-0.4.2.jar"))
            @test Cecelia.bf2raw_shuffle_values(legacy) == ("1", "0")

            modern = joinpath(d, "modern"); mkpath(modern)
            touch(joinpath(modern, "zarr-java-0.1.3.jar"))
            @test Cecelia.bf2raw_shuffle_values(modern) == ("shuffle", "noshuffle")

            # neither jar, and a missing dir → the current spelling (a wrong guess fails loudly)
            empty_dir = joinpath(d, "empty"); mkpath(empty_dir)
            @test Cecelia.bf2raw_shuffle_values(empty_dir) == ("shuffle", "noshuffle")
            @test Cecelia.bf2raw_shuffle_values(joinpath(d, "absent")) == ("shuffle", "noshuffle")
        end
        # NOT the literal "1"/"0": that is the legacy spelling, and hardcoding it here is what would
        # hide the incompatibility. Assert against whatever THIS install wants.
        shuf_on, shuf_off = Cecelia.bf2raw_shuffle_values(Cecelia._bf2raw_lib_dir())
        flags = Cecelia.bf2raw_compression_flags("zstd-shuffle")
        @test flags[1:2] == ["--compression", "blosc"]
        props = Dict(split(flags[i], "=")[1] => split(flags[i], "=")[2] for i in 4:2:length(flags))
        @test props == Dict("cname" => "zstd", "clevel" => "3", "shuffle" => shuf_on)
        @test Dict(split(f, "=")[1] => split(f, "=")[2]
                   for f in Cecelia.bf2raw_compression_flags("zstd")[4:2:end])["shuffle"] == shuf_off
        # `byteshuffle` is the alias 0.12's README documents for byte shuffle, and it is BROKEN
        # upstream (null enum → NPE → every chunk write fails). It must never be emitted.
        @test !any(occursin("byteshuffle", f) for f in flags)

        # an unknown name falls back rather than erroring - a typo in custom.toml must not fail a
        # multi-hour import
        @test Cecelia.bf2raw_compression_flags("nope") ==
              Cecelia.bf2raw_compression_flags(Cecelia.IMAGE_COMPRESSOR_DEFAULT)
        @test_throws ArgumentError Cecelia.set_image_compressor!("nope")
    end

    # Two tables on one Settings page, each varying what the other pins: the compressor rows were all
    # measured in ONE layout, the layout rows all with ONE codec. Neither set of sizes is comparable to
    # the other's without that, so each caption must name the variable it held fixed. Asserted because a
    # caption is exactly the kind of string that gets shortened later by someone who reads it as prose.
    @testset "measured-on captions name the other table's variable" begin
        cmp_cap = Cecelia.IMAGE_COMPRESSOR_MEASURED_ON
        lay_cap = Cecelia.STORE_LAYOUT_MEASURED_ON

        # the compressor was measured in one LAYOUT: format, chunk-key style, and chunk shape
        @test occursin("zarr v", cmp_cap)
        @test occursin("keys", cmp_cap)
        @test occursin("chunks", cmp_cap)

        # ...and the layouts with one CODEC — named, and actually the default the other table serves
        # (derived, not spelled out, so re-measuring under a new default has to update the caption)
        default_cname = first(c.cname for c in Cecelia.IMAGE_COMPRESSOR_CHOICES
                              if c.name == Cecelia.IMAGE_COMPRESSOR_DEFAULT)
        @test occursin(default_cname, lay_cap)

        # both stay one short line — this renders as a field hint, not a paragraph (docs/UI.md)
        for cap in (cmp_cap, lay_cap)
            @test !occursin("\n", cap)
            @test length(cap) <= 90
        end
    end

    @testset "count metrics" begin
        # pure: distinct tracks, mean cells/track, tracked-cell total; untracked = missing/NaN/≤0
        nt, ml, ntc = track_count_metrics([1, 1, 1, 2, 2, 0, -1, NaN, missing, 3])
        @test nt == 3                       # tracks 1, 2, 3
        @test ntc == 6                      # 3 + 2 + 1 cells tracked
        @test ml ≈ 2.0                      # 6 cells / 3 tracks

        # no tracks at all → zeros (drives the "No tracks formed" advisory)
        @test track_count_metrics([0, NaN, missing]) == (0, 0.0, 0)
        @test track_count_metrics(Float64[]) == (0, 0.0, 0)

        # floats round to the nearest track id
        n2, _, c2 = track_count_metrics([1.0, 1.0, 2.0])
        @test (n2, c2) == (2, 3)

        # segment counts → findings: 0 base cells warns; any base count is clean
        f0, p0 = Cecelia.segment_qc_findings(Dict("base" => 0))
        @test p0 == 0 && length(f0) == 1 && f0[1]["code"] == "segment.no_cells"
        fN, pN = Cecelia.segment_qc_findings(Dict("base" => 812, "nuc" => 790))
        @test pN == 812 && isempty(fN)
        # no explicit "base" key → primary falls back to the sole type's count
        _, pf = Cecelia.segment_qc_findings(Dict("nuc" => 5))
        @test pf == 5

        # metadata calibration findings (port of the old frontend fieldIssues) — codes + field
        codes(fs) = [f["code"] for f in fs]; fields(fs) = [f["detail"]["field"] for f in fs]
        # clean 3D timelapse with units → nothing
        @test isempty(Cecelia.metadata_qc_findings(Dict("SizeZ"=>10,"SizeT"=>5,
            "PhysicalSizeX"=>0.5,"PhysicalSizeY"=>0.5,"PhysicalSizeZ"=>2.0,"PhysicalSizeUnit"=>"micron",
            "TimeIncrement"=>30.0,"TimeIncrementUnit"=>"second")))
        # z stack, no z spacing → z_spacing_unknown
        @test codes(Cecelia.metadata_qc_findings(Dict("SizeZ"=>10,"PhysicalSizeX"=>0.5,"PhysicalSizeUnit"=>"micron"))) ==
              ["metadata.z_spacing_unknown"]
        # auto-corrected z (PhysicalSizeZ_raw marker) → z_spacing_corrected
        @test codes(Cecelia.metadata_qc_findings(Dict("SizeZ"=>10,"PhysicalSizeX"=>0.5,"PhysicalSizeZ"=>2.0,
            "PhysicalSizeUnit"=>"micron","PhysicalSizeZ_raw"=>99.0))) == ["metadata.z_spacing_corrected"]
        # unusual z:xy ratio (100:1 > 50) → z_spacing_unusual
        @test codes(Cecelia.metadata_qc_findings(Dict("SizeZ"=>10,"PhysicalSizeX"=>1.0,"PhysicalSizeZ"=>100.0,
            "PhysicalSizeUnit"=>"micron"))) == ["metadata.z_spacing_unusual"]
        # timelapse, no interval → frame_interval_unknown; string values coerce
        @test codes(Cecelia.metadata_qc_findings(Dict("SizeT"=>"8","PhysicalSizeX"=>0.5,"PhysicalSizeY"=>0.5,
            "PhysicalSizeUnit"=>"micron"))) == ["metadata.frame_interval_unknown"]
        # interval present, no unit → frame_interval_no_unit
        @test codes(Cecelia.metadata_qc_findings(Dict("SizeT"=>8,"TimeIncrement"=>30.0,
            "PhysicalSizeX"=>0.5,"PhysicalSizeUnit"=>"micron"))) == ["metadata.frame_interval_no_unit"]
        # no spatial unit, x+y+z present (2D-safe: SizeZ=1 so no z-spacing case) → three no-unit (x,y,z)
        fu = Cecelia.metadata_qc_findings(Dict("PhysicalSizeX"=>0.5,"PhysicalSizeY"=>0.5,"PhysicalSizeZ"=>2.0))
        @test all(==("metadata.pixel_size_no_unit"), codes(fu)) && fields(fu) == ["x","y","z"]
        # z-spacing case suppresses the z no-unit dup (z already flagged)
        fz = Cecelia.metadata_qc_findings(Dict("SizeZ"=>10,"PhysicalSizeX"=>0.5,"PhysicalSizeY"=>0.5))
        @test codes(fz) == ["metadata.z_spacing_unknown","metadata.pixel_size_no_unit","metadata.pixel_size_no_unit"]
        @test fields(fz) == ["z","x","y"]     # no second z entry

        # severity symbols: shape-distinct (✅/⚠️/❌), NOT same-shape circles; unknown → ""
        @test Cecelia.severity_symbol("ok")   == "✅"
        @test Cecelia.severity_symbol("warn") == "⚠️"
        @test Cecelia.severity_symbol("fail") == "❌"
        @test length(Set(values(Cecelia.SEVERITY_SYMBOLS))) == 3   # distinct glyphs
        @test !("🟢" in values(Cecelia.SEVERITY_SYMBOLS))          # not the colour-blind trap
        @test Cecelia.severity_symbol("bogus") == ""

        # clustering findings (set-scope: total = run clusters; the rest = one image's slice)
        # run collapsed to 1 cluster → warn (regardless of the per-image numbers)
        c1 = Cecelia.cluster_qc_findings(1, 500, 1, 1.0)
        @test length(c1) == 1 && c1[1]["code"] == "clustering.single_cluster" && c1[1]["level"] == "warn"
        # image's cells all in one cluster while the run found several → warn (batch outlier)
        c2 = Cecelia.cluster_qc_findings(6, 400, 1, 1.0)
        @test length(c2) == 1 && c2[1]["code"] == "clustering.image_one_cluster" && c2[1]["level"] == "warn"
        # one cluster dominates this image (≥90%) but not all → info
        c3 = Cecelia.cluster_qc_findings(6, 400, 3, 0.95; unit = "tracks")
        @test length(c3) == 1 && c3[1]["code"] == "clustering.dominant_cluster" && c3[1]["level"] == "info"
        @test occursin("tracks", c3[1]["short"])
        # a healthy spread flags nothing; an empty image (n=0) never flags
        @test isempty(Cecelia.cluster_qc_findings(6, 400, 5, 0.4))
        @test isempty(Cecelia.cluster_qc_findings(6, 0, 0, 0.0))

        # category distribution metrics (HMM states/transitions): skip NaN/missing/nothing
        m1 = Cecelia.category_dist_metrics([1.0, 1.0, 2.0, NaN, missing])
        @test m1.n == 3 && m1.n_distinct == 2 && m1.dominant_frac ≈ 2/3
        ms = Cecelia.category_dist_metrics(["1_2", "1_2", nothing, "2_1"])
        @test ms.n == 3 && ms.n_distinct == 2
        @test Cecelia.category_dist_metrics(Any[NaN, missing, nothing]).n == 0

        # HMM state findings: no decode → warn; single state → warn; ≥95% one state → info; else none
        @test Cecelia.hmm_states_qc_findings(Cecelia.category_dist_metrics(Float64[]))[1]["code"] == "hmm.no_states_decoded"
        @test Cecelia.hmm_states_qc_findings(Cecelia.category_dist_metrics([1.0, 1.0, 1.0]))[1]["code"] == "hmm.single_state"
        fd = Cecelia.hmm_states_qc_findings(Cecelia.category_dist_metrics(vcat(fill(1.0, 96), fill(2.0, 4))))
        @test fd[1]["code"] == "hmm.dominant_state" && fd[1]["level"] == "info"
        @test isempty(Cecelia.hmm_states_qc_findings(Cecelia.category_dist_metrics([1.0, 1.0, 2.0, 2.0])))
        # HMM transitions: only the no-transitions case flags
        @test Cecelia.hmm_transitions_qc_findings(Cecelia.category_dist_metrics(Any[nothing]))[1]["code"] == "hmm.no_transitions"
        @test isempty(Cecelia.hmm_transitions_qc_findings(Cecelia.category_dist_metrics(["1_2", "2_1"])))

        # track measures: auto + low-confidence motion dims → warn; confident/user-set → none
        tm = Cecelia.track_measures_qc_findings(120, "auto", 2, 2, "low", "z ambiguous")
        @test length(tm) == 1 && tm[1]["code"] == "tracking.motion_dims_uncertain" && tm[1]["level"] == "warn"
        @test isempty(Cecelia.track_measures_qc_findings(120, "auto", 3, 3, "high", "clear"))
        @test isempty(Cecelia.track_measures_qc_findings(120, "3D", 3, 2, "low", "user forced"))  # user-set: no flag

        # against the tracked fixture: metrics agree with an independent count of track_id
        h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        if !have_fixture(h5)
            @test_skip "track_count_metrics fixture (missing)"
        else
            tids = (label_props(h5) |> select_cols(["track_id"]) |> as_df).track_id
            valid = Int.(filter(t -> !isnan(t) && t > 0, tids))
            fnt, fml, fntc = track_count_metrics(tids)
            @test fnt == length(unique(valid))
            @test fntc == length(valid)
            @test fml ≈ length(valid) / length(unique(valid))
        end
    end

    @testset "cohort outliers (robust median/MAD)" begin
        # MAD>0 regime: a clear outlier flags even at n=3 (mean/SD couldn't — max |z| there = 1.15)
        r3 = Cecelia._cohort_outliers(Dict("a"=>800.0,"b"=>810.0,"c"=>100.0))
        @test r3.n == 3 && haskey(r3.outliers, "c") && !haskey(r3.outliers, "a")
        @test r3.median == 800.0 && r3.mad > 0
        @test r3.outliers["c"]["z"] |> abs > 3.5             # modified-z carried for the flag

        # MAD==0 regime — THE qcProbe case [800,800,100]: two identical baselines → MAD 0 (and the
        # old mean-abs-dev fallback missed it). Relative-departure rule flags the 100, not the 800s.
        r0 = Cecelia._cohort_outliers(Dict("a"=>800.0,"b"=>800.0,"c"=>100.0))
        @test r0.mad == 0.0 && haskey(r0.outliers, "c") && !haskey(r0.outliers, "a")
        @test r0.outliers["c"]["relDev"] >= 0.5             # relative departure carried for the flag
        # …but a near-identical value (801 vs 800) is NOT flagged (no magnitude → no false positive)
        @test isempty(Cecelia._cohort_outliers(Dict("a"=>800.0,"b"=>800.0,"c"=>801.0)).outliers)

        # too few to judge (no cohort) → no outliers even with a wild value
        @test isempty(Cecelia._cohort_outliers(Dict("a"=>5.0,"b"=>500.0)).outliers)
        # all identical → nothing flagged (no false positive)
        @test isempty(Cecelia._cohort_outliers(Dict("a"=>3.0,"b"=>3.0,"c"=>3.0)).outliers)
        # a lower explicit threshold is honoured in the MAD>0 regime (more sensitive)
        @test haskey(Cecelia._cohort_outliers(Dict("a"=>800.0,"b"=>810.0,"c"=>100.0), 1.0).outliers, "c")

        # per-image finding from an outlier entry — direction from value vs median; carries detail
        cf = Cecelia._cohort_finding("nCells", Dict{String,Any}("value"=>100.0,"z"=>-5.2), 800.0)
        @test cf["code"] == "cohort.nCells" && cf["level"] == "warn"
        @test cf["detail"]["metric"] == "nCells" && cf["detail"]["value"] == 100.0 && cf["detail"]["z"] == -5.2
        @test occursin("below", cf["long"])
        cf2 = Cecelia._cohort_finding("nTracks", Dict{String,Any}("value"=>900.0,"relDev"=>0.8), 500.0)
        @test occursin("above", cf2["long"]) && cf2["detail"]["relDev"] == 0.8

        # lab-log summary lines + has-outliers predicate (drives whether the check logs at all)
        clean = Dict{String,Any}("funName"=>"segment.cellpose", "nIncluded"=>10,
            "metrics"=>Dict("nCells"=>Dict{String,Any}("median"=>800.0,"outliers"=>Dict{String,Any}())))
        @test !cohort_has_outliers(clean)
        cl = cohort_qc_summary_lines(clean)
        @test length(cl) == 1 && occursin("all 10", cl[1]) && startswith(cl[1], "✅")
        flagged = Dict{String,Any}("funName"=>"segment.cellpose", "nIncluded"=>10,
            "metrics"=>Dict("nCells"=>Dict{String,Any}("median"=>800.0,
                "outliers"=>Dict{String,Any}("j"=>Dict{String,Any}("value"=>100.0,"z"=>-5.2)))))
        @test cohort_has_outliers(flagged)
        fl = cohort_qc_summary_lines(flagged)
        @test startswith(fl[1], "⚠️") && any(l -> occursin("j", l) && occursin("100", l), fl)
    end

    @testset "cohort round-trip (banked metrics → set sidecar)" begin
        set = CciaSet(; dir = mktempdir())
        counts = Dict("a"=>800,"b"=>810,"c"=>790,"d"=>805,"e"=>795,
                      "f"=>808,"g"=>803,"h"=>797,"i"=>802,"j"=>100)
        for (uid, n) in counts
            img = CciaImage(; uid = uid, dir = mktempdir())
            write_qc(img, "segment.measureLabels", "default", Dict{String,Any}[];
                     metrics = Dict{String,Any}("nCells" => n))
            push!(set._images, img); push!(set.image_uids, uid)
        end
        # READ-ONLY path (GET): computes outliers but writes NOTHING (no sidecar, no per-image)
        ro = cohort_qc_for(set, "segment.measureLabels", "default")
        @test haskey(ro["metrics"]["nCells"]["outliers"], "j")
        @test !isfile(cohort_qc_path(set, "segment.measureLabels", "default"))
        @test read_qc(set._images[findfirst(i -> i.uid == "j", set._images)],
                      "cohort.segment.measureLabels", "default") === nothing
        # PERSIST path (the check action): sidecar + per-image findings
        doc = cohort_qc_for!(set, "segment.measureLabels", "default")
        m = doc["metrics"]["nCells"]
        @test m["n"] == 10 && haskey(m["outliers"], "j") && !haskey(m["outliers"], "a")
        @test doc["nIncluded"] == 10
        # sidecar written + re-readable
        @test isfile(cohort_qc_path(set, "segment.measureLabels", "default"))
        @test read_cohort_qc(set, "segment.measureLabels", "default")["nIncluded"] == 10
        @test haskey(read_all_cohort_qc(set), "segment.measureLabels/default")
        # per-image write-back: the outlier (j) gets a cohort finding ON the image. A normal image
        # (a) is NOT written — no empty placeholder (that would put an empty cohort.* doc on every
        # image on every check). Under the cohort.* namespace, merged by read_all_qc.
        byid = Dict(i.uid => i for i in set._images)
        fj = read_qc(byid["j"], "cohort.segment.measureLabels", "default")
        @test fj !== nothing && !isempty(fj["findings"])
        @test fj["findings"][1]["code"] == "cohort.nCells" && fj["findings"][1]["level"] == "warn"
        @test occursin("below", fj["findings"][1]["long"])         # 100 < median 800
        @test read_qc(byid["a"], "cohort.segment.measureLabels", "default") === nothing
        @test haskey(read_all_qc(byid["j"]), "cohort.segment.measureLabels/default")
        # clear-stale: bump the outlier back into range and re-check → j's prior cohort doc is
        # CLEARED (written empty, un-flags), not left as a stale warning
        byid["j"].included = true
        write_qc(byid["j"], "segment.measureLabels", "default", Dict{String,Any}[];
                 metrics = Dict{String,Any}("nCells" => 801))
        cohort_qc_for!(set, "segment.measureLabels", "default")
        fj2 = read_qc(byid["j"], "cohort.segment.measureLabels", "default")
        @test fj2 !== nothing && isempty(fj2["findings"])          # existing doc cleared, not deleted
        # excluded images drop out of the cohort
        set._images[1].included = false                      # exclude one
        @test cohort_qc_for!(set, "segment.measureLabels", "default")["nIncluded"] == 9
        # unknown fun errors (not a metric producer)
        @test_throws ErrorException cohort_qc_for!(set, "not.aTask", "default")
    end

    @testset "cohort value_name discovery (per label set)" begin
        set = CciaSet(; dir = mktempdir())
        # clustering banks per label set: T-tracks tight, B-tracks with one sparse image (c=9)
        for (uid, nT, nB) in [("a", 40, 22), ("b", 39, 24), ("c", 41, 9)]
            img = CciaImage(; uid = uid, dir = mktempdir())
            write_qc(img, "clustTracks.cluster", "T", Dict{String,Any}[];
                     metrics = Dict{String,Any}("nTracks"=>nT, "nClusters"=>4, "largestClusterFrac"=>0.4))
            write_qc(img, "clustTracks.cluster", "B", Dict{String,Any}[];
                     metrics = Dict{String,Any}("nTracks"=>nB, "nClusters"=>3, "largestClusterFrac"=>0.5))
            push!(set._images, img); push!(set.image_uids, uid)
        end
        # discovers the banked label sets (sorted), empty for a fun that banked nothing
        @test cohort_value_names(set, "clustTracks.cluster") == ["B", "T"]
        @test cohort_value_names(set, "segment.cellpose") == String[]
        # per-value_name cohorts: T and B are SEPARATE cohorts
        allc = cohort_qc_for_all(set, "clustTracks.cluster")
        @test Set(keys(allc)) == Set(["B", "T"])
        @test allc["T"]["valueName"] == "T" && allc["B"]["valueName"] == "B"
        # the sparse B image (c) flags in the B cohort, not the T cohort
        @test haskey(allc["B"]["metrics"]["nTracks"]["outliers"], "c")
        @test !haskey(allc["T"]["metrics"]["nTracks"]["outliers"], "c")
        @test !isfile(cohort_qc_path(set, "clustTracks.cluster", "B"))   # read-only wrote nothing
        # persist variant writes each label set's sidecar
        allw = cohort_qc_for_all!(set, "clustTracks.cluster")
        @test isfile(cohort_qc_path(set, "clustTracks.cluster", "B"))
        @test isfile(cohort_qc_path(set, "clustTracks.cluster", "T"))
        @test occursin("(B)", join(cohort_qc_summary_lines(allw["B"])))   # label set named in the summary
    end

    @testset "cluster QC banked per run (suffix, no collision)" begin
        img = CciaImage(; uid = "a", dir = mktempdir())
        qcdir = mktempdir()
        # a cluster_qc.json fixture (what the Python runner writes): two segments T & B
        mkqc(path) = open(path, "w") do io
            JSON3.write(io, Dict("nClusters" => 4, "perSegment" => [
                Dict("uID" => "a", "valueName" => "T", "n" => 40, "nClusters" => 4, "largestClusterFrac" => 0.4),
                Dict("uID" => "a", "valueName" => "B", "n" => 20, "nClusters" => 3, "largestClusterFrac" => 0.5)]))
        end
        p1 = joinpath(qcdir, "run1.json"); mkqc(p1)
        Cecelia.write_cluster_qc!([img], "clustTracks.cluster", p1; unit = "tracks", suffix = "movement")
        p2 = joinpath(qcdir, "run2.json"); mkqc(p2)
        Cecelia.write_cluster_qc!([img], "clustTracks.cluster", p2; unit = "tracks", suffix = "test")
        # BOTH runs retained under composite {labelSet}.{suffix} keys — "test" did NOT overwrite "movement"
        dmov = read_qc(img, "clustTracks.cluster", "T.movement")
        dtst = read_qc(img, "clustTracks.cluster", "T.test")
        @test dmov !== nothing && dtst !== nothing
        @test dmov["metrics"]["nTracks"] == 40 && dmov["runSuffix"] == "movement" && dmov["labelSet"] == "T"
        @test dtst["runSuffix"] == "test"
        # empty suffix ⇒ bank under the bare label set (no trailing dot)
        p3 = joinpath(qcdir, "run3.json"); mkqc(p3)
        Cecelia.write_cluster_qc!([img], "clustPops.cluster", p3; unit = "cells", suffix = "")
        @test read_qc(img, "clustPops.cluster", "T") !== nothing

        set = CciaSet(; dir = mktempdir()); push!(set._images, img); push!(set.image_uids, "a")
        # cohort discovers all (label set × run) value_names
        @test cohort_value_names(set, "clustTracks.cluster") == ["B.movement", "B.test", "T.movement", "T.test"]
        # cohort_runs groups by run (segment/tracking funs bank no run → [])
        crs = cohort_runs(set, "clustTracks.cluster")
        @test Set(r.run for r in crs) == Set(["movement", "test"])
        @test sort(first(r.valueNames for r in crs if r.run == "test")) == ["B.test", "T.test"]  # each run carries its value_names
        @test isempty(cohort_runs(set, "segment.cellpose"))
        # run filter: cohort_qc_for_all!(run="test") persists ONLY the test run's value_names
        allw = cohort_qc_for_all!(set, "clustTracks.cluster"; run = "test")
        @test Set(keys(allw)) == Set(["B.test", "T.test"])
        @test occursin("(T.test)", join(cohort_qc_summary_lines(allw["T.test"])))  # run named in the lab-log line
    end

    @testset "register_cohort_metrics! (custom-module opt-in)" begin
        fun = "customExamples.qcProbeTest"
        @test !haskey(COHORT_METRICS, fun)                       # unknown → cohort errors
        register_cohort_metrics!(fun, ["nCells"])
        @test COHORT_METRICS[fun] == ["nCells"]                  # now a known producer
        register_cohort_metrics!(fun, ["nCells", "nClusters"])   # idempotent overwrite
        @test COHORT_METRICS[fun] == ["nCells", "nClusters"]
        delete!(COHORT_METRICS, fun)                             # don't leak into other testsets
    end

    @testset "board spec expander (MCP board authoring, Phase 2)" begin
        # The expander turns a SEMANTIC spec into a LayoutEntry and refuses anything the project cannot
        # plot. The failure it closes: a bad `tkey` renders an EMPTY panel with NO error, so a board can
        # look authored and show nothing. See docs/todo/MCP_BOARD_AUTHORING_PLAN.md, Phase 2.
        specs = plot_spec_index()
        @test haskey(specs, "track_measures")            # anti-vacuity: the registry actually loaded
        @test length(plot_specs()) >= 5

        # templates: "<cols>x<rows>" only — the named comic plates are a frontend catalogue and stay
        # GUI-only rather than being duplicated server-side
        @test board_template_grid("2x2", 4) == (2, 2)
        @test board_template_grid("3x2", 6) == (3, 2)
        @test board_template_grid("", 4) == (2, 2)       # empty → smallest near-square grid that fits
        @test board_template_grid("", 3) == (2, 2)
        @test board_template_grid("", 1) == (1, 1)
        @test_throws BoardSpecError board_template_grid("comic-banner", 4)
        @test_throws BoardSpecError board_template_grid("9x9", 4)
        # row-major grid areas, the uniform case of plots/layoutTemplates.ts
        @test board_slot_areas(2, 2) == ["1 / 1 / 2 / 2", "1 / 2 / 2 / 3", "2 / 1 / 3 / 2", "2 / 2 / 3 / 3"]

        proj = CciaProject(; uid = "bsp", name = "spec"); proj.root = mktempdir()
        ok(plots; kw...) = expand_board(proj, "B vs T", plots; kw...)

        # A project with no populations still expands a board that references none — pops are optional
        lay = ok([Dict("plot" => "track_measures", "measure" => "live.track.speed", "chart" => "boxplot")])
        @test lay["cols"] == 1 && lay["rows"] == 1
        @test length(lay["contents"]) == 1 && lay["contents"][1] !== nothing
        c = lay["contents"][1]
        @test c["kind"] == "summary" && c["ref"] == "track_measures"
        @test c["state"]["chartType"] == "boxplot" && c["state"]["measure"] == "live.track.speed"
        @test !haskey(c["state"], "vis")        # the panel owns its look — defaultVis() is NOT copied here
        @test !haskey(c["state"], "statUnit")   # nothing said → nothing guessed; the panel fills it

        # unfilled slots stay empty, and the board keeps the requested shape
        lay4 = ok([Dict("plot" => "track_measures"), Dict("plot" => "track_measures")]; template = "2x2")
        @test length(lay4["contents"]) == 4
        @test lay4["contents"][3] === nothing && lay4["contents"][4] === nothing
        @test lay4["activeIndex"] == 0
        # no pops requested → nothing to point the board at, so no scope is forced
        @test lay4["shared"] == Dict{String,Any}()

        # …but when we DO write per-slot `sel`, the board must be told to read it. `shared.scope`
        # defaults to "global" in useSummaryData, and panelSel then takes the board-level `shared.sel`
        # and ignores each slot's own — so an authored board rendered with NO series until the user
        # picked populations by hand. Regression test for exactly that.
        withpops = expand_board(proj, "sel", [Dict("plot" => "track_measures", "pops" => ["B/qc"])];
                                pops = Dict("B/qc" => "flow"))
        @test withpops["shared"]["scope"] == "local"
        # The SPEC's own first popType wins over the family the pop happens to be stored under: a flow
        # gate is legitimately usable as a `live` pop, and track_measures fetches live — which is why
        # the project's real boards store `live::B/qc/_tracked`. The picker's family is only the
        # fallback for a spec that offers no popTypes at all.
        @test withpops["contents"][1]["state"]["sel"] == ["live::B/qc"]
        # and nothing else is invented in the shared bag — the rest are frontend defaults
        @test collect(keys(withpops["shared"])) == ["scope"]

        # statUnit and imageAgg travel together (utils/statUnitState.ts)
        su = ok([Dict("plot" => "track_measures", "statUnit" => "image")])["contents"][1]["state"]
        @test su["statUnit"] == "image" && su["imageAgg"] == "mean"
        @test ok([Dict("plot" => "track_measures", "statUnit" => "image",
                       "imageAgg" => "median")])["contents"][1]["state"]["imageAgg"] == "median"

        # ── the rejections. Every message must name the offending value AND what was available, because
        # the caller is an agent that can correct itself if told the options.
        @test_throws BoardSpecError ok([])                                    # a board needs a plot
        @test_throws BoardSpecError expand_board(proj, "  ", [Dict("plot" => "track_measures")])
        @test_throws BoardSpecError ok([Dict("measure" => "x")])               # no `plot`
        @test_throws BoardSpecError ok([Dict("plot" => "no_such_plot")])
        @test_throws BoardSpecError ok([Dict("plot" => "track_measures", "chart" => "sankey")])
        @test_throws BoardSpecError ok([Dict("plot" => "track_measures", "measure" => "live.track.nope")])
        @test_throws BoardSpecError ok([Dict("plot" => "track_measures", "statUnit" => "per-cell")])
        @test_throws BoardSpecError ok([Dict("plot" => "track_measures", "statUnit" => "image",
                                             "imageAgg" => "mode")])
        # a population that does not exist would render an empty panel in silence — this is the one
        @test_throws BoardSpecError ok([Dict("plot" => "track_measures", "pops" => ["Ghost/qc"])])

        # ── DERIVED populations must be accepted. `/_tracked` is injected by the picker at query time
        # and is NOT stored in the gating sidecar, so an earlier version of this validator — which
        # walked the persisted populations — rejected "B/qc/_tracked", the population the real project's
        # own boards plot. board_spec_populations now goes through `plot_population_groups`, the same
        # enumerator that fills the picker, so the validator accepts exactly what the GUI offers.
        avail = Dict("B/qc/_tracked" => "live", "T/qc/_tracked" => "live", "B/qc" => "flow")
        d = expand_board(proj, "derived", [Dict("plot" => "track_measures",
                                                "pops" => ["B/qc/_tracked", "T/qc/_tracked"])];
                         pops = avail)["contents"][1]
        @test d["state"]["sel"] == ["live::B/qc/_tracked", "live::T/qc/_tracked"]
        @test d["state"]["popType"] == "live"        # taken from the picker, not guessed
        # …and the tkeys the expander writes must decode back to the pops it was given — read and write
        # describe a board the same way (Decision 2)
        @test [let t = Cecelia._parse_tkey(k); "$(t.valueName)$(t.pop)" end for k in d["state"]["sel"]] ==
              ["B/qc/_tracked", "T/qc/_tracked"]
        # an explicit popType overrides the picker's
        @test expand_board(proj, "pt", [Dict("plot" => "track_measures", "popType" => "track",
                                             "pops" => ["B/qc/_tracked"])];
                           pops = avail)["contents"][1]["state"]["sel"] == ["track::B/qc/_tracked"]
        # more plots than slots
        @test_throws BoardSpecError ok([Dict("plot" => "track_measures") for _ in 1:5]; template = "2x2")

        e = try ok([Dict("plot" => "no_such_plot")]) catch err; err end
        @test e isa BoardSpecError && occursin("no_such_plot", e.msg) && occursin("track_measures", e.msg)

        # ── append_board: ADD-ONLY (Decision 1) ────────────────────────────────────────────────────
        doc = BoardsDoc(3, Any[Dict("id" => 1, "name" => "Track measures")], 1, 1,
                        Dict{String,Any}("tab:1" => Dict("cols" => 1)), true, true)
        doc2, id = append_board(doc, "B vs T", lay)
        @test id == 2 && length(doc2.tabs) == 2
        @test doc2.tabs[1] == doc.tabs[1]                  # the existing board is untouched
        @test haskey(doc2.layouts, "tab:1") && haskey(doc2.layouts, "tab:2")
        @test doc2.next_id == 2 && doc2.version == doc.version   # version is stamped by the writer
        # the user's ACTIVE tab is not stolen — this writes into a project they may have open
        @test doc2.active_id == doc.active_id
        @test_throws BoardSpecError append_board(doc2, "B vs T", lay)          # duplicate name
        @test_throws BoardSpecError append_board(doc2, "  Track measures ", lay)  # …ignoring whitespace
    end

    @testset "boards document — one reader, both shapes, versioned writes" begin
        # analysisBoards.json has had two shapes. The tab ARRAY used to sit at `tabs.tabs` (a TabGroup
        # nested under `tabs`) — the collision that made a second parser read `b.tabs` as the array and
        # report NO boards on every project that had them. Both are read; only the flat one is written.
        dir = mktempdir()
        p = boards_doc_path(dir)
        @test endswith(p, joinpath("settings", "analysisBoards.json"))

        d0 = read_boards_doc(p)                                   # no file at all
        @test !d0.present && d0.readable && isempty(d0.tabs) && d0.version == 0

        mkpath(dirname(p))
        legacy = Dict("tabs" => Dict("tabs" => [Dict("id" => 1, "name" => "A")], "activeId" => 1, "nextId" => 2),
                      "layouts" => Dict("tab:1" => Dict("cols" => 2, "rows" => 1)))
        write(p, JSON3.write(legacy))
        d = read_boards_doc(p)
        @test d.present && d.readable
        @test length(d.tabs) == 1 && string(d.tabs[1]["name"]) == "A"
        @test d.active_id == 1 && d.next_id == 2
        @test d.version == 0                                      # no version key → 0, not an error
        @test haskey(d.layouts, "tab:1")                          # String keys, not JSON3 Symbols

        # writing converts to the flat shape and stamps the version
        write_boards_doc(p, d; version = d.version + 1)
        raw = JSON3.read(read(p, String))
        @test raw[:version] == 1
        @test raw[:tabs] isa AbstractVector                        # flat: the array is at the top level
        @test raw[:activeId] == 1 && raw[:nextId] == 2
        d2 = read_boards_doc(p)
        @test d2.version == 1 && length(d2.tabs) == 1 && d2.active_id == 1

        # a file that exists but cannot be parsed is NOT "no boards" — that silence hid the last bug
        write(p, "{not json")
        d3 = @test_logs (:warn,) match_mode=:any read_boards_doc(p)
        @test d3.present && !d3.readable

        # normalise_boards is pure, so the autosave route runs an incoming payload through exactly the
        # same reader as a load from disk
        n = normalise_boards(Dict("version" => 7, "tabs" => [Dict("id" => 2, "name" => "B")],
                                  "activeId" => 2, "nextId" => 3, "layouts" => Dict()))
        @test n.version == 7 && n.active_id == 2 && length(n.tabs) == 1
        @test normalise_boards("not a document").readable == false
        # the payload the client gets always carries the version its next write must echo
        @test boards_doc_payload(n)["version"] == 7
    end

    @testset "board read-back summarises what a board plots" begin
        # The boards file is written by the FRONTEND, so every field here is optional by construction:
        # a board from an older schema must degrade to fewer fields, never throw.
        # See docs/todo/MCP_BOARD_AUTHORING_PLAN.md, Phase 0.
        proj = CciaProject(; uid = "bdP", name = "boards"); proj.root = mktempdir()
        @test board_summaries(proj) == Any[]                       # no file yet

        mkpath(joinpath(proj.root, "settings"))
        bf = joinpath(proj.root, "settings", "analysisBoards.json")
        # The slot shape below is COPIED from a real analysisBoards.json, not hand-authored to match
        # the parser — note `title` inside the `vis` bag, which is where the frontend actually puts it.
        # An invented fixture is what certified both bugs in this file's history.
        _slot(unit) = Dict("kind" => "summary", "ref" => "track_measures",
                           "state" => Dict("specId" => "track_measures", "measure" => "live.track.speed",
                                           "chartType" => "boxplot", "popType" => "live",
                                           "sel" => ["live::B/qc", "live::T/qc"],
                                           "groupBy" => "live.cell.hmm.state.movement",
                                           "statUnit" => unit, "imageAgg" => "mean",
                                           "vis" => Dict("title" => "Speed", "logScale" => false)))
        write(bf, JSON3.write(Dict(
            "tabs" => Dict("tabs" => [Dict("id" => 1, "name" => "Track measures"),
                                      Dict("id" => 4, "name" => "Per image measures"),
                                      Dict("id" => 2, "name" => "Empty board")],
                           "activeId" => 1, "nextId" => 5),
            "layouts" => Dict(
                "tab:1" => Dict("cols" => 2, "rows" => 1, "contents" => [
                    _slot("individual"),
                    nothing,                                        # an empty slot is omitted
                ]),
                "tab:4" => Dict("cols" => 2, "rows" => 1, "contents" => [_slot("image")]),
                # tab 2 has no layout entry at all — a real state (tab created, never filled)
            ))))

        b = board_summaries(proj)
        @test length(b) == 3
        @test b[1]["name"] == "Track measures" && b[1]["cols"] == 2 && b[1]["rows"] == 1
        @test length(b[1]["plots"]) == 1                            # the nothing slot is dropped
        pl = b[1]["plots"][1]
        @test pl["kind"] == "summary" && pl["ref"] == "track_measures"
        @test pl["measure"] == "live.track.speed" && pl["chart"] == "boxplot"
        @test pl["groupBy"] == "live.cell.hmm.state.movement"
        @test pl["pops"] == ["B/qc", "T/qc"]                        # tkeys decoded to valueName/pop
        # the caption comes from state.vis.title — reading state.title returned nothing on every real
        # board, and the old fixture put it there and asserted it worked
        @test pl["title"] == "Speed"

        # THE REGRESSION. These two boards differ ONLY in summary level, and the summary must say so:
        # "Track measures" plots every track, "Per image measures" collapses each image to its mean.
        # While `statUnit` was dropped they serialised identically, and the observer reported a
        # duplicate board that wasn't one — a confident false claim about the user's own work.
        pi = b[2]["plots"][1]
        @test pl["statUnit"] == "individual" && pi["statUnit"] == "image"
        @test pl != pi
        @test pl["imageAgg"] == "mean" && pi["imageAgg"] == "mean"  # the pair travels together
        # a tab with no layout is reported, blank, rather than skipped
        @test b[3]["name"] == "Empty board" && isempty(b[3]["plots"])

        # highlighted pops + the clustered feature list define what a cluster plot SAYS
        write(bf, JSON3.write(Dict(
            "tabs" => Dict("tabs" => [Dict("id" => 1, "name" => "Clustering")]),
            "layouts" => Dict("tab:1" => Dict("cols" => 1, "rows" => 1, "contents" => [
                Dict("kind" => "summary", "ref" => "state_signature",
                     "state" => Dict("specId" => "state_signature", "hl" => ["/Directed", "/Scanning"],
                                     "features" => ["live.track.speed", "live.track.straightness"]))])))))
        pl = board_summaries(proj)[1]["plots"][1]
        @test pl["highlight"] == ["/Directed", "/Scanning"]
        @test pl["features"] == ["live.track.speed", "live.track.straightness"]

        # The pair is copied straight through — no default resolved, no guess about which slots have a
        # summary level. The panel persists it explicitly and clears it when the plot has none
        # (frontend/src/utils/statUnitState.ts), so a slot with no `statUnit` genuinely has no summary
        # level. A board written before that (or an interactive view) simply reports neither.
        write(bf, JSON3.write(Dict(
            "tabs" => Dict("tabs" => [Dict("id" => 1, "name" => "no summary level"),
                                      Dict("id" => 2, "name" => "image-level")]),
            "layouts" => Dict(
                "tab:1" => Dict("contents" => [
                    Dict("kind" => "summary", "ref" => "track_measures",
                         "state" => Dict("measure" => "live.track.speed")),      # neither key present
                    Dict("kind" => "interactive", "ref" => "umap", "state" => Dict())]),
                "tab:2" => Dict("contents" => [
                    Dict("kind" => "summary", "ref" => "track_measures",
                         "state" => Dict("measure" => "live.track.speed",
                                         "statUnit" => "image", "imageAgg" => "median"))])))))
        bb = board_summaries(proj)
        @test !haskey(bb[1]["plots"][1], "statUnit") && !haskey(bb[1]["plots"][1], "imageAgg")
        @test !haskey(bb[1]["plots"][2], "statUnit")     # nothing invented for an interactive view
        @test bb[2]["plots"][1]["statUnit"] == "image" && bb[2]["plots"][1]["imageAgg"] == "median"
        @test bb[1]["plots"][1] != bb[2]["plots"][1]     # distinguishable, which is the point

        # degradation: a slot with no state, and an unparseable file
        write(bf, JSON3.write(Dict(
            "tabs" => Dict("tabs" => [Dict("id" => 1, "name" => "bare")]),
            "layouts" => Dict("tab:1" => Dict("contents" => [Dict("kind" => "interactive", "ref" => "umap")])))))
        b = board_summaries(proj)
        @test b[1]["plots"][1]["ref"] == "umap" && !haskey(b[1]["plots"][1], "measure")
        @test b[1]["cols"] == 0                                     # missing grid → 0, not an error
        write(bf, "{not json")
        @test (@test_logs (:warn,) match_mode=:any board_summaries(proj)) == Any[]

        # A top-level `tabs` ARRAY is the CURRENT shape (it was the legacy `tabs.tabs` nesting that this
        # reader used to choke on), so it must read as a real board rather than warn.
        write(bf, JSON3.write(Dict("version" => 4, "tabs" => [Dict("id" => 9, "name" => "flat")],
                                   "activeId" => 9, "nextId" => 10, "layouts" => Dict())))
        fb = board_summaries(proj)
        @test length(fb) == 1 && fb[1]["name"] == "flat" && isempty(fb[1]["plots"])

        # A present-but-unreadable file must WARN, not quietly read as "no boards" — that silence is
        # what hid the `_board_tabs` bug (it read `b.tabs` as the array, a shape the frontend never
        # writes, and reported none on every real project for as long as it existed).
        write(bf, JSON3.write([1, 2, 3]))          # valid JSON, not a document
        @test (@test_logs (:warn,) match_mode=:any board_summaries(proj)) == Any[]
    end

    @testset "analysis lineage (Slice A synthesizer)" begin
        proj = CciaProject(; uid = "linP", name = "lineage"); proj.root = mktempdir()
        s = CciaSet(; uid = "linS", dir = mktempdir())
        push!(proj._sets, s); push!(proj.set_uids, s.uid)

        # i1 — full pipeline: import → segment(A,B) → track(A) → cluster(A/movement); A gated (flow)
        i1 = CciaImage(; uid = "i1", dir = mktempdir())
        i1.label_props = Dict("A" => "A.h5ad", "B" => "B.h5ad")
        lp1 = img_label_props_dir(i1); mkpath(lp1)
        touch(img_track_props_path(i1, "A"))                       # A is tracked
        open(joinpath(lp1, "A__tracks.clustfeatures.json"), "w") do f
            JSON3.write(f, Dict("movement" => Dict("features" => ["live.track.speed"], "partOf" => ["i1"])))
        end
        g1 = PopulationMap(; pop_type = "flow", value_name = "A")
        add_pop!(g1, "CD3"; gate = RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
        save_pop_map!(g1, i1)
        append_run_log!(i1, "importImages.omezarr", "default", "done")
        append_run_log!(i1, "segment.cellpose", "A", "done")
        append_run_log!(i1, "segment.cellpose", "B", "done")
        append_run_log!(i1, "tracking.bayesian_tracking", "A", "done")
        append_run_log!(i1, "clustTracks.cluster", "movement", "done")
        push!(s._images, i1); push!(s.image_uids, i1.uid)

        # i2 — partial + excluded: import → segment(A, failed) only
        i2 = CciaImage(; uid = "i2", dir = mktempdir()); i2.included = false
        i2.label_props = Dict("A" => "A.h5ad")
        append_run_log!(i2, "importImages.omezarr", "default", "done")
        append_run_log!(i2, "segment.cellpose", "A", "failed")
        push!(s._images, i2); push!(s.image_uids, i2.uid)

        # a wired chain + board tabs (project-level)
        save_chain_template!(proj, ChainTemplate("pipeline",
            [ChainNode(; id = "n1", fn = "segment.cellpose"),
             ChainNode(; id = "n2", fn = "tracking.bayesian_tracking")], ChainEdge[]))
        mkpath(joinpath(proj.root, "settings"))
        open(joinpath(proj.root, "settings", "analysisBoards.json"), "w") do io
            # the REAL persisted shape: `tabs` is a TabGroup ({tabs, activeId, nextId}), not a bare
            # array. This fixture used to be the bare array — written to match a parser that read
            # `b.tabs` as the list — so the test passed while lineage reported no boards on every real
            # project. Keep this mirroring stores/analysisTabs.ts `serialize`.
            JSON3.write(io, Dict("tabs" => Dict(
                "tabs" => [Dict("id" => 1, "name" => "Behaviour"), Dict("id" => 2, "name" => "Counts")],
                "activeId" => 1, "nextId" => 3)))
        end

        lin = analysis_lineage(proj)
        @test lin.projectUid == "linP" && length(lin.images) == 2
        e1 = lin.images[findfirst(e -> e.uid == "i1", lin.images)]
        e2 = lin.images[findfirst(e -> e.uid == "i2", lin.images)]
        # i1 ordered steps + stage mapping, last step's value_name is the run suffix
        @test [st.stage for st in e1.steps] == ["import", "segment", "segment", "track", "cluster"]
        @test e1.steps[end].fun == "clustTracks.cluster" && e1.steps[end].valueName == "movement"
        @test e1.segmentations == ["A", "B"] && e1.tracked == ["A"]
        @test length(e1.clusterRuns) == 1 && e1.clusterRuns[1].suffix == "movement" &&
              e1.clusterRuns[1].valueNames == ["A"]
        @test length(e1.gatedPops) == 1 && e1.gatedPops[1].valueName == "A" && "/CD3" in e1.gatedPops[1].pops
        # i2 partial + excluded + a failed step is surfaced
        @test e2.included == false && isempty(e2.tracked) && isempty(e2.clusterRuns)
        @test any(st -> st.fun == "segment.cellpose" && st.status == "failed", e2.steps)
        # project-level chains + boards
        @test length(lin.chains) == 1 && lin.chains[1].name == "pipeline"
        @test Set(lin.chains[1].tasks) == Set(["segment.cellpose", "tracking.bayesian_tracking"])
        @test lin.boards == ["Behaviour", "Counts"]
        # rollup: pipeline unions run-log steps AND artifact evidence, so i1's gated pop adds a
        # "gate" stage even though gating isn't a task step. i2 diverges (excluded + missing the
        # track/gate/cluster stages the others reached).
        @test lin.rollup.pipeline == ["import", "segment", "track", "gate", "cluster"]
        dv = lin.rollup.divergences[findfirst(d -> d.uid == "i2", lin.rollup.divergences)]
        @test dv.included == false && Set(dv.missingStages) == Set(["track", "gate", "cluster"])
        # artifact-aware stages: a segmentation/track with NO run-log step still counts as reached
        # (it predates the capped run-log window) — the fix for false "missing segment" divergences
        noStep = (; uid = "x", name = "X", included = true, steps = NamedTuple[],
                    segmentations = ["A"], tracked = ["A"], clusterRuns = Any[], gatedPops = Any[])
        @test Set(Cecelia._image_stages(noStep)) == Set(["segment", "track"])
        # scoping: one image, one set, unknown → empty
        @test length(analysis_lineage(proj; image_uid = "i1").images) == 1
        @test length(analysis_lineage(proj; set_uid = "linS").images) == 2
        @test isempty(analysis_lineage(proj; image_uid = "nope").images)
    end

    @testset "populations summary (Slice B)" begin
        proj = CciaProject(; uid = "popP", name = "pops"); proj.root = mktempdir()
        s = CciaSet(; uid = "popS", dir = mktempdir())
        push!(proj._sets, s); push!(proj.set_uids, s.uid)
        img = CciaImage(; uid = "i1", dir = mktempdir())
        img.label_props = Dict("A" => "A.h5ad")
        # a flow gate on A (CD3) + a cluster pop filtering clusters.movement
        mf = PopulationMap(; pop_type = "flow", value_name = "A")
        add_pop!(mf, "CD3"; gate = RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
        save_pop_map!(mf, img)
        mc = PopulationMap(; pop_type = "trackclust", value_name = "A")
        add_pop!(mc, "Directed"; filter_measure = "clusters.movement", filter_fun = "in", filter_values = [3])
        save_pop_map!(mc, img)
        push!(s._images, img); push!(s.image_uids, img.uid)

        out = populations_summary(proj)
        @test out.projectUid == "popP" && length(out.images) == 1
        pops = out.images[1].populations
        @test out.images[1].truncated == false
        cd3 = pops[findfirst(p -> p.name == "CD3", pops)]
        @test cd3.popType == "flow" && cd3.valueName == "A" && cd3.filter === nothing
        @test cd3.gate !== nothing && cd3.gate["kind"] == "rectangle" &&
              cd3.gate["x_channel"] == "c1" && cd3.gate["y_channel"] == "c2"
        dir = pops[findfirst(p -> p.name == "Directed", pops)]
        @test dir.popType == "trackclust" && dir.gate === nothing
        @test dir.filter.measure == "clusters.movement" && dir.filter.fun == "in" &&
              collect(dir.filter.values) == [3]
        # scoping mirrors lineage
        @test length(populations_summary(proj; image_uid = "i1").images) == 1
        @test isempty(populations_summary(proj; image_uid = "nope").images)

        # AUTO-SHARED cluster pops are reported for the borrowing segmentation too. Cluster pops are
        # global to a run: the names are authored under ONE co-clustered value_name (A) and every
        # sibling in the run (B) carries the same `clusters.movement` column, so `load_pop_map` lends
        # them relabeled. The observer used to skip any value_name with no sidecar FILE, so B looked
        # unclustered — and a board authored from that covered half the run's data.
        img.label_props["B"] = "B.h5ad"
        mkpath(Cecelia.img_label_props_dir(img))
        for vn in ("A", "B")      # both segments took part in the `movement` track-clustering run
            write(Cecelia._clustfeatures_path(img_track_props_path(img, vn)),
                  JSON3.write(Dict("clusters.movement" =>
                      Dict("features" => ["live.track.speed"], "partOf" => ["i1"]))))
        end
        shared = populations_summary(proj).images[1].populations
        bdir = shared[findall(p -> p.name == "Directed" && p.valueName == "B", shared)]
        @test length(bdir) == 1 && bdir[1].popType == "trackclust"
        @test bdir[1].filter.measure == "clusters.movement"        # the SAME run, not a new definition
        # a segment that is NOT in the run borrows nothing (no clustfeatures sidecar → no pops)
        img.label_props["C"] = "C.h5ad"
        cpops = populations_summary(proj).images[1].populations
        @test isempty(findall(p -> p.valueName == "C", cpops))
    end

    @testset "measure summary (Slice C)" begin
        # pure summary logic (always runs): median/quantiles/mean over finite values, NaN/missing dropped
        s = Cecelia._summarise_measure("x", Any[1.0, 2.0, 3.0, NaN, missing])
        @test s.n == 3 && s.median == 2.0 && s.q25 <= 2.0 <= s.q75    # mean dropped (payload trim)
        @test !hasproperty(s, :mean)
        @test Cecelia._summarise_measure("y", Any[NaN, missing]) === nothing

        # integration over the real KDIeEm B fixture: UNGATED image → the base fallback (all-cells
        # phenotype + tracked motility). The gated path (T/_qc) is validated separately off-suite.
        h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
        if !have_fixture(h5) || !have_fixture(trk)
            @test_skip "measure summary (fixture missing)"
        else
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
            cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
            img = CciaImage(uid = "KDIeEm", dir = td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
            proj = CciaProject(; uid = "mP", name = "m"); proj.root = mktempdir()
            st = CciaSet(; uid = "mS", dir = mktempdir()); push!(proj._sets, st); push!(proj.set_uids, st.uid)
            push!(st._images, img); push!(st.image_uids, img.uid)

            out = measure_summary(proj)
            @test length(out.images) == 1
            summ = out.images[1].summaries
            @test !isempty(summ)
            # motility over the tracked base (no channel-name dependence) — the robust anchor
            mi = findfirst(x -> x.kind == "motility", summ)
            @test mi !== nothing
            moti = summ[mi]
            @test moti.n > 0 && any(m -> m.name == "live.track.speed", moti.measures)
            @test all(m -> isfinite(m.median) && m.n > 0, moti.measures)
            # phenotype over all cells: more rows than tracks (cells collapse to tracks)
            pi = findfirst(x -> x.kind == "phenotype", summ)
            @test pi !== nothing && summ[pi].n > moti.n && !isempty(summ[pi].measures)
        end
    end

    @testset "behaviour + cluster summary (Slice D)" begin
        # pure category-distribution logic (always runs): fractions, distinct count, cap, null-drop
        d = Cecelia._category_distribution(Any[1.0, 1.0, 1.0, 2.0, NaN, missing, nothing])
        @test d.n == 4 && d.nDistinct == 2
        @test d.top[1].value == "1.0" && d.top[1].n == 3 && d.top[1].fraction == 0.75
        @test d.top[2].value == "2.0" && d.top[2].n == 1
        big = Cecelia._category_distribution(collect(1:100); cap = 5)
        @test big.nDistinct == 100 && length(big.top) == 5   # capped, but distinct count is the true total
        @test Cecelia._category_distribution(Any[NaN, missing, nothing]).n == 0

        # integration over the real KDIeEm B fixture: the summaries run and return the right shape
        # (behaviour/cluster entries only if the fixture banked HMM/cluster obs — asserted when present)
        h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
        if !have_fixture(h5)
            @test_skip "behaviour/cluster summary (fixture missing)"
        else
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5, joinpath(td, "labelProps", "B.h5ad"))
            img = CciaImage(uid = "KDIeEm", dir = td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
            proj = CciaProject(; uid = "bP", name = "b"); proj.root = mktempdir()
            st = CciaSet(; uid = "bS", dir = mktempdir()); push!(proj._sets, st); push!(proj.set_uids, st.uid)
            push!(st._images, img); push!(st.image_uids, img.uid)

            b = behaviour_summary(proj); c = cluster_summary(proj)
            @test length(b.images) == 1 && b.images[1].behaviour isa AbstractVector
            @test length(c.images) == 1 && c.images[1].clusters isa AbstractVector
            @test haskey(c, :featuresByRun)   # feature lists hoisted out of per-image entries
            # every behaviour entry is a well-formed distribution
            for e in b.images[1].behaviour
                @test e.kind in ("state", "transitions") && e.n > 0 && !isempty(e.distribution)
                @test all(x -> 0.0 <= x.fraction <= 1.0, e.distribution)
            end
        end
    end

    @testset "chains summary (Slice E)" begin
        proj = CciaProject(; uid = "chP", name = "ch"); proj.root = mktempdir()
        save_chain_template!(proj, ChainTemplate("pipe",
            [ChainNode(; id = "n1", fn = "segment.cellpose"),
             ChainNode(; id = "n2", fn = "tracking.bayesian_tracking")],
            [ChainEdge("n1", "n2")]))
        c = chains_summary(proj)
        @test c.projectUid == "chP" && length(c.templates) == 1
        t = c.templates[1]
        @test t.name == "pipe" && length(t.nodes) == 2 && length(t.edges) == 1
        @test t.nodes[1].fun == "segment.cellpose" && t.nodes[1].scope == "image"   # per-task default
        @test t.edges[1].from == "n1" && t.edges[1].to == "n2"
        @test c.runs isa AbstractVector && isempty(c.runs)   # no runs recorded on disk
    end
end

@testset "ss listener PID parse (_kill_listeners_on_port, Linux)" begin
    # Real `ss -tlnpH` lines: a listener appears once for IPv4 and once for IPv6 → one PID.
    raw = "LISTEN 0 128 127.0.0.1:8080 0.0.0.0:* users:((\"julia\",pid=1044704,fd=24))\n" *
          "LISTEN 0 128 [::1]:8080 [::]:* users:((\"julia\",pid=1044704,fd=25))"
    @test Cecelia._listener_pids_from_ss(raw) == [1044704]
    @test isempty(Cecelia._listener_pids_from_ss(""))                # nothing listening
    two = "users:((\"a\",pid=10,fd=1))\nusers:((\"b\",pid=22,fd=3))"
    @test Cecelia._listener_pids_from_ss(two) == [10, 22]            # distinct PIDs kept, in order
end

# Project export → import round-trip (project_io.jl / jobs.jl). Uses its own CECELIA_DEV_DIR +
# temp projects dir so it never touches the real dev/prod config; restores afterwards. Verifies:
# each .zarr store is packed to ONE .zarr.tar (no unpacked stores in the bundle), the lockfile is
# skipped, re-export/re-import refuse to clobber, and import restores byte-identical stores + uid.
@testset "project export/import" begin
    prev_env = get(ENV, "CECELIA_DEV_DIR", nothing)
    mktempdir() do tmp
        ENV["CECELIA_DEV_DIR"] = tmp
        try
            init_cecelia!()
            projroot = joinpath(tmp, "projects"); mkpath(projroot)
            set_projects_dir!(projroot)

            proj = create_project!(name = "io-test")
            uid  = proj.uid
            # fake stores + metadata mirroring the 0/ (data) + 1/ (metadata) layout
            d = joinpath(proj.root, "0", "img1", "data.ome.zarr", "0"); mkpath(d)
            write(joinpath(d, "0.0"), "chunk-bytes")
            write(joinpath(dirname(d), ".zattrs"), "{}")
            lab = joinpath(proj.root, "1", "img1", "labels", "labels.zarr"); mkpath(lab)
            write(joinpath(lab, ".zgroup"), "{}")
            lp = joinpath(proj.root, "1", "img1", "labelProps"); mkpath(lp)
            write(joinpath(lp, "base.h5ad"), "hdf-bytes")
            write(joinpath(proj.root, ".cecelia.lock"), "")
            # staging debris from a cancelled run. Matches neither `_is_store_dir` (no `.zarr`
            # suffix) nor the skip list, so the mirror walker used to RECURSE and copy every
            # chunk in as a loose file — gigabytes of unusable bytes in the bundle.
            deb = joinpath(proj.root, "0", "img1", "data.ome.zarr" * Cecelia.STORE_STAGING_SUFFIX, "0")
            mkpath(deb); write(joinpath(deb, "0.0"), "half-written")
            sup = joinpath(proj.root, "1", "img1", "labels",
                           "labels.zarr" * Cecelia.STORE_SUPERSEDED_SUFFIX)
            mkpath(sup); write(joinpath(sup, ".zgroup"), "{}")
            # a notebook with the uid hardcoded — the one place copy/reidentify must rewrite
            mkpath(joinpath(proj.root, "notebooks"))
            write(joinpath(proj.root, "notebooks", "nb.jl"), "proj = load_project(\"$uid\")\n")

            out    = joinpath(tmp, "exports")
            bundle = export_project(uid; out_dir = out)
            @test !isempty(bundle) && isdir(bundle)
            @test isfile(joinpath(bundle, "ccbundle.json"))
            @test isfile(joinpath(bundle, "0", "img1", "data.ome.zarr.tar"))
            @test isfile(joinpath(bundle, "1", "img1", "labels", "labels.zarr.tar"))
            @test !any(endswith(n, ".zarr") for (_, ds, _) in walkdir(bundle) for n in ds)  # packed, not unpacked
            @test !ispath(joinpath(bundle, ".cecelia.lock"))                                # lock skipped
            # staging debris excluded entirely — neither packed nor mirrored as loose chunks
            @test !any(any(s -> endswith(n, s), Cecelia.STORE_TMP_SUFFIXES)
                       for (_, ds, _) in walkdir(bundle) for n in ds)
            @test !occursin("half-written",
                            join((read(joinpath(r, f), String) for (r, _, fs) in walkdir(bundle)
                                  for f in fs if filesize(joinpath(r, f)) < 4096), "\n"))
            @test isempty(export_project(uid; out_dir = out))                               # refuses existing bundle

            rm(proj.root; recursive = true)                                                 # drop, then restore from bundle
            @test import_project(bundle) == uid
            tgt = joinpath(projroot, uid)
            @test read(joinpath(tgt, "0", "img1", "data.ome.zarr", "0", "0.0"), String) == "chunk-bytes"
            @test read(joinpath(tgt, "1", "img1", "labelProps", "base.h5ad"), String) == "hdf-bytes"
            @test !any(endswith(n, ".tar") for (_, _, fs) in walkdir(tgt) for n in fs)       # no leftover .tar
            @test isempty(import_project(bundle))                                           # default: refuses existing

            # bundle_info reports the collision the UI prompts on
            bi = Cecelia.bundle_info(bundle)
            @test bi.uid == uid && bi.exists == true

            # copy: new uid, both kept, name suffixed, stores intact
            copy_uid = import_project(bundle; mode = "copy")
            @test copy_uid != uid && !isempty(copy_uid)
            @test isdir(joinpath(projroot, uid)) && isdir(joinpath(projroot, copy_uid))     # both present
            cj = JSON3.read(read(joinpath(projroot, copy_uid, "project.json"), String))
            @test String(cj.uid) == copy_uid && endswith(String(cj.name), "(imported)")
            @test read(joinpath(projroot, copy_uid, "0", "img1", "data.ome.zarr", "0", "0.0"), String) == "chunk-bytes"
            # copy re-identified the notebook's hardcoded load_project (best-effort)
            @test occursin("load_project(\"$copy_uid\")",
                           read(joinpath(projroot, copy_uid, "notebooks", "nb.jl"), String))

            # replace: overwrite the existing uid in place (mutate a file first to prove it's rewritten)
            write(joinpath(tgt, "0", "img1", "data.ome.zarr", "0", "0.0"), "STALE")
            @test import_project(bundle; mode = "replace") == uid
            @test read(joinpath(tgt, "0", "img1", "data.ome.zarr", "0", "0.0"), String) == "chunk-bytes"

            # reidentify_project!: rename in place → dir + project.json + notebook re-identified
            rid = reidentify_project!(uid, "riTEST9")
            @test rid == "riTEST9"
            @test !ispath(joinpath(projroot, uid)) && isdir(joinpath(projroot, "riTEST9"))
            @test String(JSON3.read(read(joinpath(projroot, "riTEST9", "project.json"), String)).uid) == "riTEST9"
            @test occursin("load_project(\"riTEST9\")",
                           read(joinpath(projroot, "riTEST9", "notebooks", "nb.jl"), String))
            @test load_project("riTEST9").uid == "riTEST9"                                   # loads under the new id
        finally
            prev_env === nothing ? delete!(ENV, "CECELIA_DEV_DIR") :
                                   (ENV["CECELIA_DEV_DIR"] = prev_env)
            init_cecelia!()
        end
    end
end

# The tar invocation must never hand a Windows drive letter to `-f`. GNU tar reads an archive
# path as `host:path` when a colon precedes any separator, so `-f D:\a\...\x.tar` becomes a
# connection attempt to host `D` and the pack silently produces nothing — which is how `.ccbundle`
# export (the backup mechanism) came to be broken on Windows while every unix path was fine: a unix
# absolute path can NEVER trigger it, because the leading `/` comes before the colon.
#
# That asymmetry is exactly why this needs a PROPERTY test rather than a behaviour test. Running
# tar here would pass on unix no matter what the code does — a test that cannot fail. So assert the
# shape of the command instead: `-f` takes a bare filename, the directory rides on the cwd. Both
# assertions fail against the old absolute-`-f` form on every platform.
@testset "tar commands keep drive letters out of -f" begin
    pack = Cecelia._tar_pack_cmd(joinpath("D:", "a", "out", "store.zarr.tar"),
                                 joinpath("D:", "proj", "0", "img", "store.zarr"))
    # the flag is the combined `-cf`, so the archive is the token after it
    fidx = findfirst(==("-cf"), pack.exec)
    @test fidx !== nothing
    farg = pack.exec[fidx + 1]
    @test farg == "store.zarr.tar"                      # bare filename…
    @test !occursin(':', farg)                          # …so no drive letter can reach tar
    @test !occursin('/', farg) && !occursin('\\', farg)
    @test pack.dir == joinpath("D:", "a", "out")        # the directory rides on the cwd instead
    # -C is NOT subject to the host:path parse, so it stays absolute
    cidx = findfirst(==("-C"), pack.exec)
    @test cidx !== nothing && pack.exec[cidx + 1] == joinpath("D:", "proj", "0", "img")
    @test pack.exec[end] == "store.zarr"                # the member, relative to -C

    unpack = Cecelia._tar_unpack_cmd(joinpath("D:", "a", "bundle", "0", "img", "store.zarr.tar"))
    uidx = findfirst(==("-xf"), unpack.exec)
    @test uidx !== nothing && unpack.exec[uidx + 1] == "store.zarr.tar"
    @test !occursin(':', unpack.exec[uidx + 1])
    @test unpack.dir == joinpath("D:", "a", "bundle", "0", "img")
end

# ── Every directory whose params a USER actually sees ─────────────────────────────────────────
#
# All three copy testsets below walk THIS list. They each used to hardcode `src/tasks`, which
# exempted the custom-module examples — 7 tips, every one breaking the no-trailing-period rule,
# in the very file people COPY to write a drop-in module. Those specs are loaded by
# `load_custom_modules!` and rendered by the same `ParamRenderer`, so they are task specs that
# happen to live in `docs/`. Missing directories are skipped, so a trimmed checkout is fine.
#
# `app/src/plotDefinitions/` is deliberately NOT here, and the distinction is worth keeping
# straight: those files have a `params` array of the same SHAPE, but it is a defaults bag, not a
# form. Its only consumer is `SummaryPanel.vue` —
#     `props.spec.params?.find(p => p.key === k)?.default ?? d`
# — which reads `default` and nothing else. A `label` or `tip` there renders to nobody, so
# requiring one would have produced nine strings that look maintained and reach no user. The
# controls a user really operates for those plots are hand-rolled in the SFC, and the frontend
# ratchet already covers them. (The top-level `spec.label` IS rendered, in the plot picker, and
# is unchecked — a small, separate gap; don't fix it by dragging the whole directory in here.)
spec_dirs() = filter(isdir, [
    joinpath(dirname(dirname(pathof(Cecelia))), "src", "tasks"),
    joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "docs", "examples", "custom-modules"),
])
# Walk every spec, yielding (label-for-messages, parsed spec) so a failure names a findable file.
# The label carries the containing directory — `tasks/x.json` vs `plotDefinitions/x.json` — because
# base names collide across surfaces (`track_measures.json` exists in both).
function each_spec(visit)
    for dir in spec_dirs(), (root, _, files) in walkdir(dir), fname in files
        endswith(fname, ".json") || continue
        spec = try JSON3.read(read(joinpath(root, fname), String)) catch; continue end
        spec isa AbstractDict || continue
        visit(joinpath(basename(root), fname), spec)
    end
end

# ── UI copy budget: task-spec `tip` fields ────────────────────────────────────────────────────
#
# The enforceable half of `docs/UI.md` → *UI copy — keep it short*, for the surface Julia owns.
# A `tip` renders as a tooltip on the task form, so it carries the same bar as any other tooltip:
# one line, under 90 characters, no second sentence explaining itself. This lives here rather than
# in the frontend suite because task specs are backend files and the frontend never holds a copy.
#
# 56 of 175 tips had drifted past the budget (worst: 332 chars, three sentences on a form field)
# before this existed. An exact allow-list, not a count — a count silently permits swapping one
# violation for another. Before adding an entry, check whether the fact belongs in a `docs/` file:
# that was true of every tip the sweep shortened.
# ── Numeric param RANGES have to be plausible ─────────────────────────────────────────────────
#
# `min`/`max` are enforced (`_validate_leaf`) and rendered as the slider's travel, so a bound that
# was never thought about does two things: it makes the useful part of the slider a few pixels
# wide, and it lets one drag start a run nobody wants. Several were plainly copy-pasted — a cell
# **surface distance** and a "min cells" count both ran to **1000** (a cell is ~10 µm, so 1000 µm
# is 100 cell diameters; an aggregate of 1000 cells is an organ), and `nPermutations` reached
# 100 000, hours of compute one drag away.
#
# The check is a RATIO, not a table of blessed numbers: a table would just restate the JSON and
# would need editing every time a default legitimately moves. `max / default` is the tell for a
# bound nobody chose — a sane range puts the default somewhere you can reach, so a max fifty times
# the default means the default is pinned to the far left of the travel. The worst honest ratio in
# the tree is 20 (`minCells` 5→100), so 50 leaves real headroom while catching every case above.
@testset "numeric param ranges are plausible" begin
    RATIO_MAX = 50
    # A param whose range genuinely spans orders of magnitude. Empty on purpose: before adding one,
    # check that the DEFAULT isn't the thing that's wrong.
    ALLOWED_WIDE = String[]

    nums = Tuple[]
    each_spec() do f, spec
        each_spec_param(spec_get(spec, "params")) do p, _
            String(something(spec_get(p, "type"), "")) in ("int", "float") || return
            push!(nums, (f, String(something(spec_get(p, "key"), "?")),
                         spec_get(p, "min"), spec_get(p, "max"),
                         spec_get(p, "step"), spec_get(p, "default")))
        end
    end
    @test length(nums) > 20                      # the walk found the numeric params

    # structural sanity first — these are bugs, not judgement calls
    for (f, k, mn, mx, _, def) in nums
        mn === nothing && continue
        if mx !== nothing
            @test mn <= mx || "$f/$k: min $mn > max $mx" == ""
        end
        if def !== nothing && def isa Real
            @test def >= mn                                   || "$f/$k: default $def < min $mn" == ""
            @test mx === nothing || def <= mx                 || "$f/$k: default $def > max $mx" == ""
        end
    end

    # a step coarser than the whole range means the slider has one position
    coarse = ["$f/$k: step $st over range $mn..$mx" for (f, k, mn, mx, st, _) in nums
              if st !== nothing && mn !== nothing && mx !== nothing && st > (mx - mn)]
    @test isempty(coarse)

    # …then the judgement call, as a loose bound
    wide = ["$f/$k: max $mx is $(round(mx / def, digits = 1))× the default $def"
            for (f, k, _, mx, _, def) in nums
            if mx !== nothing && def isa Real && def > 0 && mx / def > RATIO_MAX &&
               !("$f/$k" in ALLOWED_WIDE)]
    @test isempty(wide)
end

@testset "task spec tips stay short" begin
    COPY_MAX = 90
    ALLOWED = String[]

    # `tip`s nest inside `section`/`group` params, so recurse.


    # A trailing dot here is an abbreviation, not a sentence end ("e.g. HMM state").
    ABBREV = r"(?:^|[\s(])(?:e\.g|i\.e|etc|vs|cf|approx|fig|no)\.$"i
    function multi_sentence(s)
        for m in eachmatch(r"\S*\.\s+(?=[A-Z(])", s)
            occursin(ABBREV, rstrip(m.match)) || return true
        end
        false
    end

    tips = Tuple{String,String}[]
    nspecs = 0
    each_spec() do f, spec
        nspecs += 1
        each_spec_param(spec_get(spec, "params")) do p, _
            t = spec_get(p, "tip")
            t isa AbstractString && push!(tips, (f, join(split(String(t)), " ")))
        end
    end

    @test nspecs > 20                       # the walk found the specs
    @test length(tips) > 100                # ...and their tips

    too_long = ["$f: [$(length(t))] $t" for (f, t) in tips
                if length(t) > COPY_MAX && !(t in ALLOWED)]
    @test isempty(too_long)

    two_sentence = ["$f: $t" for (f, t) in tips if multi_sentence(t) && !(t in ALLOWED)]
    @test isempty(two_sentence)
end

# ── UI copy COVERAGE: every task param carries a `tip` ────────────────────────────────────────
#
# The testset above polices tips that EXIST. This one polices the ones that don't. `docs/UI.md`
# asks for CellProfiler-style tip DENSITY — every setting explains itself on hover — and until
# this existed nothing could see a gap: `branching.json` shipped **twelve** parameters with no
# tip at all, so the form read "Flatten Z" / "Pre-dilation" / "Anisotropy box size (px)" with no
# way to find out what any of them did short of reading the Python runner.
#
# Presence is the half a machine can decide; whether a tip is the RIGHT tip stays a review
# question, exactly as the length ratchet can't tell you a short line is a good line. The
# frontend half of this rule — settable controls with no `v-tooltip` — is `uncoveredControls`
# in `frontend/src/utils/uiCopy.ts`, checked in `uiCopy.test.ts`.
#
# SECTIONS AND GROUPS ARE EXEMPT. They are container headers ("Advanced", "Filters"), not inputs
# — a user can't set them to anything, and requiring one would buy 18 tips saying "advanced
# options". Their CHILDREN are checked like any other param.
@testset "every task param carries a tip" begin
    CONTAINER = ("section", "group")
    # A param whose label genuinely IS the whole explanation. Empty on purpose — same reason as
    # the length ratchet's: an allow-list that starts populated never gets emptied. Before adding
    # one, try writing the tip; it is nearly always shorter than the argument for skipping it.
    ALLOWED_NO_TIP = String[]

    # Collects every SETTABLE param (container children included), flagged tipped or not, so the
    # guard below can assert the walk actually found something — a silently empty walk would
    # otherwise report perfect coverage, which is how the QC scraper once lost 40 strings.


    params = Tuple{String,String,Bool}[]
    each_spec() do f, spec
        each_spec_param(spec_get(spec, "params")) do p, _
            ptype = String(something(spec_get(p, "type"), ""))
            (haskey(p, :key) || haskey(p, "key")) && !(ptype in CONTAINER) || return
            tip = spec_get(p, "tip")
            push!(params, (f, String(something(spec_get(p, "key"), "?")),
                           !isempty(strip(tip isa AbstractString ? String(tip) : ""))))
        end
    end

    @test length(params) > 150              # the walk found the params it is meant to police

    missing_tips = ["$f: $k" for (f, k, tipped) in params
                    if !tipped && !("$f: $k" in ALLOWED_NO_TIP)]
    @test isempty(missing_tips)
end

# ── UI copy house style: task-spec `label` + `tip` ────────────────────────────────────────────
#
# The Julia half of `docs/UI.md` → *House style*, mirroring the frontend checks in
# `frontend/src/utils/uiCopy.test.ts`. Split for the same reason the `tip` budget is: task specs
# are backend files and the frontend never holds a copy of one.
#
# This is the surface that actually drifted. Nothing could see the whole corpus at once, so the
# two halves of the app diverged along the storage boundary — 14 task labels went Title Case
# ("Bayesian Tracking", "Drift Correction") while every frontend label stayed sentence case, and
# all 164 tips grew a trailing period that no tooltip in the frontend had. `pixi run ui-copy`
# found it; this keeps it found. Exact allow-lists, not counts.
@testset "task spec copy follows the house style" begin
    ALLOWED_TITLE_CASE = String[]      # a label that is really a proper name
    ALLOWED_TRAILING_PERIOD = String[] # a `tip` that is genuinely a sentence

    # `@testset` bodies are their own scope, so the collector above isn't visible here — this one
    # pulls both keys in a single walk rather than re-deriving two nearly identical recursions.


    # Mirrors `isTitleCase` in uiCopy.ts — see there for why the allowances exist. A capital is
    # only evidence of Title Case when the word isn't expected to carry one: acronyms, single
    # letters, known proper nouns, and the first word after a separator ("Spatial / Time").
    PROPER = r"^(?:Cellpose|Bayesian|Dask|Cecelia|Leiden|Python|Julia|ImageJ|Fiji|OME|Napari|Zarr|Pluto|Rscript)$"
    SEPARATOR = r"^[/+&–—|]+$"
    expected_cap(w) = occursin(r"^[A-Z0-9+&/–-]+$", w) || length(w) == 1 || occursin(PROPER, w)
    function title_case(text)
        words = [w for w in split(text) if occursin(r"^[A-Za-z]", w) || occursin(SEPARATOR, w)]
        length(words) < 2 && return false
        judged = [(w = words[i], after_sep = occursin(SEPARATOR, words[i - 1]))
                  for i in 2:length(words) if !occursin(SEPARATOR, words[i])]
        isempty(judged) && return false
        any(j -> occursin(r"^[A-Z]", j.w) && !j.after_sep && !expected_cap(j.w), judged) &&
            all(j -> occursin(r"^[A-Z]", j.w) || j.after_sep || expected_cap(j.w), judged)
    end

    labels, tips2 = Tuple{String,String}[], Tuple{String,String}[]
    each_spec() do f, spec
        l = get(spec, :label, nothing)
        l isa AbstractString && push!(labels, (f, join(split(String(l)), " ")))
        each_spec_param(spec_get(spec, "params")) do p, _
            l = spec_get(p, "label")
            l isa AbstractString && push!(labels, (f, join(split(String(l)), " ")))
            t = spec_get(p, "tip")
            t isa AbstractString && push!(tips2, (f, join(split(String(t)), " ")))
        end
    end

    @test length(labels) > 150              # the walk found task + param labels

    titled = ["$f: $l" for (f, l) in labels if title_case(l) && !(l in ALLOWED_TITLE_CASE)]
    @test isempty(titled)

    # `…`/`...` is a continuation, not a sentence end.
    dotted = ["$f: $t" for (f, t) in tips2
              if occursin(r"[^.]\.$", t) && !(t in ALLOWED_TRAILING_PERIOD)]
    @test isempty(dotted)

    # Only the words with a decided winner. Create/Add, Delete/Remove and Run/Start are NOT
    # synonyms (see the vocabulary table in docs/UI.md) and are deliberately absent.
    BANNED = ["Choose" => "Select", "Pick" => "Select", "Display" => "Show",
              "Execute" => "Run", "Modify" => "Edit", "Discard" => "Remove"]
    wrong_verb = ["$f: \"$s\" — use $good" for (f, s) in vcat(labels, tips2)
                  for (bad, good) in BANNED if occursin(Regex("\\b$bad\\b", "i"), s)]
    @test isempty(wrong_verb)
end

# ── Stats module (docs/todo/STATS_ANNOTATIONS_PLAN.md) ─────────────────────
#
# Pins the glue between `run_stats` and HypothesisTests.jl. We don't test the underlying
# test math (that's HypothesisTests' own suite) — we test that the API is wired correctly:
# test dispatch, insertion order preserved (via Vector{Pair}), pairwise Bonferroni-adjusted,
# ns/star ladder, error handling for empty and too-few groups.
@testset "run_stats" begin
    # Two clearly-different groups → mannwhitney by default; p is very small; significance
    # ladder is at least ** (matches STATS_ANNOTATIONS_PLAN.md → S0-1).
    @testset "2 groups auto → mannwhitney" begin
        r = Cecelia.run_stats(["WT" => [1.0,2,3,4,5], "KO" => [10.0,11,12,13,14]])
        @test r.test == :mannwhitney
        @test r.groups == ["WT", "KO"]
        @test r.n == [5, 5]
        @test r.means[1] ≈ 3.0 && r.means[2] ≈ 12.0
        @test r.medians[1] ≈ 3.0 && r.medians[2] ≈ 12.0
        @test r.p_value < 0.05
        @test r.significance in ("*", "**", "***", "****")
        @test occursin("Mann-Whitney", r.method_note)
        @test isempty(r.comparison_pairs)   # omnibus IS the pair for 2 groups
    end

    # `auto` also has to say WHY. The UI showed the resolved test name and nothing else, so a user
    # had no way to know the basis — and deriving the explanation in the frontend would fork the
    # rule (change `_auto_test` and the tooltip would quietly keep claiming the old basis).
    @testset "auto states its basis; a NAMED test states none" begin
        two = Cecelia.run_stats(["WT" => [1.0,2,3], "KO" => [9.0,10,11]])
        @test occursin("2 groups", two.auto_reason)
        @test occursin("Mann-Whitney", two.auto_reason)
        three = Cecelia.run_stats(["A" => [1.0,2,3], "B" => [9.0,10,11], "C" => [20.0,21,22]])
        @test occursin("3 groups", three.auto_reason)
        @test occursin("Kruskal-Wallis", three.auto_reason)
        # both auto choices are rank-based — that's the reassurance the note has to carry, since
        # `auto` never runs a normality check
        @test occursin("rank-based", two.auto_reason) && occursin("rank-based", three.auto_reason)
        # nothing was chosen for the user, so there is nothing to explain
        @test isempty(Cecelia.run_stats(["A" => [1.0,2,3], "B" => [9.0,10,11]]; test=:ttest).auto_reason)
        @test isempty(Cecelia.run_stats(["A" => [1.0,2,3], "B" => [9.0,10,11]]; test=:mannwhitney).auto_reason)
        # the reason must name the test that actually ran — one rule, not two
        for n in (2, 3, 7)
            @test occursin(n == 2 ? "Mann-Whitney" : "Kruskal-Wallis", Cecelia._auto_reason(n))
            @test occursin("$(n) groups", Cecelia._auto_reason(n))
        end
        # …and it reaches the wire under `autoReason`
        d = Cecelia._stats_result_dict(two)
        @test d["autoReason"] == two.auto_reason
        @test isempty(Cecelia._stats_result_dict(
            Cecelia.run_stats(["A" => [1.0,2,3], "B" => [9.0,10,11]]; test=:ttest))["autoReason"])
    end

    # Two identical groups → p ≈ 1, "ns".
    @testset "identical groups → ns" begin
        r = Cecelia.run_stats(["A" => [1.0,2,3,4,5], "B" => [1.0,2,3,4,5]])
        @test r.p_value > 0.9
        @test r.significance == "ns"
    end

    # Welch's t-test opt-in — different method note, still small p on separated data.
    @testset "ttest opt-in" begin
        r = Cecelia.run_stats(["A" => [1.0,2,3,4], "B" => [10.0,11,12,13]]; test=:ttest)
        @test r.test == :ttest
        @test occursin("t-test", r.method_note)
        @test r.p_value < 0.05
    end

    # Three groups → kruskal by default, pairs are populated with Bonferroni-adjusted values.
    @testset "3 groups → kruskal + pairwise" begin
        r = Cecelia.run_stats([
            "A" => [1.0,2,3,4,5], "B" => [10.0,11,12,13,14], "C" => [20.0,21,22,23,24]])
        @test r.test == :kruskal
        @test r.groups == ["A", "B", "C"]
        @test occursin("Kruskal-Wallis", r.method_note)
        @test length(r.comparison_pairs) == 3   # (A,B), (A,C), (B,C)
        for (a, b, p_adj, sig) in r.comparison_pairs
            @test p_adj >= 0.0 && p_adj <= 1.0
            @test sig in ("ns", "*", "**", "***", "****")
        end
        # A vs C is the widest gap → definitely significant post-Bonferroni.
        ac = only(p for (a, b, p, _) in r.comparison_pairs if a == "A" && b == "C")
        @test ac < 0.05
    end

    # ANOVA opt-in with 3 groups.
    @testset "anova opt-in (3 groups)" begin
        r = Cecelia.run_stats([
            "A" => [1.0,2,3,4,5], "B" => [5.0,6,7,8,9], "C" => [10.0,11,12,13,14]];
            test=:anova)
        @test r.test == :anova
        @test occursin("ANOVA", r.method_note)
        @test r.p_value < 0.05
    end

    # Insertion order preserved (Vector of Pairs guarantees it — this asserts we don't sort).
    @testset "group order preserved" begin
        r = Cecelia.run_stats(["Z" => [1.0,2,3], "A" => [4.0,5,6], "M" => [7.0,8,9]])
        @test r.groups == ["Z", "A", "M"]
    end

    # Error paths.
    @testset "errors" begin
        @test_throws ArgumentError Cecelia.run_stats(["only" => [1.0,2,3]])
        @test_throws ArgumentError Cecelia.run_stats(["A" => Float64[], "B" => [1.0,2]])
        # 2-group tests refuse when given ≠2 groups.
        three = ["A" => [1.0,2], "B" => [3.0,4], "C" => [5.0,6]]
        @test_throws ArgumentError Cecelia.run_stats(three; test=:ttest)
        @test_throws ArgumentError Cecelia.run_stats(three; test=:mannwhitney)
        @test_throws ArgumentError Cecelia.run_stats(["A"=>[1.0,2], "B"=>[3.0,4]];
                                                    test=:notarealtest)
    end
end

# ── Segmentation label-store conventions + live outputs ────────────────────
# The algorithm-agnostic half of a segmentation task (app/src/segmentation.jl) and the
# `live_outputs` trait that makes a still-being-written store discoverable (task.jl).
#
# Declared out here because a `struct` can't be defined inside a @testset (it wraps its body in a
# local scope): a task whose live-output declaration throws, used below to assert the scheduler
# treats a preview as a convenience rather than a precondition for running.
struct _BadLiveTask <: Cecelia.CciaTask end
Cecelia.live_outputs(::_BadLiveTask, ::AbstractDict) = error("boom")

@testset "segmentation conventions and live outputs" begin
    @testset "segment_label_files mirrors the writer" begin
        # 'base' → {vn}.zarr, any other matchAs → an extra {vn}_{ma}.zarr (segmentation_utils._store_path)
        @test Cecelia.segment_label_files("X", Dict("0" => Dict("matchAs" => "base"))) == ["X.zarr"]
        @test Cecelia.segment_label_files("X", Dict("0" => Dict("matchAs" => "base"),
                                                   "1" => Dict("matchAs" => "nuc"))) ==
              ["X.zarr", "X_nuc.zarr"]
        # two models of the SAME type write one store between them, not two
        @test Cecelia.segment_label_files("X", Dict("0" => Dict("matchAs" => "base"),
                                                   "1" => Dict("matchAs" => "base"))) == ["X.zarr"]
        # a model with no matchAs is 'base' (the writer's own default)
        @test Cecelia.segment_label_files("X", Dict("0" => Dict{String,Any}())) == ["X.zarr"]
        # no models at all (REPL call, malformed params) still names the primary store
        @test Cecelia.segment_label_files("X", nothing) == ["X.zarr"]
        @test Cecelia.segment_label_files("X", Dict{String,Any}()) == ["X.zarr"]
    end

    @testset "preview params are translated the way the RUN translates them" begin
        # THE reported bug: `preview worker: ValueError: invalid literal for int() with base 10: 'CH3'`.
        # The frontend sends channel NAMES and a bare model name; Python wants 0-based indices and a
        # checkpoint PATH. `_run_task` translated them inline, so the preview — which sends the
        # frontend's params straight to the worker — sent names. Sharing `predict_slice` does not make
        # the params shared; preparing them is the task's job, hence `preview_params`.
        mktempdir() do dir
            img = CciaImage(; uid = "uid1", name = "n", dir = joinpath(dir, "1", "uid1"))
            mkpath(img._dir)
            img.filepath["default"] = "ccidImage.ome.zarr"
            img.im_channel_names["default"] = ["CH1", "CH2", "CH3", "CH4"]
            save!(img)
            raw = Cecelia.read_ccid_raw(Cecelia.state_file(img))

            params = Dict{String,Any}("models" => Dict("0" => Dict{String,Any}(
                "model" => "cyto3", "matchAs" => "base",
                "cellChannels" => ["CH3"], "nucChannels" => String[])))

            m = Cecelia.cellpose_models_for_python(params, raw)["0"]
            @test m["cellChannels"] == [2]           # 0-based: CH3 is the third channel
            @test m["nucChannels"] == Int[]
            @test m["model"] == "cyto3"              # a built-in name passes through untouched

            # the hook the preview calls produces the same thing, and leaves other params alone
            got = Cecelia.preview_params(Cecelia.CellposeSegment(), params, img)
            @test got["models"]["0"]["cellChannels"] == [2]
            @test !haskey(params["models"]["0"], "cellChannels") ||
                  params["models"]["0"]["cellChannels"] == ["CH3"]   # input not mutated

            # idempotent: already-translated indices survive a second pass (a REPL/chain caller)
            @test Cecelia.cellpose_models_for_python(got, raw)["0"]["cellChannels"] == [2]

            # A channel the image does not have RAISES — it is never turned into a bogus index, and no
            # longer silently dropped either. Dropping satisfied the letter of "don't fabricate an
            # index" while still segmenting: on no channels, or on whichever of the pair survived. The
            # message names what was available, exactly like the missing-checkpoint case below; both are
            # "a param we cannot resolve", and this file already prefers raising for that.
            bad = Dict{String,Any}("models" => Dict("0" => Dict{String,Any}(
                "cellChannels" => ["CH9"], "nucChannels" => [])))
            bad_err = try; Cecelia.cellpose_models_for_python(bad, raw); nothing; catch e; e end
            @test bad_err isa ErrorException
            @test occursin("CH9", bad_err.msg) && occursin("CH1", bad_err.msg)

            # a missing CUSTOM checkpoint raises with a message worth showing, rather than failing
            # deep inside cellpose — the second translation the preview used to skip
            custom = Dict{String,Any}("models" => Dict("0" => Dict{String,Any}(
                "model" => "no-such-model.pth", "cellChannels" => ["CH1"], "nucChannels" => [])))
            err = try; Cecelia.cellpose_models_for_python(custom, raw); nothing
                  catch e; e end
            @test err isa ErrorException
            @test occursin("no-such-model.pth", err.msg)

            # and the composite delegates to its previewable step, since that is what the page runs
            composite = Cecelia._task_from_fun_name("segment.cellposeMeasure")
            @test Cecelia.preview_params(composite, params, img)["models"]["0"]["cellChannels"] == [2]

            # a task with no overload passes params through untouched
            @test Cecelia.preview_params(Cecelia.MeasureLabels(), params, img) === params

            # ── the SECOND half of "the way the RUN does it": section params must be lifted flat.
            # Reported live: "Run would tile this comes up all the time. i've set the tiling to
            # 4096 px. the image is not even 1000px." `blockSize` lives in the `imageTiling`
            # section, so the frontend sends it NESTED; `run_task` flattens, the preview did not,
            # and `SegmentationUtils` silently fell back to its own 512 default. Nothing errors —
            # which is why this needs a test rather than a fix.
            cellpose = Cecelia.CellposeSegment()
            nested = Dict{String,Any}(
                "models" => params["models"],
                "imageTiling" => Dict{String,Any}("blockSize" => 4096, "overlap" => 128))
            flat = Cecelia.preview_params_for_run(cellpose, nested, img)
            @test flat["blockSize"] == 4096            # NOT SegmentationUtils' 512 default
            @test flat["overlap"] == 128
            @test !haskey(flat, "imageTiling")         # lifted, not duplicated
            @test flat["models"]["0"]["cellChannels"] == [2]   # ...and still translated

            # idempotent, so an already-flat bag (REPL, chain, a re-translated dict) is safe
            @test Cecelia.preview_params_for_run(cellpose, flat, img)["blockSize"] == 4096

            # an explicit top-level value wins over a section entry of the same name — the preview
            # must not resurrect a stale nested copy
            both = Dict{String,Any}("models" => params["models"], "blockSize" => 1024,
                                    "imageTiling" => Dict{String,Any}("blockSize" => 4096))
            @test Cecelia.preview_params_for_run(cellpose, both, img)["blockSize"] == 1024

            # the composite goes through the same entry point, since that is what the page runs
            @test Cecelia.preview_params_for_run(composite, nested, img)["blockSize"] == 4096

            # ...and through the shape the API actually receives: a JSON body. Nested objects come
            # back as JSON3 values with SYMBOL keys, which is the standing trap (CLAUDE.md — a
            # `isa Dict` guard is false for `JSON3.Object`). If the lift missed those, this would be
            # the one path that regressed while every hand-built Dict above kept passing.
            body = """{"models":{"0":{"model":"cyto3","matchAs":"base",
                       "cellChannels":["CH3"],"nucChannels":[]}},
                       "imageTiling":{"blockSize":4096,"overlap":128}}"""
            from_json = JSON3.read(body, Dict{String,Any})
            j = Cecelia.preview_params_for_run(cellpose, from_json, img)
            @test j["blockSize"] == 4096
            @test j["overlap"] == 128
            @test !haskey(j, "imageTiling")
            @test j["models"]["0"]["cellChannels"] == [2]
        end
    end

    @testset "task_previewable is declared, and composites inherit it" begin
        # The trait replaced the frontend inferring previewability from a cellpose-shaped `models`
        # bag — right about cellpose, silently wrong about every other backend.
        @test Cecelia.task_previewable(Cecelia.CellposeSegment())

        # Default is FALSE: a task says nothing unless the worker can actually run it.
        for t in (Cecelia.MeasureLabels(), Cecelia.DriftCorrect(), Cecelia.ImportOmezarr())
            @test !Cecelia.task_previewable(t)
        end

        # THE overload that matters: the segmentation module page runs the composite, not
        # segment.cellpose. This is how the live preview shipped broken in #421.
        composite = Cecelia._task_from_fun_name("segment.cellposeMeasure")
        @test composite isa Cecelia.CompositeTask
        @test Cecelia.task_previewable(composite)
        # `any`, not `all` — measureLabels has nothing to preview but must not veto the segmentation
        @test any(Cecelia.task_previewable, Cecelia._composite_steps(composite))
        @test !all(Cecelia.task_previewable, Cecelia._composite_steps(composite))

        # every registered task answers without throwing — the definitions route stamps this onto
        # every spec, so one bad overload would otherwise break the whole task picker
        for (fun, task) in Cecelia._fun_name_map()
            @test Cecelia.task_previewable(task) isa Bool
        end
    end

    # The staging mechanism itself lives in `zarr_utils.staged_store` (Python, where the writers
    # are). Julia mirrors the two suffixes to name the in-progress store a preview watches and to
    # sweep debris a killed run leaves. Nothing connects the two at runtime, so pin them together
    # here — silent drift would aim the preview at a path no writer ever creates.
    @testset "store staging suffixes match the Python side" begin
        py = read(joinpath(dirname(dirname(@__DIR__)), "python", "cecelia", "utils",
                           "zarr_utils.py"), String)
        m_staging    = match(r"^STAGING_SUFFIX\s*=\s*'([^']+)'"m, py)
        m_superseded = match(r"^SUPERSEDED_SUFFIX\s*=\s*'([^']+)'"m, py)
        @test !isnothing(m_staging)
        @test !isnothing(m_superseded)
        @test m_staging.captures[1]    == Cecelia.STORE_STAGING_SUFFIX
        @test m_superseded.captures[1] == Cecelia.STORE_SUPERSEDED_SUFFIX
        @test Cecelia.staging_store_path(joinpath("labels", "X.zarr")) ==
              joinpath("labels", "X.zarr.partial")
    end

    @testset "live_outputs is opt-in per task" begin
        params = Dict{String,Any}("outputValueName" => "X",
                                  "models" => Dict("0" => Dict("matchAs" => "base"),
                                                   "1" => Dict("matchAs" => "nuc")))
        lo = Cecelia.live_outputs(Cecelia.CellposeSegment(), params)
        @test length(lo) == 1
        @test lo[1].kind == "labels"
        # value_name is the REGISTERED name, unsuffixed — the viewer names the layer `({vn})`
        # from it, and that prefix is what colour_labels and layer eviction match on.
        @test lo[1].value_name == "X"
        # ...but the files are the STAGING stores. A run writes through `staged_store`, so while
        # it is going the final path either doesn't exist or (on a re-run) still holds the
        # PREVIOUS segmentation — a preview aimed there would show stale labels. Asserted as
        # literals so changing the suffix has to be a deliberate edit here too.
        @test lo[1].files == ["X.zarr.partial", "X_nuc.zarr.partial"]
        @test lo[1].files ==
              Cecelia.staging_store_path.(Cecelia.segment_label_files("X", params["models"]))
        # falls back to the default value_name like the task itself does
        @test Cecelia.live_outputs(Cecelia.CellposeSegment(),
                                   Dict{String,Any}())[1].value_name == Cecelia.VERSIONED_DEFAULT_VAL

        # A task that assembles its output in RAM and writes it once has nothing to watch, and
        # must NOT claim otherwise — branching writes its store at the very end of the run.
        @test isempty(Cecelia.live_outputs(Cecelia.Branching(), params))
        @test isempty(Cecelia.live_outputs(Cecelia.MeasureLabels(), params))
        @test isempty(Cecelia.live_outputs(Cecelia.BayesianTracking(), params))
    end

    # A preview is a convenience: a task whose declaration throws must still run.
    @testset "a throwing live_outputs never blocks the task" begin
        @test isempty(Cecelia._live_outputs_for(_BadLiveTask(), Dict{String,Any}()))
    end

    # REGRESSION: the composite is what the segmentation module page actually runs, and its steps
    # execute via `_run_task` (no TaskRecord of their own), so the composite must answer for them.
    # This shipped broken — `segment.cellposeMeasure` declared nothing and no preview appeared.
    @testset "a composite declares its steps' live outputs" begin
        params = Dict{String,Any}("outputValueName" => "X",
                                  "models" => Dict("0" => Dict("matchAs" => "base")))
        lo = Cecelia.live_outputs(Cecelia.CompositeTask("segment.cellposeMeasure"), params)
        @test length(lo) == 1
        @test lo[1] == (kind = "labels", value_name = "X", files = ["X.zarr.partial"])

        # a composite of non-streaming steps still declares nothing
        @test isempty(Cecelia.live_outputs(Cecelia.CompositeTask("cleanupImages.afDriftCorrect"), params))
        # unknown composite / no spec → empty, never a throw
        @test isempty(Cecelia.live_outputs(Cecelia.CompositeTask("not.a.composite"), params))
    end

    # The preview worker's request shape. The worker owns the region DECISION (one z-plane,
    # clamping, the 2D fallback — tested in python/cecelia/tests/test_preview_region.py); Julia
    # only resolves which image version to read and where the scratch store goes. Both halves are
    # pinned so they can't drift into disagreeing about the contract.
    @testset "preview_request resolves the same image version a run would" begin
        mktempdir() do dir
            img = CciaImage(; uid = "uid1", name = "n", dir = joinpath(dir, "1", "uid1"))
            img.filepath["default"]   = "ccidImage.ome.zarr"
            img.filepath["corrected"] = "ccidDriftCorrected.ome.zarr"

            region = Dict("xy" => Dict("X" => [0, 512], "Y" => [0, 512]),
                          "z" => 8, "t" => 0, "ndisplay" => 2)

            # reads the version named by the task's OWN valueName — a preview of a corrected
            # image must not silently segment the original
            req = Cecelia.preview_request(
                img, Dict("valueName" => "corrected", "models" => Dict()), region)
            @test req["type"] == "preview"
            @test endswith(req["imPath"], joinpath("0", "uid1", "ccidDriftCorrected.ome.zarr"))
            @test req["taskDir"] == img._dir
            @test req["region"]["z"] == 8

            # default falls back to the primary version, like the task does
            req_default = Cecelia.preview_request(img, Dict("models" => Dict()), region)
            @test endswith(req_default["imPath"], "ccidImage.ome.zarr")

            # an unknown valueName is an error, not a silent segmentation of the wrong image
            @test_throws ErrorException Cecelia.preview_request(
                img, Dict("valueName" => "nope", "models" => Dict()), region)

            # the explicit-paths form: what the API uses, with the store the VIEWER has open, so
            # the pixels and the region can't come from differently-shaped versions
            direct = Cecelia.preview_request(
                "/somewhere/open.ome.zarr", "/somewhere/meta",
                Dict("valueName" => "corrected", "models" => Dict()), region;
                value_name = "B")
            @test direct["imPath"] == "/somewhere/open.ome.zarr"   # NOT re-resolved from ccid
            @test direct["taskDir"] == "/somewhere/meta"
            @test direct["outputValueName"] == "B"
            @test direct["region"]["z"] == 8

            # Channel DISPLAY names travel with the request, because `ccid.json` is the only
            # authoritative copy — the worker deriving them from the store's OME-XML instead is what
            # made every corrected AF layer render grey (its `source` named a layer that did not
            # exist). Sent only when known: an empty list would overwrite the fallback with nothing.
            @test !haskey(direct, "channelNames")
            named = Cecelia.preview_request(
                "/somewhere/open.ome.zarr", "/somewhere/meta",
                Dict("models" => Dict()), region; channel_names = ["SHG", "mem-TOM"])
            @test named["channelNames"] == ["SHG", "mem-TOM"]

            # and the image form fills them in from ccid, per the version being previewed
            img.im_channel_names["default"] = ["SHG", "nuc-GFP", "mem-TOM", "CD169-Kat"]
            from_img = Cecelia.preview_request(img, Dict("models" => Dict()), region)
            @test from_img["channelNames"] == ["SHG", "nuc-GFP", "mem-TOM", "CD169-Kat"]
        end
    end

    @testset "a preview reply becomes a viewer command without Julia decoding it" begin
        # Julia is a pass-through: the blocks move worker → viewer untouched (the codec lives in
        # cecelia.utils.block_transfer, used by both Python ends).
        #
        # A reply carries a LIST of layers, each with its own `kind`, because one task can preview
        # several things: AF correction returns one image layer per corrected channel so they sit
        # beside the originals to be flipped against. A single mask field plus a type flag — which is
        # what this was — could not express that.
        mask = Dict("shape" => [1, 1, 4, 4], "dtype" => "<u4", "data" => "eJxjYGBgAAAABAAB")
        img  = Dict("shape" => [1, 1, 4, 4], "dtype" => "<u2", "data" => "eJxjYGBgAAAABAAC")
        layers = [
            Dict("kind" => "labels", "name" => "Preview", "block" => mask,
                 "shape" => [10, 5, 64, 64], "axes" => ["T", "Z", "Y", "X"]),
            Dict("kind" => "image", "name" => "nuc-GFP AF", "block" => img,
                 "shape" => [10, 5, 64, 64], "axes" => ["T", "Z", "Y", "X"]),
        ]
        reply = Dict("layers" => layers,
                     "region" => Dict("T" => [3, 4], "Z" => [1, 2],
                                      "Y" => [0, 4], "X" => [0, 4]),
                     "valueName" => "A", "counts" => Dict("base" => 7))
        cmd = Cecelia.preview_show_command(reply)
        @test cmd["type"] == "show_task_preview"
        @test cmd["layers"] === layers                      # not re-encoded, not copied
        @test cmd["layers"][1]["block"] === mask
        @test cmd["show"] == true
        # the value_name is the REAL one — an unsuffixed stem is what lets `({vn}) Preview` and
        # `({vn}) Labels` evict each other in the viewer instead of stacking
        @test cmd["value_name"] == "A"
        @test !occursin("__preview", cmd["value_name"])

        # no layers at all is a fault, not a viewer showing nothing
        @test_throws ErrorException Cecelia.preview_show_command(
            filter(p -> first(p) != "layers", reply))
        @test_throws ErrorException Cecelia.preview_show_command(
            merge(reply, Dict("layers" => Any[])))

        # a layer missing any of its geometry is a fault too — caught HERE rather than as a Python
        # traceback in the viewer, which is the point of validating in the pass-through
        for missing_key in ("kind", "name", "block", "shape", "axes")
            broken_layer = filter(p -> first(p) != missing_key, layers[1])
            @test_throws ErrorException Cecelia.preview_show_command(
                merge(reply, Dict("layers" => Any[broken_layer])))
        end

        # an unknown kind is refused rather than passed on for the viewer to guess at
        @test_throws ErrorException Cecelia.preview_show_command(
            merge(reply, Dict("layers" => Any[merge(layers[1], Dict("kind" => "heatmap"))])))

        # `source` (the viewer layer a corrected channel derives from, so the bridge can mirror its
        # colormap) rides through untouched and is OPTIONAL — Julia neither requires it nor interprets
        # it. Required here would break the one thing the pass-through exists to allow: the two Python
        # ends evolving the payload without this file learning every field.
        sourced = merge(reply, Dict("layers" => Any[merge(layers[2], Dict("source" => "nuc-GFP"))]))
        @test Cecelia.preview_show_command(sourced)["layers"][1]["source"] == "nuc-GFP"
        @test Cecelia.preview_show_command(
            merge(reply, Dict("layers" => Any[layers[2]])))["layers"][1] === layers[2]
    end

    @testset "a composite says which steps it does not preview" begin
        # `preview_params` delegates to the FIRST previewable step, so a composite previews one step
        # and the others silently do not happen. Correct — the alternative is previewing nothing — but
        # it has to be said, because a skipped step can change what the previewed one means:
        # afDriftCorrect previews AF and skips drift correction, which expands the canvas and shifts
        # every frame, so the geometry on screen is not the geometry the run produces.
        af_drift = Cecelia._task_from_fun_name("cleanupImages.afDriftCorrect")
        skipped = Cecelia.preview_steps_not_previewed(af_drift)
        @test length(skipped) == 1
        @test skipped[1]["fun"] == "cleanupImages.driftCorrect"
        @test skipped[1]["label"] == "Drift correction"      # the spec's own label, not a fun_name

        # the segmentation composite likewise: measurement is not previewed
        seg = Cecelia._task_from_fun_name("segment.cellposeMeasure")
        @test [x["fun"] for x in Cecelia.preview_steps_not_previewed(seg)] == ["segment.measureLabels"]

        # a plain task skips nothing, and neither does a non-composite previewable one
        for fn in ("segment.cellpose", "cleanupImages.afCorrect", "cleanupImages.driftCorrect")
            @test isempty(Cecelia.preview_steps_not_previewed(Cecelia._task_from_fun_name(fn)))
        end
    end

    @testset "AF correction is previewable, with its own param translation" begin
        @test Cecelia.task_previewable(Cecelia.AfCorrect())
        mktempdir() do dir
            img = CciaImage(; uid = "af1", name = "n", dir = joinpath(dir, "1", "af1"))
            mkpath(img._dir)
            img.filepath["default"] = "ccidImage.ome.zarr"
            img.im_channel_names["default"] = ["SHG", "nuc-GFP", "mem-TOM", "CD169-Kat"]
            save!(img)

            # A name this image does not have RAISES rather than dropping out of the competitor list.
            # Dropping looked harmless but changes the correction silently: the weight's denominator
            # loses a term, so every corrected voxel is wrong by an amount nothing reports. Naming a
            # channel that isn't there is a stale saved param, and the fix is to re-pick it.
            stale = Dict{String,Any}("afCombinations" => Dict("1" => Dict{String,Any}(
                "competingChannels" => ["CH4", "CD169-Kat"],
                "targetChannel"     => ["mem-TOM"])))
            stale_err = try
                Cecelia.preview_params_for_run(Cecelia.AfCorrect(), stale, img); nothing
            catch e; e end
            @test stale_err isa ErrorException
            @test occursin("CH4", stale_err.msg) && occursin("CD169-Kat", stale_err.msg)

            params = Dict{String,Any}("afCombinations" => Dict("1" => Dict{String,Any}(
                "competingChannels" => ["CD169-Kat"],
                "targetChannel"     => ["mem-TOM"])))
            out = Cecelia.preview_params_for_run(Cecelia.AfCorrect(), params, img)
            # targetChannel re-keys the combination to the channel being corrected (mem-TOM → 2)
            @test collect(keys(out["afCombinations"])) == ["2"]
            @test out["afCombinations"]["2"]["competingChannels"] == [3]
            @test !haskey(out["afCombinations"]["2"], "targetChannel")

            # idempotent: already-translated indices survive a second pass (a chain or REPL caller)
            again = Cecelia.preview_params_for_run(Cecelia.AfCorrect(), out, img)
            @test again["afCombinations"]["2"]["competingChannels"] == [3]

            # and the composite delegates to AF, since that is the step it can preview
            comp = Cecelia._task_from_fun_name("cleanupImages.afDriftCorrect")
            @test Cecelia.preview_params_for_run(comp, params, img)["afCombinations"]["2"]["competingChannels"] == [3]

            # A target named inside its OWN competitor list is dropped, not squared into the denominator
            # a second time — that would quietly halve the channel's own output. Two separate widgets,
            # so picking the same channel in both is an easy slip with one obvious intent.
            self_ref = Dict{String,Any}("afCombinations" => Dict("1" => Dict{String,Any}(
                "competingChannels" => ["mem-TOM", "CD169-Kat"],
                "targetChannel"     => ["mem-TOM"])))
            selfed = Cecelia.preview_params_for_run(Cecelia.AfCorrect(), self_ref, img)
            @test selfed["afCombinations"]["2"]["competingChannels"] == [3]

            # ...and duplicates collapse, so a name listed twice cannot double its weight either
            dupes = Dict{String,Any}("afCombinations" => Dict("1" => Dict{String,Any}(
                "competingChannels" => ["CD169-Kat", "CD169-Kat"],
                "targetChannel"     => ["mem-TOM"])))
            @test Cecelia.preview_params_for_run(Cecelia.AfCorrect(), dupes,
                                                 img)["afCombinations"]["2"]["competingChannels"] == [3]
        end
    end

    @testset "the preview worker gets its own port" begin
        # a second resident process must not collide with the napari bridge (7655) or Pluto (7660)
        @test Cecelia.PREVIEW_PORT != Cecelia.NAPARI_PORT
        @test Cecelia.PREVIEW_PORT ∉ (7655, 7660, 8080, 5173)
        # not alive until launched — `preview_alive` must never report true for a null process
        @test !Cecelia.preview_alive(Cecelia.PreviewWorker())
    end

    @testset "a stale preview worker is stopped by port, not by handle" begin
        # THE BUG THIS PINS. On a protocol mismatch the backend must remove the worker holding :7656.
        # It only ever PINGED that process, so the handle it has is a bare `PreviewWorker()` with no
        # `proc` — and `close!` on that is a silent no-op. Kill-by-handle therefore left the stale
        # worker listening, the replacement could not bind, and the replacement's readiness ping was
        # answered by the process being replaced: a relaunch loop serving the old code, strictly worse
        # than the mismatch. So the adoption path must kill by PORT, like `_ensure_viewer!` does.
        adopted = Cecelia.PreviewWorker()          # exactly what `_ensure_preview!` probes with
        @test adopted.proc === nothing
        Cecelia.close!(adopted)                    # no-op, and must not throw pretending otherwise
        @test adopted.proc === nothing

        # `_kill_listeners_on_port` is the one helper for this (never inline kill/lsof/taskkill), and
        # the mismatch branch in the API layer has to use it. Source-level because that branch needs a
        # live stale worker to exercise.
        api_src = read(joinpath(dirname(dirname(pathof(Cecelia))), "..", "api", "src",
                                "preview_api.jl"), String)
        @test occursin("_kill_listeners_on_port(PREVIEW_PORT)", api_src)
        # CODE only — the comment above that call names `close!(probe)` to say why it is wrong, and a
        # naive text search cannot tell an explanation from the thing it warns about.
        api_code = filter(l -> !startswith(strip(l), "#"), split(api_src, '\n'))
        @test !any(l -> occursin("close!(probe)", l), api_code)

        # And readiness is the protocol, not merely a reply — the other half of the same loop.
        preview_src = read(joinpath(dirname(pathof(Cecelia)), "preview.jl"), String)
        @test occursin("protocol == PREVIEW_PROTOCOL", preview_src)
    end

    @testset "language boundaries agree on their protocol" begin
        # THE PROBLEM THIS TABLE SOLVES. Julia and Python each hold their own copy of a version, and the
        # only thing keeping them equal is that someone remembers to change both. Measured record before
        # this test existed: the preview pair was bumped by hand three times and the fourth was nearly
        # missed; the napari bridge and the params contract had no version at all.
        #
        # A mismatch is never a clean failure. A stale peer answers the handshake perfectly and then
        # misreads the actual work — it has surfaced as `unexpected keyword argument 'mask'`, as a bare
        # "Preview failed", and as `invalid literal for int() with base 10: 'CH3'`, none of which name the
        # cause. So every boundary gets a version, and every version gets asserted here.
        #
        # A fourth boundary = one more row.
        repo = joinpath(dirname(dirname(pathof(Cecelia))), "..")
        boundaries = [
            # (what,            python file,                          python const,       julia value)
            ("preview worker",  "preview/preview_worker.py",           "PROTOCOL",         Cecelia.PREVIEW_PROTOCOL),
            ("napari bridge",   "napari/napari_bridge.py",             "PROTOCOL",         Cecelia.NAPARI_PROTOCOL),
            ("params contract", "python/cecelia/utils/script_utils.py", "CONTRACT_VERSION", Cecelia.PY_CONTRACT_VERSION),
        ]
        for (what, rel, const_name, julia_value) in boundaries
            path = joinpath(repo, rel)
            @test isfile(path)
            m = match(Regex("^" * const_name * raw"\s*=\s*(\d+)", "m"), read(path, String))
            @test m !== nothing
            m === nothing && continue
            py = parse(Int, m.captures[1])
            @test py == julia_value
            py == julia_value ||
                @warn "$what: Python says $py, Julia says $julia_value — bump BOTH sides" rel
        end
    end

    @testset "the params contract is checked where every runner already goes" begin
        # The guard lives in `script_params`, not in each runner, so a NEW runner is covered by writing
        # nothing. Asserted on the source because the check runs in a subprocess we do not spawn here.
        su = read(joinpath(dirname(dirname(pathof(Cecelia))), "..",
                           "python", "cecelia", "utils", "script_utils.py"), String)
        @test occursin("def check_contract_version", su)
        @test occursin(r"def script_params\(\):(?s).{0,600}check_contract_version\(\)", su)
        # ...and run_py is what supplies it, as an env var rather than a params field
        pr = read(joinpath(dirname(dirname(pathof(Cecelia))), "src", "py_runner.jl"), String)
        @test occursin("CECELIA_PY_CONTRACT", pr)
        @test occursin("PY_CONTRACT_VERSION", pr)
    end

    @testset "img_labels_path resolves registered and in-progress stores" begin
        mktempdir() do dir
            img = CciaImage(; uid = "uid1", name = "name1", dir = joinpath(dir, "1", "uid1"))
            img.labels["A"] = ["A.zarr", "A_nuc.zarr"]
            @test Cecelia.img_labels_dir(img) == joinpath(img._dir, "labels")
            # registered → the recorded filename (first of the set)
            @test Cecelia.img_labels_path(img, "A") == joinpath(img._dir, "labels", "A.zarr")
            # NOT registered → the convention. This is where a FINISHED store lands; a run in
            # progress writes to the staging sibling and is renamed here on completion.
            @test Cecelia.img_labels_path(img, "X") == joinpath(img._dir, "labels", "X.zarr")
            @test Cecelia.staging_store_path(Cecelia.img_labels_path(img, "X")) ==
                  joinpath(img._dir, "labels", "X.zarr.partial")
        end
    end

    # A patch with a bad script path fails only when a user clicks Apply, so resolve every
    # registered one the same way `run_py` does. Nothing else covers maintenance.jl.
    @testset "every maintenance patch resolves to a script on disk" begin
        repo = dirname(dirname(@__DIR__))
        patches = Cecelia.maintenance_patches()
        @test !isempty(patches)
        @test length(unique(p.id for p in patches)) == length(patches)
        for p in patches
            path = startswith(p.script, "tasks/") ?
                   joinpath(repo, "app", "src", p.script) :
                   joinpath(repo, "python", "cecelia", p.script)
            @test isfile(path)
            @test !isnothing(Cecelia.maintenance_patch(p.id))
            # Copy budget (docs/UI.md → UI copy). This description sits in Settings and is read every
            # time, so it gets one line + the one caveat that matters — the store-debris entry had
            # grown to 674 characters explaining its own detection strategy, which belongs in the
            # runner. 160 leaves room for a caveat and none for an essay.
            @test length(p.description) <= 160
            @test !isempty(p.description)
            @test !endswith(p.title, ".")            # a title is a fragment, not a sentence
        end
    end
end


# ── chipSelect param type ────────────────────────────────────────────────────
# "1,2,4,8" was a raw text field, which is a parse error waiting to happen and reads as unfinished.
# The values are validated per element like a `select`, because they reach a runner that can only
# fail much later and much less clearly — a bad temporal scale corrupts the model's channel layout.
@testset "chipSelect validation" begin
    spec = Cecelia._task_spec(TrainFlowModel())
    scales = only(p for p in spec["params"] if get(p, "key", "") == "temporalScales")
    @test scales["type"] == "chipSelect"
    @test [string(o["value"]) for o in scales["options"]] == ["1", "2", "3", "4", "6", "8", "12", "16"]

    @test validate_params(TrainFlowModel(),
        Dict{String,Any}("temporalScales" => ["1", "2", "8"])) === nothing
    @test_throws ParamValidationError validate_params(TrainFlowModel(),
        Dict{String,Any}("temporalScales" => ["1", "5"]))       # 5 is not offered
    @test_throws ParamValidationError validate_params(TrainFlowModel(),
        Dict{String,Any}("temporalScales" => "1,2,4,8"))        # a string is no longer the shape

    # …and the runner-side parser still takes what the chips produce
    @test parse_temporal_scales(["1", "2", "8"]) == [1, 2, 8]
end

@testset "OME-ZARR metadata reads v2 and v3 alike" begin
    # `read_ome_metadata` feeds ccid.json `meta`, which CLAUDE.md → *Calibration* makes authoritative
    # for every physical number in the app. NGFF 0.5 nests attributes under `ome`; a reader that misses
    # that returns an EMPTY Dict, and the caller then has no PhysicalSize/TimeIncrement at all — which
    # downstream becomes 1.0 rather than an error. So the two formats are asserted to agree, against two
    # committed stores of the same real pixels. See test-data/README.md, docs/todo/ZARR_V3_PLAN.md.
    v2 = fixture_path("ZARRFMT", "0", "ZV2img", "ccidImage.ome.zarr")
    v3 = fixture_path("ZARRFMT", "0", "ZV3img", "ccidImage.ome.zarr")
    if !(have_fixture(v2) && have_fixture(v3))
        @test_skip "zarr format fixtures missing"
    else
        # the series wrapper is found structurally in BOTH formats (v2 `.zattrs`, v3 `zarr.json`)
        @test Cecelia.series_base(v2) == joinpath(v2, "0")
        @test Cecelia.series_base(v3) == joinpath(v3, "0")

        # the one resolver: attributes come back unwrapped regardless of the `ome` nesting
        for p in (v2, v3)
            attrs = ngff_group_attrs(joinpath(p, "0"))
            @test !isnothing(attrs)
            @test haskey(attrs, :multiscales)          # NOT nested under :ome by the time we see it
            ms = ngff_multiscales(joinpath(p, "0"))
            @test !isnothing(ms) && !isempty(ms)
        end
        # a directory with no zarr metadata answers nothing rather than throwing
        @test isnothing(ngff_group_attrs(joinpath(v2, "does-not-exist")))
        # array metadata resolves for both; a GROUP dir must NOT be mistaken for an array (v3 shares
        # the filename `zarr.json` between the two)
        @test !isnothing(zarr_array_meta(joinpath(v3, "0", "0")))
        @test isnothing(zarr_array_meta(joinpath(v3, "0")))

        m2 = read_ome_metadata(v2)
        m3 = read_ome_metadata(v3)
        @test !isempty(m2) && !isempty(m3)
        for k in ("SizeC", "SizeT", "SizeZ")
            @test m2[k] == m3[k]
        end
        @test (m2["SizeC"], m2["SizeT"], m2["SizeZ"]) == (4, 3, 3)

        # Calibration — the whole reason these fixtures are real. Deliberately not 1.0, so a correct
        # read is distinguishable from the "unknown" fallback.
        for k in ("PhysicalSizeX", "PhysicalSizeY", "PhysicalSizeZ", "TimeIncrement")
            @test haskey(m2, k) && haskey(m3, k)
            @test isapprox(m2[k], m3[k]; rtol = 1e-9)
        end
        @test isapprox(m2["PhysicalSizeX"], 0.5964274525755702; rtol = 1e-6)
        @test !isapprox(m2["PhysicalSizeX"], 1.0; atol = 1e-6)    # not the silent fallback
        @test isapprox(m2["PhysicalSizeZ"], 3.0; rtol = 1e-6)
        @test isapprox(m2["TimeIncrement"], 30.0; rtol = 1e-6)
    end
end

@testset "bioformats2raw chunk flags" begin
    # These flags were the bug: `chunkSizeX`/`chunkSizeY` existed in omezarr.json and were read by
    # NOTHING — no tile flag ever reached the CLI, so a user who chose 512 still got bioformats2raw's
    # 1024. One `chunkSize` param now, and it is passed.
    @test Cecelia.bf2raw_chunk_flags("512") == ["--tile-width", "512", "--tile-height", "512"]
    @test Cecelia.bf2raw_chunk_flags(1024)  == ["--tile-width", "1024", "--tile-height", "1024"]

    # "auto" passes NOTHING on purpose: bioformats2raw's own default is 1024 ALREADY CAPPED to the
    # frame, which is exactly the rule we want (one chunk per plane, up to 1024) and needs no source
    # dimensions — which we do not have, since the image is not converted yet.
    @test isempty(Cecelia.bf2raw_chunk_flags("auto"))
    @test isempty(Cecelia.bf2raw_chunk_flags("AUTO"))
    @test isempty(Cecelia.bf2raw_chunk_flags(""))

    # unparseable / absurd falls back to auto rather than raising — same call as the compression
    # flags: a bad value must not fail an hour-long import
    @test isempty(Cecelia.bf2raw_chunk_flags("banana"))
    @test isempty(Cecelia.bf2raw_chunk_flags(0))
    @test isempty(Cecelia.bf2raw_chunk_flags(-8))
    @test isempty(Cecelia.bf2raw_chunk_flags(16))       # below 32: not a sane chunk

    # every option the task spec offers must actually resolve (a spec/handler drift here is silent —
    # the import would just ignore the choice, which is the bug this whole testset exists for)
    spec = JSON3.read(read(joinpath(@__DIR__, "..", "src", "tasks", "importImages", "omezarr.json"), String))
    adv  = only(filter(p -> get(p, :type, "") == "section", collect(spec.params)))
    cs   = only(filter(p -> get(p, :key, "") == "chunkSize", collect(adv.params)))
    vals = [string(get(o, :value, o)) for o in cs.options]
    @test "auto" in vals
    @test string(cs.default) in vals
    for v in vals
        @test v == "auto" ? isempty(Cecelia.bf2raw_chunk_flags(v)) :
                            Cecelia.bf2raw_chunk_flags(v) == ["--tile-width", v, "--tile-height", v]
    end

    # and the tips must not merely restate the label — that is what made these params guesswork
    for p in vcat(collect(spec.params), collect(adv.params))
        get(p, :type, "") == "section" && continue
        tip = String(get(p, :tip, ""))
        @test !isempty(tip)
        @test lowercase(tip) != lowercase(String(get(p, :label, "")))
    end
end

@testset "bioformats2raw format flags" begin
    # The import is the ONLY place the store format is chosen; derived stores inherit it
    # (docs/todo/ZARR_V3_PLAN.md D9).
    ff(args...; kw...) = Cecelia.bf2raw_format_flags(args...; kw...)[1]
    conflicted(args...; kw...) = Cecelia.bf2raw_format_flags(args...; kw...)[2]

    @test isempty(ff("0.4", "auto"))                       # default = the command we always ran
    @test ff("0.5", "auto") == ["--ngff-version", "0.5"]
    @test ff("0.5", "1024") ==
          ["--ngff-version", "0.5", "--shard-width", "1024", "--shard-height", "1024"]

    # Sharding is NGFF 0.5 only, and is dropped for 0.4 rather than raising: they are separate controls
    # and switching the version back must still produce a working import.
    @test isempty(ff("0.4", "1024"))

    # unparseable / absurd falls back to upstream's default rather than raising
    for bad in ("banana", "0", "-8", "16", "")
        @test ff("0.5", bad) == ["--ngff-version", "0.5"]
    end

    # ── chunk-key separator ──────────────────────────────────────────────────────
    # Flat keys are the measured "fewer files" lever: 4 directories vs 224 on one conversion, 56x, with
    # no format change. Nested is bioformats2raw's default and stays ours.
    @test ff("0.4", "auto"; separator = "nested") == String[]
    @test ff("0.4", "auto"; separator = "flat")   == ["--no-nested"]

    # THE CONFLICT: --no-nested + --ngff-version 0.5 makes bioformats2raw silently write zarr v2
    # (verified both flag orders). The two must never be emitted together, and the caller must be told.
    @test ff("0.5", "auto"; separator = "flat") == ["--no-nested"]     # 0.5 dropped, not both
    @test !("--ngff-version" in ff("0.5", "auto"; separator = "flat"))
    @test conflicted("0.5", "auto"; separator = "flat")
    @test !conflicted("0.5", "auto"; separator = "nested")
    @test !conflicted("0.4", "auto"; separator = "flat")
    # a conflicted request is no longer 0.5, so no shard flags ride along with it
    @test ff("0.5", "1024"; separator = "flat", shard_depth = "all", z_planes = 13) == ["--no-nested"]

    # ── shard depth ──────────────────────────────────────────────────────────────
    # The ONLY axis that reduces the file count on a 512x512 frame — width/height cap to the frame, so
    # the shard equals the chunk and packs nothing (measured: depth 13 -> 13 files vs 109).
    @test ff("0.5", "auto"; shard_depth = "13") == ["--ngff-version", "0.5", "--shard-depth", "13"]
    @test ff("0.5", "auto"; shard_depth = "all", z_planes = 13) ==
          ["--ngff-version", "0.5", "--shard-depth", "13"]
    @test ff("0.5", "auto"; shard_depth = "1") == ["--ngff-version", "0.5"]      # the default: no flag
    # "all" with no usable z count drops the flag rather than guessing a depth
    @test ff("0.5", "auto"; shard_depth = "all", z_planes = 0) == ["--ngff-version", "0.5"]
    @test ff("0.5", "auto"; shard_depth = "all", z_planes = 1) == ["--ngff-version", "0.5"]
    # depth is NGFF 0.5 only, like the rest of sharding
    @test isempty(ff("0.4", "auto"; shard_depth = "13"))

    # Every option the spec offers must resolve, and there must be NO option claiming to disable
    # sharding: --shard-width cannot be turned off, so bioformats2raw shards every v3 store (verified
    # against 0.12.1 — a 0.5 import with no shard flag still produces a sharding_indexed codec), and an
    # "off" option would be a lie.
    spec = JSON3.read(read(joinpath(@__DIR__, "..", "src", "tasks", "importImages", "omezarr.json"), String))
    adv  = only(filter(p -> get(p, :type, "") == "section", collect(spec.params)))
    for key in ("ngffVersion", "shardSize", "chunkSeparator", "shardDepth")
        prm  = only(filter(p -> get(p, :key, "") == key, collect(adv.params)))
        vals = [string(get(o, :value, o)) for o in prm.options]
        @test string(prm.default) in vals
        @test !isempty(String(get(prm, :tip, "")))
    end
    shard = only(filter(p -> get(p, :key, "") == "shardSize", collect(adv.params)))
    @test !any(lowercase(string(get(o, :value, o))) in ("none", "off", "0") for o in shard.options)

    # Transparency: someone who knows zarr must be able to map each control onto what lands on disk, so
    # every one of these tips names its bioformats2raw flag or the metadata key it sets.
    for key in ("chunkSize", "ngffVersion", "shardSize", "chunkSeparator", "shardDepth")
        prm = only(filter(p -> get(p, :key, "") == key, collect(adv.params)))
        tip = String(get(prm, :tip, ""))
        @test occursin("--", tip) || occursin("_", tip)   # a CLI flag or a zarr metadata key
    end
end

@testset "OME-TIFF export carries the calibration" begin
    # The task exists because the OLD route (OME-TIFF → ImageJ → plain TIFF → Imaris File Converter)
    # lost the pixel sizes: a plain TIFF has nowhere to record Z spacing, so the converter guessed the
    # voxel size. Every assertion below is about the calibration surviving — that IS the feature.

    meta = Dict{String,Any}("PhysicalSizeX" => 0.325, "PhysicalSizeY" => 0.325,
                            "PhysicalSizeZ" => 2.0,   "PhysicalSizeUnit" => "µm",
                            "TimeIncrement" => 10.0,  "TimeIncrementUnit" => "s")

    cal = Cecelia._export_calibration(meta)
    @test cal["PhysicalSizeZ"] == 2.0                     # the field the old workflow dropped
    @test cal["PhysicalSizeZUnit"] == "µm"

    # UNITS MUST BE THE OME SYMBOL, not the NGFF/UDUNITS name ccid.json stores. OME's UnitsLength and
    # UnitsTime are ENUMERATIONS; "micrometer" is not a member, so one such attribute makes <Pixels>
    # schema-invalid and Bio-Formats discards the ENTIRE OME block and falls back to counting IFDs —
    # a 31x4x32 movie then opens as 3968 timepoints, one channel, no names, no voxel size. Verified
    # against real Bio-Formats (bioformats2raw): "µm" reads back in full, "micrometer" reads nothing.
    ngff = Dict{String,Any}("PhysicalSizeX" => 0.33, "PhysicalSizeY" => 0.33,
                            "PhysicalSizeZ" => 2.0,  "PhysicalSizeUnit" => "micrometer",
                            "TimeIncrement" => 15.0, "TimeIncrementUnit" => "second")
    ncal = Cecelia._export_calibration(ngff)
    for k in ("PhysicalSizeXUnit", "PhysicalSizeYUnit", "PhysicalSizeZUnit")
        @test ncal[k] == "µm"
    end
    @test ncal["TimeIncrementUnit"] == "s"
    # An unknown unit passes through rather than being guessed at — same rule as the converter.
    @test Cecelia._export_calibration(
        Dict{String,Any}("PhysicalSizeX" => 1.0, "PhysicalSizeUnit" => "furlong")
    )["PhysicalSizeXUnit"] == "furlong"
    @test cal["PhysicalSizeX"] == 0.325 && cal["PhysicalSizeXUnit"] == "µm"
    @test cal["TimeIncrement"] == 10.0 && cal["TimeIncrementUnit"] == "s"

    # A Z-MIP has no z extent left and a single frame has no interval — writing either would state a
    # geometry the file doesn't have.
    @test !haskey(Cecelia._export_calibration(meta; z_mip = true), "PhysicalSizeZ")
    @test haskey(Cecelia._export_calibration(meta; z_mip = true), "PhysicalSizeX")
    @test !haskey(Cecelia._export_calibration(meta; one_frame = true), "TimeIncrement")

    # Unknown must stay unknown. Defaulting an absent/zero/garbage size to 1.0 would tell Imaris the
    # pixel is one micron, which is a claim, not a fallback.
    for bad in (Dict{String,Any}(), Dict{String,Any}("PhysicalSizeX" => ""),
                Dict{String,Any}("PhysicalSizeX" => 0.0), Dict{String,Any}("PhysicalSizeX" => "abc"))
        @test !haskey(Cecelia._export_calibration(bad), "PhysicalSizeX")
    end

    # …and that absence is exactly what QC flags, since the write itself always "succeeds".
    codes(f) = [x["code"] for x in f]
    @test isempty(Cecelia._export_qc_findings(cal, 21))     # fully calibrated → nothing to say
    @test isempty(Cecelia._export_qc_findings(cal, 1))
    # A 2D image legitimately has no Z spacing — don't cry wolf on SizeZ == 1.
    @test isempty(Cecelia._export_qc_findings(Cecelia._export_calibration(meta; z_mip = true), 1))
    @test "export.no_z_calibration" in
          codes(Cecelia._export_qc_findings(Cecelia._export_calibration(meta; z_mip = true), 21))
    @test "export.no_xy_calibration" in codes(Cecelia._export_qc_findings(Dict{String,Any}(), 1))

    # channelSelection submits channel NAMES, not indices. Converting them by hand threw
    # `Int("DAPI")` straight out of the task; `channel_indices` is the resolver, and it is 0-based —
    # which is what the runner slices with, so an off-by-one here exports the wrong channel.
    names = ["DAPI", "GFP", "mem-Tom"]
    @test channel_indices(["GFP"], names; what = "channels") == [1]
    @test channel_indices(["mem-Tom", "DAPI"], names; what = "channels") == [2, 0]
    @test channel_indices(nothing, names; what = "channels") == Int[]
    @test channel_indices(String[], names; what = "channels") == Int[]
    # A name this version doesn't have must say so rather than silently pick something.
    @test_throws ErrorException channel_indices(["nope"], names; what = "channels")

    # …and the NAMES must come from `channel_names`, which falls back to the active version. Channel
    # names are typically registered only under `default` while a processed version carries none, so
    # reading the requested version's raw field returns nothing and the task reports "(none
    # registered)" for an image whose channels the picker is listing — the picker is fed by
    # `channel_names(img)`, so any other source disagrees with what the user just clicked.
    proj = create_project!(name = "chan-fallback-$(rand(1000:9999))")
    st   = add_set!(proj; name = "s")
    im   = add_image!(st; name = "chan-fallback")
    set_channel_names!(im, ["DAPI", "SHG"]; value_name = VERSIONED_DEFAULT_VAL, check_length = false)
    save!(im)

    @test channel_names(im) == ["DAPI", "SHG"]
    # An explicit version with no entry of its own still resolves — this is the bug.
    @test channel_names(im; value_name = "corrected") == ["DAPI", "SHG"]
    @test channel_indices(["SHG"],
                          something(channel_names(im; value_name = "corrected"), String[]);
                          what = "channels") == [1]

    # Dispatch + spec wiring
    @test Cecelia._task_from_fun_name("exportImages.ome_tiff") isa ExportOmeTiff
    spec = JSON3.read(read(Cecelia._spec_path(ExportOmeTiff()), String))
    @test String(get(spec, :fun_name, "")) == "exportImages.ome_tiff"
    @test String(get(spec, :resource_pool, "")) == "io"
    # The output is an ARTEFACT, not a version — nothing may register an image version from it.
    @test !any(String(get(p, :key, "")) == "outputValueName" for p in get(spec, :params, []))

    # One filename rule, shared with the movie recorders — an image called "… (cropped)" must not
    # produce a name that ends in a separator (that bug shipped once already).
    @test safe_name_part("A B (cropped)") == "A_B_cropped"
    @test safe_name_part("  ") == ""
    @test safe_name_part(nothing) == ""
end

# ── Canonical-helper detectors ────────────────────────────────────────────────
# These exist because both rules below have now cost real debugging time, and neither was enforced.
# The pattern is the repo's existing one (`no_bare_write_h5ad`, `TextIoDeclaresEncodingTest`, the
# store-compressor/staging conventions): scan the SOURCE, fail on a new bypass.

@testset "channelSelection params resolve through channel_indices" begin
    # A `channelSelection` param submits channel NAMES. `channel_indices` is the one resolver — it
    # takes names OR already-resolved indices, returns 0-based values, and errors by name on a miss
    # (with a case-difference hint). Its own comment records SIX handlers that had hand-rolled
    # `findfirst(==(String(ch)), ch_names)` and drifted into three different wrong behaviours: an
    # index crashed four of them, an unmatched name was silently DROPPED by five, and drift
    # correction silently fell back to channel 0 — registering a whole timelapse against SHG.
    #
    # It happened again on the OME-TIFF export (`Int("SHG")` → MethodError, straight out of the task),
    # which is why this is now a test rather than a comment.
    function _has_channel_param(ps)::Bool
        for p in ps
            p isa AbstractDict || continue
            String(get(p, "type", "")) == "channelSelection" && return true
            inner = get(p, "params", nothing)
            inner isa AbstractVector && _has_channel_param(inner) && return true
        end
        false
    end

    checked = String[]
    for (fun, task) in Cecelia._fun_name_map()
        spec = Cecelia._task_spec(task)
        isnothing(spec) && continue
        _has_channel_param(get(spec, "params", [])) || continue
        spec_path = Cecelia._spec_path(task)
        isnothing(spec_path) && continue
        jl = replace(spec_path, r"\.json$" => ".jl")
        isfile(jl) || continue        # a composite resolves nothing itself; its steps are checked
        push!(checked, fun)
        @test occursin("channel_indices", read(jl, String)) ||
              error("$fun declares a channelSelection param but its handler never calls " *
                    "`channel_indices`. Resolve names with it (0-based, errors by name) rather " *
                    "than converting them by hand — see CLAUDE.md and channel_index's own comment.")
    end
    # The scan must actually find tasks; a rename that silently matched nothing would "pass".
    @test length(checked) >= 8
end

@testset "a process exit check also checks termsignal" begin
    # libuv reports `exitcode = 0` for a SIGNAL-KILLED child, and `task:cancel` kills by design — so
    # `exitcode == 0` alone reads a cancelled or timed-out process as a clean success. That is how a
    # timed-out agent run had its TRUNCATED output handed to the result parser.
    offenders = String[]
    for root in (joinpath(@__DIR__, "..", "src"), joinpath(@__DIR__, "..", "..", "api", "src"))
        isdir(root) || continue
        for (dir, _, files) in walkdir(root), f in files
            endswith(f, ".jl") || continue
            path  = joinpath(dir, f)
            lines = readlines(path)
            for (i, ln) in enumerate(lines)
                occursin(".exitcode", ln) || continue
                # A window, not the same line: the check is often split over two lines, or guarded by
                # a `killed` flag derived from termsignal a few lines above.
                lo, hi = max(1, i - 6), min(length(lines), i + 6)
                any(occursin("termsignal", lines[j]) for j in lo:hi) && continue
                push!(offenders, "$(basename(path)):$i  $(strip(ln))")
            end
        end
    end
    isempty(offenders) && @test true
    isempty(offenders) || error("`.exitcode` used without a nearby `termsignal` check:\n  " *
                                join(offenders, "\n  ") *
                                "\nlibuv sets exitcode 0 for a signal-killed process — check " *
                                "`proc.exitcode == 0 && proc.termsignal == 0`.")
end

@testset "dirPath param validation" begin
    # A destination folder, typed by hand or picked with the FileBrowser. The failure this guards is
    # late and expensive: without it a bad destination is only discovered after the task has read,
    # converted and tried to write the whole output.
    spec = [Dict{String,Any}("key" => "outDir", "label" => "Destination", "type" => "dirPath")]

    # Empty is legal — every consumer falls back to its own default (default_export_dir()).
    Cecelia._validate_params_against_spec(Dict{String,Any}("outDir" => ""), spec)
    Cecelia._validate_params_against_spec(Dict{String,Any}(), spec)

    mktempdir() do dir
        # An existing folder is the normal case.
        Cecelia._validate_params_against_spec(Dict{String,Any}("outDir" => dir), spec)

        # One that does not exist yet is fine too — a destination is created on demand, so rejecting
        # it would stop someone naming a new subfolder, which is the obvious thing to want.
        Cecelia._validate_params_against_spec(
            Dict{String,Any}("outDir" => joinpath(dir, "new_subfolder")), spec)

        # An existing FILE is the one unambiguous mistake: nothing can write output into it.
        f = joinpath(dir, "not_a_dir.txt"); write(f, "x")
        @test_throws Cecelia.ParamValidationError Cecelia._validate_params_against_spec(
            Dict{String,Any}("outDir" => f), spec)
    end

    @test_throws Cecelia.ParamValidationError Cecelia._validate_params_against_spec(
        Dict{String,Any}("outDir" => 42), spec)

    # The export's destination actually uses the type — the point of adding it.
    ospec = JSON3.read(read(Cecelia._spec_path(ExportOmeTiff()), String))
    outdir = only(filter(p -> String(get(p, :key, "")) == "outDir", collect(ospec[:params])))
    @test String(get(outdir, :type, "")) == "dirPath"
end

@testset "units written into OME-XML are schema-valid symbols" begin
    # OME's UnitsLength / UnitsTime are ENUMERATIONS of symbols. A value outside them makes the
    # whole <Pixels> element schema-invalid, and Bio-Formats then discards the ENTIRE OME block and
    # falls back to counting IFDs — a 31x4x32 movie opened as 3968 timepoints, one channel, no
    # names, no voxel size. Verified against real Bio-Formats (bioformats2raw): "µm" round-trips in
    # full, "micrometer" yields nothing.
    #
    # The trap is that "micrometer" is CORRECT in the two places it comes from: NGFF `.zattrs` axes
    # use UDUNITS-2 names, and `ccid.json` mirrors them because the importer reads the unit from the
    # axes. Only the OME-XML boundary needs the symbol — which is what `ome_xml_unit_name` is for.
    valid_length = Set(["Ym","Zm","Em","Pm","Tm","Gm","Mm","km","hm","dam","m","dm","cm","mm",
                        "µm","nm","pm","fm","am","zm","ym","Å","thou","li","in","ft","yd","mi",
                        "ua","ly","pc","pt","pixel","reference frame"])
    valid_time   = Set(["Ys","Zs","Es","Ps","Ts","Gs","Ms","ks","hs","das","s","ds","cs","ms",
                        "µs","ns","ps","fs","as","zs","ys","min","h","d"])
    valid = union(valid_length, valid_time)

    # Every output of the converter is a member — including for inputs already in symbol form.
    for (ngff, sym) in Cecelia._OME_XML_UNIT
        @test sym in valid
        @test Cecelia.ome_xml_unit_name(ngff) == sym
        @test Cecelia.ome_xml_unit_name(sym) in valid    # idempotent: a symbol stays valid
    end
    # The vocabularies the importer actually stores in ccid.json must all convert.
    for ngff in ("micrometer", "nanometer", "millimeter", "second", "minute")
        @test Cecelia.ome_xml_unit_name(ngff) in valid
    end
    # An unknown unit passes through — we do not guess a conversion — so it is the CALLER's job not
    # to invent one, and the scan below is what keeps a caller from skipping the converter entirely.
    @test Cecelia.ome_xml_unit_name("furlong") == "furlong"

    # Anything that ASSIGNS an OME unit attribute must route through the converter. This is the
    # bypass that shipped: the OME-TIFF export copied ccid.json's "micrometer" straight into
    # PhysicalSizeXUnit, while every other writer converted.
    offenders = String[]
    for root in (joinpath(@__DIR__, "..", "src"), joinpath(@__DIR__, "..", "..", "api", "src"))
        isdir(root) || continue
        for (dir, _, files) in walkdir(root), f in files
            endswith(f, ".jl") || continue
            path = joinpath(dir, f); src = read(path, String)
            occursin(r"\"(PhysicalSize[XYZ]Unit|TimeIncrementUnit)\"\s*(=>|\]\s*=)", src) || continue
            occursin("ome_xml_unit_name", src) && continue
            push!(offenders, basename(path))
        end
    end
    isempty(offenders) && @test true
    isempty(offenders) || error("these assign an OME-XML unit attribute without calling " *
                                "`ome_xml_unit_name`:\n  " * join(offenders, "\n  ") *
                                "\nccid.json/NGFF store UDUNITS names ('micrometer'); OME-XML " *
                                "needs the symbol ('µm'), and an invalid one voids the whole block.")
end
