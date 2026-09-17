# ── Python spawning + setup wizard + preview worker testsets ──────────
# Five sections covering the Python subprocess dispatch environment (custom-modules PYTHONPATH
# via config_dir(), OPENBLAS_NUM_THREADS bounds, task-workers widening under BOTH conditions),
# the first-launch config setup wizard (isolated CECELIA_DEV_DIR temp dir, setup_required +
# set_projects_dir! merge + reload), and the resident preview worker PYTHONPATH pin. Extracted
# from suite.jl to keep it small enough to merge without EOF conflicts on every append. The
# extracted file loads inside this file's aggregating testset scope, so any helpers defined
# earlier in suite.jl are still in scope (lexical include).
#
# The one `dirname(dirname(@__DIR__))` repo-root walk (the widening testset reads
# python/cecelia/utils/coastal_utils.py to check the FLOW_WORKERS pin) is rerouted through
# pathof(Cecelia) so it resolves identically whether the file sits at app/test/ or
# app/test/suite/.

_repo = dirname(dirname(dirname(pathof(Cecelia))))

@testset "run_py custom-modules PYTHONPATH (config_dir not shadowed)" begin
    # Regression: run_py's task-dir parameter was named `config_dir`, which shadowed the
    # config_dir() function, so the custom-modules PYTHONPATH line `joinpath(config_dir(), …)`
    # called the task-dir STRING as a function → every Python task died with
    # `MethodError(<task dir>, (), …)` before Python was ever spawned. The call now lives in a
    # standalone helper with no shadowing param in scope. This asserts it resolves via the real
    # config_dir() function (equality would fail if it ever called anything else).
    # Post-#332 the custom-modules python dir IS the modules ROOT (runners are co-located under
    # modules/<cat>/, launched by absolute path so their own dir is sys.path[0]; the root just makes
    # the wider tree importable) — not the old shared modules/python. See py_runner.jl:_custom_modules_pydirs.
    mods = joinpath(config_dir(), "modules")
    mkpath(mods)
    @test first(Cecelia._custom_modules_pydirs()) == mods
    @test endswith(first(Cecelia._custom_modules_pydirs()), "modules")

    # …and every installed plugin's `python/` dir is listed alongside it (PLUGINS_PLAN R2). Via the
    # modules root alone a plugin's shared code spells `plugins.<plugin>.python.<mod>`, and a plugin
    # directory name is not required to be a Python identifier — the hyphen here is exactly the case
    # that can never be imported that way. Naming `python/` directly makes it a plain `import <mod>`,
    # so the plugin's directory name never appears in an import at all.
    pdir = joinpath(mods, Cecelia.PLUGINS_SUBDIR, "trackimport-smithlab")
    mkpath(joinpath(pdir, "python"))
    try
        @test joinpath(pdir, "python") ∈ Cecelia._custom_modules_pydirs()
        @test pdir ∉ Cecelia._custom_modules_pydirs()   # the ROOT is not added — see the helper's note
        # a plugin with no shared code contributes no entry
        mkpath(joinpath(mods, Cecelia.PLUGINS_SUBDIR, "nopython"))
        @test !any(p -> occursin("nopython", p), Cecelia._custom_modules_pydirs())
    finally
        rm(joinpath(mods, Cecelia.PLUGINS_SUBDIR); recursive = true, force = true)
    end
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

    # A task's OWN thread pools (coastal maps z-planes over one) are a separate budget from BLAS,
    # and derived from the box rather than a constant — the number it replaced was picked on one
    # 32-core laptop, which oversubscribes a small machine and idles a large one.
    @test env["CECELIA_TASK_WORKERS"] == string(Cecelia.task_worker_threads())
    # …and the same number in joblib's spelling. coastal's flow stage is `Parallel(n_jobs=-1)` —
    # processes, not threads, so nothing above bounds it — and `joblib.cpu_count()` honours this.
    # Asserted EQUAL to the task budget: two numbers for "how wide may one task go" would drift.
    @test env["LOKY_MAX_CPU_COUNT"] == env["CECELIA_TASK_WORKERS"]

    # The escape hatch for a stage MEASURED to keep scaling with width — coastal's flow metrics,
    # 14x at 32 threads against 6.8x at 8. Two variables, and both halves matter:
    #
    #   * WIDEN is the AND of the user's flag and "the budget was derived". Computed HERE rather than
    #     in Python, so the config decision has one home; a Python-side `or` would let a stage widen
    #     past a thread count somebody typed, making the slider a suggestion.
    #   * USABLE_CPUS is `usable_cpus()` — the affinity mask and cgroup quota applied. Passed in
    #     because re-deriving it from `os.cpu_count()` would hand out threads for CPUs this process
    #     cannot touch, which is the entire reason that helper exists.
    @test haskey(env, "CECELIA_TASK_WORKERS_WIDEN")
    @test env["CECELIA_TASK_WORKERS_WIDEN"] in ("0", "1")
    @test env["CECELIA_TASK_WORKERS_WIDEN"] ==
          ((Cecelia.task_workers_widen() && Cecelia.task_workers_derived()) ? "1" : "0")
    @test env["CECELIA_USABLE_CPUS"] == string(Cecelia.usable_cpus())
    @test parse(Int, env["CECELIA_USABLE_CPUS"]) >= 1
    # Never wider than the box: the widening target is a real ceiling, not "unbounded".
    @test parse(Int, env["CECELIA_USABLE_CPUS"]) <= max(Sys.CPU_THREADS, 1)

    @test Cecelia.task_worker_threads() >= 1
    @test Cecelia.default_task_worker_threads() <= 16
    # never wider than the machine, however many tasks are assumed concurrent
    @test Cecelia.default_task_worker_threads() <= max(Sys.CPU_THREADS, 1)

    # The preview worker runs the tasks' OWN compute, so it inherits the same budget.
    prev = read(joinpath(Cecelia._app_dir(), "src", "preview.jl"), String)
    @test occursin("OPENBLAS_NUM_THREADS", prev)
end

@testset "widening a linear stage needs BOTH conditions" begin
    # `task_workers_widen()` alone is not enough, and neither is a derived budget alone. The AND is
    # computed in Julia (see `_py_task_env`) precisely so there is one place that can be read.
    #
    # No config is WRITTEN here — `set_task_workers_widen!` touches the user's own custom.toml, and a
    # test must not. The predicates are pure over `cecelia_conf()`, so they are exercised directly.
    @test Cecelia.task_workers_widen() isa Bool
    @test Cecelia.task_workers_derived() isa Bool

    # `task_workers_derived()` must agree with the number actually in effect, or the UI offers a
    # control the backend ignores. A typo counts as DERIVED, matching `task_worker_threads`'s own
    # fallback — a bad value there does not stop a run, it falls back to the machine.
    conf = get(get(Cecelia.cecelia_conf(), "tasks", Dict{String,Any}()), "workerThreads", nothing)
    expected = isnothing(conf) || (try Int(conf) < 1 catch; true end)
    @test Cecelia.task_workers_derived() == expected
    if Cecelia.task_workers_derived()
        @test Cecelia.task_worker_threads() == Cecelia.default_task_worker_threads()
    end

    # Only ONE call site claims linear scaling, and it is the measured one. `PREDICT_WORKERS` must
    # never claim it — its curve turns DOWN past 4, the opposite claim, already served by `cap=`.
    src = read(joinpath(_repo, "python", "cecelia", "utils",
                        "coastal_utils.py"), String)
    @test occursin("FLOW_WORKERS = cpu_utils.task_workers(scales_linearly=True)", src)
    predict = src[findfirst("PREDICT_WORKERS =", src)[1]:end]
    predict = predict[1:findfirst("\n", predict)[1]]
    @test !occursin("scales_linearly", predict)
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

# ── Resident preview worker resolves `cecelia` from THIS checkout ───────────
# The worker is launched by PATH but imports `cecelia` by NAME, so without an explicit PYTHONPATH
# it uses whatever pip has installed — in dev an editable install pointing at the MAIN checkout.
# A worktree would then run its own `preview_worker.py` against another checkout's library and the
# halves drift with no error until one calls something the other lacks. `run_py` always set
# PYTHONPATH; the resident preview worker's `launch!` does too. (The napari bridge used to be
# subject to the same rule; retired in P9.)
@testset "resident preview worker pins PYTHONPATH" begin
    pyroot = Cecelia._python_dir()
    @test isdir(joinpath(pyroot, "cecelia"))              # the dir we are pinning really is the package

    # the worker's launch is inside `launch!` (which spawns), so assert on the source rather than run it
    src = read(joinpath(dirname(pathof(Cecelia)), "preview.jl"), String)
    body = src[findfirst("function launch!(", src)[1]:end]
    @test occursin("PYTHONPATH", body[1:findfirst("\nend", body)[1]])
end
