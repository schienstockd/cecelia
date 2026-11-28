# What a run USED, stamped on its run-log entry as `usage` (app/src/run_usage.jl): the accumulator's
# merge rules, the cluster accounting as a usage report, and a real scheduled run whose Python runners
# report their own peak memory through `run_py`.

# A SLURM `sacct -n -P -o JobID,State,Elapsed,TotalCPU,MaxRSS,ReqMem,AllocCPUS,Timelimit` answer and
# Gadi's `qstat -x -f -w` — copied verbatim from the parser tests (hpc_remote.jl, hpc_pbs.jl).
const _RU_SACCT = "4211|COMPLETED|00:02:26|00:40.123||64G|12|08:00:00\n" *
                  "4211.batch|COMPLETED|00:02:26|00:40.120|2814552K||12|\n" *
                  "4211.extern|COMPLETED|00:02:26|00:00:00|1024K||12|\n"
const _RU_QSTAT = """
Job Id: 182343901.gadi-pbs
    resources_used.cpupercent = 31
    resources_used.cput = 00:00:40
    resources_used.gpu_mem = 0
    resources_used.gpu_util = 0
    resources_used.mem = 2814552kb
    resources_used.ncpus = 12
    resources_used.walltime = 00:02:26
    job_state = F
    Resource_List.mem = 68719476736b
    Resource_List.ncpus = 12
    Resource_List.ngpus = 1
    Resource_List.walltime = 08:00:00
    Exit_status = 1
"""

# A backend whose scheduler answers with the sacct text above — the one seam `_hpc_log_usage` reads.
struct _RuSacctBackend <: Cecelia.HpcBackend end
Cecelia.hpc_job_usage(::_RuSacctBackend, ::Cecelia.HpcConn, ::AbstractString) = Cecelia.slurm_usage(_RU_SACCT)
Cecelia.hpc_backend_site(::_RuSacctBackend) =
    HpcSite("slurm", "login.example.org", 22, "~/cecelia", "", HpcClass("", "", "", "", ""), HpcClass("", "", "", "", ""))

# `_RuTwoPyTask` (two Python runs) lives in runtests.jl: the scheduler's workers predate this file's
# world, so a task defined here would not be seen by `_run_task` there.

@testset "Run usage: SLURM and PBS accounting as a usage report" begin
    u = run_usage_from_hpc(Cecelia.slurm_usage(_RU_SACCT), "slurm")
    @test u["source"] == "slurm" && u["runs"] == 1
    @test u["peakRssBytes"] == 2814552 * 1024            # MaxRSS 2814552K, back to bytes exactly
    @test u["reqMemBytes"] == 64 * 1024^3                # ReqMem 64G
    @test u["wallSeconds"] == 146.0                      # Elapsed 00:02:26
    @test u["reqWallSeconds"] == 8 * 3600.0              # Timelimit 08:00:00
    @test u["cpus"] == 12 && u["cpuSeconds"] ≈ 40.123    # TotalCPU 00:40.123
    @test u["final"] === true && !haskey(u, "gpuUtilPct") && !haskey(u, "host")

    p = run_usage_from_hpc(Cecelia.pbs_usage(_RU_QSTAT), "pbs")
    @test p["source"] == "pbs" && p["peakRssBytes"] == 2814552 * 1024 && p["reqMemBytes"] == 68719476736
    @test p["cpuSeconds"] ≈ 40 && p["gpuUtilPct"] == 0.0 && p["reqWallSeconds"] == 28800.0

    # no time limit → no reqWallSeconds (0 means unlimited, not zero seconds)
    v = Cecelia.slurm_usage("7|COMPLETING|01:00:00|2-00:00:00||4Gc|4|UNLIMITED\n7.batch|COMPLETING|01:00:00|||||\n7.0|RUNNING||||||\n" *
                            "7.1|COMPLETED|00:59:00|1-00:00:00|9G|||\n")
    w = run_usage_from_hpc(v, "slurm")
    @test !haskey(w, "reqWallSeconds") && w["final"] === false && w["reqMemBytes"] == 16 * 1024^3
end

@testset "Run usage: the accumulator keeps maxima and sums, and only inside a run" begin
    acc = Dict{String,Any}()
    merge_run_usage!(acc, Dict("source" => "local", "host" => "a", "runs" => 1, "peakRssBytes" => 100))
    merge_run_usage!(acc, Dict("source" => "local", "host" => "a", "runs" => 1, "peakRssBytes" => 300))
    merge_run_usage!(acc, Dict("source" => "local", "host" => "a", "runs" => 1, "peakRssBytes" => 200))
    @test acc == Dict("source" => "local", "host" => "a", "runs" => 3, "peakRssBytes" => 300)

    h = Dict{String,Any}()
    merge_run_usage!(h, Dict("wallSeconds" => 10.0, "cpuSeconds" => 4.0, "reqMemBytes" => 8, "final" => true, "runs" => 1))
    merge_run_usage!(h, Dict("wallSeconds" => 5.0, "cpuSeconds" => 1.0, "reqMemBytes" => 16, "final" => false, "runs" => 1))
    @test h["wallSeconds"] == 15.0 && h["cpuSeconds"] == 5.0 && h["reqMemBytes"] == 16 && h["final"] === false

    # outside a scheduled run nothing is recorded; inside, every report lands in the bound accumulator
    @test !record_run_usage!(Dict("peakRssBytes" => 1))
    r = RunUsage()
    with_run_usage(r) do
        @test record_run_usage!(Dict("runs" => 1, "peakRssBytes" => 5))
        fetch(Threads.@spawn record_run_usage!(Dict("runs" => 1, "peakRssBytes" => 9)))   # a child task
    end
    @test run_usage(r) == Dict("runs" => 2, "peakRssBytes" => 9)

    # the HPC dispatch reports the job's accounting into the run
    d = Cecelia.HpcDispatch("t", HpcLocalConn(), _RuSacctBackend(), "", "", "", "cpu", Cecelia.HpcResources(), "")
    r2 = RunUsage()
    log = String[]
    with_run_usage(() -> Cecelia._hpc_log_usage(d, "4211", l -> push!(log, l)), r2)
    @test run_usage(r2) == run_usage_from_hpc(Cecelia.slurm_usage(_RU_SACCT), "slurm")
    @test first(log) == "[HPC] Resources used:"

    # a runner's exit report: a missing file or a non-positive peak is no report, never a zero
    f = tempname()
    @test isempty(read_local_py_usage(f))
    write(f, """{"peakRssBytes": 0}"""); @test isempty(read_local_py_usage(f))
    write(f, """{"peakRssBytes": 1234}""")
    lu = read_local_py_usage(f)
    @test lu["peakRssBytes"] == 1234 && lu["source"] == "local" && lu["runs"] == 1 && lu["host"] == Libc.gethostname()
    rm(f; force = true)
end

@testset "Run usage: a scheduled run's entry carries its Python peak memory and host" begin
    proj = create_project!(name = "run-usage-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name = "s"); name = "img")
    try
        tid = "ru$(rand(1000:9999))"
        @test run_task(_RuTwoPyTask(), img, Dict{String,Any}("mb" => 120); task_id = tid) !== nothing
        e = only(filter(e -> String(get(e, "taskId", "")) == tid, read_run_log(img)))
        u = e["usage"]
        @test String(u["source"]) == "local" && String(u["host"]) == Libc.gethostname()
        @test u["runs"] == 2
        # each runner held ≥ 120 MiB; two runs are the larger peak, not the sum
        @test 120 * 2^20 <= u["peakRssBytes"] < 240 * 2^20
        @test isempty(filter(endswith(".usage.json"), readdir(Cecelia.task_run_dir(img._dir))))   # read, then removed

        # a task that runs no Python reports nothing, and gets no `usage`
        tid2 = "ru2$(rand(1000:9999))"
        run_task(_RunEnvTask(), img, Dict{String,Any}(); task_id = tid2)
        @test !haskey(only(filter(e -> String(get(e, "taskId", "")) == tid2, read_run_log(img))), "usage")
    finally
        rm(proj.root; recursive = true)
    end
end

# The task tier sizes its parallelism from `RUN_CPU_BUDGET`, which the scheduler binds (the cpu pool's
# headroom) — the task no longer calls `pool_status` / `hpc_task_cpus` above it. The HPC rebinding is
# in suite/hpc_dispatch.jl.
@testset "Run CPU budget: unset outside a run, the cpu pool's headroom inside one" begin
    @test Cecelia.RUN_CPU_BUDGET[] === nothing
    @test Cecelia.run_cpu_threads(; cap = 8) == 1                    # a task body called directly
    Base.ScopedValues.with(Cecelia.RUN_CPU_BUDGET => (; cpus = 12, dedicated = false)) do
        @test Cecelia.run_cpu_threads(; cap = 8) == 8                # a share of this machine: capped
    end
    Base.ScopedValues.with(Cecelia.RUN_CPU_BUDGET => (; cpus = 12, dedicated = true)) do
        @test Cecelia.run_cpu_threads(; cap = 8) == 12               # a cluster job's own: all of it
    end

    proj = create_project!(name = "cpu-budget-$(rand(1000:9999))")
    img  = add_image!(add_set!(proj; name = "s"); name = "img")
    try
        resize_pool!("cpu", 3)
        # no spec ⇒ the cpu pool; its own slot is counted back in, so the headroom is the limit
        r = run_task(_CpuBudgetTask(), img, Dict{String,Any}())
        @test r["budget"] == (; cpus = 3, dedicated = false)
        @test r["threads"] == 3
        resize_pool!("cpu", 9)
        r = run_task(_CpuBudgetTask(), img, Dict{String,Any}())
        @test r["budget"] == (; cpus = 9, dedicated = false) && r["threads"] == 4   # cap = 4
        @test Cecelia._pool_cpu_budget() == (; cpus = 10, dedicated = false)       # idle: limit + 1, as before
    finally
        resize_pool!("cpu", 20)   # restore default so later tests are unaffected
        rm(proj.root; recursive = true)
    end
end

@testset "run_py remote hook: a dispatch with no runner refuses, never runs locally" begin
    log = String[]
    ok = Base.ScopedValues.with(Cecelia.HPC_DISPATCH => _NoRunnerDispatch()) do
        Cecelia.run_py("tasks/segment/measure_labels_run.py", (; a = 1), mktempdir();
                       on_log = l -> push!(log, l), reads = ["/x"], writes = ["/y"])
    end
    @test ok === false
    @test occursin("no runner for a _NoRunnerDispatch dispatch", only(log))
end
