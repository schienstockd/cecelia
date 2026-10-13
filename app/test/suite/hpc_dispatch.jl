# HPC P3 — dispatching a task's `run_py` to a cluster (docs/todo/HPC_PLAN.md → D3, D5, P3).
#
# The end-to-end testsets run a REAL task (Measure labels on the label-import fixture) through
# `run_task(...; target = TARGET_HPC)`. With no cluster, `_HPC_TRANSPORT_OVERRIDE` points the dispatch
# at this machine (`HpcLocalConn` + `LocalShellBackend`) and a stub env whose python is this one, so the
# whole path runs: env check, code sync, mirror, params rewrite, job, log stream, pull-back, and the
# undeclared-change check. The LAST testset does the same against the SLURM container (verify-hpc.yml),
# after hpc_remote.jl's live testset installed the real env there.
#
# Needs `_label_import_project` (label_import.jl) and `_hpc_fake_pixi!` (hpc_remote.jl).

const _HPC_DISPATCH_E2E = !Sys.iswindows() && Sys.islinux() && Sys.ARCH === :x86_64 && !isempty(Cecelia.rsync_bin())

_hpc_dummy_dispatch(proj, assets; task_id = "t1") =
    Cecelia.HpcDispatch(task_id, HpcLocalConn(), LocalShellBackend(), "/remote/root", proj, assets, "cpu",
                        HpcResources(), "")

@testset "HPC dispatch: run target + request round-trip" begin
    @test run_target("") === TARGET_LOCAL && run_target("local") === TARGET_LOCAL
    @test run_target("HPC") === TARGET_HPC
    @test_throws ArgumentError run_target("cluster")
    @test string(TARGET_HPC) == "hpc" && string(TARGET_LOCAL) == "local"
    req = Cecelia.TaskRequest(; task_id = "t", fun_name = "segment.measureLabels", project_uid = "p",
                              image_uid = "i", target = TARGET_HPC, hpc = Dict{String,Any}("cpus" => 2))
    d = Cecelia.task_request_dict(req)
    @test d["target"] == "hpc" && d["hpc"] == Dict("cpus" => 2)
    back = Cecelia.task_request(Cecelia.JSON3.read(Cecelia.JSON3.write(d), Dict{String,Any}))
    @test back.target === TARGET_HPC && back.hpc["cpus"] == 2
    @test Cecelia.task_request(Dict{String,Any}("taskId" => "t")).target === TARGET_LOCAL   # older peer
end

@testset "HPC dispatch: eligibility + resources" begin
    cp = Cecelia._task_from_fun_name("segment.cellpose")
    ms = Cecelia._task_from_fun_name("segment.measureLabels")
    @test task_runs_on_hpc(cp) && task_runs_on_hpc(ms)
    @test task_runs_on_hpc(Cecelia._task_from_fun_name("segment.cellposeMeasure"))
    @test !task_runs_on_hpc(Cecelia._task_from_fun_name("segment.importLabels"))
    @test Cecelia.hpc_task_class(cp) == "gpu" && Cecelia.hpc_task_class(ms) == "cpu"
    @test hpc_resources(cp) == HpcResources(; cpus = 8, mem_gb = 64, gpus = 1, walltime_min = 240)
    r = hpc_resources(cp, Dict{String,Any}("memGb" => 128, "walltimeMin" => "30"))
    @test r.mem_gb == 128 && r.walltime_min == 30 && r.gpus == 1
    e = try; hpc_resources(ms, Dict{String,Any}("cpus" => 0)); catch x; x; end
    @test e isa ArgumentError && e.msg == "Cluster cpus must be 1–256"
    e = try; hpc_resources(ms, Dict{String,Any}("ram" => 4)); catch x; x; end
    @test e isa ArgumentError && occursin("Unknown cluster resource 'ram'", e.msg)
    @test_throws ArgumentError hpc_resources(ms, Dict{String,Any}("gpus" => "lots"))
end

# Every task offered on the cluster declares the I/O of every `run_py` it makes, and a composite offered
# there is made only of steps that are. A new `run_py` in a ported task without them would otherwise
# refuse at run time, on the cluster, after the user waited for a slot.
@testset "HPC dispatch: ported tasks declare every run_py (ratchet)" begin
    tasks_root = joinpath(dirname(pathof(Cecelia)), "tasks")
    n = 0
    for (dir, _, files) in walkdir(tasks_root), f in files
        endswith(f, ".json") || continue
        spec = try; Cecelia.JSON3.read(read(joinpath(dir, f), String), Dict{String,Any}); catch; continue; end
        spec isa AbstractDict && haskey(spec, "fun_name") || continue
        "hpc" in string.(get(spec, "env", String[])) || continue
        n += 1
        # the whole job block: the run form and `hpc_resources` read it, and neither invents a size
        @test get(spec, "hpc", nothing) isa AbstractDict &&
              all(k -> get(spec["hpc"], k, nothing) isa Integer, ("cpus", "mem_gb", "gpus", "walltime_min")) ||
              error("$(spec["fun_name"]) is on the cluster without a complete \"hpc\" block")
        if haskey(spec, "composite")
            for step in spec["composite"]
                @test task_runs_on_hpc(Cecelia._task_from_fun_name(string(step))) ||
                      error("$(spec["fun_name"]) is on the cluster but its step $step is not")
            end
            continue
        end
        jl = joinpath(dir, splitext(f)[1] * ".jl")
        @test isfile(jl)
        src = read(jl, String)
        calls = 0
        for m in eachmatch(r"run_py\(", src)
            # the call's text: from `run_py(` to its matching paren
            depth, i = 0, m.offset + 5
            while i <= ncodeunits(src)
                c = src[i]
                c == '(' && (depth += 1)
                c == ')' && (depth -= 1; depth == 0 && break)
                i = nextind(src, i)
            end
            call = src[m.offset:i]
            calls += 1
            @test (occursin(r"\breads\s*=", call) && occursin(r"\bwrites\s*=", call)) ||
                  error("$(relpath(jl, tasks_root)): a run_py call declares no reads/writes")
        end
        @test calls >= 1
    end
    @test n >= 5                                                      # anti-vacuity
end

# Unix paths: dispatch from Windows is deferred (HPC_PLAN Q8), and `isabspath("/x")` is false there.
@testset "HPC dispatch: params rewrite + the outside-path refusal" begin
  if Sys.iswindows()
    @test_skip false
  else
    proj, assets = "/data/projects/pA", "/home/u/.cecelia"
    d = _hpc_dummy_dispatch(proj, assets)
    p = (; imPath = "$proj/0/i1/ccidImage.ome.zarr", taskDir = "$proj/1/i1", n = 3, flag = true,
         models = Dict("m" => Dict("model" => "$assets/models/cellposeModels/mine", "matchAs" => "base")),
         files = ["a.zarr", "$proj/1/i1/labels/x.zarr"], name = "CD4/CD8", root = "/",
         stray = "/tmp/elsewhere.tif", sym = :x)
    out, outside = Cecelia.hpc_rewrite_params(d, p)
    @test out["imPath"] == "/remote/root/projects/pA/0/i1/ccidImage.ome.zarr"
    @test out["taskDir"] == "/remote/root/projects/pA/1/i1"
    @test out["models"]["m"]["model"] == "/remote/root/assets/models/cellposeModels/mine"
    @test out["files"] == ["a.zarr", "/remote/root/projects/pA/1/i1/labels/x.zarr"]
    @test out["n"] == 3 && out["flag"] === true && out["name"] == "CD4/CD8" && out["sym"] == "x"
    @test outside == ["/tmp/elsewhere.tif"]                           # "/" alone is not a path
    @test Cecelia.hpc_map_path(d, "/data/projects/pAB/x") === nothing  # a sibling with the same prefix
    @test Cecelia.hpc_map_path(d, "$assets/custom.toml") === nothing   # only models/ is an asset
    @test Cecelia.hpc_map_path(d, proj).rel == ""
    @test Cecelia._hpc_object_dir("0/i1/ccidImage.ome.zarr") == "0/i1"
    @test Cecelia._hpc_object_dir("settings.json") === nothing

    # Inside a dispatched task, a run_py with no declaration refuses rather than running locally.
    log = String[]
    ok = Base.ScopedValues.with(Cecelia.HPC_DISPATCH => d) do
        Cecelia.run_py("tasks/segment/measure_labels_run.py", (; a = 1), mktempdir(); on_log = l -> push!(log, l))
    end
    @test !ok && occursin("declares no reads or writes", only(log))
    @test hpc_task_cpus() === nothing
    @test Base.ScopedValues.with(() -> hpc_task_cpus(), Cecelia.HPC_DISPATCH => d) == 1
  end
end

@testset "HPC dispatch: refusals before anything is queued" begin
    mktempdir() do dir
        img = CciaImage(; uid = "u1", name = "u1", dir = joinpath(dir, "1", "u1"))
        mkpath(img._dir)
        write(state_file(img), "{\"uID\":\"u1\"}")
        e = try
            run_task(Cecelia._task_from_fun_name("segment.importLabels"), img,
                     Dict{String,Any}("labelsPath" => "/x.tif", "outputValueName" => "a"); target = TARGET_HPC)
        catch x; x; end
        @test e isa ArgumentError && e.msg == "segment.importLabels cannot run on the cluster"
        e = try
            run_task(Cecelia._task_from_fun_name("segment.measureLabels"), img, Dict{String,Any}();
                     target = TARGET_HPC, hpc = Dict{String,Any}("gpus" => 99))
        catch x; x; end
        @test e isa ArgumentError && occursin("gpus", e.msg)
        @test isempty(list_tasks())                                    # refused before registering
    end
end

# Run Measure labels on `img` with `target`, returning (result, log lines, the task record's target).
function _hpc_run_measure(img, vn; target = TARGET_HPC, hpc = Dict{String,Any}())
    # Measure labels reports microns, so `run_task` refuses an uncalibrated image; the fixture's OME
    # says 0.5 µm, which an import would have copied into `meta`.
    img.meta["PhysicalSizeX"] = "0.5"; img.meta["PhysicalSizeY"] = "0.5"
    log = String[]
    seen = Ref{Any}(nothing)
    res = run_task(Cecelia._task_from_fun_name("segment.measureLabels"), img,
                   Dict{String,Any}("outputValueName" => vn, "intensityValueName" => "default");
                   target, hpc, on_log = l -> push!(log, l),
                   on_status_change = rec -> (seen[] = (rec.target, rec.pool_name)))
    res, log, seen[]
end

_hpc_table(path) = (lp = label_props(path); (n_obs(lp), sort(collect(lp |> as_df |> df -> df.label))))

@testset "HPC dispatch: a real task end to end (local remote)" begin
    if !_HPC_DISPATCH_E2E
        @test_skip false
    else
        prev_poll = Cecelia.HPC_POLL_S[]
        Cecelia.HPC_POLL_S[] = 0.2
        Cecelia._HPC_TRANSPORT_OVERRIDE[] = (HpcLocalConn(), LocalShellBackend())
        try
            mktempdir() do dir
                img, paths = _label_import_project(dir)
                @test Cecelia._run_task(Cecelia.ImportLabels(), img, Dict{String,Any}(
                    "labelsPath" => paths.basePath, "outputValueName" => "imp"); on_log = _ -> nothing) !== nothing
                h5 = joinpath(img._dir, "labelProps", "imp.h5ad")

                # The reference: the same task, run here.
                res, _ = _hpc_run_measure(img, "imp"; target = TARGET_LOCAL)
                @test res !== nothing
                local_table = _hpc_table(h5)
                rm(h5)

                # No env on the "cluster" yet: refused by name, nothing submitted.
                res, log, _ = _hpc_run_measure(img, "imp")
                @test res === nothing
                @test any(l -> occursin("Install it in Settings → HPC", l), log)
                @test !any(l -> occursin("Submitted job", l), log)

                root = hpc_remote_root(HpcLocalConn(), joinpath(config_dir(), "hpc-local"))
                _hpc_fake_pixi!(root)
                @test hpc_install_env!(LocalShellBackend(), HpcLocalConn(), root; poll_s = 0.2,
                                       on_log = _ -> nothing).ok

                res, log, seen = _hpc_run_measure(img, "imp"; hpc = Dict{String,Any}("cpus" => 2))
                res === nothing && @error "dispatch failed" log = join(log, "\n")
                @test res !== nothing
                @test seen == (TARGET_HPC, "hpc")
                @test any(l -> occursin("[HPC] Submitted job", l), log)
                @test any(l -> occursin("2 CPUs", l), log)
                @test any(l -> occursin(">> done", l), log)                # the runner's own log, streamed
                @test !any(l -> occursin("not copied back", l), log)       # nothing undeclared
                @test !any(l -> startswith(l, "[ERROR]"), log)
                @test _hpc_table(h5) == local_table                        # same answer as here
                @test read_qc(img, "segment.measureLabels", "imp")["metrics"]["nCells"] == 12
                mirror = joinpath(root, "projects", basename(dir))
                @test isdir(joinpath(mirror, "0", img.uid, "ccidImage.ome.zarr"))
                @test isfile(joinpath(mirror, ".cecelia-mirror"))
                @test !isdir(joinpath(mirror, "external"))                  # only what was declared went up
                @test !ispath(h5 * ".hpc-incoming") && !ispath(h5 * ".hpc-old")

                # Rerun: nothing re-uploaded.
                res, log, _ = _hpc_run_measure(img, "imp")
                @test res !== nothing && any(l -> occursin("[HPC] Inputs synced: 0 B sent", l), log)

                # A purged mirror (chunks gone) is re-sent, and the answer is the same.
                rm(joinpath(mirror, "1", img.uid, "labels", "imp.zarr"); recursive = true)
                res, log, _ = _hpc_run_measure(img, "imp")
                @test res !== nothing
                @test any(l -> occursin(r"\[HPC\] Inputs synced: [1-9]", l), log)
                @test _hpc_table(h5) == local_table

                # A file the job writes without declaring it is named, and not copied back.
                stray = joinpath(mirror, "1", img.uid, "stray.txt")
                write(stray, "x")
                @test only(Cecelia.hpc_undeclared_changes(
                    Cecelia.HpcDispatch("t", HpcLocalConn(), LocalShellBackend(), root, dir, config_dir(), "cpu",
                                        HpcResources(), ""),
                    [joinpath(dir, "0", img.uid, "ccidImage.ome.zarr")], [h5])) == "1/$(img.uid)/stray.txt"
                rm(stray)

                # Another machine's mirror of a same-named project is never written over.
                marker = joinpath(mirror, ".cecelia-mirror")
                write(marker, "elsewhere:/other/path\n")
                res, log, _ = _hpc_run_measure(img, "imp")
                @test res === nothing && any(l -> occursin("belongs to elsewhere:/other/path", l), log)
                rm(marker)

            end
        finally
            Cecelia._HPC_TRANSPORT_OVERRIDE[] = nothing
            Cecelia.HPC_POLL_S[] = prev_poll
        end
    end
end

@testset "HPC dispatch: cancel reaches the cluster job (local remote)" begin
    if !_HPC_DISPATCH_E2E
        @test_skip false
    else
        mktempdir() do dir
            proj = joinpath(dir, "proj"); mkpath(joinpath(proj, "1", "u"))
            input = joinpath(proj, "1", "u", "in.txt"); write(input, "x")
            root = hpc_remote_root(HpcLocalConn(), joinpath(dir, "remote"))
            _hpc_fake_pixi!(root)
            @test hpc_install_env!(LocalShellBackend(), HpcLocalConn(), root; poll_s = 0.2, on_log = _ -> nothing).ok
            # The env's "python" becomes a runner that never ends, whatever script it is handed.
            py = Cecelia.hpc_env_python(root, hpc_lock_hash())
            rm(py)
            write(py, "#!/bin/sh\necho started\nexec sleep 300\n"); chmod(py, 0o755)
            d = Cecelia.HpcDispatch("cancel-me", HpcLocalConn(), LocalShellBackend(), root, proj, config_dir(),
                                    "cpu", HpcResources(), "")
            rec = Cecelia._register_task!("cancel-me", "x", "hpc", "u", "", _ -> nothing)
            Cecelia._set_status!(rec, Cecelia.TASK_RUNNING)
            prev = Cecelia.HPC_POLL_S[]; Cecelia.HPC_POLL_S[] = 0.2
            log = String[]
            t = @async Cecelia._run_py_hpc("tasks/segment/measure_labels_run.py", (; a = 1),
                                           joinpath(proj, "1", "u", "tasks"), d;
                                           reads = [input], writes = String[], on_log = l -> push!(log, l))
            try
                deadline = time() + 60
                while !any(l -> l == "started", log) && time() < deadline; sleep(0.1); end
                @test any(l -> l == "started", log)                       # it really was running
                @test cancel_task!("cancel-me")
                @test fetch(t) == false
                @test any(l -> occursin("[HPC] Cancelled job", l), log)
            finally
                Cecelia.HPC_POLL_S[] = prev
                Cecelia._deregister_task!(rec)
            end
        end
    end
end

@testset "HPC dispatch: a real task against a live SLURM cluster" begin
    if !_HPC_LIVE || get(ENV, "CECELIA_HPC_TEST_ENV_INSTALL", "") != "1"
        @info "HPC live dispatch test skipped: needs CECELIA_HPC_TEST_HOST and CECELIA_HPC_TEST_ENV_INSTALL=1 (verify-hpc.yml)"
        @test_skip false
    else
        empty = HpcClass("", "", "", "", "")
        site = HpcSite("slurm", ENV["CECELIA_HPC_TEST_HOST"], parse(Int, get(ENV, "CECELIA_HPC_TEST_PORT", "22")),
                       get(ENV, "CECELIA_HPC_TEST_REMOTE_DIR", "~/cecelia-test"), "", empty, empty)
        ident = HpcIdentity(ENV["CECELIA_HPC_TEST_USER"], ENV["CECELIA_HPC_TEST_KEY"])
        kh = joinpath(mktempdir(), "hpc", "known_hosts")
        @test hpc_test_connection(site, ident; known_hosts = [kh]).ok
        conn = hpc_conn(site, ident; known_hosts = [kh])
        prev_poll = Cecelia.HPC_POLL_S[]
        Cecelia.HPC_POLL_S[] = 2.0
        Cecelia._HPC_TRANSPORT_OVERRIDE[] = (conn, SlurmBackend(site))
        try
            mktempdir() do dir
                img, paths = _label_import_project(dir)
                @test Cecelia._run_task(Cecelia.ImportLabels(), img, Dict{String,Any}(
                    "labelsPath" => paths.basePath, "outputValueName" => "imp"); on_log = _ -> nothing) !== nothing
                h5 = joinpath(img._dir, "labelProps", "imp.h5ad")
                @test _hpc_run_measure(img, "imp"; target = TARGET_LOCAL)[1] !== nothing
                local_table = _hpc_table(h5)
                rm(h5)
                res, log, _ = _hpc_run_measure(img, "imp"; hpc = Dict{String,Any}("cpus" => 1, "memGb" => 2))
                foreach(println, log)
                @test res !== nothing
                @test any(l -> occursin("[HPC] Submitted job", l), log)
                @test !any(l -> occursin("not copied back", l), log)
                @test _hpc_table(h5) == local_table
                res, log, _ = _hpc_run_measure(img, "imp"; hpc = Dict{String,Any}("cpus" => 1, "memGb" => 2))
                @test res !== nothing && any(l -> occursin("[HPC] Inputs synced: 0 B sent", l), log)
            end
        finally
            Cecelia._HPC_TRANSPORT_OVERRIDE[] = nothing
            Cecelia.HPC_POLL_S[] = prev_poll
        end
    end
end
