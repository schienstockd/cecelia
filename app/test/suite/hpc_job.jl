# HPC P4 — what outlives a process: the Task Manager's view of a cluster run, reattach by rerun, and a
# cancel retried until it lands (docs/todo/HPC_PLAN.md → Q4, Q5, Q6).
#
# Runs on the local "remote" like hpc_dispatch.jl, whose helpers (`_hpc_run_measure`, `_hpc_table`) and
# whose fixtures (`_label_import_project`, `_hpc_fake_pixi!`) it reuses.

# A backend whose first `n` cancels fail as an unreachable cluster would.
mutable struct _FlakyCancel <: HpcBackend
    inner::LocalShellBackend
    fails::Int
end
Cecelia.hpc_render_script(b::_FlakyCancel, spec::HpcJobSpec) = hpc_render_script(b.inner, spec)
Cecelia.hpc_submit(b::_FlakyCancel, c::HpcConn, spec::HpcJobSpec) = hpc_submit(b.inner, c, spec)
Cecelia._hpc_status_script(b::_FlakyCancel, ids) = Cecelia._hpc_status_script(b.inner, ids)
Cecelia._hpc_parse_status(b::_FlakyCancel, out) = Cecelia._hpc_parse_status(b.inner, out)
Cecelia.hpc_exit_reason(b::_FlakyCancel, c::HpcConn, id::AbstractString) = ""
function Cecelia.hpc_cancel(b::_FlakyCancel, c::HpcConn, id::AbstractString)
    b.fails > 0 && (b.fails -= 1; throw(HpcError("Cancel: No response from host")))
    hpc_cancel(b.inner, c, id)
end

@testset "HPC job: a cancel that cannot reach the cluster is retried" begin
    if !_HPC_DISPATCH_E2E
        @test_skip false
    else
        prev = Cecelia.HPC_CANCEL_RETRY_S[]
        Cecelia.HPC_CANCEL_RETRY_S[] = 0.2
        try
            c, b = HpcLocalConn(), _FlakyCancel(LocalShellBackend(), 2)
            root = hpc_remote_root(c, mktempdir())
            dir = root * "/jobs/flaky"
            id = hpc_submit(b, c, HpcJobSpec(; name = "flaky", dir = dir, body = "sleep 300"))
            log = String[]
            t0 = time()
            r = hpc_watch_job(b, c, id, dir; on_log = l -> push!(log, l), poll_s = 0.2,
                              cancelled = () -> time() - t0 > 0.5)
            @test r.cancelled
            @test any(l -> occursin("Could not cancel job $id on the cluster yet", l), log)
            @test id in hpc_pending_cancels()
            deadline = time() + 20
            while !isempty(hpc_pending_cancels()) && time() < deadline; sleep(0.1); end
            @test isempty(hpc_pending_cancels())                       # landed on a later try
            @test b.fails == 0
            @test only(hpc_poll(b, c, [Cecelia.HpcPollJob(id, dir, 0)])).state == JOB_GONE
        finally
            Cecelia.HPC_CANCEL_RETRY_S[] = prev
        end
    end
end

@testset "HPC job: pending cluster cancels survive a restart" begin
    if Sys.iswindows()
        @test_skip false
    else
        _with_temp_config() do cfg
            key = joinpath(cfg, "id_test"); write(key, "not a real key")
            empty = HpcClass("", "", "", "", "")
            site = HpcSite("slurm", "login.example.org", 22, "~/c", "", empty, empty)
            ident = HpcIdentity("alice", key)
            prev = Cecelia.HPC_CANCEL_RETRY_S[]
            Cecelia.HPC_CANCEL_RETRY_S[] = 3600.0                      # never retried during the test
            try
                c = hpc_conn(site, ident; known_hosts = [joinpath(cfg, "kh")])
                hpc_queue_cancel!(SlurmBackend(site), c, "777")
                saved = Cecelia.JSON3.read(read(Cecelia._hpc_pending_cancels_path(), String))
                @test only(saved)["id"] == "777" && only(saved)["host"] == "login.example.org"
                # A "restart": the in-memory list is gone, the file is not.
                lock(() -> empty!(Cecelia._HPC_PENDING_CANCELS), Cecelia._HPC_PENDING_LOCK)
                Cecelia._HPC_PENDING_LOADED[] = false
                Cecelia.hpc_load_pending_cancels!(HpcSite("slurm", "other.example.org", 22, "~/c", "", empty, empty), ident)
                @test isempty(hpc_pending_cancels())                   # another cluster: not ours to retry
                Cecelia._HPC_PENDING_LOADED[] = false
                Cecelia.hpc_load_pending_cancels!(site, ident)
                @test hpc_pending_cancels() == ["777"]
            finally
                lock(() -> empty!(Cecelia._HPC_PENDING_CANCELS), Cecelia._HPC_PENDING_LOCK)
                Cecelia._hpc_persist_pending_cancels()
                Cecelia._HPC_PENDING_LOADED[] = false
                Cecelia.HPC_CANCEL_RETRY_S[] = prev
            end
            @test !isfile(Cecelia._hpc_pending_cancels_path())
        end
    end
end

@testset "HPC job: the run fingerprint" begin
    if Sys.iswindows()
        @test_skip false
    else
        mktempdir() do dir
            f = joinpath(dir, "in.txt"); write(f, "a")
            d = _hpc_dummy_dispatch(dir, dir)
            fp() = Cecelia.hpc_run_fingerprint(d, "tasks/x_run.py", Dict("a" => 1), "c0de", [f], [joinpath(dir, "o")])
            a = fp()
            @test a == fp()                                            # stable
            @test a != Cecelia.hpc_run_fingerprint(d, "tasks/x_run.py", Dict("a" => 2), "c0de", [f], [joinpath(dir, "o")])
            @test a != Cecelia.hpc_run_fingerprint(d, "tasks/x_run.py", Dict("a" => 1), "beef", [f], [joinpath(dir, "o")])
            write(f, "ab")                                             # an input changed on disk
            @test a != fp()
            rec = joinpath(dir, "x_run.hpc.json")
            Cecelia.hpc_write_job_record(rec, "fp1", "42", "/r/jobs/j")
            @test Cecelia.hpc_read_job_record(rec, "fp1") == (; id = "42", dir = "/r/jobs/j")
            @test Cecelia.hpc_read_job_record(rec, "fp2") === nothing
        end
    end
end

@testset "HPC job: stages, and reattach by rerun after a crash (local remote)" begin
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
                @test _hpc_run_measure(img, "imp"; target = TARGET_LOCAL)[1] !== nothing
                local_table = _hpc_table(h5)

                root = hpc_remote_root(HpcLocalConn(), joinpath(config_dir(), "hpc-local"))
                _hpc_fake_pixi!(root)
                @test hpc_install_env!(LocalShellBackend(), HpcLocalConn(), root; poll_s = 0.2, on_log = _ -> nothing).ok

                # The stages a cluster run walks through, as the Task Manager receives them.
                stages = Tuple{String,String}[]
                res, log, _ = _hpc_run_measure(img, "imp"; stages)
                @test res !== nothing
                names = first.(stages)
                @test names[1] == "sync" && "running" in names && "pull" in names && names[end] == ""
                job = only(unique(filter(!isempty, last.(stages))))
                @test any(l -> occursin("Submitted job $job", l), log)
                @test isempty(list_tasks())                            # the record went with the run

                # A slow env, so the job is still out when the backend "crashes".
                py = Cecelia.hpc_env_python(root, hpc_lock_hash())
                real = readlink(py)
                rm(py)
                write(py, "#!/bin/sh\nsleep 3\nexec '$real' \"\$@\"\n"); chmod(py, 0o755)
                rm(h5)
                rec = Cecelia._register_task!("crash-1", "segment.measureLabels", "hpc", img.uid, "", _ -> nothing)
                d = Cecelia.HpcDispatch("crash-1", HpcLocalConn(), LocalShellBackend(), root, dir, config_dir(),
                                        "cpu", HpcResources(; cpus = 1), "")
                params = Dict{String,Any}("outputValueName" => "imp", "intensityValueName" => "default")
                log1 = String[]
                t = @async Base.ScopedValues.with(Cecelia.HPC_DISPATCH => d) do
                    Cecelia._run_task(Cecelia.MeasureLabels(), img, params; on_log = l -> push!(log1, l))
                end
                deadline = time() + 60
                while !any(l -> occursin("Submitted job", l), log1) && time() < deadline; sleep(0.05); end
                @test any(l -> occursin("Submitted job", l), log1)
                schedule(t, InterruptException(); error = true)        # the backend dies here
                try; wait(t); catch; end
                Cecelia._deregister_task!(rec)
                @test !isfile(h5)
                records = filter(f -> endswith(f, ".hpc.json"), readdir(joinpath(img._dir, "tasks")))
                @test length(records) == 1                             # the job's record is left behind

                # Rerun the same thing: it follows (or collects) that job, and submits nothing.
                log2 = String[]
                rec2 = Cecelia._register_task!("crash-2", "segment.measureLabels", "hpc", img.uid, "", _ -> nothing)
                d2 = Cecelia.HpcDispatch("crash-2", HpcLocalConn(), LocalShellBackend(), root, dir, config_dir(),
                                         "cpu", HpcResources(; cpus = 1), "")
                res2 = Base.ScopedValues.with(Cecelia.HPC_DISPATCH => d2) do
                    Cecelia._run_task(Cecelia.MeasureLabels(), img, params; on_log = l -> push!(log2, l))
                end
                Cecelia._deregister_task!(rec2)
                res2 === nothing && @error "reattach failed" log = join(log2, "\n")
                @test res2 !== nothing
                @test any(l -> occursin("Re-attached to job", l) || occursin("had already finished", l), log2)
                @test !any(l -> occursin("Submitted job", l), log2)
                @test _hpc_table(h5) == local_table
                @test isempty(filter(f -> endswith(f, ".hpc.json"), readdir(joinpath(img._dir, "tasks"))))
                rm(py); symlink(real, py)
            end
        finally
            Cecelia._HPC_TRANSPORT_OVERRIDE[] = nothing
            Cecelia.HPC_POLL_S[] = prev_poll
        end
    end
end
