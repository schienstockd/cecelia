# HPC P5 — the PBS Pro / OpenPBS backend (docs/todo/HPC_PLAN.md → D7/D8, P5).
#
# No PBS server runs in CI yet. Pinned here: the script render (D8's resource model as `#PBS` lines),
# the status and exit-reason parsers on PBS's own output, and — through shim `qsub`/`qselect`/`qdel`
# commands that run the job with `bash` on this machine — the backend's generated shell end to end:
# submit, poll, log, exit code and cancel. What the shims cannot show is PBS's own handling of the
# directives; that needs a real server (HPC_PLAN P6).

@testset "HPC PBS: script render" begin
    gpu = HpcClass("gpuq", "lab01", "", "#PBS -l place=excl", "module load cuda/12")
    b = PbsBackend(HpcSite("pbs", "login", 22, "~/c", "me@uni.edu", HpcClass("", "", "", "", ""), gpu))
    s = hpc_render_script(b, HpcJobSpec(; name = "seg one", dir = "/scratch/c/jobs/t1", body = "python run.py",
                                        class = "gpu", cpus = 8, mem_gb = 32, gpus = 1, walltime_min = 90))
    @test startswith(s, "#!/bin/bash\n")
    for d in ("-N cecelia-seg_one", "-o /scratch/c/jobs/t1/job.log", "-j oe",
              "-l select=1:ncpus=8:mem=32gb:ngpus=1", "-l walltime=1:30:00", "-q gpuq", "-A lab01",
              "-M me@uni.edu", "-m ae")
        @test occursin("#PBS $d\n", s)
    end
    @test occursin("#PBS -l place=excl\n", s)
    @test findfirst("#PBS -l place", s)[1] < findfirst("module load", s)[1] < findfirst("python run.py", s)[1]
    s2 = hpc_render_script(PbsBackend(HpcSite("pbs", "h", 22, "~/c", "", HpcClass("", "", "", "", ""),
                                              HpcClass("", "", "", "", ""))),
                           HpcJobSpec(; name = "x", dir = "/d", body = "true", account = "grant7"))
    @test occursin("#PBS -l select=1:ncpus=1:mem=4gb\n", s2) && !occursin("ngpus", s2)
    @test occursin("#PBS -A grant7\n", s2) && !occursin("#PBS -q", s2) && !occursin("#PBS -M", s2)
end

@testset "HPC PBS: status + exit-reason parsing" begin
    b = PbsBackend(HpcSite("pbs", "h", 22, "~/c", "", HpcClass("", "", "", "", ""), HpcClass("", "", "", "", "")))
    st = Cecelia._hpc_parse_status(b, "CECELIA-STATE 12.pbs01 Q\nCECELIA-STATE 13.pbs01 R\n" *
                                      "CECELIA-STATE 14.pbs01 E\nCECELIA-STATE 15.pbs01 H\nnoise\n")
    @test st["12.pbs01"] == JOB_QUEUED && st["15.pbs01"] == JOB_QUEUED
    @test st["13.pbs01"] == JOB_RUNNING && st["14.pbs01"] == JOB_RUNNING
    @test !haskey(st, "16.pbs01")
    @test occursin("qselect -u \"\$u\" -s Q", Cecelia._hpc_status_script(b, ["1"]))
    @test Cecelia.pbs_exit_reason("Job Id: 12.pbs01\n    Exit_status = -29\n") == "Walltime reached"
    @test Cecelia.pbs_exit_reason("    Exit_status = -27\n") == "Out of memory"
    @test Cecelia.pbs_exit_reason("    Exit_status = 271\n") == "Killed by signal 15"
    @test Cecelia.pbs_exit_reason("    comment = job killed: walltime 3720 exceeded limit 3600\n") == "Walltime reached"
    @test Cecelia.pbs_exit_reason("    comment = Job deleted by user\n") == "Cancelled on the cluster"
    @test Cecelia.pbs_exit_reason("    Exit_status = 0\n") == ""
    @test Cecelia.pbs_exit_reason("") == ""
end

# Shims for a PBS-shaped command line on this machine: `qsub` starts the job with bash in the
# background and prints `<pid>.shim`; `qselect -s R` lists the running ones; `qdel` kills one.
function _pbs_shims(dir::AbstractString)
    write(joinpath(dir, "qsub"), """
        #!/bin/sh
        nohup bash "\$1" > job.log 2>&1 < /dev/null &
        echo "\$!.shim"
        """)
    write(joinpath(dir, "qselect"), """
        #!/bin/sh
        state=""; while [ \$# -gt 0 ]; do [ "\$1" = "-s" ] && state="\$2"; shift; done
        [ "\$state" = "R" ] || exit 0
        for p in \$(pgrep -f 'bash job.sh' 2>/dev/null); do echo "\$p.shim"; done
        """)
    write(joinpath(dir, "qdel"), """
        #!/bin/sh
        p="\${1%.shim}"
        k() { for c in \$(pgrep -P "\$1" 2>/dev/null); do k "\$c"; done; kill -TERM "\$1" 2>/dev/null; }
        k "\$p"
        """)
    write(joinpath(dir, "qstat"), "#!/bin/sh\nexit 0\n")
    for f in ("qsub", "qselect", "qdel", "qstat"); chmod(joinpath(dir, f), 0o755); end
    dir
end

@testset "HPC PBS: the backend's shell end to end (shim scheduler)" begin
    if Sys.iswindows() || isnothing(Sys.which("pgrep"))
        @test_skip false
    else
        shims = _pbs_shims(mktempdir())
        withenv("PATH" => shims * ":" * ENV["PATH"]) do
            c = HpcLocalConn()
            b = PbsBackend(HpcSite("pbs", "h", 22, "~/c", "", HpcClass("", "", "", "", ""), HpcClass("", "", "", "", "")))
            root = hpc_remote_root(c, mktempdir())
            dir = root * "/jobs/p1"
            id = hpc_submit(b, c, HpcJobSpec(; name = "p1", dir = dir, body = "echo from-pbs\nexit 5"))
            @test endswith(id, ".shim")
            lines = String[]
            r = hpc_watch_job(b, c, id, dir; on_log = l -> push!(lines, l), poll_s = 0.2)
            @test r.exit == 5 && "from-pbs" in lines
            @test occursin("#PBS -l select=1:ncpus=1:mem=4gb", read(joinpath(dir, "job.sh"), String))

            dir_c = root * "/jobs/p2"
            id_c = hpc_submit(b, c, HpcJobSpec(; name = "p2", dir = dir_c, body = "sleep 300"))
            states = RemoteJobState[]
            t0 = time()
            rc = hpc_watch_job(b, c, id_c, dir_c; on_log = _ -> nothing, poll_s = 0.2,
                               on_state = s -> push!(states, s),
                               cancelled = () -> JOB_RUNNING in states || time() - t0 > 20)
            @test rc.cancelled && JOB_RUNNING in states
            sleep(0.5)
            @test only(hpc_poll(b, c, [Cecelia.HpcPollJob(id_c, dir_c, 0)])).state == JOB_GONE
        end
    end
end
