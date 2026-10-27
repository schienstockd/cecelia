# HPC P5 — the PBS Pro / OpenPBS backend (docs/todo/HPC_PLAN.md → D7/D8, P5).
#
# No PBS server runs in CI yet. Pinned here: the script render (D8's resource model as `#PBS` lines),
# the status and exit-reason parsers on PBS's own output, and — through shim `qsub`/`qstat`/`qdel`
# commands that run the job with `bash` on this machine — the backend's generated shell end to end:
# submit, poll, log, exit code and cancel. What the shims cannot show is PBS's own handling of the
# directives; that needs a real server (HPC_PLAN P6).

@testset "HPC PBS: script render" begin
    gpu = HpcClass("gpuq", "lab01", "", "#PBS -l place=excl", "module load cuda/12")
    b = PbsBackend(HpcSite("pbs", "login", 22, "~/c", "me@uni.edu", HpcClass("", "", "", "", ""), gpu))
    s = hpc_render_script(b, HpcJobSpec(; name = "seg one", dir = "/scratch/c/jobs/t1", body = "python run.py",
                                        class = "gpu", cpus = 8, mem_gb = 32, gpus = 1, walltime_min = 90))
    @test startswith(s, "#!/bin/bash\n")
    for d in ("-N cecelia-seg_one", "-o /scratch/c/jobs/t1/scheduler.log", "-j oe",
              "-l ncpus=8", "-l mem=32gb", "-l ngpus=1", "-l walltime=1:30:00", "-q gpuq", "-A lab01", "-P lab01",
              "-M me@uni.edu", "-m ae")
        @test occursin("#PBS $d\n", s)
    end
    @test occursin("#PBS -l place=excl\n", s)
    @test findfirst("#PBS -l place", s)[1] < findfirst("module load", s)[1] < findfirst("python run.py", s)[1]
    s2 = hpc_render_script(PbsBackend(HpcSite("pbs", "h", 22, "~/c", "", HpcClass("", "", "", "", ""),
                                              HpcClass("", "", "", "", ""))),
                           HpcJobSpec(; name = "x", dir = "/d", body = "true", account = "grant7"))
    # Job-wide resources, never `select`: Gadi refuses `-l select` outright.
    @test occursin("#PBS -l ncpus=1\n#PBS -l mem=4gb\n", s2) && !occursin("ngpus", s2) && !occursin("select", s2)
    # The account goes out as `-P` too: Gadi charges and grants storage by project, ignoring `-A`.
    @test occursin("#PBS -A grant7\n#PBS -P grant7\n", s2) && !occursin("#PBS -q", s2) && !occursin("#PBS -M", s2)

    # A downloading job goes to the internet queue (Gadi's copyq); without one, to the class's queue.
    cpu = HpcClass("normal", "", "", "", "")
    net = HpcSite("pbs", "h", 22, "~/c", "", cpu, cpu, "copyq")
    @test occursin("#PBS -q copyq\n", hpc_render_script(PbsBackend(net),
                   HpcJobSpec(; name = "x", dir = "/d", body = "true", net = true)))
    @test occursin("#PBS -q normal\n", hpc_render_script(PbsBackend(net),
                   HpcJobSpec(; name = "x", dir = "/d", body = "true")))
    @test occursin("#PBS -q normal\n", hpc_render_script(PbsBackend(HpcSite("pbs", "h", 22, "~/c", "", cpu, cpu)),
                   HpcJobSpec(; name = "x", dir = "/d", body = "true", net = true)))
end

@testset "HPC PBS: status + exit-reason parsing" begin
    b = PbsBackend(HpcSite("pbs", "h", 22, "~/c", "", HpcClass("", "", "", "", ""), HpcClass("", "", "", "", "")))
    st = Cecelia._hpc_parse_status(b, "CECELIA-STATE 12.pbs01 Q\nCECELIA-STATE 13.pbs01 R\n" *
                                      "CECELIA-STATE 14.pbs01 E\nCECELIA-STATE 15.pbs01 H\nnoise\n")
    @test st["12.pbs01"] == JOB_QUEUED && st["15.pbs01"] == JOB_QUEUED
    @test st["13.pbs01"] == JOB_RUNNING && st["14.pbs01"] == JOB_RUNNING
    @test !haskey(st, "16.pbs01")
    # one scheduler query per poll, for the ids asked about
    @test occursin("qstat -f '1' '2'", Cecelia._hpc_status_script(b, ["1", "2"])) &&
          !occursin("qselect", Cecelia._hpc_status_script(b, ["1"]))
    @test Cecelia.pbs_exit_reason("Job Id: 12.pbs01\n    Exit_status = -29\n") == "Walltime reached"
    @test Cecelia.pbs_exit_reason("    Exit_status = -27\n") == "Out of memory"
    @test Cecelia.pbs_exit_reason("    Exit_status = 271\n") == "Killed by signal 15"
    @test Cecelia.pbs_exit_reason("    comment = job killed: walltime 3720 exceeded limit 3600\n") == "Walltime reached"
    @test Cecelia.pbs_exit_reason("    comment = Job deleted by user\n") == "Cancelled on the cluster"
    @test Cecelia.pbs_exit_reason("    Exit_status = 0\n") == ""
    @test Cecelia.pbs_exit_reason("") == ""
end

# Shims for a PBS-shaped command line on this machine: `qsub` starts the job with bash in the
# background and prints `<pid>.shim`; `qstat -f <ids>` reports a live one as running and a gone one the
# way Gadi does (stderr, non-zero exit); `qdel` kills one. An id written to `<shims>/queued` reads as
# queued with PBS's comment; a `<shims>/down` file makes the server unreachable.
function _pbs_shims(dir::AbstractString)
    write(joinpath(dir, "qsub"), """
        #!/bin/sh
        nohup bash "\$1" > scheduler.log 2>&1 < /dev/null &
        echo "\$!.shim"
        """)
    write(joinpath(dir, "qselect"), """
        #!/bin/sh
        for p in \$(pgrep -f 'bash job.sh' 2>/dev/null); do echo "\$p.shim"; done
        """)
    write(joinpath(dir, "qdel"), """
        #!/bin/sh
        p="\${1%.shim}"
        k() { for c in \$(pgrep -P "\$1" 2>/dev/null); do k "\$c"; done; kill -TERM "\$1" 2>/dev/null; }
        k "\$p"
        """)
    write(joinpath(dir, "qstat"), """
        #!/bin/sh
        [ "\$1" = "-f" ] || exit 0
        shift
        d="\$(dirname "\$0")"; touch "\$d/queued"
        [ -f "\$d/down" ] && { echo "qstat: cannot connect to server pbs01 (errno=111)" >&2; exit 2; }
        rc=0
        for i in "\$@"; do
          if grep -qx "\$i" "\$d/queued"; then
            printf 'Job Id: %s\\n    Job_Name = cecelia-q\\n    job_state = Q\\n    comment = Not Running: Insufficient amount of resource: ncpus\\n' "\$i"
          elif kill -0 "\${i%.shim}" 2>/dev/null; then
            printf 'Job Id: %s\\n    Job_Name = cecelia-shim\\n    job_state = R\\n' "\$i"
          else
            echo "qstat: \$i Job has finished, use -x or -H to obtain historical job information" >&2; rc=153
          fi
        done
        exit \$rc
        """)
    for f in ("qsub", "qselect", "qdel", "qstat"); chmod(joinpath(dir, f), 0o755); end
    dir
end

@testset "HPC PBS: node shapes (shim qstat -Q/-B + pbsnodes)" begin
    if Sys.iswindows()
        @test_skip false
    else
        shims = mktempdir()
        # gpuq routes to gpuq-exec, whose nodes carry `queue`; cpuq owns none, so it runs on the untagged
        # ones; the server's default queue is cpuq
        write(joinpath(shims, "qstat"), """
            #!/bin/sh
            case "\$*" in
              "-Bf") printf 'Server: pbs\\n    default_queue = cpuq\\n';;
              "-Qf gpuq") printf 'Queue: gpuq\\n    queue_type = Route\\n    route_destinations = gpuq-exec\\n';;
              "-Qf cpuq") printf 'Queue: cpuq\\n    queue_type = Execution\\n';;
              *) echo "qstat: Unknown queue" >&2; exit 1;;
            esac
            """)
        write(joinpath(shims, "pbsnodes"), """
            #!/bin/sh
            for i in 1 2; do
              printf 'gpu-%s\\n     queue = gpuq-exec\\n     resources_available.mem = 402653184kb\\n' \$i
              printf '     resources_available.ncpus = 48\\n     resources_available.ngpus = 4\\n\\n'
            done
            printf 'cpu-1\\n     resources_available.mem = 192gb\\n     resources_available.ncpus = 48\\n\\n'
            printf 'big-1\\n     resources_available.mem = 3tb\\n     resources_available.ncpus = 32\\n\\n'
            printf 'copy-1\\n     queue = copyq-exec\\n     resources_available.ncpus = 48\\n     resources_available.mem = 196608mb\\n'
            """)
        for f in ("qstat", "pbsnodes"); chmod(joinpath(shims, f), 0o755); end
        withenv("PATH" => shims * ":" * ENV["PATH"]) do
            b, c = PbsBackend(HpcSite("pbs", "h", 22, "~/c", "", HpcClass("", "", "", "", ""), HpcClass("", "", "", "", ""))), HpcLocalConn()
            g = hpc_node_shapes(b, c, "gpuq")
            @test !g.shared && g.shapes == [Cecelia.HpcNodeShape(2, 48, 4, 384)]
            d = hpc_node_shapes(b, c, "")                       # the default queue, on the shared pool
            @test d.shared && d.shapes == [Cecelia.HpcNodeShape(1, 48, 0, 192), Cecelia.HpcNodeShape(1, 32, 0, 3072)]
            e = try; hpc_node_shapes(b, c, "nope"); catch x; x; end
            @test e isa HpcError && e.msg == "No queue named nope on the cluster"
        end
    end
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
            @test occursin("#PBS -l ncpus=1\n#PBS -l mem=4gb", read(joinpath(dir, "job.sh"), String))

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

            # A queued job: its wait reason reaches `on_reason`, and once it runs, the log streams from
            # the job's own `job.log` (the shim, like PBS, sends the scheduler's output elsewhere). A failed
            # job's scheduler output is shown at the end.
            dir_q = root * "/jobs/p3"
            id_q = hpc_submit(b, c, HpcJobSpec(; name = "p3", dir = dir_q, body = """
                while [ ! -f go ]; do sleep 0.1; done
                echo live-line
                sleep 1
                echo sched-footer >> scheduler.log
                exit 4"""))
            write(joinpath(shims, "queued"), id_q * "\n")
            p = only(hpc_poll(b, c, [Cecelia.HpcPollJob(id_q, dir_q, 0)]))
            @test p.state == JOB_QUEUED && p.reason == "Insufficient amount of resource: ncpus"
            @test id_q in hpc_active_jobs(b, c)       # a queued Cecelia job blocks a clean-up
            @test id_q in hpc_active_jobs(b, c; name = "cecelia-q")
            @test !(id_q in hpc_active_jobs(b, c; name = "cecelia-install-env"))
            reasons, qlines, qstates = String[], String[], RemoteJobState[]
            release = @async begin
                t = time()
                while isempty(reasons) && time() - t < 20; sleep(0.1); end
                write(joinpath(shims, "queued"), "")
                touch(joinpath(dir_q, "go"))
            end
            rq = hpc_watch_job(b, c, id_q, dir_q; on_log = l -> push!(qlines, l), poll_s = 0.2,
                               queued_poll_s = 0.2, on_reason = r -> push!(reasons, r),
                               on_state = s -> (push!(qstates, s); push!(qlines, "state $s")))
            wait(release)
            @test reasons == ["Insufficient amount of resource: ncpus"]
            @test first(qstates) == JOB_QUEUED && JOB_RUNNING in qstates
            @test rq.exit == 4 && "live-line" in qlines && "[scheduler] sched-footer" in qlines
            @test findfirst(==("state JOB_RUNNING"), qlines) < findfirst(==("live-line"), qlines)   # "Running" before its lines

            # An unreachable server fails the poll rather than reading as "the job is gone"; a gone job
            # beside a live one does not.
            touch(joinpath(shims, "down"))
            @test_throws HpcError hpc_poll(b, c, [Cecelia.HpcPollJob(id_q, dir_q, 0)])
            rm(joinpath(shims, "down"))
            @test only(hpc_poll(b, c, [Cecelia.HpcPollJob(id_q, dir_q, 0)])).state == JOB_GONE
        end
    end
end
