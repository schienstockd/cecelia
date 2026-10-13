# HPC P2 — the connection layer, the scheduler backends and the code + env on the cluster
# (docs/todo/HPC_PLAN.md → D7, Q1, Q7, P2).
#
# Most of it runs with no cluster: `HpcLocalConn` + `LocalShellBackend` make this machine the
# "remote", so submit, poll, cancel, the code sync and the env install run end-to-end here (Linux and
# macOS; Windows has no `sh`). The renderers and parsers are pure and run everywhere.
#
# The LAST testset runs against the SLURM container in CI (`verify-hpc.yml`), like the live
# connection test in hpc.jl, whose `_HPC_LIVE` it reuses. Its real `pixi install` runs only when
# `CECELIA_HPC_TEST_ENV_INSTALL=1` too: it downloads the whole env.

const _HPC_SH = !Sys.iswindows() && !isempty(Cecelia.rsync_bin())

_hpc_test_site(; cpu = HpcClass("", "", "", "", ""), gpu = HpcClass("", "", "", "", ""), email = "") =
    HpcSite("slurm", "login.example.org", 22, "~/cecelia", email, cpu, gpu)

@testset "HPC: SLURM script render" begin
    gpu = HpcClass("gpu-a100", "lab01", "normal", "#SBATCH --constraint=a100", "module load cuda/12")
    b = SlurmBackend(_hpc_test_site(; gpu = gpu, email = "me@uni.edu"))
    spec = HpcJobSpec(; name = "seg one", dir = "/scratch/c/jobs/t1", body = "python run.py",
                      class = "gpu", cpus = 8, mem_gb = 32, gpus = 1, walltime_min = 90)
    s = hpc_render_script(b, spec)
    @test startswith(s, "#!/bin/bash\n")
    for d in ("--job-name=cecelia-seg_one", "--output=job.log", "--cpus-per-task=8", "--mem=32G",
              "--time=1:30:00", "--partition=gpu-a100", "--account=lab01", "--qos=normal",
              "--gres=gpu:1", "--mail-user=me@uni.edu", "--mail-type=END,FAIL")
        @test occursin("#SBATCH $d\n", s)
    end
    @test occursin("#SBATCH --constraint=a100\n", s)              # extra directives verbatim
    # Directives first (sbatch stops reading them at the first command), then the preamble, the body.
    @test findfirst("#SBATCH --constraint", s)[1] < findfirst("module load cuda/12", s)[1] <
          findfirst("python run.py", s)[1] < findfirst(".exit.tmp", s)[1]
    @test occursin("cd '/scratch/c/jobs/t1' || exit 99", s)

    # The project's account overrides the class's; an empty class adds no flag at all.
    s2 = hpc_render_script(b, HpcJobSpec(; name = "x", dir = "/d", body = "true", class = "gpu", account = "grant7"))
    @test occursin("--account=grant7", s2) && !occursin("--account=lab01", s2)
    s3 = hpc_render_script(SlurmBackend(_hpc_test_site()), HpcJobSpec(; name = "x", dir = "/d", body = "true"))
    @test !occursin("--partition", s3) && !occursin("--account", s3) && !occursin("--qos", s3) &&
          !occursin("--gres", s3) && !occursin("--mail", s3)
    @test occursin("--time=1:00:00", s3) && occursin("--mem=4G", s3)
end

@testset "HPC: SLURM status + exit-reason parsing" begin
    b = SlurmBackend(_hpc_test_site())
    out = "noise\nCECELIA-STATE 11 PENDING\nCECELIA-STATE 12 RUNNING\nCECELIA-STATE 13 COMPLETING\n" *
          "CECELIA-STATE 14 CONFIGURING\nCECELIA-STATE 15 COMPLETED\n"
    st = Cecelia._hpc_parse_status(b, out)
    @test st["11"] == JOB_QUEUED && st["14"] == JOB_QUEUED
    @test st["12"] == JOB_RUNNING && st["13"] == JOB_RUNNING
    @test st["15"] == JOB_GONE && !haskey(st, "99")
    @test Cecelia.slurm_exit_reason("TIMEOUT|0:0\nJobState=TIMEOUT") == "Walltime reached"
    @test Cecelia.slurm_exit_reason("OUT_OF_MEMORY|0:125") == "Out of memory"
    @test Cecelia.slurm_exit_reason("JobState=NODE_FAIL") == "Node failed"
    @test Cecelia.slurm_exit_reason("CANCELLED by 1000|0:0") == "Cancelled on the cluster"
    @test Cecelia.slurm_exit_reason("FAILED|1:0") == "Job failed"
    @test Cecelia.slurm_exit_reason("COMPLETED|0:0") == ""
    @test Cecelia.slurm_exit_reason("") == ""
end

@testset "HPC: poll reply parsing" begin
    jobs = [Cecelia.HpcPollJob("1", "/a", 0), Cecelia.HpcPollJob("2", "/b", 10)]
    log1 = "héllo\n[PROGRESS] 1/2\nhalf a li"                    # multi-byte, and a line still being written
    out = "CECELIA-STATE 1 RUNNING\n" *
          "CECELIA-EXIT 1 \nCECELIA-LOG 1 $(ncodeunits(log1))\n$(log1)\n" *
          "CECELIA-EXIT 2 3\nCECELIA-LOG 2 0\n\n"
    r = Cecelia._hpc_parse_poll(out, jobs, Dict("1" => JOB_RUNNING))
    @test r[1].state == JOB_RUNNING && r[1].exit === nothing
    @test r[1].log == "héllo\n[PROGRESS] 1/2\n"                   # whole lines only
    @test r[1].log_offset == ncodeunits("héllo\n[PROGRESS] 1/2\n")
    @test r[2].state == JOB_GONE && r[2].exit == 3 && r[2].log == "" && r[2].log_offset == 10
end

@testset "HPC: connection refusals name the setting" begin
    blank = HpcIdentity("", "")
    @test_throws HpcError hpc_conn(_hpc_test_site(), blank)
    e = try; hpc_conn(HpcSite("slurm", "", 22, "~/c", "", HpcClass("", "", "", "", ""), HpcClass("", "", "", "", "")), blank); catch x; x; end
    @test e isa HpcError && occursin(Sys.iswindows() ? "not available on Windows" : "host", e.msg)
    @test hpc_backend(HpcSite("pbs", "h", 22, "~/c", "", HpcClass("", "", "", "", ""), HpcClass("", "", "", "", ""))) isa PbsBackend
    e = try; hpc_backend(HpcSite("lsf", "h", 22, "~/c", "", HpcClass("", "", "", "", ""), HpcClass("", "", "", "", ""))); catch x; x; end
    @test e isa HpcError && occursin("not supported yet", e.msg)
    # rsync's -e is split by rsync: single quotes, doubled to escape.
    @test Cecelia._rsync_e_word("UserKnownHostsFile=\"/a b\"") == "'UserKnownHostsFile=\"/a b\"'"
    @test Cecelia._rsync_e_word("it's") == "'it''s'"
    @test Cecelia._rsync_stats("Number of regular files transferred: 1,234\nLiteral data: 5,678 bytes\n") ==
          Cecelia.HpcRsyncStats(1234, 5678)
    # A multiplexed call never becomes a master itself.
    a = Cecelia.hpc_ssh_args(_hpc_test_site(), HpcIdentity("u", "/k"); known_hosts = ["/kh"], control_path = "/cm-%C")
    @test "ControlMaster=no" in a && "ControlPath=/cm-%C" in a && last(a) == "login.example.org"
end

# A throwaway "repo" with the files the code sync and the env install read.
function _hpc_fake_repo()
    r = mktempdir()
    mkpath(joinpath(r, "python", "cecelia", "tests")); mkpath(joinpath(r, "python", "cecelia", "__pycache__"))
    mkpath(joinpath(r, "app", "src", "tasks", "seg"))
    write(joinpath(r, "python", "pyproject.toml"), "[project]\nname='cecelia'\n")
    write(joinpath(r, "python", "cecelia", "__init__.py"), "")
    write(joinpath(r, "python", "cecelia", "tests", "test_x.py"), "")
    write(joinpath(r, "python", "cecelia", "__pycache__", "x.pyc"), "")
    write(joinpath(r, "app", "src", "tasks", "seg", "seg_run.py"), "print('hi')\n")
    write(joinpath(r, "app", "src", "tasks", "seg", "seg.jl"), "# not synced\n")
    write(joinpath(r, "app", "src", "tasks", "seg", "cell_config.json"), "{}")
    write(joinpath(r, "pixi.toml"), "[workspace]\n"); write(joinpath(r, "pixi.lock"), "version: 6\n")
    r
end

@testset "HPC: code sync (local remote)" begin
    if !_HPC_SH
        @test_skip false
    else
        repo = _hpc_fake_repo()
        files = Cecelia.hpc_code_files(; root = repo)
        @test sort(files) == ["app/src/tasks/seg/cell_config.json", "app/src/tasks/seg/seg_run.py",
                              "python/cecelia/__init__.py", "python/pyproject.toml"]
        c = HpcLocalConn()
        root = hpc_remote_root(c, mktempdir())
        s1 = hpc_sync_code!(c, root; root = repo)
        @test s1.synced && isfile(joinpath(s1.dir, "app", "src", "tasks", "seg", "seg_run.py"))
        @test !isdir(joinpath(s1.dir, "python", "cecelia", "tests"))
        s2 = hpc_sync_code!(c, root; root = repo)
        @test !s2.synced && s2.hash == s1.hash                     # unchanged: nothing sent
        write(joinpath(repo, "app", "src", "tasks", "seg", "seg_run.py"), "print('changed')\n")
        s3 = hpc_sync_code!(c, root; root = repo)
        @test s3.synced && s3.hash != s1.hash && s3.dir != s1.dir
        @test read(joinpath(s1.dir, "app", "src", "tasks", "seg", "seg_run.py"), String) == "print('hi')\n"  # old tree kept
        # rsync's own counters: a rerun of an unchanged file sends no literal data.
        dest = joinpath(root, "plain")
        st1 = hpc_rsync(c, repo * "/", dest * "/"; files_from = files)
        st2 = hpc_rsync(c, repo * "/", dest * "/"; files_from = files)
        @test st1.files_transferred == 4 && st2.files_transferred == 0 && st2.bytes_sent == 0
        e = try; hpc_rsync(c, joinpath(repo, "missing") * "/", dest; what = "Up"); catch x; x; end
        @test e isa HpcError && startswith(e.msg, "Up: ")
        e = try; hpc_exec!(c, "no-such-scheduler-cmd --version\n"; what = "Submit"); catch x; x; end
        @test e isa HpcError && startswith(e.msg, "Submit: not installed on the cluster (")
    end
end

@testset "HPC: LocalShellBackend submit / watch / cancel" begin
    if !_HPC_SH
        @test_skip false
    else
        c, b = HpcLocalConn(), LocalShellBackend()
        root = hpc_remote_root(c, mktempdir())
        dir = root * "/jobs/a"
        id = hpc_submit(b, c, HpcJobSpec(; name = "a", dir = dir,
                                         body = "echo one\necho 'two ü'\necho '[PROGRESS] 1/1'\nexit 3"))
        @test occursin(r"^\d+$", id)
        lines = String[]
        r = hpc_watch_job(b, c, id, dir; on_log = l -> push!(lines, l), poll_s = 0.2)
        @test r.exit == 3 && r.reason == "Exit code 3" && !r.cancelled
        @test lines == ["one", "two ü", "[PROGRESS] 1/1"]
        @test isfile(joinpath(dir, "job.sh")) && strip(read(joinpath(dir, ".exit"), String)) == "3"

        ok = hpc_watch_job(b, c, hpc_submit(b, c, HpcJobSpec(; name = "b", dir = root * "/jobs/b", body = "true")),
                           root * "/jobs/b"; on_log = _ -> nothing, poll_s = 0.2)
        @test ok.exit == 0 && ok.reason == ""

        # Cancel reaches the job's children, not only its shell.
        dir_c = root * "/jobs/c"
        id_c = hpc_submit(b, c, HpcJobSpec(; name = "c", dir = dir_c,
                                           body = "sleep 300 & echo \$! > child.pid\nwait"))
        t0 = time()
        rc = hpc_watch_job(b, c, id_c, dir_c; on_log = _ -> nothing, poll_s = 0.2,
                           cancelled = () -> time() - t0 > 1.0)
        @test rc.cancelled && rc.exit === nothing
        sleep(0.5)
        child = strip(read(joinpath(dir_c, "child.pid"), String))
        @test !success(pipeline(`kill -0 $child`; stderr = devnull))
        @test only(hpc_poll(b, c, [Cecelia.HpcPollJob(id_c, dir_c, 0)])).state == JOB_GONE

        # Gone with no exit file (the job's shell killed outright, as a walltime or OOM kill does):
        # reported once the grace polls pass, never read as success.
        dir_d = root * "/jobs/d"
        id_d = hpc_submit(b, c, HpcJobSpec(; name = "d", dir = dir_d, body = "kill -KILL \$\$"))
        rd = hpc_watch_job(b, c, id_d, dir_d; on_log = _ -> nothing, poll_s = 0.1, gone_grace = 1)
        @test rd.exit === nothing && rd.reason == "Job ended without an exit code"
    end
end

# A stand-in for pixi: "installs" the env by linking this machine's python, or fails on demand.
function _hpc_fake_pixi!(root::AbstractString; fail::Bool = false)
    p = joinpath(root, "bin", "pixi-" * Cecelia.HPC_PIXI_VERSION)
    mkpath(dirname(p))
    py = something(Sys.which("python3"), Sys.which("python"))
    write(p, fail ? "#!/bin/sh\necho 'pixi: solve failed' >&2\nexit 1\n" :
              "#!/bin/sh\necho \"fake pixi \$*\"\nmkdir -p .pixi/envs/default/bin && ln -sf '$py' .pixi/envs/default/bin/python\n")
    chmod(p, 0o755)
    p
end

@testset "HPC: env install, status and the dispatch refusal (local remote)" begin
    # The install body refuses anything but Linux x86_64 — the only platform pixi.lock has for it.
    if !_HPC_SH || !(Sys.islinux() && Sys.ARCH === :x86_64)
        @test_skip false
    else
        repo = _hpc_fake_repo()
        c, b = HpcLocalConn(), LocalShellBackend()
        root = hpc_remote_root(c, mktempdir())
        lock = hpc_lock_hash(; root = repo)
        st = hpc_env_status(b, c, root; lock)
        @test !st.installed && st.job == ""
        e = try; hpc_require_env(b, c, root; lock); catch x; x; end
        @test e isa HpcError && occursin("Install it in Settings → HPC", e.msg)

        _hpc_fake_pixi!(root; fail = true)
        lines = String[]
        r = hpc_install_env!(b, c, root; root = repo, poll_s = 0.2, on_log = l -> push!(lines, l))
        @test !r.ok && r.message == "Install failed: Exit code 1"
        @test any(l -> occursin("pixi: solve failed", l), lines)
        @test !hpc_env_status(b, c, root; lock).installed

        _hpc_fake_pixi!(root)
        empty!(lines)
        r = hpc_install_env!(b, c, root; root = repo, poll_s = 0.2, on_log = l -> push!(lines, l))
        @test r.ok && r.message == "Installed"
        @test any(l -> occursin("fake pixi install --frozen -e default", l), lines)
        @test any(l -> occursin("Environment imports OK", l), lines)
        @test isfile(joinpath(Cecelia.hpc_env_dir(root, lock), "pixi.lock"))
        @test !isdir(joinpath(Cecelia.hpc_env_dir(root, lock), "python", "cecelia", "tests"))
        @test hpc_env_status(b, c, root; lock).installed
        @test hpc_require_env(b, c, root; lock) == Cecelia.hpc_env_python(root, lock)
        @test hpc_install_env!(b, c, root; root = repo, poll_s = 0.2).message == "Already installed"
        # A new pixi.lock is a different env: refused until installed.
        write(joinpath(repo, "pixi.lock"), "version: 6\n# changed\n")
        @test !hpc_env_status(b, c, root; lock = hpc_lock_hash(; root = repo)).installed

        # Cancel mid-install.
        p = joinpath(root, "bin", "pixi-" * Cecelia.HPC_PIXI_VERSION)
        write(p, "#!/bin/sh\nsleep 300\n")
        t0 = time()
        rc = hpc_install_env!(b, c, root; root = repo, poll_s = 0.2, on_log = _ -> nothing,
                              cancelled = () -> time() - t0 > 1.5)
        @test !rc.ok && rc.message == "Cancelled"
    end
end

@testset "HPC: SLURM backend + code sync against a live cluster" begin
    if !_HPC_LIVE || !_HPC_SH
        @info "HPC live backend test skipped: CECELIA_HPC_TEST_HOST is not set (CI runs it in verify-hpc.yml)"
        @test_skip false
    else
        empty = HpcClass("", "", "", "", "")
        site = HpcSite("slurm", ENV["CECELIA_HPC_TEST_HOST"], parse(Int, get(ENV, "CECELIA_HPC_TEST_PORT", "22")),
                       get(ENV, "CECELIA_HPC_TEST_REMOTE_DIR", "~/cecelia-test"), "", empty, empty)
        ident = HpcIdentity(ENV["CECELIA_HPC_TEST_USER"], ENV["CECELIA_HPC_TEST_KEY"])
        kh = joinpath(mktempdir(), "hpc", "known_hosts")
        @test hpc_test_connection(site, ident; known_hosts = [kh]).ok      # trusts the host key once
        c = hpc_conn(site, ident; known_hosts = [kh])
        b = hpc_backend(site)
        root = hpc_remote_root(c)
        @test startswith(root, "/") && !occursin("~", root)

        s1 = hpc_sync_code!(c, root)
        s2 = hpc_sync_code!(c, root)
        @test !s2.synced && s2.dir == s1.dir
        @test occursin("yes", hpc_exec!(c, "test -f '$(s1.dir)/python/cecelia/__init__.py' && echo yes\n"; what = "check"))

        dir = root * "/jobs/live-exit"
        id = hpc_submit(b, c, HpcJobSpec(; name = "live-exit", dir = dir, body = "echo from-slurm\nexit 4"))
        lines = String[]
        r = hpc_watch_job(b, c, id, dir; on_log = l -> push!(lines, l), poll_s = 2)
        @test r.exit == 4 && "from-slurm" in lines

        dir_c = root * "/jobs/live-cancel"
        id_c = hpc_submit(b, c, HpcJobSpec(; name = "live-cancel", dir = dir_c, body = "sleep 600"))
        states = RemoteJobState[]
        t0 = time()
        rc = hpc_watch_job(b, c, id_c, dir_c; on_log = _ -> nothing, poll_s = 2, on_state = s -> push!(states, s),
                           cancelled = () -> JOB_RUNNING in states || time() - t0 > 120)
        @test rc.cancelled && JOB_RUNNING in states
        sleep(3)
        @test only(hpc_poll(b, c, [Cecelia.HpcPollJob(id_c, dir_c, 0)])).state == JOB_GONE

        # A walltime kill leaves no exit file; the reason comes from the scheduler.
        dir_t = root * "/jobs/live-timeout"
        id_t = hpc_submit(b, c, HpcJobSpec(; name = "live-timeout", dir = dir_t, body = "sleep 600", walltime_min = 1))
        rt = hpc_watch_job(b, c, id_t, dir_t; on_log = _ -> nothing, poll_s = 5)
        @test rt.exit === nothing && rt.reason == "Walltime reached"

        if get(ENV, "CECELIA_HPC_TEST_ENV_INSTALL", "") == "1"
            lines = String[]
            ri = hpc_install_env!(b, c, root; poll_s = 10, on_log = l -> (push!(lines, l); println(l)))
            @test ri.ok
            ri.ok || @error "Env install failed" message = ri.message
            py = hpc_require_env(b, c, root)
            @test occursin("ok", hpc_exec!(c, "PYTHONPATH='$(s1.dir)/python' '$py' -c 'import cecelia.utils.zarr_utils, cellpose; print(\"ok\")'\n";
                                          what = "env check", timeout_s = 300))
        end
    end
end
