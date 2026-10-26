# The code and the Python environment on the cluster — docs/todo/HPC_PLAN.md → Q1 and P2.
#
#   <root>/code/<code hash>/   python/ + the task runners under app/src/, synced before each run.
#                              Keyed by content, so a running job's code never changes under it, and
#                              an unchanged tree is one `test -f`.
#   <root>/env/<lock hash>/    pixi.toml + pixi.lock + python/ and the env pixi builds from them.
#                              Rebuilt only when pixi.lock changes; "Install on cluster" in Settings.
#   <root>/bin/pixi-<version>  pixi itself, fetched by the install job.
#   <root>/jobs/<name>/        each job's script, log and `.exit` (backend.jl).
#
# The params contract cannot drift remotely: the code runs from the synced tree, so the runner and the
# Julia that wrote its params are the same revision, and `CECELIA_PY_CONTRACT` (py_runner.jl) holds as
# it does locally. What CAN drift is the env, so a dispatch refuses an env whose lock hash differs
# (`hpc_env_status`).

# The pixi release the install job fetches: the version CI pins (.github/workflows/*.yml
# `pixi-version`), which is the one pixi.lock is written and checked with.
const HPC_PIXI_VERSION = "0.71.1"

const HPC_ENV_MARKER  = ".cecelia-env"
const HPC_CODE_MARKER = ".complete"

# Resources for the install job: the solve is single-threaded, the download and unpack of a CUDA torch
# want memory and time more than cores. Measured: SLURM site 1 min 16 s, MaxRSS 0.6 GB; NCI Gadi copyq
# 2 min, "Memory Used" 4.0 GB of 4 GB requested (PBS counts page cache, so a bigger env reads as more).
# 8 GB / 30 min keeps headroom; 8.3 GB env on disk; the cold download dominates the time. One CPU: an internet queue may allow no more
# (Gadi's copyq). `net`: it downloads pixi and the packages, so it goes to the site's internet queue.
const _HPC_INSTALL_JOB = (; cpus = 1, mem_gb = 8, walltime_min = 30, net = true)

_hpc_repo_root()::String = dirname(_app_dir())

"""`pixi.lock`'s hash, shortened: the name of the env dir on the cluster."""
hpc_lock_hash(; root::AbstractString = _hpc_repo_root())::String =
    _file_sha256(joinpath(root, "pixi.lock"))[1:12]

const _HPC_CODE_SKIP_DIRS = ("__pycache__", "tests", ".pytest_cache")

# Every file under `dir` (relative to `root`) that `keep` accepts, skipping caches, tests and
# egg-info. Sorted, so the hash does not depend on directory order.
function _hpc_tree(root::AbstractString, dir::AbstractString, keep::Function)::Vector{String}
    out = String[]
    base = joinpath(root, dir)
    isdir(base) || return out
    for (d, subdirs, files) in walkdir(base)
        filter!(s -> !(s in _HPC_CODE_SKIP_DIRS) && !endswith(s, ".egg-info"), subdirs)
        for f in files
            keep(f) || continue
            push!(out, replace(relpath(joinpath(d, f), root), '\\' => '/'))
        end
    end
    sort!(out)
end

_hpc_pyfile(f) = !endswith(f, ".pyc")

"""
    hpc_code_files(; root) -> Vector{String}

What a task needs of the code on the cluster, relative to the repo root: the `cecelia` library
(`python/`) and the runners co-located with their tasks (`app/src/**.py`, plus their `.json` side
files: btrack's config is read from beside its runner).
"""
hpc_code_files(; root::AbstractString = _hpc_repo_root())::Vector{String} =
    vcat(_hpc_tree(root, "python", _hpc_pyfile),
         _hpc_tree(root, joinpath("app", "src"), f -> endswith(f, ".py") || endswith(f, ".json")))

"""Content hash of `files` (paths and bytes), shortened: the name of the code dir on the cluster."""
function hpc_code_hash(files::AbstractVector{<:AbstractString}; root::AbstractString = _hpc_repo_root())::String
    ctx = SHA.SHA256_CTX()
    for f in files
        SHA.update!(ctx, codeunits(f * "\0"))
        SHA.update!(ctx, read(joinpath(root, f)))
        SHA.update!(ctx, codeunits("\0"))
    end
    bytes2hex(SHA.digest!(ctx))[1:12]
end

hpc_code_dir(remote_root::AbstractString, hash::AbstractString)::String = remote_root * "/code/" * hash

"""
    hpc_sync_code!(conn, remote_root; root) -> (; hash, dir, synced)

Make this checkout's code exist on the cluster, under its content hash. An unchanged tree is one
remote `test -f`; a changed one is uploaded to a fresh dir, so a job still running the old code keeps
it. `synced` says whether anything was sent.
"""
function hpc_sync_code!(c::HpcConn, remote_root::AbstractString; root::AbstractString = _hpc_repo_root())
    files = hpc_code_files(; root)
    hash = hpc_code_hash(files; root)
    dir = hpc_code_dir(remote_root, hash)
    marker = _sh_quote(dir * "/" * HPC_CODE_MARKER)
    present = strip(hpc_exec!(c, "test -f $marker && echo yes || true\n"; what = "Code sync")) == "yes"
    present && return (; hash, dir, synced = false)
    hpc_exec!(c, "mkdir -p $(_sh_quote(dir))\n"; what = "Code sync")
    hpc_rsync(c, String(root) * "/", dir * "/"; files_from = files, what = "Code sync")
    hpc_exec!(c, ": > $marker\n"; what = "Code sync")
    (; hash, dir, synced = true)
end

# ── The env ────────────────────────────────────────────────────────────────────────────────────────

hpc_env_dir(remote_root::AbstractString, lock::AbstractString)::String = remote_root * "/env/" * lock
hpc_env_python(remote_root::AbstractString, lock::AbstractString)::String =
    hpc_env_dir(remote_root, lock) * "/.pixi/envs/default/bin/python"

# What `pixi install` needs: the manifest, the lock, the editable `cecelia` it installs from python/,
# and the activation scripts the manifest names (pixi checks they exist; the .bat is Windows-only but
# named in the same manifest).
hpc_env_files(; root::AbstractString = _hpc_repo_root())::Vector{String} =
    vcat(["pixi.toml", "pixi.lock"],
         filter(f -> isfile(joinpath(root, f)), ["scripts/activate_juliaup.sh", "scripts/activate_juliaup.bat"]),
         _hpc_tree(root, "python", _hpc_pyfile))

"""
    hpc_env_install_body(remote_root, lock; pixi_version) -> String

The install job's shell: refuse a node pixi.lock has no packages for, fetch pixi once, `pixi install
--frozen` the default env, check it imports, then write the marker `hpc_env_status` looks for. The
pixi cache goes under the remote folder: cluster home quotas are small and a CUDA torch is not.
"""
function hpc_env_install_body(remote_root::AbstractString, lock::AbstractString;
                              pixi_version::AbstractString = HPC_PIXI_VERSION)::String
    R = _sh_quote(remote_root)
    E = _sh_quote(hpc_env_dir(remote_root, lock))
    url = "https://github.com/prefix-dev/pixi/releases/download/v$(pixi_version)/pixi-x86_64-unknown-linux-musl.tar.gz"
    """
    set -eu
    export NO_COLOR=1 PIXI_CACHE_DIR=$R/cache/pixi
    case "\$(uname -sm)" in
      "Linux x86_64") ;;
      *) echo "[ERROR] Cecelia's environment needs Linux x86_64; this node is \$(uname -sm)"; exit 3 ;;
    esac
    P=$R/bin/pixi-$(pixi_version)
    if [ ! -x "\$P" ]; then
      echo "Downloading pixi $(pixi_version)"
      mkdir -p $R/bin && t=\$(mktemp -d)
      if command -v curl >/dev/null 2>&1; then curl -fsSL $(_sh_quote(url)) | tar -xz -C "\$t"
      else wget -qO- $(_sh_quote(url)) | tar -xz -C "\$t"; fi
      mv -f "\$t/pixi" "\$P" && rm -rf "\$t"
    fi
    cd $E
    echo "pixi install (lock $(lock))"
    "\$P" install --frozen -e default --manifest-path $E/pixi.toml
    PYTHONPATH=$E/python $E/.pixi/envs/default/bin/python -c "import cecelia, numpy, zarr; print('Environment imports OK')"
    printf '{"lock":"%s","pixi":"%s"}\\n' $(_sh_quote(lock)) $(_sh_quote(pixi_version)) > $(HPC_ENV_MARKER)
    """
end

"""What the cluster has for this checkout's pixi.lock."""
struct HpcEnvStatus
    lock::String           # this checkout's lock hash
    installed::Bool
    job::String            # an install job still queued or running there, "" if none
end

hpc_env_status_dict(s::HpcEnvStatus)::Dict{String,Any} =
    Dict{String,Any}("lock" => s.lock, "installed" => s.installed, "job" => s.job)

const _HPC_INSTALL_JOBFILE = ".install-job"

"""
    hpc_env_status(b, conn, remote_root; lock) -> HpcEnvStatus

One remote call: is the env for `lock` installed, and is an install for it in flight (one started
by an earlier session counts, so a second click attaches rather than installing twice).
"""
function hpc_env_status(b::HpcBackend, c::HpcConn, remote_root::AbstractString;
                        lock::AbstractString = hpc_lock_hash())::HpcEnvStatus
    E = hpc_env_dir(remote_root, lock)
    out = hpc_exec!(c, """
        test -f $(_sh_quote(E * "/" * HPC_ENV_MARKER)) && echo CECELIA-ENV installed
        j=\$(cat $(_sh_quote(E * "/" * _HPC_INSTALL_JOBFILE)) 2>/dev/null) && echo "CECELIA-ENV job \$j"
        true
        """; what = "Cluster environment")
    installed = occursin("CECELIA-ENV installed", out)
    m = match(r"CECELIA-ENV job (\S+)", out)
    job = ""
    if !installed && !isnothing(m)
        r = only(hpc_poll(b, c, [HpcPollJob(m[1], _hpc_install_job_dir(remote_root, lock), 0)]))
        r.state != JOB_GONE && isnothing(r.exit) && (job = String(m[1]))
    end
    HpcEnvStatus(String(lock), installed, job)
end

_hpc_install_job_dir(remote_root::AbstractString, lock::AbstractString)::String =
    remote_root * "/jobs/install-env-" * lock

"""
    hpc_install_env!(b, conn, remote_root; on_log, on_state, on_job, cancelled, poll_s, root) -> (; ok, message)

"Install on cluster": sync the env files, submit the install job (or attach to one in flight), and
follow it to the end. Sink-agnostic, like a task: it reports through `on_log`, `on_job(id)` (once the
job is known) and `on_state(state)`, and never throws for a failed install; `message` is one brief line.
"""
function hpc_install_env!(b::HpcBackend, c::HpcConn, remote_root::AbstractString;
                          on_log::Function = println, on_state::Function = _ -> nothing,
                          on_job::Function = _ -> nothing,
                          cancelled::Function = () -> false, poll_s::Real = HPC_POLL_S[],
                          root::AbstractString = _hpc_repo_root(),
                          pixi_version::AbstractString = HPC_PIXI_VERSION)
    lock = hpc_lock_hash(; root)
    try
        st = hpc_env_status(b, c, remote_root; lock)
        st.installed && return (; ok = true, message = "Already installed")
        E = hpc_env_dir(remote_root, lock)
        dir = _hpc_install_job_dir(remote_root, lock)
        id = st.job
        if isempty(id)
            on_log("Uploading the environment files")
            hpc_exec!(c, "mkdir -p $(_sh_quote(E))\n"; what = "Upload")
            hpc_rsync(c, String(root) * "/", E * "/"; files_from = hpc_env_files(; root), what = "Upload")
            spec = HpcJobSpec(; name = "install-env", dir = dir, class = "cpu",
                              body = hpc_env_install_body(remote_root, lock; pixi_version),
                              _HPC_INSTALL_JOB...)
            id = hpc_submit(b, c, spec)
            hpc_exec!(c, "printf '%s\\n' $(_sh_quote(id)) > $(_sh_quote(E * "/" * _HPC_INSTALL_JOBFILE))\n";
                      what = "Submit")
            on_log("Submitted install job $id")
        else
            on_log("Following install job $id, already in flight")
        end
        on_job(id)
        state(s) = (on_state(s); on_log(s === JOB_QUEUED ? "Queued on the cluster (job $id)" :
                                        s === JOB_RUNNING ? "Running on the cluster (job $id)" : "Job $id ended"))
        r = hpc_watch_job(b, c, id, dir; on_log, on_state = state, on_reason = x -> on_log("Waiting: $x"),
                          cancelled, poll_s)
        hpc_exec(c, "rm -f $(_sh_quote(E * "/" * _HPC_INSTALL_JOBFILE))\n")
        r.cancelled && return (; ok = false, message = "Cancelled")
        r.exit == 0 || return (; ok = false, message = "Install failed: $(r.reason)")
        hpc_env_status(b, c, remote_root; lock).installed ||
            return (; ok = false, message = "Install finished but the environment is incomplete")
        (; ok = true, message = "Installed")
    catch e
        e isa HpcError || rethrow()
        (; ok = false, message = e.msg)
    end
end

"""
    hpc_require_env(b, conn, remote_root) -> String

The env's python on the cluster, or `HpcError` saying to install it: the refusal a dispatch makes
before submitting anything, naming the cause (the env is for a different pixi.lock).
"""
function hpc_require_env(b::HpcBackend, c::HpcConn, remote_root::AbstractString;
                         lock::AbstractString = hpc_lock_hash())::String
    st = hpc_env_status(b, c, remote_root; lock)
    st.installed && return hpc_env_python(remote_root, lock)
    throw(HpcError(isempty(st.job) ?
        "The cluster has no environment for this version of Cecelia. Install it in Settings → HPC" :
        "The cluster environment is still installing (job $(st.job))"))
end
