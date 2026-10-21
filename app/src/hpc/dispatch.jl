# Dispatch: a task's `run_py` on the cluster — docs/todo/HPC_PLAN.md → D3, D5 and P3.
#
# The handler runs HERE, unchanged: state, `ccid.json`, QC and the run log never leave this machine.
# Only a `run_py` call that declares what it reads and writes is sent away:
#
#   1. the env for this pixi.lock must exist there (`hpc_require_env`), and the code is synced
#      (`hpc_sync_code!`, content-hashed);
#   2. the declared reads are rsynced into the project's MIRROR (`<root>/projects/<project dir>/`, a
#      cache; this machine stays authoritative), plus model files into `<root>/assets/`;
#   3. every path in the params is rewritten to its mirror path; any other absolute path refuses the
#      run, naming it (an undeclared read would otherwise fail on the cluster, minutes later);
#   4. one job runs the runner (backend.jl); its log streams back as task log lines;
#   5. on success the declared writes come back through a local staging path and a rename, so a
#      store is never half-replaced; anything else the job changed is named in the log, not copied.
#
# The run's target is set when it is submitted (`run_task(...; target = TARGET_HPC)`) and reaches
# `run_py` through `HPC_DISPATCH`, a scoped value: the handlers take no new argument, and a composite's
# steps (or a handler's own child tasks) inherit it.

"""Cluster resources for one run (HPC_PLAN D8): the spec's `"hpc"` block, overridden per run."""
Base.@kwdef struct HpcResources
    cpus::Int = 1
    mem_gb::Int = 4
    gpus::Int = 0
    walltime_min::Int = 60
end

# Wire key (task:run `hpc`, camelCase) → spec key (snake, like `resource_pool`) → bounds.
const _HPC_RESOURCE_KEYS = (
    (wire = "cpus",        spec = "cpus",         lo = 1, hi = 256),
    (wire = "memGb",       spec = "mem_gb",       lo = 1, hi = 4096),
    (wire = "gpus",        spec = "gpus",         lo = 0, hi = 16),
    (wire = "walltimeMin", spec = "walltime_min", lo = 1, hi = 30 * 24 * 60),
)

"""
    task_runs_on_hpc(task) -> Bool

Whether the task may be dispatched: its spec lists `"hpc"` in `env` (code reality 11 — the env chip is
the switch, and this is the one eligibility marker).
"""
function task_runs_on_hpc(task::CciaTask)::Bool
    spec = _task_spec(task)
    isnothing(spec) && return false
    "hpc" in string.(get(spec, "env", String[]))
end

"""The job class: the GPU class for a task whose local pool is `gpu`, else CPU (HPC_PLAN Q2)."""
hpc_task_class(task::CciaTask)::String = _task_pool_name(task) == "gpu" ? "gpu" : "cpu"

_hpc_int(v) = v isa Integer ? Int(v) : v isa AbstractFloat && isinteger(v) ? Int(v) :
              tryparse(Int, strip(string(v)))

"""
    hpc_resources(task, overrides = Dict()) -> HpcResources

The spec's `"hpc"` block with the run's `overrides` (`{cpus, memGb, gpus, walltimeMin}`, any subset)
on top. Throws `ArgumentError` naming a malformed or unknown field. Every ported spec carries the whole
block (ratcheted), so there are no class defaults to keep in step with the form's.
"""
function hpc_resources(task::CciaTask, overrides::AbstractDict = Dict{String,Any}())::HpcResources
    spec = something(_task_spec(task), Dict{String,Any}())
    block = get(spec, "hpc", nothing)
    (block isa AbstractDict && all(k -> haskey(block, k.spec), _HPC_RESOURCE_KEYS)) ||
        throw(ArgumentError("$(_fun_name_from_task(task)) has no complete \"hpc\" block in its spec"))
    vals = Dict{String,Int}(k.spec => something(_hpc_int(block[k.spec]), typemin(Int)) for k in _HPC_RESOURCE_KEYS)
    known = Set(k.wire for k in _HPC_RESOURCE_KEYS)
    for (k, v) in overrides
        String(k) in known || throw(ArgumentError("Unknown cluster resource '$k'"))
    end
    for k in _HPC_RESOURCE_KEYS
        v = get(overrides, k.wire, nothing)
        isnothing(v) || (vals[k.spec] = something(_hpc_int(v), typemin(Int)))
        k.lo <= vals[k.spec] <= k.hi ||
            throw(ArgumentError("Cluster $(k.wire) must be $(k.lo)–$(k.hi)"))
    end
    HpcResources(; cpus = vals["cpus"], mem_gb = vals["mem_gb"], gpus = vals["gpus"],
                 walltime_min = vals["walltime_min"])
end

"""Everything a dispatched `run_py` needs, fixed when the job starts."""
struct HpcDispatch
    task_id::String
    conn::HpcConn
    backend::HpcBackend
    remote_root::String
    project_root::String        # local; its basename names the mirror
    assets_root::String         # local `config_dir()`; only its `models/` may be read
    class::String
    resources::HpcResources
    account::String             # the project's override, "" = the class account
    on_stage::Function          # (::HpcStage, job_id) → the task record + the `task:hpc` frame
end

HpcDispatch(task_id, conn, backend, remote_root, project_root, assets_root, class, resources, account) =
    HpcDispatch(task_id, conn, backend, remote_root, project_root, assets_root, class, resources, account,
                (stage, job) -> nothing)

"""The active dispatch, `nothing` for a local run. Set by `_with_run_target`."""
const HPC_DISPATCH = Base.ScopedValues.ScopedValue{Union{Nothing,HpcDispatch}}(nothing)

"""The cluster job's CPU count while a task is dispatched, else `nothing` (code reality 7)."""
hpc_task_cpus()::Union{Int,Nothing} = (d = HPC_DISPATCH[]; isnothing(d) ? nothing : d.resources.cpus)

# Tests only: `(conn, backend)` used instead of the configured cluster, so a dispatch runs end-to-end
# against `HpcLocalConn` + `LocalShellBackend`. A Ref, not a scoped value: the pool's dispatcher task
# predates any scope a test could open.
const _HPC_TRANSPORT_OVERRIDE = Ref{Any}(nothing)

function _hpc_transport()
    o = _HPC_TRANSPORT_OVERRIDE[]
    isnothing(o) || return o
    site = hpc_site()
    (hpc_conn(site), hpc_backend(site))
end

"""
    hpc_dispatch_for(rec, task, img) -> HpcDispatch

Resolve a cluster run's connection, remote folder, class, resources and account. Throws `HpcError`
(an unconfigured or unreachable cluster) or `ArgumentError` (bad resources) with a brief message.
"""
function hpc_dispatch_for(rec::TaskRecord, task::CciaTask, img::CciaImage;
                          on_stage::Function = (stage, job) -> nothing)::HpcDispatch
    conn, backend = _hpc_transport()
    conn isa HpcSshConn && hpc_load_pending_cancels!()
    root = conn isa HpcLocalConn ? hpc_remote_root(conn, joinpath(config_dir(), "hpc-local")) :
                                   hpc_remote_root(conn)
    proj = dirname(dirname(img._dir))
    account = read_hpc_project_settings(proj)["account"]
    HpcDispatch(rec.id, conn, backend, root, proj, config_dir(), hpc_task_class(task),
                hpc_resources(task, rec.hpc), account, on_stage)
end

"""
    _with_run_target(f, rec, task, img, on_log, on_hpc = (stage, job) -> nothing)

Run `f()` (the handler) locally, or inside an `HPC_DISPATCH` scope for a cluster run. A dispatch that
cannot be set up fails the task with its brief reason, before the handler starts. The run's stage is
kept on `rec` (for `list_tasks`) and announced through `on_hpc`.
"""
function _with_run_target(f::Function, rec::TaskRecord, task::CciaTask, img::CciaImage, on_log::Function,
                          on_hpc::Function = (stage, job) -> nothing)
    rec.target === TARGET_LOCAL && return f()
    stage!(stage::HpcStage, job) = (rec.hpc_stage = stage; rec.hpc_job = String(job);
                                    try; on_hpc(string(stage), rec.hpc_job); catch; end)
    d = try
        hpc_dispatch_for(rec, task, img; on_stage = stage!)
    catch e
        (e isa HpcError || e isa ArgumentError) || rethrow()
        on_log("[ERROR] " * (e isa HpcError ? e.msg : e.msg))
        return nothing
    end
    on_log("[HPC] Running on the cluster: $(d.class) class, $(d.resources.cpus) CPUs, " *
           "$(d.resources.mem_gb) GB, $(d.resources.gpus) GPUs, $(d.resources.walltime_min) min")
    try
        Base.ScopedValues.with(f, HPC_DISPATCH => d)
    finally
        stage!(HPC_STAGE_NONE, "")
    end
end

# ── Paths: local ↔ mirror ──────────────────────────────────────────────────────────────────────────

hpc_mirror_dir(d::HpcDispatch)::String = d.remote_root * "/projects/" * basename(d.project_root)
hpc_assets_dir(d::HpcDispatch)::String = d.remote_root * "/assets"

_under(p::AbstractString, root::AbstractString) = p == root || startswith(p, root * "/")

"""
    hpc_map_path(d, local) -> Union{Nothing, (kind, rel, remote)}

Where a local path lives on the cluster: in the project mirror (`:project`) or the assets
(`:assets`, models only), with its path relative to that root. `nothing` = not reachable there.
"""
function hpc_map_path(d::HpcDispatch, p::AbstractString)
    s = rstrip(String(p), '/')
    if _under(s, d.project_root)
        rel = lstrip(s[ncodeunits(d.project_root)+1:end], '/')
        return (; kind = :project, rel = String(rel), remote = rstrip(hpc_mirror_dir(d) * "/" * rel, '/'))
    end
    models = joinpath(d.assets_root, "models")
    if _under(s, models)
        rel = lstrip(s[ncodeunits(d.assets_root)+1:end], '/')
        return (; kind = :assets, rel = String(rel), remote = hpc_assets_dir(d) * "/" * rel)
    end
    nothing
end

"""
    hpc_rewrite_params(d, params) -> (params, outside)

`params` as JSON-native values with every path under the project or the models dir rewritten to its
cluster path; `outside` lists every other absolute path, which the caller refuses to send.
"""
function hpc_rewrite_params(d::HpcDispatch, params)
    outside = String[]
    walk(x::AbstractString) = begin
        s = String(x)
        m = hpc_map_path(d, s)
        isnothing(m) || return m.remote * (endswith(s, "/") ? "/" : "")
        (length(s) > 1 && isabspath(s)) && push!(outside, s)
        s
    end
    walk(x::AbstractDict) = Dict{String,Any}(String(k) => walk(v) for (k, v) in x)
    walk(x::NamedTuple) = Dict{String,Any}(String(k) => walk(v) for (k, v) in pairs(x))
    walk(x::Union{AbstractVector,Tuple}) = Any[walk(v) for v in x]
    walk(x::Symbol) = String(x)
    walk(x) = x
    (walk(params), unique(outside))
end

# ── The mirror ─────────────────────────────────────────────────────────────────────────────────────

const HPC_MIRROR_MARKER = ".cecelia-mirror"

_hpc_mirror_owner(d::HpcDispatch)::String = gethostname() * ":" * d.project_root

# Claim (or confirm) the project's mirror. Two machines with a project of the same name must not
# overwrite each other, and a mirror that vanished (scratch purge) is named, not silently re-sent.
function _hpc_claim_mirror!(d::HpcDispatch; on_log::Function)
    m = hpc_mirror_dir(d)
    owner = _hpc_mirror_owner(d)
    out = hpc_exec!(d.conn, """
        mkdir -p $(_sh_quote(m)) || exit 1
        f=$(_sh_quote(m * "/" * HPC_MIRROR_MARKER))
        if [ -f "\$f" ]; then echo "CECELIA-OWNER \$(cat "\$f")"
        else printf '%s\\n' $(_sh_quote(owner)) > "\$f" && echo CECELIA-NEW; fi
        """; what = "Mirror")
    occursin("CECELIA-NEW", out) &&
        (on_log("[HPC] No mirror of this project on the cluster yet: the first run uploads its inputs"); return)
    mm = match(r"CECELIA-OWNER (.*)", out)
    theirs = isnothing(mm) ? "" : strip(mm[1])
    theirs == owner || throw(HpcError("The cluster copy of project '$(basename(d.project_root))' " *
        "belongs to $(theirs). Remove $(m) on the cluster to use it from here"))
end

function _hpc_classify(d::HpcDispatch, paths::AbstractVector{<:AbstractString}, what::AbstractString)
    out = Dict(:project => String[], :assets => String[])
    for p in paths
        m = hpc_map_path(d, p)
        isnothing(m) && throw(HpcError("$what $p is outside the project; it cannot reach the cluster"))
        push!(out[m.kind], m.rel)
    end
    out
end

# What else a declared read needs beside it. An image read is often a series INSIDE a store
# (`ccidImage.ome.zarr/0`) whose OME-XML and group attributes live at the store root, one level up —
# the reader falls back there. And `<read>.normstats` is the whole-image normalisation cache: without
# it the cluster recomputes over every plane (the name is `norm_cache.SUFFIX`'s). Only paths that exist
# here are returned. The root files are the ones `ome_xml_utils.load_ome_xml` falls back to.
function hpc_read_companions(path::AbstractString)::Vector{String}
    p = rstrip(String(path), ('/', '\\'))
    out = String[]
    ispath(p * ".normstats") && push!(out, p * ".normstats")
    parent = dirname(p)
    group = zarr_group_meta_files(parent)
    if !isempty(group)
        append!(out, group)
        for f in ("OME", "METADATA.ome.xml")
            ispath(joinpath(parent, f)) && push!(out, joinpath(parent, f))
        end
    end
    out
end

# The declared reads plus their companions: what is synced, fingerprinted and checked, so a changed
# root OME-XML is a different run too.
hpc_with_companions(reads::AbstractVector{<:AbstractString})::Vector{String} =
    unique(vcat(String.(reads), reduce(vcat, hpc_read_companions.(reads); init = String[])))

# Upload the reads. Directories (zarr stores) go with `--delete`, so a chunk removed here is removed
# there too: a stale chunk would otherwise read as data where this machine has fill value.
function _hpc_sync_reads!(d::HpcDispatch, reads::AbstractVector{<:AbstractString}; on_log::Function)
    g = _hpc_classify(d, reads, "Input")
    sent = 0
    for (kind, local_root, remote_root) in ((:project, d.project_root, hpc_mirror_dir(d)),
                                            (:assets, d.assets_root, hpc_assets_dir(d)))
        rels = g[kind]
        isempty(rels) && continue
        for r in rels
            ispath(joinpath(local_root, r)) || throw(HpcError("Input $(joinpath(local_root, r)) does not exist"))
        end
        hpc_exec!(d.conn, "mkdir -p $(_sh_quote(remote_root))\n"; what = "Upload")
        st = hpc_rsync(d.conn, local_root * "/", remote_root * "/"; files_from = rels,
                       args = ["-r", "--delete"], what = "Upload")
        sent += st.bytes_sent
    end
    on_log("[HPC] Inputs synced: $(_hpc_human_bytes(sent)) sent")
    sent
end

_hpc_human_bytes(n::Integer)::String =
    n < 1024 ? "$n B" : n < 1024^2 ? "$(round(n / 1024; digits = 1)) KB" :
    n < 1024^3 ? "$(round(n / 1024^2; digits = 1)) MB" : "$(round(n / 1024^3; digits = 2)) GB"

# Bring the declared writes back. Each lands in `<path>.hpc-incoming` first — seeded by hard links to
# the current local copy (`--link-dest`), so unchanged chunks cost nothing — and is then swapped in by
# rename, so a reader never sees a half-replaced store. Writes the job did not produce are named.
function _hpc_pull_writes!(d::HpcDispatch, writes::AbstractVector{<:AbstractString}; on_log::Function)
    g = _hpc_classify(d, writes, "Output")
    isempty(g[:assets]) || throw(HpcError("A task cannot write into the models folder from the cluster"))
    rels = g[:project]
    isempty(rels) && return 0
    m = hpc_mirror_dir(d)
    probe = join(["if [ -d $(_sh_quote(m * "/" * r)) ]; then echo \"CECELIA-W d $i\"; " *
                  "elif [ -e $(_sh_quote(m * "/" * r)) ]; then echo \"CECELIA-W f $i\"; fi"
                  for (i, r) in enumerate(rels)], "\n") * "\ntrue\n"
    kinds = Dict{Int,Char}(parse(Int, x[2]) => x[1][1]
                           for x in eachmatch(r"CECELIA-W ([df]) (\d+)", hpc_exec!(d.conn, probe; what = "Download")))
    got = 0
    for (i, r) in enumerate(rels)
        dest = joinpath(d.project_root, r)
        k = get(kinds, i, nothing)
        if isnothing(k)
            on_log("[WARN] The cluster job did not write $r")
            continue
        end
        incoming = dest * ".hpc-incoming"
        rm(incoming; recursive = true, force = true)
        mkpath(dirname(dest))
        if k == 'd'
            args = isdir(dest) ? ["-r", "--link-dest=" * abspath(dest)] : ["-r"]
            st = hpc_rsync(d.conn, m * "/" * r * "/", incoming * "/"; up = false, args = args, what = "Download")
        else
            st = hpc_rsync(d.conn, m * "/" * r, incoming; up = false, what = "Download")
        end
        got += st.bytes_sent
        old = dest * ".hpc-old"
        rm(old; recursive = true, force = true)
        ispath(dest) && mv(dest, old)
        mv(incoming, dest)
        rm(old; recursive = true, force = true)
    end
    on_log("[HPC] Outputs copied back: $(_hpc_human_bytes(got))")
    got
end

# Files the job changed under the image dirs it touched that were NOT declared: listed by a dry-run
# pull (`-u`: a file newer here, e.g. this machine's own ccid.json, is not a change there). Logged, not
# copied — by design a remote side effect is lost (D3), and a test asserts there are none. The params
# file, task logs and the normalisation cache are the job's own business.
const _HPC_UNDECLARED_IGNORE = ["--exclude=tasks/", "--exclude=logs/", "--exclude=*.normstats",
                                "--exclude=*.hpc-incoming", "--exclude=*.hpc-old", "--exclude=.rsync-partial/"]

# The object dir (`0/<uid>`, `1/<uid>`) a project-relative path sits in, `nothing` above that level.
function _hpc_object_dir(rel::AbstractString)::Union{String,Nothing}
    p = split(rel, '/')
    length(p) >= 3 ? join(p[1:2], "/") : nothing
end

function hpc_undeclared_changes(d::HpcDispatch, reads, writes)::Vector{String}
    r_rels = _hpc_classify(d, reads, "Input")[:project]
    w_rels = _hpc_classify(d, writes, "Output")[:project]
    dirs = unique(filter(!isnothing, _hpc_object_dir.(vcat(r_rels, w_rels))))
    changed = String[]
    for dir in dirs
        isdir(joinpath(d.project_root, dir)) || continue
        own = ["--exclude=/" * w[ncodeunits(dir)+2:end] for w in w_rels if startswith(w, dir * "/")]
        for f in hpc_rsync_itemize(d.conn, hpc_mirror_dir(d) * "/" * dir * "/", joinpath(d.project_root, dir) * "/";
                                   args = vcat(["-r", "-u"], own, _HPC_UNDECLARED_IGNORE))
            push!(changed, dir * "/" * f)
        end
    end
    changed
end

# ── run_py on the cluster ──────────────────────────────────────────────────────────────────────────

# The environment a remote runner gets: `_py_task_env`'s, with every machine-derived number replaced by
# the job's own allocation (SLURM_CPUS_PER_TASK / PBS NCPUS, else what was asked for), PYTHONPATH on the
# synced code, and cellpose's built-in weights cached under the remote folder.
function _hpc_job_env(d::HpcDispatch, code_dir::AbstractString)::String
    derived = ("CECELIA_TASK_WORKERS", "LOKY_MAX_CPU_COUNT", "CECELIA_USABLE_CPUS")
    io = IOBuffer()
    println(io, "n=\${SLURM_CPUS_PER_TASK:-\${NCPUS:-$(d.resources.cpus)}}")
    for (k, v) in _py_task_env("")
        k == "PYTHONPATH" && (v = code_dir * "/python")
        k == "CECELIA_TASK_WORKERS_WIDEN" && (v = "0")
        if k in derived
            println(io, "export $k=\"\$n\"")
        else
            println(io, "export $k=$(_sh_quote(v))")
        end
    end
    println(io, "export CELLPOSE_LOCAL_MODELS_PATH=$(_sh_quote(hpc_assets_dir(d) * "/cellpose"))")
    String(take!(io))
end

"""
    _run_py_hpc(script_rel, params, task_dir, d; reads, writes, env, on_log, on_progress) -> Bool

`run_py` for a dispatched task: see the header of this file for the steps. Never throws: every
failure is an `[ERROR]` line and `false`, as for a local run.
"""
function _run_py_hpc(script_rel::AbstractString, params, task_dir::AbstractString, d::HpcDispatch;
                     reads::AbstractVector{<:AbstractString}, writes::AbstractVector{<:AbstractString},
                     env::Union{Symbol,Nothing} = nothing,
                     on_log::Function = println, on_progress::Function = (n, t) -> nothing)::Bool
    try
        reads = hpc_with_companions(reads)
        isnothing(env) || throw(HpcError("The '$(pixi_env_name(env))' environment is not available on the cluster"))
        isabspath(String(script_rel)) && throw(HpcError("Custom tasks cannot run on the cluster yet"))
        rel_script = startswith(String(script_rel), "tasks/") ? "app/src/" * script_rel :
                     "python/cecelia/" * script_rel
        py = hpc_require_env(d.backend, d.conn, d.remote_root)
        code = hpc_sync_code!(d.conn, d.remote_root)
        code.synced && on_log("[HPC] Code synced ($(code.hash))")
        p, outside = hpc_rewrite_params(d, params)
        isempty(outside) || throw(HpcError("Cannot send $(first(outside)) to the cluster: it is outside the " *
                                           "project. This task needs porting before it can run there"))
        _hpc_classify(d, writes, "Output")

        # Reattach by rerun (HPC_PLAN Q6): a job this exact run submitted before — same script,
        # params, code, env, resources and inputs — is followed or collected instead of submitted
        # again. The record lives beside the params file and goes once the outputs are home.
        stem = splitext(basename(String(script_rel)))[1]
        fp = hpc_run_fingerprint(d, script_rel, p, code.hash, reads, writes)
        record = joinpath(task_dir, stem * HPC_JOB_RECORD_SUFFIX)
        prev = hpc_read_job_record(record, fp)
        id, dir, done = "", "", false
        if !isnothing(prev)
            r = only(hpc_poll(d.backend, d.conn, [HpcPollJob(prev.id, prev.dir, 0)]))
            if r.exit == 0
                id, dir, done = prev.id, prev.dir, true
                on_log("[HPC] Job $id had already finished: copying its results back")
            elseif isnothing(r.exit) && r.state !== JOB_GONE
                id, dir = prev.id, prev.dir
                on_log("[HPC] Re-attached to job $id, still on the cluster")
            else
                rm(record; force = true)
            end
        end

        if isempty(id)
            d.on_stage(HPC_STAGE_SYNC, "")
            _hpc_claim_mirror!(d; on_log)
            _hpc_sync_reads!(d, reads; on_log)
            # The params travel inside the job script (a quoted heredoc), so submitting is one call.
            jobname = d.task_id * "-" * stem * "-" * string(rand(UInt32); base = 16)
            dir = d.remote_root * "/jobs/" * jobname
            eof = "CECELIA_PARAMS_" * string(rand(UInt64); base = 16)
            body = _hpc_job_env(d, code.dir) *
                   "cat > params.json <<'$eof'\n$(JSON3.write(p))\n$eof\n" *
                   "$(_sh_quote(py)) $(_sh_quote(code.dir * "/" * rel_script)) --params params.json\n"
            r = d.resources
            spec = HpcJobSpec(; name = jobname, dir = dir, body = body, class = d.class, cpus = r.cpus,
                              mem_gb = r.mem_gb, gpus = r.gpus, walltime_min = r.walltime_min, account = d.account)
            id = hpc_submit(d.backend, d.conn, spec)
            hpc_write_job_record(record, fp, id, dir)
            on_log("[HPC] Submitted job $id")
        end
        if !done
            line(l) = (m = match(r"^\[PROGRESS\] (\d+)/(\d+)$", l);
                       isnothing(m) ? on_log(l) : on_progress(parse(Int, m[1]), parse(Int, m[2])))
            state(st) = begin
                st === JOB_GONE || d.on_stage(st === JOB_QUEUED ? HPC_STAGE_QUEUED : HPC_STAGE_RUNNING, id)
                on_log(st === JOB_QUEUED ? "[HPC] Queued on the cluster (job $id)" :
                       st === JOB_RUNNING ? "[HPC] Running on the cluster (job $id)" : "[HPC] Job $id ended")
            end
            w = hpc_watch_job(d.backend, d.conn, id, dir; on_log = line, on_state = state,
                              cancelled = () -> is_cancelled(d.task_id), poll_s = HPC_POLL_S[])
            if w.cancelled
                rm(record; force = true)
                on_log("[HPC] Cancelled job $id")
                return false
            end
            if w.exit != 0
                rm(record; force = true)
                on_log("[ERROR] Cluster job $id failed: $(w.reason)")
                return false
            end
        end
        d.on_stage(HPC_STAGE_PULL, id)
        _hpc_pull_writes!(d, writes; on_log)
        # Only now: a pull that failed (the network, a full disk) keeps the record, so a rerun collects
        # the finished job instead of running it again.
        rm(record; force = true)
        for f in hpc_undeclared_changes(d, reads, writes)
            on_log("[WARN] The cluster job also changed $f — not copied back (not declared)")
        end
        true
    catch e
        e isa HpcError || rethrow()
        on_log("[ERROR] $(e.msg)")
        false
    end
end

"""How often a dispatched task asks the cluster for news, in seconds (the tests shorten it)."""
const HPC_POLL_S = Ref{Float64}(15.0)
