# PBS Pro / OpenPBS — the second scheduler backend (docs/todo/HPC_PLAN.md → Decision 7/8, P5): qsub /
# qselect / qstat -x / qdel. Pure renderers and parsers, plus one remote call per verb.
#
# Torque (the older PBS fork) is not this: it has no `qstat -x`.

struct PbsBackend <: HpcBackend
    site::HpcSite
end

# PBS job names: up to 236 characters at most sites (15 on old ones), starting with a letter.
_pbs_job_name(name::AbstractString)::String =
    first("cecelia-" * replace(String(name), r"[^A-Za-z0-9_.-]" => "_"), 230)

"""
    hpc_render_script(b::PbsBackend, spec) -> String

The qsub script: D8's resource model as `#PBS` lines, the class's extra directives verbatim, then the
common body. The log is an absolute path: PBS resolves a relative `-o` on the execution host's home,
not the submit dir.

Resources are job-wide (`-l ncpus`, `-l mem`, `-l ngpus`), not a `select` chunk: PBS Pro turns a
job-wide request into a one-chunk select itself, and NCI Gadi refuses `-l select` outright. The account
goes out as both `-A` (the PBS account string) and `-P` (the project): Gadi charges and grants storage
by `-P` alone, and records `-A` without using it.
"""
function hpc_render_script(b::PbsBackend, spec::HpcJobSpec)::String
    cls = hpc_class(b.site, spec.class)
    d = ["-N " * _pbs_job_name(spec.name),
         "-o " * spec.dir * "/" * HPC_JOB_LOG,
         "-j oe",
         "-l ncpus=$(spec.cpus)",
         "-l mem=$(spec.mem_gb)gb"]
    spec.gpus > 0 && push!(d, "-l ngpus=$(spec.gpus)")
    push!(d, "-l walltime=" * _hpc_walltime(spec.walltime_min))
    q = hpc_queue(b.site, spec)
    isempty(q) || push!(d, "-q " * q)
    acct = isempty(spec.account) ? cls.account : spec.account
    isempty(acct) || append!(d, ["-A " * acct, "-P " * acct])
    isempty(b.site.email) || append!(d, ["-M " * b.site.email, "-m ae"])
    io = IOBuffer()
    println(io, "#!/bin/bash")
    foreach(x -> println(io, "#PBS ", x), d)
    isempty(cls.directives) || println(io, cls.directives)
    print(io, hpc_job_body(spec, cls.preamble))
    String(take!(io))
end

function hpc_submit(b::PbsBackend, c::HpcConn, spec::HpcJobSpec)::String
    out = hpc_exec!(c, _hpc_submit_script(spec, hpc_render_script(b, spec), "qsub $(HPC_JOB_SCRIPT)";
                                          prelude = _hpc_sched_prelude("qsub"));
                    what = "Submit")
    id = _last_line(out, "")
    occursin(r"^\d+(\.[A-Za-z0-9_.-]+)?$", id) || throw(HpcError("Submit: unexpected reply from qsub"))
    String(id)
end

# `qselect` prints FULL job ids (the `qstat` table truncates long ones), one per line, per state.
const _PBS_QUEUED  = ("Q", "H", "W", "T", "S")
const _PBS_RUNNING = ("R", "E", "B")

function _hpc_status_script(::PbsBackend, ids::AbstractVector{<:AbstractString})
    io = IOBuffer()
    print(io, _hpc_sched_prelude("qselect"))
    println(io, "u=\$(id -un)")
    for s in vcat(collect(_PBS_QUEUED), collect(_PBS_RUNNING))
        println(io, "qselect -u \"\$u\" -s $s | sed 's/^/$(_HPC_STATUS_TAG) /; s/\$/ $s/' || exit 96")
    end
    String(take!(io))
end

function _hpc_parse_status(::PbsBackend, out::AbstractString)::Dict{String,RemoteJobState}
    st = Dict{String,RemoteJobState}()
    for l in split(out, '\n')
        m = match(Regex("^$(_HPC_STATUS_TAG) (\\S+) ([A-Z])\$"), strip(l))
        isnothing(m) && continue
        s = m[2]
        st[m[1]] = s in _PBS_QUEUED ? JOB_QUEUED : s in _PBS_RUNNING ? JOB_RUNNING : JOB_GONE
    end
    st
end

hpc_cancel(::PbsBackend, c::HpcConn, id::AbstractString) =
    (hpc_exec!(c, _hpc_sched_prelude("qdel") * "qdel $(_sh_quote(id))\n"; what = "Cancel"); nothing)

# PBS Pro's own kill codes (`Exit_status`), from its job_exec header: the scheduler, not the job,
# ended it.
const _PBS_EXIT_CODES = Dict(
    -29 => "Walltime reached",
    -27 => "Out of memory",
    -26 => "Out of memory",
    -28 => "CPU time limit reached",
    -11 => "Node failed",
    -3  => "Job could not start on the node",
)

"""
    pbs_exit_reason(text) -> String

A vanished job's end in brief, from `qstat -x -f` output (`Exit_status`, else the `comment` PBS
leaves); `""` when neither says. Pure, so it is tested without a cluster.
"""
function pbs_exit_reason(text::AbstractString)::String
    m = match(r"Exit_status\s*=\s*(-?\d+)", text)
    if !isnothing(m)
        code = parse(Int, m[1])
        haskey(_PBS_EXIT_CODES, code) && return _PBS_EXIT_CODES[code]
        code >= 256 && return "Killed by signal $(code - 256)"
    end
    c = match(r"comment\s*=\s*(.*)", text)
    if !isnothing(c)
        cm = lowercase(c[1])
        occursin("walltime", cm) && occursin("exceed", cm) && return "Walltime reached"
        occursin(r"\bmem\b|memory", cm) && occursin("exceed", cm) && return "Out of memory"
        occursin("deleted", cm) && return "Cancelled on the cluster"
    end
    ""
end

function hpc_exit_reason(::PbsBackend, c::HpcConn, id::AbstractString)::String
    out = hpc_exec(c, _hpc_sched_prelude("qstat") * "qstat -x -f $(_sh_quote(id)) 2>/dev/null\n").out
    pbs_exit_reason(out)
end
