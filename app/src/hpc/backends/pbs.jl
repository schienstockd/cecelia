# PBS Pro / OpenPBS — the second scheduler backend (docs/todo/HPC_PLAN.md → Decision 7/8, P5): qsub /
# qstat -f / qstat -x / qdel. Pure renderers and parsers, plus one remote call per verb.
#
# Torque (the older PBS fork) is not this: it has no `qstat -x`.

struct PbsBackend <: HpcBackend
    site::HpcSite
end
hpc_backend_site(b::PbsBackend)::HpcSite = b.site

# PBS job names: up to 236 characters at most sites (15 on old ones), starting with a letter.
_pbs_job_name(name::AbstractString)::String =
    first("cecelia-" * replace(String(name), r"[^A-Za-z0-9_.-]" => "_"), 230)
hpc_job_name(::PbsBackend, name::AbstractString) = _pbs_job_name(name)

"""
    hpc_render_script(b::PbsBackend, spec) -> String

The qsub script: D8's resource model as `#PBS` lines, the class's extra directives verbatim, then the
common body. `-o` is the scheduler's own log (the job writes `job.log` itself, because PBS spools `-o`
on the node until the job ends); it is an absolute path: PBS resolves a relative `-o` on the execution
host's home, not the submit dir.

Resources are job-wide (`-l ncpus`, `-l mem`, `-l ngpus`), not a `select` chunk: PBS Pro turns a
job-wide request into a one-chunk select itself, and NCI Gadi refuses `-l select` outright. The account
goes out as both `-A` (the PBS account string) and `-P` (the project): Gadi charges and grants storage
by `-P` alone, and records `-A` without using it.
"""
function hpc_render_script(b::PbsBackend, spec::HpcJobSpec)::String
    cls = hpc_class(b.site, spec.class)
    d = ["-N " * _pbs_job_name(spec.name),
         "-o " * spec.dir * "/" * HPC_SCHED_LOG,
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
                    what = "Submit", timeout_s = HPC_SUBMIT_TIMEOUT_S)
    id = _last_line(out, "")
    occursin(r"^\d+(\.[A-Za-z0-9_.-]+)?$", id) || throw(HpcError("Submit: unexpected reply from qsub"))
    String(id)
end

const _PBS_QUEUED  = ("Q", "H", "W", "T", "S")
const _PBS_RUNNING = ("R", "E", "B")

# ONE scheduler query per poll, for the jobs asked about: `qstat -f <ids>` gives each one's
# `job_state` and, for a waiting job, the `comment` PBS keeps on it ("Not Running: Insufficient amount
# of resource: ncpus …"; first line only). A finished or unknown id is only absent (it reads as gone);
# PBS says so on stderr and exits non-zero, so the exit code cannot tell an unreachable server from a
# finished job. Any OTHER stderr line can, and fails the poll: an outage must not read as "the job is
# gone".
function _hpc_status_script(::PbsBackend, ids::AbstractVector{<:AbstractString})
    q = join(_sh_quote.(ids), " ")
    _hpc_sched_prelude("qstat") * """
    e=\$(mktemp) || exit 96
    qstat -f $q 2>"\$e" | awk '/^Job Id:/ {id = \$3}
      /^ +job_state = / {print "$(_HPC_STATUS_TAG) " id " " \$3}
      /^ +comment = / {sub(/^ +comment = /, ""); sub(/^Not Running: /, ""); print "$(_HPC_REASON_TAG) " id " " \$0}'
    if grep -v -e 'Job has finished' -e 'Unknown Job Id' "\$e" | grep -q .; then cat "\$e" >&2; rm -f "\$e"; exit 96; fi
    rm -f "\$e"
    """
end

# The nodes behind a queue: a routing queue (Gadi's `gpuvolta`) is followed one hop to where it sends
# jobs, and nodes are matched by their `queue` attribute. A queue that owns no nodes runs on the
# untagged ones (PBS's rule), printed as a shared pool. Memory is converted to MB from PBS's units.
_hpc_nodes_script(::PbsBackend, queue::AbstractString) = _hpc_sched_prelude("pbsnodes") * """
    q=$(_sh_quote(queue))
    [ -n "\$q" ] || q=\$(qstat -Bf 2>/dev/null | awk -F' = ' '/^ *default_queue = / {print \$2; exit}')
    qstat -Qf "\$q" >/dev/null 2>&1 || { echo "$(_HPC_NOQUEUE_TAG)"; exit 0; }
    d=\$(qstat -Qf "\$q" 2>/dev/null | awk -F' = ' '/^ *route_destinations = / {print \$2; exit}')
    [ -n "\$d" ] || d=\$q
    pbsnodes -a 2>/dev/null | awk -v want=",\$d," '
      function mb(v) { u = tolower(v); n = u + 0
        if (u ~ /tb\$/) return n * 1048576; if (u ~ /gb\$/) return n * 1024; if (u ~ /mb\$/) return n
        if (u ~ /kb\$/) return int(n / 1024); return int(n / 1048576) }
      function flush() { if (seen) print (q == "" ? "u" : index(want, "," q ",") ? "q" : "x"), c, g, m
                         seen = 0; q = ""; c = 0; g = 0; m = 0 }
      /^[^ \\t]/ { flush(); seen = 1; next }
      /^[ \\t]+queue = / { q = \$3 }
      /^[ \\t]+resources_available\\.ncpus = / { c = \$3 }
      /^[ \\t]+resources_available\\.ngpus = / { g = \$3 }
      /^[ \\t]+resources_available\\.mem = / { m = mb(\$3) }
      END { flush() }' | grep -v '^x' | sort | uniq -c | awk '{print "$(_HPC_NODES_TAG)", \$2, \$1, \$3, \$4, \$5}'
    true
    """

# The user's Cecelia jobs still queued or running, by job name (`hpc_active_jobs`): `qselect` with no
# state lists every unfinished job; their names come from one `qstat -f`.
_hpc_active_script(::PbsBackend) = _hpc_sched_prelude("qselect") * """
    ids=\$(qselect -u "\$(id -un)") || exit 96
    [ -z "\$ids" ] || qstat -f \$ids 2>/dev/null | awk '/^Job Id:/ {id = \$3} /^ +Job_Name = cecelia-/ {print "$(_HPC_ACTIVE_TAG) " id " " \$3}'
    true
    """

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
