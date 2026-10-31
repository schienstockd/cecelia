# What a finished job used against what it asked for: the scheduler's own accounting, read once the job
# is over and logged with the task, so the next run's resources can be sized from a real run
# (docs/HPC.md → *Resources used*).
#
#   hpc_job_usage(b, conn, id)   → HpcUsage, or `nothing` when the scheduler keeps no accounting
#   hpc_usage_lines(u)           → the log lines

"""
A finished job's use of what it booked. Memory in GB, times in minutes (a requested walltime of 0 = no limit); `cpu_busy` is the average
number of CPUs busy (CPU time ÷ walltime). `gpu_util` is the scheduler's GPU utilisation in percent,
`nothing` where it reports none. `final` is false when the scheduler had not closed the job's
accounting yet, so the figures may stop short of the end.
"""
struct HpcUsage
    mem_gb::Float64
    mem_req_gb::Float64
    walltime_min::Float64
    walltime_req_min::Float64
    cpus::Int
    cpu_busy::Float64
    gpu_util::Union{Nothing,Float64}
    final::Bool
end

"""The scheduler's accounting for a finished job; `nothing` for a backend or site that keeps none."""
hpc_job_usage(::HpcBackend, ::HpcConn, ::AbstractString)::Union{HpcUsage,Nothing} = nothing

# A size with a unit as GB (1024-based, as both schedulers count): PBS `2814552kb` / `68719476736b`,
# SLURM `2814552K` / `64G` / `64Gn` (older per-node / per-CPU suffix). `nothing` if it is not a size.
function _hpc_size_gb(s::AbstractString)::Union{Nothing,Float64}
    m = match(r"^\s*([0-9.]+)\s*([kmgtp]?)b?[nc]?\s*$"i, s)
    isnothing(m) && return nothing
    v = tryparse(Float64, m[1])
    isnothing(v) && return nothing
    v * 1024.0^(findfirst(==(lowercase(m[2])), ["", "k", "m", "g", "t", "p"]) - 1) / 1024^3
end

# `[D-]HH:MM:SS[.fff]`, `MM:SS[.fff]` as minutes; `nothing` otherwise (SLURM's `UNLIMITED`).
function _hpc_duration_min(s::AbstractString)::Union{Nothing,Float64}
    m = match(r"^\s*(?:(\d+)-)?(\d+(?::\d+){0,2})(\.\d+)?\s*$", s)
    isnothing(m) && return nothing
    parts = parse.(Int, split(m[2], ':'))
    secs = foldl((a, x) -> 60a + x, parts; init = 0) + (isnothing(m[3]) ? 0.0 : parse(Float64, m[3]))
    length(parts) == 1 && (secs *= 60)        # a bare number is minutes (SLURM Timelimit)
    (parse(Int, something(m[1], "0")) * 86400 + secs) / 60
end

# The remote side of a usage query: ask until the scheduler says the job is over (`done` matches its
# final state), at most `tries` × `pause_s` — a job's `.exit` appears before its accounting is closed.
function _hpc_usage_script(ask::AbstractString, done::AbstractString; tries::Int = 12, pause_s::Real = 5)::String
    """
    i=0
    while :; do out=\$($ask); i=\$((i + 1))
      printf '%s\\n' "\$out" | grep -Eq '$done' && break
      [ -z "\$out" ] && [ \$i -ge 2 ] && break      # no accounting on this site: don't wait a minute
      [ \$i -ge $tries ] && break; sleep $pause_s
    done
    printf '%s\\n' "\$out"
    """
end

function _hpc_fmt_gb(x::Real)::String
    r = x < 10 ? round(x; digits = 1) : round(x)
    string(isinteger(r) ? Int(r) : r, " GB")
end
function _hpc_fmt_min(x::Real)::String
    x < 1 && return string(round(Int, 60x), " s")
    m = round(Int, x)
    m < 60 ? "$m min" : mod(m, 60) == 0 ? "$(div(m, 60)) h" : string(div(m, 60), " h ", lpad(mod(m, 60), 2, '0'))
end
_hpc_pct(a::Real, b::Real) = b > 0 ? string(" (", round(Int, 100a / b), "%)") : ""

"""
    hpc_usage_lines(u) -> Vector{String}

The `[HPC]` lines a finished run logs: each resource as used of booked, with the share, so the next
run's form can be set from them.
"""
function hpc_usage_lines(u::HpcUsage)::Vector{String}
    out = ["[HPC] Resources used" * (u.final ? ":" : " (the scheduler had not closed the job yet):"),
           "[HPC]   Memory    $(_hpc_fmt_gb(u.mem_gb)) of $(_hpc_fmt_gb(u.mem_req_gb))$(_hpc_pct(u.mem_gb, u.mem_req_gb))",
           "[HPC]   Walltime  $(_hpc_fmt_min(u.walltime_min))" *
               (u.walltime_req_min > 0 ? " of $(_hpc_fmt_min(u.walltime_req_min))" : " (no limit)") *
               _hpc_pct(u.walltime_min, u.walltime_req_min),
           "[HPC]   CPUs      $(round(u.cpu_busy; digits = 1)) of $(u.cpus) busy on average$(_hpc_pct(u.cpu_busy, u.cpus))"]
    isnothing(u.gpu_util) || push!(out, "[HPC]   GPU       $(round(Int, u.gpu_util))% utilised")
    out
end
