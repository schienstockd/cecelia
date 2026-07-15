# Auto-generated LAB-LOG CONTEXT — the app's own account of what the user has done, written into the
# lab log as `[Cecelia]` entries so the notebook records not just the *why* (human notes) but the
# *what* (activity) too. v1 source: the per-image run logs (run_log.jl) — every task that ran
# (segmentation, tracking, clustering, measures, …). Grouped into one dated digest per capture.
# See docs/ai-assist/LAB-LOG.md.
#
# Idempotent: a cutoff timestamp is persisted in a sidecar so repeated captures (button or the
# auto-on-open toggle) never re-report the same activity. Manual gate/population edits are not
# timestamped centrally yet — a future source folds those in (a per-project activity log).

const CONTEXT_AUTHOR = "Cecelia"

_context_state_path(proj::CciaProject)::String =
    joinpath(proj.root, "settings", "lab-log-context.json")

# ISO timestamp of the last activity already rolled into the log; "" when never captured.
function _context_cutoff(proj::CciaProject)::String
    p = _context_state_path(proj)
    isfile(p) || return ""
    try
        String(get(JSON3.read(read(p, String), Dict{String,Any}), "lastCapturedAt", ""))
    catch
        ""   # corrupt/legacy → treat as never captured (worst case: one re-report)
    end
end

function _set_context_cutoff!(proj::CciaProject, at::AbstractString)
    p = _context_state_path(proj)
    mkpath(dirname(p))
    open(p, "w") do io
        JSON3.write(io, Dict("lastCapturedAt" => String(at)))
    end
end

# Human-readable image list, capped so a big cohort doesn't produce a wall of names.
function _summarise_images(names::Vector{String})::String
    u = unique(names)
    length(u) > 6 ? string(join(u[1:6], ", "), ", +", length(u) - 6, " more") : join(u, ", ")
end

"""
Append a dated `[Cecelia]` digest of run-log activity since the last capture, and advance the cutoff.
Returns the appended block text, or `nothing` when there is no new activity (so callers — the button
and the auto-on-open toggle — can stay silent). ISO timestamps are fixed-width, so a lexical `>`
compare against the cutoff is a correct "happened after" test.
"""
function capture_context!(proj::CciaProject; date::Dates.Date = Dates.today())::Union{String,Nothing}
    cutoff = _context_cutoff(proj)
    by_fun = Dict{String,Vector{String}}()   # fun => image names
    max_at = cutoff
    for img in images(proj)
        for e in read_run_log(img)
            at = String(get(e, "at", ""))
            at > cutoff || continue
            push!(get!(by_fun, String(get(e, "fun", "?")), String[]), img.name)
            at > max_at && (max_at = at)
        end
    end
    isempty(by_fun) && return nothing
    lines = String[]
    for fun in sort(collect(keys(by_fun)))
        names = unique(by_fun[fun])
        n = length(names)
        push!(lines, "$fun on $n image$(n == 1 ? "" : "s") ($(_summarise_images(names)))")
    end
    block = append_lab_log!(proj, CONTEXT_AUTHOR, lines; date = date)
    _set_context_cutoff!(proj, max_at)
    block
end
