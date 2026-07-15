# Auto-generated LAB-LOG CONTEXT — the app's own account of what the user has done, written into the
# lab log as `[Cecelia]` entries so the notebook records the *what* (activity) alongside the human
# *why*. See docs/ai-assist/LAB-LOG.md.
#
# Design: **snapshot-diff, not an edit log.** `capture_context!` reports the NET change since the last
# capture — never per-edit keystrokes — so a fiddly gating session with 50 polygon nudges that ends
# with one changed gate is ONE line, and nudges that cancel out are nothing. Sources:
#   • tasks   — the per-image run logs (run_log.jl): every task that ran (segment/track/cluster/…),
#               time-filtered by a stored cutoff.
#   • gating  — a per-pop fingerprint (name + a hash of the gate spec) diffed against the last snapshot
#               → populations added/removed and which pops' GATES changed (by name). Reads the gating
#               JSON files directly (source of truth), so it doesn't depend on the segmentation list.
#   • exclusions — the set of excluded image uids, diffed → newly excluded / re-included.
# Snapshots + cutoff live in `settings/lab-log-context.json`. The FIRST capture seeds the gating/
# exclusion baselines silently (no giant retro digest); only genuine deltas thereafter.
#
# Not captured / to tune as we go: gate-change *magnitude* (e.g. % of cells shifted) — only that a
# gate changed, by population. Threshold/geometry values themselves are intentionally out (that's the
# undo-list we don't want).

import Dates

const CONTEXT_AUTHOR = "Cecelia"

_context_state_path(proj::CciaProject)::String =
    joinpath(proj.root, "settings", "lab-log-context.json")

# recursively convert JSON3 values to native String-keyed Dicts / Vectors (avoids the Symbol-key +
# JSON3.Object-isa-Dict gotchas when diffing a re-read snapshot).
_native(x) = x
_native(x::JSON3.Object) = Dict{String,Any}(String(k) => _native(v) for (k, v) in x)
_native(x::JSON3.Array)  = Any[_native(v) for v in x]

function _read_context_state(proj::CciaProject)::Dict{String,Any}
    p = _context_state_path(proj)
    isfile(p) || return Dict{String,Any}()
    try
        s = _native(JSON3.read(read(p, String)))
        s isa Dict{String,Any} ? s : Dict{String,Any}()
    catch
        Dict{String,Any}()
    end
end

function _write_context_state!(proj::CciaProject, state::AbstractDict)
    p = _context_state_path(proj)
    mkpath(dirname(p))
    open(p, "w") do io
        JSON3.write(io, state)
    end
end

# Human-readable image list, capped so a big cohort doesn't produce a wall of names.
function _summarise_images(names::Vector{String})::String
    u = unique(names)
    length(u) > 6 ? string(join(u[1:6], ", "), ", +", length(u) - 6, " more") : join(u, ", ")
end

# ── tasks (run logs) ────────────────────────────────────────────────────────────
# Returns (lines, max_at). Only run-log entries strictly after `cutoff` (ISO timestamps are
# fixed-width, so a lexical `>` is a correct "happened after"). Second-granular + strict, so activity
# in the same second as a prior capture is skipped (negligible — captures are minutes apart).
function _task_lines(proj::CciaProject, cutoff::AbstractString)::Tuple{Vector{String},String}
    by_fun = Dict{String,Vector{String}}()
    max_at = String(cutoff)
    for img in images(proj)
        for e in read_run_log(img)
            at = String(get(e, "at", ""))
            at > cutoff || continue
            push!(get!(by_fun, String(get(e, "fun", "?")), String[]), img.name)
            at > max_at && (max_at = at)
        end
    end
    lines = String[]
    for fun in sort(collect(keys(by_fun)))
        names = unique(by_fun[fun])
        n = length(names)
        push!(lines, "$fun on $n image$(n == 1 ? "" : "s") ($(_summarise_images(names)))")
    end
    (lines, max_at)
end

# ── gating fingerprint + diff ─────────────────────────────────────────────────────
# Per image: { "value_name|pop_type|pop_path" => gate_hash }. gate_hash = "" for a gateless pop; a
# stable hash of the gate spec otherwise. Reads the gating JSON files on disk (the source of truth).
function _gating_fingerprint(proj::CciaProject)::Dict{String,Any}
    fp = Dict{String,Any}()
    for img in images(proj)
        gdir = gating_dir(img._dir)
        isdir(gdir) || continue
        m_all = Dict{String,String}()
        for f in readdir(gdir)
            endswith(f, ".json") || continue
            m = try
                from_tree(JSON3.read(read(joinpath(gdir, f), String), Dict{String,Any}))
            catch
                continue   # skip a corrupt/half-written map rather than fail the whole capture
            end
            for path in pop_paths(m)
                is_root(path) && continue
                g = pop_at(m, path).gate
                m_all["$(m.value_name)|$(m.pop_type)|$path"] =
                    g === nothing ? "" : string(hash(JSON3.write(gate_spec(g))))
            end
        end
        isempty(m_all) || (fp[img.uid] = m_all)
    end
    fp
end

_popname_of_key(k::AbstractString)::String = pop_name(String(split(k, "|")[end]))

function _gating_lines(prev::AbstractDict, cur::AbstractDict, name_of)::Vector{String}
    lines = String[]
    for u in sort(collect(union(keys(prev), keys(cur))))
        p = get(prev, u, Dict{String,Any}())
        c = get(cur, u, Dict{String,Any}())
        added   = sort(unique(String[_popname_of_key(k) for k in keys(c) if !haskey(p, k)]))
        removed = sort(unique(String[_popname_of_key(k) for k in keys(p) if !haskey(c, k)]))
        changed = sort(unique(String[_popname_of_key(k) for k in keys(c)
                                     if haskey(p, k) && string(p[k]) != string(c[k]) && !isempty(string(c[k]))]))
        (isempty(added) && isempty(removed) && isempty(changed)) && continue
        parts = String[]
        isempty(added)   || push!(parts, "added "           * join(added, ", "))
        isempty(removed) || push!(parts, "removed "         * join(removed, ", "))
        isempty(changed) || push!(parts, "gate changed on " * join(changed, ", "))
        push!(lines, "Gating $(name_of(u)): " * join(parts, "; "))
    end
    lines
end

# ── exclusions ─────────────────────────────────────────────────────────────────
_excluded_uids(proj::CciaProject)::Vector{String} =
    sort(String[img.uid for img in images(proj) if !image_included(img)])

function _exclusion_lines(prev::Vector{String}, cur::Vector{String}, name_of)::Vector{String}
    ps, cs = Set(prev), Set(cur)
    newly_ex = sort(String[u for u in cur if !(u in ps)])
    newly_in = sort(String[u for u in prev if !(u in cs)])
    lines = String[]
    isempty(newly_ex) || push!(lines, "Excluded "    * join(String[name_of(u) for u in newly_ex], ", "))
    isempty(newly_in) || push!(lines, "Re-included " * join(String[name_of(u) for u in newly_in], ", "))
    lines
end

"""
Append a dated `[Cecelia]` digest of the NET change since the last capture (tasks run, gating
population/gate changes, exclusions), and advance the stored snapshot. Returns the appended block, or
`nothing` when there is no change. The first capture seeds the gating/exclusion baselines silently.
"""
function capture_context!(proj::CciaProject; date::Dates.Date = Dates.today())::Union{String,Nothing}
    state   = _read_context_state(proj)
    cutoff  = String(get(state, "lastCapturedAt", ""))
    name_by = Dict(img.uid => img.name for img in images(proj))
    name_of(u) = get(name_by, String(u), String(u))

    task_lines, max_at = _task_lines(proj, cutoff)

    cur_gating  = _gating_fingerprint(proj)
    prev_gating = get(state, "gating", nothing)          # absent → first capture → seed silently
    gating_lines = prev_gating === nothing ? String[] : _gating_lines(prev_gating, cur_gating, name_of)

    cur_excl  = _excluded_uids(proj)
    prev_excl = get(state, "excluded", nothing)
    excl_lines = prev_excl === nothing ? String[] :
                 _exclusion_lines(String[String(u) for u in prev_excl], cur_excl, name_of)

    lines = vcat(task_lines, gating_lines, excl_lines)

    # always persist the advanced snapshot (seeds baselines on first capture)
    state["lastCapturedAt"] = max_at
    state["gating"]   = cur_gating
    state["excluded"] = cur_excl
    _write_context_state!(proj, state)

    isempty(lines) && return nothing
    append_lab_log!(proj, CONTEXT_AUTHOR, lines; date = date)
end
