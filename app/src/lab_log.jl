# Per-PROJECT LAB LOG — an append-only, human-and-AI-writable markdown file that is the persistent
# cross-session memory for AI-assisted analysis: methodology, the "why", edge cases, corrections.
# It complements, and does not duplicate, the two existing sidecars:
#   • the per-image RUN LOG (run_log.jl) — machine-written task provenance, and
#   • the per-image `note` field (model/image.jl) — quick per-image observations.
# The lab log is per-PROJECT cross-image knowledge that would otherwise be lost between sessions.
# Stored as `{proj.root}/lab-log.md`, at the project ROOT (not under settings/) so it is visible next
# to the data — the first thing you see returning to a project. Full design: docs/ai-assist/LAB-LOG.md.
#
# Format: one `## YYYY-MM-DD [Author]` block per entry, followed by `-` bullet lines. Entries are
# NEVER edited or deleted — a correction is a new appended `[User — correction]` block. Append-only is
# enforced here (every write only ever appends) and guarded by the project lockfile (with_transaction)
# so a human panel write and a Claude write cannot clobber each other. The date and author tag are
# INJECTED on write — callers never supply the `## …` header — which keeps `[Claude]` entries
# unforgeable and the on-disk format uniform.

import Dates   # used at method-definition time (the `date::Dates.Date` default), not only at runtime

const LAB_LOG_FILENAME = "lab-log.md"

lab_log_path(proj::CciaProject)::String = joinpath(proj.root, LAB_LOG_FILENAME)

# strip anything that would break the one-block-per-entry structure out of a tag/line
_ll_clean(s::AbstractString)::String = strip(replace(string(s), r"[\r\n]+" => " "))

# raw markdown content of the lab log; "" when the file doesn't exist yet.
function read_lab_log(proj::CciaProject)::String
    p = lab_log_path(proj)
    isfile(p) ? read(p, String) : ""
end

"""
Parse lab-log markdown into entry blocks, returned NEWEST-FIRST for efficient context loading.
Each entry is a `Dict{String,Any}` with `date`, `author`, `lines::Vector{String}` and `raw::String`.
A `## <date> [<author>]` line starts an entry; any other `## ` header (e.g. a `[Version boundary…]`
marker) terminates the current entry without becoming one. Robust to hand-editing.
"""
function parse_lab_log(content::AbstractString)::Vector{Dict{String,Any}}
    entries = Dict{String,Any}[]
    cur = nothing
    for line in split(content, '\n')
        m = match(r"^##\s+(\S+)\s+\[(.*?)\]\s*$", line)
        if m !== nothing
            cur = Dict{String,Any}("date" => String(m.captures[1]),
                                   "author" => String(m.captures[2]),
                                   "lines" => String[], "raw" => String[String(line)])
            push!(entries, cur)
        elseif startswith(line, "## ")
            cur = nothing   # a non-entry header — don't attach following lines to the prior entry
        elseif cur !== nothing
            push!(cur["raw"], String(line))
            s = strip(line)
            startswith(s, "- ") && push!(cur["lines"], String(strip(s[3:end])))
        end
    end
    for e in entries
        e["raw"] = strip(join(e["raw"], "\n"))
    end
    reverse(entries)   # newest-first
end

"""
Append one dated, author-tagged block to the project's lab log and persist. The date and author tag
are injected here (callers pass only the author name and the bullet lines). Append-only: existing
content is never rewritten. Guarded by the project lockfile so concurrent human/AI writes don't
clobber. `author` is typically "Claude", "User", or "User — correction"; `lines` is a string or an
iterable of strings (blank lines are dropped — an entry needs at least one non-empty line). Returns
the appended block text.
"""
function append_lab_log!(proj::CciaProject, author::AbstractString, lines;
                         date::Dates.Date = Dates.today())::String
    lines_vec = lines isa AbstractString ? [lines] : collect(lines)
    clean = filter!(!isempty, String[_ll_clean(l) for l in lines_vec])
    isempty(clean) && error("lab log entry needs at least one non-empty line")
    tag = strip(replace(_ll_clean(author), r"[\[\]]" => ""))
    isempty(tag) && error("lab log entry needs an author")
    body  = join(("- " * l for l in clean), "\n")
    block = "## $(Dates.format(date, "yyyy-mm-dd")) [$(tag)]\n$(body)\n"
    with_transaction(proj) do
        p   = lab_log_path(proj)
        sep = (isfile(p) && !isempty(read(p, String))) ? "\n" : ""   # blank line between blocks
        open(p, "a") do io                                           # append-only, never rewrite
            print(io, sep, block)
        end
    end
    block
end

# ── Tuning ratings ────────────────────────────────────────────────────────────
# "Tuning" mode feedback: a 👍/👎 on an auto-entry meaning *this kind of entry is useful / noise* —
# distinct from a "Notes"-mode decision assessment (which is a real [User] log entry). Tuning ratings
# are disposable config, NOT science, so they live in a sidecar keyed by a frontend-computed entry id
# (a stable hash of the entry text) — never in the lab log itself. This is the signal for dialling
# back noisy digest categories. See docs/ai-assist/LAB-LOG.md.

_tuning_path(proj::CciaProject)::String = joinpath(proj.root, "settings", "lab-log-tuning.json")

# { entry_id => "up" | "down" }; {} when none.
function read_tuning(proj::CciaProject)::Dict{String,String}
    p = _tuning_path(proj)
    isfile(p) || return Dict{String,String}()
    try
        Dict{String,String}(String(k) => String(v) for (k, v) in JSON3.read(read(p, String), Dict{String,Any}))
    catch
        Dict{String,String}()
    end
end

"""
Set (or clear) the tuning rating for an entry id. `vote` ∈ `"up"`, `"down"`, or `""` (clear).
Lock-guarded. Returns the updated map.
"""
function set_tuning!(proj::CciaProject, entry_id::AbstractString, vote::AbstractString)::Dict{String,String}
    v = String(vote)
    (v in ("up", "down", "")) || error("tuning vote must be \"up\", \"down\", or \"\" (clear)")
    r = read_tuning(proj)
    isempty(v) ? delete!(r, String(entry_id)) : (r[String(entry_id)] = v)
    with_transaction(proj) do
        p = _tuning_path(proj)
        mkpath(dirname(p))
        open(p, "w") do io
            JSON3.write(io, r)
        end
    end
    r
end

# ── Muted digest categories ─────────────────────────────────────────────────────
# "Mute this kind": stop emitting a whole category (a task-manager tag — Segment, Tracking, Gating, …)
# of [Cecelia] auto-context line from future digests. Config (a sidecar list), not the log. Snapshots
# still advance while muted (capture_context! only skips the LINES), so unmuting reports only future
# change, never a backlog dump. Categories are the dynamic task-spec tags (see `lab_log_categories`),
# so muting is deliberately lenient here — it stores any non-empty category the caller sends (the
# panel offers only real ones); a stale/unknown key simply never matches a digest line. See
# docs/ai-assist/LAB-LOG.md and lab_log_context.jl.

_mutes_path(proj::CciaProject)::String = joinpath(proj.root, "settings", "lab-log-mutes.json")

# the muted category keys; [] when none.
function read_mutes(proj::CciaProject)::Vector{String}
    p = _mutes_path(proj)
    isfile(p) || return String[]
    try
        String[String(x) for x in JSON3.read(read(p, String), Vector{Any})]
    catch
        String[]
    end
end

"""
Mute (`muted=true`) or unmute a digest category. Lenient: any non-empty category string is accepted.
Lock-guarded. Returns the updated muted-category list.
"""
function set_mute!(proj::CciaProject, category::AbstractString, muted::Bool)::Vector{String}
    c = strip(String(category))
    isempty(c) && error("mute category required")
    s = Set(read_mutes(proj))
    muted ? push!(s, c) : delete!(s, c)
    out = sort(collect(s))
    with_transaction(proj) do
        p = _mutes_path(proj)
        mkpath(dirname(p))
        open(p, "w") do io
            JSON3.write(io, out)
        end
    end
    out
end
