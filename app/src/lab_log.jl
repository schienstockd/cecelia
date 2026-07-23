# Per-PROJECT LAB LOG — an append-only, human-and-AI-writable markdown file that is the persistent
# cross-session memory for AI-assisted analysis: methodology, the "why", edge cases, corrections.
# It complements, and does not duplicate, the two existing sidecars:
#   • the per-image RUN LOG (run_log.jl) — machine-written task provenance, and
#   • the per-image `note` field (model/image.jl) — quick per-image observations.
# The lab log is per-PROJECT cross-image knowledge that would otherwise be lost between sessions.
# Stored as `{proj.root}/lab-log.md`, at the project ROOT (not under settings/) so it is visible next
# to the data — the first thing you see returning to a project. Full design: docs/ai-assist/LAB-LOG.md.
#
# Format: one `## YYYY-MM-DD [Author]` block per entry, followed by `-` bullet lines. The HUMAN record
# (`[User]`/`[Claude]` methodology, corrections) is NEVER edited or deleted — a correction is a new
# appended `[User — correction]` block (`append_lab_log!`). The ONE exception is the app's own DERIVED
# activity digest: the rolling daily `[Cecelia]` block is regenerable from the run logs/gating/
# exclusions, so `upsert_daily_context_block!` rewrites *that day's* `[Cecelia]` block in place as
# activity accrues — every other block (all human entries, prior days' digests) is preserved
# byte-for-byte, so the append-only guarantee that matters (the science record) is intact. Both writers
# are guarded by the project lockfile (with_transaction) so a human panel write and a Claude/app write
# cannot clobber each other. The date and author tag are INJECTED on write — callers never supply the
# `## …` header — which keeps `[Claude]` entries unforgeable and the on-disk format uniform.

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
    tag, clean = _ll_tag_and_lines(author, lines)
    block = _format_lab_block(tag, clean, date)
    with_transaction(proj) do
        p   = lab_log_path(proj)
        sep = (isfile(p) && !isempty(read(p, String))) ? "\n" : ""   # blank line between blocks
        open(p, "a") do io                                           # append-only, never rewrite
            print(io, sep, block)
        end
    end
    block
end

# validate + normalise an (author, lines) pair into (tag, clean_lines); shared by append + upsert.
function _ll_tag_and_lines(author::AbstractString, lines)::Tuple{String,Vector{String}}
    lines_vec = lines isa AbstractString ? [lines] : collect(lines)
    clean = filter!(!isempty, String[_ll_clean(l) for l in lines_vec])
    isempty(clean) && error("lab log entry needs at least one non-empty line")
    tag = strip(replace(_ll_clean(author), r"[\[\]]" => ""))
    isempty(tag) && error("lab log entry needs an author")
    (String(tag), clean)
end

# canonical on-disk block text: `## <date> [<tag>]` header + `-` bullets, trailing newline.
_format_lab_block(tag::AbstractString, clean::Vector{String}, date::Dates.Date)::String =
    "## $(Dates.format(date, "yyyy-mm-dd")) [$(tag)]\n$(join(("- " * l for l in clean), "\n"))\n"

# Split raw lab-log markdown into ordered segments, each the verbatim text of one block (a `## ` header
# and every line up to — but not including — the next `## ` header). Any preamble before the first
# header is its own leading segment. Round-trips: `join(_ll_segments(c), "\n") == c`. Used to rewrite a
# single derived block in place while preserving every other block byte-for-byte.
function _ll_segments(content::AbstractString)::Vector{String}
    segs = String[]
    cur  = String[]
    for ln in split(content, '\n')
        if startswith(ln, "## ") && !isempty(cur)
            push!(segs, join(cur, "\n"))
            cur = String[String(ln)]
        else
            push!(cur, String(ln))
        end
    end
    isempty(cur) || push!(segs, join(cur, "\n"))
    segs
end

# true when a segment is a `[<tag>]` block dated `date` (its first line is the matching `##` header).
function _ll_is_dated_block(seg::AbstractString, tag::AbstractString, date::Dates.Date)::Bool
    first_line = String(first(split(seg, '\n'; limit = 2)))
    m = match(r"^##\s+(\S+)\s+\[(.*?)\]\s*$", first_line)
    m !== nothing && String(m.captures[1]) == Dates.format(date, "yyyy-mm-dd") &&
        strip(String(m.captures[2])) == strip(tag)
end

"""
Upsert the app's rolling DAILY `[Cecelia]` activity block for `date`. The `[Cecelia]` digest is
DERIVED, regenerable data (from run logs / gating / exclusions), so — unlike the human `[User]`/
`[Claude]` methodology record, which stays strictly append-only — the day's block is rewritten in
place as activity accrues: any existing `[Cecelia]` block(s) dated `date` are removed and the freshly
regenerated one is appended at the end. Every OTHER block (all human entries, prior days' `[Cecelia]`
blocks, version boundaries) is preserved byte-for-byte, so the append-only guarantee that matters —
the science record is never rewritten — is intact. Lock-guarded. Returns the block text on a real
change; `nothing` when the regenerated block is identical to the one already on disk (a no-op, so an
open panel doesn't churn). `author` is fixed to `CONTEXT_AUTHOR` by the only caller (capture_context!).
"""
function upsert_daily_context_block!(proj::CciaProject, author::AbstractString, lines;
                                     date::Dates.Date = Dates.today())::Union{String,Nothing}
    tag, clean = _ll_tag_and_lines(author, lines)
    block = _format_lab_block(tag, clean, date)
    with_transaction(proj) do
        p       = lab_log_path(proj)
        content = (isfile(p) && !isempty(read(p, String))) ? read(p, String) : ""
        if isempty(content)
            open(p, "w") do io; print(io, block); end
            return block
        end
        segs    = _ll_segments(content)
        today   = [s for s in segs if _ll_is_dated_block(s, tag, date)]
        # no-op: exactly the same single block already on disk → don't rewrite (avoids panel churn)
        length(today) == 1 && strip(today[1]) == strip(block) && return nothing
        kept    = String[s for s in segs if !_ll_is_dated_block(s, tag, date)]
        joined  = join(kept, "\n")
        joined  = rstrip(joined) == "" ? "" : joined   # kept was only blanks → treat as empty
        sep     = isempty(joined) ? "" : (endswith(joined, "\n") ? "\n" : "\n\n")
        open(p, "w") do io; print(io, joined, sep, block); end
        return block
    end
end

# ── Dismissed (hidden) entries ───────────────────────────────────────────────────
# "Hide this entry": drop a single [Cecelia]/[Claude]/user line from the PANEL view without touching
# the log. The lab log stays APPEND-ONLY (the methodology record is never rewritten) — this is a config
# sidecar of entry ids, exactly like tuning/mutes; the panel filters them out. Un-hide by removing the
# id. The id is the same `entryId(raw)` the frontend computes for tuning.
_dismissed_path(proj::CciaProject)::String = joinpath(proj.root, "settings", "lab-log-dismissed.json")

# the dismissed entry ids; [] when none.
function read_dismissed(proj::CciaProject)::Vector{String}
    p = _dismissed_path(proj)
    isfile(p) || return String[]
    try
        String[String(x) for x in JSON3.read(read(p, String), Vector{Any})]
    catch
        String[]
    end
end

"""
Hide (`dismissed=true`) or un-hide a lab-log entry by its id. Config sidecar only — the log file is
never modified (append-only). Lock-guarded. Returns the updated id list.
"""
function set_dismissed!(proj::CciaProject, entry_id::AbstractString, dismissed::Bool)::Vector{String}
    id = strip(String(entry_id))
    isempty(id) && error("dismiss entry id required")
    s = Set(read_dismissed(proj))
    dismissed ? push!(s, id) : delete!(s, id)
    out = sort(collect(s))
    with_transaction(proj) do
        p = _dismissed_path(proj)
        mkpath(dirname(p))
        open(p, "w") do io
            JSON3.write(io, out)
        end
    end
    out
end
