# ── LabArchives CONTEXT sidecar — the experiment, as recorded in the lab's ELN ────────────────────
# The experimental context that never lives in the file tree: what the cohort was, what protocol was
# followed, what question the imaging is meant to answer. Held per project at
# `settings/labarchives.json` and handed to a fresh Claude session through `session_briefing`, so the
# session is oriented by the EXPERIMENT and not just by the images. Full design + rationale:
# docs/todo/LABARCHIVES_SYNC_PLAN.md.
#
# WHO WRITES IT. Not cecelia. LabArchives is reached through a read-only MCP connector registered in
# the USER's Claude session (Okta-linked, per-user permissions); the backend has no credentials and
# deliberately never learns any. So Claude pulls and calls `set_labarchives_context`, and this file
# stores and serves it. That makes the sidecar a CACHE of an external system of record — which is what
# licenses rewriting it in place while the lab log stays append-only: nothing here is primary data,
# and LabArchives is itself versioned. Deltas worth remembering go to the lab log as dated
# `[LabArchives]` blocks (see `append_lab_log!`), and those are never rewritten.
#
# WHAT IT IS NOT. Not a mirror of a notebook page, not a place for measurements or conclusions, and
# not a second lab log. Sections are a handful of short lines — the briefing carries only their
# headings + the gaps, because an orientation that has to be read in full is not an orientation.

import Dates

const LA_DOC_FILE    = "labarchives.json"
const LA_DOC_VERSION = 1
const LA_MAX_SECTIONS = 12       # a notebook page is not a section; keep this an orientation
const LA_MAX_LINES    = 12       # per section
const LA_LINE_CAP     = 400      # chars

la_doc_path(proj::CciaProject)::String = joinpath(proj.root, "settings", LA_DOC_FILE)

_la_base() = Dict{String,Any}("version" => LA_DOC_VERSION, "source" => Dict{String,Any}(),
                              "syncedAt" => "", "syncedBy" => "", "sections" => Any[],
                              "cohort" => Any[])

"""
    read_la_doc(proj) -> Dict{String,Any}

The LabArchives context sidecar; a base doc with `present=false` when there is none. A file that
won't parse reports `present=true, readable=false` rather than reading as "no context" — the
distinction the analysis-boards doc had to learn the hard way (two parsers disagreed and every
project silently reported no boards).
"""
function read_la_doc(proj::CciaProject)::Dict{String,Any}
    p = la_doc_path(proj)
    isfile(p) || return merge(_la_base(), Dict{String,Any}("present" => false, "readable" => true))
    try
        # json_native (helpers.jl) — JSON3 gives Symbol keys, and everything below indexes by string.
        merge(_la_base(), json_native(JSON3.read(read(p, String))),
              Dict{String,Any}("present" => true, "readable" => true))
    catch
        merge(_la_base(), Dict{String,Any}("present" => true, "readable" => false))
    end
end

_la_str(x, k::AbstractString, dflt::AbstractString = "") =
    (v = get(x, k, nothing); v === nothing ? dflt : strip(first(string(v), LA_LINE_CAP)))

# one section: a heading + its bullet lines (+ optional provenance). Bounded on both axes.
function _la_section(s)::Union{Dict{String,Any},Nothing}
    s isa AbstractDict || return nothing
    heading = _la_str(s, "heading")
    raw     = get(s, "lines", nothing)
    lines   = raw isa AbstractVector ? String[strip(first(string(l), LA_LINE_CAP)) for l in raw] : String[]
    filter!(!isempty, lines)
    length(lines) > LA_MAX_LINES && (lines = lines[1:LA_MAX_LINES])
    (isempty(heading) && isempty(lines)) && return nothing
    Dict{String,Any}("heading" => heading, "lines" => lines,
                     "sourceDate" => _la_str(s, "sourceDate"), "url" => _la_str(s, "url"))
end

# one declared cohort arm: which attribute VALUE the ELN says exists, and how many animals/samples.
function _la_cohort_arm(c)::Union{Dict{String,Any},Nothing}
    c isa AbstractDict || return nothing
    attr  = _la_str(c, "attr")
    value = _la_str(c, "value")
    (isempty(attr) || isempty(value)) && return nothing
    n_raw = get(c, "n", nothing)
    n = n_raw isa Integer ? Int(n_raw) : (n_raw isa AbstractFloat ? round(Int, n_raw) :
                                          something(tryparse(Int, string(something(n_raw, ""))), 0))
    Dict{String,Any}("attr" => attr, "value" => value, "n" => max(n, 0))
end

"""
    write_la_doc!(proj; source, sections, cohort, synced_by="claude") -> Dict{String,Any}

Replace the project's LabArchives context. Normalises and BOUNDS the input (sections, lines per
section, line length) — the writer decides the shape, not the caller — and stamps `syncedAt`.
Lock-guarded + atomic, like every other project sidecar. Returns the stored doc.

A full replace, not a merge: the sidecar mirrors what LabArchives says NOW, and a merge would let a
stale section outlive its deletion in the notebook with no way to tell.
"""
function write_la_doc!(proj::CciaProject; source = Dict{String,Any}(), sections = Any[],
                       cohort = Any[], synced_by::AbstractString = "claude")::Dict{String,Any}
    src = source isa AbstractDict ? source : Dict{String,Any}()
    secs = Any[]
    for s in (sections isa AbstractVector ? sections : Any[])
        length(secs) >= LA_MAX_SECTIONS && break
        sec = _la_section(s)
        sec === nothing || push!(secs, sec)
    end
    arms = Any[]
    for c in (cohort isa AbstractVector ? cohort : Any[])
        arm = _la_cohort_arm(c)
        arm === nothing || push!(arms, arm)
    end
    doc = Dict{String,Any}(
        "version" => LA_DOC_VERSION,
        "source"  => Dict{String,Any}("notebookId"   => _la_str(src, "notebookId"),
                                      "notebookName" => _la_str(src, "notebookName"),
                                      "url"          => _la_str(src, "url"),
                                      "pageIds"      => (v = get(src, "pageIds", nothing);
                                                         v isa AbstractVector ?
                                                         String[string(x) for x in v] : String[])),
        "syncedAt" => Dates.format(Dates.now(Dates.UTC), Dates.dateformat"yyyy-mm-ddTHH:MM:SSZ"),
        "syncedBy" => _la_str(Dict("v" => synced_by), "v", "claude"),
        "sections" => secs,
        "cohort"   => arms)
    with_transaction(proj) do
        p = la_doc_path(proj)
        mkpath(dirname(p))
        write_json_atomic(p, doc)
    end
    merge(doc, Dict{String,Any}("present" => true, "readable" => true))
end

"""
    la_gaps(proj[, doc]) -> Vector{Dict{String,Any}}

Arms the ELN declares that the project has NO images for, as
`[{attr, value, declared, present}]` (`present` is always 0 today — an arm with any image is not a
gap). DERIVED on every read, never stored, so it cannot go stale against the images.

This is the load-bearing half of the feature. Attribute levels are computed from the images PRESENT
(`attr_value_counts`), so deleting an arm also deletes every trace that it was ever planned — after
which nothing inside cecelia can tell you the comparison was supposed to exist. The ELN is the only
remaining record, and this is what surfaces it.

What it deliberately does NOT do is guess WHY. An arm can be missing because it was never imaged,
because it failed QC, or because someone deleted it on purpose — indistinguishable from here. The
reason is a human line in the lab log; this only says the absence is real.
"""
function la_gaps(proj::CciaProject, doc::Dict{String,Any} = read_la_doc(proj))::Vector{Dict{String,Any}}
    arms = get(doc, "cohort", Any[])
    (arms isa AbstractVector && !isempty(arms)) || return Dict{String,Any}[]
    counts = Dict(name => Dict(v => n for (v, n) in vals)
                  for (name, vals) in attr_value_counts(images(proj)))
    out = Dict{String,Any}[]
    for a in arms
        a isa AbstractDict || continue
        attr, value = string(get(a, "attr", "")), string(get(a, "value", ""))
        (isempty(attr) || isempty(value)) && continue
        present = get(get(counts, attr, Dict{String,Int}()), value, 0)
        present == 0 || continue
        push!(out, Dict{String,Any}("attr" => attr, "value" => value,
                                    "declared" => Int(get(a, "n", 0)), "present" => 0))
    end
    out
end

"""
    la_briefing(proj) -> Union{NamedTuple,Nothing}

The COMPACT form for `session_briefing`: where the context came from, when it was synced, the
section HEADINGS (not their text), and the gaps in full. `nothing` when the project has no sidecar,
so the briefing simply omits the key rather than carrying an empty shell.

Headings only is the point — the briefing orients a session in a few lines; a session that wants the
text calls `get_labarchives_context`. Gaps ride along in full because they are short, and because a
gap the session never sees is a gap it will reason straight past.
"""
function la_briefing(proj::CciaProject)
    doc = read_la_doc(proj)
    get(doc, "present", false) || return nothing
    get(doc, "readable", true) ||
        return (; notebookName = "", syncedAt = "", readable = false, sections = String[],
                  gaps = Dict{String,Any}[])
    secs = get(doc, "sections", Any[])
    (; notebookName = string(get(get(doc, "source", Dict()), "notebookName", "")),
       syncedAt = string(get(doc, "syncedAt", "")),
       readable = true,
       sections = String[string(get(s, "heading", "")) for s in secs if s isa AbstractDict],
       gaps = la_gaps(proj, doc))
end
