# The ONE reader/writer of `settings/analysisBoards.json` — the persisted /analysis boards (the tab
# list + each board's grid layout and slot state).
#
# Why this exists rather than each caller parsing the file: there are three callers in two layers —
# the autosave route, the project-open payload (both `api/src/routes.jl`) and `board_summaries`
# (`ai/lineage.jl`, what the observer reads back). The last time this file had two parsers they
# disagreed about its shape and `get_analysis_lineage` reported NO boards on every project that had
# them, silently, for as long as the code existed. One normaliser, used everywhere.
#
# ── The document has two shapes ──────────────────────────────────────────────────────────────────────
# The original was written verbatim from the frontend's stores, which put the tab ARRAY at `tabs.tabs`
# (a `TabGroup` nested inside the `tabs` key) — a collision that reads badly and is exactly what the
# second parser got wrong. The current shape flattens it:
#
#   {version, tabs: [{id, name}], activeId, nextId, layouts: {"tab:<id>" => …}}     ← current
#   {tabs: {tabs: [{id, name}], activeId, nextId}, layouts: {…}}                    ← legacy, still read
#
# Both are READ; only the current one is written. There is no migration step: real projects have boards
# on disk and `.ccbundle` exports carry the file, so a document is converted the next time it is saved.
#
# ── `version` is optimistic concurrency, not artefact history ────────────────────────────────────────
# The autosave is a debounced whole-document overwrite, so two browser tabs open on one project used to
# clobber each other with no error — the later write simply won. A writer now sends the version it last
# read; a stale one is rejected (409) and reloads. Deliberately NOT a snapshot/restore system: boards
# are add-only from the MCP side and cheap to rebuild, see docs/todo/MCP_BOARD_AUTHORING_PLAN.md
# Decision 1. A document with no `version` (every file written before this) reads as 0.

const BOARDS_DOC_FILE = "analysisBoards.json"

"""
    BoardsDoc

The normalised contents of `analysisBoards.json`. `present` distinguishes "no file" from "a file I
could not read" — degrading those two into the same empty result is what hid the `_board_tabs` bug,
so callers can tell them apart and say so.
"""
struct BoardsDoc
    version::Int
    tabs::Vector{Any}                 # [{id, name}]
    active_id::Int
    next_id::Int
    layouts::Dict{String,Any}         # "tab:<id>" => LayoutEntry (opaque here — the frontend owns it)
    present::Bool
    readable::Bool
end

BoardsDoc() = BoardsDoc(0, Any[], 0, 0, Dict{String,Any}(), false, true)

boards_doc_path(root::AbstractString) = joinpath(root, "settings", BOARDS_DOC_FILE)
boards_doc_path(proj::CciaProject)    = boards_doc_path(proj.root)

_boards_int(v, default::Int = 0) = v isa Integer ? Int(v) :
    v isa AbstractString ? something(tryparse(Int, v), default) :
    v isa Real ? Int(round(v)) : default

"""
    normalise_boards(raw) -> BoardsDoc

Read either document shape out of an already-parsed JSON object. Pure — no file IO — so the autosave
route can run an incoming payload through exactly the same normalisation as a read from disk.
"""
function normalise_boards(raw)::BoardsDoc
    raw isa AbstractDict || return BoardsDoc(0, Any[], 0, 0, Dict{String,Any}(), true, false)
    # JSON3 yields SYMBOL keys, a hand-built or round-tripped Dict yields STRING ones, and this reader
    # is fed by both (a file, and an incoming request body). Key on strings once rather than have every
    # lookup below silently miss — `get(json3_obj, :version, 0)` and `get(dict, :version, 0)` do not
    # agree, which is the JSON3 gotcha in CLAUDE.md.
    d = Dict{String,Any}(string(k) => v for (k, v) in pairs(raw))
    t = get(d, "tabs", nothing)
    tabs, active_id, next_id = if t isa AbstractVector
        (collect(t), _boards_int(get(d, "activeId", 0)), _boards_int(get(d, "nextId", 0)))
    elseif t isa AbstractDict
        # legacy: a TabGroup nested under `tabs`, so the array is at `tabs.tabs`
        g = Dict{String,Any}(string(k) => v for (k, v) in pairs(t))
        inner = get(g, "tabs", nothing)
        (inner isa AbstractVector ? collect(inner) : Any[],
         _boards_int(get(g, "activeId", 0)), _boards_int(get(g, "nextId", 0)))
    else
        (Any[], 0, 0)
    end
    lay = get(d, "layouts", nothing)
    layouts = lay isa AbstractDict ?
        Dict{String,Any}(string(k) => v for (k, v) in pairs(lay)) : Dict{String,Any}()
    BoardsDoc(_boards_int(get(d, "version", 0)), tabs, active_id, next_id, layouts, true, true)
end

"""
    read_boards_doc(path) -> BoardsDoc

The boards document at `path`, in either shape. A missing file is `BoardsDoc()` (`present=false`); an
unreadable one warns and returns `readable=false` rather than passing as "no boards".
"""
function read_boards_doc(path::AbstractString)::BoardsDoc
    isfile(path) || return BoardsDoc()
    raw = try
        JSON3.read(read(path, String))
    catch e
        @warn "Could not parse analysis boards" path exception = e
        return BoardsDoc(0, Any[], 0, 0, Dict{String,Any}(), true, false)
    end
    normalise_boards(raw)
end

"""
    write_boards_doc(path, doc; version = doc.version) -> Int

Write the document in the CURRENT shape (always — a legacy document is converted here) at `version`,
and return that version. Atomic: the autosave fires on every board edit, so a half-written file would
be a routine occurrence rather than a rare one.
"""
function write_boards_doc(path::AbstractString, doc::BoardsDoc; version::Int = doc.version)::Int
    write_json_atomic(path, Dict{String,Any}(
        "version"  => version,
        "tabs"     => doc.tabs,
        "activeId" => doc.active_id,
        "nextId"   => doc.next_id,
        "layouts"  => doc.layouts))
    version
end

"""
    boards_doc_payload(doc) -> Dict

The document as the frontend consumes it (project open, and the reload after a rejected write). Flat,
current-shape, and always carrying `version` — the token the next write must send back.
"""
boards_doc_payload(doc::BoardsDoc) = Dict{String,Any}(
    "version"  => doc.version,
    "tabs"     => doc.tabs,
    "activeId" => doc.active_id,
    "nextId"   => doc.next_id,
    "layouts"  => doc.layouts)
