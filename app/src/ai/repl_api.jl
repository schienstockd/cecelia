# REPL / notebook data-access surface exposed to Claude (Observer Phase 2 — foundation).
#
# ONE curated allow-list (`NOTEBOOK_API`) of the notebook-safe READ accessors, plus live introspection
# of their docstrings from the loaded package. `docs/REPL.md` embeds a GENERATED section built from
# this, and a golden test (`app/test/runtests.jl`) fails if that committed section drifts from the live
# docstrings — so REPL.md can never silently fall behind a signature/behaviour change. That is the
# maintenance guarantee: change a listed function's docstring → regenerate REPL.md or CI goes red.
#
# Regenerate the doc after editing a listed function's docstring (or the list):
#     cd app && julia --project -e 'using Cecelia; Cecelia.write_repl_doc()'
#
# WHY docstring-driven, not method-signature introspection: docstrings are stable (no file:line, no
# Julia-version-dependent signature rendering) and already carry the call signature + kwargs (see
# `label_props`/`pop_df`). And the drift-guard then fires exactly when someone edits a *documented*
# function — precisely when REPL.md should change. Per-project structure (which value_names/pops/columns
# actually exist) is NOT here — that stays live via the observer tools (get_populations/get_measure_summary).

# The notebook-safe read accessors. Curated on purpose (small, intentional) — NOT the full ~200-symbol
# export surface. Every entry must be defined, exported, and documented (the test enforces all three).
const NOTEBOOK_API = Symbol[
    # project / image navigation
    :load_project, :sets, :images, :image_by_uid, :img_value_names, :is_tracked,
    # the unified population + cell/track table
    :pop_df,
    # lazy label-props view: build → refine → terminal verb
    :label_props, :select_cols, :view_channel_cols, :view_centroid_cols, :filter_rows, :as_df,
    :channel_columns, :centroid_columns, :col_names,
    # tracks
    :track_props, :track_cell_measures,
    # summary-plot data (the same data behind the analysis-board summary plots)
    :plot_summary_data,
]

# Markers delimiting the generated block inside docs/REPL.md. Everything OUTSIDE them is the
# hand-written cookbook; everything BETWEEN them is regenerated from NOTEBOOK_API.
const REPL_DOC_BEGIN = "<!-- BEGIN GENERATED API — regenerate via Cecelia.write_repl_doc(); do not edit by hand -->"
const REPL_DOC_END   = "<!-- END GENERATED API -->"

# One reference entry: the accessor name, whether it's exported, whether it carries a docstring, and
# the docstring text itself (markdown, as authored on the function).
struct ReplApiEntry
    name::String
    exported::Bool
    documented::Bool
    doc::String
end

# Raw docstring text for a Cecelia symbol — the docstring *as authored*, joined across methods. We pull
# the RAW text (Base.Docs.meta → MultiDoc → DocStr.text) rather than string(Base.Docs.doc(...)): the
# latter re-renders the parsed Markdown and mangles identifiers (`value_name` → `value*name`, because
# `_…_` reads as emphasis). Falls back to the rendered form if that internal layout ever changes.
# Returns "" when there is no docstring.
function _repl_doc_text(sym::Symbol)::String
    m = @__MODULE__
    binding = Base.Docs.Binding(m, sym)
    try
        meta = Base.Docs.meta(m)
        if haskey(meta, binding)
            multidoc = meta[binding]
            parts = String[]
            for sig in multidoc.order
                ds = multidoc.docs[sig]
                raw = strip(join((p for p in ds.text if p isa AbstractString), ""))
                isempty(raw) || push!(parts, raw)
            end
            isempty(parts) || return join(parts, "\n\n")
        end
    catch
        # fall through to the rendered form below
    end
    s = strip(string(Base.Docs.doc(binding)))
    occursin("No documentation found", s) ? "" : s
end

"""
    repl_api_reference() -> Vector{ReplApiEntry}

Introspect the live package for every symbol in `NOTEBOOK_API`: its export status and its docstring.
This is the ground truth the generated REPL.md section and the `get_repl_api` MCP tool are built from —
it can never be stale because it reads the loaded functions, not a hand-maintained list of signatures.
Errors if a listed symbol is not defined in `Cecelia` (a typo/rename in `NOTEBOOK_API`).
"""
function repl_api_reference()::Vector{ReplApiEntry}
    exported = Set(names(@__MODULE__))
    map(NOTEBOOK_API) do sym
        isdefined(@__MODULE__, sym) ||
            error("NOTEBOOK_API lists :$sym but it is not defined in Cecelia (typo or removed?)")
        doc = _repl_doc_text(sym)
        ReplApiEntry(string(sym), sym in exported, !isempty(doc), doc)
    end
end

# The generated markdown block (between the markers), built from `repl_api_reference()`.
function repl_api_section()::String
    io = IOBuffer()
    println(io, REPL_DOC_BEGIN)
    println(io)
    println(io, "*This section is generated from the live docstrings of the `NOTEBOOK_API` accessors ",
                "(`app/src/ai/repl_api.jl`). Do not edit between the markers — after changing a listed ",
                "function's docstring, run `Cecelia.write_repl_doc()`; a golden test fails if it drifts.*")
    for e in repl_api_reference()
        println(io)
        println(io, "### `", e.name, "`", e.exported ? "" : "  *(not exported)*")
        println(io)
        if e.documented
            println(io, e.doc)
        else
            println(io, "*(undocumented — add a docstring to `", e.name, "`.)*")
        end
    end
    println(io)
    print(io, REPL_DOC_END)
    String(take!(io))
end

# Repo-root docs/REPL.md. pkgdir(Cecelia) is the `app/` package dir; docs/ sits one level up at the
# repo root. Only meaningful in the dev checkout (the generator + golden test run there).
repl_doc_path() = joinpath(dirname(pkgdir(@__MODULE__)), "docs", "REPL.md")

# Splice a fresh generated section into an existing REPL.md string (hand-written parts preserved).
# The single source of truth for the splice, shared by the generator and the golden test so they agree.
function render_repl_doc(existing::AbstractString)::String
    b = findfirst(REPL_DOC_BEGIN, existing)
    e = findfirst(REPL_DOC_END, existing)
    (b === nothing || e === nothing) &&
        error("REPL.md is missing the generated-section markers; add REPL_DOC_BEGIN/END back")
    string(existing[1:first(b) - 1], repl_api_section(), existing[last(e) + 1:end])
end

"""
    write_repl_doc([path]) -> path

Regenerate the `<!-- BEGIN/END GENERATED API -->` section of `docs/REPL.md` from the live docstrings,
preserving the hand-written cookbook around it. Run after changing a `NOTEBOOK_API` function's
docstring (or the list itself). The golden test asserts this has been run (idempotent output).
"""
function write_repl_doc(path::AbstractString = repl_doc_path())
    write(path, render_repl_doc(read(path, String)))
    path
end
