# ── RO-Crate of a chain run — a Workflow Run Crate that Zenodo and WorkflowHub can read ────────────────
#
# docs/ANALYSIS.md → *RO-Crate export*. One chain run becomes a crate of four files:
#   ro-crate-metadata.json — the JSON-LD description (below);
#   methods.md, parameters.csv — the methods report's Markdown and parameter table, from `methods_report`
#                            (ai/methods_report.jl) — the same text, not a second assembly;
#   chain_template.json    — the frozen template the run executed (the content cache, by its hash).
# Image data and results are NOT copied. They are described by their path inside the project folder,
# under a local `#…` identifier (RO-Crate allows a local identifier for data outside the payload), with
# the path itself in `alternateName`; the root's `description` says so.
#
# Specifications, cited per entity below:
#   RO-Crate 1.1                — https://www.researchobject.org/ro-crate/1.1/ (w3id.org/ro/crate/1.1)
#   Process Run Crate 0.5       — https://w3id.org/ro/wfrun/process/0.5
#   Workflow Run Crate 0.5      — https://w3id.org/ro/wfrun/workflow/0.5
#   Workflow RO-Crate 1.0       — https://w3id.org/workflowhub/workflow-ro-crate/1.0
# Version: 0.5 is the newest Workflow Run Crate that extends RO-Crate 1.1. 0.6 also resolves, but it
# extends RO-Crate 1.3 (a different JSON-LD context), so 0.5 + 1.1 is what current consumers read.
#
# Actions: one CreateAction for the whole run (instrument = the template: the Workflow Run Crate MUST),
# plus one per node × image run-log entry (instrument = the task as a SoftwareApplication: Process Run
# Crate's "one or more computational tools have been executed"). Per image, not per node, because each
# image's run is its own execution with its own times and outcome. A node that never ran on an image
# (skipped, not reached) has no execution and no action; the methods report states those.

const RO_CRATE_METADATA = "ro-crate-metadata.json"
const RO_CRATE_TEMPLATE = "chain_template.json"
const RO_CRATE_METHODS  = "methods.md"
const RO_CRATE_PARAMS   = "parameters.csv"
const _ROC_SPEC    = "https://w3id.org/ro/crate/1.1"
const _ROC_CONTEXT = ["https://w3id.org/ro/crate/1.1/context", "https://w3id.org/ro/terms/workflow-run/context"]
const _ROC_WROC    = "https://w3id.org/workflowhub/workflow-ro-crate/1.0"
# (id, name, version) of each profile the root conforms to (Workflow Run Crate → *Requirements*)
const _ROC_PROFILES = [("https://w3id.org/ro/wfrun/process/0.5", "Process Run Crate", "0.5"),
                       ("https://w3id.org/ro/wfrun/workflow/0.5", "Workflow Run Crate", "0.5"),
                       (_ROC_WROC, "Workflow RO-Crate", "1.0")]
const _ROC_CECELIA_URL = "https://github.com/schienstockd/cecelia"
const _ROC_CECELIA_PAPER = "10.1038/s41467-025-57193-y"     # CITATION.cff → preferred-citation
const _ROC_CLAUDE_URL = "https://www.anthropic.com/claude"
# schema.org ActionStatusType, written as the IRI string the profile lists (Process Run Crate →
# CreateAction `actionStatus`). Active = still running at export: outside the profile's two values,
# which is what the record says.
const _ROC_DONE   = "http://schema.org/CompletedActionStatus"
const _ROC_FAILED = "http://schema.org/FailedActionStatus"
const _ROC_ACTIVE = "http://schema.org/ActiveActionStatus"

_roc_ref(id) = Dict{String,Any}("@id" => id)
# one reference bare, several as a list (RO-Crate 1.1 → *JSON-LD*: no singleton arrays)
_roc_refs(ids) = (r = [_roc_ref(i) for i in ids]; length(r) == 1 ? only(r) : r)
# A local identifier from free text: `#` + the text with each path segment percent-encoded
_roc_local(s::AbstractString) = "#" * join((HTTP.escapeuri(p) for p in split(s, '/')), "/")
_roc_doi(d) = "https://doi.org/" * replace(strip(d), r"^(https?://(dx\.)?doi\.org/|doi:)"i => "")
# A time in UTC as ISO 8601 with its offset (the validator's pattern wants `+hh:mm`). The run log writes
# local wall-clock time with no zone (`_run_log_now`); it is read in this machine's zone at that
# instant (`mktime`, DST included), which is the run's zone whenever the run and the export share a
# machine. "" when absent or unreadable.
_roc_utc(t::DateTime) = Dates.format(t, "yyyy-mm-ddTHH:MM:SS") * "+00:00"
_roc_epoch_utc(epoch::Real) = _roc_utc(Dates.unix2datetime(round(Int, epoch)))
function _roc_time(s)
    t = _mr_time(s)
    t === nothing && return ""
    tm = Libc.TmStruct()
    tm.year = Dates.year(t) - 1900; tm.month = Dates.month(t) - 1; tm.mday = Dates.day(t)
    tm.hour = Dates.hour(t); tm.min = Dates.minute(t); tm.sec = Dates.second(t); tm.isdst = -1
    _roc_epoch_utc(time(tm))
end

# Run-log status → schema.org ActionStatusType (Process Run Crate → CreateAction `actionStatus`).
# `nothing` = not an execution (skipped: asked for, never dispatched).
function _roc_status(s::AbstractString)
    s == "done" && return _ROC_DONE
    s in ("failed", "cancelled", RUN_LOG_INTERRUPTED) && return _ROC_FAILED
    s == RUN_LOG_RUNNING && return _ROC_ACTIVE
    nothing
end

# A crate under construction: entities by @id (the graph is emitted sorted, so it is deterministic)
struct _RocGraph
    ents::Dict{String,Dict{String,Any}}
end
_RocGraph() = _RocGraph(Dict{String,Dict{String,Any}}())
_roc_add!(g::_RocGraph, e::Dict{String,Any}) = (get!(g.ents, e["@id"], e); e["@id"])

# The person a profile stamp names, and Claude when the stamp says Claude or Kiwi started it
# (`author_stamp` shape {profile, via?, model?}). Returns the agents' ids.
function _roc_agents!(g::_RocGraph, stamp)::Vector{String}
    out = String[]
    prof = _mr_str(stamp, "profile")
    if !isempty(prof)
        push!(out, _roc_add!(g, Dict{String,Any}("@id" => _roc_local("profile/" * prof), "@type" => "Person",
                                                 "name" => prof, "description" => "Cecelia profile")))
    end
    via = _mr_str(stamp, "via")
    if haskey(_METHODS_AI_VIAS, via)
        m = _mr_str(stamp, "model")
        e = Dict{String,Any}("@id" => _roc_local("claude/" * (isempty(m) ? "unknown" : m)),
                             "@type" => "SoftwareApplication", "name" => "Claude", "url" => _ROC_CLAUDE_URL)
        isempty(m) ? (e["description"] = METHODS_MODEL_UNKNOWN) : (e["softwareVersion"] = m)
        push!(out, _roc_add!(g, e))
    end
    out
end

# The data entities for `name` in namespace `ns` on `img` (at `version`, "" = latest): image versions
# and label sets resolve to their files' paths inside the project; any other namespace (models,
# clusters, …) has no single file and is described by name.
function _roc_data!(g::_RocGraph, img::CciaImage, root::AbstractString, name::AbstractString,
                    ns::AbstractString, version::AbstractString)::Vector{String}
    v = isempty(version) ? nothing : String(version)
    paths = ns == ""       ? [img_filepath(img, String(name); version = v)] :
            ns == "labels" ? [img_labels_path(img, name; version = v), img_label_props_path(img, name; version = v)] :
                             nothing
    if paths === nothing
        id = _roc_local("$(ns)/$(name)@$(img.uid)")
        return [_roc_add!(g, Dict{String,Any}("@id" => id, "@type" => "CreativeWork",
                                              "name" => "$(ns) \"$(name)\" ($(img.name))"))]
    end
    out = String[]
    for p in paths
        p === nothing && continue
        rel = replace(relpath(p, root), '\\' => '/')
        dir = isdir(p) || endswith(p, ".zarr")
        e = Dict{String,Any}("@id" => _roc_local(rel), "@type" => dir ? "Dataset" : "File",
                             "name" => "$(basename(p)) ($(img.name))", "alternateName" => rel)
        endswith(p, ".h5ad") && (e["encodingFormat"] = "application/x-hdf5")   # AnnData on HDF5
        ispath(p) || (e["description"] = "Not on disk when the crate was written.")
        push!(out, _roc_add!(g, e))
    end
    out
end

# One CreateAction for one run-log entry of a chain node on one image; `nothing` when it was no execution
function _roc_node_action!(g::_RocGraph, run, n, img::CciaImage, root, e, label::AbstractString,
                           task_app::AbstractString)
    st = _roc_status(run_entry_status(e))
    st === nothing && return nothing
    row = run_log_row(e)
    obj = String[]
    for d in _rl_get(e, "inputs", Any[])
        d isa AbstractDict || continue
        append!(obj, _roc_data!(g, img, root, _mr_str(d, "name"), _mr_str(d, "ns"), _mr_str(d, "version")))
    end
    res = String[]
    leaves = _history_leaves(n.fn)
    if leaves !== nothing && st != _ROC_FAILED && !run_entry_unchanged(e)
        name, ns, _ = try _run_output_identity(n.fn, row.params, leaves) catch; ("", "", false) end
        isempty(name) || append!(res, _roc_data!(g, img, root, name, ns, ""))
    end
    a = Dict{String,Any}("@id" => _roc_local("run/$(run.id)/$(n.id)/$(img.uid)"), "@type" => "CreateAction",
                         "name" => "$label on $(img.name)", "instrument" => _roc_ref(task_app),
                         "actionStatus" => st)
    changed = _mr_node_params(run, n)
    a["description"] = "Chain node $(n.id) (`$(n.fn)`), task $(_rl_str(e, "taskId")); parameters changed from default: $changed"
    t0 = _roc_time(_rl_str(e, "at")); t1 = _roc_time(_rl_str(e, "finishedAt"))
    isempty(t0) || (a["startTime"] = t0)
    isempty(t1) || (a["endTime"] = t1)
    st == _ROC_FAILED && (a["error"] = run_entry_status(e))
    isempty(obj) || (a["object"] = _roc_refs(unique(obj)))
    isempty(res) || (a["result"] = _roc_refs(unique(res)))
    ag = _roc_agents!(g, _rl_get(e, "author", nothing))
    isempty(ag) || (a["agent"] = _roc_refs(ag))
    _roc_add!(g, a)
    (a, obj, res)
end

# Each object's keys in reading order — `@context`/`@graph`, then `@id`, `@type`, `name`, then the rest
# sorted — so the same record gives the same bytes and a person can read the file (`_pretty_json`).
const _ROC_KEY_ORDER = Dict("@context" => 1, "@graph" => 2, "@id" => 3, "@type" => 4, "name" => 5)
_roc_key_order(k) = (get(_ROC_KEY_ORDER, string(k), 9), string(k))

"""
    chain_run_ro_crate_files(proj, run_id; published = today()) -> Vector{Pair{String,Vector{UInt8}}}

The files of a chain run's RO-Crate, `name => bytes`, metadata first: `ro-crate-metadata.json`
(Workflow Run Crate 0.5 on RO-Crate 1.1), `methods.md` + `parameters.csv` (`methods_report`, chain-run
scope) and `chain_template.json` (the frozen template the run executed). `published` is the root's
`datePublished`; everything else comes from the run record and the run log, so the same record and
date give the same bytes. Read-only.
"""
function chain_run_ro_crate_files(proj::CciaProject, run_id::AbstractString;
                                  published::Date = Dates.today())::Vector{Pair{String,Vector{UInt8}}}
    run = load_chain_run(proj, String(run_id))
    report = methods_report(proj; run_id = String(run_id))
    root = rstrip(proj.root, ['/', '\\'])
    cache = joinpath(_cache_dir(proj), "$(run.template_hash).json")
    tpl_bytes = isfile(cache) ? read(cache) : Vector{UInt8}(codeunits(_template_json(run.template_snapshot)))
    g = _RocGraph()

    # ── the software: Cecelia per recorded version (+ commit), each task as a tool of it ──
    imgs = Dict{String,CciaImage}()
    for uid in run.image_uids
        img = try init_object(proj.uid, uid) catch; nothing end
        img isa CciaImage && (imgs[uid] = img)
    end
    entries = Dict{Tuple{String,String},Any}()            # (uid, taskId) => newest entry
    for (uid, img) in imgs, e in read_run_log_history(img)
        id = _rl_str(e, "taskId"); isempty(id) || (entries[(uid, id)] = e)
    end
    paper = _roc_add!(g, Dict{String,Any}("@id" => _roc_doi(_ROC_CECELIA_PAPER), "@type" => "ScholarlyArticle",
        "name" => "Cecelia: a multifunctional image analysis toolbox for decoding spatial cellular interactions and behaviour",
        "identifier" => _roc_doi(_ROC_CECELIA_PAPER)))
    function cecelia_app(e)
        v = _rl_str(e, "ceceliaVersion"); c = _rl_str(e, "ceceliaCommit")
        # an absolute IRI where one exists (Process Run Crate → SoftwareApplication `@id` SHOULD): the
        # commit, else the release tag, else a local id
        id = !isempty(c) ? "$(_ROC_CECELIA_URL)/commit/$c" : !isempty(v) ? "$(_ROC_CECELIA_URL)/releases/tag/v$v" :
             _roc_local("software/cecelia/unknown")
        app = Dict{String,Any}("@id" => id, "@type" => "SoftwareApplication", "name" => "Cecelia",
                               "url" => _ROC_CECELIA_URL, "citation" => _roc_ref(paper))
        isempty(v) ? (app["description"] = "Version not recorded on this run") : (app["softwareVersion"] = v)
        isempty(c) || (app["identifier"] = c)
        _roc_add!(g, app)
    end
    cites = Dict{String,Vector{String}}()                   # fn => DOIs, from the parameter table's `cite`
    for r in report["table"]
        c = _mr_cite(r); isempty(c) && continue
        append!(get!(Vector{String}, cites, string(get(r, "task", ""))), filter(!isempty, strip.(split(c, ";"))))
    end
    label_of(fn) = (spec = try _task_spec(_task_from_fun_name(fn)) catch; nothing end;
                    spec isa AbstractDict ? string(get(spec, "label", fn)) : fn)

    # ── one action per node × image execution, then the run's ──
    node_actions = String[]; all_obj = String[]; all_res = String[]; ends = String[]
    task_versions = Dict{String,Vector{String}}()           # task app id => Cecelia versions it ran under
    nodes = _topo_sort(run.template_snapshot)
    for n in nodes, uid in sort(run.image_uids)
        st = get(get(run.image_states, uid, Dict{String,Any}()), n.id, nothing)
        (st === nothing || st.task_id === nothing || !haskey(imgs, uid)) && continue
        e = get(entries, (uid, st.task_id), nothing)
        e === nothing && continue
        tid = _roc_local("software/task/" * n.fn)
        app = get!(g.ents, tid) do
            d = Dict{String,Any}("@id" => tid, "@type" => "SoftwareApplication", "name" => label_of(n.fn),
                                 "alternateName" => n.fn, "url" => _ROC_CECELIA_URL, "softwareRequirements" => Any[])
            dois = sort!(unique(get(cites, n.fn, String[])))
            for d0 in dois
                _roc_add!(g, Dict{String,Any}("@id" => _roc_doi(d0), "@type" => "ScholarlyArticle",
                                              "identifier" => _roc_doi(d0), "name" => d0))
            end
            isempty(dois) || (d["citation"] = _roc_refs(_roc_doi.(dois)))
            d
        end
        req = _roc_ref(cecelia_app(e))
        req in app["softwareRequirements"] || push!(app["softwareRequirements"], req)
        v = _rl_str(e, "ceceliaVersion")
        isempty(v) || push!(get!(Vector{String}, task_versions, tid), v)
        r = _roc_node_action!(g, run, n, imgs[uid], root, e, label_of(n.fn), tid)
        r === nothing && continue
        a, obj, res = r
        push!(node_actions, a["@id"]); append!(all_obj, obj); append!(all_res, res)
        haskey(a, "endTime") && push!(ends, a["endTime"])
    end
    for app in values(g.ents)
        haskey(app, "softwareRequirements") || continue
        app["softwareRequirements"] = _roc_refs(sort!(unique(x["@id"] for x in app["softwareRequirements"])))
        # a task is part of Cecelia: its version is the one Cecelia version it ran under, when one
        vs = unique(get(task_versions, app["@id"], String[]))
        length(vs) == 1 && (app["softwareVersion"] = only(vs))
    end

    states = [st for m in values(run.image_states) for st in values(m)]
    run_status = any(st -> st.status in (NODE_FAILED, NODE_CANCELLED), states) ? _ROC_FAILED :
                 any(st -> st.status in (NODE_PENDING, NODE_QUEUED, NODE_RUNNING, NODE_WAITING), states) ?
                     _ROC_ACTIVE : _ROC_DONE
    run_action = Dict{String,Any}("@id" => _roc_local("run/$(run.id)"), "@type" => "CreateAction",
        "name" => "Run $(run.id) of chain \"$(run.chain_name)\"",
        "description" => "The chain run as a whole: its images in, every step's results out. Each step on each image is its own action (mentions).",
        "instrument" => _roc_ref(RO_CRATE_TEMPLATE), "actionStatus" => run_status,
        "startTime" => _roc_epoch_utc(run.created_at),
        "subjectOf" => _roc_ref(RO_CRATE_METHODS))
    isempty(ends) || (run_action["endTime"] = maximum(ends))
    made = Set(all_res)
    ins = sort!(unique(filter(!in(made), all_obj)))
    isempty(ins) || (run_action["object"] = _roc_refs(ins))
    isempty(all_res) || (run_action["result"] = _roc_refs(sort!(unique(all_res))))
    ag = _roc_agents!(g, isempty(run.by) ? nothing : Dict("profile" => run.by))
    isempty(ag) || (run_action["agent"] = _roc_refs(ag))
    run_status == _ROC_FAILED && (run_action["error"] = "a step failed or was cancelled; see methods.md")
    _roc_add!(g, run_action)

    # ── the files, the workflow, the profiles, the root ──
    _roc_add!(g, Dict{String,Any}("@id" => RO_CRATE_TEMPLATE, "@type" => ["File", "SoftwareSourceCode", "ComputationalWorkflow"],
        "name" => run.chain_name, "encodingFormat" => "application/json",
        "description" => "The chain template as this run executed it, frozen. Its version is its content hash.",
        "version" => run.template_hash,
        "programmingLanguage" => _roc_ref("#cecelia-chain")))
    _roc_add!(g, Dict{String,Any}("@id" => "#cecelia-chain", "@type" => "ComputerLanguage",
        "name" => "Cecelia chain template", "url" => _ROC_CECELIA_URL))
    _roc_add!(g, Dict{String,Any}("@id" => RO_CRATE_METHODS, "@type" => "File", "name" => "Methods (draft)",
        "encodingFormat" => "text/markdown", "description" => "Draft Methods section generated from the project record. Check it before use."))
    _roc_add!(g, Dict{String,Any}("@id" => RO_CRATE_PARAMS, "@type" => "File", "name" => "Parameter table",
        "encodingFormat" => "text/csv", "description" => "One row per step: parameters changed from their defaults."))
    for (id, name, v) in _ROC_PROFILES
        _roc_add!(g, Dict{String,Any}("@id" => id, "@type" => "CreativeWork", "name" => name, "version" => v))
    end
    _roc_add!(g, Dict{String,Any}("@id" => "#license", "@type" => "CreativeWork", "name" => "No licence chosen",
        "description" => "Cecelia does not choose a licence for your data. Set one before you publish this crate."))
    _roc_add!(g, Dict{String,Any}("@id" => RO_CRATE_METADATA, "@type" => "CreativeWork",
        "conformsTo" => _roc_refs([_ROC_SPEC, _ROC_WROC]), "about" => _roc_ref("./")))
    # every data entity the actions name hangs off the root (RO-Crate 1.1: data entities MUST be
    # reachable from the root through `hasPart`), after the four files actually in the crate
    data_ids = sort!([id for (id, e) in g.ents if startswith(id, "#") && get(e, "@type", "") in ("File", "Dataset")])
    _roc_add!(g, Dict{String,Any}("@id" => "./", "@type" => "Dataset",
        "name" => "Chain run $(run.id): $(run.chain_name)",
        "description" => "Chain \"$(run.chain_name)\" (run $(run.id)) on $(length(run.image_uids)) images in Cecelia project " *
                         "\"$(proj.name)\". Image data and results are not included: each is referenced by its path inside " *
                         "the project folder (alternateName), relative to the project root. Times are the run log's " *
                         "local wall-clock times, given in UTC.",
        "datePublished" => Dates.format(published, "yyyy-mm-dd"),
        "license" => _roc_ref("#license"),
        "conformsTo" => _roc_refs(first.(_ROC_PROFILES)),
        "mainEntity" => _roc_ref(RO_CRATE_TEMPLATE),
        (isempty(run.by) ? () : ("author" => _roc_ref(_roc_local("profile/" * run.by)),))...,
        "hasPart" => _roc_refs(vcat([RO_CRATE_TEMPLATE, RO_CRATE_METHODS, RO_CRATE_PARAMS], data_ids)),
        "mentions" => _roc_refs(vcat(run_action["@id"], node_actions))))

    order(id) = id == RO_CRATE_METADATA ? (0, id) : id == "./" ? (1, id) : (2, id)
    graph = [g.ents[id] for id in sort!(collect(keys(g.ents)); by = order)]
    doc = Dict{String,Any}("@context" => _ROC_CONTEXT, "@graph" => graph)
    meta = sprint(io -> _pretty_json(io, doc; key_order = _roc_key_order))
    [RO_CRATE_METADATA => Vector{UInt8}(codeunits(meta)),
     RO_CRATE_METHODS  => Vector{UInt8}(codeunits(report["markdown"])),
     RO_CRATE_PARAMS   => Vector{UInt8}(codeunits(report["csv"])),
     RO_CRATE_TEMPLATE => tpl_bytes]
end

"""
    chain_run_ro_crate_zip(proj, run_id; published = today()) -> Vector{UInt8}

The crate as one `.zip` (stored; every entry dated `published`), in memory — what the download route
serves. Same files, same order as `chain_run_ro_crate_files`.
"""
chain_run_ro_crate_zip(proj::CciaProject, run_id::AbstractString; published::Date = Dates.today())::Vector{UInt8} =
    take!(write_stored_zip(IOBuffer(), chain_run_ro_crate_files(proj, run_id; published); modified = DateTime(published)))

"""
    chain_run_ro_crate(proj, run_id, dest; published = today()) -> String

Write a chain run's RO-Crate (`chain_run_ro_crate_files`) to `dest`: a `.zip` when `dest` ends in
`.zip` (stored, one file), else a directory. Returns `dest`. No image data is copied.
"""
function chain_run_ro_crate(proj::CciaProject, run_id::AbstractString, dest::AbstractString;
                            published::Date = Dates.today())::String
    if endswith(lowercase(dest), ".zip")
        bytes = chain_run_ro_crate_zip(proj, run_id; published)
        write_atomic(io -> write(io, bytes), dest)
    else
        for (name, bytes) in chain_run_ro_crate_files(proj, run_id; published)
            write_atomic(io -> write(io, bytes), joinpath(dest, name))
        end
    end
    String(dest)
end
