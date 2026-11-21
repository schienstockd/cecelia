# Experiment design routes — the one design a set follows (app/src/experiment_design.jl).
# Plan: docs/todo/EXPERIMENT_DESIGN_PLAN.md.
#
# The design lives on the set (`meta["experiment_design"]`), its changes in the set's
# `experiment_design.history.jsonl`. The grid is computed from the image attributes on every read.
# One reply shape for all three routes:
#
#   { design (null = no design), version, hash, grid (null without a design), history: [{version, hash,
#     at, by, change, mode, status}] }

function _xd_reply(set::CciaSet, d::Union{ExperimentDesign,Nothing}; attrs = Dict{String,Any}())::String
    hist = experiment_design_history(set)
    JSON3.write((;
        design  = d === nothing ? nothing : experiment_design_dict(d),
        version = length(hist),
        hash    = d === nothing ? nothing : experiment_design_hash(d),
        grid    = d === nothing ? nothing : experiment_design_grid(set, d; attrs = attrs),
        history = [(; version = get(e, "version", i), hash = get(e, "hash", nothing), at = get(e, "at", ""),
                      by = get(e, "by", nothing), change = get(e, "change", ""),
                      mode = get(something(get(e, "design", nothing), Dict()), "mode", nothing),
                      status = get(something(get(e, "design", nothing), Dict()), "status", nothing))
                   for (i, e) in enumerate(hist)]))
end

# The design in a request body (`design`: a dict, or null to clear), as a (design, nothing) or an
# (nothing, error reply) pair.
function _xd_design_from_body(body)
    raw = get(body, :design, nothing)
    raw === nothing && return nothing, nothing
    raw isa AbstractDict || return nothing, (400, JSON3.write((; error = "design must be an object or null")))
    try
        experiment_design_from(raw), nothing
    catch e
        e isa ErrorException || rethrow()
        nothing, (400, JSON3.write((; error = e.msg)))
    end
end

"""
    GET /api/sets/experiment-design?projectUid&setUid

The set's design, its version and history, and the grid placing its images. Writes nothing.
"""
function api_sets_experiment_design(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    uid = get(q, "projectUid", ""); set_uid = get(q, "setUid", "")
    (isempty(uid) || isempty(set_uid)) && return 400, JSON3.write((; error = "projectUid and setUid required"))
    set = _cohort_set(uid, set_uid)
    set isa Tuple && return set
    d = try
        experiment_design(set)
    catch e
        e isa ErrorException || rethrow()
        return 500, JSON3.write((; error = "The stored design does not read: $(e.msg)"))
    end
    200, _xd_reply(set, d)
end

"""
    POST /api/sets/experiment-design  { projectUid, setUid, design | null, change? }

Save the set's design (null clears it) and record the change in its history. A design that says what
the stored one says records nothing. Reply as GET.
"""
function api_sets_experiment_design_save(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid); set_uid = _wstr(body, :setUid)
    (isempty(project_uid) || isempty(set_uid)) && return 400, JSON3.write((; error = "projectUid and setUid required"))
    set = _cohort_set(project_uid, set_uid)
    set isa Tuple && return set
    d, err = _xd_design_from_body(body)
    err === nothing || return err
    set_experiment_design!(load_project(project_uid), set_uid, d; change = _wstr(body, :change))
    @info "Set experiment design" set = set_uid project = project_uid cleared = d === nothing
    200, _xd_reply(_cohort_set(project_uid, set_uid), d)
end

"""
    POST /api/sets/experiment-design/preview  { projectUid, setUid, design, attrs? }

The grid for a design that is not saved, with `attrs` (`{imageUid: {attribute: value}}`) standing in
for values not written yet — what the design dialog shows before anything is confirmed. Writes
nothing. Reply as GET, with the stored version and history.
"""
function api_sets_experiment_design_preview(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid); set_uid = _wstr(body, :setUid)
    (isempty(project_uid) || isempty(set_uid)) && return 400, JSON3.write((; error = "projectUid and setUid required"))
    set = _cohort_set(project_uid, set_uid)
    set isa Tuple && return set
    d, err = _xd_design_from_body(body)
    err === nothing || return err
    d === nothing && return 400, JSON3.write((; error = "design required"))
    attrs = get(body, :attrs, nothing)
    attrs isa Union{Nothing,AbstractDict} || return 400, JSON3.write((; error = "attrs must be an object"))
    a = Dict{String,Any}(string(k) => (v isa AbstractDict ? Dict{String,Any}(string(kk) => vv for (kk, vv) in v) : Dict{String,Any}())
                         for (k, v) in something(attrs, Dict()))
    200, _xd_reply(set, d; attrs = a)
end
