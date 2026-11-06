# Claim recital routes — the mechanical checks (S0 record, S1 consistency, S2 conventions) for a claim's
# scope over a set. Read-only: nothing is written. Plan: docs/todo/IMAGE_ANALYSIS_RECITAL_PLAN.md.

# The scope from query params, or a (400, body) tuple. `pops` is comma-separated pop paths
# ("B/qc,T/qc"); `measure` a measure column ("live.track.speed"); `unitAttr` the image attribute that
# names the statistical unit ("Mouse"), optional.
function _claim_scope_from_query(q::AbstractDict)
    project_uid = get(q, "projectUid", ""); set_uid = get(q, "setUid", "")
    pops = _query_list(q, "pops")
    measure = strip(get(q, "measure", ""))
    (isempty(project_uid) || isempty(set_uid) || isempty(pops) || isempty(measure)) &&
        return (400, JSON3.write((; error = "projectUid, setUid, pops and measure required")))
    set = _cohort_set(project_uid, set_uid)
    set isa Tuple && return set
    ClaimScope(set, pops, measure; unit_attr = strip(get(q, "unitAttr", "")))
end

# GET /api/claim/checks?projectUid&setUid&pops&measure[&unitAttr][&rows=1]
# → {findings, ran, skipped, notCompared, nRows, nUsed[, rows]}. `rows` (the per-(image, population)
# record table the rules read) only on request: it carries every flattened param and is large.
function api_claim_checks(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    scope = _claim_scope_from_query(q)
    scope isa Tuple && return scope
    out = try
        claim_checks(scope)
    catch e
        return 500, JSON3.write((; error = sprint(showerror, e)))
    end
    get(q, "rows", "") in ("1", "true") || delete!(out, "rows")
    200, JSON3.write(out)
end
