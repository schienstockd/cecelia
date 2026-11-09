# ── Model review turns — the pieces the claim review and the acquisition review share ────────────────
#
# A "review" is ONE Kiwi turn (`kiwi_start_turn`) that reads a Blackboard entry's subject — a claim
# (claim_review.jl) or a set's acquisition plan (acq_review.jl) — with its own system prompt and a
# narrower tool list, and writes its points back onto that entry as `### sNN · …` sections under a
# `### <Category>` heading, which the user marks Good / Bad / Unsure. Advisory: the points are
# `inferred`, and never change what the rule checks found.
#
# The stored shape (the entry meta's `review`, under its `claim` / `acqCheck` key):
#
#   review = {turnId, at, status, model, abstain, note, lastError?,
#             claims: [{kind, category, text, action, refs, sectionId} | {kind: "interpretation", text, refs}]}

# A review point's text → `(category, text, action)`: the leading `<Category>:` names one of
# `categories` by its `label(id)` (else `other`, text kept whole), the trailing `→ …` is what to do
# ("" if absent).
function _review_point(text::AbstractString, categories, label::Function)
    t = strip(text); cat = "other"
    m = match(r"^([^:]{2,40}):\s*(.+)$"s, t)
    if m !== nothing
        hit = findfirst(c -> lowercase(label(c)) == lowercase(strip(m[1])), categories)
        hit === nothing || (cat = categories[hit]; t = strip(m[2]))
    end
    parts = split(t, r"\s*(→|->)\s*"; limit = 2)
    # the category was the sentence's start, so what's left starts mid-sentence ("on image 2 T is…")
    (cat, uppercasefirst(String(parts[1])), length(parts) == 2 ? uppercasefirst(String(parts[2])) : "")
end

# One reply claim → a review point: one line, filed under its category, the action split off.
function _review_item(x, categories, label::Function)
    kind = string(get(x, "kind", ""))
    text = replace(strip(string(get(x, "text", ""))), r"\s*\n\s*" => " ")
    # the refs as Kiwi checked them (`{ref, result, seen}`), so the point links to what it is about
    # the way any Kiwi claim does — the popover's chips and the entry's `kiwiRefs`
    refs = Any[r for r in get(x, "refs", Any[]) if r isa AbstractDict && get(r, "ref", nothing) isa AbstractDict]
    kind == "interpretation" && return Dict{String,Any}("kind" => kind, "text" => text, "refs" => refs)
    cat, t, act = _review_point(text, categories, label)
    Dict{String,Any}("kind" => kind, "category" => cat, "text" => t, "action" => act, "refs" => refs)
end

"""
    _review_target(rec, extra_key, meta_key) -> (uid, id, subject, req) | nothing

The entry a finished review turn writes onto: `rec[extra_key]` (`{entryId, round}`, set when the turn
started) names it, `meta[meta_key]` is its subject (a copy). `nothing` when the entry or its subject is
gone, or the subject has moved to another round — a review of an earlier round answers the wrong question.
"""
function _review_target(rec::AbstractDict, extra_key::AbstractString, meta_key::AbstractString)
    req = get(rec, extra_key, nothing)
    req isa AbstractDict || return nothing
    uid, id = String(rec["projectUid"]), String(req["entryId"])
    meta = _read_bb_meta(uid, id)
    (meta === nothing || !(get(meta, meta_key, nothing) isa AbstractDict)) && return nothing
    subj = Dict{String,Any}(String(k) => v for (k, v) in meta[meta_key])
    get(subj, "round", 1) == get(req, "round", 1) || return nothing
    (uid, id, subj, req)
end

"""
    _review_from_turn(rec, prev; categories, label) -> Dict

The review a finished Kiwi turn record leaves. A turn that didn't finish (usage limit, no CLI,
cancelled) is not "nothing found": it keeps `prev` (the last review that did finish) and says why in
`lastError`. A finished one is the new review, its points filed under `categories`.
"""
function _review_from_turn(rec::AbstractDict, prev; categories, label::Function)::Dict{String,Any}
    reply = get(rec, "reply", nothing)
    if rec["status"] != "done"
        errs = reply isa AbstractDict ? get(reply, "errors", Any[]) : Any[]
        why = !isempty(errs) ? string(first(errs)) : string(get(rec, "error", rec["status"]))
        rv = prev isa AbstractDict ? Dict{String,Any}(String(k) => v for (k, v) in prev) :
             Dict{String,Any}("status" => rec["status"], "model" => get(rec, "model", ""), "claims" => Any[],
                              "abstain" => false, "note" => "", "turnId" => rec["turnId"])
        rv["lastError"] = replace(strip(why), r"\s*\n\s*" => " ")
        return rv
    end
    claims = reply isa AbstractDict ? collect(get(reply, "claims", Any[])) : Any[]
    Dict{String,Any}(
        "turnId" => rec["turnId"], "at" => get(rec, "finishedAt", string(Dates.now())),
        "status" => rec["status"], "model" => get(rec, "model", ""),
        "abstain" => reply isa AbstractDict && get(reply, "abstain", false) == true,
        "note" => reply isa AbstractDict ? string(get(reply, "note", "")) : "",
        # one line each: a point's text becomes a `### sNN` heading
        "claims" => [_review_item(x, categories, label) for x in claims])
end

# The section id for `key`: the one it had, else the next number after every id the entry has used.
# Claim findings and review points draw from one counter per entry (`findingIds`), so ids never collide.
function _entry_section_id!(ids::AbstractDict, key::AbstractString)::String
    haskey(ids, key) && return String(ids[key])
    n = maximum((parse(Int, String(v)[2:end]) for v in values(ids)); init = 0)
    ids[key] = "s" * lpad(n + 1, 2, '0')
end

# Review points get section ids keyed by their text: a re-run that finds the same thing keeps the id
# (and its verdict); a new wording is a new section.
function _review_assign_ids!(rv, ids::AbstractDict)
    rv isa AbstractDict || return ids
    for x in get(rv, "claims", Any[])
        x["kind"] == "interpretation" && continue
        x["sectionId"] = _entry_section_id!(ids, "review|" * x["text"])
    end
    ids
end

# A review point's refs as text, labelled as Kiwi resolved them — the line a saved Kiwi turn has under
# each claim (`kiwiTurnSave.ts` `claimBullet`); the clickable chips are the entry's `kiwiRefs`.
_review_ref_labels(x) = unique!(String[string(get(r["result"], "label", ""))
                                       for r in get(x, "refs", Any[]) if get(r, "result", nothing) isa AbstractDict &&
                                       !isempty(string(get(r["result"], "label", "")))])

"""
    _review_md!(io, rv; categories, label, intro, nothing_more, supports_heading)

The entry's `## Model review` block: "Not run." without a review; else the last failure (if any), what
the turn read (`intro`), its points as `### sNN · text → action` sections under a `### <Category>`
heading (not a section — it has no id — so it closes the previous point's section), and the one
interpretation under `supports_heading` (`nothing`: the caller writes it, as the acquisition verdict does).
"""
function _review_md!(io::IO, rv; categories, label::Function, intro::AbstractString,
                     nothing_more::AbstractString, supports_heading::Union{AbstractString,Nothing})
    println(io, "## Model review\n")
    if !(rv isa AbstractDict)
        println(io, "Not run.\n")
        return io
    end
    err = get(rv, "lastError", "")
    isempty(err) || println(io, "⚠ The last run failed: ", err, "\n")
    done = get(rv, "status", "") == "done"
    done && println(io, "One ", isempty(get(rv, "model", "")) ? "" : rv["model"] * " ",
                    "turn that ", intro, " Mark each point Good, Bad or Unsure.\n")
    rs = filter(x -> x["kind"] != "interpretation", rv["claims"])
    done && isempty(rs) && println(io, get(rv, "abstain", false) ? "Nothing raised. " * get(rv, "note", "") : nothing_more, "\n")
    cat_of(x) = (k = get(x, "category", "other"); k in categories ? k : "other")
    for cat in categories
        xs = filter(x -> cat_of(x) == cat, rs)
        isempty(xs) && continue
        println(io, "### ", label(cat), "\n")
        for x in xs
            act = get(x, "action", "")
            println(io, "### ", x["sectionId"], " · ", x["text"], isempty(act) ? "" : " → " * act, "\n")
            ls = _review_ref_labels(x)
            isempty(ls) || println(io, "_refs:_ ", join(("`" * l * "`" for l in ls), ", "), "\n")
        end
    end
    supports_heading === nothing && return io   # the caller writes the interpretation itself
    for x in filter(x -> x["kind"] == "interpretation", rv["claims"])
        println(io, "## ", supports_heading, "\n\n", x["text"], "\n")
    end
    io
end

# The review's refs as the entry's `kiwiRefs` sidecar, which the Blackboard renders as clickable chips:
# `{key → {ref, label, savedAt}}`, one per distinct ref, labelled as Kiwi resolved it. The key follows
# `refKey` (`kiwiTurn.ts`). Unlike a saved Kiwi turn (`kiwiTurnSave.ts`) it keeps every ref kind and
# no `snapshot` (optional in the sidecar): a gone target shows "was: <label>". No review, no refs.
function _review_kiwi_refs(rv)::Dict{String,Any}
    out = Dict{String,Any}()
    rv isa AbstractDict || return out
    at = string(get(rv, "at", ""))
    for x in get(rv, "claims", Any[]), r in get(x, "refs", Any[])
        ref = get(r, "ref", nothing)
        ref isa AbstractDict || continue
        key = JSON3.write([[String(k), ref[k]] for k in sort!(collect(keys(ref)); by = string)])
        haskey(out, key) && continue
        res = get(r, "result", nothing)
        label = res isa AbstractDict ? string(get(res, "label", "")) : ""
        out[key] = Dict{String,Any}("ref" => ref, "label" => label, "savedAt" => at)
    end
    out
end

# Write the review's refs onto the entry as its `kiwiRefs` sidecar, with the create path's guards (a
# review has ≤ KIWI_MAX_CLAIMS points, so the size cap is a backstop): over it, the entry keeps the
# `_refs:_` lines and loses only the chips. No refs clears the sidecar.
function _set_review_kiwi_refs!(uid::AbstractString, id::AbstractString, rv)
    kr = _review_kiwi_refs(rv)
    ok = !isempty(kr) && _valid_bb_kiwi_refs(kr) && length(codeunits(JSON3.write(kr))) <= _BB_KIWI_REFS_MAX_BYTES
    _set_bb_meta_field!(uid, id, "kiwiRefs", ok ? kr : nothing)
end
