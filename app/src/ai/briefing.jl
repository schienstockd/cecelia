# Observer SESSION BRIEFING (Observer Phase 2 §2) — the small, live context a fresh Chat-to-Claude
# session pulls first (via the get_session_briefing MCP tool) so the user need not re-explain: project
# name + image count, the images currently flagged (⚠️/❌ QC), and recent lab-log entries. Read-only,
# project-level. Deliberately compact — it orients the session, it is not a full report.

# Robust field read across JSON3.Object (Symbol keys — from read_all_qc's parsed sidecars) and plain
# Dict (String keys — the computed calibration fallback / qc_finding). Returns nothing when absent.
_briefing_field(x, k::AbstractString) =
    (v = get(x, Symbol(k), nothing); v === nothing ? get(x, k, nothing) : v)

"""
    session_briefing(proj; recent_days=7, max_entries=20) -> NamedTuple

Startup context for an observer chat session:
`{projectUid, projectName, imageCount, excludedCount, flagged, recentLabLog}`.

- `flagged`: images carrying a warn/fail QC finding — SAME source as the image table (`all_qc_docs`),
  each `{uid, name, worst, included, findings: [{level, short, fun}]}` (findings capped per image).
- `excludedCount`: how many of `imageCount` are EXCLUDED from analysis. An excluded image still sits
  in its set as "done", so a reader that treats `imageCount` as the cohort size is over by this many.
- `recentLabLog`: lab-log entries from the last `recent_days` days, newest-first,
  `{date, author, summary}` (summary = the entry's first bullet).

`included`/`excludedCount` are here because a flagged image the user has already EXCLUDED is not
news — the first session on a real project opened by highlighting a drift anomaly on an image its
owner had dropped weeks earlier, which reads as "your data is broken" instead of "you already handled
this". Excluded images are still listed (a warn on one is information, and hiding it would make the
count disagree with the image table); they are LABELLED so a session can lead with the ones that
count. Each finding also carries its `fun`, so a reading session can tell which task's QC is talking —
a probe or example module banking findings is otherwise indistinguishable from segmentation's.

Read-only; a compact orientation, not a full report.
"""
function session_briefing(proj::CciaProject; recent_days::Int = 7, max_entries::Int = 20)
    imgs = images(proj)
    flagged = Any[]
    excluded = 0
    for img in imgs
        image_included(img) || (excluded += 1)
        picked = Any[]
        worst = "ok"
        for (key, doc) in all_qc_docs(img)
            # WHICH task's QC is talking. The doc carries `funName`; the key ("<fun>/<valueName>") is
            # the fallback. Without it every finding reads as if the pipeline produced it, and a probe
            # or example module banking a hardcoded threshold is indistinguishable from segmentation —
            # which cost a whole session chasing "4 images measured 0 cells" that no segmentation ran.
            fun = string(something(_briefing_field(doc, "funName"), first(split(string(key), "/"))))
            for f in something(_briefing_field(doc, "findings"), ())
                lvl = string(something(_briefing_field(f, "level"), "ok"))
                (lvl == "warn" || lvl == "fail") || continue
                lvl == "fail" && (worst = "fail")
                (lvl == "warn" && worst == "ok") && (worst = "warn")
                length(picked) < 5 &&
                    push!(picked, (; level = lvl,
                                     short = string(something(_briefing_field(f, "short"), "")),
                                     fun = fun))
            end
        end
        isempty(picked) && continue
        # `included` on every entry, not only the excluded ones: an ABSENT field reads as "unknown", and
        # a session deciding what to lead with should not have to infer it.
        push!(flagged, (; uid = img.uid, name = img.name, worst = worst,
                          included = image_included(img), findings = picked))
    end

    recent = Any[]
    cutoff = Dates.today() - Dates.Day(recent_days)
    for e in parse_lab_log(read_lab_log(proj))   # newest-first
        d = tryparse(Dates.Date, string(get(e, "date", "")))
        (d === nothing || d < cutoff) && continue
        lines = get(e, "lines", String[])
        push!(recent, (; date = e["date"], author = e["author"],
                         summary = isempty(lines) ? "" : first(lines)))
        length(recent) >= max_entries && break
    end

    # LabArchives context, when the project has any — headings + gaps only (ai/labarchives.jl). The
    # key is OMITTED rather than sent empty, so "no ELN link" and "linked but nothing to say" stay
    # distinguishable to a reading session.
    la = la_briefing(proj)

    base = (; projectUid = proj.uid, projectName = proj.name, imageCount = length(imgs),
              excludedCount = excluded, flagged = flagged, recentLabLog = recent)
    la === nothing ? base : merge(base, (; labarchives = la))
end
