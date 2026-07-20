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
`{projectUid, projectName, imageCount, flagged, recentLabLog}`.

- `flagged`: images carrying a warn/fail QC finding — SAME source as the image table (`all_qc_docs`),
  each `{uid, name, worst, findings: [{level, short}]}` (findings capped per image).
- `recentLabLog`: lab-log entries from the last `recent_days` days, newest-first,
  `{date, author, summary}` (summary = the entry's first bullet).

Read-only; a compact orientation, not a full report.
"""
function session_briefing(proj::CciaProject; recent_days::Int = 7, max_entries::Int = 20)
    imgs = images(proj)
    flagged = Any[]
    for img in imgs
        picked = Any[]
        worst = "ok"
        for (_, doc) in all_qc_docs(img)
            for f in something(_briefing_field(doc, "findings"), ())
                lvl = string(something(_briefing_field(f, "level"), "ok"))
                (lvl == "warn" || lvl == "fail") || continue
                lvl == "fail" && (worst = "fail")
                (lvl == "warn" && worst == "ok") && (worst = "warn")
                length(picked) < 5 &&
                    push!(picked, (; level = lvl,
                                     short = string(something(_briefing_field(f, "short"), ""))))
            end
        end
        isempty(picked) && continue
        push!(flagged, (; uid = img.uid, name = img.name, worst = worst, findings = picked))
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

    (; projectUid = proj.uid, projectName = proj.name, imageCount = length(imgs),
       flagged = flagged, recentLabLog = recent)
end
