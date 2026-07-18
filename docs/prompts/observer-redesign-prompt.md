# Plan: Observer Redesign — Cecelia as Primary Reporter, Claude as Escalation

Opus planning pass. No code yet. Read `qc-observer-relationship.md` alongside this — both prompts are coordinated. Read `docs/ai-assist/OBSERVER.md` for the validated current state (Slices A-C done). This document proposes changes to the current Watch toggle behaviour and introduces Cecelia as the primary lab log author.

---

## Core principle

**Cecelia must work without Claude.** The lab log, traffic lights, QC badges, and cohort summaries are Cecelia features. Claude is an escalation path and a reasoning layer, not a dependency. Users without Claude Code access should get full value from the QC and reporting system.

---

## What changes from the current implementation

The current Watch toggle (PR #191) auto-fires an observer pass after every task completion (8s debounce). In practice, most task completions have nothing worth flagging. The result: "looks good, continue" responses that cost tokens and add noise. Users stop reading them.

**The proposed change:**

Auto-fire narrows to two triggers only:
1. **Repeated failure** — >3 attempts same image/function (already in `monitor.py`, keep)
2. **`qc_flag_fired`** — when a warn-level QC finding is detected (deferred until `:flagged` node state lands with QC work)

Everything else: Cecelia reports via the lab log. Claude is on-demand.

---

## Escalation ladder

```
🟢 All green      → Cecelia logs (no badge), no Claude
🟡 Warning        → Cecelia logs with badge, no auto-Claude
   Multiple 🟡    → Claude called automatically
🔴 Error/failure  → Cecelia logs with badge, no auto-Claude
   Multiple 🔴    → Claude called automatically
```

"Multiple" = configurable threshold (default: 3 warnings or 2 errors in one session). Opus: propose the right thresholds and whether they should be per-image or per-session.

**Always available (regardless of state):**
- **Ask Claude button** — fires an on-demand observer pass. User clicks when they want reassurance ("is everything ok?"), a summary ("what's happened in the last hour?"), or are stuck. Claude has full MCP context: QC findings, task history, lab log.
- **Chat to Claude button** — launches an external Claude Code session. For when the user wants a full conversation, not just a summary. This is a URL/deep link, not an in-app chat.

---

## Cecelia as lab log author

Cecelia writes to the lab log directly — not through Claude. These are `[Cecelia]` entries, not `[Claude]` entries. The format uses traffic light symbols:

```markdown
## 2026-07-18 [Cecelia]
🟢 segmentation.cellposeMeasure — 20/20 images completed (KDIeEm: 1377 cells, PABXLt: 892 cells...)
🟡 tracking.bayesianTracking — image KDIeEm: 1043 tracks (cohort mean 187 ± 42, flagged by cohort QC)
🔴 cleanupImages.driftCorrect — image PABXLt: failed after 4 attempts (see task log)
```

Green entries are compact — one line per stage, summary counts. Amber and red entries include the specific finding and a pointer to where to look.

**Lab log badge for Cecelia entries:**

Same badge pattern as Claude entries — a Cecelia badge appears in the lab log panel toggle when there are unseen `[Cecelia]` entries with 🟡 or 🔴. Green entries do not badge (they're informational, not actionable). The badge uses a different colour/icon from the Claude sparkles badge so the user can distinguish who wrote it.

**Lab log writes when closed:**

The lab log panel being closed must never suppress a Cecelia entry. This was an issue with Claude entries (PR #191 notes it) — do not repeat it for Cecelia. Cecelia writes to the lab log file via `POST /api/lablog/append` regardless of whether the panel is open. The badge appears when the panel opens.

---

## Traffic light in image table

Each image row in the image table shows a traffic light derived from `read_all_qc(img)`:
- 🟢 No findings, or all info-level
- 🟡 One or more warn-level findings
- 🔴 One or more error-level findings, or repeated failures

This is deterministic. No Claude needed. The QC store is the source (`qc.jl` — see `qc-consolidation-prompt.md`). The traffic light must reflect all findings: import, segmentation, gating, tracking, HMM, cohort outliers — everything in the store.

---

## What Cecelia writes vs. what Claude writes

| Event | Author | Badge | Auto-Claude? |
|---|---|---|---|
| Stage completed, all green | `[Cecelia]` 🟢 | No | No |
| Single warn-level QC finding | `[Cecelia]` 🟡 | Yes | No |
| Multiple warn-level findings | `[Cecelia]` 🟡 | Yes | Yes |
| Single error / failure | `[Cecelia]` 🔴 | Yes | No |
| Multiple errors / repeat failures | `[Cecelia]` 🔴 | Yes | Yes |
| Cohort QC run | `[Cecelia]` (mixed) | If 🟡/🔴 | No |
| User-triggered Ask Claude | `[Claude]` | Yes (sparkles) | N/A |
| Repeat-failure pattern (>3) | `[Claude]` | Yes (sparkles) | Auto |
| `qc_flag_fired` (future) | `[Claude]` | Yes (sparkles) | Auto |

---

## Implementation plan for Opus to produce

Cover:
1. The Cecelia lab log writer — where it lives (Julia API post-task hook?), when it fires, what it writes per stage/outcome
2. The `[Cecelia]` entry format — exact markdown, traffic light symbols, compact green vs. verbose amber/red
3. The Cecelia badge — how it differs from the Claude sparkles badge, when it appears/clears
4. Lab-log-closed safety — confirm writes go through `POST /api/lablog/append` regardless of panel state; how the backend badge counter works for Cecelia entries
5. The Watch toggle redesign — what "Watch" means now (narrowed triggers), UI label change if needed
6. The Ask Claude button — where it lives, what context it passes (same as current Watch pass), how it differs from the Chat to Claude button
7. The Chat to Claude button — what it opens (deep link to Claude Code with project context?), how it passes context
8. The escalation thresholds — proposed values for "multiple warnings → call Claude", how they're configured
9. Changes to `OBSERVER.md` reflecting the new design

---

## Constraints

- Cecelia entries and Claude entries go to the same lab log file — same append mechanism, different author tags
- Traffic lights computed from `qc.jl` store only — no parallel data source
- Claude is optional throughout — every Cecelia feature works without the MCP server running
- Do not remove the Ask Claude button or the manual Watch toggle — users who want Claude after every task can still enable it
