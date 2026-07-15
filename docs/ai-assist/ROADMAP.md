# AI Assistant Integration Roadmap

This document tracks the phased rollout of Claude as a scientific peer reviewer in Cecelia. It is a living document — updated at each check-in. Read `OBSERVER.md`, `QC-PROCESS.md`, and `LAB-LOG.md` in this folder for the design detail behind each phase.

The goal is not convenience. It is a collaborator that catches what the user would miss — the person who used to sit at the image analysis computer and notice things.

---

## Phase 1 — Observer (current phase)

**Goal**: Claude sits next to the user. Watches what's happening. Builds context. Stays silent unless something is worth saying.

**What gets built:**
- Read-only MCP server exposing project state, task logs, image notes, QC metrics
- Lab log: per-project append-only file, written by human and Claude
- Observer session: event-driven chat window in Cecelia Vue frontend
- Lab log Vue panel: new Claude entries visually prominent, user input field, correction template

**Definition of done:**
- [ ] MCP server connects, Claude describes project state without being told
- [ ] Task failure pattern (>3 attempts same image) surfaces in observer session without being asked
- [ ] User adds image note → Claude incorporates in session context
- [ ] Lab log appends correctly with `[Claude]` / `[User]` tags and dates
- [ ] Token cost measured across 3 real analysis sessions and within acceptable range
- [ ] Vue lab log panel shows new Claude entries prominently

**Check-in criteria before Phase 2:**
- Lab log has accumulated entries from at least 10 real analysis sessions
- Claude's observations are more signal than noise — user finds them useful, not annoying
- Token cost per session is known and acceptable
- At least one instance where Claude caught something the user would have missed
- At least one instance where Claude's observation was wrong and the correction was recorded

**Do not start Phase 2 until all check-in criteria are met.**

---

## Phase 2 — Designer (deferred)

**Goal**: Claude proposes analysis strategies, suggests parameter adjustments, flags before things go wrong rather than after.

**Depends on:**
- Phase 1 check-in criteria met
- Chain scheduler and population manager stable (API not changing)
- Lab log rich enough that Claude's sense of "normal" is calibrated for at least one cohort

**What gets built:**
- Write-capable MCP: task submission, parameter adjustment, flag acknowledgement — all with human confirmation
- Hold state in task scheduler: `flagged` state, per-image pause, downstream blocking in Collaborative mode
- Cohort statistics: computed after all images complete a stage, accessible via MCP
- Parameter adjustment branch runs: proposed alternative, side-by-side results, user accepts one

**Check-in criteria before Phase 3:**
- Hold state working correctly: flagged images pause, others continue
- Parameter adjustment proposals are concrete and within spec bounds
- Cohort statistics compute at the right time (cohort-complete definition settled)
- At least one full experiment run in Advisory mode with useful flag output
- Morning summary format validated: objective state, no conclusions, actionable
- "Dry run" mode validated: thresholds tuned on subset before overnight run

**Do not start Phase 3 until all check-in criteria are met.**

---

## Phase 3 — Analyst (deferred)

**Goal**: The overnight analyst. User describes the experiment and goes home. Claude runs the pipeline, flags edge cases, writes the morning summary. User arrives to results, not a blank screen.

**Depends on:**
- Phase 2 check-in criteria met
- Hold state proven reliable across multiple experiments
- Lab log has cross-experiment history for at least 2-3 cohort types
- Token cost at Phase 2 scale measured and acceptable

**What gets built:**
- Collaborative mode fully operational with configurable checkpoints
- Cross-experiment pattern recognition: lab log cross-reference at flag time
- Morning summary: structured evidence only, no biological conclusions
- The why question: triggers on unexpected decisions, captures user rationale in lab log

**Honest reservation built into this phase:**
The overnight analyst will occasionally produce a result that looks clean in the morning summary but has a subtle flaw a 30-second human glance would have caught. This is not a bug to fix — it is a fundamental limitation of autonomous analysis. The hold state and morning summary exist to minimise this risk, not eliminate it. Scientific validation remains the user's responsibility.

---

## Check-in schedule

Aligned with the existing release schedule. At each scheduled GitHub release check-in, also review this roadmap:

- **Is Phase 1 done?** → review check-in criteria, decide whether to begin Phase 2
- **Is Phase 2 done?** → review check-in criteria, decide whether to begin Phase 3
- **Is the lab log accumulating useful entries?** → review recent entries, adjust observer sensitivity if needed
- **Is token cost acceptable?** → review session costs, adjust throttle if needed

The roadmap is updated at each check-in. If a phase is taking longer than expected, document why — don't silently extend without acknowledging the delay.

---

## Files in this folder

```
docs/ai-assist/
  ROADMAP.md          ← this file
  OBSERVER.md         ← MCP server and observer session design
  QC-PROCESS.md       ← QC flags, hold state, parameter proposals, morning summary
  LAB-LOG.md          ← lab log format, MCP tools, Vue panel, versioning
```

---

## What this is not

- A feature list to ship as fast as possible
- A replacement for scientific judgment
- A system that draws conclusions — that is always the user's job

The value compounds over time. Phase 1 is not impressive on day one. It becomes impressive when the lab log has six months of entries and Claude cross-references a current deviation against something it saw in March. Build slowly. Measure. Don't skip phases.

---

## The lab log has standalone value — start it now

The lab log does not require the MCP server, the observer session, or any AI integration to be useful. It solves a problem that predates AI: image analysis methodology gets lost.

Most analytical decisions live in the analyst's head and disappear when they leave the lab. The person who figured out that the CD4 gate runs loose on spleen tissue, that mouse 7 had Z-drift in the late timepoints, that HMM with 3 states fits this cohort better than 2 — that knowledge either gets passed down informally or gets rediscovered by the next person who spends two days confused.

Nobody keeps an image analysis notebook because you try many things and take the one that seems to work. The lab log is not a notebook — it is append-only, low friction, and lives next to the data. One line after a decision is enough.

**Start using the lab log before Phase 1 is built.** The file format is plain markdown. Nothing prevents creating `lab-log.md` in the project directory today and writing to it manually. When the MCP server arrives, Claude reads what's already there. When the Vue panel arrives, it surfaces what's already there. The AI integration makes the lab log more useful — it does not make the lab log possible.

**The Vue panel is load-bearing for adoption.** Nobody will persistently edit a file on the side. The panel must be a persistent slide-out accessible from any module page, with a pre-filled context-aware entry field that just needs the reason. Zero navigation, zero friction. If opening the lab log requires leaving the current page, people will stop using it within a week. The GUI design is specified in `LAB-LOG.md` — treat it as a first-class feature, not an afterthought.

When handing off a project, returning to it after six months, or trying to reproduce a result, the lab log is the thing that makes it possible to understand not just what was done but why. That value exists regardless of whether Claude ever reads it.
