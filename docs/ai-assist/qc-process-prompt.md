# Phase: Claude QC Integration — Scientific Peer Reviewer

Design phase for Opus. Read `mcp-readonly-prompt.md` and `lab-log-prompt.md` alongside this. The three documents form one system. This document defines what Claude does with the data the MCP server provides.

---

## The distinction that matters

**Convenience** (not the goal): Claude tells you what's in your project, summarises what ran, produces plots. The whiteboard, task manager, and analysis canvas already do this.

**Value** (the goal): Claude functions as the person who used to sit at the image analysis computer and notice things. "You've tried this 10 times with the same outcome. Maybe try X." "From my reasoning I wouldn't have done it that way — but the outcome looks great. Why did you do that?" The answer to that second question is exactly what gets lost in every lab and exactly what the lab log captures.

---

## Three operating modes

Stored in project config, not the lab log. User switches at any time.

**Silent**: Claude observes, writes to lab log, never interrupts. For routine runs the user is confident in. Claude is building context for future sessions.

**Advisory**: Claude surfaces observations in the chat window when the pattern is worth naming. Never pauses the pipeline. "Image 7 gated to 23 cells — 8x below cohort mean. Continuing." User acts or ignores.

**Collaborative**: Claude pauses at configurable checkpoints and requires acknowledgement. "3 images flagged at segmentation. Review before tracking?" For overnight runs — user sets checkpoints before leaving, reviews flags in the morning.

---

## What Claude flags

Flags are statistical, not interpretive. Claude says "this gate produced 23 cells, 8x below the cohort mean of 187." Not "this gate is wrong." Interpretation is the user's job.

**Per-stage QC metrics:**

*Segmentation*:
- Cell count per image vs. cohort mean
- Mean Cellpose confidence (if available)
- Fraction of Z-slices with low cell count (Z-drift indicator)
- Label size distribution — flag bimodal when unimodal expected

*Gating*:
- Cells per population as fraction of parent — flag <5% or >95%
- Absolute count <50 cells
- Count >3 SD from cohort mean for same population

*Tracking*:
- Track length distribution — flag bimodal when unimodal expected
- Fraction of cells tracked vs. segmented — flag <30%

*HMM/behaviour*:
- State frequency per image vs. cohort — flag >2 SD deviation
- Single state >95% dominant in one image but not others

**Cohort statistics**: computed after all images complete a stage. Per-image flags that depend on cohort context are deferred until the cohort is complete — Claude needs the full set to know what's an outlier.

**Cross-experiment memory**: if the lab log contains prior experiment records, Claude cross-references. "This pattern (low cell count + bimodal track lengths) appeared in experiment X on 2026-03-12 — root cause was Z-drift. Check Z-stack integrity."

**Repeat attempt pattern**: if the same function has been run >3 times on the same image — regardless of outcome — Claude surfaces it. This is the most reliable signal that something is wrong that the user hasn't named yet.

---

## The hold state

A task can currently run, fail, or complete. There is no "completed but flagged, awaiting decision." This is the most important missing piece.

Design a `flagged` task state:
- Task completed successfully — output is valid
- QC check produced a flag above threshold
- In Collaborative mode: downstream stages for this image pause pending acknowledgement
- In Advisory mode: downstream stages proceed, flag surfaced but not blocking
- Hold is per-image, not per-chain — image 7 held does not block images 1-6

User actions on a flagged image:
- Acknowledge and proceed
- Acknowledge and exclude (logged to lab log)
- Acknowledge and re-run with adjusted params

---

## Parameter adjustment proposals

When an image is flagged, Claude proposes a specific concrete alternative — not a general suggestion:

"Standard threshold 0.3 → 23 cells. Proposed: threshold 0.2 → estimated ~180 cells (based on cohort distribution). Run alternative?"

If accepted: branch run with adjusted params, results presented side by side. User accepts one branch, other discarded. Accepted params and rationale logged.

Parameter search space is bounded — only within the range defined in the module JSON spec.

---

## The why question

When a user does something unexpected — parameter deviation from prior runs, an unusual image note, a decision that contradicts lab log entries — Claude asks why in the chat window. Not to judge. To capture methodology.

"You set the HMM to 4 states here but the lab log notes that 3 states worked well for this cohort. What are you trying to capture with the additional state?"

The user's answer goes in the lab log as a `[User]` entry. This is how domain knowledge stops getting lost.

---

## Morning summary format

For Collaborative/overnight mode. Objective state, no conclusions:

```
COMPLETED CLEANLY (n images)
  → ready for next stage

COMPLETED WITH FLAGS (n images)
  Image 7: gate produced 23 cells (cohort mean 187) — awaiting acknowledgement
  Image 12: track fraction 18% (cohort mean 67%) — awaiting acknowledgement

PAUSED — REQUIRES DECISION (n images)
  Image 3: segmentation attempt 8, consistent low confidence — recommend parameter review

FAILED (n images)
  Image 15: btrack OOM error — see task log
```

No biological conclusions. No "there is a significant difference." The biologist reads the summary and draws the conclusion.

---

## What Claude must NOT do

- Draw biological conclusions
- Recommend exclusion without quantitative justification
- Adjust parameters without user approval in any mode
- Write speculative entries to the lab log
- Flag things inferable from a single image without cohort context
- Surface observations in Silent mode

## Honest reservations

**The why question will misfire early.** The why-question trigger requires Claude to have a model of what's expected — built from the lab log and prior runs. Early in a project, before the lab log is rich, Claude's sense of "unexpected" is poorly calibrated. It may ask obvious questions or miss genuinely unusual decisions. Document this for users: the observer becomes more useful over time, not immediately.

**"Cohort complete" needs a definition.** Per-image flags that depend on cohort statistics are deferred until the cohort is complete — but images get added, excluded, and re-processed. Define what "cohort complete" means: all images that have passed a given stage, or an explicit user declaration. Ambiguity here means cohort flags either fire too early (incomplete cohort) or never fire (waiting for a declaration that never comes). Recommend: cohort stats computed automatically once all currently-included images reach a stage, recomputed when images are added or excluded.

**Overnight calibration.** Before running unsupervised overnight, the user should run a supervised session on a subset of images to calibrate flag thresholds. If too many images hit the hold state overnight, the morning summary becomes a wall of decisions rather than a result — worse than no overnight run. Recommend a "dry run" mode: run the pipeline with all QC checks active but hold states advisory only, so the user can tune thresholds before committing to an unattended run.

---

## Lab log entries from QC

Claude appends when:
- A flag fires and user acknowledges — what was flagged, what action taken
- A parameter adjustment is accepted — original, adjusted, outcome
- An image is excluded — which image, metric, who decided
- A cross-experiment pattern recognised — what matched, prior root cause
- A why question is answered — the user's stated rationale

Claude does NOT append for routine completions with no flags.

---

## Implementation plan

Opus: produce a numbered step plan covering:

1. The `flagged` task state — where it lives in the scheduler, how it interacts with the chain pipeline
2. Per-stage QC metric computation — where it runs (Julia, post-task), what it writes
3. Cohort statistics computation — timing, storage, MCP access
4. Operating mode storage and enforcement — how mode gates Claude's behaviour
5. Parameter adjustment branch run — how proposed, how branch executes, how results compared
6. The why question trigger — what constitutes "unexpected" that triggers it, how answer is captured
7. Morning summary generation — trigger, format, surfaced where (lab log + Vue notification)
8. Event notification design — what events the MCP server pushes, payload per event, how Claude decides to speak vs. stay silent
9. Dependencies on framework components not yet built — hold state, branch runs, cohort stats — and build order
