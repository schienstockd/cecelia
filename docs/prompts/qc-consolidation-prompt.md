# Audit + Plan: QC Single-Source Consolidation + Cohort QC

Opus planning pass. Read the codebase thoroughly before producing a plan. No code yet. Sonnet executes against the plan. Read `qc-observer-relationship.md` alongside this — both prompts are coordinated and must not produce diverging solutions.

---

## The single-source problem

Three consumers need QC warnings but currently draw from different sources:

- **Image table** — reads import warnings from `ccid.json`
- **Whiteboard** — reads from the `qc.jl` store (`write_qc!`)
- **Lab log + MCP/Claude** — reads from `qc.jl` store (partially)

Import warnings (missing physical size, axis labels, time increment, Z-spacing correction) are written inline in `omezarr.jl` to `ccid.json`. They never reach `qc.jl`. The result: a user who imports 20 images with missing calibration sees warnings in the image table only. A user who runs clustering and gets a cohort outlier sees it on the whiteboard only. The lab log sees neither reliably.

**Target:**
```
qc.jl store (single source of truth)
  ├── image table       → per-image traffic light badge (🟢/🟡/🔴)
  ├── whiteboard        → per-node badge on task runs
  ├── lab log           → Cecelia entries with traffic light symbols (see observer-redesign-prompt.md)
  └── MCP/Claude        → cohort-level pattern detection
```

`ccid.json` keeps metadata correction *values* (PhysicalSizeZ, TimeIncrement). Only the warnings about their absence go to `qc.jl`.

---

## Step 1 — Read everything before planning

Read in full:
- `app/src/qc.jl` — `write_qc!`, `read_all_qc`, findings format, storage path `{uid}/qc/{funName}/{valueName}.json`
- `app/src/qc_cohort.jl` — what cohort aggregation already exists, `GET /api/qc/cohort` route
- `app/src/tasks/importImages/omezarr.jl` — how import warnings are currently produced
- Image table Vue component — how it currently reads and displays warnings
- Whiteboard Vue component — how it reads QC badges from `qc.jl`
- `mcp/cecelia_mcp/` — what QC the MCP server currently reads
- `docs/ai-assist/OBSERVER.md` — the validated MCP implementation state (Slices A-C done)

---

## Step 2 — Audit the divergence

Produce a table of every QC finding currently produced anywhere in the codebase:

| Finding | Where produced | Written to | Image table? | Whiteboard? | Lab log? | MCP? |
|---|---|---|---|---|---|---|

Do not guess — read the code.

---

## Step 3 — Import warnings → qc.jl

Which conditions in `omezarr.jl` should produce QC findings (not just log them):
- Missing PhysicalSizeX/Y/Z → warn
- Missing TimeIncrement on timecourse → warn
- ImageJ Z-spacing correction applied → info
- Missing axis labels → warn

Call `write_qc!` at the end of the import task. Severity mapping: warn → 🔴, info → 🟡.

Migration must be atomic: image table stops reading import warnings from `ccid.json` and reads `qc.jl` in the same PR. No gap where warnings disappear.

---

## Step 4 — Cohort QC (new)

This is the most important new capability. Individual-image QC catches "this image has no physical size." Cohort QC catches "this image has 1000 tracks while all others have 200."

**Design: a Cohort QC button on every module page**

A "Check cohort consistency" button on each module page runs cohort QC on the currently selected images for that stage's relevant metrics. Not automatic — user-triggered. The button fires a Julia endpoint that:
1. Loads QC metrics for all selected images at that stage
2. Computes mean ± SD across the cohort for each metric
3. Flags images whose metric is >N SD from the mean (configurable, default 2)
4. Writes outlier findings back through `write_qc!` so they appear in the image table, whiteboard, and lab log

**Metrics by stage (design the right set, don't enumerate exhaustively yet — Opus should propose):**

For each stage, what are the consistency metrics worth checking? Examples:
- *Segmentation*: cell count, mean cell area, segmentation confidence
- *Tracking*: track count, mean track length, fraction tracked
- *Gating*: cells per population as fraction of parent
- *HMM*: state frequency distribution per state

Opus: propose the full metric set for each stage based on what's actually written to `qc.jl` already (from `write_qc!` calls in existing task modules). Don't design metrics that don't have data yet.

**Cohort QC in the whiteboard:**

The whiteboard already badges per-node from `qc.jl`. Cohort outlier findings written via `write_qc!` appear automatically — no new whiteboard work needed if the store is the single source.

**Cohort QC in the lab log:**

After cohort QC runs, Cecelia appends a summary entry to the lab log:
```
## 2026-07-18 [Cecelia — Cohort QC]
🟡 Tracking: image KDIeEm has 1043 tracks (cohort mean 187 ± 42, 2.3 SD above)
🟢 Segmentation: all 20 images within normal range
```

This is a Cecelia entry, not a Claude entry. See `observer-redesign-prompt.md` for the entry format and badge rules.

---

## Step 5 — Plan the implementation

Cover:
- What changes in `qc.jl` / `qc_cohort.jl` (if any) to support import findings and cohort outlier writes
- The API endpoint for the Cohort QC button: `POST /api/qc/cohort/check?projectUid&setUid&funName`
- The image table reads: confirm it reads from `qc.jl` for all findings after migration
- The whiteboard: confirm cohort outlier findings appear via existing badge logic
- The atomic migration plan for import warnings
- What additions to `INVENTORY.md` for the QC store and cohort endpoint

---

## Step 6 — Toast notification convention (establish once, use everywhere)

Cohort QC needs progress and completion feedback. Before implementing anything, search the codebase for existing toast/notification usage — check `INVENTORY.md` and search for `useToast`, `Toast`, snackbar, alert, or notification components. PrimeVue ships `useToast()` which may already be wired.

**If `<Toast />` already exists in `App.vue`:** use it directly. No new component.
**If it doesn't:** add `<Toast />` to `App.vue` once, use `useToast()` from there everywhere. One addition, never a second system.

**Establish a general toast convention** that all modules use going forward — not just cohort QC. Document it in `INVENTORY.md` under "UI conventions." The convention:

| Scenario | Severity | When to use |
|---|---|---|
| Long operation started | `info` | Any task/operation taking >2s |
| Operation completed cleanly | `success` | Optional — only when the user is waiting for it |
| QC warning found | `warn` | When 🟡 findings are written to `qc.jl` |
| Operation failed | `error` | Task failure, API error |

**Cohort QC specifically needs:**
- `info` toast on start: "Checking cohort consistency across N images..."
- `warn` or `success` toast on completion: "🟡 2 images flagged — see lab log" or "🟢 All images within normal range"
- `error` toast if the endpoint fails

**What toast is NOT for:**
- Routine task completions that the task manager already shows
- Every single lab log entry (those badge, not toast)
- Background operations the user didn't initiate

Toast is for foreground operations the user triggered and is actively waiting on. Background scheduler progress belongs in the task manager. This distinction must be in the convention so it doesn't get used for everything.

---



Tight Sonnet prompt with:
- Exact files to change and what to change
- Change order (what must land before what)
- Verification checklist: import warning → image table AND whiteboard AND lab log AND MCP for same image
- Cohort QC verification: outlier flagged → image table badge AND whiteboard badge AND lab log Cecelia entry
- Whether #206-208 need rebasing onto this work

---

## Toast notification convention

Cohort QC is the first feature that needs progress feedback during a longer-running operation (checking consistency across many images). Use this as the opportunity to establish a general toast convention for Cecelia rather than a one-off implementation.

**Before implementing anything:** search the codebase for existing `useToast()` calls, any notification/alert/snackbar components, and whether PrimeVue's `<Toast />` is already in `App.vue`. Check `INVENTORY.md` first. If it exists, use it. Do not introduce a second notification system.

**If not already present:** add `<Toast />` to `App.vue` once, wire `useToast()` as a composable, and use it from there. This is a one-time setup; every subsequent feature that needs a toast just calls `useToast()`.

**Convention for cohort QC (and as the general pattern going forward):**

```
Progress (while running):  severity='info',   "Checking cohort consistency across N images..."
Completion with findings:  severity='warn',   "🟡 N images flagged — see lab log"
Completion all clear:      severity='success', "🟢 Cohort QC complete — all images consistent"
Error:                     severity='error',   "Cohort QC failed — see console"
```

Severity maps directly to the traffic light scale used everywhere else (`info`/🟢, `warn`/🟡, `error`/🔴). One scale, consistent across toasts, badges, lab log entries, and traffic lights.

**Document the toast convention in `INVENTORY.md`** under a new `## UI conventions` section so future features use the same pattern rather than inventing their own. Include: which component, which composable, severity mapping, and when to use toast vs. badge vs. lab log entry:
- **Toast**: transient feedback for operations in progress or just completed. Auto-dismisses.
- **Badge**: persistent indicator that something needs attention (unseen lab log entry, QC warning). Stays until acknowledged.
- **Lab log entry**: durable record. Stays forever. For findings worth remembering across sessions.
- **Traffic light**: per-image summary state in the image table. Always visible, always current.

These four are the complete set of notification surfaces. Nothing else should be introduced without updating `INVENTORY.md` and justifying why none of the four existing surfaces fit.

---

- `qc.jl` is the canonical store — do not design a parallel system
- Cohort QC writes through `write_qc!` — not a separate storage mechanism
- The Cohort QC button is user-triggered, not automatic (see `observer-redesign-prompt.md` for why)
- Cecelia must work without Claude — cohort QC, traffic lights, and lab log entries are Cecelia features that function independently of whether the MCP server is running
