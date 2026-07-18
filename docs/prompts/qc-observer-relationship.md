# Coordination: QC Consolidation + Observer Redesign

Read this before starting either `qc-consolidation-prompt.md` or `observer-redesign-prompt.md`. Both prompts address overlapping systems. This document defines the boundary between them, the shared components they must not duplicate, and the order in which Opus should tackle them.

---

## Why two prompts, one coordination doc

The QC consolidation and the observer redesign share:
- `qc.jl` store as the single source of truth
- The lab log as the output surface
- The image table as a consumer of QC data
- The badge/notification system

If built independently without coordination, they will drift into duplicate solutions — two badge systems, two lab log write paths, two definitions of what "warn-level" means. This document prevents that.

---

## The boundary

**`qc-consolidation-prompt.md` owns:**
- What gets written to `qc.jl` and when (import warnings, task QC, cohort outliers)
- The `qc.jl` store format and access pattern
- The Cohort QC button and its endpoint
- How the image table reads from `qc.jl`
- How the whiteboard reads from `qc.jl`

**`observer-redesign-prompt.md` owns:**
- How Cecelia writes to the lab log based on QC findings from `qc.jl`
- The `[Cecelia]` vs `[Claude]` entry format and badge rules
- The escalation ladder (when Claude is called automatically)
- The Watch toggle redesign
- The Ask Claude and Chat to Claude buttons
- The traffic light display in the image table (reads from `qc.jl`, renders the light)

**Shared, must be consistent:**
- Severity mapping: `info` → 🟢, `warn` → 🟡, `error`/failure → 🔴. One definition, used everywhere.
- `POST /api/lablog/append` — the single write path for all lab log entries, Cecelia and Claude alike. `qc-consolidation-prompt.md` does not add a second write path.
- Badge state for Cecelia entries — tracked the same way as Claude badge state, different icon/colour. One mechanism, two sources.

---

## The order Opus should tackle them

**Do QC consolidation first.** The observer redesign depends on `qc.jl` being the single source. If import warnings are still in `ccid.json` when the observer redesign lands, Cecelia's lab log entries will be incomplete (they'll miss import findings). The consolidation is the foundation.

Specifically, before observer redesign starts:
- Import warnings must write to `qc.jl` ✓
- Image table must read from `qc.jl` for all findings ✓
- Cohort QC must write through `write_qc!` ✓

**Then observer redesign.** Cecelia's lab log author reads from `read_all_qc` — which by this point reflects all findings including import and cohort. The traffic light in the image table is a render of what `read_all_qc` returns, not a separate data source.

---

## What must not happen

- Two badge systems: one for Claude entries, one for Cecelia entries, with different underlying mechanisms. One badge mechanism, two author tags, two icons.
- Two lab log write paths: one for Claude via MCP `append_lab_log`, one for Cecelia via a different route. One route (`POST /api/lablog/append`), used by both.
- Two definitions of severity: `qc.jl` using one scale, the lab log entries using another. The severity in `qc.jl` findings maps directly to the traffic light symbol in the lab log entry.
- Cohort QC as a separate store from `qc.jl`. Cohort outlier findings write through `write_qc!` and live in `{uid}/qc/qc_cohort/{funName}.json` or similar — in the same directory structure, readable by `read_all_qc`.

---

## How to verify coordination

At the end of both implementations, run this end-to-end test:

1. Import an image with missing physical size
2. Run segmentation, tracking, gating
3. Run Cohort QC on the image set
4. Force a repeated failure on one image

Then check:
- Image table: shows correct traffic light for each image (all findings, all stages)
- Whiteboard: badges on affected nodes
- Lab log: Cecelia entries with correct symbols for each finding; no duplicates; no missing entries
- Lab log panel closed during all of the above: entries still appear when panel opens
- MCP `get_qc_metrics`: returns all findings for an image
- Claude (Ask Claude button): can reason about all findings without being told separately

If all six are consistent, the systems are correctly integrated.

---

## Relationship to #206-208

PRs #206 (clustering QC), #207 (HMM QC), and #208 (track measures QC) add per-task `write_qc!` calls. They should be rebased onto the consolidation work (which unifies the QC store as single source) before merging, not before. They are compatible with the observer redesign as long as their severity levels use the same `info`/`warn`/`error` scale defined here.
