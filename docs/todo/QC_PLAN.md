# QC framework — plan

Status: **Phase 1 landed** (convention + drift producer + image badge). Phases 2–3 open. Durable
parts summarised in `docs/ARCHITECTURE.md` (QC section) + `docs/UI.md` (QC badge).

## Motivation

A task can complete "successfully" (exit 0) yet produce output that is quietly wrong. The trigger
case: **drift correction on the wrong reference channel**. `fHqhyb` (project `4kS67f`) drift-corrected
against a channel that didn't track — the estimated per-timepoint shift jumped at T15→T16, so the
output canvas ballooned to accommodate a spurious drift:

| image | source `[Z,Y,X]` | drift `[Z,Y,X]` | ΔZ | ΔY | ΔX |
|---|---|---|---|---|---|
| LUkCpP (normal ref) | `[13,512,512]` | `[19,541,527]` | +46% | **+6%** | **+3%** |
| …7 others (normal) | `[13,512,512]` | +8–69% Z | | **+2–15%** | **+2–15%** |
| **fHqhyb (bad ref)** | `[13,512,512]` | `[26,728,618]` | +100% | **+42%** | **+21%** |

**Z growing is expected** (drift expands the Z canvas too); the tells are the **XY canvas blow-out**
(normal ≤15%, fHqhyb +42%) and, upstream of it, a **large inter-frame jump** in the applied drift.

We want a general, reusable **QC layer**: any module task can emit QC findings for the output it
produced; the GUI surfaces them as a non-blocking **"we processed this, but it looks off — check it"**
badge + tooltip on the image, and on the chain whiteboard node that produced it. Same spirit as the
physical/timing metadata warning (`imageMetadataWarnings.ts` → ImageTable icon + `PhysicalSizeDialog`),
generalised across tasks.

## Locked decisions (from discussion)

1. **QC artifact convention** — one JSON per (task, output) under a `qc/` tree:
   `1/{uid}/qc/{funName}/{valueName}.json` — e.g. `qc/cleanupImages.driftCorrect/driftCorrected.json`.
   Image-scoped + durable (survives reload, readable by the whiteboard), keyed by the fully-qualified
   `fun_name` then the output `value_name`. (`1/{uid}` is the per-image "task dir"; `task_run_dir`
   still holds the raw run log.) A task with **no** output `value_name` falls back to
   `VERSIONED_DEFAULT_VAL` (`"default"`) so the helper always has a key.
2. **Drift task must persist the applied drift.** It currently computes per-T shifts and only *logs*
   them (`drift_correct_run.py`: `log.log(f'shifts: {shifts}')`). Persist the trajectory so it's
   inspectable and is the data QC reads.
3. **General engine, two first checks:** an inter-frame **drift-jump** check (precise) and a general
   **canvas-expansion** check (works for any spatially-transforming output). More checks per task
   added as we go.
4. **QC is advisory, never blocking.** It flags for a human; it never fails a task or gates a run.
5. **Backend computes the findings; the frontend renders them.** Thresholds live in one place (Julia),
   not duplicated in TS — the GUI just formats the QC findings into the badge + tooltip.
6. **Surfaces:** (a) ImageTable row badge + tooltip, (b) MetadataPanel, (c) chain whiteboard node.

## QC file schema (draft)

```jsonc
{
  "funName":   "cleanupImages.driftCorrect",
  "valueName": "driftCorrected",
  "source":    { "shape": [94,4,13,512,512] },   // [T,C,Z,Y,X]
  "output":    { "shape": [94,4,26,728,618] },
  "findings": [                                    // generic; the engine renders these verbatim
    { "level": "warn", "code": "drift.canvas_expansion",
      "short": "Drift output canvas grew +42% in XY",
      "long":  "Normal drift correction expands XY ≤~15%; this grew +42%/+21%, which usually means the reference channel didn't track. Re-run with a stronger/structural channel.",
      "detail": { "xyExpansionPct": 42 } },
    { "level": "warn", "code": "drift.jump",
      "short": "Large drift jump at T=16",
      "long":  "The applied drift jumps sharply between T15 and T16 (…px), far above the trajectory's typical step — a sign the correlation locked onto noise.",
      "detail": { "atT": 16, "jumpPx": 137 } }
  ],
  "trajectory": { "shifts": [[z,y,x], …] }         // applied drift, per T (drift task only)
}
```

`findings` is the contract the frontend depends on; `trajectory`/`detail` are producer-specific extras.
`level ∈ info | warn` (reserve `error` — QC never blocks). Worst level on an image drives the badge.

## Finding text — clean, brief, ACTIONABLE (a hard rule)

The user should read a flag and immediately know **what to do**. Every producer's findings follow this:

- **`short`** — the *problem*, one terse line (≤ ~8 words). A single key figure is fine
  (`"Drift jumped sharply at T=15"`, `"Output canvas grew +42% in XY"`). No mechanism, no paragraphs.
- **`long`** — the *action*, ONE imperative sentence: what the user should do next
  (`"Re-run drift with a clearer/structural channel."`). Not an explanation of *why*.
- **Numbers / mechanism → `detail`** (structured), never prose. The tooltip renders `short` then `→ long`.

Bad (verbose, explains the mechanism, no clear action): *"The applied drift jumps sharply at timepoint 15
(137 px vs a typical 2 px step) — a sign the reference channel briefly locked onto noise instead of
tracking real drift. Check the output / try another channel."*
Good: short `"Drift jumped sharply at T=15"` · long `"The reference channel likely lost tracking — re-run
drift with a clearer/structural channel."` · detail `{atT:15, jumpPx:137, medianPx:2}`.

This mirrors the app-wide "keep info/warning UI text terse" rule — a flag is only useful if the next
step is obvious.

## Architecture

**Producer (Julia task layer).** New `app/src/qc.jl` — image-owned QC accessor (per the image-owned
accessors convention, not buried in a task): `write_qc(img, fun_name, value_name; source, output,
findings, extras)` writes `qc/{fun}/{vn}.json`, and `read_qc(img[, fun][, vn])` reads it back. Each
task computes its own findings after the run and calls `write_qc`. Drift: `drift_correct_run.py`
returns/persists the shifts; `drift_correct.jl` computes jump + canvas-expansion findings and writes QC.

**Live update.** `task:result` already carries `valueName`/`filename` in `meta` — add an optional
`qc` summary (worst level + count, not the whole file) so the badge appears in-session without a reload
(ARCHITECTURE.md → *task:result mandatory fields*). Durable state is the sidecar.

**Exposure.** `api_images_meta` (or a dedicated `GET /api/images/qc`) includes per-image QC findings
so the GUI can badge on load. `CciaImage` gains `qc?: Record<valueName, QcFinding[]>` (or keyed by
`funName/valueName`).

**Frontend engine.** `frontend/src/lib/qc.ts` — sibling of `imageMetadataWarnings.ts`; given an image's
QC findings, returns the badge level + `{short, long}` tooltip lines. Pure renderer (no thresholds).
ImageTable + MetadataPanel show a QC badge (distinct icon/colour from the metadata warning).

**Whiteboard.** Chain live nodes render off `data.status` today; the chain→store bridge attaches a QC
flag per (image, fun_name) so a produced node shows a QC dot + tooltip. Reuses the same findings.

## Phased build sequence

- **Phase 1 — convention + drift producer + image badge.** `qc.jl` (write/read), persist drift
  shifts, compute drift findings, `qc/*` sidecar, expose on `CciaImage`, `qc.ts` renderer, ImageTable +
  MetadataPanel badge/tooltip. Deliverable: `fHqhyb` shows a QC warning end-to-end. Tests: QC round-trip
  + drift-finding thresholds (golden values from the table above).
- **Phase 2 — whiteboard QC.** QC dot + tooltip on chain live nodes for images with findings.
- **Phase 3 — more producers.** Segmentation (implausible object count / all-empty labels), tracking
  (few/short tracks), cellpose-correct, etc. — one finding set per task as needed.

## Resolved

- **Storage root:** `1/{uid}/qc/{funName}/{valueName}.json` (the image metadata dir — Dominik's "task
  dir"). Durable, per-image, queryable on reload without run context.
- **No-value_name tasks:** fall back to `VERSIONED_DEFAULT_VAL` (`"default"`) as the key.

## Thresholds (initial, from the data above)

- `drift.canvas_expansion`: warn when XY grows **> 25%** (normal ≤15%, bad = 42% — clean separation).
- `drift.jump`: warn when the max single inter-frame step exceeds **N×** the median step (N ≈ 4–5;
  tune once real trajectories are persisted). Prefer the jump check as the *primary* signal (it's the
  cause); canvas-expansion is the robust shape-only fallback that needs no trajectory.
