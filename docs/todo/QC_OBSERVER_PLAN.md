# QC Consolidation + Observer Redesign — build plan

Parked plan (see `docs/todo/README.md`). Supersedes the three exploratory prompts in
`docs/ai-assist/` (`qc-consolidation-prompt.md`, `observer-redesign-prompt.md`,
`qc-observer-relationship.md`) — those framed the problem; this is the agreed, code-grounded plan.
Locked decisions below were made with Dominik on 2026-07-18.

## The thesis (locked)

1. **`qc.jl` is the single source of truth** for "something about this image/output needs attention."
   The image table today has *two* overlapping affordances — the frontend-derived `metadataWarning`
   (yellow triangle) and the `qcFor` badge (`read_all_qc`). Consolidate to one qc.jl-driven signal.
2. **Cecelia is the primary reporter; Claude is on-demand only.** Cecelia already auto-digests activity
   to the lab log (`capture_context!`, author `Cecelia`). Keep that, enrich it with traffic-light
   symbols, and add completion triggers. **Remove the Watch auto-Claude entirely** — Claude fires only
   when the user clicks **Ask Claude**, plus a new **Chat to Claude** hand-off button.

Cecelia must work with no Claude/MCP running: the lab log, the badge, QC findings and cohort checks
are all Cecelia features.

## Traffic-light semantics (locked)

One scale, everywhere (lab-log entry symbols, image-table indicator, cohort summary). **Colour-blind
safe by construction** (built 2026-07-18, `feat/severity-tokens`): a validated CVD-safe hue paired
with a **shape-distinct icon + label** — colour is NEVER the sole cue (WCAG 1.4.1).

| State | Hue (`--cc-sev-*`) | Icon (shape) | Lab-log emoji | Meaning / source |
|---|---|---|---|---|
| ok   | `#0ca30c` | `pi-check-circle` ✓ | ✅ | ran fine — no findings + task succeeded |
| warn | `#fab219` | `pi-exclamation-triangle` ⚠ | ⚠️ | a QC **warn** finding (`qc.jl` `level=="warn"`: metadata gap, cohort outlier, any task warn) |
| fail | `#d03b3b` | `pi-times-circle` ✕ | ❌ | a task **failed** (scheduler run-status `failed` / repeat-failure >3) |

- The canonical definitions live in `frontend/src/lib/severity.ts` (TS) and `qc.jl`
  `SEVERITY_SYMBOLS` / `severity_symbol` (lab-log glyphs). Do NOT hand-pick colours or use the
  same-shape circles 🟢🟡🔴 (they differ only in hue → unreadable under red-green colour blindness).
- `info`-level findings are informational → do **not** raise the light above `ok` and do **not** badge.
- The image-table indicator is a **view** composing advisory QC (ok/warn) with run-status (fail).
  `qc.jl` stays advisory (`level ∈ info|warn`, never blocks) — `fail` comes from run-status, not a qc level.
- **The lab-log toggle badge lights on unseen warn or fail** (Cecelia or Claude authored). `ok` never badges.

## Slices (each a focused PR; consolidation before observer)

### A1 — Metadata warnings → qc.jl (kill the image-table double-affordance) — DONE (`feat/qc-metadata-consolidation`)
- New pure Julia helper `metadata_qc_findings(img)` replicating `frontend/src/lib/imageMetadataWarnings.ts`
  `fieldIssues` (missing Z spacing, auto-corrected Z, unusual Z ratio, missing frame interval,
  no-unit interval, no-unit pixel size). Each finding carries `detail.field ∈ {x,y,z,t}` so the fix
  dialog can keep its per-field highlight. All are `warn`.
- Write via `write_qc(img, "importImages.omezarr", value_name, findings; …)` and **recompute on every
  path that can change calibration**: import completion (`omezarr.jl`), `resync_ome_meta!`, and
  `api_images_meta_set`. No gap where the signal disappears.
- Frontend: `metadataWarning`/`fieldIssues` stop being the SOURCE — the image row and
  `PhysicalSizeDialog` read the metadata finding (from `img.qc`) instead. `flaggedFields` derives from
  the finding's `detail.field`. The yellow-triangle icon and the qc badge merge into ONE indicator.
- Keep the batch-fix flow (`flaggedUids`, `PhysicalSizeDialog`, `api_images_meta_resync`).
- Tests: `metadata_qc_findings` (jl, golden cases mirroring `fieldIssues`); frontend unit for the new
  finding→dialog mapping.

### A2 — Cohort QC writes per-image findings — DONE (`feat/qc-cohort-per-image`)
- `qc_cohort.jl` computes outliers but writes only a *set* sidecar — outliers never reach the image.
  Add per-image write-back under a **`cohort.*` fun namespace** (e.g. `cohort.segment.cellpose`) so it
  can't clobber the task's own `{uid}/qc/{fun}/{vn}.json` and surfaces automatically via `read_all_qc`.
- Outlier → `warn` finding on that image (⚠️); non-outliers clear any prior cohort finding.
- Tests: per-image cohort finding written to the `cohort.*` namespace; `read_all_qc` merges it.

### A3 — Cohort QC button + toast convention
- "Check cohort consistency" button on module pages → `POST /api/qc/cohort/check?projectUid&setUid&funName`
  → runs `cohort_qc_for!` for the stage, writes per-image findings (A2), appends a
  `[Cecelia — Cohort QC]` lab-log summary line (✅ all-clear / ⚠️ N flagged).
- Establish the toast convention ONCE: check whether PrimeVue `<Toast/>` is already in `App.vue`; if
  not, add it once + `useToast()`. Record in `INVENTORY.md` under a `UI conventions` section
  (toast = transient foreground feedback; badge = persistent needs-attention; lab-log = durable record).
  info→✅ / warn→⚠️ / error→❌ severity, consistent with the traffic light.

### B1 — Remove Watch (auto-Claude)
- Delete the `'auto'` trigger: `observer.ts` `installAutoWatch`, its `useTaskCompletionWatch` use, the
  `App.vue` install, and the Watch toggle/label in `LabLogPanel`. **Keep** `runPass('manual')` = Ask
  Claude (+ model picker, activity log, prompt display). The generic `useTaskCompletionWatch`
  composable STAYS — repurposed by B2.

### B2 — Cecelia automatic summaries (enriched) + completion triggers
- Enrich `capture_context!` (`lab_log_context.jl`) line rendering with a leading symbol per module:
  ❌ if any run failed, ⚠️ if that module produced `warn` QC findings this window, else ✅. (Failure
  counts are already captured — `— N failed`.)
- Add triggers: fire a capture on **chain completion** and **set-scope run completion** (reuse the
  freed-up `useTaskCompletionWatch`; capture aggregates "since last capture" so it stays coalesced and
  catches up on panel-open). Keep the manual "Capture activity" button.
- Badge: when a capture appends unseen ⚠️/❌ while the panel is closed, light the lab-log badge
  (extend `settings.labLogUnseen` with a level so the badge colour reflects ⚠️ vs ❌). Writes go through
  `POST /api/lablog/append`/capture regardless of panel state.

### B3 — Chat to Claude button
- Copies a **starter prompt** to the clipboard (project uid + "use the cecelia-observer MCP tools to
  review recent activity/QC and help with …", plus a one-line MCP-connect pointer to
  `docs/ai-assist/OBSERVER-SETUP.md`) and shows a toast: "Prompt copied — open your chat bot and paste
  it." Clicking again re-copies. Generic to any MCP-capable assistant, not tied to a terminal.

## Sequence & PR shape
A1 → A2 → A3 → B1 → B2 → B3. Consolidation (A*) first so Cecelia's entries reflect all findings before
the observer redesign lands. Each slice: pure helper unit-tested + wiring + doc update
(`QC-PROCESS.md`, `OBSERVER.md`, `INVENTORY.md` as touched).

## Explicitly out of scope (parked)
- Auto-calling Claude on "multiple warnings" (escalation ladder) — Dom chose on-demand-only; revisit
  only if on-demand proves insufficient.
- `qc_flag_fired` / `:flagged` node state + hold (needs QC_PLAN.md advisory-vs-blocking ratification).
- afCorrect / cellposeCorrect QC (no agreed objective metric — see the QC-coverage notes).
- A separate Claude-vs-Cecelia badge *icon* — v1 uses one badge coloured by worst unseen level; a
  distinct author icon is a nice-to-have.

## Invariants (do not violate)
- One QC store (`qc.jl`); cohort writes through `write_qc` under `cohort.*`, never a parallel store.
- One lab-log write path (`POST /api/lablog/append` / `capture_context!`), author-tagged.
- One severity scale (info/✅, warn/⚠️, failure/❌) across findings, badge, lab log, toast.
- The function is `write_qc` (no bang) — the exploratory prompts wrote `write_qc!`.
