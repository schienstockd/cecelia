# Phase: Lab Log — Shared Project Memory

Design phase for Opus. Read `OBSERVER.md` and `QC-PROCESS.md` alongside this. The lab log is the shared artefact that makes Claude useful across sessions. Without it, every session starts blind. With it, Claude accumulates the non-obvious knowledge that isn't inferable from data or framework alone.

---

## What the lab log is

A per-project, append-only, human-and-AI-writable file that lives in the project directory. It is the persistent memory for AI-assisted analysis sessions.

It is NOT:
- Documentation (that's `docs/`)
- A task log (that's the per-image task log the scheduler writes)
- A results file (that's the analysis output)
- Image notes (those are per-image, written by the user in Cecelia)

It IS: knowledge that would otherwise be lost between sessions. The methodology. The why. The edge cases. The corrections.

**Filename — decided: `lab-log.md` at the project root** (`{projects_dir}/{proj_uid}/lab-log.md`).

Rationale: it is the working title, and it wins on every axis that matters here. A biologist opening the project directory reads "lab log" instantly — no decoding. Lowercase-hyphen keeps it visually distinct from the uppercase design doc `docs/ai-assist/LAB-LOG.md` (this file), so "the lab log" (per-project data) and "LAB-LOG.md" (the spec) never get confused. Project **root**, not `settings/`, is deliberate: the spec's whole thesis is that this lives *next to the data*, visible, the first thing you see when you return to a project — burying it under `settings/` (where machine-written sidecars like `chains/`, `analysisBoards.json` live, `api/src/routes.jl:7`) would signal "config, not for humans." Rejected alternatives: `NOTES.md`/`README.md` (collide with generic meaning and existing conventions), `METHODS.md` (implies a finished write-up, not an append-only running log), `analysis-log.md` (accurate but less immediately human than "lab log").

---

## Relationship to image notes and task logs

The lab log complements, not duplicates:

- **Image notes** (per-image, user-written in Cecelia UI) — quick observations about a specific image. "This section has a fold." "Channel 2 autofluorescence high."
- **Task logs** (per-image-per-task, written by the scheduler) — what ran, parameters, output paths, errors. Machine-written.
- **Lab log** (per-project, human and Claude) — cross-image knowledge, methodology rationale, pattern recognition, corrections. What image notes and task logs don't capture.

Claude reads all three. The lab log is the only one Claude writes to.

---

## What goes in it

**Yes:**
- Gating decisions not derivable from data ("CD4+ gate runs loose on spleen, channel 2 lower boundary ~0.3 too low for this tissue prep")
- Image exclusions with quantitative justification
- Parameter choices that worked for this cohort and why
- Cross-experiment patterns Claude recognised
- Corrections to Claude's prior suggestions
- User's stated rationale when Claude asked "why did you do that"
- QC flags that were acknowledged and what action was taken

**No — inferable from framework or data:**
- Task completion status (task manager)
- Image dimensions, channel names (MCP tools)
- Population counts (MCP tools)
- What module functions exist (CLAUDE.md)

---

## Format

Append-only, dated, author-tagged entries. Never edit old entries — add a correction entry instead.

```markdown
## 2026-07-14 [Claude]
- Image 7 flagged: gate produced 23 cells (cohort mean 187). User excluded image.
- Repeat attempt pattern: cellpose on image 12 run 8 times. User confirmed tissue folding 
  is the issue — cannot be recovered. Excluded.

## 2026-07-14 [User]
- CD4+ gate: lower boundary channel 2 should be ~0.25 for this tissue prep, not 0.3 default.
  Spleen samples in this batch have higher background than thymus.
- Image 3 state 1 dominance is real biology — early-stage infection. Keep, note in analysis.

## 2026-07-14 [User — correction]
- Corrects 2026-07-12 [Claude]: Image 5 speed distribution is NOT an outlier. This mouse 
  received a different treatment — the higher motility is expected. Do not flag this pattern
  for treated cohort.
```

Author tag `[Claude]` or `[User]` on every entry block. Correction entries reference the original by date and author. Claude reads corrections and adjusts reasoning within the session.

### App-generated context — `[Cecelia]` entries

A third author tag, `[Cecelia]`, marks entries the **app** generated automatically — the *what* (activity) to complement the human *why*. So the log records not just decisions but the work that surrounded them, and returning to a project shows what happened without anyone having typed it.

- **One rolling DAILY block, regenerated from source (not append-per-capture).** `capture_context!(proj)` (`app/src/lab_log_context.jl`) maintains a **single `[Cecelia]` block per day**, rewritten in place as the day's activity accrues (`upsert_daily_context_block!`, `app/src/lab_log.jl`). This is a deliberate, scoped relaxation of append-only: the `[Cecelia]` digest is *derived* data (regenerable from the run logs / gating / exclusions), so only *that day's* `[Cecelia]` block is ever rewritten — every human `[User]`/`[Claude]` block and every prior day's digest is preserved byte-for-byte, so the append-only guarantee that matters (the science record) is intact. Why it changed: capture fires after every task, and with long-running tasks finishing hours apart the old "append a fresh block per capture" produced *one block per task* — a wall of near-identical dated blocks. Now they all fold into the day's one block regardless of capture timing. Tasks are grouped by the **local date of their run-log `at`**; gating/exclusion changes are the **net change since the start of the day** (a `dayDate`/`dayGating`/`dayExcluded` baseline in `settings/lab-log-context.json` that resets on day rollover). A fresh `⚠️`/`❌` still surfaces the moment its task lands (the block updates and the badge fires); a day with zero captures is not retro-summarised (the activity stays in the run logs). Route: `POST /api/lablog/capture`.
- **Module-centric + collapsed.** The digest is grouped by **module category — the same tags the task manager shows** (a task's `category` from its spec, read via `lab_log_categories()`; non-task activity maps to the nearest tag — gate pops → `Gating`, cluster pops → `Clustering`, exclusions → `Manage images`). One bullet per category, in task-manager order, and **identical changes across many images collapse to one entry with a count** (change-centric, not image-centric). So a cohort-wide edit reads:
  ```
  - Segment — cellpose on 5 images (…)
  - Gating — gate changed: qc (9 images)
  - Cluster tracks — redefined: Directed, Meandering, Scanning (8 images)
  ```
  Sources: **tasks** (run logs since the cutoff, `category.` prefix dropped); **populations** (a per-pop fingerprint hashing each pop's **whole membership definition** — gate *and* filters/cluster-membership/any future field, via generic reflection minus cosmetic fields — so gate edits *and* cluster-population edits are caught, `gate changed` vs `redefined`); **exclusions** (the excluded-image set diffed). Labels aren't invented here — if a category reads wrong, fix it in the task JSON and the log + task manager update together.
- **Images are referenced by stable UID, not name.** Every image reference the digest writes (the `on N images (…)` list, the ↳ detail lines, population/exclusion lists, the cohort-check summary) is the image's **UID** — names change, UIDs don't, and UIDs are compact. The panel has a **"Show image names"** toggle (`labLogShowNames`, default off) that swaps each known UID for its *current* name at render time (resolved against the live `imageNames` map from `GET /api/lablog`) — a display-only substitution, so a rename shows through with no rewrite of the append-only log.
- **First capture seeds the population/exclusion baselines silently** (no giant retro dump); only genuine deltas thereafter.
- **Triggers: manual + auto.** The panel has a "Capture activity" button and an "Auto" toggle (`labLogAutoContext`, default off) that captures on project open.
- **Known limits / to tune as we go:** run-log timestamps are second-granular and the cutoff compare is strict (`>`), so task activity in the *same second* as a prior capture is skipped (negligible). Gate changes report only *that* a definition changed (by population), **not magnitude** (scrapped for now). Threshold/geometry values are intentionally excluded (that's the undo-list we don't want).
- Digests are ordinary appended entries — the user annotates by adding normal `[User]` notes (or a correction) around them.

### Interacting with auto/AI entries — a thumb prefills a note

On-hover, `[Cecelia]` and `[Claude]` entries expose 👍/👎/💬. Each **judges the DECISION → recorded content**: 👍/👎 prefills a `[User]` note with the verdict + a reference (`👍 re 2026-07-15 [Cecelia]: `); the user adds the *why/outcome* and saves. This is genuine lab-notebook methodology — *"changed CD8 gate 👍 — got the separation I was after"*, *"changed seg params 👎 — 2 h to revert, didn't help"*. 💬 is the same, without a verdict. Lands in the log via `append_lab_log!`.

Only app/AI entries are ratable — you don't thumb your own `[User]` notes (they keep the plain "correct" affordance). The thumbs carry no persisted state; they are just a shortcut to compose a note, so the science record is the only thing that's stored.

> **Dropped (2026-07): entry-type "tuning" votes + digest category mutes.** An earlier design had a second "Rating" mode where 👍/👎 rated an auto-entry's *type* (useful/noise) into a `lab-log-tuning.json` sidecar, plus a mute-chip bar to silence whole digest categories (`lab-log-mutes.json`). The tuning vote had **no consumer** — nothing ever read it back to change behaviour — and the mutes went unused in practice, so both (and the mode toggle they needed) were removed. A 👍/👎 now unambiguously means "this decision was good/bad → note it".

---

## MCP tools

```
read_lab_log       → full log content, returned newest-first for efficient context loading
append_lab_log     → append a dated [Claude] entry, never edits, enforces append-only
```

User writes directly to the file or via the Vue panel. Claude writes only via `append_lab_log`. This keeps Claude entries identifiable and prevents overwrites.

---

## Vue panel

The lab log GUI is what determines whether anyone actually uses it. The file is the backend. The panel is the product. Design for zero friction between "I just made a decision" and "it's recorded."

**Placement**: persistent slide-out panel accessible from any module page without navigation. Not a separate page. The user makes a gating decision, glances right, types one line, continues. If they have to navigate somewhere to log it, they won't.

**Always-visible entry field**: a single text input at the top of the panel, always focused, date and author pre-filled. One sentence minimum. Submit with Enter. No form, no fields, no save button to click.

**Context-aware prefill**: when the user is on a module page and something changes — threshold adjusted, image excluded, task re-run — a "note this" prompt appears inline on that page, pre-filled with the context (image UID, stage, what changed). The user adds the reason and submits. The worst version is an empty box. The best version already knows what happened and just needs the why.

Example prefill from a gating action:
```
[2026-07-14] CD4+ gate threshold changed 0.3 → 0.25 on image KDIeEm — [your note here]
```

**Project open summary**: when a project is opened that has lab log entries, the most recent 3-5 entries surface automatically in a banner or sidebar without the user having to open the panel. If you haven't touched this project in three months, the first thing you see is what you were doing and why.

**Entry display**: chronological, newest first. Clear visual distinction between `[Claude]` and `[User]` entries — different background colour or left border colour, not just a tag. New Claude entries since last session are badged so the user is prompted to review them.

**Correction**: one-click on any entry opens a pre-filled correction: `[correction to YYYY-MM-DD User/Claude]: ` — user completes it and submits. No editing of old entries, only appending corrections.

**Read-only for Claude entries in the UI**: Claude entries cannot be edited from the panel. Append-only enforced both in the UI and in the MCP write tool.

Note: voice input is explicitly out of scope — it solves a problem that doesn't exist for this workflow and adds unnecessary complexity.

---

## Versioning

The lab log is per-project and per-version. On version bump, prior lab log is copied with a version boundary header:

```markdown
## [Version boundary: v1 → v2, 2026-07-15]
Entries below are from v1. Context may still be relevant.
---
```

Institutional memory does not get lost on version bump.

---

## Claude session behaviour

On session start: Claude reads the full lab log. Weights recent entries more heavily. Explicitly notes correction entries and adjusts reasoning — a corrected pattern should not be flagged again.

Claude appends to the lab log during the session when:
- A QC flag fires and the user acknowledges it
- A why question is answered by the user
- A cross-experiment pattern is recognised
- A parameter adjustment is accepted or rejected

Claude does NOT append for routine completions, ambient observations that don't reach flag threshold, or anything that duplicates what image notes or task logs already capture.

---

## Implementation plan

> Grounded against the codebase as of 2026-07. The lab log has **standalone value and no hard
> dependency on the MCP server or observer** — steps 1, 4, 5 can ship first and be written manually
> via the panel; Claude reads/writes what's already there once the MCP arrives (see the ROADMAP's
> "start it now").

### 1. File format spec

- Plain markdown, append-only, at `{proj}/lab-log.md`. One `##`-level block per entry: `## YYYY-MM-DD [Author]` where `Author ∈ {Claude, User, User — correction}`, followed by `-` bullet lines. Exactly the format already shown in this doc above — treat that as the normative example.
- **Required per block**: ISO date, author tag. Correction blocks reference the original by date + author (`## 2026-07-14 [User — correction]` → first bullet begins `Corrects 2026-07-12 [Claude]:`).
- **Never edit or delete existing entries** — corrections are new appended blocks. This is what makes the file safe for concurrent human + Claude writers and auditable.

### 2. Filename — decided

`lab-log.md` at the project root. Full rationale is under *What the lab log is* above (decided, not deferred).

### 3. MCP read tool — `read_lab_log`

- New thin read-only route `GET /api/lablog?projectUid=…` → reads `{proj}/lab-log.md`, returns `{ content, mtime }`. Add one `elseif` in the GET block of `handle_http` (`api/src/server.jl` ~`:243`) calling a new `api_lablog_read(req)`; response via `JSON3.write((; content, mtime))` (routing convention `docs/API.md:36-47`). Restart required (api/ is not Revise-tracked).
- **Ordering**: return raw content plus a parsed, **newest-first** list of blocks for efficient context loading (the MCP tool hands Claude newest-first). Parsing is trivial (split on `^## \d{4}-`).
- **Long-log summarisation**: for the MCP layer, if the file exceeds a token budget, return the full most-recent N entries verbatim + an older-entries digest, and always return **all** `[correction]` blocks verbatim regardless of age (corrections must never be summarised away — they gate future behaviour, step 7).

### 4. MCP write tool — `append_lab_log`

- New route `POST /api/lablog/append` → `api_lablog_append(body)`, body `{ projectUid, author, lines: [...] }`. Server **injects the date and author tag** and formats the block — the caller never supplies the header, which keeps `[Claude]` entries unforgeable and the format uniform.
- **Append-only + concurrency**: write under the sanctioned project write lock `with_transaction(f, proj)` (`app/src/model/project.jl:150-166`, holds `{proj}/.cecelia.lock`) so a human panel write and a Claude write can't clobber each other. Open the file in append mode; never rewrite existing bytes.
- **Author enforcement**: the MCP `append_lab_log` tool always passes `author="Claude"`; the panel passes `author="User"`/`"User — correction"`. (When Phase-2 auth exists this can be server-attributed instead of client-declared.)
- Broadcast `{type:"lab:entry", projectUid, author}` via `broadcast_ws` (`server.jl:52-62`) after a **user** append, so the observer/MCP sees `lab_log_entry_added` (see `QC-PROCESS.md` step 8). Do **not** broadcast Claude's own appends (avoids a feedback loop).

### 5. Vue panel

- **No slide-out/drawer component exists yet** — the reusable "floats above any page" primitive is `frontend/src/components/FloatingPanel.vue` (draggable/resizable, `position:fixed`, persists geometry to `localStorage` under `cc.floating.<key>`; `docs/UI.md:212-232`). Mount the lab-log panel in the app shell `frontend/src/App.vue` **as a sibling to the existing `FloatingPanel`** (App.vue:62-65 already mounts `ViewerPanel` this way), so it is reachable on every module page with zero navigation — the spec's load-bearing requirement.
- **Open toggle**: add a persisted flag mirroring `viewerPanelOpen` — a `ref` in the `settings` store watched to `localStorage` (`frontend/src/stores/settings.ts:48` + `:160`). Every other user-settable option on the panel goes through `useViewState` over a store-backed bag (`frontend/src/composables/useViewState.ts`), per the hard "persist every user-settable option" rule.
- **Always-visible entry field** at the top, focused, date/author pre-filled, submit-on-Enter (one input, no form). **Entry display** chronological newest-first, with a distinct left-border/background colour for `[Claude]` vs `[User]` (not just the tag); new Claude entries since last open are badged. **Correction**: one click on any entry opens a pre-filled `[correction to YYYY-MM-DD Author]:` append (never an in-place edit). Claude entries are read-only in the UI (append-only enforced in both the panel and the write route).
- **Context-aware prefill**: when a module-page action changes something (threshold edited, image excluded, task re-run), show an inline "note this" prompt pre-filled with the context (image UID, stage, what changed) — the user adds only the reason. These actions already have hook points (e.g. the note/inclusion write `POST /api/images/inclusion/set`, `routes.jl:891`).
- Voice input is explicitly out of scope.

### 6. Version copy behaviour

- **There is no project/set-level version concept today** — the only versioning is the per-field versioned-variable pattern *inside a single image's* ccid.json (`versioned_get_field`/`set_active!`, `app/src/model/image.jl:3-93`); `CciaProject`/`CciaSet` have no `version` field, and there is **no project-level "version bump" event to hook**. So a lab-log version boundary must be defined here, not borrowed.
- **Recommended trigger**: an explicit, user-invoked "start a new version" action in the panel (and, later, at release/freeze time). On trigger, insert a boundary header into the same append-only file rather than copying to a new file:
  ```markdown
  ## [Version boundary: v1 → v2, 2026-07-15]
  Entries below are from v1. Context may still be relevant.
  ---
  ```
  Keeping it in one file (vs a `lab-log.v1.md` copy) means institutional memory stays in the single document Claude already reads; the boundary is just a parseable marker that lets the reader weight pre-boundary entries as historical. (If the project later gains a real version field — or adopts the notebooks-style snapshot/restore in `docs/NOTEBOOKS.md` — wire the boundary to that instead.)

### 7. Claude weighting

- On session start Claude reads the full log (newest-first, step 3), **weights recent entries more heavily**, and treats entries below a `[Version boundary]` as historical context, not current truth.
- **Corrections gate future behaviour**: a `[User — correction]` block that says "do not flag pattern X for the treated cohort" must suppress that flag for the rest of the session. Implement as: parse all correction blocks, build a suppression set keyed by (pattern/metric, scope), and check it before the observer surfaces any flag (`QC-PROCESS.md` step 8, speak-vs-silent). This is why corrections are never summarised away (step 3).

### 8. Integration with image notes and task logs

- Claude reads three sources and the lab log is the only one it writes. Avoid duplication by role:
  - **Image notes** — per-image `note` field on `CciaImage` (ccid.json; read via `_image_payload`/`get_image_notes`, written via `POST /api/images/inclusion/set`, `routes.jl:891`). Quick per-image observations. Claude *cites* them, never copies them into the lab log.
  - **Task logs** — `{img._dir}/logs/{fun}.log` (`scheduler.jl:321-335`) + the success-only run log `{proj}/1/{uid}/runlog.json` (`app/src/run_log.jl`). Machine-written provenance. Claude reads for the attempt/failure pattern; it does not restate completions in the lab log.
  - **Lab log** — cross-image methodology, rationale, corrections, acknowledged flags. Only what the other two don't capture.
- Concretely: Claude appends to the lab log **only** for the events listed under *Lab log entries from QC* in `QC-PROCESS.md` (flag acknowledged, param adjustment accepted, image excluded, cross-experiment pattern, why-answer). Never for routine completions.

### 9. Dependencies and build order

1. **Read/append routes + file format (steps 1, 3, 4)** — pure additions to `api/src` + a lock-guarded writer; no scheduler or model changes. Ships independently.
2. **Vue panel (step 5)** — depends only on step 1's routes; delivers standalone value with manual entries before any AI integration exists.
3. **Project-open surfacing** — have `api_projects_load` (`routes.jl:340-411`) return the recent lab-log tail alongside `project/sets/boards/...`, surfaced by the frontend `openProject` after `loadFromApi` (`frontend/src/stores/projectMeta.ts:64-118`).
4. **MCP `read_lab_log`/`append_lab_log` tools** — thin wrappers over the step-1 routes; depend on the `OBSERVER.md` MCP server.
5. **QC-triggered appends + correction-gating (steps 7, 8)** — depend on the QC events (`QC-PROCESS.md` steps 2, 8) and the observer being live.
6. **Version boundary (step 6)** — independent; can land with the panel.
