> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was investigated at
> the time. It is not a description of how the code works now, and not instructions to re-run.
> Current design lives in [`docs/todo/KIWI_ASSISTANT_PLAN.md`](../todo/KIWI_ASSISTANT_PLAN.md) and
> [`docs/todo/AGENT_RUN_REVIEW_PLAN.md`](../todo/AGENT_RUN_REVIEW_PLAN.md).
>
> **What this is:** the read-only answer to
> [`OPUS_AUDIT_CLAUDE_CO_DRIVING_CECELIA.md`](OPUS_AUDIT_CLAUDE_CO_DRIVING_CECELIA.md), run
> 2026-10-22 at `a67e8b5c` by three parallel agents (surfaces / run evidence + prior art / error
> modes). The surprising claims were re-checked by hand (marked ✔). Nothing was run live: every "unit"
> below means no browser has exercised it.

# Co-driving audit — where the user and Claude interact today

**Bottom line.** Claude can read a lot and point at a lot, but every Claude action fires immediately;
the only code-enforced "Claude proposes, user decides" paths are blackboard section verdicts and
Kiwi's `proposedPlot`. Nothing is tested in a browser (no playwright/e2e in the repo). The autonomous
runs show the errors that matter happen at four **stage boundaries** (after segmenting, at gating,
after tracking, before behaviour) and are visible in a picture plus n-per-image — so that is where a
human checkpoint belongs, not on every tool call.

## 1. What exists

| Capability | Status | Evidence |
|---|---|---|
| Read: images, QC, pops, measures, boards, captures, landscape, task history | unit-tested only | `mcp/cecelia_mcp/server.py`; `mcp/tests/test_server.py` |
| `mark_tracks` / `mark_cells` | unit-tested only; frontend handler untested | `api/src/marks_api.jl:206-243`; `frontend/src/stores/ws.ts:277-299` |
| `select_on_plot` | unit-tested only; **multi-source keeps only the last image's viewer outline** ✔ (single highlight bag) | `ws.ts:323-347`; `frontend/src/stores/viewer.ts:417-422` |
| `mark_tile` | **broken** ✔ — server broadcasts, `ws.ts` has no `tile` branch; Kiwi tile chip is `action:'none'` | `marks_api.jl:443-468`; `ws.ts:277-356`; `kiwiTurn.ts:198` |
| `mark_freeform`, `mark_plot`, `point_at_ui` | unit-tested only; TTL; dismiss UI except `mark_plot` | `marks_api.jl:366-540`; `PointerBubble.vue:82` |
| `seek_viewer` | unit-tested only; **always returns `ok:true`** ✔ even with no browser (docstring says `no_browser`); no way back to the previous t/z | `api/src/viewer_nav_api.jl:10-11,59` |
| `open_analysis_board_plot` | 0/1/many resolved in MCP (tested); **tab-lookup race still present**, silent fallback to last tab; now shared by Kiwi "Plot this" | `server.py:1862-1911`; `frontend/src/lib/boardNav.ts:4-19`; `useKiwiPoint.ts:117` |
| Landscape tiles (#1710) | zoomed+panned registration unit-tested both sides; tile-ref `(t,z)` key has no viewport — a re-zoom silently re-points old refs; no t/z-mismatch test | `landscape.test.ts`; `api/src/kiwi_refs.jl:316-329`; `landscape_api.jl:53` |
| Annotate: blackboard, lab log, notebooks, chains, boards | unit-tested; create-only 409 / snapshot-before-revise; authorship stamp except **boards** and **`set_labarchives_context` (replaced, no history)** | `server.jl:231,464,498`; `routes/project.jl:206-229`; `labarchives.jl:92-121` |
| Kiwi claims + `KiwiRef` | refs must resolve and appear in a tool result this turn — **enforced in code**; numbers themselves are not checked | `api/src/kiwi_turn.jl:314-318,426-469` |
| Claude proposes, user accepts | **two only**: section verdicts (stored as proposal, never counted, 409 over a person's) and `proposedPlot` | `blackboard_run_review.jl:136,173-175`; `kiwi_api.jl:165-225` |
| Change gate / parameter, launch step | **missing interactively**; exists only in `cecelia-autonomous` (project lock + prefix + allow-list, no human approval) | `autonomous.py:36-82`; `test_server.py:271` `test_no_tool_can_start_work` |
| Shared selection, Claude vs user | **last writer wins** ✔ — Claude's select replaces the user's brush, no stash | `linkedSelection.ts:73-87`; `linkedSelection.test.ts:59` |
| Session record | **none** for marks/seeks/selections; Kiwi `turns.json` keeps tool names, not results; #1192 "telemetry" is only the blackboard outcome-note counter | `marks_api.jl:7-10`; `kiwi_turn.jl:546-552`; `blackboard_api.jl:20-45` |
| Login / profiles (#1717/#1718) | unit-tested; default profile cannot be logged out ✔ | `app/src/ai/claude_auth.jl:27,88` |

Reusable human patterns: gating undo/redo (50 deep, `gating_api.jl:266-335`), correction cockpit
pending-op queue (`lib/trackCorrection.ts:148-158`), task preview (`/api/preview/*`), `ConfirmButton`,
blackboard/notebook restore, authorship `{profile, via}`.

## 2. Evidence from runs

15 app-tier runs (2026-10-04 → 10-09), **all on tSJpBI**, n = 1–3 per condition, **zero recorded
human verdicts** (`sectionOutcomes` empty; no `kind: review` events). Source:
`~/.cecelia-effectiveness/app-runs/NIGHT{,2,3}_NOTES.md`, run records, `events.jsonl`.

| Observed | Where a checkpoint would catch it |
|---|---|
| AF/bleedthrough skipped (0/3, 0/6); CTV bleed left in OTI gate | cleanup plan → channel × channel scatter |
| No QC gate, or QC gate keeping 55–100% vs reference 23–67% | gate → kept vs excluded cells on the image, % kept |
| One image's gates copied to others unseen, widened after re-seg | multi-image apply → one strip per image |
| Under-segmentation (≈ half the cells) carried into tracking; no run went back | before tracking → per-frame counts + overlay |
| Tracking radius raised "so every image got tracks" → speeds 2× reference | after tracking → speed distribution vs expected |
| Image/pop silently dropped from HMM/clustering; clustering on 1–4 tracks | before behaviour → n table per image × pop |
| Hallucinated precondition for skipping a step | skip → stated reason beside the guide text |
| Rejected flow model reused by a later run | model choice → origin + rejected status |

Agents found the pipeline and recovered from tool errors; they failed on judgement calls, often
*noticed* ("speeds look high") and carried on. Prior art (Omega, BioImage.IO Chatbot, Claude Code
permissions, LangGraph `interrupt`) all **gate actions, not results**; Omega's self-correction covers
code tracebacks only. Worth copying: plan mode, per-action-class ask rules, approve-with-edit at a
durable pause.

## 3. Control model

| Action | Exists today? | Proposed default | Undo | Logged |
|---|---|---|---|---|
| Read | yes | free | n/a | Kiwi tool names only → add results digest |
| Point (mark, select, pointer) | yes (`mark_tile` broken) | free, **never replaces the user's selection** | TTL / clear; restore the user's selection | add, with `origin` |
| Navigate (seek, board) | yes | free | add "back to where you were" | add |
| Annotate (blackboard, log, notebook, chain, board) | yes | free (additive) | restore / delete; add history to LabArchives sidecar | yes, except boards |
| Change gate or parameter | autonomous only | **ask first — staged ghost, user accepts or edits** | gating undo stack | add, per author |
| Launch step | autonomous only | **ask with cost** (preview + est. time) | n/a; outputs versioned | run record |
| Delete / overwrite | no | keep missing for Claude | — | — |

**Knobs:** ask-first level per action class (Claude Code's deny/ask/allow model); "my selection is
locked"; **stop at stage boundaries** (default on); a stop button for anything queued.
**Guardrails in code (not prompt):** allow-list, create-only, project lock (exist); `origin:'claude'`
on every write and mark; proposals never count until a person accepts (exists for verdicts — extend);
"unchecked" shown as unchecked.

## 4. Error modes

| Mode | Observed? | Guarded today | Code-checkable |
|---|---|---|---|
| Wrong region/scope | yes (pooled label set, dropped image) | partly (clustering form warns; cohort excludes) | analysed uids ≠ set's included images → warn |
| Units/scale | no | missing calibration warns; **present-but-wrong not caught** | calibration as cohort metric (outlier) |
| Channel/slice | yes (AF/bleed) | name errors; cohort `nChannels` | in-cell vs outside intensity ≈ 1 |
| Bad segmentation | yes | `seg.*` **info-only, uncalibrated, hidden from Claude unless discovery is on** ✔ | calibrate thresholds → warn |
| Parameter fragility | yes | no | ±15% rerun, flag count change |
| Tracking errors | yes | partly; `find_track_issues` only in correction panel | bank jumps/gaps/short tracks at tracking time |
| Wrong denominator | yes (partial cohort) | partly; summary fraction divides by **plotted** pops | return + warn on denominator |
| Wrong statistical unit | yes (clusters on few tracks) | **default per cell** ✔; tests on cells, prose warning only | `pseudoreplication: {nImages, nAnimals}` on comparisons |
| Confound as biology | no | saturation, AF, photon-limited | metric ~ acquisition date/depth correlation |
| *New:* loose/copied QC gate | yes | no | kept-fraction vs set median |
| *New:* tuned-until-output | yes | no | flag param raised after `no_tracks` |
| *New:* stale/rejected artefact reuse | yes | no | rejected flag carried by model/vn |

Cohort findings only persist when someone presses "Check cohort" (`api/src/routes/image.jl:747`).

## 5. Top ideas

1. **Stage-boundary checkpoint** — at seg / gate / track / behaviour, Claude stops and shows a
   code-computed card: overlay, n per image × pop, % kept, speed range vs reference; user approves,
   edits or sends back. *Value:* matches where every observed error sat. *Builds on:* QC findings,
   `get_cohort_qc`, `gate_cells_view`, section verdicts. *Missing:* the card and the stop. *Cost:*
   medium. *Wheel:* user. **After PRs 1–2.**
2. **Staged gate / parameter (ghost)** — Claude proposes, user accepts or drags. *Builds on:* gating
   undo, correction pending queue, verdict-proposal pattern. *Risk:* first write path for the observer.
   **After 1; needs your call (Q1).**
3. **Co-drive record** — every mark/seek/select/proposal logged with `origin` and accept/reject; Kiwi
   persists tool-result digests so a number traces to its call. *Cost:* small. **Do now.**
4. **Selection etiquette** — Claude never clobbers a live user brush; stash + restore, or reply
   "user selection active". *Cost:* small. **Do now.**
5. **Three cheap code checks** — calibration cohort metrics, pseudoreplication flag, banked track
   issues. *Cost:* small each. **Do now.**

Rejected: an approval prompt on every tool call (read/point are harmless; gate fatigue); asking the
agent to judge its own result (runs show it notices and carries on); regex-checking numbers in prose
(brittle — Kiwi's ref rule is the better lever); reviving overnight autonomy (parked).

## 6. Next 3 PRs

1. **Make the pointing tools do what they say.** `mark_tile` handler + tile-ref viewport key;
   per-source highlights for multi-source `select_on_plot`; stash/restore the user's brush;
   `seek_viewer` returns `no_browser`; board-tab race reports failure instead of falling back.
   Files: `frontend/src/stores/{ws,viewer,linkedSelection}.ts`, `lib/boardNav.ts`,
   `api/src/{viewer_nav_api,landscape_api,kiwi_refs}.jl`. **Verified:** a live session on a real
   image driving every tool from MCP, a checklist ticked with captures — the step #1192/#1206 skipped.
2. **Code checks** (idea 5). Files: `app/src/qc.jl`, `qc_cohort.jl`, `plotting/{plot_data,stats}.jl`,
   `tasks/tracking/bayesian_tracking.jl`, `qc/text.jl`. **Verified:** unit tests plus the tSJpBI
   runs' images re-checked — the too-fast tracks and per-cell p-values get flagged.
3. **Co-drive record** (ideas 3 + 4 logging). Files: `marks_api.jl`, `viewer_nav_api.jl`,
   `kiwi_turn.jl`, an events sidecar. **Verified:** one live co-driven session replayed from its record.

## 7. Open questions

1. Should Claude ever change a gate or parameter interactively (staged, user accepts), or stay
   strictly additive and leave changes to you? Blocks idea 2.
2. `seg.*` findings and cleanup facts are hidden from Claude unless discovery is on — an eval control.
   In co-driving, should Claude always see them?
3. Which project is the live reference for PR 1's browser check — tSJpBI (the run reference) or jFWePN?

## Reservations — confirmed or refuted

- *Error list from domain knowledge:* **partly refuted** — 15 runs now evidence 7 of 9 modes and add three.
- *Tools unit-tested only:* **confirmed**, and worse — `mark_tile` does nothing in the browser.
- *Too many gates worse than none:* **agreed** — errors cluster at four boundaries; gate there.
- *"Unchecked" should stay visible:* `seg.*` is info-only with uncalibrated thresholds and can be hidden from Claude.
- *Evidence thin:* **confirmed** — one project, n ≤ 3, no recorded human verdicts.
- *Hypothesised gaps:* all **confirmed** (no live proof; citation + approval prompt-only — though Kiwi
  enforces refs in code; tools act immediately except two proposal paths; no session record;
  last-writer-wins selection, no knobs; no approved change/launch path).
