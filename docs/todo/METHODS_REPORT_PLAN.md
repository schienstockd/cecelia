# Methods report — a draft Methods paragraph from what the project already records

**Status:** planning (2026-10-29). Nothing built. Prompted by the Janelia *AI in Microscopy* guide
(ch. 3 §3.6–3.8: "Document models, prompts, and validation in methods"; ch. 11: data "with clear
provenance"), and by the model stamps that landed in #1826.

## The ask

A user can produce a **draft Methods paragraph** (plus a supplementary table) for a set of images:
which steps ran in which order, with which parameters and software versions, which populations were
gated, and **which steps Claude designed or wrote** and with which model. Today none of this is
reachable from the UI: `get_analysis_lineage` exists only for Claude (MCP + `/api/analysis/lineage`).

## What already exists (the inputs)

| Input | Where | Gives |
|---|---|---|
| Pipeline per image | `analysis_lineage` (`app/src/ai/lineage.jl`) → `steps` from the run log | `{stage, fun, valueName, status, at}` in order |
| Parameters | ccid.json `meta["funParams"][fun]` (+ per-output-name blobs, `app/src/model/image.jl`) | what each step ran with |
| Who / which model | `author_stamp()` = `{profile, via, model?, session?}` on chains, blackboard, notebooks, boards, LabArchives context; run log `by`; lab log `[Claude · <model>]` | which artefacts Claude authored, and with what |
| Populations | `analysis_lineage` → `gatedPops`, `clusterRuns` | what was gated / clustered |
| Software | the app version (`/api/version`), the task's tool (cellpose / coastal / btrack …) | versions to cite |

## Locked decisions

1. **Deterministic template, not LLM prose.** The report is assembled from the records by code, so the
   same project always produces the same text and nothing is invented. Claude may *polish* it on
   request (Kiwi / a terminal session), but the source of truth is the template. A hallucinated Methods
   section is the failure mode the guide warns about first (ch. 3 §3.8).
2. **Draft, clearly marked.** The output says it is a draft generated from the project record and
   must be checked by the author — the guide's "independently verified before use in research".
3. **Claude's part is stated, by model.** Any step whose chain, board, notebook or blackboard entry
   carries `via: claude` is named as AI-assisted, with the recorded model ID. Unknown model (records
   from before #1826, or a session without the hook) is written as "model not recorded", never guessed.
4. **The lab log is not the store.** The lineage is computed from records that already exist;
   nothing new is written to the lab log (it stays the human-readable *why*).
5. **One generator, two front doors.** A pure Julia function produces the report; a route serves it to
   the UI and the observer MCP gets a read tool on the same route. No second assembly path.

## Open questions (his call)

- **Where the button lives.** The Analysis board's `⤓ Export` dropdown (beside PDF/CSV) reads
  naturally — "export the methods for what this board shows". Alternative: the project/set panel.
- **Scope unit.** Per set (the natural experiment unit) or per board (what is actually in the figure)?
- **Format.** Markdown + a CSV/TSV parameter table is the minimum; DOCX is a later nicety.
- **Paired-terminal suggestions.** A step Claude *suggested* and the user clicked is stamped as the
  user. The session id on Claude's own writes points at the transcript, but the transcript lives on the
  machine where Claude Code ran. Accept that ceiling, or offer an opt-in "attach session summary"?

## Phases

- **P0 — stamp coverage audit.** Every Claude write route stamps (done in #1826 for boards + LabArchives;
  re-check the MCP allow-list when a write route is added). Decide whether the UI shows the model
  (hover on "by Claude").
- **P1 — generator.** `methods_report(proj; set_uid | board)` → `{markdown, table}` from `analysis_lineage`
  + `funParams` + stamps; per-task one-line templates live beside each task spec (a `methodsText` field
  with `{param}` slots), so a new task documents itself and a missing template is a test failure.
- **P2 — route + UI.** `GET /api/analysis/methods`, the Export entry, a preview modal with copy.
- **P3 — MCP read tool + guidance** so Claude can draft/polish from the same text.

## Touchpoints (estimate)

P1: new `app/src/ai/methods_report.jl` + `methodsText` on task specs (the `tip` ratchet pattern) + tests
(~4 files + one line per task JSON). P2: one route, one modal, Export dropdown entry (~4 files).
P3: `server.py` + `guidance.py`.
