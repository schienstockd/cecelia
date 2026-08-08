# MCP board authoring — Claude adds an Analysis board, the user keeps it

**Status:** planning (2026-08-08), branch `work/mcp-boards`. Extends the observer's
design-but-don't-run split to the Analysis board — the third artefact after notebooks
([`NOTEBOOK_PLAYGROUND_PLAN.md`](NOTEBOOK_PLAYGROUND_PLAN.md)) and chains.

## Goal

*"can you plot out the key pieces in project 4kS67f"* should produce a board worth looking at.

Dominik: *"claude do add board only … it can only add one board at a time. it cannot modify or delete
boards. add yes. modify no. delete no."*

One MCP tool that **adds one board**, previewed before it lands, validated against real project state,
and merged into the project without fighting the browser.

## Why this is not just "let the MCP POST the board JSON"

Boards already persist: the frontend autosaves `{tabs, layouts}` to `settings/analysisBoards.json` via
`POST /api/projects/boards`. So a write surface exists. Three things make the naive version wrong.

1. **That route is a verbatim whole-document overwrite.** Its own handler documents the payload as
   *"Opaque frontend JSON, stored verbatim"* — it never parses it. Allowlisting it would let Claude
   clobber every board in the project, and the server could not validate a single field.
2. **The on-disk layout is the wrong wire format.** `LayoutEntry` carries `slotAreas`, `gridArea`
   strings, `rowTracks`, `vis` bags and `tkey`-encoded selections. Claude emitting that is unreadable
   in a preview, unvalidatable by the server, and coupled to a schema that has already grown fields
   (`sheet`, `rowTracks`) with back-compat reads.
3. **It races the browser.** The frontend POSTs the whole payload on any change (800 ms debounce) and
   only reads it at project open. Write while the app is open → the next autosave clobbers you; write
   while it is closed → invisible until reload.

## Decisions (2026-08-08)

1. **Add-only. No board versioning — dropped 2026-08-08.** A brand-new board, never a touched one.
   **No modify-in-place, no delete, no rename, no reorder** — those are the user's, in the GUI.

   An earlier draft had a Phase 4 giving boards notebook-style snapshot/restore. Dominik: *"i'm not
   sure we need restorable artefacts for boards? … I don't want another parallel versioning system."*
   Correct on both counts:
   - **Nothing needs restoring.** Versioning exists for notebooks because `revise` overwrites the
     user's own hand-written Pluto cells. Under add-only nothing is ever overwritten, and a board Claude
     got wrong is one click to delete — a board is cheap to regenerate from its spec, which is the whole
     point of Decision 2. The clutter worry that motivated Phase 4 is solved by deleting a tab.
   - **The notebook system would not transfer cleanly anyway.** It is *file*-based:
     `cp(src, .snapshots/<stem>@v<N>.jl)` plus a per-project registry tracking `current`
     (`api_notebooks_snapshot`). A board is not a file — it is one subtree of the single shared
     `analysisBoards.json`. Generalising would mean an abstraction that is a union of "copy a file" and
     "extract a JSON subtree", which is worse than either.

   **If boards ever do need versions**, the order is: make each board its own file *first* (which
   independently simplifies the Decision 6 concurrency problem), and only then reuse the notebook
   snapshot primitive. Do not build a second versioning system.

   NB the `version` counter in Decision 6 is a different thing entirely — an optimistic-concurrency
   sequence number on the document, not artefact history.

2. **A semantic spec at the MCP boundary, expanded server-side.** Claude sends what the board should
   *show*; the server turns it into a `LayoutEntry`. **One call, not two** — the expansion is the
   server's, exactly as `create_chain` takes `nodes`/`edges` and the server fills `start_targets` and
   validates before writing. This is the same boundary discipline `get_module_params` already applies
   in the other direction (strip spec bloat at the MCP edge, leave the frontend's route untouched).

3. **The compulsory preview is Claude Code's MCP permission prompt** — harness-enforced, so it cannot
   be skipped. A "show the user first" instruction in a docstring is *not* a gate: this repo already
   shipped `analysisBoard: true` as a flag wired to nothing, and nothing failed. **The spec must
   therefore stay readable** — if it grows to a page of layout knobs it has stopped being a preview.

4. **Layout control stops at the plot list.** Claude picks which plots, in what order, and optionally a
   named template (`2x2`, a comic-plate id). The server assigns grid areas. Dragging, resizing and
   captions stay in the GUI.

5. **A separate create-only route, not the autosave one.** `POST /api/boards/add`, 409 on an existing
   tab name — mirroring `/api/chains/create` being deliberately distinct from `/api/chains/save`
   ("NOT /api/chains/save, which overwrites"). Only the new route is allowlisted in the MCP client.

6. **Server-side merge + a document version + a WS push.** The route appends one tab to the existing
   document; the boards JSON gains an integer `version`; the frontend's autosave sends the version it
   read and a stale write is rejected with 409, on which it reloads. A "boards changed" broadcast over
   the existing `api/src/sockets.jl` machinery makes an open app pick the new board up, reusing the
   `_restoring` suppression already in `analysisLayout.ts` (900 ms > the 800 ms debounce).

   **This fixes an existing bug.** Because the save is a debounced verbatim whole-document overwrite,
   two browser tabs open on the same project already clobber each other's boards today — nothing to do
   with MCP.

7. **Read-side gaps are prerequisites, not follow-ups.** Without them the tool can add boards but not
   *good* ones, which is the whole point. They are read-only, so they need no permission debate.

## What the audit found in `4kS67f` (2026-08-08)

Probed live via the observer client. The metadata is rich enough to choose well — measures arrive with
`n`/median/quartiles *before* plotting, 9 summary specs declare their `chartTypes` and `dataSource`, and
the movement clustering produced semantically meaningful pops (`/Scanning`, `/Directed`, `/Meandering`)
tied to their run by `filter: {measure: "clusters.movement"}`.

Four things would make an unattended attempt produce bad plots:

| Hazard | Detail | Mitigation |
|---|---|---|
| **Junk cluster runs** | 5 suffixes: `movement`, `immune`, and `here` / `there` / `test`. Nothing marks which is canonical. | Docstring discipline (below) — validation cannot fix intent. |
| **Leftover pops** | `/Population 1` sits beside the real ones and would land on a figure looking like a mistake. | Same. |
| **Attributes invisible to the observer** | The `B and T` set **does** carry `Mouse` (1-4) and `Location` (a-d) — checked live via `/api/plots/attrs`. Neither reached Claude: `list_images` returned no `attr` and no tool exposed the axes. So *compare by attribute*, the most valuable axis, was unusable on data that supports it. (An earlier note here said mouse identity lived only in filenames — wrong.) | **Phase 0 — done.** |
| **Thin n** | `B/qc` motility is 9 tracks on `ii9Qvg`. Per-image boxplots of 9 are weak; pooled across the 8-image set they are fine. | Claude can see `n`; the docstring must make it choose pooling. |

Also: one image is excluded (`excludedCount: 1`) and correctly surfaced, and `get_analysis_lineage`
returns `boards: []` — it cannot read back what exists.

## Phases

### Phase 0 — read-side prerequisites (read-only, ship alone)
- **Attributes — DONE.** Two distinct things, deliberately not merged:
  - the **axes** (what you may group by) — `get_image_attributes`, which reads the *existing*
    `/api/plots/attrs`, the same route the summary compare picker and the UMAP colour/facet picker use.
    Exposing it rather than inventing a second attribute surface (`useImageAttrs.ts` calls it "the ONE
    fetch"). GET-only, so the write allowlist is untouched — pinned by a test.
  - the **assignment** (which image has which value) — `attr` added per image to `/api/images`. Needed
    to size the groups: the axes say what you may group by, this says how many images land in each
    group once excluded ones are dropped. A group of one is not a comparison.
- **Board read-back — DONE.** `board_summaries` (`app/src/ai/lineage.jl`) → `GET /api/analysis/boards`
  → `get_analysis_boards`. Per board: `{name, cols, rows, plots: [{slot, kind, ref, measure?, chart?,
  popType?, pops?, title?}]}`, `tkey`s decoded to `valueName/pop`, empty slots omitted. A SUMMARY, never
  the stored geometry — and deliberately the **same vocabulary the write side will accept** (Decision 2),
  so read and write describe a board the same way. `_board_tabs` stays the cheap name-only view lineage
  embeds; its "plot detail is not exposed here" note now points here. Every field is optional by
  construction (the file is frontend-written), covered by degradation cases in the package suite.

**Checkpoint:** ask Claude "what would you plot for 4kS67f" with no write tool; judge the answer. **If
the suggestions are not good here, stop — the rest of the plan only makes bad plots faster.**

### Phase 1 — make the boards document safe to write concurrently
- Add `version` to `analysisBoards.json`; `POST /api/projects/boards` rejects a stale version with 409.
- Frontend: send the last-read version; on 409, `load()` and retry.
- WS "boards changed" broadcast + frontend reload through the existing `_restoring` path.

**Checkpoint:** two browser tabs on one project stop clobbering each other. Shippable on its own as a
bug fix, independent of everything below.

### Phase 2 — the board spec and its server-side expander
- Define the semantic spec (Decision 2/4). Sketch, to be pinned in Phase 2:
  ```
  { name: "B vs T motility",
    template: "2x2",
    plots: [ {plot: "track_measures", measure: "live.track.speed", chart: "boxplot",
              pops: ["B/qc", "T/qc"], compare: "pooled"}, … ] }
  ```
- Expander → `LayoutEntry` (grid areas, slot state, `tkey` selections, `vis` defaults).
- **Validator** against live project state: known `specId`, chart offered by that spec, populations that
  exist, measures present. A bad `tkey` currently renders an empty plot with **no error** — that is the
  failure this closes.
- Pure Julia, headless-testable: package tests for expand + reject cases.

**Checkpoint:** a spec round-trips to a board the GUI renders identically to a hand-built one.

### Phase 3 — the MCP tool
- `POST /api/boards/add` (create-only, 409 on an existing tab name), added to the client allowlist as
  write 6/6.
- `add_analysis_board(project_uid, name, plots, template="")` — one board per call.
- Docstring carrying the `create_chain` discipline, which is what actually governs quality:
  **resolve what is resolvable first** (lineage, populations, measure summary, existing boards), pick
  the canonical clustering run rather than guessing, drop excluded images, prefer pooling at small `n`,
  and **say in chat what was inferred versus read**. State plainly that the board is added beside the
  user's own and can be deleted in the GUI.

**Checkpoint:** the permission prompt is readable enough to approve or reject on sight.

### Phase 4 — (removed)
Board versioning was cut on 2026-08-08; see Decision 1 for why, and for the order to follow if it is
ever revived.

## Risks

- **Validation cannot check intent.** Nothing will stop a well-formed board built on the `test`
  clustering run. Phase 0's checkpoint is the real quality gate, not the validator.
- **Preview legibility is load-bearing** (Decision 3). Any future layout knob trades directly against
  the only compulsory gate in the design.
- **Phase 1 touches a path every board save goes through.** It is a bug fix, but a hot one — it wants
  the two-tab case tested explicitly, not assumed.

## References

- [`ANALYSIS_CANVAS_PLAN.md`](ANALYSIS_CANVAS_PLAN.md) — the board, its slots, the docked rail.
- [`CANVAS_MANAGER_RAIL_PLAN.md`](CANVAS_MANAGER_RAIL_PLAN.md) — how a slot declares what it needs.
- `docs/ANALYSIS.md` — board persistence keys, the plot-hosting registries.
- `docs/ai-assist/OBSERVER.md`, `mcp/README.md` — the observer's no-mutation guarantee and the write allowlist.
- `mcp/cecelia_mcp/server.py` → `create_chain` — the precedent this mirrors, docstring included.
