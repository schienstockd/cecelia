> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in [`docs/todo/FIGURES_PLAN.md`](../todo/FIGURES_PLAN.md).
>
> **Outcome:** not run. Archived 2026-11-01. Checking it against `origin/main` showed the drift below;
> a rewrite anchored to the PI's actual ask (a high-res, high-contrast still of one cell) was then
> folded straight into `docs/todo/FIGURES_PLAN.md` instead of being run. Two premises were wrong
> besides the drift: the example strip came from a bespoke script, not a view, and figures are not
> Analysis-board output (the board is a live overview; a figure is a fixed paper artifact). Drift found:
> - **The board already has a filmstrip slot.** `filmstrip` ("Image / strip",
>   `frontend/src/components/canvas/interactiveViews.ts:93`, `ImageStripView.vue`) holds N images
>   captured from the pop-out viewer's canvas, stored as sidecar PNGs (`settings/board-assets/`) with
>   the `viewState` snapshot + `imageUid` as provenance. It is screenshot-based, not re-rendered from
>   raw — so the brief's "no structured way" and "start `sequence` from scratch" are wrong; the open
>   question is whether that snapshot becomes the spec and gets a raw-data render path.
> - **`plot_spec_id` is not an envelope.** It is a board slot's `ref`, i.e. a plot-family id from
>   `get_available_plots` such as `"track_measures"` (`mcp/cecelia_mcp/server.py:1915`) — a catalogue
>   key, not a stored per-figure spec. Q1/Q2 are answered by that.
> - **Board export is client-side**: `plots/pdf.ts` (`pdf-lib`), one A4 page per board, PDF raster or
>   SVG vector (`docs/ANALYSIS.md` "Export"). Not a server "PDF page compile".
> - **A shared still renderer already exists**: stills run on the viewer's shaders via the preview
>   worker's `render` (`preview/preview_worker.py:1549`; PR #1393, merged 2026-10-03), the same
>   `render_frames` path as movies. Q4 should start from that, not pick Julia vs Python afresh.
> - #1856 has **merged** (2026-10-31). The own-only pattern is `add_gate` / `set_gate` /
>   `delete_gate` with `dry_run` (`mcp/cecelia_mcp/gating_views.py:230`), enforced server-side by the
>   `set-own-gate` / `delete-own` routes (`api/src/gating_api.jl:1401`). There are no `set_own_*` tool
>   names; mirror the tool names, not the route names.
> - `cell_cards.jl` is `app/src/cell_cards.jl`, not `api/src/`. `pick_card_example` is used there and
>   in `api/src/motif_cards_api.jl` / `hmm_state_cards_api.jl`; `render_medoid_filmstrip` is
>   `api/src/behaviour_cards.jl:77`; `_render_still` is `api/src/gating_views_api.jl:193`.
> - Related plans the brief does not name: `docs/todo/ANIMATION_PLAN.md`, `ANALYSIS_CANVAS_PLAN.md`
>   (image/filmstrip slot, Phase D), `PROJECT_MEMORY_PLAN.md`, `KIWI_CAPTURE_AND_BLACKBOARD_PLAN.md`.

# FIGURE_SPEC_PROMPT

Read-only brainstorm + audit. Do not edit code, do not open a PR. Output is a recommendation doc.

## Context

Cecelia has an **Analysis board** that compiles plots into a PDF page. It has no structured, standardised way to build **custom plots and sequences** (e.g. a filmstrip of one cell over chosen timepoints, with channels, crop, labels and scale bar). Today an agent would have to write a notebook, or drop files in a directory, to persist one. That is not structured: nothing validates it, nothing re-renders it, nothing standardises style, and the board cannot place it.

Example to design around: a filmstrip of a single cell with a fragment inside, tdTom (magenta) + Katushka (green), frames t50 to t110 with time labels, fixed crop, same contrast across frames, a second row for a third channel. The user wants a single still from it at high resolution and contrast, and the whole strip as a reusable figure.

**Scope guards.**

- Do not design a plotting language. For the `plot` kind, evaluate adopting an existing grammar (Vega-Lite-like) or the existing `plot_spec` rather than inventing one. `sequence` is the genuinely custom part: start there and keep it small.
- The renderer and the high-resolution-from-raw path are the real cost. The spec and the agent tools are the cheap part. Size the work accordingly.
- The value must stand **without agents**: a user flow like "save this view as a figure" should be useful on its own. The agent loop is a bonus.
- A one-off native-resolution still export of a single frame does not depend on this framework; do not make it wait for it.

Proposal (challenge it): a **declarative figure spec**, stored in the project and rendered by the app, so an agent writes only the spec.

- One envelope: `id`, `kind`, `version`, `createdBy`, source references (image, populations, tracks, measures), style preset, panel size.
- `kind` carries the specifics: `plot`, `sequence` (filmstrip), later `montage`, `table`.
- One renderer in the app (Julia and/or Python worker), used by the board panel, the PDF page and the preview. Claude never writes rendering code.
- Standardisation via **style presets** (fonts, LUTs, scale bar, time label format, crop rules), set once.
- Agent loop mirrors the gating toolset in #1856: list kinds, `add_figure` with `dryRun` (returns a rendered preview), `set_own_figure` / `delete_own` (Claude-made only, `createdBy` stamp), `render_figure` as a look.
- Notebooks remain an escape hatch; recurring notebook patterns become new kinds.

PR #1192 added `open_analysis_board_plot(project_uid, plot_spec_id, measure?, pop?)` and `get_analysis_boards` / `add_analysis_board`, so a `plot_spec_id` already exists in some form. **Check whether that is the envelope.**

## Read first

- Root `CLAUDE.md`
- `docs/inventory/MCP.md`, `JULIA_API.md`, `JULIA_APP.md`, and the analysis board / blackboard / notebook docs
- Analysis board code (backend and frontend), PDF page compilation, plot spec definitions (`plot_spec_id`)
- Notebook and Blackboard persistence (how agents persist things today)
- `mcp/cecelia_mcp/gating_views.py` and #1856 for the propose / dry-run / own-only pattern
- Existing still / filmstrip / example rendering: `api/src/cell_cards.jl` (`pick_card_example`), `behaviour_cards.jl` (`render_medoid_filmstrip`), `_render_still`, the movie / animation code
- `api/src/kiwi_turn.jl` tool exclusion table; the project-memory / lab-log plans if they touch persistence

## Questions

1. **What exists.** What is a `plot_spec` today (fields, storage, who creates it, how the board and PDF consume it)? What persistence already exists for agent output (boards, notebooks, Blackboard entries, lab-log)? Which of these already approximates a figure spec?
2. **Envelope.** Can the existing plot spec be the envelope, extended with `kind`? What would change in storage, API, MCP tools and frontend? Or is a new envelope cleaner? Recommend one.
3. **Kinds.** (Keep it small; reuse an existing grammar for `plot` if one fits.) Define the minimal field set for `plot` and `sequence`. For `sequence` cover: image, population or track (fixed vs track-following crop), t list, z handling, channels with LUT and display range (shared across frames vs per frame), layout (rows, extra channel rows), labels, scale bar, time format. What is the smallest second kind that proves the envelope is general (montage? table?)?
4. **Renderer.** (This is the bulk of the work; estimate it separately from spec and tools.) Where does rendering live (Julia, Python worker, frontend canvas)? One render path for preview, board panel and PDF export? How does high-resolution export come from the **raw data** at native resolution rather than a screenshot? Vector vs raster, determinism, cost and caching.
5. **Style presets.** What is standardised, where it is stored (project, lab, global), how a preset is versioned, and what happens to existing figures when it changes.
6. **Staleness and provenance.** A figure references populations, measures, segmentations and images that can change. What must be recorded to detect staleness, how is it surfaced, and what does re-render do? How does this tie into the lab-log / Blackboard provenance?
7. **Agent loop and ownership.** Tool set, `dryRun` returning a preview Claude can look at, `createdBy` / `via: claude` stamp, own-only edits, what a user edit of a Claude figure does to ownership. Which server(s) get the tools (observer vs autonomous, Kiwi lists, allow-lists, recorders: preview = look, write = write)?
8. **Validation.** Schema validation and error messages good enough for an agent to self-correct. Limits (max frames, max resolution) so a spec cannot run away.
9. **UI.** How the board places a figure, edit in place vs agent only, how a user turns an on-screen view into a spec ("save this as a figure"), and how a figure gets into a PDF page.
10. **Migration and escape hatch.** How existing plots and notebook outputs move to the spec, and how to recognise a recurring notebook pattern that deserves a new kind.
11. **Failure modes.** Schema creep, preview/export divergence, silent defaults that make two figures look inconsistent, an agent producing a plausible but wrong figure (wrong channel mapping, mismatched contrast across frames). What guards and tests catch each?
12. **Slicing.** Smallest useful first PR (likely: `sequence` kind plus renderer plus board placement, using the filmstrip above), then follow-ups. What is checkable headlessly on UMq4v3, and what needs a live session?

## Output

- Recommendation first (3–6 lines), then answers by question number.
- Cite evidence as `file:line`. Mark each claim **verified** (read in code) or **assumed** (not checked).
- List the open decisions only I can make, with your pick for each.
- Name the top 3 ways this could go wrong, and the test that would catch each.
- Terse. No restating the context above.
