# Observer Phase 2 — Actionable suggestions, notebook generation, session entry

Parked plan. Builds on the working observer data-access foundation (Slices A–E, see
`docs/ai-assist/OBSERVER.md` and `OBSERVER_DATA_ACCESS_PLAN.md`). Source brief:
`docs/prompts/phase2-observer-prompt.md` — this doc is the **corrected, locked** version
of it. Read both; where they disagree, this doc wins (the brief was written without the
real REPL interface).

**Core principle (unchanged):** Cecelia works without Claude. Everything here is
Claude-*optional*. Cecelia's own QC/cohort/reporting functions stand alone; Claude adds
suggestions, notebooks, and a guided session entry on top.

---

## Ground-truth corrections to the brief (verified against the code)

The brief was written before the real interface was checked. These are wrong in it and
right here:

| Brief says | Reality |
|---|---|
| `pop_dt(...)` | `pop_df(img, pop_type, pops; value_name=…)` |
| `get_image(project; uid)` / `get_images(project; set_uid)` | `images(proj)` / `images(set)` / `sets(proj)`; **no image-by-uid helper** — filter by `.uid` (we add a tiny `image(proj; uid=)` convenience) |
| `track_measures(img; value_name=)` as a read accessor | `track_measures` is a **task** (`TrackMeasures`). Track data reads via `track_props` / `track_cell_measures` / `is_tracked` and `pop_df` |
| "Chat to Claude launches a session; pass a startup payload / `_build_claude_cmd`" | It **copies a starter prompt to the clipboard** (`buildChatPrompt` → clipboard in `LabLogPanel.vue`). No launch command exists. Context is delivered by a tool the pasted prompt calls, not an injected payload |
| "Reuse the project versioning system for notebooks" | Notebooks **already have** their own snapshot/registry versioning (`.snapshots/<name>@v<N>.jl` + `notebooks.json`, see `docs/NOTEBOOKS.md`). Reuse **that**, not project-state versioning |
| "Read param JSON at `inputDefinitions/`" | Real specs are the co-located task `<name>.json`, already served by `api_task_definitions` (`GET /api/tasks/definitions`). Reuse that route |
| "Parameter history per image from the run log" | **The run log stores no params** (`append_run_log!` = fun/valueName/status/at only). Current last-used params come from `read_module_fun_params`. Historical param↔outcome correlation is **not** available — see §1 boundary |

Already true (no work needed): `append_lab_log` MCP tool exists; observer analysis tools
(lineage/populations/measures/behaviour/clusters/chains/cohort-QC) exist; Pluto engine +
notebooks exist.

---

## Locked decisions

1. **How Claude knows the REPL — introspection-backed doc, drift-guarded by a test.**
   - One curated `const NOTEBOOK_API::Vector{Symbol}` in the package = the notebook-safe
     read-accessor allow-list (`load_project`, `images`, `sets`, `image`, `pop_df`,
     `label_props`, `select_cols`/`view_*`/`as_df`, `track_props`, `track_cell_measures`,
     `is_tracked`, `img_value_names`, `plot_summary_data`, …).
   - An introspection fn `repl_api_reference()` reads the **live package** for each symbol:
     exported? + `Base.Docs.doc` docstring + `methods()` signatures → structured entries.
   - `docs/REPL.md` = human cookbook (the `|>` chain idiom, pop_type/value_name model, the
     **notebook write rules**) **plus** a generated `<!-- BEGIN GENERATED API -->…<!-- END -->`
     section produced from `repl_api_reference()`.
   - **Drift guard (the "wire it in"):** a `test-pkg` golden test regenerates that section
     and asserts it equals what's committed. Change a documented signature without updating
     `REPL.md` → CI red. There is a `pixi run gen-repl-doc` (or a Julia script) to regenerate.
   - Claude reads `REPL.md` as an MCP **resource** (`get_repl_api` tool returns the same
     structured reference for programmatic use). Per-project structure (actual value_names /
     pops / columns) keeps coming from the existing `populations_summary`/`measure_summary`.

2. **Session context = `get_session_briefing` MCP tool**, not an injected payload. The
   clipboard prompt (`buildChatPrompt`) tells Claude to call it first. Returns: project
   name + image count, images currently at 🟡/🔴, recent lab-log entries (last 7 days).
   Small, live, no launch mechanism needed.

3. **§1 param suggestions are IN scope, bounded honestly.** Claude cites the *current*
   param value (`read_module_fun_params`) + valid range (task JSON via `get_module_params`)
   + current QC/cohort outcome, and suggests a direction. It does **not** claim historical
   "params X → outcome Y" correlation — the run log doesn't store params, so that regression
   isn't buildable without new per-run snapshotting (explicitly deferred, noted in the tool
   docstring). Suggestions land as `[Claude]` 🟡 lab-log entries framed "suggestion, not
   instruction". Read-only; user approves any actual re-run.

4. **Notebook writes are figures + CSV only.** Never `.h5ad`, qc store, lab log, ccid.json.
   Enforced by convention in `REPL.md` + the notebook generation guidance; notebooks use
   `using Cecelia` only (no raw HDF5/zarr, no `sys.path`).

5. **No new launch mechanism, no GUI plot creation** (brief §7 stays deferred — Pluto is the
   safe write path). No automatic param adjustment. No cross-project learning.

---

## Build order

Foundation first (everything leans on the REPL layer), then the features, panel last.

1. **REPL knowledge foundation** — ✅ **done**: `NOTEBOOK_API` const + `repl_api_reference()`
   (live docstring introspection) + `docs/REPL.md` (cookbook + generated section, golden-tested) +
   `image_by_uid(proj/s; uid=)` convenience + `GET /api/repl/api` → `get_repl_api` MCP tool.
   *(app/ + api/ + mcp/ + docs/ + test-pkg/api/mcp)*
2. **`get_module_params` MCP tool** — expose the existing task-definition specs (valid
   ranges/defaults/types) to Claude. Route already exists; add the allow-list entry + client
   method + server tool. *(mcp/ + allow-list; possibly a thin `/api` shim if the current one
   isn't shaped for it)*
3. **§1 param suggestions** — `read_module_fun_params` exposure (current params per fun/image)
   + the suggestion pattern documented in the observer prompt so Claude produces range-valid,
   QC-anchored 🟡 suggestions. *(mcp/ + app/ accessor exposure + observer_prompt.jl)*
4. **`get_session_briefing` MCP tool** + `buildChatPrompt` update to call it first.
   *(mcp/ + frontend/ chatHandoff.ts + test-frontend)*
5. **Notebook generation guidance** — document the collaborator-request→notebook workflow so
   Claude generates a correct `using Cecelia` Pluto notebook via the existing notebooks
   registry/snapshot system; verify the generated patterns run. *(docs/ + mcp/ prompt; leans
   on `docs/NOTEBOOKS.md`)*
6. **`get_available_plots` MCP tool** — from the existing plot JSON specs (name/module/data
   needs/scope modes). *(mcp/ + allow-list + a `/api/plots/available` read if not already
   served)*
7. **In-app Claude overview panel** — static reference panel (what Claude can see / suggest /
   create / cannot do / can write). Verified against real capabilities. *(frontend/)*

Each step ships with its tests (`test-pkg`/`test-api`/`test-mcp`/`test-frontend`) and doc
updates (`docs/ai-assist/OBSERVER.md`, `mcp/README.md`, `docs/API.md`, `docs/NOTEBOOKS.md`)
in the same change. Land as focused PRs, not one mega-PR.

---

## Boundaries / non-goals

- No historical param↔QC regression (run log has no params). §1 is current-state only.
- No writes outside notebook figures/CSV and lab-log `[Claude]` entries.
- No task auto-run; Claude suggests, user approves.
- No GUI-canvas plot creation (deferred; Pluto is the write path).
- No cross-project learning; stay within the current project.

---

## Verification (from the brief, kept)

- `get_repl_api`/`REPL.md` reflect the real exports; the golden test fails on undocumented
  signature drift.
- Chat-to-Claude opens with context loaded (Claude calls `get_session_briefing`) — user
  doesn't re-explain.
- Claude suggests a range-valid param value for a flagged image, citing current value + QC.
- "Make a notebook for this" → a `using Cecelia` Pluto notebook via the existing snapshot
  versioning; runs clean, produces figure/CSV, touches no data store.
- `get_available_plots` returns the plot-type list Claude uses to suggest a visualization.
- The overview panel accurately describes real capabilities.
