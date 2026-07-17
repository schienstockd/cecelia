# Audit: Codebase Inventory + Duplicate Detection → INVENTORY.md + CLAUDE.md Update

Opus audit pass. Read the entire codebase before producing anything. This is not a planning task — it is a ground-truth audit. Sonnet will use the output to create `INVENTORY.md` and add a discovery-first rule to `CLAUDE.md`.

---

## Why this exists

The codebase has grown large enough that fresh context windows don't know what already exists. The result: duplicate implementations. Two shutdown buttons. Zarr access hand-rolled instead of using the utils. Napari bridge reimplemented instead of using the existing one. Every duplicate costs more to fix than a discovery step would have cost. This audit produces the inventory that prevents future duplicates.

`CLAUDE.md` stays focused on rules and patterns — how to behave, what not to do, hard constraints. `INVENTORY.md` is the living index of what exists and where. They are separate files for a reason: `CLAUDE.md` must be short enough to be read every session; `INVENTORY.md` can be as long as the codebase needs.

---

## Step 1 — Full codebase read

Read every file in:
- `app/src/` — Julia package
- `app/py/` — Python utilities and task scripts
- `frontend/src/` — Vue components
- `api/` — Julia API routes and handlers
- `napari/` — Napari bridge

Do not skim. The point is to find things that exist, not to understand the architecture (ARCHITECTURE.md covers that). Pay specific attention to utility functions, shared components, and any function that reads/writes files, opens network connections, or handles a UI concern like buttons, panels, or modals.

---

## Step 2 — Find all duplicates

For each of the following categories, list every implementation you find — file, function/component name, what it does. Flag any category where more than one implementation exists:

**Data access**
- Zarr file reading (how many places read zarr directly vs. via a util?)
- H5AD / label props reading (should all go through `LabelProps` chain — find any that don't)
- H5AD writing (should go through `add_obs`/`save!` — find any that don't)

**IPC / bridges**
- Napari WebSocket connection (should be one — find all)
- Julia ↔ Vue WebSocket (should be one handler — find all)

**UI components**
- Shutdown action (button, handler, or any code that stops the Julia server)
- Update action (button, handler, or any code that triggers an update)
- Image table (the shared component used for selecting images — find all instances and confirm they use the same component vs. hand-rolled copies)
- Any modal, panel, or floating window that appears in more than one place — check whether it's a shared component or reimplemented each time

**Task system**
- Task log writing (should go through one path — find all)
- Task status updates to the frontend (should go through one WebSocket push — find all)

For each duplicate found: name the files, describe what each does, and recommend which is canonical and which should be removed.

---

## Step 3 — Produce INVENTORY.md

From ground truth — not from ARCHITECTURE.md, not from memory — produce the complete content for a new file `INVENTORY.md` at the repo root. Format:

```markdown
# Inventory

Living index of key existing components. Maintained by Sonnet — when you add a
significant new component, add it here. When a task touches something in this list,
find it and use it — don't rebuild it.

Last audited: [date]

## Data access
- **LabelProps chain (Julia)**: `app/src/label_props.jl` — lazy H5AD reader/writer,
  use for all cell-level reads and obs column writes
- **[next entry]**: ...

## Python utilities
- **[name]**: `app/py/utils/[file]` — [what it does]

## Vue components
- **[name]**: `frontend/src/[path]` — [what it does]

## Julia API
- **[name]**: `api/[path]` — [what it does]

## Napari bridge
- **[name]**: `[path]` — [what it does]
```

Every entry must be a file path you actually found in the codebase, not one you inferred or guessed.

---

## Step 4 — Minimal CLAUDE.md addition

Add one short section near the top of `CLAUDE.md`, after the opening architecture summary. Nothing else in `CLAUDE.md` changes:

```markdown
## Before implementing anything — mandatory discovery step

Before writing any code, search the codebase for existing implementations of
everything the task touches.

1. Check `INVENTORY.md` for canonical components — use them, don't rebuild them
2. Search with grep/find for the specific function, component, or pattern
3. Report what you find before writing any code
4. Build on what exists — only build new if the search comes up empty
5. If in doubt: search, don't build

Using a hand-rolled solution when a util or component already exists is a bug,
not a style choice.
```

---

## Step 5 — Produce the Sonnet execution prompt

Once the audit is complete, produce a tight Sonnet prompt that:
- Creates `INVENTORY.md` with the full ground-truth inventory from Step 3
- Adds the discovery-first rule to `CLAUDE.md` from Step 4
- Lists every duplicate to remove or consolidate, with the canonical version named explicitly
- Tells Sonnet not to reorganise or rewrite anything else in `CLAUDE.md`

The Sonnet prompt is the deliverable. The audit findings are the input to it.

---

## What not to do

- Do not guess file paths — read the files
- Do not infer what exists from the architecture docs — find it in the code
- Do not produce a Sonnet prompt until the audit is complete
- Do not skip Step 2 — the duplicate detection is the most important part
