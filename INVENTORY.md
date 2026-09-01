# Inventory — index

Living index of the key existing components — the things a new task should **find and reuse**, not
rebuild. This is the *what exists and where* companion to `CLAUDE.md` (*how to behave*) and
`docs/ARCHITECTURE.md` (*how the layers fit together*). Intentionally not exhaustive: leaf task files
and one-off components are omitted; shared/cross-cutting things are not.

**Open only the area you're touching.** The content was one 145 KB file until 2026-08-20, which meant
every discovery step paid ~39k tokens to read eight areas to use one. Each area file below is
self-contained.

| Area you're touching | Open | Size |
|---|---|---|
| Anything cross-cutting — shutdown, update, the WS rails, the log rail, image selection, task gating | [`docs/inventory/FLOWS.md`](docs/inventory/FLOWS.md) | 4 KB |
| Reading/writing cell data, images, label stores, populations | [`docs/inventory/DATA_ACCESS.md`](docs/inventory/DATA_ACCESS.md) | 19 KB |
| `frontend/src/` — Vue components, stores, utils | [`docs/inventory/FRONTEND.md`](docs/inventory/FRONTEND.md) | 68 KB |
| `app/src/` — the Julia package: model, scheduler, gating, tasks | [`docs/inventory/JULIA_APP.md`](docs/inventory/JULIA_APP.md) | 37 KB |
| `api/src/` — HTTP/WS handlers | [`docs/inventory/JULIA_API.md`](docs/inventory/JULIA_API.md) | 8 KB |
| `python/cecelia/` — utils, writers, task runners | [`docs/inventory/PYTHON.md`](docs/inventory/PYTHON.md) | 8 KB |
| `mcp/` — the read-only observer server | [`docs/inventory/MCP.md`](docs/inventory/MCP.md) | 9 KB |

**FRONTEND.md and JULIA_APP.md are still big — slice them.** `grep -n -i '<thing>' docs/inventory/FRONTEND.md`
finds the entry directly; the file is a flat bullet list, so one grep hit is the whole answer and you
never need to read the file.

## The rules you cannot skip, even if you read nothing else

These are the duplications that have actually cost us. Full detail in `CLAUDE.md`:

- **Cell data** — `label_props(...) |> … |> as_df` (Julia) / `LabelPropsView(...)` (Python). Never `h5open`/`h5py`/`anndata` on an existing `.h5ad`.
- **Images / label stores** — `python/cecelia/utils/zarr_utils.py` only. Never a bare `zarr.open`/`tifffile.imread`, never read `.zattrs` yourself.
- **Spawning Python** — `run_py` (`app/src/py_runner.jl`). Never a hand-rolled `run(pipeline(...))`.
- **Channel names → indices** — `channel_indices` + `channel_names` (`app/src/model/image.jl`).
- **Any UI control** — the canonical component in [`docs/ui/PRIMITIVES.md`](docs/ui/PRIMITIVES.md). Never a new variant of a primitive that already exists.

## Keeping this current

When you add a significant new shared component (a shared util, a canonical helper, a reusable Vue
component, a new API handler file), add a line to the matching area file in the same change — not to
this index. Only add a row here if you create a genuinely new *area*.

Last audited: 2026-07-16 (full six-area ground-truth read; against `main` @ c1ce165).
