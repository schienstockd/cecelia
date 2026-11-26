# Architecture views

How Cecelia is put together, in pictures and tables, and where it does not line up. Most of it is
**generated from the code**, so it cannot quietly drift. The two hand-traced parts (the system view
and the flows) are pinned to the code by a test.

## Start here

1. **[`system.md`](system.md)** — the processes, their ports, and what crosses between them. One diagram.
2. **[`boundaries.md`](boundaries.md)** — where the languages meet: which file holds each end of
   each boundary, how the contract is kept, and the data files two languages share.
3. **[`flows/`](flows/)** — four journeys traced step by step across the runtimes:
   [run a task](flows/run_task.md), [run a chain](flows/run_chain.md),
   [open an image in the viewer](flows/viewer_open_image.md), [Claude through MCP](flows/claude_mcp.md).
4. **[`gaps.md`](gaps.md)** — what does not line up: two ends of a boundary that do not match, routes
   nothing calls, loops. The list to close to keep the framework coherent.

## Inside each runtime

| View | Read it when |
|---|---|
| [julia/interfaces.md](julia/interfaces.md) | Adding a task, gate, transform or HPC backend — the checklist of what a new subtype must implement |
| [julia/datamodel.md](julia/datamodel.md) | Project / Set / Image, and which areas hold the code that works on each |
| [julia/areas.md](julia/areas.md) | Which part of the backend leans on which. A size map; the two-way pairs are the part to act on |
| [python/packages.md](python/packages.md) | Runners vs the `cecelia` library vs side processes |
| [python/utils.md](python/utils.md) | The helpers everything depends on (the de-facto API), cycles, class trees |
| [vue/layers.md](vue/layers.md) | How the frontend directories depend on each other (a size map); where HTTP is spoken |
| [vue/stores.md](vue/stores.md) | The Pinia stores, which lean on which, who reads each |
| [vue/viewer.md](vue/viewer.md) | How self-contained the viewer is |

Lookup tables — every `run_py` script and the params it reads, every route area, every WebSocket
type with sender and handler — are in [`boundaries_reference.md`](boundaries_reference.md).

## For agents

- **Before changing a boundary** (a route, a WebSocket type, a `run_py` runner's params, a shared data
  file), find its row in `boundaries.md` and change both ends in the same PR.
- **After a structural change**, run `pixi run arch-diagrams` and read the diff of `gaps/*.json`. A
  gap your change added gets closed in the same PR, or named in the PR description with the reason.
- **A gap can be a false alarm** — the extractors read text, so a name built at runtime is invisible.
  Close it by making the name a literal the extractor can see, not by editing the generated files.

## Keeping it current

```bash
pixi run arch-diagrams      # regenerate every generated view and gaps.md (~15 s, no network)
pixi run test-py            # includes test_architecture_docs.py — the pins and the Julia area map
```

- Generators: `scripts/architecture/` — `julia_extract.jl`, `python_extract.py`, `vue_extract.py`,
  `boundaries_extract.py`, then `gaps.py` to compose the gap list. Each script's header says what it
  sees and what it misses.
- Hand-kept: `system.md` and `flows/*.md` (every arrow and step pinned to a file and symbol), and the
  grouping of Julia files into areas, `scripts/architecture/julia_areas.toml`. The test fails when a
  pin no longer matches the code, or when a Julia file belongs to no area.
- Diagrams are Mermaid and render on GitHub and in any Markdown preview with Mermaid support.

## Not here yet

- **Declared rules.** These views show what the code *is*. Which arrows are *allowed* — and a test that
  fails when a gap is added — is the next step, written from what these views show.
- **Freshness check.** Regenerating is by hand. A test that fails when the generated files are stale
  comes with the rules.
