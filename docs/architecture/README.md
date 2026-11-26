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
4. **[`gaps.md`](gaps.md)** — where the code breaks the rules below: two ends of a boundary that do
   not match, calls up the layer order, loops. The list to close; its counts may only go down.

## Inside each runtime

| View | Read it when |
|---|---|
| [julia/interfaces.md](julia/interfaces.md) | Adding a task, gate, transform or HPC backend — the checklist of what a new subtype must implement |
| [julia/datamodel.md](julia/datamodel.md) | Project / Set / Image, and which areas hold the code that works on each |
| [julia/areas.md](julia/areas.md) | Which part of the backend leans on which. A size map; the calls up the declared layer order are the part to act on |
| [python/packages.md](python/packages.md) | Runners vs the `cecelia` library vs side processes |
| [python/utils.md](python/utils.md) | The helpers everything depends on (the de-facto API), cycles, class trees |
| [vue/layers.md](vue/layers.md) | How the frontend directories depend on each other (a size map); where HTTP is spoken |
| [vue/stores.md](vue/stores.md) | The Pinia stores, which lean on which, who reads each |
| [vue/viewer.md](vue/viewer.md) | How self-contained the viewer is |

Lookup tables — every `run_py` script and the params it reads, every route area, every WebSocket
type with sender and handler — are in [`boundaries_reference.md`](boundaries_reference.md).

## Rules, and how they are enforced

The views show what the code *is*; three declared rules say what it should be. A break of any rule
is a gap in `gaps.md`, and **the count of each gap kind may only go down**:
`python/cecelia/tests/test_architecture_gaps.py` (part of `pixi run test-py`, so CI) fails a change
that adds one, naming it, and fails again once gaps are closed until the ceiling in
`scripts/architecture/gaps_baseline.json` is lowered to match.

| Rule | Declared in | Gap kind |
|---|---|---|
| Both ends of every boundary match: no WebSocket type sent without a handler or handled without a sender, no route nothing calls, no runner nothing launches, every shared data file named by a test | `boundaries_extract.py` | `ws-*`, `route-uncalled`, `runner-unlaunched`, `shared-file-unguarded` |
| Julia areas call down the declared layer order, never up | `[layers]` in `scripts/architecture/julia_areas.toml` | `julia-upward` (live in `test-pkg`) |
| The viewer engine (`lib/webgpu`, `utils/viewer`) takes inputs and imports no app store but its own | `vue_extract.py` | `viewer-store` |
| The frontend calls the backend through `frontend/src/utils/api.ts` (status checked, failures logged), never a bare `fetch(` | `vue_extract.py`; `frontend/CLAUDE.md` | `fetch-direct` |
| No import loops between Python modules or between Pinia stores | `python_extract.py`, `vue_extract.py` | `py-cycle`, `store-cycle` |

The Julia kind needs the package loaded, so `test_architecture_gaps.py` reads it from the committed
`gaps/julia.json`. The live check is in the package suite instead: the *julia-upward ratchet* testset
(`app/test/suite/ratchets.jl`, part of `pixi run test-pkg`, so CI on every PR) computes the upward
calls from the code with the extractor's own `upward_gaps` and holds them to the same ceiling, with
the same two failures. `pixi run arch-diagrams` refreshes `gaps/julia.json` for the docs.

**On purpose, not a gap.** A route kept without a caller on purpose (a smoke test, a consumer to
come) carries `# ARCH-EXEMPT: <reason>` on its line in `api/src/server.jl`; it is listed with its
reason in `boundaries_reference.md` and counted by neither this test nor the weekly judge. Nothing
else can be exempted: any other false alarm is fixed by making the name visible to the extractor.

**Closing them is scheduled, not hoped for.** Each weekly judge pass ends with `judge-gaps`: the
easiest few gaps, one unattended session and PR each, never merged by itself. Gaps that need a
decision (deleting a route nobody calls, a design choice) come back to the maintainer instead.
[`docs/ai-assist/WEEKLY_JUDGE.md`](../ai-assist/WEEKLY_JUDGE.md) → *Coherence gaps*.

## For agents

- **Before changing a boundary** (a route, a WebSocket type, a `run_py` runner's params, a shared data
  file), find its row in `boundaries.md` and change both ends in the same PR.
- **When `test_architecture_gaps.py` or the julia-upward ratchet fails**, it names the gap your change added and how to close it.
  A false alarm (a name built at runtime) is closed by making the name a literal the extractor can
  see — never by raising the baseline.
- **After closing gaps**, `pixi run arch-diagrams && pixi run arch-gaps-baseline`, and commit both.
- **After a structural change**, `pixi run arch-diagrams` so the views and `gaps/julia.json` match.

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

- **Freshness of the views.** The generated pages are refreshed by hand; only the gap counts are
  checked on every change (the Julia upward calls by `test-pkg`, the rest by `test-py`).
