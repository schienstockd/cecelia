# Architecture views

Pictures of how Cecelia is put together, per runtime, for a human reader. Mostly **generated from
the code** so they cannot silently go stale; the one hand-traced view is pinned to the code by a test.

```bash
pixi run arch-diagrams      # regenerate every generated view (~15 s, no network)
pixi run test-py            # includes test_architecture_docs.py — the pins and the Julia area map
```

Diagrams are Mermaid: they render on GitHub and in any Markdown preview with Mermaid support.

| View | Read it when | How it is made |
|---|---|---|
| [`system.md`](system.md) | You want the whole picture: which processes exist, on which port, what crosses between them | **Hand-traced**, every arrow pinned to `file` + symbol |
| [`julia/interfaces.md`](julia/interfaces.md) | Adding a task, gate, transform, HPC backend: what must a new subtype implement? | Generated — introspection of the loaded `Cecelia` (`subtypes`, `methods`) |
| [`julia/datamodel.md`](julia/datamodel.md) | Project / Set / Image and where the code that works on each lives | Generated — struct fields + method signatures |
| [`julia/areas.md`](julia/areas.md) | Which part of the backend leans on which; two-way dependencies | Generated — static name references; areas curated in `scripts/architecture/julia_areas.toml` |
| [`python/packages.md`](python/packages.md) | Runners vs the `cecelia` library vs side processes | Generated — `ast` imports |
| [`python/utils.md`](python/utils.md) | Which helpers everything depends on (the de-facto API), cycles, class trees | Generated — `ast` imports + attribute uses |
| [`vue/layers.md`](vue/layers.md) | How the frontend directories depend on each other; where HTTP is spoken | Generated — import statements |
| [`vue/stores.md`](vue/stores.md) | The Pinia stores, which lean on which, who reads each | Generated — import statements |
| [`vue/viewer.md`](vue/viewer.md) | How self-contained the viewer is | Generated — import statements |

Generators: `scripts/architecture/` (`julia_extract.jl`, `python_extract.py`, `vue_extract.py`). Each
script's header states what it sees and what it misses. Size rules (strongest N arrows, grouping
thresholds) are in the scripts and restated above each diagram.

## What is not here yet

- **Declared rules.** These views show what the code *is*. Which arrows are *allowed* is the next
  step, written from what these views show — not before.
- **Automatic regeneration.** Run `pixi run arch-diagrams` by hand after a structural change. CI
  regeneration comes once the views have proved useful.
- **Sequence diagrams** for key flows (run a task, run a chain, open an image in the viewer).
