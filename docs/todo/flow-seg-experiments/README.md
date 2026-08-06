# 2026-08-06 flow-segmentation experiments

Kept in the repo deliberately: the *previous* session's scripts lived in an ephemeral scratchpad and
were lost, which `../SEGMENTATION_OPEN_PROBLEM.md` records as a cost. These are the two that carry
today's conclusions.

Both are **analysis scripts, not tasks** — they read a project read-only, write nothing into it, and
render to `~/Downloads/TMP/`. Run them from the `cecelia-feijoa` checkout so the pixi env resolves:

```bash
PYTHONPATH=python pixi run python docs/todo/flow-seg-experiments/<script>.py
```

They insert the coastal dev checkout on `sys.path` because pixi pins `PYTHONPATH`. That is fine for
a scratch analysis and is *not* the runner convention — a real task runner must never do it
(see the root `CLAUDE.md` → *Spawning Python*).

| script | what it establishes |
|---|---|
| `flow_seg_run.py` | retrains coastal's segmenter on spatially-smoothed, float32-flow input and scores it against the six-line intensity baseline |
| `diagnose_fragments.py` | sweeps spatial sigma for cell/background flow contrast, retrains at the best, and separates prob-head fragments from region-growing fragments |

Figures land as `~/Downloads/TMP/flow_{4,5}_*.png`.
