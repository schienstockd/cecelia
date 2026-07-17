# Example custom modules

Two complete, runnable **user drop-in tasks**. Copy the contents of this folder into your config
dir's `modules/` (see [`../../CUSTOM_MODULES.md`](../../CUSTOM_MODULES.md) for where that is), e.g.
for an installed app:

```bash
mkdir -p ~/.cecelia/modules
cp -r sources inputDefinitions python ~/.cecelia/modules/
```

Then restart the server (or Settings → Custom modules → **Reload**, or `POST
/api/tasks/custom-modules/reload`).

## 1. `behaviour.exampleNormalise` — minimal, Julia-only

Min-max normalises one per-cell measure to 0..1 and writes it back as a new obs column. Category
`behaviour`, so it appears in the existing **Behaviour** module page's task list as *"Example:
normalise a measure (custom)"*. Demonstrates the whole wiring (`struct` + `_run_task` +
`register_task!`) and the label-props read/write idiom — no Python.

## 2. `customExamples.trackContext` — Julia + Python + nested params

The fuller pattern:

- **Julia** computes `<measure>.trackMean` — for each cell, the mean of a measure (default
  `live.cell.speed`) over its whole track — and writes it to the `.h5ad`.
- **Python** (`python/customExamples/trackContext_run.py`, launched via `run_py`) reads that column
  back, computes a standardised version (`z-score` / robust `median/MAD`), and writes
  `<measure>.trackMean.<method>` back too.
- **Nested parameters**: a *Track filtering* section (`minTrackLength`) and a *Python standardisation*
  section (`method`).

Category `customExamples` has **no built-in page**, so this also shows the generic
`/custom/:category` page: a **Custom** group appears in the sidebar with a *Track context* entry.
Both computed columns land in the `.h5ad` and can then be plotted / gated / clustered like any measure.

## What they demonstrate

- `struct <: Cecelia.CciaTask` + `Cecelia._run_task` + `Cecelia.register_task!` (the whole wiring).
- Reading/writing cell data through the label-props view in both languages (`as_df`,
  `add_obs |> save!` in Julia; `LabelPropsView(...).as_df()` / `.add_obs(df).save()` in Python) —
  never touching the `.h5ad` directly.
- Launching Python from a custom task with `run_py` (absolute script path, user `modules/python/` on
  `PYTHONPATH`).
- A JSON spec with nested `section` params, using the same schema as the built-in tasks.
