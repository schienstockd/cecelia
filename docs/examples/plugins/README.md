# Example plugin

One complete, runnable **plugin**: `tracktools-example`. Where
[`../custom-modules/`](../custom-modules/) shows how to add a single drop-in task, this shows what a
plugin adds on top — **a custom task *and* the custom module page that inspects it**, as one
directory.

That pairing is the whole point. The custom-module loader already lets anyone drop a task in; if a
plugin were only "several tasks in a folder" it would be packaging and nothing more. What it actually
buys is the page.

```
tracktools-example/
  plugin.json                          manifest: name, version, requiresCecelia, categories
  tracking/importCsvTracks.{jl,json,_run.py}    → lands on the BUILT-IN Tracking page
  trackTools/cumulativeChange.{jl,json}         → new category, so it gets its OWN page
  plotDefinitions/cumulative_change.json        → the plot canvas on that page
  templates/{imagej_manual,trackmate,imaris}.json   column mappings for the importer
  python/csv_tracks.py                          shared helper, importable by any of its runners
```

Install it exactly as a user would, then restart the server (or Settings → Custom modules →
**Reload** — a *new* plugin loads on reload; editing an already-loaded `.jl` needs a restart):

```bash
mkdir -p ~/.cecelia/modules/plugins
cp -r tracktools-example ~/.cecelia/modules/plugins/
```

## What each half demonstrates

**1. `tracking.importCsvTracks` — a task on an existing page.** Attaches tracks produced in *another*
tool (ImageJ Manual Tracking, TrackMate, Imaris) to cecelia's own segmentation. Its category is
`tracking`, a built-in page, so it simply appears in that page's task list. This is the driving use
case from [`../../todo/PLUGINS_PLAN.md`](../../todo/PLUGINS_PLAN.md).

Two things make it a template rather than a hard-coded reader:

- **A column mapping, not a format.** The task needs exactly four things — track id, frame, X, Y (Z if
  3D). `templates/*.json` ship ready-made mappings and any field you fill on the form overrides the
  template, so an unlisted tool works by mapping its columns and a *nearly*-matching one works by
  fixing one field. Supporting a new source is a new template file, not new code.
- **A spatial match, not a join.** An external tracker knows nothing about cecelia's labels, so there
  is no id to join on: each cell takes the track of the nearest spot **in its own frame**, within
  `maxDistance` pixels. Cells with nothing inside the cutoff stay `-1`. That cutoff is the safety
  margin — without it every spot finds *some* nearest cell and a mismatched export yields a full,
  wrong column instead of an obvious failure.

> The shipped templates are inferred from each tool's documented output, **not verified against a real
> export**. Check one against an actual file before trusting it.

**2. `trackTools.cumulativeChange` — a task with its own page.** The Feijoa counterpart of the old R
tutorial's [`behaviourAnalysis.cumulativeChange`](https://cecelia.readthedocs.io/en/latest/create_custom_module.html):
speed measured between *consecutive* positions is noisy for a cell jittering in place, so this measures
displacement, speed and straightness over a larger gap along each track. Category `trackTools` has no
built-in page, so it gets the generic `/custom/trackTools` page and a **Custom** sidebar group.

**3. `plotDefinitions/cumulative_change.json` — the page's canvas.** A plot spec declaring
`module: "trackTools"`, offering the three measures the task wrote. This is what turns
`/custom/trackTools` from a bare task runner into a real module page: run the task on the right,
inspect the distribution below.

**4. `python/csv_tracks.py` — shared code.** `run_py` puts each plugin's `python/` directory on
`PYTHONPATH`, so a runner imports it as plain `from csv_tracks import read_track_csv`. Note the plugin's
own name never appears in that import — `tracktools-example` has a hyphen and is not a Python
identifier, so anything that spelled the plugin name into a module path could not work.

## A plugin describes its page; it does not implement it

Both halves of a module page are already declarative — the form comes from the task spec's `params`
(rendered by `ParamRenderer`), the canvas from a plot spec (rendered by `SummaryCanvas`).

This is a **decision**, not an impossibility. A stable install ships a prebuilt `frontend/dist` and
precompiles SFCs, so a plugin's `.vue` could not be compiled there — but pre-compiled ESM using render
functions would `import()` fine, and the dev channel builds the frontend locally anyway. It is
excluded because shipping renderable code turns the frontend into a **plugin ABI**: a component
contract that cannot be refactored freely, a loader to maintain, and version skew between a plugin and
the app drawing it. Declarative specs cost a plugin author far less across cecelia releases. The
trade-off is written up in [`../../todo/PLUGINS_PLAN.md`](../../todo/PLUGINS_PLAN.md).

## Not sandboxed

A plugin's `.jl` is `Base.include`d into the `Cecelia` module with full access to the machine, exactly
like a module you dropped in yourself. Installing one is trusting whoever wrote it.

CI loads this example on every run (`app/test/suite.jl` → *"The shipped example plugin loads end to
end"*), so it cannot rot into a plausible-looking file that no longer works.
