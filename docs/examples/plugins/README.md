# Example plugins

Two complete, runnable **plugins**. Where [`../custom-modules/`](../custom-modules/) shows how to add
a single drop-in task, these show what a plugin adds on top: a directory that is installed, updated
and removed as one unit — and, in the second case, **a custom task *and* the custom module page that
inspects it**.

That pairing is the point of a plugin. The custom-module loader already lets anyone drop a task in; if
a plugin were only "several tasks in a folder" it would be packaging and nothing more. What it
actually buys is the page.

Both are published as their own repos and listed in Settings → Plugins, so they install the way any
other plugin does. **The copies here are the source** — CI loads and runs them, so they cannot rot —
and `scripts/publish_plugin.jl` mirrors them out at release time.

| Directory | Repo | What it demonstrates |
|---|---|---|
| `ccia-importTracks` | [`schienstockd/ccia-importTracks`](https://github.com/schienstockd/ccia-importTracks) | a task on a **built-in** page; templates; shared Python |
| `ccia-trackMeasures` | [`schienstockd/ccia-trackMeasures`](https://github.com/schienstockd/ccia-trackMeasures) | a **new category**, so its own page, with a plot canvas |

Install either exactly as a user would — Settings → Plugins, or by hand:

```bash
mkdir -p ~/.cecelia/modules/plugins
cp -r ccia-trackMeasures ~/.cecelia/modules/plugins/
```

Then restart the server, or Settings → Custom modules → **Reload**. A *new* plugin loads on reload;
editing an already-loaded `.jl` needs a restart, because Julia cannot redefine a struct in place.

## 1. `ccia-importTracks` — a task on an existing page

```
ccia-importTracks/
  plugin.json                                   manifest: name, version, requiresCecelia, categories
  tracking/importCsvTracks.{jl,json,_run.py}    → lands on the BUILT-IN Tracking page
  templates/{imagej_manual,trackmate,trackmate_xml,imaris}.json   column mappings
  python/track_readers.py                       shared helper, importable by any of its runners
```

Attaches tracks produced in *another* tool (ImageJ Manual Tracking, TrackMate, Imaris) to cecelia's
own segmentation. Its category is `tracking`, a built-in page, so it simply appears in that page's
task list. This is the driving use case from [`../../todo/PLUGINS_PLAN.md`](../../todo/PLUGINS_PLAN.md).

Three things make it a template rather than a hard-coded reader:

- **A column mapping, not a format.** The task needs exactly four things — track id, frame, X, Y (Z if
  3D). `templates/*.json` ship ready-made mappings and any field you fill on the form overrides the
  template, so an unlisted tool works by mapping its columns and a *nearly*-matching one works by
  fixing one field. Supporting a new source is a new template file, not new code.
- **A spatial match, not a join.** An external tracker knows nothing about cecelia's labels, so there
  is no id to join on: each cell takes the track of the nearest spot **in its own frame**, within
  `maxDistance` pixels. Cells with nothing inside the cutoff stay `-1`. That cutoff is the safety
  margin — without it every spot finds *some* nearest cell and a mismatched export yields a full,
  wrong column instead of an obvious failure.
- **The form asks only what applies.** Which fields are visible is `showIf` in the task's JSON — an
  XML export has no columns to map, so the whole mapping section is simply absent — while the *column
  names* offered come from a server hook, because only the server can open the file. The line is
  "could the form decide this on its own?", and it moved: the XML rule started life in the hook and
  reappeared on every restored form until it became a spec field.

**`python/track_readers.py` — shared code.** `run_py` puts each plugin's `python/` directory on
`PYTHONPATH`, so a runner imports it as plain `from track_readers import ...`. Note the plugin's own
name never appears in that import — `ccia-importTracks` has a hyphen and is not a Python identifier,
so anything that spelled the plugin name into a module path could not work.

> The shipped templates are inferred from each tool's documented output; only the TrackMate ones have
> been checked against a real export. Check yours against an actual file before trusting it.

## 2. `ccia-trackMeasures` — a task with its own page

```
ccia-trackMeasures/
  plugin.json                              manifest, incl. an optional `contributions` block
  trackTools/cumulativeChange.{jl,json}    new category, so it gets its OWN page
  plotDefinitions/cumulative_change.json   the plot canvas on that page
```

The Feijoa counterpart of the old R tutorial's
[`behaviourAnalysis.cumulativeChange`](https://cecelia.readthedocs.io/en/latest/create_custom_module.html):
speed measured between *consecutive* positions is noisy for a cell jittering in place, so this measures
displacement, speed and straightness over a larger gap along each track. Category `trackTools` has no
built-in page, so it gets the generic `/custom/trackTools` page and a **Custom** sidebar group.

`plotDefinitions/cumulative_change.json` is what turns that page from a bare task runner into a real
module page: a plot spec declaring `module: "trackTools"`, offering the three measures the task wrote.
Run the task on the right, inspect the distribution below.

Its manifest also carries a `contributions` block naming those two things. It does not have to — the
layout already says it — and what the block buys is a **check**: rename the `fun_name` or move the
plot spec and Cecelia reports which line of the manifest disagrees. See
[`../../CUSTOM_MODULES.md`](../../CUSTOM_MODULES.md) → *Declaring what you contribute*.

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

CI loads both of these on every run (`app/test/suite.jl` → *"The shipped custom-module examples load
end to end"*), so they cannot rot into plausible-looking files that no longer work.
