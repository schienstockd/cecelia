# ccia-trackMeasures

A [Cecelia](https://github.com/schienstockd/cecelia) plugin: **track behaviour measures over a
temporal gap**, with a module page of its own.

## What it measures, and why a gap

Per-step speed is computed between *consecutive* positions, which is noisy for a cell that jitters in
place. Measuring the same quantities over a larger gap smooths that out, and is often what actually
separates migrating from searching cells — the reason the original R tutorial used it as its worked
example.

For each cell, over a gap of `gap` positions along that cell's own track:

| Column | Meaning |
|---|---|
| `trackTools.cumulativeDisplacement` | straight-line distance from the position `gap` steps back |
| `trackTools.cumulativeSpeed` | that distance divided by `gap` |
| `trackTools.cumulativeStraightness` | net displacement ÷ path length over the same window (0–1; ~1 directed, ~0 searching in place) |

Cells with fewer than `gap` prior positions on their track get no value rather than a guess.

## Install

**Settings → Plugins**, paste:

```
https://github.com/schienstockd/ccia-trackMeasures
```

No `git` needed. Then run it from **Custom → trackTools**.

## What it needs

A segmentation with tracks — a `track_id` column and centroids. That includes tracks imported
straight from another tool with no segmentation at all (see
[ccia-importTracks](https://github.com/schienstockd/ccia-importTracks)), because these measures read
positions and track ids and nothing else.

## Why this is a plugin and not a drop-in module

It ships a task **and the page that inspects it**. `trackTools` is not a built-in category, so the
task gets the generic `/custom/trackTools` page, and the `plotDefinitions/cumulative_change.json` in
this repo gives that page a plot canvas over what the task just wrote. Both halves are declarative —
no Vue, no rebuild.

That pairing is the whole point of the plugin format; the drop-in module loader already gives you the
task on its own.

## Layout

```
plugin.json                              manifest
trackTools/cumulativeChange.jl           the task
trackTools/cumulativeChange.json         its form
plotDefinitions/cumulative_change.json   the page's plot canvas
```

The layout above is the whole contribution list — nothing else is needed. This manifest nonetheless
repeats it in a `contributions` block, which buys a **check**, not a capability: rename the task's
`fun_name` or move the plot spec and Cecelia reports which line of the manifest now disagrees.
(`ccia-importTracks` declares nothing and is equally complete.)

The block does add one thing the layout cannot express — it **borrows a built-in plot**:

```json
"views": [{ "module": "trackTools", "view": "trackPaths", "label": "Tracks" }]
```

so the page offers Cecelia's own track-paths plot beside the measures, under **Interactive** in the
`+ Plot…` picker. See
[Custom modules and plugins](https://github.com/schienstockd/cecelia/blob/main/docs/CUSTOM_MODULES.md)
→ *Declaring what you contribute*.

## Not sandboxed

A plugin's Julia is `include`d into Cecelia with full access to your machine, exactly like an R
package. Install what you wrote or trust.

## License

GPL-3.0-or-later, matching Cecelia.
