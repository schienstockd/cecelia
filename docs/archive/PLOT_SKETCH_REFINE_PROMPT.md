> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. No
> `docs/todo/*_PLAN.md` exists for it yet.
>
> **Outcome:** not run as written; overtaken in the same PR (#2357, 2026-11-19). Looking at the
> picker showed its core rule — sketches show shape, never meaning — could not work: every Compare-groups
> plot opens as a box plot, so a shape sketch is the same on every card. The maintainer dropped the rule.
> The picker now lists the kinds of plot (each chart shape drawn once) beside the picked kind's plots,
> each card sketching what the plot is about from a few shared subjects. Current design: `docs/PLOTS.md` → *The
> `+ Plot…` picker*. Its keyboard-navigation slice is still open.
>
> Read against the branch at 7a6611090, before that change, these premises had moved:
>
> - `frontend/src/plots/plotCatalogue.ts` does not exist; the file is
>   `frontend/src/lib/guides/plotCatalogue.ts`. Every other path in "Read first" exists.
> - The vocabulary is already 13 kinds (`PLOT_SKETCH_KINDS`: box, hist, bars, stacked, line, elbow,
>   heatmap, scatter, image, cards, tracks, timeline, gates), inside the 10–15 target. The likely
>   gap is that `boxplot`, `violin` and `strip` all map to `box` in `CHART_SKETCH`.
> - Fallback exists but is not neutral: an unknown or missing `sketch` resolves to the spec's first
>   chart type via `sketchForCharts`, and to `box` when that is unknown too. No generic kind.
> - Drift guard exists in part: `plotPicker.test.ts` checks that a spec JSON's `sketch`, when set, is a
>   known kind, and that every `CLUSTER_PANELS` entry has one. `INTERACTIVE_VIEWS` is covered only by
>   the TypeScript type.
> - Only one of 15 spec JSONs sets `sketch` (`population_over_time`: `line`); the rest derive it.
> - The pilot ids all exist (`cell_properties`, `population_summary`, `spatial_cell_properties`,
>   `population_over_time`, `spatial_interactions`, `transition_matrix`).
> - Keyboard navigation is still missing (only Enter on the search box picks the first match),
>   listed as a reservation on 7a6611090.

# PLOT_SKETCH_REFINE_PROMPT

Refine the picker's schematics so each one reads as the plot it stands for. Pilot first, then the rest.

## Context

Commit `7a66110` ("Group the + Plot picker by question, with schematic previews") gave every plot card in `PlotPicker.vue` a drawn schematic from `PlotSketch.vue` (one inline SVG per shape, no data needed) plus its `answers` line.

The current sketches are generic. Several plots in one category (for example "Compare groups") look nearly identical, so the sketch doesn't help someone choose. The goal is a picker that works like the chooser in GraphPad Prism: the user sees what a plot does at a glance, before any data renders.

Decisions already made:

- **No thumbnail engine, no cached renders.** Sketches stay static, hand-built SVG. The improvement comes from looking at the real plots first and then drawing better schematics from what they actually look like.
- **Sketches convey shape, not meaning.** A schematic can show box vs violin vs bars vs lines vs matrix. It cannot show what is on the axes (speed vs turning angle). The `answers` line carries that. Do not try to encode meaning in the sketch, and do not change `answers`.
- **Keep the vocabulary small.** Aim for roughly 10-15 shared sketch kinds, not one bespoke drawing per plot. Per-plot drawings drift from the real plots when those change.

## Precondition: observed confusion

This run is only worth doing if people still pick the wrong plot after the grouping and search from `7a66110`. The maintainer fills this in before the run starts:

```
Used the picker live: [yes/no]
Cases where a card was still hard to tell from another: [list plot ids]
```

If this block is empty, stop and ask for it. Use the listed cases to choose the pilot set. Do not start the full run for plots nobody has confused.

## Read first (ground truth)

Verify every path exists. If one is missing, say so.

- `frontend/src/components/canvas/PlotSketch.vue`, `PlotPicker.vue`
- `frontend/src/plots/plotPicker.ts` (`PlotSketchKind`, `sketchForCharts` / `CHART_SKETCH`, `specOption`)
- `frontend/src/plots/plotCatalogue.ts`, `types.ts`
- `app/src/plotDefinitions/*.json` (`category`, `sketch`, `answers`, `family`, `module`)
- `frontend/src/components/canvas/interactiveViews.ts`, `frontend/src/modules/cluster/clusterPanels.ts`
- `docs/PLOTS.md`, `docs/inventory/PLOTS.md`, `docs/ui/PRIMITIVES.md`
- `docs/ai-assist/CONVENTION_CHECK.md`
- Existing capture tooling (docshots capture engine, headless Chrome capture) for getting screenshots of real plots

## Step 1: inventory (no edits)

Build a table of every plot a user can add: id, label, category, current sketch kind, how the sketch was chosen (explicit `sketch` field or derived from chart types), and the plot's `answers` line.

Then group by current sketch kind and flag:
- kinds shared by more than one plot where the real plots differ visibly
- plots whose sketch doesn't match what the real plot shows
- plots with no sketch at all, including plugin plots (see fallback below)

Propose the target vocabulary: the list of sketch kinds (10-15), which plots map to each, and which distinctions the vocabulary deliberately does not make. Get this list confirmed before drawing anything.

## Step 2: look at the real plots

For each plot in the pilot set (below) and then the rest:

1. Render the real plot with the maintainer's own data, using existing tooling where possible. If real data or a way to render is not available in this environment, stop and report that. **Do not invent data and do not draw sketches from imagination.**
2. Save screenshots to a scratch directory outside the repo. Do not commit them.
3. Write down what makes this plot's *shape* recognisable and different from its neighbours in the same category: mark type (box, violin, bars, dots, lines, heatmap cells), grouping, faceting, annotation such as significance brackets or reference lines. Ignore what the axes measure.

## Step 3: redraw

Rules for every sketch:

- Static inline SVG, fixed `viewBox`, no data, no scripts.
- Theme-aware: colours via CSS variables, correct in light and dark.
- Same stroke weight, palette and framing across all sketches. A card set should look like one family.
- Show the distinguishing shape feature from step 2, and drop everything else. If a detail doesn't help someone choose between two plots, remove it.
- Readable at actual card size. No text in the sketch unless a plot cannot be told apart without it.
- Prefer a new shared `PlotSketchKind` over overloading one kind, within the vocabulary agreed in step 1. Keep the union, the `specOption` runtime check and the plugin fallback consistent.
- **Fallback:** plugin JSON is unvalidated, so a missing or unknown `sketch` must resolve to a deliberate generic sketch that looks intentional next to the others, not a blank or broken card. Add one neutral sketch kind for this and test it.
- **Drift guard:** add a test that every plot spec resolves to a known sketch kind, so a new or changed plot cannot silently lose its sketch.
- Do not change categories, labels or `answers` lines.

## Pilot

Choose the pilot from the confusion list above. If none are given, use:

1. Three plots from one category that currently look alike, preferably "Compare groups" (e.g. `cell_properties`, `population_summary`, `spatial_cell_properties`). Show before and after side by side.
2. One plot from "Over time" (e.g. `population_over_time`).
3. One plot from "Profiles & matrices" (e.g. `spatial_interactions` or `transition_matrix`).

Stop after the pilot and report:
- before/after sketches in both themes, rendered at **actual card size** as well as enlarged
- how long each took, what went wrong, whether it scales to the full set
- an honest answer to: does the new sketch let someone tell these plots apart without reading the `answers` line? If not, say so. Do not claim success from the redraw alone.

Wait for the maintainer to look at the before/after and confirm before the full run. Self-reported quality is not the check; the maintainer's eyes are.

## Full run

After approval, do the remaining plots in category order, one category per commit. Add or adjust tests in the style of `plotPicker.test.ts`. Update `docs/PLOTS.md` and `docs/inventory/PLOTS.md` where sketch kinds are documented. Follow `CONVENTION_CHECK.md`.

## Separate slice: keyboard navigation

Independent of the sketches and can ship first. Arrow keys should move through the picker cards: roving focus, Enter picks, Esc closes, focus returns to the trigger, search input unaffected. Reuse `TeleportPopover` and `.cc-actions-menu` behaviour before adding anything. Specify and implement it as its own commit, with tests in the style of `plotPicker.test.ts`. A keyboard gap is a definite flaw, so do not hold this behind the sketch work.

## Out of scope

- Thumbnail engine, caching, or any render-to-image pipeline
- Changes to the plots themselves
- Changes to category assignments or picker grouping
- Encoding axis meaning in sketches

## Output

- Step 1 inventory table and proposed vocabulary, as a markdown file under `docs/audit/`.
- Pilot report with before/after sketches (both themes, card size).
- Mark every claim as `[verified in code]`, `[verified on real render]` or `[assumption]`. No preamble.
