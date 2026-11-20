> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in [`docs/todo/LENS_QUERY_PLAN.md`](../todo/LENS_QUERY_PLAN.md).

# CHANNEL_LENS_POPULATIONS_PROMPT

Scope-and-design audit. **Do not implement.** Produce a plan I can accept, cut or redirect.

## Context

Commit `dc6103b` (branch `feat/channel-lens`) added a channel lens to the 2D viewer, after Scope2Screen (doi:10.1109/TVCG.2021.3114786): a circle on the pointer that draws its own channel set, or dims everything outside it when no channel is picked. A chip shows per-population cell counts inside it. A click pins it to the tissue.

Read first:
- the commit message and its listed reservations
- `frontend/src/utils/viewer/channelLens.ts`, `shaders/lens.wgsl`
- the lens state in `ViewerWindow.vue` (`lensOn`, `lensPin`, `lensAt`, `lensChip`, `placeLens`)
- `api/src/movie_render.jl::lens_state`, `api/src/kiwi_refs.jl`, `kiwiRef.schema.json`
- the gating page and the population model (including the Claude co-driving gating work, PR #1856)
- `docs/inventory/FRONTEND.md`, `docs/ui/PRIMITIVES.md`

## The idea

1. **Lens as a population.** A pinned lens can be saved as a population on the gating page, so what I see inside the lens becomes something I can analyse (counts, plots, downstream tasks), not only look at.
2. **Multiple lenses.** More than one lens on the view, each with its own channel set, for side-by-side comparison of marker combinations in different regions.
3. **Lens as ROI query.** A lens population carries a signature of its region (phenotype composition, behaviour, interactions). Candidate regions from the spatial neighbourhood graph are scored against it, and the best matches come back as ROIs, alongside the existing "Find ROIs" discovery.

4. **Lens plot type.** A canvas plot type just for the lens, modelled on the cell cards that show behaviour properties, showing the lens region's composition and behaviour properties.

**These are ideas, not decisions.** Any of the four may not survive contact with real data. Do not plan a full lens → population → ROI pipeline on the assumption that it works. Test the weakest links first (see "Validate first").

## Questions to answer

**A. What kind of population is it?**
- Candidate: a spatial gate, stored as parameters (`x, y, r`, t/z scope, rule "centroid inside circle"), re-evaluated on demand. Alternative: a frozen snapshot of cell IDs at pin time. Recommend one, or both with a clear split.
- Can it combine with marker gates (e.g. CD8+ AND inside lens)? If the population model can't express this, say what would have to change.
- Pixel coordinates are image-specific. State plainly what happens in a batch or chain-from-history run, and whether this is a per-image ROI population rather than a reusable gate.
- How does it behave if segmentation or the cell table is regenerated?
- Does the lens channel set belong to the population, to the view, or to neither?

**B. Multiple lenses**
- Data model: list of lenses in view state vs. one lens plus named saved lenses. Effect on `viewState.ts`, the Kiwi ref schema (`viewer.lens` is currently a single object), `lens_state`, the render payload `st["lens"]`, and `lens.wgsl`.
- Rendering: overlap rules, per-lens cost in the shader, a sane cap on lens count.
- UI: how a user adds, selects, deletes and labels lenses without growing the 7k-line window. Where the chip goes with several lenses.
- Is the real need multiple lenses, or one lens plus a saved-lens list? Challenge the premise if it doesn't hold.

**C. Scale**
- Counts are untimed at 154k cells. Propose the cheapest correct approach (spatial grid or index, throttling, recount on pin or settle) and how to measure it.
- Population membership for many cells in a gate: compute where, cache where.

**D. Existing gaps this feature depends on**
- The pin is not persisted across reload, and clicking a Kiwi ref does not re-pin the lens. A saved lens population is not meaningful if these stay open. Say which must land first.
- Batch and ROI movies do not draw the lens. Say whether that matters for the new feature.
- `lens_state` accepts `r > 0` while the Kiwi schema requires `r >= 1`. Reconcile.

**E. Kiwi and MCP**
- What should Kiwi be able to read, create and reference (a lens population, multiple lenses)? What shape checks does `_kiwi_value_error` need?
- Does `get_viewer_screenshot` / `set_viewer_view` need to change for multiple lenses?

**F. Prior art and fit**
- What do Scope2Screen, Facetto and TissUUmaps do for lens-to-selection and lens-to-gate? Take only what fits Cecelia's model. Name what to skip.

**G. Lens as ROI query (idea 3)**
- **Signature.** What goes in the region vector: phenotype fractions, behaviour-cluster fractions, interaction and contact stats, density, mean marker intensity. Which of these can the current tables produce cheaply for an arbitrary region?
- **Candidates.** Regions from the spatial neighbourhood graph (k-hop neighbourhoods around cells, or tiles). Which does the graph support today?
- **Scoring.** Similarity measure for compositions (cosine, Jensen-Shannon) vs. continuous stats (z-scored distance). Normalising for region size and density, so scores don't just track cell count. Minimum-count threshold for sparse regions.
- **Behaviour.** Tracks span regions and time. Define membership by track position at a stated timepoint or window, or say that behaviour is out of scope for a first version.
- **Interactions.** Relational features (neighbour-type counts, contact frequency), not per-cell labels.
- **User control.** Feature-group toggles ("same phenotype mix, any behaviour"). Positive and negative lenses ("like A, not like B"), if multiple lenses are kept.
- **Transfer.** Fractions and rates carry across images, pixel geometry does not. Does this make batch use work, and where does it break?
- **Fit with Find ROIs.** Same ROI objects as the existing discovery (PR #1793)? Reuse its scoring or output path, or does this need its own?

**H. Lens plot type (idea 4)**
- Find how the cell cards render behaviour properties on the canvas (plot type registration, data source, `PlotPicker` / `PlotSketch` entry, figure or analysis-board export). A lens plot should reuse that path, not add a parallel one.
- What it shows depends on the image type, for one pinned lens only:
  - **Live (time-lapse) images:** behaviour properties of the tracks in the lens, like the cell cards.
  - **Static images:** cell phenotypes and composition (phenotype fractions, optionally marker intensity and interaction stats).
  - Decide how the plot knows which mode applies, and what an image with both should show. Say which properties are cheap now and which wait on G.
- The same split applies to the signature in G: behaviour features only make sense for live images, composition features for both. Say how the scoring handles each.
- Which lens: the pinned one, a saved lens population, or a selectable one. With multiple lenses, one plot per lens or a comparison plot.
- Does it update while the lens moves, or only on pin? Tie this to the counting cost in C.
- Is it exportable (analysis board, PDF) and Kiwi-readable, like other plot types?
- This plot may be the cheapest place to look at the signature before any scoring exists, so say whether it should come first.

## Validate first

Before any pipeline is built, say what to test, in what order, and what result would kill each idea. Prefer experiments that need no new UI: a script or notebook on existing data, a Kiwi or MCP session, or a throwaway view.

Suggested starting points, change them if you see cheaper ones:
0. **Lens plot as probe.** If the lens plot is cheap, a rough version may answer test 1 by eye before anything else is built. Say whether to start there.
1. **Does the signature separate regions?** Take two or three regions I'd call different by eye (e.g. tumour core vs. margin, a blebbing dying-cell event vs. background) and check whether composition, behaviour and interaction vectors separate them.
2. **Does the query retrieve?** Use a hand-picked region as the query and see whether the top-scoring graph regions are ones I'd agree are similar. Include a negative control. Use the missed blebbing events from the Find ROIs work as ground truth if they fit.
3. **Is it stable?** Does the ranking survive changes to radius, k, and the minimum-count threshold, and a re-run of segmentation?
4. **Is it cheap enough?** Time the signature computation over all candidate regions at the expected table size (154k cells).
5. **Does it add anything?** Compare against what Find ROIs and plain gating already find. If it only reproduces them, say so.

For each: the test, the data, the pass/fail criterion, and the cost. Recommend stopping after any failed test, and say which later steps that makes pointless.

## Constraints

- Follow the convention docs. Lens logic belongs in a `useVw*` composable and chip chrome in its own component, not further inline in `ViewerWindow.vue`. Say whether that extraction is a prerequisite.
- Reuse existing primitives (`.cc-card`, popover, population model). Flag anything that would need a new one.
- The lens was not checked live (pin, chip buttons, channel menu). Don't assume that behaviour is settled.
- 0.3.0 hardening: weigh this against the feature freeze. Is it in, out, or a post-freeze item?

## Deliverable

One markdown plan:
1. Recommendation in a few lines: build, test first, defer or cut, for each of the three ideas separately.
2. The validation plan (see "Validate first"), with kill criteria. This comes before any build steps.
3. Only for ideas that pass or are cheap enough to try: data model (concrete field names) and ordered steps, each small enough for one PR, prerequisites first.
4. Risks and open questions, most important first.
5. What you would not build, and why.

Be direct. If the answer to A, B or G is "this isn't worth it", or "test first and probably stop", say so with the reason.
