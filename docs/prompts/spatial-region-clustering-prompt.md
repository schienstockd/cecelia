# Part 1 + 2: Spatial Analysis Port + Region Clustering

Opus planning pass. Read everything listed before producing any plan. Sonnet executes against the plan.

---

## Attribution (add to THIRD_PARTY.md before any code lands)

**CytoMAP** — MIT license.
Stoltzfus, C.R., Filipek, J., Gern, B.H., Olin, B.E., Leal, J.M., Wu, Y., ... & Gerner, M.Y. (2020). CytoMAP: a spatial analysis toolbox reveals features of myeloid cell organization in lymphoid tissues. *Cell Reports*, 31(3), 107523.
Source: https://github.com/DrStoltzfus/CytoMAP

Any algorithm ported or derived from CytoMAP must be noted with an inline comment and a THIRD_PARTY.md entry. Same pattern as celltrackR attribution in `track_measures.jl`.

---

## Read first

**Legacy Cecelia R:**
- `old-R-shiny-version/inst/modules/sources/spatialAnalysis/` — all R module functions
- `old-R-shiny-version/inst/modules/sources/regionClustering/` — dedicated region clustering modules
- `old-R-shiny-version/R/populationUtils.R` — how the `region` poptype is handled alongside `flow`/`live`/`clust`
- Legacy vignettes using spatial analysis and region clustering — understand the REPL API and the cross-poptype query pattern
- Legacy Shiny region clustering module page — what the UI looked like

**CytoMAP MATLAB source:**
- `DrStoltzfus/CytoMAP/CytoMAP/` — all `.m` files. Focus on:
  - Neighbourhood composition vector computation
  - HMRT/SOM clustering pipeline
  - Permutation testing for spatial associations
  - Any region-membership queries (which cells are in which region)

**New Cecelia:**
- `app/src/tasks/` — confirm what spatial/region modules already exist if any
- `app/py/utils/` — any spatial utilities already there

---

## CytoMAP parity target — what Gerner lab users expect

The Gerner lab (University of Washington) has used CytoMAP extensively across multiple published studies. If they transition to Cecelia, they need equivalent readouts. **Cecelia must produce these natively — not require a CSV export to CytoMAP for the statistics.** If the user has to export to MATLAB to do their analysis, Cecelia is just a coordinate export machine, not a replacement.

From the Cell Reports paper and known Gerner lab usage, CytoMAP produces:

**Neighbourhood statistics:**
- Cell count per neighbourhood (e.g. number of each cell type within 30µm / 50µm radius)
- Pearson correlation heatmap between population densities across neighbourhoods — "which cell types co-localise?"
- Spatial proximity analysis — distance from cell type X to nearest cell type Y

**Region readouts:**
- Heatmap of neighbourhood composition per region (what cell types define each region)
- Positional remapping — colour-coded dots showing region identity in the original spatial coordinates (recreates the tissue anatomy)
- UMAP of neighbourhood composition coloured by region — how regions relate to each other in composition space
- Per-region population frequency — what fraction of each cell type lives in each region
- Region adjacency — which regions border which (network representation)

**Statistical tests:**
- Permutation testing for spatial association — is population X closer to population Y than expected by chance? (shuffle positions, build null distribution, report p-value)
- Random point distribution (RDP) as null model — CytoMAP generates random points and compares observed spatial patterns against them

**Plots expected (equivalent needed in Cecelia analysis canvas or Pluto notebooks):**
- Positional scatter plot coloured by region or population
- Neighbourhood composition heatmap (regions × cell types)
- Pearson correlation heatmap (cell types × cell types, across neighbourhoods)
- UMAP coloured by region label
- Bar chart: population frequency per region
- Spatial association p-value plot

Opus: for each of these, assess whether squidpy already provides it (squidpy has neighbourhood enrichment, co-occurrence, Moran's I, Ripley's statistics). Where squidpy covers it, use it. Where it doesn't (e.g. the RDP permutation test), flag as needing implementation. The goal is full parity with CytoMAP's analytical outputs before claiming this is a viable transition path for Gerner lab users.

**MCP accessibility rule applies to all CytoMAP-equivalent readouts.** Every statistical output — neighbourhood correlation, population frequency per region, spatial association p-values, distance distributions — must be MCP-accessible in a structured, interpretable format. Same principle as the spatial analysis functions: a flat table of results is more MCP-useful than a matrix. Claude should be able to answer "which regions are my CD8 T cells enriched in?" or "is there a significant spatial association between DCs and T cells?" by pulling these readouts via MCP, without the user having to manually extract numbers from a plot. Design the output formats with this in mind from the start, not as a retrofit.



H5AD was chosen as Cecelia's data format partly because **squidpy** — the standard Python spatial single-cell analysis library — is built directly on AnnData. squidpy already provides:
- Spatial graph construction (KD-tree, Delaunay, radius-based)
- Neighbourhood enrichment analysis
- Co-occurrence scores
- Ripley's statistics
- Moran's I spatial autocorrelation
- Region-level aggregation

Before designing any Julia spatial analysis, Opus must evaluate: **does keeping squidpy via PythonCall.jl buy more than a Julia port would?** Given that Python stays for napari and DL anyway, squidpy is already in the env. The question is whether any Julia spatial library provides meaningful advantages over what squidpy already does on the same AnnData files. Be explicit about this recommendation — don't assume one way.

---

## The `region` poptype — this is not `clust`

Region is a **dedicated population type** (`"region"`) with its own:
- Module page (dedicated, like the cell clustering module page)
- Population manager display (alongside `flow`/`live`/`clust`/`region`)
- REPL access via `pop_dt(img, "region", [...])`

Regions are fundamentally different from cell clusters:
- **Cell clusters (`clust`)**: group cells by their own per-cell measurements (intensity, morphology, behaviour)
- **Regions (`region`)**: group cells by the composition of their spatial neighbourhood — what cell types surround them

The `region` poptype was separate in the legacy R version for a specific reason: regions are spatial territories, not cell identities. A CD8+ T cell has one cluster label (from its own measurements) AND one region label (from where it is spatially). Both apply simultaneously to the same cell. The cross-poptype query depends on this.

---

## The cross-poptype containment query

The key scientific question regions enable:

> "I have gated CD8+CD29+ cells (`flow` type). I have defined spatial regions (`region` type) based on neighbourhood composition. **Which regions do these CD8 cells end up in?**"

And for live imaging:

> "I have defined behaviour clusters (`clust` type) from HMM. I have defined behaviour regions (`region` type) based on the spatial distribution of behaviour clusters. **Which cell interactions occur in the arrest-behaviour region?**"

This cross-poptype containment query — "which cells of type X are in region Y?" — is what the `region` poptype exists to answer. Design the data model to make this query natural:

```julia
# Which regions do CD8+CD29+ cells end up in?
region_membership(img, 
  query_pop=("flow", "cd8+cd29+"),
  regions=("region", ["region_1", "region_2", "region_3"])
)

# What cell populations are enriched in the arrested-behaviour region?
region_enrichment(img,
  region=("region", "arrest_zone"),
  query_pops=[("clust", "Arrested"), ("flow", "cd4+"), ("live", "tcells/tracked")]
)
```

---

## Extension to live cell imaging — behaviour regions

This is the genuinely new contribution beyond CytoMAP (which was static images only):

1. Run behaviour clustering (HMM → `clust` type: Arrested, Directed, Meandering...)
2. Use the spatial distribution of those behaviour clusters as the **population basis** for neighbourhood composition vectors
3. Cluster those composition vectors → define **behaviour regions** (`region` type: "arrest zone", "motility corridor", "transition zone")
4. Ask: which cell interactions occur in the arrest zone? Which T cells are found there?

This extends CytoMAP's concept into 4D (x, y, z, t). The spatial position at each timepoint defines neighbourhood composition; regions are defined across the timecourse. Opus: think about how the temporal dimension affects neighbourhood computation — is it per-timepoint or aggregated across time?

---

## Napari visualization

The legacy version had:
- Dots colored by region population
- Simple network/graph representations (adjacency between regions)

Needs for the new version:
- Per-cell dots colored by region label (same mechanism as other pop colorings)
- Region adjacency network overlay (which regions border which)
- Region boundaries or hulls if feasible

Opus: evaluate what the existing napari bridge supports for region-type populations and what would need to be added. The coloring mechanism should be consistent with how `flow`/`live`/`clust` populations are already colored in napari.

---

## Part 1 — Spatial analysis functions

From the legacy R module directory, audit each spatial analysis function:
- What it computed
- Whether squidpy already provides it (and if so, is the squidpy version better?)
- Whether a Julia port would be meaningfully faster or simpler
- What it needs from H5AD (positions, population labels)
- What it writes back

Produce a table: function → squidpy equivalent? → port to Julia? → recommendation.

The likely split:
- **squidpy handles**: neighbourhood graphs, enrichment, co-occurrence, Ripley's, Moran's I
- **Julia handles**: the neighbourhood composition vector computation for region clustering (tight loop over cells, compute composition of radius neighbourhood)
- **Mesh contact detection**: Opus evaluates ImplicitBVH.jl vs. keeping Python trimesh — report recommendation

---

## Part 2 — Region clustering

### Approach (informed by CytoMAP + legacy + new live-imaging extension)

1. **Neighbourhood composition**: for each cell, compute the composition vector of its spatial neighbourhood (fraction of each population type within radius R). This is the input feature.
   - For static images: neighbourhood of current cell positions
   - For live images: neighbourhood at each timepoint, optionally aggregated across time
   - Population basis: all populations, or a specified subset — user-configurable

2. **Clustering**: cluster the composition vectors. Options:
   - **SOM** (CytoMAP approach, preferred for discovery): assess `SOM.jl` maturity; if not reliable, use Python `minisom` via PythonCall.jl
   - **k-means** (legacy, kept as option): faster, reproducible, requires k
   - **Leiden on composition similarity graph** (extension): if squidpy is in use, Leiden on a composition-similarity graph is a natural fit

3. **Region assignment**: assign cluster label to each cell as its `region` population label

4. **Cross-poptype query**: given a region label set, compute which cells from other poptypes fall within each region — the `region_membership` / `region_enrichment` functions above

### Module page

Dedicated module page for region clustering, analogous to the cell clustering page:
- Image table (same shared component)
- Population basis selector: which populations define the neighbourhood composition (all? a subset?)
- Neighbourhood radius or k-NN parameter
- Clustering method selector (SOM / k-means / Leiden)
- Clustering parameters (k for k-means, SOM grid size, etc.) — DynamicWidget from JSON spec
- Result: region population labels assignable in the population manager

Reference `old-R-shiny-version/inst/app/modules/server/clustRegionsServer.R` for the old UI — read it to understand what the user could configure and what outputs were shown. Not a blueprint to replicate exactly (it's Shiny, it's old), but a guide to what the region clustering workflow looked like from the user's perspective: what decisions the user made, in what order, what feedback they got after running.

---

## Spatial analysis module page

Spatial analysis is its own dedicated module page — not buried inside region clustering, not shared with other analysis types. Same pattern as segmentation, tracking, gating: a first-class page in the processing workflow sidebar.

**All poptypes as inputs**: the spatial analysis functions must accept any poptype as input — `flow`, `live`, `clust`, `region`. A user should be able to ask "how far are `flow` CD8+ T cells from `region` tumour regions?" with the same function that answers "how many `live` tracked T cells are in contact with a `clust` arrested-behaviour aggregate?" Population selectors on the spatial analysis module page should show all available poptypes and their populations, not just one type.

**Read the legacy vignettes for canonical questions**: before designing the module page or the analysis functions, read every vignette `.Rmd` file in `old-R-shiny-version/vignettes/` that mentions spatial analysis, neighbours, contacts, or aggregates. The scientific questions asked there are the readouts Cecelia must support natively. Typical questions from these vignettes and from the lab's usage:
- How many T cells are aggregating? (aggregate detection)
- How many CD4+ T cells are in contact with a CD8+ T cell aggregate?
- How far away are CD8+ T cells from a tumour region?
- Are T cells aggregating inside tumour regions?
- What is the neighbourhood composition around a given population?
- Is population X closer to population Y than expected by chance?

Each of these is a concrete readout the module must be able to produce. Opus: map each question to the specific analysis function and output format needed to answer it.

---

## Aggregate detection (not cluster detection)

Rename the concept of spatially-grouped cell clusters to **aggregate** throughout — "aggregate detection" not "cluster detection." This avoids confusion with cell clustering (`clust` poptype) which is a completely different operation. An aggregate is a spatial grouping of cells of the same or related type — a T cell aggregate is a set of T cells that are physically proximal to each other in the tissue. Aggregates are detected by spatial density analysis, not by feature-space clustering.

The spatial analysis functions that detect and characterise aggregates should use "aggregate" terminology consistently in:
- Function names (`detect_aggregates`, `aggregate_size`, etc.)
- Module page labels
- Population names assigned in the population map (e.g. `"aggregated"` vs `"non-aggregated"`)
- Lab log entries and QC output

---

## MCP and Claude for flexible spatial readouts

The spatial analysis functions will produce structured outputs (neighbourhood tables, distance distributions, aggregate membership, region enrichment). These should be MCP-accessible so Claude can answer the kinds of questions researchers actually ask:

> "Are my CD8 T cells aggregating in tumour regions?"
> "How many CD4 cells are in contact with CD8 aggregates?"
> "Is there a statistically significant spatial association between regulatory T cells and dendritic cells?"

Claude can pull the relevant population data, spatial statistics, and region membership via MCP and reason about them — without the user having to know which specific function to run or which plot to make. The MCP tool `get_spatial_stats` (or equivalent) should return structured results that are natural language-interpretable, not raw matrices.

This is where the combination of flexible spatial analysis + MCP + region clustering makes Cecelia genuinely more powerful than CytoMAP: a researcher can ask a question in plain language and Claude can both compute the answer from available data AND suggest how to visualise it in the analysis canvas or a Pluto notebook.

Opus: when designing the spatial analysis output formats, consider whether they are MCP-friendly (small, structured, interpretable) in addition to being scientifically correct. A flat table of "population pair → mean distance → p-value" is more MCP-useful than a raw distance matrix.

**This MCP accessibility rule applies equally to the CytoMAP parity readouts.** Every output listed in the CytoMAP parity target section — neighbourhood statistics, Pearson correlation between population densities, per-region population frequencies, spatial association p-values — must be MCP-accessible in a structured form. The goal is that Claude can answer:

> "Which cell types co-localise in this sample?"
> "Is CD8+ T cell proximity to dendritic cells statistically significant?"  
> "What fraction of regulatory T cells live in the tumour region vs. the stromal region?"

...by pulling the relevant CytoMAP-equivalent readouts via MCP, without the user having to run a specific plot or export a CSV. Design the stored output format for each analysis function with this in mind — if the result isn't structured enough for Claude to interpret, it isn't structured enough.



1. **Where are neighbourhood relationships stored?** Options:
   - AnnData `obsp` (sparse adjacency matrix — standard for spatial graphs, what squidpy uses)
   - Separate file per image in the task dir
   - H5AD `obs` columns (label IDs of neighbours per cell — inflexible)
   Opus: recommend based on what squidpy writes and what downstream queries need.

2. **Where are neighbourhood composition vectors stored?** Intermediate data — probably not persisted unless the user wants to reuse them without recomputing.

3. **Where are region labels stored?** Same as cell cluster labels — in the population map, accessible via `pop_dt(img, "region", [...])`. Region populations appear in the population manager alongside other types.

4. **How is the temporal dimension handled for live imaging?** Per-timepoint neighbourhood composition, or aggregate across time? Should be a parameter the user controls.

---

## Build order

1. Spatial data model agreement (Opus resolves the four questions above)
2. squidpy vs. Julia recommendation per function
3. Neighbourhood computation function (used by both spatial analysis and region clustering)
4. Basic spatial analysis functions (neighbour detection, distance stats)
5. Region clustering: composition vector → SOM/k-means → region labels
6. Cross-poptype containment query: `region_membership`, `region_enrichment`
7. Region clustering module page (Vue, same pattern as cell clustering page)
8. Napari visualization for region populations
9. Live imaging extension (temporal neighbourhood — may be a separate sub-phase)

---

## Constraints

- `region` is a dedicated poptype, not `clust`. The population manager must display it.
- All module functions REPL-runnable without API or Vue
- squidpy evaluation is mandatory before writing any spatial Julia code
- CytoMAP attribution on every derived function
- Region labels stored in population map, same mechanism as cell clusters
- Cross-poptype query is a first-class requirement, not an afterthought
- Live imaging extension should be scoped separately if it risks delaying static image support
