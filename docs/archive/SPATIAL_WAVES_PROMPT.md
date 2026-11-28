# SPATIAL_WAVES_PROMPT

You are auditing and designing a feature for Cecelia (`schienstockd/cecelia`). Read the repo first. Do not build anything until the validation plan (section 6, item 7) is agreed. These are ideas, not decisions.

## 1. Context

Cecelia now has a function that extracts regions on images, computes each cell's distance to the region edge, and flags whether the cell is inside or outside the region. Three questions:

1. **Readouts.** Which standard readouts should this function provide, compared with QuPath and other packages?
2. **Waves.** How do we describe spatial "waves" of cell populations within a tissue region, meaning the ordered, overlapping distance profiles of populations, and use them to define spatial interactions between populations?
3. **Gap between static and live.** Many spatial analysis methods from multiplex spatial microscopy (region distances, bands, neighbourhoods, cross-population statistics) are not available for live cell imaging, and live-imaging track analysis rarely uses them. The aim is to close that gap: make the spatial methods work on live data in the same framework, not to claim novelty. Decide which static methods transfer to live data directly, which need a time-aware version, and which do not transfer.

Locate the region-extraction function and its output schema yourself. Do not assume file names.

## 2. Example: image 2dR91B

Static multiplexed tissue image, 2D. About 171k cells on this plane. Observations from the viewer screenshots:

- Region outlines (pink in one view, white in another) are many small islands and one large contiguous region with holes. Treat multi-part regions as a first-class case.
- Structure is follicle-centric, not edge-centric. CD20 B cells form dense cores. Mixed lymphoid cells surround them. Myeloid cells sit at the interface. Stromal cells are outermost and sparse.
- A distance-to-tissue-edge axis blurs this. The natural axis is the signed distance to an anchor (B-cell cluster or follicle core).
- Populations come in two sets: plain, and a duplicated "(AF)" set (e.g. Epithelial 31712 vs Epithelial (AF) 21635; CD3-PGP95, CD45 low (AF) 18726 vs 7 in the plain set; Endothelial 4083 vs Endothelial (AF) 16225). Decide how to handle this: merge, exclude AF, or analyse separately. Check whether AF is an annotation pass, an autofluorescence-corrected pass, or a different classification.
- Empty slide (black) is large. Densities must be normalised to available tissue area per distance bin.

Use 2dR91B as the worked example and as the first test case.

## 3. Research summary (what other tools provide)

### Per cell
- QuPath: `DistanceTools` computes the distance from every detection centroid to the closest annotation of each class, with a signed variant. Negative inside, positive outside. Split by class is optional.
  https://qupath.github.io/javadoc/docs/qupath/lib/analysis/DistanceTools.html
  https://montpellierressourcesimagerie.github.io/qupath-bcm-workshop/exercises/cells-classification/ex1.html
- SPIAT (R): `calculate_distance_to_margin` adds a `Distance.To.Border` column per cell.
  https://rdrr.io/github/TrigosTeam/SPIAT/man/calculate_distance_to_margin.html
- SpatialCells (Python): annotates each cell with its region, extends boundaries by a user distance, and computes infiltration score and tumor-immune distance.
  https://pmc.ncbi.nlm.nih.gov/articles/PMC11066940
- Enable Medicine: Euclidean distance transform (scipy `distance_transform_edt`) on the mask and the inverted mask, sampled at each cell centroid; inside distances negative.
  https://manual.enablemedicine.com/Distance-from-Tumor-Boundary-Analysis-3a2ef55b481e45719f68d5d8008a0431

### Per region
- QuPath scripts (Svidro gist collection): per-class cell counts, percentage and cells/mm² written to the parent annotation; area, perimeter, circularity; summary measurements per annotation.
  https://gist.github.com/Svidro/68dd668af64ad91b2f76022015dd8a45

### Distance bands
- QuPath scripts: rings around an annotation (stroma within X, 2X, 3X µm of a tumor); tumor-invasion areas from tissue border and tumor annotation.
  https://gist.github.com/5829ba53f927e79bb6e370a6a6747cfd
- Gastric cancer study: tumor centre, inner invasive margin, outer invasive margin, with tile density per zone.
  https://scholarlypublications.universiteitleiden.nl/access/item%3A3762280/download
- Enable Medicine: density per band per phenotype as a curve per sample; weighted average distance in and out.

### Region-level spatial statistics
- Region-to-cell extension of Ripley's K across level sets; signed distance-density (sDD) space combining boundary distance with local density.
  https://www.ncbi.nlm.nih.gov/pmc/articles/PMC13620163/
- SASHIMI: 27 spatial summary statistics.
  https://arxiv.org/pdf/2512.06116v2
- Spatiopath evaluates cell-to-tumor relationships against nulls that account for tissue geometry (cited in https://arxiv.org/pdf/2609.27755).

### Caveat from the literature
Without accounting for tissue inhomogeneity, proximity measures can reflect sampling artefacts rather than real structure. Nulls must respect tissue geometry.

### Live imaging (what exists, found in a quick search)
This is partial. Do your own search (see section 6, item 1) and treat the list below as a starting point, not a verdict.
- CellTracksColab has a "Distance to ROI" notebook and spatial clustering (Ripley's L with Monte Carlo). Its paper says it can track distances over time and compute related metrics. Input comes from TrackMate, CellProfiler, Icy, ilastik and Fiji Manual Tracker. I did not open the notebook, so it is unknown whether it gives signed distance, per-step radial velocity, crossings or dwell time.
  https://github.com/CellMigrationLab/CellTracksColab
  https://www.ncbi.nlm.nih.gov/pmc/articles/PMC11335138/
- celltrackR (R): distance and angle to a reference point or plane, but the distance is for the first coordinate of the track (a per-track measure). Also step-based and staggered analyses and track simulation, which are relevant for nulls.
  https://cran.ma.imperial.ac.uk/web/packages/celltrackR/refman/celltrackR.html
- A survey table of track-analysis tools lists "angle/distance to point" and "angle/distance to plane" as features and marks TrackMate as lacking them. Snippet was garbled; verify.
  https://pmc.ncbi.nlm.nih.gov/articles/PMC10079262/table/T1
- One-off published analyses doing region-relative track work by hand: T-cell velocity vs distance to tumor margin (https://pmc.ncbi.nlm.nih.gov/articles/PMC6423744/figure/Fig4); track speed stratified by distance to SHG collagen fibers and to tumor cells (https://pmc.ncbi.nlm.nih.gov/articles/PMC8102369/figure/F7); T-cell speed varying by region and local tumor density in human live biopsies (https://pmc.ncbi.nlm.nih.gov/articles/PMC8439597).
- Commercial tools (Imaris, Arivis and similar) have distance-to-surface statistics. Not checked.

Not found in this search: signed distance with a hysteresis margin for crossings; per-step radial and tangential velocity against a region gradient; flux per band over time; geometry-aware nulls for tracks; behaviour-defined regions with a circularity control; and linking track readouts to static waves in one framework.

## 4. Proposed design to audit (challenge it)

### 4.1 Readouts to compare against the current function
Per cell: signed distance with region label, distance to nearest region of each class (separately from the union), region ID for multi-part regions, optional normalised depth (distance / local region thickness).
Per region: area, perimeter, solidity, number of components, cell count and density per population, fraction positive.
Per band: density and fraction per population per distance band, density-vs-distance profile, mean/median signed distance per population.
Per region summary: infiltrated fraction (share of a population inside), share within X µm of the edge, distance at which density falls to half.

### 4.2 Waves
1. Pick an axis. Default: signed distance to an anchor (cluster of a chosen population, or follicle core). For many anchors, assign each cell to its nearest anchor and measure distance to that one, never to the union. Keep distance to region edge as a second axis.
2. Per population, bin distance in µm. Counts per bin divided by tissue area available in that bin (from the distance transform). Report density and fraction of the population per bin. Smooth lightly.
3. Describe each wave: peak position, centroid (mean distance), width (SD or FWHM), onset (distance where 10% has accumulated), skew. Wave order = populations sorted by peak.
4. Interactions: cross-correlation between pairs of profiles (lag = offset, height = co-occurrence). Cell-level interactions via cross-K or nearest-neighbour statistics against a null that shuffles population labels within distance bins, which preserves the shared gradient. Report enrichment z-score or permutation p per pair per band.
5. Compute per region first, then aggregate across regions and images. Minimum cell count and minimum area per bin. Absolute µm and normalised depth both.

Target output: one tidy table (image, region, anchor, population, bin, n, area, density), with peak, centroid, width, order rank and the lag matrix derived from it.

## 5. Test scenarios

Three scenarios, each with a pass criterion. They extend section 4 to live data and behaviour-defined regions. Treat all as ideas to validate, not commitments.

### Scenario 1: static waves around a follicle (image 2dR91B)
- Setup: anchor = follicle core (B-cell cluster). Axis = signed distance to nearest anchor. Zones expected from the screenshots: B core, lymphoid mixed, myeloid at the interface, stromal outside.
- Readouts: density per tissue area per distance bin per population, peak position, width, wave order, peak offsets between pairs.
- Pass: the recovered order matches the visible B → lymphoid → myeloid → stromal order; peaks are stable under bin width, smoothing and anchor choice; label shuffling within bins removes the pairwise signal.

### Scenario 2: live tracks crossing a region edge
- Setup: regions fixed (drift-corrected) or recomputed per frame; state which. Cells tracked across the edge of a region from section 4.
- Readouts per step: speed, radial velocity (step component along the distance gradient), tangential velocity, turning angle, all vs signed distance. Per track: time inside/outside, dwell per band, entry and exit events, time to first crossing. Per population: net flux per band over time.
- Noise control: a crossing requires passing a margin around the edge (hysteresis), not just touching zero. Choose the margin from localisation error and test several widths.
- Null: a persistent random walk with matched step and turning distributions in the same geometry, or tracks randomly shifted or rotated within the region.
- Biases to handle: tracks lost near the edge (survivorship), frame-interval dependence of speed, tissue drift.
- Pass: a speed dip or radial drift near the edge that exceeds the null, is stable across margin widths, and replicates in a second movie. If it does not, reduce the live extension to the static readouts applied per frame, and say which dynamic readouts were dropped.

### Scenario 3: region defined by behaviour
- Setup: define a region from behaviour feature A (e.g. arrest coefficient or an HMM state), build signed distance and rings around it, then quantify behaviour per ring.
- Circularity: A decays outward by construction, so it carries no information. Only held-out measures count.
- Controls (implement at least two): quantify a different feature B than the one that defined the region; define the region from one population and measure another; define the region from half the cells or one time window and quantify the rest; compare against regions from permuted features with the same geometry.
- Also test: sensitivity of the region to its smoothing and threshold; static vs per-window regions; per-step assignment of positions to rings; normalisation to tracked-cell density so low-density areas are not read as "no behaviour".
- Pass: a held-out feature shows a gradient across rings that permuted-feature regions do not reproduce, and that survives changes to region threshold.

### Additional aspects to cover in the analysis
- Replication unit: infer at region or image level, with cells as measurements within it (mixed model or per-region summaries), not at cell level.
- Edge censoring: image border and tissue holes truncate bands; correct or drop mostly censored bands.
- Region sensitivity: perturb boundaries and check that wave order survives.
- Bin width not below about one cell diameter.
- Classification uncertainty: weight by class probability where available; the AF duplicates in 2dR91B are a likely case.
- 2D section of 3D tissue: compare fractions and shapes, not absolute densities, across sections.
- State along the axis: marker expression vs distance, not just counts.
- Second axis: distance to epithelium or vessels in addition to follicle distance.
- Cross-check waves against local neighbourhood composition.
- 3D live data: anisotropic voxels need a 3D distance transform with correct spacing.
- Scale: use a raster distance transform sampled at centroids, not polygon distances per cell.
- Prior art: see section 6, item 1. The point is reuse and closing the static-live gap, not novelty.
- Output: population × distance heatmap, ridge plots ordered by peak, band overlay on the image for QC. Store anchor, bin width, smoothing, margin and null with every result table.

## 6. What to deliver

Write `docs/todo/SPATIAL_WAVES_ANALYSIS.md` containing:

1. **Own search and prior art.** Do your own literature and software search; the research above was quick and incomplete. Cover: multiplex spatial tools (QuPath, SPIAT, SpatialCells, Squidpy, scimap, Giotto and others), live-imaging and track-analysis tools (CellTracksColab, celltrackR, TrackMate and its ecosystem, Imaris, napari plugins, Python track-analysis packages), and papers that apply spatial statistics to tracked cells. Check CellTracksColab's Distance to ROI notebook directly. Report what exists, what you could not verify, and what is reusable instead of rebuilt.
2. **Static-to-live method map.** List the static spatial methods (region distances, bands, neighbourhood composition, cross-K or nearest-neighbour enrichment, waves, region clustering) and mark each as: transfers directly to live data, needs a time-aware version (and what that is), or does not transfer (and why). This is the core of the static-live gap work.
3. **Gap analysis.** Current function output vs section 4.1. Mark each readout as present, missing, or not worth it, with a reason.
4. **Axis decision for 2dR91B.** Anchor choice, how multi-part regions and multiple follicles are handled, what to do with the AF duplicates.
5. **Wave definition.** Confirm or change section 4.2. Say which statistics are robust at this cell count and which need smoothing or pooling.
6. **Null model.** Which null for interactions, and why it is not fooled by shared gradients or empty slide.
7. **Validation plan with kill criteria, before any build.** Cover the three scenarios in section 5, starting with scenario 1 on 2dR91B. Cheap tests on 2dR91B: (a) do the waves reproduce the visible B → lymphoid → myeloid → stromal order; (b) are peaks stable under bin size, smoothing, and anchor choice; (c) do shuffled labels within bins give no interaction signal; (d) does a second image or region give a comparable order. State what result would make us drop the idea or simplify it.
8. **Minimal build.** Smallest useful slice (probably the tidy table plus one profile plot), where it lives in the existing architecture, and what to defer. Flag where it overlaps with existing region, lens, or neighbourhood-graph features.
9. **Open questions** that need my decision, including whether the live (scenario 2) and behaviour-region (scenario 3) parts are in scope for the first build.

## 7. Constraints

- Verify claims against the repo and the data, not against this prompt.
- Prefer cheap tools (scipy distance transforms, existing neighbour graph) over new dependencies. Check licences before suggesting packages.
- Keep the figure and readout naming consistent with existing Cecelia conventions.
- Be concise. List what you checked and what you could not check.
