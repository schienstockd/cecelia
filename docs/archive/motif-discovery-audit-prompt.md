# Audit Prompt: Sub-Behavior Motif Discovery in Cecelia.jl

## Context

Cecelia currently classifies cell tracks at two levels:
1. **HMM states** — per-timepoint/short-window behavioral labels (e.g. arrest, migrate)
2. **Track-level clustering** — Leiden clustering over whole-track features, producing one label per track

There is no representation for **sub-track recurring motifs** — a shared temporal pattern shorter than a full track and not equivalent to a single HMM state (e.g. "approach-to-DC" vs "approach-to-tumor-cell", each a multi-step composite of state transitions + spatial context).

## Proposed addition

Motif discovery over multivariate per-timepoint feature streams, where features include:
- Existing kinematic features (speed, turning angle, etc.)
- Distance-to-other-cells / distance-to-structures (already computable from track + population data)
- HMM state at each timepoint (discrete channel, used either as an additional feature or as the primary alphabet for discrete motif search)

Candidate methods: matrix profile (STUMPY `mstump`, multivariate, window-based) for continuous features; subsequence/open-end DTW for partial-match motif detection where instances vary in length/speed and aren't well captured by a fixed window; discrete sequence motif search (SAX-style, or direct substring/edit-distance search) over HMM state sequences. Output: motif instances → clustered into discrete sub-behavior classes → track annotated as a *sequence* of sub-behaviors rather than one dominant state/cluster.

## References & inspiration

Movement ecology and ethology are ahead of us here — worth citing as precedent/inspiration, same role momentuHMM played for the HMM step:

- Berman, Choi, Bialek, Shaevitz (2014), *Mapping the stereotyped behaviour of freely moving fruit flies*, J. R. Soc. Interface 11:20140672. DOI: 10.1098/rsif.2014.0672 — https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4233753/. Unsupervised discovery of >100 stereotyped motifs from movement data with no predefined ethogram. Closest conceptual match to "let sub-behaviors emerge from the data."
- Luxem, Sun, Fuseler, Krause et al. (2022), VAME, *Communications Biology* 5:1267. https://www.nature.com/articles/s42003-022-04080-7 — variational embedding of animal motion into discrete motifs, clustered further into hierarchical "communities." Closest existing tool for the motif → sub-behavior-class clustering step.
- Vissat et al. (2021) relative-motion method, applied in a lion/hyena dyadic movement study: *Predators in motion...*, Frontiers in Ethology (2026). https://www.frontiersin.org/journals/ethology/articles/10.3389/fetho.2026.1901233/full — classifies one animal's movement *relative to a second individual*, directly analogous to "approaching DC" vs "approaching tumor cell" defined relative to a target, not just self-kinematics.
- Tiger interaction analysis, ORTEGA method: *Analyzing tiger interaction and home range shifts using a time-geographic approach*, Movement Ecology (2024). https://link.springer.com/article/10.1186/s40462-024-00454-0 — categorizes interaction *types* (encounter, following, latency, avoidance) from trajectory intersections between individuals; a reference for classifying approach/engagement types rather than just detecting them.
- Gurarie, Andrews, Laidre (2009), *A novel method for identifying behavioural changes in animal movement data*, Ecology Letters 12(5):395-408. DOI: 10.1111/j.1461-0248.2009.01293.x. R package (BCPA): https://github.com/eligurarie/bcpa — standard movement-ecology segmentation baseline alongside HMM.
- Garriga, Palmer, Oltra, Bartumeus, *Expectation-Maximization Binary Clustering for Behavioural Annotation*, PLOS ONE. DOI: 10.1371/journal.pone.0151984. R package (EMbC): https://CRAN.R-project.org/package=EMbC — another standard movement-ecology segmentation baseline, unsupervised and multivariate.
- UCR Matrix Profile applications page: https://www.cs.ucr.edu/~eamonn/MatrixProfile.html — documents matrix profile use directly on animal/entomology behavioral time series, as precedent for the specific method (not just motif discovery generally).

## Task

Audit how this would fit into the current Cecelia.jl architecture. Do not implement — produce a feasibility and integration report.

### Phase 1 — Data audit
- Confirm what per-timepoint feature streams already exist in track/population data structures (`pop_df`, `LabelProps` chain, TrackMeasures.jl) and in what schema (H5AD, physical vs pixel units).
- Confirm whether distance-to-other-cells/structures is already computed anywhere (spatial analysis / region clustering module) or needs new computation, and at what cost for typical dataset sizes (track count, timepoints, cell density).
- Confirm HMM state sequences are stored per-track in an accessible, indexable form (not just aggregate cluster labels).

### Phase 2 — Compute placement
- Weigh matrix profile vs subsequence DTW for motif detection: matrix profile needs a fixed window and scales better; DTW handles variable-length/variable-speed partial matches natively but is more expensive pairwise. Recommend which (or both, at different stages) fits the actual data.
- Determine whether the chosen method belongs in Julia (native, consistent with Julia-as-scientific-layer decision) or is a case for PythonCall.jl bridging to STUMPY (matrix profile) — flag if a Julia-native equivalent exists for either approach.
- Estimate compute cost/scaling for `mstump`-style multivariate matrix profile and for pairwise subsequence DTW at realistic track counts and window sizes; identify whether this is a batch/offline job or needs interactive responsiveness.
- Identify where in the existing pipeline (Vue Flow pipeline whiteboard / chain executor) this would slot as a node — inputs, outputs, params (window size, top-k motifs).

### Phase 3 — Clustering & storage integration
- Discrete-vs-continuum decision: hard clustering (Leiden, as used elsewhere in Cecelia) gives interpretable, Cell-Card-ready categories but forces boundary cases into one bin; a continuum representation (motifs as basis functions / mixture weights, cf. motif-based continuous dynamics — Learning Task-Agnostic Motifs to Capture the Continuous Nature of Animal Behavior, arXiv 2506.15190) is more faithful to the underlying biology but harder to communicate and doesn't map onto existing discrete population/UI infrastructure. Recommend clustering for the UI/output layer, but require every motif instance to carry a confidence or distance-to-centroid value alongside its hard label, so ambiguous/boundary instances aren't silently treated as equally certain as clear-cut ones.
- Known failure mode to guard against: cluster/resolution parameters (e.g. Leiden resolution, number of motif classes) can be tuned post hoc to produce a population distribution that matches an expected or desired result ("resolution picking"), especially once a specific cluster's size is treated as a result that must replicate. The confidence value partly mitigates this by making it visible when a population shift is actually a shift in confident assignments vs. a shift caused by boundary cases moving between bins at a given resolution. Audit should specify a fixed protocol for choosing/reporting resolution (e.g. resolution chosen and locked before looking at group comparisons) rather than leaving it to be reset per analysis.
- Determine how motif instances get clustered into discrete sub-behavior classes: reuse existing Leiden clustering machinery, or does variable-length/variable-alignment motif data need a different similarity metric first (DTW distance matrix, then Leiden) — this is likely needed regardless of whether matrix profile or DTW was used for detection, since motif instances will rarely be identical length.
- Design the storage schema for: motif definitions, motif instances (track ID, time range, class label, confidence/distance-to-centroid), and per-track sub-behavior sequences. Confirm compatibility with existing Population Manager types (flow/live/clust/region) — is this a new population type or an annotation layer on existing tracks?
- Identify whether sub-behavior sequences should also become an input feature to existing track-level clustering (i.e. feed back upstream) or stay a separate downstream annotation.

### Phase 4 — UX/output integration
- This should live on the existing behaviour module page, not a new page/surface — confirm what's already there (HMM state views, track cluster views) and how sub-behavior motifs slot in alongside them without duplicating navigation/layout.
- Assess fit with the planned "Cell Cards" feature — could sub-behavior motifs become a card attribute or a separate card type per motif class?
- Identify what visualization is needed (e.g. motif instance viewer, sub-behavior timeline per track) and whether it fits existing floating-plots/AlgebraOfGraphics.jl infrastructure or needs something new.

## Deliverable

A written report answering the above, structured as: data readiness, compute/architecture placement, storage/schema proposal, open risks or blockers, and a recommended minimal proof-of-concept scope (smallest slice to validate the approach before full pipeline integration). Flag any point requiring a design decision rather than deciding it.
