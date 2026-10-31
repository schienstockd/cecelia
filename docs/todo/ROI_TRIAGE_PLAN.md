# ROI discovery + Claude triage

Status: **in progress** (2026-10-27) · branch `feat/roi-triage` · P1 built (generic discovery task,
ROI sidecar + review routes, viewer ROI section, containment on mesh contacts); P2–P4 open. Brief:
[`docs/archive/OPUS_ROI_TRIAGE_PROMPT.md`](../archive/OPUS_ROI_TRIAGE_PROMPT.md). Test case:
zolIMa / `UMq4v3` (M2b MERTK, cropped; 181 t × 4 c × 35 z × 468 × 467, 0.33 µm xy, 2 µm z, 15 s/frame).

## Goal

Find the few places in an image where the objects of a population gather, rank them, and review
them in the viewer — with Claude labelling candidates, the person deciding. **Any image, any cells,
any question.** The brief's case (B-cell fragments taken up by tingible-body macrophages, TBMs) is
the test case, not the design.

**The question lives in the population, not in the task.** Segmentation + gating already express
"which objects matter": a gate, or a filter population on any measure (PopulationManager → *New
filter population*, compound conditions; `docs/POPULATION.md`). "Fragments inside a macrophage" is
`frags` filtered on `contained_by#<macrophages>` > 0 (from Cell contacts (meshes) → *Containment*);
"naked nuclei" is `nuc` minus `mem`. Find ROIs then answers one general question: **where do these
objects gather in space and time?**

**Numbers come from the pipeline. Judgement comes from Claude and the person.** Claude is not the
tracker and not a coordinate source.

Tags: **[code]** verified in code / on disk · **[data]** measured on UMq4v3 · **[inferred]**.

---

## 1. Audit

### 1a. The test image and its object table [data]

- Image `zolIMa/0/UMq4v3/ccidImage.ome.zarr`: `(t,c,z,y,x) = (181, 4, 35, 468, 467)` uint16.
  Channels `SHG`, `nuc-GFP`, `mem-TOM`, `CD169-Kat`. **`ccid.json` says `SizeZ: 31`; the store and
  labels have 35.** Geometry comes from the store, never ccid `meta`.
- Segmentation `Bcells` (coastal two-model run): 247,853 objects over 181 frames. `mean_intensity_k`
  is channel k, so CD169 = `mean_intensity_3`.
- Gating (`gating/Bcells.json`): `/qc` (volume × surface/volume), `/qc/cells` and `/qc/frags` (volume
  × solidity), `/qc/frags/nuc` and `/qc/frags/mem` (both polygons in the TOM × GFP plane — **not**
  "green only" / "magenta only", and not exclusive), `/qc/frags/small` and `/qc/frags/big`.
  53,555 objects in `frags`, 89,074 in `qc`.
- No macrophage segmentation exists on this image.

### 1b. Macrophages: segment, don't threshold [data, decision]

The first P1 thresholded the CD169 channel inside the task (smooth → threshold → per-plane hole fill
→ min size). Rejected:

- **It is a hidden segmentation** — not visible, not correctable, not reusable — and it cost four
  settings. A threshold is per image and can drift across timepoints, so it would have to be set by
  hand for every image.
- **It was fragile** [data]: one-variable sweep (§9) — thresholds 4 / 6 / 8 / Otsu each kept a
  different 68–84% of the sites of the threshold-6 run.
- B-cell bleed-through is in CD169 on this image (r = 0.15 with TOM, so no linear unmixing), and dim
  TBMs are missed at any threshold that excludes the bleed [data, visual].

Instead: **segment macrophages** with the existing segmentation tasks (cellpose / coastal on CD169),
correct them with the existing tools, then run **Cell contacts (meshes)** with **Containment** on:
`contained_by#<target>` (0/1, the A object sits inside a B object) and `contains_n#<target>` (B
objects inside this A object — the per-TBM fragment count of the MERTK question). This is the old R
`checkBContainsA` (`old-R-shiny-version/inst/modules/sources/spatialAnalysis/py/cell_contacts_mesh.py`),
dropped in the port. It is measured on label voxels **after per-plane hole filling**: an engulfed
fragment is dark in CD169, so the host's segmentation has a hole where it sits, and an unfilled test
reads it as outside. The dim-TBM problem does not go away — it moves into segmentation, where it can
be seen and fixed.

### 1c. Existing pieces reused [code]

| Need | Exists | Where |
|---|---|---|
| Pop membership → labels | `pop_df_multi` | `app/src/gating/popmanager/` |
| Filter populations on any measure | PopulationManager *New filter population* | `docs/POPULATION.md` → *User-defined filter populations* |
| Surface contacts (+ containment, new) | `spatialAnalysis.contactsMeshes` | `contactsMeshes.jl`, `cell_contacts_mesh_run.py`, `mesh_utils.containment_per_target` |
| Measure picker | `labelPropsColsSelection` (+ `allObs`, new) | `frontend/src/tasks/ParamRenderer.vue` |
| Sidecar path convention | `img_stats_path` / `img_stats_suffixes` | `app/src/model/image.jl` |
| Atomic JSON | `write_json_atomic` | `app/src/utils.jl`, `python/cecelia/utils/atomic_io.py` |
| Server-side stills | `render_view_stills` | `api/src/movie_render.jl` |
| Viewer box framing / highlight | `buildFocusViewState`, `setPickHighlight` | `frontend/src/utils/viewer/focusOnCell.ts`, `stores/viewer.ts` |
| Prev/next, list | `reviewPager`, `SelectionTable` | `frontend/src/utils/reviewPager.ts`, `components/SelectionTable.vue` |
| Attribution, "Claude proposes" | `author_stamp()`; Blackboard verdict rule | `app/src/config/profile_settings.jl`, `api/src/blackboard_run_review.jl` |

## 2. The method [code, data]

`python/cecelia/utils/roi_utils.py` (`find_rois`). No tracking, no pixels — the object table only.

1. **Pool fixed time windows** (`WINDOW_FRAMES` = 12 frames). A place objects keep returning to piles
   up over a window; a passing crowd smears out. Persistence without tracking or frame linking.
2. **Group the pooled positions with HDBSCAN** (`leaf` selection, core members only —
   membership ≥ 0.5). HDBSCAN finds groups at whatever density they occur, so its one setting is a
   COUNT (*Min objects*), not a radius that would have to match the cell size.
   - `leaf`, because the default `eom` keeps the most persistent cluster — on UMq4v3 the whole
     follicle against empty space, one group per frame the size of the image [data].
   - Core only, because HDBSCAN hands every point to some group, so a group would also collect the
     sparse objects around it (membership ~0.05 against ~1 for the core) [data].
3. **Rank by excess over the background**: summed object weight minus what the window's average
   density would put in a box the group's size. Weight is 1 per object, or the mean percentile of the
   *Rank by* measures. A loose group at background density scores ~0 and is dropped; a compact one
   scores high. Ranking by plain count put big loose groups first and the known TBM at rank 9 [data].
4. **Sites**: ROIs whose boxes overlap (IoU ≥ 0.3, any window) share a site, so a sessile event in
   every window is seen as one place.

Rejected on the way [data]: 4D DBSCAN (needs a radius and a µm-per-frame conversion; a 3 µm radius
gave no ROIs), per-frame groups chained by box overlap (transitively joins the image), one-to-one
chaining (boxes drift with loose groups).

**Catch**: HDBSCAN is relative — evenly spread objects still produce some groups. The excess ranking
is what separates real gatherings from that; the bottom of the list is noise by construction.

## 3. Prior art (verified via Crossref / PubMed / arXiv)

- **TBM biology.** Grootveld *et al.*, *Cell* 186:1144 (2023), doi:10.1016/j.cell.2023.02.004 —
  TBMs are sessile and capture fragments with processes. Gurwicz *et al.*, *J Exp Med* 220:e20222173
  (2023), doi:10.1084/jem.20222173 — uptake at protrusions. Baudon & Bajénoff, *JEM* (2023),
  doi:10.1084/jem.20230250. Mayer *et al.*, *Science* 358:eaao2602 (2017),
  doi:10.1126/science.aao2602 — up to half of GC B cells die every 6 h. Rahman *et al.*, *J Immunol*
  (2010), doi:10.4049/jimmunol.1001187 — Mer-deficient TBMs clear poorly. **Consequence:** for this
  question containment beats centroid distance — hence the containment port.
- **Event detection.** Held *et al.*, CellCognition, *Nat Methods* 7:747 (2010),
  doi:10.1038/nmeth.1486. Wu *et al.*, *Bioinformatics* 39:btad584 (2023),
  doi:10.1093/bioinformatics/btad584. Ounissi *et al.*, PhagoStat, *Sci Rep* 14:6482 (2024),
  doi:10.1038/s41598-024-56081-7. None is 3D intravital.
- **Density clustering.** Campello, Moulavi & Sander, HDBSCAN, *PAKDD* (2013),
  doi:10.1007/978-3-642-37456-2_14 (scikit-learn ≥ 1.3 `sklearn.cluster.HDBSCAN`).
- **VLM triage.** μ-Bench arXiv:2407.01791; MicroVQA arXiv:2503.13399 (best ≈ 53%); Wu *et al.*
  arXiv:2310.09909; "VLMs are blind" arXiv:2407.06581; Kamath *et al.* arXiv:2310.19785; Pang *et al.*
  arXiv:2605.30170. **Consequence:** never ask Claude to count or localise; give it the outlined
  candidates, the numbers and a label question, and measure its error rate.

## 4. Error modes the triage loop must catch

| Error | Detection | Action |
|---|---|---|
| **The population asks the wrong question** (ungated objects, a bleed-through-driven macrophage) | the sweep: dropping the `frags` gate kept 40% of sites [data] | not the task's to fix — the population is the user's; the ROI set records its populations |
| **Engulfed object reads as outside** | containment on hole-filled labels (`test_mesh_containment.py`) | segmentation must close the host around the hole; check outlines |
| **Projection ambiguity** (MIP overlap ≠ inside) | `contained_by#` from the pipeline vs Claude's claim | Claude must cite the measure, not the picture |
| **Contrast drift between frames** | one spec set for every montage row | the montage builder refuses per-row specs |
| **Hallucinated ids / frames** | `objectIds` ⊄ the ROI's objects, `frames` ∉ `[t0, t1]` | 400 from `POST /api/rois/review` (tested) |
| **Label outside the set's vocabulary** | not in the ROI set's `labels` | 400 (tested) |
| **Stale ROI after a re-run** | review id absent from the set | kept as *orphaned*, never deleted |
| **Wrong geometry source** | shape read from the label store | tested |
| **Duplicate ROIs** (one place, several windows) | box IoU ≥ 0.3 → same `site` | the list shows the site |

## 5. Co-driving model

- **The ROI list is the shared surface.** The person and Claude read and write the same sidecar
  through the same routes.
- **Claude proposes, the person decides.** A `via: claude` write may set `claudeLabel` and flag an
  undecided ROI. It may **not** accept / reject, set the person's label or note, or delete. Every
  change appends `{at, by, field, from, to}`; undo replays it backwards; Claude may undo only its own
  latest change.
- **Labels fit the question.** Each ROI set carries its own review labels (*Review labels* on the
  run, default `of interest, artefact, unsure`); the person's and Claude's labels are checked against
  them.
- **Knobs.** Four: Population, Rank by, Min objects, Max ROIs. A re-run is a task run — reproducible,
  in the run log. Claude can suggest one in prose; it cannot start the task (MCP is read + additive),
  so the brief's `roi_generate` is not built.

## 6. Validation protocol

1. **Labels.** The person hand-labels ~30 ROIs of one set in the panel, stratified: top 10, 10 from
   ranks 11–40, 10 random from the rest, with Claude's labels hidden (P3 blind toggle).
2. **Triage.** Claude labels the same 30 with a fixed prompt; model id recorded.
3. **Report.** Cohen's κ and agreement per label and per confidence bin (`<0.5 / 0.5–0.8 / ≥0.8`),
   confusion matrix; CSV export (P3).
4. **Acceptance bar** (set before seeing results): precision ≥ 0.8 at confidence ≥ 0.8 for the label
   of interest; recall of the person's label-of-interest among Claude's same-or-`unsure` ≥ 0.9;
   κ ≥ 0.6. Below the bar Claude stays an ordering aid.
5. **Discovery check.** Fraction of the person's label-of-interest ROIs in the top 20. Below 0.5 →
   rethink the population before any Claude work.

## 7. GUI + MCP design

**Task form** ("Find ROIs", Spatial analysis → Regions of interest): Name · **Population** ·
**Rank by** (optional, any measure incl. spatial readouts — `allObs`) · **Min objects** (pooled over
12 frames, default 20) · **Max ROIs** · **Review labels**. Down from 26 settings.

**Panel.** A "ROIs" section in the viewer window's side rail (`ViewerWindow.vue`, key `rois`):
`components/RoiSection.vue`, logic in `utils/rois.ts`. Set picker, `pageSummary` + prev/next, status
`ChipSelect`, `SelectionTable` (# · site · objects · t · status · Claude), accept / reject / flag /
undo, the set's labels, a note. A row moves the viewer (seek t/z, fit the box) and outlines the ROI's
objects (pick highlight).

**State.** Definitions `{img}/rois/{suffix}.json` (task output, atomic, immutable between runs);
reviews `{img}/roiReviews/{suffix}.json` (API, under a lock; kept by an analysis reset, like
`gating/`). ROI ids hash the t-window and box, so an identical re-run keeps them.

```jsonc
// rois/{suffix}.json  (task output)
{ "version": 2, "suffix": "frags", "valueName": "Bcells", "generatedAt": "…",
  "params": { …every task param… }, "labels": ["of interest", "artefact", "unsure"], "rankBy": [],
  "geometry": { "shape": {"t":181,"z":35,"y":468,"x":467}, "umPerPx": {"x":0.33,"y":0.33,"z":2},
                "frameIntervalS": 15 },
  "rois": [ { "id": "r3f2a91c0", "rank": 1, "site": 1, "score": 106.4, "expected": 9.6,
              "t0": 0, "t1": 12, "box": {"x0":…,"x1":…,"y0":…,"y1":…,"z0":0,"z1":12},
              "centroid": {"t":5.4,"z":4,"y":250,"x":181}, "nObjects": 116, "nFrames": 12,
              "meanWeight": 1.0,
              "objects": [ {"label":145529,"t":3,"x":…,"y":…,"z":…,"weight":1.0} ] } ] }
// roiReviews/{suffix}.json  (API output)
{ "version": 1, "reviews": { "r3f2a91c0": {
    "status": "pending|accepted|rejected|flagged", "userLabel": "of interest", "note": "…",
    "claudeLabel": {"label":"of interest","confidence":0.8,"objectIds":[…],"frames":[…],
                    "reason":"…","by":{…},"at":"…"},
    "history": [ {"at":"…","by":{"profile":"…","via":"app"},"field":"status","from":"pending","to":"accepted"} ] } } }
```

**Routes:** `GET /api/rois?projectUid&imageUid[&suffix][&objects=1]` and `POST /api/rois/review`
(P1, built); `GET /api/rois/montage?…&roiId` (P2).

**MCP tools** (P2; each with the add-a-tool checklist — `client.py` `ALLOWED_ROUTES`, the write-set
test, `guidance.py`, `kiwi_turn.jl`, `docs/inventory/MCP.md`): `roi_list`, `roi_get` (montage +
JSON), `roi_goto` (seek box + `mark_cells`), `roi_label` (`label` ∈ the set's labels, confidence,
reason, object_ids, frames).

**Montage** (P2), from `render_view_stills` with one spec set: up to 6 timepoints over `[t0, t1]` ×
(composite, each channel, composite with contours), label ids drawn at centroids, a ±2-plane z-slab,
scale bar.

## 8. Staged implementation

| Phase | Scope | Tests |
|---|---|---|
| **P1 — built** | `spatialAnalysis.roiDiscovery` (pooled-window HDBSCAN, excess ranking, sites → `rois/{suffix}.json`, QC); `roi_utils` pure core; containment on `contactsMeshes`; `allObs` on the measure picker; `GET /api/rois`, `POST /api/rois/review` (attribution, Claude guard, per-set labels, history, undo); viewer ROI section | py goldens (`test_roi_utils.py`, `test_mesh_containment.py`); Julia dispatch, bad params, QC, review guard, CONTMESH containment; API route test; `utils/rois.ts` |
| P2 | Montage route; MCP `roi_*`; box-aware `viewer:seek`; a `figure` beside *Min objects* (`tasks/paramFigures.ts`) showing pooled groups at two densities | MCP write-set + guidance tests, montage golden |
| P3 | Blind toggle, disagreement sort, review CSV export, one-per-site view | export round-trip |
| P4 | Validation (§6) on UMq4v3 with a CD169 segmentation + containment population | the acceptance bar |

## 9. Results on UMq4v3

**Threshold design, one-variable sweep (2026-10-26)** [data] — every setting of the first P1 changed
against the threshold-6 run; % of its 25 sites still found. Figure:
`~/Downloads/TMP/roi_triage_UMq4v3_param_sweep.png`.

- The top site (one dim TBM, 13 windows) survived almost everything; below it nearly every single
  change lost 15–50% of the sites — the settings reshuffled the tail rather than tuning it.
- Inputs mattered most: no `frags` gate 40%, no contact cue 20%; nuc/mem or partners removed ~90%.
- Three numbers could wreck a run: cluster radius 3 µm → no ROIs; min objects 10 → 12%; 4 µm per
  frame → 36%.

**Generic design (2026-10-27)** [data] — `run_task(RoiDiscovery(), …)` on a scratch copy, defaults
(Min objects 20, no Rank by). No image reads: 24 s for `frags`, 5 s for `qc`.

- `Bcells/qc/frags`: 50 ROIs, 17 sites. The known TBM (x ≈ 60, y ≈ 83, z ≈ 8 µm) is ROIs 1, 2, 4, 6
  and 8–12 (one per window), boxes ≈ 20 × 20 µm, 62–116 objects against 2–11 expected. Other compact
  sites rank among them. Figure: `~/Downloads/TMP/roi_generic_UMq4v3_frags_top_sites.png`.
- `Bcells/qc` (no fragments gate): the same TBM is ROI 1.
- **Unverified:** the biology of any ROI (§6), and the containment path end to end — no CD169
  segmentation on UMq4v3 yet.

## Decisions

1. **Generic** (2026-10-27): the population is the question; the task has no cue of its own.
   Supersedes the five-cue score (contact, density, turnover, naked, partner) — those are gates,
   filter populations or *Rank by* measures now. Turnover is dropped (gating can't express it); if
   wanted, it would come back as a *Rank by* option.
2. **No thresholding** (2026-10-27): macrophages are segmented, containment is a measure on
   `contactsMeshes`. Supersedes the per-frame CD169 mask.
3. **No radius** (2026-10-27): HDBSCAN with a count; pooled windows instead of a space–time
   conversion.
4. **Rank by excess over the window's background density**, not by count.
5. **ROI ids are content hashes** of the t-window and box.
6. **Reviews are separate from definitions.**
7. **Claude cannot accept, reject, delete, or replace a person's label**, nor start discovery.
8. **Built in Feijoa, as a viewer side-rail section.**
9. **Sites group windows of one place** (box IoU ≥ 0.3, transitive).
10. **Review labels belong to the ROI set** (2026-10-27), chosen on the run.
