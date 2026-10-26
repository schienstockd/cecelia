# ROI auto-discovery + Claude triage for B-cell fragments

Status: **in progress** (2026-10-26) · branch `feat/roi-triage` · P1 built (discovery task, ROI
sidecar + review routes, viewer ROI section); P2–P4 open. Brief:
[`docs/archive/OPUS_ROI_TRIAGE_PROMPT.md`](../archive/OPUS_ROI_TRIAGE_PROMPT.md). Test case:
zolIMa / `UMq4v3` (M2b MERTK, cropped; 181 t × 4 c × 35 z × 468 × 467, 0.33 µm xy, 2 µm z, 15 s/frame).

## Goal

The user can't track the many small GFP/TOM fragments in intravital B-cell / tingible-body-macrophage
(TBM) movies. Instead of tracking them: **find the few regions where something happens to fragments**
(engulfment, blebbing, fragmentation, uptake). Rank them, let Claude label them from images plus
numbers, and have the user review only the top few. Segmentation is taken as given.

**Numbers come from the pipeline. Judgement comes from Claude.** Claude is not the tracker and not a
coordinate source. It labels candidates that were computed beforehand.

Tags used below: **[code]** = verified in code or on disk · **[data]** = measured on UMq4v3 this
session · **[inferred]** · **[unknown]**.

---

## 1. Audit — what exists, what's missing

### 1a. The test image and its object table [data]

- Image `zolIMa/0/UMq4v3/ccidImage.ome.zarr`: `(t,c,z,y,x) = (181, 4, 35, 468, 467)` uint16.
  Channels: `SHG`, `nuc-GFP`, `mem-TOM`, `CD169-Kat`. **`ccid.json` says `SizeZ: 31`, but the store
  and the labels both have 35 planes.** Read geometry from the store, never from ccid `meta`.
- Segmentation `Bcells` (`labels/Bcells.zarr`, uint32, same shape minus c) has 247,853 objects over
  181 frames. It is a coastal two-model run (mem-TOM model + nuc+mem model, `matchAs: base`). Tracks
  exist only for `/qc/frags` (`Bcells__tracks.h5ad`, 68,530 tracked rows).
- `labelProps/Bcells.h5ad` obs columns (complete list): `area extent major_axis_length
  interm_axis_length minor_axis_length surface_area volume_mesh convex_hull_area convex_hull_volume
  euler_number_mesh solidity surface_to_volume sphericity feret_diameter_max_mesh ellipticity_oblate
  ellipticity_prolate ellipticity_interm_oblate ellipticity_interm_prolate mean_intensity_0..3 pass
  track_id track_parent track_root track_state track_generation cell_id track_source live.cell.speed
  live.cell.angle label`, plus centroids `centroid_z/y/x` (pixels; `uns.spatial_cols`) and
  `centroid_t` (`uns.temporal_cols`). `mean_intensity_k` is channel k (`intensity_measure: mean`), so
  **CD169 = `mean_intensity_3`**, GFP = `_1`, TOM = `_2`. No column is literally named `CD169-Kat`.
- Gating (`gating/Bcells.json`, flow pops, polygons in **transformed** coordinates):

  | Pop | x | y | Transforms |
  |---|---|---|---|
  | `/qc` | `volume_mesh` | `surface_to_volume` | logicle / linear |
  | `/qc/cells`, `/qc/frags` | `volume_mesh` | `solidity` | logicle / logicle |
  | `/qc/frags/nuc`, `/qc/frags/mem` | `mean_intensity_2` (TOM) | `mean_intensity_1` (GFP) | logicle / logicle |
  | `/qc/frags/small`, `/qc/frags/big` | `volume_mesh` | `ellipticity_interm_oblate` | logicle / linear |

  So **`nuc` is not "green only" and `mem` is not "magenta only"**. Both are polygons in the same
  TOM×GFP plane, they sit next to each other near the origin, and they are not exclusive by
  construction. "Naked nuclear fragment" therefore means `nuc ∧ ¬mem` as a set operation. Their
  intersection can be non-empty [inferred from the vertex lists]. `big` is a child of `frags`: a big
  *fragment*, not an intact cell. Intact cells are `/qc/cells`.

### 1b. Macrophages [data, code]

- No macrophage, CD169 or TBM handling exists anywhere in the code [code]. CD169+ macrophage
  segmentation is an open, failing problem (`docs/todo/SEGMENTATION_OPEN_PROBLEM.md`).
- On UMq4v3 the CD169 channel is sparse (frame median 0, 99th pct ≈ 8, 99.9th ≈ 16). It also carries
  **B-cell bleed-through**: every B cell is faintly visible in the CD169 MIP. The per-object
  correlation of CD169 with TOM is low (r = 0.15), so a linear spillover subtraction would not remove
  it [data].
- Threshold sweep at t = 60 (Gaussian σ = 0.5/1.5/1.5 px z/y/x, then remove < 500 voxels).
  Threshold 6 outlines the bright TBMs cleanly. Threshold 4 starts outlining B-cell bleed. **Neither
  catches the dim TBMs that hold green puncta.** The bright TBMs show **holes** where engulfed
  fragments sit, because the fragment displaces CD169 [data, visual].
- Consequences:
  1. A raw-threshold mask needs **per-plane hole filling**, or every engulfed fragment reads as
     "outside" the mask.
  2. The threshold is a user knob, not a constant.
  3. A **mask-free cue** is needed for the dim TBMs: local fragment density. TBMs collect fragments,
     so a dense knot of small GFP objects is itself the TBM signature.
- Per-object `mean_intensity_3` of fragments is near 0 even next to bright TBMs. A 2-px shell mean
  around fragments is ≈ 0–1.5 [data]. Per-object CD169 alone is therefore a weak contact cue.

### 1c. Existing pieces to reuse [code]

| Need | Exists | Where |
|---|---|---|
| Pop membership → labels | `pop_df_multi` / `pop_df` | `app/src/gating/popmanager/{mixed_resolution,allow_list}.jl` |
| Nearest-neighbour contacts | `spatialAnalysis.cellContacts` (centroid KDTree, per frame) | `app/src/tasks/spatialAnalysis/cellContacts.jl` (#1497, #1734) |
| Surface contacts | `spatialAnalysis.contactsMeshes` | `contactsMeshes.jl` + `cell_contacts_mesh_run.py` |
| Per-frame DBSCAN | `spatialAnalysis.detectAggregates` (x,y,z only, never t) | `detectAggregates.jl` (#271, #1649) |
| Thresholding helpers | `intensity_utils.background_threshold`, Otsu/triangle | `python/cecelia/utils/intensity_utils.py` |
| 3D CC + small-object removal | `ridges_run._label_volume_3d` | `app/src/tasks/segment/ridges_run.py` (#933) |
| Sidecar path convention | `img_stats_path` / `img_stats_suffixes` (listed from disk) | `app/src/model/image.jl` |
| Atomic JSON | `write_json_atomic` (Julia + Python) | `app/src/utils.jl`, `python/cecelia/utils/atomic_io.py` |
| Server-side stills | `render_view_stills` (viewer shader, crop, z-slab MIP, label contours via `MovieMask`, scale bar, timestamp) | `api/src/movie_render.jl` (#1393) |
| Still → base64 PNG → MCP `Image` | `api_gating_cells_image` + `gate_cells_view` | `api/src/gating_views_api.jl` (#1407), `mcp/cecelia_mcp/server.py` |
| Fixed per-image contrast | viewer props JSON `data/<zarr>.json` → `_render_default_specs` | `api/src/movie_helpers.jl`, `viewer_api.jl` |
| Viewer seek (t, z) | `POST /api/viewer/seek` → WS `viewer:seek` | `api/src/viewer_nav_api.jl` (#1192) |
| Viewer box framing | `buildFocusViewState` (pan + zoom-out-to-fit) | `frontend/src/utils/viewer/focusOnCell.ts` |
| Object highlight | `setPickHighlight` / MCP `mark_cells` (5-min TTL) | `stores/viewer.ts`, `api/src/marks_api.jl` (#1042) |
| Prev/next pager | `nextIndex/prevIndex/clampIndex/pageSummary` | `frontend/src/utils/reviewPager.ts` |
| List primitive | `SelectionTable` (sort, compact density, row click) | `frontend/src/components/SelectionTable.vue` |
| Attribution | `author_stamp()` → `{profile, via: app\|claude}` from the `X-Cecelia-Client` header | `app/src/config/profile_settings.jl` (#1355) |
| "Claude proposes, person wins" | Blackboard section verdicts (409 when a person's verdict exists) | `api/src/blackboard_run_review.jl` (#1431) |
| Viewer side-rail sections | `CollapsibleSection` accordion `cc.vw.section` | `frontend/src/modules/ViewerWindow.vue` |

### 1d. Missing [code]

- A mask from an intensity channel, distance to that mask, and containment.
- A parent / container link across populations.
- Spatio-temporal (x,y,z,t) clustering. Hotspot ranking.
- A montage compositor: rows = t, columns = channels, with label-ID text. No orthogonal xz/yz views.
- `seek_viewer` takes t/z only, no box.
- No ROI concept, sidecar, panel or MCP tool.

---

## 2. Realism check

| Part | Verdict | Why |
|---|---|---|
| Per-frame macrophage mask (threshold + smooth + per-plane fill + min size) | **feasible now** | ~1.5 s/frame for 35×468×467 [data] → ~4 min for 181 frames. Quality is the risk, not cost: dim TBMs are missed (§1b) |
| Distance to mask (3D EDT, anisotropic `sampling`) | **feasible** | `scipy.ndimage.distance_transform_edt` on the complement, one per frame, ~0.5 s [inferred] |
| Fraction of object voxels inside mask | **feasible, cheap** | `bincount(labels[mask])` / `bincount(labels)` per frame |
| Surface-fraction-in-contact (meshes) | **over-engineered for P1** | the fraction of an object's voxels inside the filled mask is a good enough stand-in; mesh contact already exists as a task |
| Macrophage *objects* / tracking | **not now** | open segmentation problem; the brief's design rightly avoids it |
| Local density + change across t±1 | **feasible, cheap** | cKDTree per frame on centroids |
| 4D clustering | **feasible** | DBSCAN on (x,y,z µm, t·`timeScale` µm/frame) over the top-scoring objects only (thousands, not 250 k) |
| Birth/contact/loss/turnover typing without tracking | **risky** | Without identity, birth vs loss is only "density went up/down next to X". P1 labels the ROI by its **dominant cue** and makes no biological claim |
| Montage rendering | **feasible, P2** | `render_view_stills` already does crop + z-slab MIP + contours + scale bar + fixed contrast. Only label-ID text and the grid layout are new. Orthogonal views need a rotated camera (untested) → P3 |
| 3D handling for Claude | **risky** | a z-MIP cannot tell "inside" from "overlapping" (brief, correct). The montage needs a thin z-slab (±2 planes at the ROI centroid) and the containment number from the pipeline. Orthogonal view deferred |
| Claude labelling quality | **unknown** | measured by the validation protocol (§6) before any scale-up |

## 3. Prior art (all verified this session or by the review sub-agent via Crossref/PubMed/arXiv)

- **TBM biology.**
  - Grootveld *et al.*, *Cell* 186:1144 (2023), doi:10.1016/j.cell.2023.02.004. TBMs come from a
    CD169-lineage precursor, are sessile, and capture dead-cell fragments with processes ("lazy
    search").
  - Gurwicz *et al.*, *J Exp Med* 220:e20222173 (2023), doi:10.1084/jem.20222173. Uptake happens at
    dynamic protrusions.
  - Baudon & Bajénoff, *JEM* (2023), doi:10.1084/jem.20230250. Commentary.
  - Mayer *et al.*, *Science* 358:eaao2602 (2017), doi:10.1126/science.aao2602. Up to half of GC B
    cells die every 6 h, which sets the expected event rate.
  - Rahman *et al.*, *J Immunol* (2010), doi:10.4049/jimmunol.1001187. Mer-deficient TBMs clear poorly
    (relevant to MERTK).
  - **Design consequence:** the event signature is a fragment *at or inside* a CD169 process, not
    cell–cell contact. Containment beats centroid distance.
- **Automated event detection.**
  - Held *et al.*, CellCognition, *Nat Methods* 7:747 (2010), doi:10.1038/nmeth.1486. A per-frame
    classifier plus temporal smoothing (HMM); the smoothing was essential at state transitions.
  - Wu *et al.*, *Bioinformatics* 39:btad584 (2023), doi:10.1093/bioinformatics/btad584. CNN
    detection of apoptotic bodies, 2D nanowells.
  - Ounissi *et al.*, PhagoStat, *Sci Rep* 14:6482 (2024), doi:10.1038/s41598-024-56081-7.
    Phagocytosis quantification pipeline with a data-quality module.
  - None of these is 3D intravital.
- **VLM triage.**
  - μ-Bench, arXiv:2407.01791. Models struggle even on modality recognition.
  - MicroVQA, arXiv:2503.13399. Best ≈ 53%; perception errors dominate.
  - Wu *et al.*, arXiv:2310.09909. GPT-4V localisation weak.
  - "VLMs are blind", arXiv:2407.06581. Counting and overlapping shapes fail; accuracy recovers when
    objects are spaced apart.
  - Kamath *et al.*, arXiv:2310.19785. Spatial relations ≈ 56%.
  - Pang *et al.*, arXiv:2605.30170. Counting bottleneck.
  - **Design consequence:** never ask Claude to count or localise. Give it the outlined candidates,
    the numbers, and a category question. Treat it as a filter whose error rate is measured.

## 4. Error modes the triage loop must catch (excluding segmentation)

| Error | Detection | Agent action |
|---|---|---|
| **Spillover-driven false macrophage**: B-cell bleed in CD169 passes the threshold | mask component overlaps a `/qc/cells` object by > 50% of its voxels; or the ROI's CD169 lies only on B-cell voxels | flag `artifact` candidate; say "CD169 here is bleed-through" and cite the per-object CD169 vs component mean |
| **Missed dim TBM** (mask silent, fragments dense) | density cue high, mask cue 0 | ROI still surfaces via density; Claude is told "no mask here; judge from the yellow channel" |
| **Projection ambiguity** (MIP overlap ≠ inside) | pipeline `fracInside` vs Claude's "engulfed" | Claude must cite `fracInside`; "engulfed" with `fracInside < 0.3` → downgrade to `contact` / `unsure` and say so |
| **Contrast drift between frames** | fixed per-image specs (one spec set for every row) — never per-frame autoscale | the montage builder refuses per-row specs; JSON carries the specs used |
| **Hallucinated counts** | Claude's `object_ids` ⊄ ROI's object table, or a number in `reason` that isn't in the JSON | reject the label, ask once more with the id list; second failure → `unsure` |
| **Hallucinated frames** | `frames` outside `[t0, t1]` | clamp + flag |
| **Stale ROI after regeneration** | review's `roiId` absent from the current ROI set | review kept, shown as *orphaned*; never deleted |
| **Wrong geometry source** (`SizeZ` 31 vs store 35, §1a) | runner reads shape from the store; the sidecar records it | tested |
| **Over-confidence** | confidence ≥ 0.8 on a type the user rejects in ≥ 30% of validation cases | per-type calibration table from §6; Claude's label shown with that bin's agreement |
| **Duplicate ROIs** (one place, several time windows — a sessile TBM scores in every window) | box IoU ≥ 0.3, any time → same `site` (`roi_utils.assign_sites`) | the list shows the site; Claude triages one window per site first. Measured on UMq4v3: ranks 1–6 were one dim TBM; 50 ROIs → 25 sites |

## 5. Co-driving model

- **The ROI list is the shared surface.** The user and Claude read and write the same sidecar through
  the same routes. Claude never scrapes the screen.
- **Claude proposes, the user disposes.**
  - A `via: claude` write may set `claudeLabel` and `flagged`, and add a note.
  - It may **not** set `accepted` / `rejected`, may **not** overwrite a person's label (409, the
    Blackboard section-verdict rule), and may **not** delete anything.
  - Every change appends `{at, by, field, from, to}` to the ROI's history. Undo replays it
    backwards (reversible).
- **Navigation.**
  - Claude: `roi_goto` → box-aware seek + `mark_cells` on the ROI's objects (5-min TTL, so the marks
    go away by themselves).
  - User: the panel's prev/next and row click do the same locally.
- **Knobs.**
  - Scoring weights and the mask threshold are task params. "Regenerate" is a task run, so it is
    reproducible and recorded in the run log.
  - Claude can suggest a knob change in prose; it cannot start the task (MCP is read+additive). The
    one exception is the separate `cecelia-autonomous` server.
  - **The brief's `roi_generate` MCP tool conflicts with that invariant, so it is not built.**
- **Not autonomous.** Claude works one ROI or a selection at the user's request. There is no
  overnight loop.

## 6. Validation protocol

1. **Labels.** The user hand-labels ~30 ROIs from UMq4v3 in the panel (`userLabel`), stratified: top
   10, 10 from ranks 11–40, 10 random from the rest. No Claude labels are visible while labelling.
   The panel hides `claudeLabel` when the "blind" toggle is on (P3).
2. **Triage.** Claude runs `roi_get` + `roi_label` on the same 30 with a fixed prompt and model id
   recorded in the label.
3. **Report.** Cohen's κ and raw agreement per type and per confidence bin
   (`<0.5 / 0.5–0.8 / ≥0.8`), plus the confusion matrix. Exported as CSV from the sidecar (P3
   export).
4. **Acceptance bar** (set now, before seeing results):
   - `engulfment` precision ≥ 0.8 at confidence ≥ 0.8;
   - recall of user-`engulfment` among Claude-`engulfment|unsure` ≥ 0.9 (Claude must not *drop*
     events);
   - overall κ ≥ 0.6.
   - Below the bar, Claude stays an ordering aid (sorts the queue), not a labeller.
5. **Discovery check.** Fraction of user-labelled `engulfment` ROIs ranked in the top 20. Below 0.5
   means retune the weights before any Claude work.

## 7. GUI + MCP design

**Where it's built.** Feijoa (this Julia + Vue app). The brief's "pineapple" is the R/Shiny → Feijoa
rewrite. The R/Shiny app is the legacy reference only (`old-R-shiny-version/`), so nothing is built
there.

**Panel.** A **"ROIs" section in the viewer window's side rail** (`ViewerWindow.vue` accordion, key
`rois`), not a separate module page, because every action ends in moving the viewer. Component:
`frontend/src/components/RoiSection.vue`, logic in `frontend/src/utils/rois.ts`.

- **Header:** ROI set picker (suffix), counter `pageSummary`, prev/next (`reviewPager`), and a status
  filter (`ChipSelect`).
- **Table** (`SelectionTable density=compact`): rank · type · score · t-window · status · Claude
  label (+ confidence; reason in the tooltip) · note. Sortable by rank, score, status and t.
- **Row actions:** accept · reject · flag · note. The active row is focused: seek t, z and the box
  via `buildFocusViewState`, and highlight the objects via `setPickHighlight`.
- **Thumbnail column:** P2, from the montage route.

**State model.**
- **Definitions** in `{img}/rois/{suffix}.json`. The task writes them atomically and they are
  immutable between runs.
- **Reviews** in `{img}/roiReviews/{suffix}.json`. The API writes them under a lock; they are keyed
  by ROI id, which is a hash of the t-window and the box (`roi_id(t0, t1, box)`), so re-running with
  the same params yields the same ids. Reviews of ids that vanished are kept as orphaned.

```jsonc
// rois/{suffix}.json  (task output)
{ "version": 1, "suffix": "default", "valueName": "Bcells", "generatedAt": "…",
  "params": { …every task param… },
  "geometry": { "shape": {"t":181,"z":35,"y":468,"x":467}, "umPerPx": {"x":0.33,"y":0.33,"z":2},
                "frameIntervalS": 15 },
  "rois": [ { "id": "r3f2a91c0", "rank": 1, "score": 12.4, "type": "contact",
              "site": 1, "t0": 52, "t1": 61, "box": {"x0":180,"x1":231,"y0":214,"y1":276,"z0":15,"z1":27},
              "centroid": {"t":56.5,"z":21,"y":245,"x":205}, "nObjects": 37,
              "cues": {"contact":0.71,"density":0.64,"turnover":0.22,"naked":0.41,"partner":0.10},
              "objects": [ {"label":145529,"t":56,"x":204.3,"y":247.5,"z":21.4,"score":0.83,
                            "pop":"nuc","volume_mesh":…, "solidity":…, "ellipticity_interm_oblate":…,
                            "gfp":…, "tom":…, "cd169":…, "fracInside":0.92, "distMaskUm":0,
                            "density":11, "partnerDistUm":3.1} ] } ] }
// roiReviews/{suffix}.json  (API output; user work — kept by an analysis reset, like gating/)
{ "version": 1, "reviews": { "r3f2a91c0": {
    "status": "pending|claude-labelled|accepted|rejected|flagged",
    "userLabel": "engulfment", "note": "…",
    "claudeLabel": {"type":"engulfment","confidence":0.8,"objectIds":[…],"frames":[…],
                    "reason":"…","model":"…","by":{…},"at":"…"},
    "history": [ {"at":"…","by":{"profile":"…","via":"app"},"field":"status","from":"pending","to":"accepted"} ] } } }
```

**Routes** (P1 = built):

| Route | Purpose |
|---|---|
| `GET /api/rois?projectUid&imageUid[&suffix]` | **P1.** Definitions merged with reviews; `suffixes` listed from disk |
| `POST /api/rois/review` `{projectUid, imageUid, suffix, roiId, status?, userLabel?, note?, claudeLabel?, undo?}` | **P1.** Attributed, guarded, history-appending |
| `GET /api/rois/montage?…&roiId` | P2. `{png, meta}` montage for one ROI |

**MCP tools** (P2). Each one gets the full add-a-tool checklist: `client.py` `ALLOWED_ROUTES`,
`test_writes_are_only_the_recoverable_routes`, `guidance.py`, `kiwi_turn.jl` classification, and
`docs/inventory/MCP.md`.

| Tool | Args | Returns |
|---|---|---|
| `roi_list` | `project_uid, image_uid, suffix?, status?` | ranked rows (no object tables) |
| `roi_get` | `project_uid, image_uid, roi_id, suffix?` | `[Image(montage), json]` — the ROI package of the brief |
| `roi_goto` | `project_uid, image_uid, roi_id, suffix?` | seek box + `mark_cells(objects)` |
| `roi_label` | `project_uid, image_uid, roi_id, type ∈ {engulfment, free_debris, bleb_fragmentation, dividing, artifact, unsure}, confidence, reason, object_ids, frames, suffix?` | the stored `claudeLabel`; 400 when ids/frames fall outside the ROI (§4), 409 if a person decided |

`unsure` is added to the brief's type list. Without it a forced choice turns uncertainty into a
confident wrong label.

**Montage** (P2), from `render_view_stills` with one shared spec set:
- Rows: up to 6 timepoints evenly spread over `[t0, t1]`, with the time printed.
- Columns: composite · GFP · TOM · CD169 · composite with class-coded contours. Label ids are drawn
  in PIL at the centroids, with `title_card._font` reused.
- Z: a z-slab of ±2 planes around the ROI centroid, not a full MIP.
- Scale: nearest-neighbour upscale to ~300 px per cell, scale bar on the first cell.

## 8. Staged implementation

| Phase | Scope | Tests / checks |
|---|---|---|
| **P1 — built** | Task `spatialAnalysis.roiDiscovery` (mask + cues + score + 4D DBSCAN → `rois/{suffix}.json`, QC); `python/cecelia/utils/roi_utils.py` pure core; routes `GET /api/rois`, `POST /api/rois/review` (attribution, Claude guard, history, undo); viewer "ROIs" section (list, prev/next, seek box, highlight objects, accept/reject/flag/note) | py: cue/score/cluster/id goldens; Julia: dispatch + bad param + QC finding + review guard; frontend: `utils/rois.ts`; recital |
| P2 | Montage route + label-ID text; MCP `roi_list/get/goto/label`; box-aware `viewer:seek`; Claude label column | MCP write-set test, guidance test, montage golden size/shape |
| P3 | Knobs in the panel (re-run with edited weights through the task launcher), blind toggle, disagreement sort, CSV/JSON export of reviews, duplicate-ROI merge, xz/yz view | export round-trip |
| P4 | Validation run (§6) on UMq4v3, report in the Blackboard; decide scale-up | the acceptance bar |

## P1 result on UMq4v3 (2026-10-26)

Run through the real task (`run_task(RoiDiscovery(), …)` on a scratch copy of the image's metadata),
candidates `Bcells/qc/frags`, partners `…/big`, nuc/mem `…/nuc` / `…/mem`, mask `CD169-Kat` at 6:

- 53,555 candidates, 28,216 partners → 2,586 high scorers → 50 ROIs, 25 sites, 3 min 12 s.
- Mean mask coverage was 1.4% of voxels, and only 0.1% of fragments are > 50% inside the mask: the
  segmented fragments mostly sit beside, not inside, the thresholded CD169.
- Ranks 1–6 are one site (x 150–220, y 216–276, z 0–10): a knot of GFP fragments that is visible in
  every window. Contact = 0, density ≈ 0.97, so it surfaced on the mask-free cue (Decision 2).
- Lower ranks, which I looked at on the earlier proxy run, include bright CD169 macrophages with GFP
  content and a free GFP fragment beside a CD169 process. Plausible candidates, not yet judged by the
  person.
- Figures: `~/Downloads/TMP/roi_triage_UMq4v3_*.png`.
- **Unverified:** biology of any ROI. That is §6.

## Decisions (2026-10-26)

1. **No macrophage objects.** A per-frame CD169 mask (threshold knob + per-plane hole fill + min
   size) is the only macrophage representation.
2. **Density is a first-class cue next to the mask**, because the mask misses dim TBMs (§1b).
3. **Cues are rank-normalised per image** (percentile 0–1) before weighting, so the weights compare
   like with like and survive intensity differences between movies.
4. **ROI type = dominant weighted cue** in P1. It carries no biological claim, so the panel shows it
   as "cue". Biological types come from Claude or the user.
5. **ROI ids are content hashes** of the t-window and box, so reviews survive an identical re-run.
6. **Reviews are separate from definitions.** A re-run never touches a review, and a review never
   rewrites the task output.
7. **Claude cannot accept, reject, delete, or replace a person's label.** It cannot start discovery
   either.
8. **Built in Feijoa, as a viewer side-rail section.**
9. **Sites group windows of one place** (box IoU ≥ 0.3, transitive), so a sessile event is seen once,
   not six times.
