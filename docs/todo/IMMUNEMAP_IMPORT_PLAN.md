# Immunemap import — plan

> **Status: images built (2026-11-08), branch `feat/immunemap-import`; curated tracks not built.**
> Read *Built 2026-11-08* first, then *Revision 2026-11-08*. The driving use changed: Immunemap is
> a source of public intravital datasets **with several treatment conditions**, to show Cecelia's
> analysis across conditions. The curated-track benchmark is a nice-to-have, not the point. That
> makes it Option B, in core rather than a plugin (the maintainer, 2026-11-08). D1, D3 and D6 are
> superseded for images; they still describe the track side, which isn't built.

## Built 2026-11-08 — Import from Immunemap (images)

Same shape as legacy migration: preview → register placeholders → a per-image task does the work.

| Piece | Where |
|---|---|
| Client: experiment manifest, channel labels, resumable verified download | `python/cecelia/utils/immunemap.py` (tests `test_immunemap.py`, real fixtures in `test-data/immunemap/`) |
| Imaris voxel size, to cross-check Immunemap's | `ims_meta.voxel_size` |
| `POST /api/import/scan-immunemap`, `POST /api/import/register-immunemap` | `api/src/routes/immunemap.jl` |
| `importImages.immunemap` = composite of `importImages.immunemapFetch` (download into the image folder, set `ori_path`) → `importImages.omezarr` | `app/src/tasks/importImages/immunemap*.{jl,json}` |
| Dialog: paste the experiment link, scan, tick videos, add | `ImmunemapImportDialog.vue` on Manage images; logic in `utils/immunemapImport.ts` |

Decisions — B1, B3 and the one-set half of B2 are the maintainer's (2026-11-08); the rest were
made while building and are open to his review:

- **B1. Core, not a plugin** (maintainer). Public data needs no credentials, so the reason for a plugin is gone.
  The cost: core depends on `api.immunemap.org` staying up for this one feature.
- **B2. One experiment = one new set** (maintainer); **the condition is the image's `condition`
  attribute** (proposed). The raw
  Immunemap group name, unparsed. Compare by attribute groups on it. (Answers Q7.)
- **B3. The download is deleted after a successful import** (maintainer). The composite deletes step 1's file
  once `omezarr` succeeds. A failed run keeps the file or its `.partial`, and the next run resumes.
  `ori_path` then points at a deleted file, which a metadata refresh already tolerates.
- **B4. Frame interval from Immunemap when the file has none** (proposed). The first import read
  "no timing in the file": wrong — `TimeInfo` holds a stamp per frame, but Imaris writes nine
  fractional digits and `ims_meta` parsed six, so the series was dropped. Fixed (and the nominal
  `LSM Dataset Size/Time Interval [s]` added as a tier), so the file's own measured interval is used
  now. The declared tier stays as the backstop for a file that really has none. Registration stores `meta.declaredTimeIncrement` (the timestamps' median gap, else
  1/fps), and `omezarr` applies it **last**, after the store and the file, recording the source as
  `TimeIncrementDeclared`. Any source can use the tier; only Immunemap does today.
- **B5. The file's voxel size wins** (proposed); a disagreement with Immunemap's `spacing` is a QC warning
  (`import.immunemap_spacing`). On experiment 63 they agree everywhere, including video 309's
  1.8 µm, which is a 10× overview (the header names a 10x/0.3 objective), not an error.

Provenance per image lives in `meta.immunemap`: acquisition and video ids, experiment and group,
DOIs, checksums, Immunemap's calibration, and when it was retrieved (with `md5Verified`).

**Acquisition settings from Imaris files**: `ims_meta.acquisition_settings` reads what Imaris
copies from the source microscope (system, objective + NA, per-channel excitation, emission filter
and pinhole, planned z-stack and time-lapse). It is not Immunemap-specific: every `.ims` import gets it.
The Zeiss `Subblock*` laser and detector entries are left unread — their active flags are unverified.

**Group attributes (Q9, built):** the dialog shows a group × attribute table; `time` is prefilled from
the group names (`timeFromGroupName`, `utils/immunemapImport.ts`), other columns are added and typed by
the user, and each becomes an image attribute beside the raw `condition`
(`immunemap_image_attrs`, `api/src/routes/immunemap.jl`).

**Not built:** curated tracks (Q6, Q8 below), and a guide (the feature is listed in
`lib/guides/unguided.ts`).

## Goal

Import cell tracks (and optionally imagery) from **Immunemap**
([immunemap.org](https://www.immunemap.org/) — EMBO J 2025 open intravital
microscopy atlas, 20 labs, ~400 videos, 58k tracks) into Cecelia.

**Two driving use cases** — the choice between them decides the shape of
the plugin:

1. **One-off**: user grabs one Immunemap video's tracks as a validation
   baseline against Cecelia's own tracking or as input to the motif
   discovery work. File-download workflow, no auth needed at import time.
2. **Cohort/systematic**: user pulls many Immunemap videos routinely for
   cross-lab comparison. URL/auth-driven, whole-workflow "one button."

## Verdict — read first

**Start with Option A (add an Immunemap template to the existing
`ccia-importTracks` plugin). Promote to Option B (dedicated plugin) only
when a specific cohort/API-driven ask lands, not on speculation.**

Two independent reasons:

- The existing plugin already covers 90% of what Option B would need
  — spot-matching, the `create` mode's centroids-only "points
  segmentation" (the data-model concern flagged earlier is already
  solved), the column-mapping architecture, the `attach`-to-Cecelia-labels
  flow. The delta for Immunemap is a template file + a small JSON reader
  branch (~40 LOC).
- Option B introduces two new capabilities the plugin ecosystem does not
  have today (URL fetch + JWT storage). Those are legitimate plugin-wide
  design questions — settings surface, credential storage location,
  refresh handling — and inventing them as a side-effect of one atlas
  integration is exactly the "one way to do each thing" trap
  `CLAUDE.md` warns about. Answer them once, deliberately, when a second
  API-driven plugin appears.

## Revision 2026-11-08 — premises that moved

Triggered by a full-importer brief (`docs/archive/IMMUNEMAP_IMPORT_PROMPT.md`), which adds a third
use case: **curated tracks as ground truth for benchmarking Cecelia's tracking**. Re-probed
`api.immunemap.org` unauthenticated, using acquisition 309, video 409 (uid `2JBABTBNRF8MR978J016GM606A`)
and experiment 101:

- **Public data needs no JWT.** `/acquisition/{id}`, `/experiment/{id}`, `/video/{id}`,
  `/video/{id}/tracks` and `/video/{uid}/ims` all return 200 with no token. The IMS download is
  `application/octet-stream`, and its `Content-Length` equals `original_file.size`, so it is the
  original file and can be checked against `original_file.md5`/`sha1`. Visibility is
  `access.public` on each acquisition. This removes D6's blocker for public data, so Option B no
  longer needs credential storage.
- **Track frames are 0-based** (`t` 0–59 for 60 frames). The old line `frame_base = 1` was wrong.
- **Curated tracks are 2D on a 3D video.** Video 409 has 9 slices, but every point has `z = 1`.
  x/y are sub-pixel pixel coordinates. A benchmark against Cecelia's 3D tracks must compare in xy
  only. Whether that is true of every video is untested.
- **Track fields:** `id`, `label` (the source CSV name), `color`, `author`, `video`, `date`,
  `points`, `cellTypeName` (e.g. "B cells") and `cellTypeObject`.
- **Experiment and group IDs are different ID spaces.** Acquisition 309 has
  `experimental_group: 101`, but `/experiment/101` is a neutrophil study whose only group is 155
  (acquisitions 526–528). To enumerate an experiment, read `/experiment/{id}` →
  `experimental_groups[].acquisition[]` and fetch each one. Groups have no endpoint of their own
  (404), and auto-created groups are called "Default Group".
- **Metadata the old plan didn't have** (from `/acquisition/{id}`):
  - Channel identity comes from `cell_stainings[].channels[].selected` with `class` and
    `fluorophore`. `channel.name` is null, and `channel_configuration` lists 6 virtual channels
    for 3 real ones.
  - Each acquisition has a DOI (`links[].doi`), `tags`, `notes` and `area`.
  - `time_steps` gives ISO timestamps to cross-check against `1/fps`.
  - `upload_status` is a number (4 seen); its meaning is unknown.
- **Recall only.** The curated tracks are a hand-picked subset of cells, so recall against them
  means something and precision does not.
- **There is no arrest coefficient to benchmark against yet.** Today arrest exists only as a
  fraction of time in a state: the HMM `arrested`-state frequency through `track_props`
  (`{m}.{cat}`). The designed route is `MOTILITY_METRICS_PLAN.md` D1/Q2, which is time in a gated
  slow population with dwell bouts, P3b. It isn't built. The plan answered Q3 with no smoothed
  arrest column.
- **D7 caveat:** in `attach` mode the import writes `track_id`, the column
  `tracking.bayesian_tracking` writes, so it replaces Cecelia's tracks on that segmentation. Ground
  truth that must stay separate goes in through `create`.

**Reopened:** D3 (manual two-step flow) and D6 (JWT out of scope). Their reasons assumed a login,
and public data needs none. Whether to promote to Option B is open question 5.

## Locked decisions

Numbered so future code and other docs can cite them
(`# see IMMUNEMAP_IMPORT_PLAN Decision N`).

1. **Track import lives in `ccia-importTracks`, not a new plugin, at
   MVP.** New template `templates/immunemap.json` + JSON reader branch in
   `python/track_readers.py`. Do not spawn a second plugin for a format
   the existing one is one file away from supporting.
2. **Movie import stays with `importImages.omezarr`, not the tracks
   plugin.** Bio-Formats already reads Imaris IMS natively (Immunemap's
   `/video/{uid}/ims` payload is IMS/HDF5), so the existing pipeline
   handles it once the file is on disk. No new task category.
3. **Two-step user flow is acceptable at MVP.** User (a) exports the
   tracks JSON from Immunemap manually (browser save-as from
   `api.immunemap.org/video/{id}/tracks`, or curl+JWT), (b) if they want
   imagery, downloads the `.ims` and runs `importImages.omezarr` on it,
   (c) runs `tracking.importCsvTracks` with source = Immunemap. One-button
   automation is Option B territory, gated by real cohort demand.
4. **The reader must load Immunemap's sibling metadata endpoint OR the
   user must type the scale.** Tracks are pixels + frames; scale
   (`size.fps`, `size.spacing`) lives on the sibling `/video/{id}.json`
   endpoint. MVP: user enters fps + µm/px on the form (matches the
   existing `spotUnit = pixel` pattern). Better: the JSON reader also
   parses a sibling `-meta.json` file if present. Best (Option B): the
   plugin fetches both endpoints itself.
5. **Existing plugin's "not verified" warning applies to the Immunemap
   template too.** README states the four shipped templates are inferred
   from documented output, not confirmed against a real export. A fifth
   built the same way inherits that. **Ratchet: verify at least one
   template against a real file when the Immunemap template lands**
   (Immunemap is publicly downloadable) — first ground-truth validation
   for the whole plugin.
6. **JWT storage is out of scope for MVP.** No credential-storage surface
   exists in any Cecelia plugin today. Option B's URL fetch would need
   one, and that is a plugin-ecosystem decision (per-plugin
   `~/.cecelia/plugins/<name>/credentials.json`? Settings-managed?
   OS keychain?), not an Immunemap decision. Defer.
7. **Attach vs create mode remains the user's call.** The plugin's two
   modes cover both:
   - `attach` — spatial-match Immunemap spots to existing Cecelia
     segmentation labels within `maxDistance` (px). For a user who
     imported the movie via `importImages.omezarr` and segmented it in
     Cecelia.
   - `create` — make a points segmentation from the Immunemap spots
     themselves ("motility only: no shape or intensity"). For a user who
     only wants the tracks and doesn't care about morphology-derived
     measures.
   No new mode needed.

## Options considered

Kept for the record — Option A is the choice under Decision 1; Option B
is what the choice would look like if the workflow evolves.

### Option A — Template in `ccia-importTracks` (MVP)

- **Files added, in the plugin repo** (`github.com/schienstockd/ccia-importTracks`):
  - `templates/immunemap.json` — column mapping for the flattened Immunemap
    tracks JSON (`points[]` → per-spot rows with `id, t, x, y, z`,
    frame_base = 0 — corrected 2026-11-08, spotUnit = pixel).
  - `python/track_readers.py` — new `read_immunemap_json(path)` branch,
    called when file extension is `.json` and template = `immunemap`.
    Flattens `[{id, points: [[t,x,y,z], …]}]` to a per-spot dataframe.
    ~40 LOC.
- **No changes to Cecelia proper.**
- **No new dependencies** (plugin's Python env already has `json` via
  stdlib).
- **Validation**: download one public tracks JSON from Immunemap, run
  through the plugin, `create` mode → 1 track per video, view in Cecelia,
  verify per-frame spot counts match the source.

### Option B — New dedicated plugin `ccia-immunemap`

Only if Option A becomes a workflow bottleneck.

- **Files added**, new plugin repo:
  - `plugin.json`, `python/immunemap_client.py` (URL + JWT), `tracking/*`
    calling `from track_readers import match_spots_to_cells` in the
    sibling plugin (plugins share PYTHONPATH — no copy).
  - Optional `importImages/immunemap.jl` that fetches the `.ims` and
    delegates to the existing bioformats2raw path.
- **New plugin-ecosystem question**: credential storage (Decision 6).
- **~200–300 LOC + a live Immunemap account for test coverage.**

## Immunemap format reference

Extracted from celltrackR's importer + live probe of `api.immunemap.org`
2026-09-17. Kept in the plan because there is no public OpenAPI spec —
consumers rely on celltrackR's reader.

### Track endpoint — `GET /video/{uid}/tracks` (JWT)

Array of track objects:

```json
[
  {
    "id": "42",
    "points": [[t, x, y, z], [t, x, y, z], ...],
    "cellTypeName": "CD8 T cell",
    "cellTypeObject": { ... },
    "date": {"date": "...", "timezone": "..."},
    "color": "#RRGGBB"
  }
]
```

- `points` is fixed length-4 arrays: `[t, x, y, z]`.
- **z = 1 sentinel for 2D videos** — celltrackR's `simplify.2D` drops it
  automatically.
- **Units on disk = pixels + frames.** Physical units come from the
  video-level metadata endpoint.
- Any extra top-level per-track key becomes per-track metadata.

### Video metadata endpoint — `GET /video/{uid}` (JWT)

- `size.fps` — frames per second (celltrackR: `scale.t = 1/fps`).
- `size.spacing` — µm per pixel (celltrackR: `scale.pos`, applied to
  x/y/z alike).
- Additional metadata (microscope, mouse model, staining, channels,
  tissue, condition) — not authoritatively schema'd in the celltrackR
  reader; not required for basic import.

### Route table — probed from `GET /video/`

```
/<uid>/<channel>/<frame>/<slice>   jpeg   per-slice image
/<uid|id>/thumbnail                jpeg   preview
/<uid|id>                          json   video info
/<uid>/ims                         ims/hdf5   raw video file (Bitplane Imaris)
/<uid>/frame/<frame>               json   per-frame info
/<uid>/channel/<channel>           json   per-channel info
```

Plus `/acquisition/{id}`, `/experiment/{id}`, `/search?q=…`, `/user`,
`/register`. ~~Everything except `/video/` and `/search` requires JWT.~~
**Corrected 2026-11-08:** public data is served without a JWT (see *Revision*). The 2026-09-17
probe either predates that or tested non-public items.

### What Cecelia already handles

- **IMS/HDF5 imagery**: `importImages.omezarr` uses `bioformats2raw`,
  which reads Imaris IMS natively. No new code needed for the movie side.
- **Centroids-only tracks**: `ccia-importTracks` `create` mode already
  makes a "points segmentation" with the motility-only caveat. The
  earlier concern about needing to invent a data-model kind for
  centroids-only is void — the plugin already ships this.
- **Column-mapping**: `spotUnit = pixel` + `frameBase = 0` (corrected 2026-11-08) + per-column
  select boxes on the form cover Immunemap's default coordinate/frame
  convention without a code change.

## Files touched (once unblocked)

**Option A (MVP)** — in the plugin repo, not Cecelia:
- `ccia-importTracks/templates/immunemap.json` (new).
- `ccia-importTracks/python/track_readers.py` (+ JSON branch, ~40 LOC).
- `ccia-importTracks/README.md` (add Immunemap to the source list; note
  the fps/spacing entry).

**Option B** — new plugin, not built until an ask lands. Files enumerated
at that point.

## Open questions (defer to the revisit)

1. **Verify Decision 5** by running the shipped ImageJ/TrackMate/Imaris
   templates against a real export before adding a fifth in the same
   mode. Cheap; unblocks the whole plugin's ground-truth story.
2. **Motif work's validation-baseline need** — is a public Immunemap
   video actually the right baseline for `MOTIF_DISCOVERY_PLAN` P3, or is
   an in-house dataset the cleaner comparator? Answer decides whether
   this plan is prerequisite for motif work or independent.
3. **Sibling metadata handling in the JSON reader** (Decision 4). Simplest
   path: user types fps + µm/px on the form. Better: parse a co-located
   `<file>-meta.json`. Doesn't matter for MVP; pick when we ship.
4. **Credential storage for Option B** (Decision 6) is a plugin-ecosystem
   design question, not an Immunemap one. Do not answer it in this plan;
   flag it in `docs/todo/PLUGINS_PLAN.md` if Option B ever becomes real.

5. ~~**(2026-11-08) Option A or B?**~~ **Answered: B, in core** (B1).
6. **(2026-11-08) Where curated tracks live.** `create` mode, a points segmentation named e.g.
   `immunemap`, is separate today. Does the benchmark need them on the *same* cells as a Cecelia
   segmentation, which is `attach` without overwriting and would need a second track column?
7. ~~**(2026-11-08) Experiment mapping.**~~ **Answered:** one set per experiment, condition as an
   attribute (B2).
8. **(2026-11-08) Benchmark scope.** Recall, ID switches and length coverage in xy (curated tracks
   are 2D, see *Revision*), plus motility comparison on the measures that exist. Arrest waits for
   `MOTILITY_METRICS_PLAN` P3b, or uses HMM state frequency in the meantime?

9. **(2026-11-08) Facets from group names.** The maintainer wants conditions chunked "more
   naturally" than one raw `condition`. A rule split gets the time reliably (`30 min`, `24 h`) and
   little else: "irrelevant antigens (BSA)" vs "irrelevant antigens", brackets meaning an agent in one
   experiment and "(for anti-beta1 integrin)" in another. Proposal: a group × facet table in the
   dialog, time prefilled, the rest typed or corrected by the user, stored as one attribute per facet
   beside the raw `condition`. **Answered: build it** (maintainer, 2026-11-08). Built as proposed;
   only time is parsed (a time written unit-first, "Day 3", is left blank rather than guessed).

## References

Repo-relative paths.

- `docs/examples/plugins/ccia-importTracks/README.md` — the plugin's own
  README, with the "templates not verified against real exports"
  warning.
- `docs/examples/plugins/ccia-importTracks/plugin.json` — plugin metadata.
- `docs/examples/plugins/ccia-importTracks/tracking/importCsvTracks.json`
  — the task JSON (form fields, modes, column mapping).
- `docs/examples/plugins/ccia-importTracks/python/track_readers.py` — the
  shared reader; JSON branch under Option A lands here.
- `docs/CUSTOM_MODULES.md` — plugin drop-in mechanics, PYTHONPATH,
  Limits (no pip deps in a plugin).
- `docs/todo/PLUGINS_PLAN.md` — parent plugins-plan; credential storage
  question (Decision 6) would land here if Option B is ever built.
- `docs/todo/MOTIF_DISCOVERY_PLAN.md` — P3 validation baselines section
  is a candidate consumer of this plugin.
- `docs/todo/BEHAVIOUR_READOUT_PLAN.md` — sibling parked plan; the
  reframe from "invent a vocabulary" to "read from an existing atlas" is
  captured in its Verdict.
- `docs/archive/behavior-readout-standardization-prompt.md` — the
  original prompt whose Immunemap reference triggered this discovery.
- `docs/archive/IMMUNEMAP_IMPORT_PROMPT.md` — a later full-importer brief
  (2026-11-08, not run). Its outcome note lists what it adds: the
  acquisition JSON's `cell_stainings` channel mapping, and the point that
  only recall is meaningful against the curated tracks.

External (source of the format spec):

- [celltrackR `immunemap-integration.R`](https://raw.githubusercontent.com/ingewortel/celltrackR/master/R/immunemap-integration.R)
  — authoritative reader; treat as the format spec until an OpenAPI
  spec is published.
- [Immunemap API root](https://api.immunemap.org/) — Symfony backend,
  no public swagger; route registry accessible at `GET /video/`.
- [Systematic analysis of immune cell motility leveraging Immunemap —
  EMBO J 2025](https://pmc.ncbi.nlm.nih.gov/articles/PMC12759063/) —
  the driving paper; combined the celltrackR importer with the atlas.
- [From microscopes to maps — companion EMBO J 2025 piece](https://pmc.ncbi.nlm.nih.gov/articles/PMC12759062/)
  — data-commons framing (20 labs, 400 videos, harmonized metadata).
