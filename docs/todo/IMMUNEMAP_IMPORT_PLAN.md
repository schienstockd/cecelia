# Immunemap import — plan

> **Status: parked (2026-09-17) — revisit when a real need lands.** No
> branch. Sits alongside `docs/todo/MOTIF_DISCOVERY_PLAN.md` (motif work's
> P3 lists validation baselines as a to-do — an Immunemap track is one
> candidate) and `docs/todo/BEHAVIOUR_READOUT_PLAN.md` (also parked). The
> discovery work — the Immunemap format spec, the existing
> `ccia-importTracks` plugin's fit — is captured here so the next session
> starts from the answer, not the search.

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
    frame_base = 1, spotUnit = pixel).
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
`/register`. Everything except `/video/` (route registry) and
`/search` requires JWT. Register at `api.immunemap.org/register`.

### What Cecelia already handles

- **IMS/HDF5 imagery**: `importImages.omezarr` uses `bioformats2raw`,
  which reads Imaris IMS natively. No new code needed for the movie side.
- **Centroids-only tracks**: `ccia-importTracks` `create` mode already
  makes a "points segmentation" with the motility-only caveat. The
  earlier concern about needing to invent a data-model kind for
  centroids-only is void — the plugin already ships this.
- **Column-mapping**: `spotUnit = pixel` + `frameBase = 1` + per-column
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
