> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in [`docs/todo/IMMUNEMAP_IMPORT_PLAN.md`](../todo/IMMUNEMAP_IMPORT_PLAN.md).
>
> **Outcome:** not run. Archived 2026-11-08 (`origin/main` 19f2139b). It was written without knowing
> that `IMMUNEMAP_IMPORT_PLAN.md` (parked 2026-09-17) already exists. Checked against main:
> - **Most of "Phase 0" is already answered in the plan.** It has the API (`api.immunemap.org`,
>   probed route table), the track format (`/video/{uid}/tracks`, `points` = `[t, x, y, z]` in
>   **pixels + frames**, z = 1 for 2D), the scale source (`size.spacing`, `size.fps`) and the raw
>   `.ims` download. Its auth answer was wrong: a re-probe on archiving showed **public data needs
>   no JWT**, and its frame base was wrong too (tracks are 0-based). Both are corrected in the
>   plan's *Revision 2026-11-08*, which reopens D3 and D6.
> - **The brief asks for what the plan deliberately deferred.** One-button fetch and download is
>   the plan's Option B, put off until a cohort need is real (D3), because credential storage
>   would be a new ecosystem-wide decision (D6). The plan's MVP is an `immunemap` template plus a
>   JSON reader in the existing `ccia-importTracks` plugin, with the movie going through
>   `importImages.omezarr` (D1, D2). Not built: the plugin's `templates/` still holds only
>   ImageJ, Imaris and the two TrackMate templates.
> - **"Ground truth stored separately" only holds in `create` mode.** `attach` writes `track_id`,
>   the same column `tracking.bayesian_tracking` writes, so it **replaces** Cecelia's own tracks on
>   that segmentation. `create` writes a separate, named points segmentation, so curated tracks
>   should go in through `create`.
> - **No arrest-coefficient column.** Arrest exists only as the HMM `arrested`-state frequency via
>   `track_props`. The designed route, time in a gated slow population with dwell bouts, is
>   `MOTILITY_METRICS_PLAN.md` P3b and isn't built. No recall or ID-switch tracking benchmark exists.
> - **Still true:** Bio-Formats reads `.ims`, with Imaris-specific worker and heap limits in
>   `config/image_format.jl`. The IMS time interval is read by
>   `importImages/read_ims_time_interval_run.py`. The DOI is verified through Crossref
>   (Pizzagalli, Carrillo-Barberà et al., *EMBO J* 2025-11-20).
> - **New in this brief:** the acquisition JSON schema. Channel names come from `cell_stainings[]`
>   and their `selected: true` channel, `experimental_group` maps to condition, and checksums come
>   from `original_file`. Also new: the point that recall against curated tracks is meaningful but
>   precision is not, because the tracks are a hand-picked subset. Both are now in the plan's
>   *Revision 2026-11-08*.

# IMMUNEMAP_IMPORT_PROMPT

## Goal

Build an importer for Cecelia that takes an **Immunemap experiment** (https://app.immunemap.org/experiment-public-view?id=<N>) and:

1. Fetches all relevant images (videos) of that experiment.
2. Creates a Cecelia experiment with correct calibration, channel names and condition/treatment metadata.
3. Where available, imports the **curated tracks as ground truth**, stored separately from Cecelia-generated tracks, for benchmarking tracking and behaviour clustering.

Immunemap is an open atlas of intravital microscopy (Pizzagalli et al., EMBO J 2025, doi:10.1038/s44318-025-00629-4). Videos come from ~20 labs; tracks were manually curated with TrackMate.

## Before you write code

1. Read `CLAUDE.md` and the repo layout. Determine which architecture is current (R/Shiny vs the Julia + Vue rewrite) and follow its conventions for experiments, import tasks, image storage, labels/tracks and tests. Do not assume.
2. Find how Cecelia currently represents: an experiment, an image set/condition, channel names, pixel size, time interval, and externally supplied tracks. Reuse these. Add a new concept only where nothing fits.
3. Propose the design in a short plan and **wait for confirmation** before implementing.

## What is already known (from browser-captured JSON)

The SPA at `app.immunemap.org` cannot be scraped (JS shell). Its backend serves JSON. The acquisition object looks like this (one acquisition = one video):

- `id` (acquisition, e.g. 309), `experimental_group` (e.g. 101, 103), `name` (original file, e.g. `Obino-Movie1-SteadyState.ims`), `video` (video DB id, e.g. 409)
- `notes` (free-text treatment description), `tags[]`, `links[]` (DOI), `area`, `gender`, `mouseId`, `public`, `access.permissions`
- `cell_stainings[]`: each has `class` (e.g. "B cells", "Collagen", "Microspheres"), `fluorophore`, `moleculeStructure`, `staining_method`, `imagedEntity`, and `channels[]` where **`selected: true` marks the channel that carries that staining**. Channel names in `channel.name` are generic ("Channel 0") and must NOT be used as labels.
- `video_details`:
  - `id`/`uid` (26-char string), `DB_id`
  - `size`: `width`, `height`, `slices`, `frames`, `number_of_channels`, `spacing` [x, y, z] in µm, `fps` (Hz; interval = 1/fps, ~30 s), `time_steps[]` (ISO timestamps)
  - `original_file`: `md5`, `sha1`, `size` (bytes, 500-800 MB per video)
  - `tracks[]`: list of track IDs (100-250 per video)
  - `channel_configuration.channels[]` (display colour/contrast only)
  - `location`: **server-internal path, not usable for download**
  - `recorded`, `author`, `owner`, `upload_status`

Observed variation that the importer must handle: voxel size differs per video (0.45 µm vs 1.80 µm in xy in two videos of the same paper), so calibration must be read per video. `experimental_group` differs between conditions of the same study (101 steady state, 103 BSA beads 90 min).

Per the Immunemap paper, the public APIs serve: videos in HDF5, experimental details in JSON, and track coordinates as CSV or JSON.

## Unknowns: Phase 0 discovery (do this first)

I could not confirm these. Do not guess endpoints.

1. Base URL and path of the JSON API behind the acquisition and experiment pages.
2. How to list all acquisitions for an experiment, and how the page's `id=<N>` in `experiment-public-view` relates to `experimental_group` (may be the same ID space or not).
3. Video download endpoint and format (HDF5 vs original `.ims`; `.ims` is HDF5-based).
4. Track endpoint(s): per track ID, per video, or bulk; JSON vs CSV; coordinate units (µm or px? time in s or frame?); axis order; which fields exist (track ID, cell type, spot IDs).
5. Whether auth or rate limits apply to public data.

If the endpoints cannot be determined from the repo, the paper's supplementary code (Python/R/Matlab examples, including the CelltrackR importer) or public docs, **stop and ask me** to paste Network-tab request URLs and one sample response each. Provide a short checklist of exactly what you need.

## Design requirements

**Input:** experiment ID (or URL). Options: dry run (list acquisitions, sizes, conditions, no download), subset selection (by acquisition ID, group or tag), `--no-tracks`.

**Safety and etiquette**
- Dry run is the default: print number of videos, total download size, conditions, and licence/citation info; require explicit confirmation to download (an experiment can be many GB).
- Sequential or low-concurrency downloads with retry/backoff; resumable; skip files already present with matching md5.
- Verify md5/sha1 from `original_file` after download.
- Record provenance per image: Immunemap acquisition ID, video uid, experiment/group ID, source URL, DOI, retrieval date, original filename, checksums.
- Surface the DOI/reference and a note that data come from contributing labs and licences should be checked per dataset; do not strip attribution.

**Image import**
- Convert to Cecelia's native image format using existing import code; do not write a parallel pipeline.
- Apply per-video calibration: spacing x/y/z, frame interval (1/fps, cross-check against `time_steps` deltas and warn on mismatch), dimensions.
- Channel naming: for each `cell_stainings` entry take the channel with `selected: true`, and name it from `class` (+ `fluorophore`). Handle channels with no selected staining and stainings with several selected channels. Keep the raw Immunemap channel index.
- Condition/treatment: derive from `experimental_group` and `notes`/tags. Keep the raw text; do not over-parse free text. Make the grouping overridable.

**Curated tracks as ground truth**
- Store as a distinct, clearly labelled track set (e.g. "Immunemap curated"), never mixed into or overwriting Cecelia tracking outputs.
- Convert coordinates to Cecelia's convention (units, axis order, frame vs time) with an explicit unit test against a known track. Check that tracks land on visible cells when overlaid (provide a quick overlay check).
- Keep per-track metadata (cell type/class if provided).
- Important caveat to encode in code and docs: the curated tracks are a **manually selected subset of cells**, not an exhaustive annotation. Therefore recall against them is meaningful; precision is not (an untracked cell is not necessarily a false positive).

**Benchmark hook (small, optional)**
- A function that compares a Cecelia track set to the curated set: centroid match within a distance threshold per frame, track-level matching (e.g. fraction of curated tracks recovered, ID switches, length coverage), and comparison of motility metrics (speed, directionality, arrest coefficient) between curated and Cecelia tracks. Keep it minimal; no new UI unless trivial.

## Edge cases to handle explicitly

- Videos with `upload_status` not complete, `deleted: true`, or without tracks (annotations `[]`, empty `tracks`): import the image, skip tracks, report.
- Mixed pixel sizes within one experiment: do not resample silently; keep native calibration.
- Videos with more channels than `number_of_channels` (6 virtual channels listed, 3 real).
- Large files: stream to disk, never load whole files into memory.
- Duplicate import (same video twice): detect via uid/md5 and reuse.
- Network failure mid-experiment: partial state must be resumable and visible.

## Deliverables

1. Phase 0 findings (endpoints, schemas, units) with sample responses, or the exact list of what you still need from me.
2. Short design plan for confirmation.
3. Implementation following repo conventions, with tests (mocked HTTP using the captured JSON shapes above; a unit test for coordinate conversion; a test for channel-name mapping from `cell_stainings`).
4. A brief doc section: how to run, what is stored where, ground-truth caveat, licence/citation note.

## Non-goals

- No upload to Immunemap.
- No scraping of the rendered web UI.
- No automatic tracking or clustering runs after import.
- No changes to existing tracking code beyond what the importer needs.

## Questions to answer in your plan

1. Where does a "ground truth" track set best live in Cecelia's data model, and does anything existing already fit?
2. Should an Immunemap experiment map to one Cecelia experiment with conditions, or one experiment per `experimental_group`?
3. What is the smallest end-to-end slice (one acquisition: image + tracks + overlay check) to build first?
