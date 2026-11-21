# Galaxy lessons — what Cecelia takes from the Galaxy platform, and what it doesn't

**Status:** **parked** (2026-11-21). Nothing built. Six lessons kept (L1–L6), plus a batch of small
record fields (L0). Two of them lean on plans already in flight: [`METHODS_REPORT_PLAN.md`](METHODS_REPORT_PLAN.md)
and [`EXPERIMENT_DESIGN_PLAN.md`](EXPERIMENT_DESIGN_PLAN.md) (#2414–#2416). Revisit when one of those
reaches the step named under each lesson, or when the maintainer picks a lesson.

## Goal

[Galaxy](https://github.com/galaxyproject/galaxy) is a multi-user web platform that runs bioinformatics
tools and workflows. It is MIT-licensed as of 2026-02-25. Most of it solves problems Cecelia doesn't
have: many users on one server, storage backends, and installing thousands of third-party tools. Its
strong part is the one we share: recording a run, repeating it, and reporting it. This doc records which
Galaxy patterns were checked against Cecelia, which are worth building, and which are rejected and why,
so Galaxy isn't audited again.

## Provenance — read before citing anything here

- **Who read Galaxy.** One Claude agent read a shallow clone at `6864679` (2026-11-20; 26.1 released,
  26.2 in development). It ran nothing. The coordinating session spot-checked the image assertions
  (`lib/galaxy/tool_util/verify/asserts/image.py`) and the job-cache query (`lib/galaxy/managers/jobs.py`).
  Everything else about Galaxy here is **an agent's reading**.
- **Who checked Cecelia.** One agent read `origin/main` at `97012b7a7`. The anchors cited below
  (`params_hash`, the run-log `inputs`/`gates`/`env` fields, `cecelia_version`, the `sacct` usage
  fields, `history_params_table`) were re-checked by the coordinating session.
- **Second pass.** A fresh agent fact-checked every row on both sides (2026-11-21). Its nine corrections
  are folded in: Galaxy records no git SHA; engine versions are already in `env`; `params_hash` also
  resets changed nodes downstream; wall time is already logged; chain-run timing is a join; the
  imaging row was incomplete; and Kiwi's failure reading was verified.
- **Licence.** MIT, so code may be borrowed with attribution. In practice only ideas transfer: Galaxy
  is Python, SQLAlchemy and Vue over a database, and Cecelia keeps its records in project files.

## Verdicts

| # | Galaxy has | Cecelia today | Verdict |
|---|---|---|---|
| L0 | Each job records `tool_version`, dependencies and container, but `galaxy_version` is only the major version (`26.1`, `tools/actions/__init__.py:1026`) and there is no SHA; tools carry `<citations>` | `cecelia_version()` = `pkgversion` only (`app/src/config.jl:130`). Engine versions are already in `env` (cellpose; coastal as `version+commit`, `coastal_utils.py:615`). Missing: Cecelia's own commit, the plugin version on run-log entries, and a citation field in task specs | **Keep.** Small fields. The SHA goes further than Galaxy does |
| L1 | Invocation reports: Galaxy-flavoured Markdown with directives (`job_parameters`, `invocation_inputs/outputs`, `history_dataset_as_image`, `job_metrics`), exported to HTML/PDF (`lib/galaxy/managers/markdown_util.py`, `_markdown_directives.py`) | Methods report planned, not built. The chain run record, `history_params_table`, cohort QC and board provenance exist | **Keep, as input to METHODS_REPORT_PLAN.** No second generator |
| L2 | Tests embedded in each tool's XML, with image assertions `has_image_{width,height,channels,depth,frames,mean_intensity,center_of_mass,n_labels,mean_object_size}` (`verify/asserts/image.py:396–812`), which read TIFF/Pillow only | Tests live centrally in `app/test`. Custom modules and plugins have no test convention for authors; the shipped examples are load-tested in CI (`app/test/suite/plugins.jl`) | **Keep.** Ours must read OME-Zarr through the image readers |
| L3 | Job cache: an opt-in per run (`use_cached_job`) reuses a past job if it has the same tool and version, the same parameters, inputs that are the same dataset rows (unchanged since: a version or `update_time` check, **never a content hash**), and outputs that aren't deleted (`JobSearch`, `managers/jobs.py` ~480–900). It also matches jobs in other users' published histories and still-running jobs | Within one chain run, `params_hash` skips done nodes on resume and `_reset_stale_nodes!` resets changed nodes and everything downstream (`execute.jl:917–950`). `stale_outputs_for` (`correction_staleness.jl`) already walks downstream staleness from the run log. The run log stores `inputs` (names + vN) and `gates` (content hashes) (`run_log.jl:551–553`) | **Keep.** Across runs, built on the staleness walk |
| L4 | Job metrics plugins (cgroup, meminfo, cpuinfo, hostname…) stored per job and shown in the UI, with carbon and AWS-cost estimates derived from them | HPC asks `sacct` (and PBS `qstat`) for `Elapsed,TotalCPU,MaxRSS,ReqMem,AllocCPUS,Timelimit` (`hpc/backends/slurm.jl:140`) but only writes them to the task log (`hpc/dispatch.jl:413`). Every run-log entry has `at`/`finishedAt`, so wall time is there; peak memory and host are not | **Keep** |
| L5 | Sample sheets (25.1): CSV/TSV/Excel workflow input with templates the author defines; the rule-based uploader builds collections from a pasted table | Image `attr` per set and `/api/images/attr/set`; no table import. The design record and its `preview` route (with an `attrs` override) merged in #2415; the dialog and Metadata-page work are still open in #2417 | **Keep, after #2417 merges** (it rewrites the dialog and Metadata panel L5 would touch) |
| L6 | RO-Crate export of an invocation (Workflow Run Crate profile, `lib/galaxy/model/store/ro_crate_utils.py`); BioCompute Objects | `.ccbundle` (own format); no standard export | **Keep, after L1** |
| — | Roles, groups, sharing, quotas, object stores, vault | Profiles are identity, not security; one OS user | **Reject.** Desktop / lab-server app |
| — | Conda or container per tool, tool shed | One shared pixi env plus opt-in envs | **Reject** |
| — | GA4GH WES / DRS / TRS servers | — | **Reject.** Nobody calls Cecelia as a service |
| — | Interactive tools (Jupyter, ilastik, RStudio in containers) | Pluto notebooks, own viewer | **Reject** |
| — | Datatype registry with sniffers and implicit converters | Everything converted to OME-Zarr at import | **Reject.** One internal format is the better design for imaging |
| — | Workflow best-practice lint (annotation, creator, licence, readme) | Chain templates carry `author` | **Not now.** Low value until chains are shared between labs |
| — | GalaxyAI error-analysis agent on failed jobs (26.0) | Kiwi has `get_task_log`/`get_task_history` (`api/src/kiwi_turn.jl:30`), a `task` ref resolves to the last run's status, and the MCP guidance routes failures to logs and `report_issue`. Missing: an "explain this failure" entry on a failed task | **Small, not now.** Kiwi can already do it when asked |

Where Cecelia is level or ahead: imaging (Galaxy has OME-TIFF and `ome_zarr` datatypes; viewers
vizarr, Avivator, Vitessce, Niivue, tiffviewer and annotateimage; an ilastik interactive tool; and
cellpose/CellProfiler wrappers in the Tool Shed. It has no napari or QuPath, and OMERO only as a file
source), AI (Galaxy's agents and its MCP server arrived in 26.0/26.1, and the MCP server is off by
default), chain replay with drift logging and an exact-results test (`app/test/suite/chain_replay.jl`),
and exported figures that carry their own provenance.

## Decisions (proposed, 2026-11-21)

1. **Ideas, not code.** The licence allows borrowing, but nothing ports. If a snippet is ever copied,
   its file carries an MIT attribution line.
2. **No second report generator.** L1 extends METHODS_REPORT_PLAN Decision 5 ("one generator, two front
   doors"). A chain-run report is that generator scoped to a chain run.
3. **Reuse is opt-in and visible.** L3 never skips silently. A reused step says so in the run record and
   in the live view, the way Galaxy marks a cached job.
4. **Tests use the fixture registry.** L2 tests name a fixture via `fixture_for` / `NEEDS.toml`. They
   never ship images in the PR.
5. **Sample sheets do not get their own store.** L5 writes `img.attr`, which the experiment design
   reads. It is a way in, not a new grouping.

## Phases

Each phase can ship on its own.

- **L0 — record fields.**
  - A git SHA next to `cecelia_version()` when running from a checkout, `""` in a release, read once per
    process through `git_probe` (as the REPL and runner already do) or a dev install's `.cecelia-version`.
  - The plugin name + version + ref on run-log entries written by plugin tasks.
  - An optional `citations` list (DOI) in the task JSON spec, filled for cellpose, btrack, coastal etc.
  - Checkpoint: one run-log entry from a plugin task shows all three. METHODS_REPORT_PLAN's "software
    versions to cite" row reads them.
- **L1 — chain-run report** (with METHODS_REPORT_PLAN P1). The methods generator accepts a chain run id
  as a scope. Sections taken from Galaxy's directive list, filled from records we already keep:
  - inputs (images and their design group);
  - parameters per node (`history_params_table`);
  - per-node status and timing;
  - cohort QC;
  - selected board figures.
  - Markdown first.
  - Checkpoint: the same chain run always gives the same text (METHODS_REPORT_PLAN Decision 1).
  - Timing: persisted chain runs keep no timing (`docs/SCHEDULER.md`, *Loading past runs*), but node state
    keeps `task_id` (`persistence.jl:212`), so timing is a join to the run log's `at`/`finishedAt`.
  - Galaxy 26.1 merged Pages and notebooks into one history-attached workspace with AI drafting. Our
    counterpart is the notebooks and the blackboard; L1 stays a deterministic document.
- **L2 — tests in the module spec.**
  - A `tests` block in the task JSON: fixture name, params, and assertions on the output.
  - Assertions after Galaxy's names: label count, mean object size, centre of mass, mean intensity,
    width/height/channels/depth/frames, all with tolerance, read through the zarr/label readers.
  - One runner in `app/test` iterates every built-in and custom spec that declares tests.
  - The `docs/examples/` plugin gets one test, so plugin authors have a pattern to copy.
  - Checkpoint: CI fails when a built-in segmentation's label count on its fixture drifts beyond
    tolerance.
- **L3 — reuse an identical run.**
  - Key = task, Cecelia version (and plugin version), sanitised params, and every input's name, vN and
    content stamp (gate hashes already exist; image stores have `store_rev`, the stat-only fingerprint in `importImages/omezarr/fingerprint.jl` the viewer already uses).
  - A match counts only when its output still exists and is unchanged.
  - Opt-in per chain run ("Reuse unchanged steps").
  - Checkpoint: re-running a chain over a cohort after editing one gate re-runs only the steps
    downstream of that gate.
  - "Unchanged" for a zarr store: Galaxy's answer is identity plus a version/`update_time` check, never a
    content hash. Ours: the input's vN plus its run-log entry (`taskId`, `finishedAt`) as the identity;
    a newer run-log entry for that output = changed. Built on `stale_outputs_for`, the same walk run in
    reverse.
  - **Built** (`app/src/tasks/chain/reuse.jl`; `docs/SCHEDULER.md` → *Reuse unchanged steps*):
    image-scope nodes only, set-scope never reused; the key has no plugin version or git commit yet (L0).
- **L4 — resource use per run.**
  - The `sacct`/`qstat` fields already fetched go into the run-log entry as `usage`.
  - Local runs add peak RSS of the Python child and the host (wall time is already `at`/`finishedAt`).
  - The `hpc.mem` memory model in each spec can then be checked against real numbers.
  - Checkpoint: the task history shows memory used next to memory requested.
- **L5 — attributes from a table** (inside EXPERIMENT_DESIGN_PLAN step 1).
  - Paste or drop a CSV/TSV keyed by image name or original path; it writes `img.attr` through
    `/api/images/attr/set`, previewed in the design grid (`/api/sets/experiment-design/preview` takes an
    `attrs` override since #2415).
  - It sits beside the dialog's suggestion from names (`regexBuilder.ts`), for the case where the
    mouse and donor list lives in a spreadsheet.
  - Checkpoint: xxHqjQ's tissue and donor per core entered from one table (that plan's Open decision 4).
- **L6 — RO-Crate export of a chain run.** It reuses L1's content and writes `ro-crate-metadata.json`
  (Workflow Run Crate profile) beside the report and parameter table, so Zenodo and WorkflowHub can read
  it. Checkpoint: the crate passes an RO-Crate validator.

## Open decisions (the maintainer's call)

1. **Order.** Proposed: L0 → L4 (cheap, independent) → L1 with METHODS_REPORT_PLAN → L2 → L3 → L6. L5
   after #2417 merges.
2. **L3 scope.** Within one project only (proposed), or also across copies of a project?
3. **L2 for custom modules.** Is a declared test optional for plugins, or required before
   Settings → Plugins installs one?
4. **Error explanation.** Add an "Ask Kiwi why" entry on a failed task? The reading side exists.
