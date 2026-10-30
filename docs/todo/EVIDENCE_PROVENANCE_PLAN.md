# Evidence provenance — run stamps on captures + backlinks for staleness

**Status:** parked (2026-10-30). Trigger to un-park: a Blackboard entry or capture misleads you or
Claude after a re-segment / re-track / gate edit. No such case was on record when this was written.

## Goal

Make it possible to tell, from an entry or capture, whether the evidence it cites still describes the
data on disk. A capture is only provenance if it records which **run** made what it shows, and an
entry is only trustworthy if something can find it when that run is superseded.

This plan came out of a review of Tuva/Rao (a vendor "research brain" knowledge graph) against what
Cecelia already has. Result: no graph store and no third-party index. The Blackboard plus typed
`kiwiRefs` plus capture addresses already form an asserted link graph, and every link is either
checkable (`/api/kiwi/refs/resolve`) or reviewable. The two pieces it lacks are below.

## What exists (verified 2026-10-30)

- **Capture address** (`frontend/src/utils/captureAddress.ts` → `CaptureAddress`, written by
  `api/src/captures_api.jl` → `_build_capture_envelope`): `projectUid`, `imageUid`, `valueName`,
  `t`/`z`, `plotSpec`. It names the output, not the run that produced it.
- **Run log** (`app/src/run_log.jl`, `{1/uid}/runlog.json`): per run `{fun, valueName, status,
  params, at, taskId, pool, by, finishedAt}`, with params sanitised. Capped at `RUN_LOG_CAP = 200`
  entries per image, oldest dropped first. `run_log_params_for_output` already resolves "params of
  the last `done` run that wrote this output".
- **Overwrite by default.** `keep_previous_version()` is off by default and only the pixel writers
  call `plan_versioned_target`. Segmentation (`app/src/segmentation.jl` → `unversioned_set_field!`
  on `labels`) and `measure_labels` (`label_props`) always overwrite. Gating
  (`gating/{vn}.json`) is a single file per output.
- **Links that already exist:** entry `kiwiRefs` (typed; resolve-checked), entry attachments
  (→ captures), capture ids in entry prose (`CAPTURE_ID_RE`), `previousCaptureId`, knowledge
  `from`, `get_analysis_lineage`. All of them point one way. No reverse lookup exists, and substring
  search is the only answer to "what cites X?".

## Decisions

1. **No graph store, no inferred edges.** Backlinks are computed on demand by scanning the
   project's entries and captures. Nothing is persisted, so nothing can drift from the source.
   Projects hold tens to hundreds of entries, so the scan is cheap.
2. **The stamp points at the run log; it doesn't copy it.** A capture records
   `producedBy: {fun, taskId, at, version}`. Params are looked up in `runlog.json` when needed.
   A stamp whose run has rolled out of the 200-entry cap reads as "run no longer on record". It
   still dates the evidence.
3. **Stamped server-side at write.** `_build_capture_envelope` resolves the latest `done` run-log
   entry whose **output** is `address.valueName` on `address.imageUid`, plus `resolve_version` for
   that output. A run-log entry's own `valueName` is the task's *input* version
   (`app/src/tasks/scheduler/jobs.jl` records `job.params["valueName"]`), so matching on it finds
   the wrong run. Reuse the output resolution in `run_log_params_for_output`
   (`task_output_name(task, params)`): extract it into an entry-returning sibling rather than
   re-deriving it. No frontend change, and no new client field to forget.
4. **Gate edits are covered by a content hash, not a run.** Gate edits never hit the run log. A
   population-bearing capture or ref also stamps a hash of `gating/{vn}.json` for its pop type,
   whole file, via the existing `_file_sha256` (`app/src/utils.jl`). A hash rather than mtime,
   because export/import and copy rewrite mtimes. Whole-file means editing any gate in that file
   flags every population in it. Narrowing it to one population would need canonical-JSON hashing
   of a slice (see the sorted-pairs hashing in `validate.jl`); only do that if the coarse flag
   proves noisy.
5. **Staleness is reported, never repaired.** An entry or capture whose stamp predates the current
   run or gating hash is flagged ("evidence predates current segmentation"). Entries are never
   edited automatically. This matches the analysis-canvas rule that reads never mutate.
6. **Legacy captures (no stamp) fall back to inference, labelled as such.** Join `createdAt`
   against the run log (last `done` run before it), and report the result as *inferred*, never as
   recorded.
7. **Versioning stays opt-in.** This plan doesn't turn on `keep_previous_version` or version
   seg/track outputs. A stale flag tells you the data changed; keeping old outputs costs disk most
   users don't want. Full versioning stays in [`VN_VERSIONING_PLAN.md`](VN_VERSIONING_PLAN.md).

## Phases

- **P1 — run stamp on viewer captures** (~½ day). `producedBy` in `_build_capture_envelope`, an
  API test with a fixture run log (copied from a real `runlog.json`), and `get_capture` returns
  it. Plot captures are skipped: one capture can pool many outputs.
- **P2 — backlinks + staleness on read** (~1 day). One Julia function, `evidence_refs(proj)`, scans
  entry `kiwiRefs`, attachments and prose capture ids, and resolves each capture's stamp against
  the current run log. Surface it as a `citedBy` / `stale` field on the entry read and a one-line
  count in `get_session_briefing`. No new MCP tool unless the field proves too coarse.
- **P3 — gating hash** (~½ day). Decision 4, for population/cells/tracks refs and captures of a
  population overlay.
- **P4 — plot captures** (only if P2 shows they matter). Per-`dataRef` stamps.

## Open questions

- Is 200 runs per image enough history for stamps to stay resolvable? Check one long-running
  project's `runlog.json` lengths before P1.
- `app/CLAUDE.md` says "the `labels` / `label_props` / `branch_labels` writers use the same
  helper", but `segmentation.jl` writes `labels` via `unversioned_set_field!`. Confirm which is
  meant and correct the doc in P1.
- Should a stale flag also show in the UI (Blackboard page, capture reshow), or stay MCP-only at
  first? MCP-only is cheaper; it's Dominik's call once P2 exists.
