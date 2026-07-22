# Project I/O — export / import

Status: **in progress** (branch `feat/project-io`). Foundation (per-store pack/unpack) landing first;
Julia task + API routes + frontend follow.

## Problem

The old R/Shiny cecelia had a clunky project transfer path (`projectManager.R::exportProject` →
uncompressed `tar` of the **whole** project tree; import = `untar`; plus a now-dead Mediaflux/HPC/SMB
backup path in `mflux/*` + `sshUtils.R`). It was slow, and backups were a nightmare.

Root cause is **not** compression — it's file count. Images are `bioformats2raw` OME-Zarr **v2
DirectoryStores** (one file per chunk). With per-plane chunking (1 along T/C/Z, ~512² tiles on Y/X) a
single large image is hundreds of thousands to millions of tiny files. `tar`/`rsync`/cloud/object-store
are all `O(files)` on `open`/`stat`/`readdir` syscalls, not on bytes — so a store that's tens of GB
takes forever to walk. Chunks are already blosc-compressed, so `tar -z` on top burns CPU for ~zero gain.

**Sharding is off the table (correctly).** Sharding is a Zarr v3 feature; OME-NGFF only adopts Zarr v3
in its 0.5 line, and the tooling we depend on (`bioformats2raw` output, the napari OME-Zarr reader) is
still NGFF 0.4 / Zarr v2. Adopting sharding means waiting for that ecosystem to move — not our call.
So the fix lives entirely at the **packaging/transport layer** and leaves the working store untouched
(still NGFF 0.4 / v2, still napari-readable).

Honest consequence: while the working store is a directory-of-chunks, *packing* it still reads every
chunk once — that one-time `O(files)` cost is unavoidable without sharding. The win is you pay it
**once per store**, and because a finished image store is **immutable**, everything downstream (copy,
sync, backup, restore) becomes `O(stores)` instead of `O(millions of chunks)`. The R version threw that
away by re-taring the whole project tree on every export/backup.

## Locked decisions

- **All Julia, no Python.** Export/import is filesystem work (walk the tree, copy files, archive each
  store) and the project/object model is Julia's — so `app/src/project_io.jl` owns it. (Earlier drafts
  packed stores in Python with `zipfile`/`ZipStore`; dropped — the zarr-ZipStore "lazy read without
  unpacking" bonus isn't used by any pathway, so it didn't justify a Python subprocess.)
- **Archiver = system `tar`.** One file per store via `tar -cf …` / `tar -xf …` — no new Julia dep
  (Julia has no stdlib ZIP; `ZipFile.jl` would trigger the 3-manifest re-resolve) and it matches the
  self-updater, which already shells out to `tar`. No compression flag: chunks are already blosc'd.
- **Parallel over `Threads.@spawn`** (bounded, `_pack_concurrency()`): the per-store read is I/O- and
  latency-bound, so concurrent tars overlap per-file stat/open wait — big win on SSD/NVMe + network
  storage, neutral on a single spinning disk.
- **Background jobs, not scheduler tasks** — project-scoped, no image target, no pool/chain/QC. Built
  on `app/src/jobs.jl` (`track_job!`/`cancel_job!`), same base as Settings data patches. See `docs/JOBS.md`.
- **Neither side needs an open project** — export reads a project dir off disk by uid; import creates a
  new one. Both live in the **Project Manager** (`ProjectPanel.vue`).

## Bundle format
`{proj_uid}.ccbundle/` = a faithful mirror of the project tree with each `.zarr` store replaced by one
`<name>.zarr.tar`, plus a `ccbundle.json` manifest (`formatVersion`, `projectUid`, `projectName`,
`packedStores`). Non-store metadata (`project.json`, `ccid.json`, `labelProps/*.h5ad`, CSVs) copied
verbatim; `.cecelia.lock` + `*.bak.ome.zarr`/temp dirs skipped. Bundle file count = O(stores).

## Pathways (export + import)

### Pathway 1 — Export a project  ✅ built
`export_project(uid; out_dir, task_id, on_log, on_progress)` (`project_io.jl`). WS `project:export`
(`handle_project_export`) spawns it, streams over the task rail, cancellable. Writes the bundle above.

### Pathway 2 — Import a project  ✅ built
`import_project(bundle; mode)`. WS `project:import` (`handle_project_import`). Copies the tree, unpacks
each `.zarr.tar` in parallel, returns the imported uid. **Source is any server path** — picked via the
shared `FileBrowser` (`bundle` mode; mounts/drives reachable) or the `cecelia_exports` dropdown or a
pasted path. **uid collision** → the UI peeks via `GET /api/projects/bundle-info` and prompts
(`bundle_info`): **Replace** (overwrite in place — danger-styled, blocked for the currently-open
project) or **Cancel**. Merge is deliberately out of scope (ambiguous conflict semantics).

#### `copy` (import under a new uid) — RE-ENABLED after the project-identity cleanup
Originally hidden because the project uid was baked into project-internal files (a new-uid copy left
stale references). The cleanup below removed that coupling, so `copy` is now exposed (Replace / Copy /
Cancel prompt) and correct:

| Where the uid was embedded | Fix | Result |
|---|---|---|
| `settings/analysisBoards.json` layout keys `analysis:<uid>:tab:N` | stored **project-relative** (`tab:N`); re-keyed to the current uid on load (`frontend/src/utils/boardKeys.ts`) | boards survive a uid change |
| `settings/chains/runs/*/run.json` `project_uid` | `load_chain_run` uses `proj.uid` (identity from **location**); stored field is advisory | runs resume against the current project |
| `notebooks/*.jl` `load_project("<uid>")` | best-effort literal rewrite (`_reidentify_files!`) | common case handled; fancier user refs are the user's to fix |
| `1/*/logs`, `1/*/tasks/*.params.json`, `ccid.json` | — | already path-relative / uid-absent |

The general model op is **`reidentify_project!(proj_uid, new_uid; new_name)`** (rename dir + `project.json`
+ notebook rewrite); import-copy re-identifies inline. Invariant now documented in `docs/OBJECTMODEL.md`:
*a project-internal file must not bake in its own uid — identity comes from location.* This also unblocks
a future in-place project **rename** (change the uid) reusing `reidentify_project!`.

### Backup — NOT a separate feature (decided against)
No recurring/automated backup machinery. **Export _is_ the manual backup**: a `.ccbundle` is one
portable artefact per project you copy/sync wherever you like. (Rationale kept for history: because a
finished store is immutable, syncing bundles with a dumb file tool is naturally incremental — but we're
not building any of that; the user drives it.)

## Explicitly out of scope
Recurring/automated backup, working-store format changes, sharding, consolidated metadata, and the dead
Mediaflux/HPC/SMB machinery.

## Build order
1. ✅ **Foundation + export + import backend** — `jobs.jl` (generalised registry + cancel, kill
   primitives moved from scheduler.jl), `project_io.jl` (`export_project`/`import_project`, parallel
   `tar`), package test (`app/test/runtests.jl` → "project export/import"). Docs: `docs/JOBS.md`.
2. ✅ WS handlers — `project:export` / `project:import` in `api/src/sockets.jl` (+ route `task:cancel` →
   `cancel_job!`) + `GET /api/projects/bundles` for the import picker.
3. ✅ Frontend — Export (per-project) + Import picker in `ProjectPanel.vue`, WS send + task-rail progress.

Feature complete (export + import). Manual browser click-through pending.
