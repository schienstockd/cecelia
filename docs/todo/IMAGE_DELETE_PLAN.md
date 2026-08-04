# Structured image delete — one modal on Import, one automatic sweep in Settings

Status: **planning** (no branch). Supersedes `docs/TODO.md` **#00094**, which recorded the problem;
this holds the design. Dominik, 2026-08-04.

## Goal

Collapse the five scattered ways to delete part of an image down to **two sites**:

1. **Import page → Delete** opens a *modal* offering every per-image deletion, on the current
   selection: whole images, specific image *versions* (several at once, with the new `_active`
   picked), specific label sets, and "drop all the numbers".
2. **Settings → Storage** stays as-is: whole-project, automatic, "free what is obviously reclaimable".

Two sites go away: the `importImages.remove` **task function** disappears from the UI, and the
**napari ViewerPanel** label delete is scrapped.

## Problem (what exists today)

| Entry point | Granularity | Deletes |
|---|---|---|
| `importImages.remove` task ("Remove image data") | one version, one image (task rail + chains) | one registered version, via `remove_image_version!` |
| Settings → Storage → *Free up space* | **every** non-active version in the project | same core, via `reclaim_inactive!` |
| Settings → Data patches → *Remove leftover stores* | unreachable debris | staging/unregistered/truncated stores (`store_sweep`) |
| napari ViewerPanel → label delete | one label set, one image | `labels/*.zarr` + `labelProps/*.h5ad` (`POST /api/images/labels/delete`) |
| Import action bar → **Delete** (#471) | the image selection | everything: `{proj}/0/{uid}` + `{proj}/1/{uid}` |

The deletion *cores* are already shared and fine (`remove_image_version!` `app/src/storage.jl:125`
backs both the task and the reclaim API; `store_sweep.summarise` backs both the debris count and its
patch). The problem is above them: five doors, four locations, two mental models (per-version vs
per-image) — plus three gaps: nothing drops derived analysis wholesale, `StorageSummary.imageBytes`
can't see `1/{uid}` at all, and `delete_image!` is both dirs or nothing.

## Locked decisions (2026-08-04)

1. **The Delete button becomes a modal**, not a straight two-click action. `ImageFileActions.vue`'s
   `ConfirmDeleteButton` is replaced by a button that opens `DeleteImagesDialog.vue`; the destructive
   confirm moves *inside* the modal (arm/confirm on its primary button), so nothing deletes on one
   click. Scope line at the top states what the selection is ("3 images").
2. **Four delete scopes in one modal**, radio-selected, because they answer different questions and
   must not be silently combinable:
   - **Whole images** — today's behaviour (`/api/images/delete`, both dirs).
   - **Image versions** — a multi-select list of value names + a "new active version" picker
     (`remove_image_version!` already takes `new_default`).
   - **Label sets** — a multi-select list (`/api/images/labels/delete` per set).
   - **All analysis ("the numbers")** — keep `ccid.json` + the image stores, drop everything derived.
3. **`importImages.remove` is UNLISTED from the UI, not deleted.** It stays registered and runnable.
   Reason: `app/test/suite.jl` uses it as its real-task workhorse in ~15 testsets (chain end-to-end,
   fault isolation, scope resolution, `_spec_output_value_name`, registry dispatch) *because* it does
   genuine ccid.json + disk work with no external binary, and it is a legitimate **chain node** (a
   pipeline that corrects an image and then frees the original). Deleting the struct would rip out the
   chain suite's workhorse and remove a chain capability to save a duplicate menu entry. Unlisting
   needs a new `"hidden": true` flag in the task spec, honoured by `GET /api/tasks/definitions` — the
   first such flag, so it goes in `docs/MODULES.md` with the param-type reference.
4. **The ViewerPanel label delete is scrapped**, per Dominik. Cost accepted: in the viewer you have the
   layer in front of you, which is the most natural place to decide it is junk; after this you go to
   the Import page instead.
5. **Settings → Storage is untouched.** It is the *automatic, whole-project* site ("free every non-active
   version", "sweep debris"); the modal is the *deliberate, per-image* site. Neither grows the other's
   job. Its one gap — `imageBytes` ignores `1/{uid}` — is fixed by Phase 4, not by moving anything.
6. **Multi-image selection resolves names by intersection**, like `CopyDialog.vue`: with several images
   selected, the version and label-set lists offer only names *every* selected image carries, and the
   modal says so. A name present on some images only is not silently skipped.
7. **"All analysis" uses a KEEP-list, never a delete-list.** `ccid.json` is the only non-derived file in
   `1/{uid}`; everything else (`labels/ labelProps/ gating/ populations/ mesh/ branchLabels/
   spatialGraph/ spatialStats/ stats/ cl/ shapes/ out/ data/ qc/ tasks/ logs/`) is output. A
   delete-list silently leaks whatever analysis dir is added next. The keep-list is pinned by a package
   test that fails when a new sibling appears.

## Architecture

**New Julia core** — one function, the only genuinely new backend piece:

```julia
# app/src/storage.jl — beside remove_image_version!, sharing its lock discipline
reset_image_analysis!(img::CciaImage; on_log) -> (freedBytes, dropped::Vector{String})
```
`rm -r`s every child of `img._dir` except the keep-list (Decision 7), then in ONE `commit_state!`
clears the analysis registrations (`labels`, `label_props`, `branch_labels`) while leaving `filepath`,
`imChannelNames`, `meta`, `attr`, `included`/`note`/`starred` and `status` alone. Same discipline as
`remove_image_version!`: delete outside the lock (multi-GB), commit inside it, re-read fresh.

**Routes** (`api/src/routes.jl`, registered in `server.jl`, added to the route-list test):
- `POST /api/images/analysis/reset {projectUid, imageUids}` → `reset_image_analysis!` per image.
- Existing, reused unchanged: `/api/images/delete`, `/api/images/labels/delete`,
  and a new `/api/images/version/remove {projectUid, imageUid, valueName, newDefault}` thin adapter
  over `remove_image_version!` (today only the task and `reclaim_inactive!` reach it).

**Frontend**: `DeleteImagesDialog.vue` (BaseModal) owns the scope radio + the two multi-selects + the
new-active picker; `ImageFileActions.vue` keeps the loop-with-`k/N`-readout + toast it already has
(`docs/UI.md` → *File operations*), so progress reporting is not re-invented.

**The archive preset is nearly free — verify before designing more.** `remove_image_version!` on
*every* version already leaves the image un-imported: `filepath` empty, `status="pending"`,
`SizeC/T/Z` cleared (its safe-primary rule). The frontend's `isImported(img)` is
`Object.keys(filepaths).length > 0`, so the napari eye, Copy and Crop **already** disable themselves
with "Import this image first". So "drop pixels, keep numbers" may need no new state at all — Phase 3
is mostly checking that the *backend* tasks refuse a filepath-less image cleanly rather than half-write.

## Phases

1. **Modal shell + whole-image scope.** `DeleteImagesDialog.vue` with the scope radio, wired to
   today's `/api/images/delete` path. No behaviour change, no new backend. Ships alone.
2. **Versions + label sets.** The two multi-selects, the new-active picker, the
   `/api/images/version/remove` adapter. Then unlist `importImages.remove` (Decision 3) and delete the
   ViewerPanel delete (Decision 4) — *after* the modal covers both, never before.
3. **All-analysis scope.** `reset_image_analysis!` + its route + the keep-list package test. Check the
   filepath-less/analysis-less image renders and refuses cleanly across module pages.
4. **Storage box sees analysis.** Add derived bytes to `StorageSummary` so the one screen that reports
   disk usage stops being silent about `1/{uid}`.

## Open questions

- **Does `runlog.json` survive an analysis reset?** Keeping it means the table shows a last-run tag for
  an image with no output; dropping it loses the provenance of what *was* done. Leaning keep, and let
  the run tag be honest about history rather than current state.
- **Does the analysis reset also drop derived image *versions*** (`ccidDriftCorrected.ome.zarr`), or is
  that strictly the versions scope? Leaning strictly-versions — one scope, one meaning.
- **Multi-version removal ordering.** Removing several versions in one go must not trip the
  safe-primary rule mid-loop (remove `default` first while others remain → fine; remove others first
  then `default` → un-imports). Fix by ordering `default` last, and test it.

## Reservations

- Phase 2 removes two working affordances. If the modal's version list is wrong, the fallback (the
  task, the viewer button) is gone — hence the ordering rule in Phase 2.
- Three of the four scopes are irreversible and now operate on a *selection*; the modal must state the
  count and what survives, per `docs/UI.md` → *File operations*.
