# Structured image delete — one modal on Import, one automatic sweep in Settings

Status: **BUILT** — all four phases landed in one change (2026-08-04). Kept as the record of *why* the
shape is what it is; the durable "how it works" now lives in `docs/UI.md` → *Deleting is one modal with
four scopes*, `docs/OBJECTMODEL.md` → *Dropping the analysis*, `docs/MODULES.md` → *`hidden`*, and
`docs/API.md`. Supersedes `docs/TODO.md` **#00094**. Dominik, 2026-08-04.

**What shipped, against the phases below:** `DeleteImagesDialog.vue` (four scopes) + the plan/execute
split with `ImageFileActions`; `utils/imageDelete.ts` (intersection, `default`-last, surviving
new-active — 12 tests); `reset_image_analysis!` + `analysis_bytes_of` + `ANALYSIS_KEEP` in
`app/src/storage.jl` (27 assertions, keep-list pinned by name); routes
`/api/images/version/remove` + `/api/images/analysis/reset`; the `hidden` task-spec flag filtered in
`useTaskDefs`; the ViewerPanel label delete removed; and `analysisBytes` in the Settings storage box.

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
   - **Label sets** — a multi-select list (`/api/images/labels/delete` per set). That route now also
     sweeps the set's **companions**: the branch-label zarr (`branch_labels[vn]`, a separate registry
     that had no delete anywhere before) and every `labelProps/` sidecar derived from the name
     (`__tracks`, `__branch`, `.clustfeatures.json`). Prefix-driven, so a companion added later goes
     too. Deleting a segmentation used to leave those as files nothing could reach.
   - **All analysis ("the numbers")** — keep `ccid.json` + the image stores, drop everything derived.
3. **`importImages.remove` is UNLISTED from the UI, not deleted** (confirmed by Dominik, 2026-08-04,
   after the cost below surfaced). It stays registered and runnable — the duplicate *entry* goes, the
   task does not. `app/test/suite.jl` uses it as its real-task workhorse in ~15 testsets (chain end-to-end,
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
6. **Multi-image selection resolves names by UNION, skipping the images that lack the name**
   (Dominik, 2026-08-04, superseding the original intersection rule). Select three images where two
   carry `B` and one doesn't: `B` is offered, and applied to the two that have it. Intersection was
   worse than the problem it avoided — it made `B` undeletable until the selection was narrowed, and
   the silent-no-op worry it was guarding against is answered by badging each chip `k/n` when the name
   isn't on every image.

   Two consequences, both real bugs if missed: the **new-active version must be resolved per image**
   (the user's pick comes from the union, so it may not exist on a given image, and writing it into
   that image's `_active` would name a version that was never registered), and the **"becomes
   un-imported" warning counts images**, not the selection (taking `default` can strip one image of
   everything while another keeps its corrected version).

12. **A conflict blocks, a skip only warns** (Dominik, 2026-08-04). The test is whether the user's
   stated intent can be honoured at all:
   - **Blocks** (confirm greyed, `activeMismatches`): an image keeps a version but not the one chosen
     to stay active. Per-image substitution was the original behaviour and it is worse than a block —
     it reads as success while leaving that image on a version the user did not pick. An image that
     loses *every* version is explicitly NOT a conflict: there is no active to set, and the un-import
     is a legitimate outcome with its own warning.
   - **Warns only** (`partialNames`): a version or label set that isn't on every selected image. Applied
     where present, skipped elsewhere — that is the point of Decision 6, so blocking would undo it. The
     chip badge (`k/n`) plus a note carry it. Label sets never block for any reason.
7. **"All analysis" uses a KEEP-list, never a delete-list.** Everything under `1/{uid}` except the
   keep-list (`labels/ labelProps/ gating/ populations/ mesh/ branchLabels/ spatialGraph/ spatialStats/
   stats/ cl/ shapes/ out/ data/ qc/ tasks/ logs/`) is output. A delete-list silently leaks whatever
   analysis dir is added next. The keep-list is pinned by a package test that fails when a new sibling
   appears.
8. **The keep-list is `ccid.json` + `runlog.json`** (Dominik: keep the run log). Consequence, accepted
   deliberately: the image-table run tag then reflects **history, not current state** — an image whose
   outputs are gone still shows "last run: Cleanup · Cellpose correct". That is preferable to losing
   the record of what was done. `qc/` does **not** survive (my call, not Dominik's): its findings score
   outputs that no longer exist, so keeping them would assert a QC verdict about nothing.
9. **The analysis reset never touches image versions**, and the versions scope never touches analysis —
   two orthogonal scopes. Rationale (Dominik): *the derived version is the one you keep; once everything
   is derived you no longer need the raw and the intermediates.* So dropping the numbers must leave
   every store intact, and shedding stores is a separate, deliberate act.
10. **The versions scope pre-selects every NON-ACTIVE version**, with the new-active picker defaulting
   to the version that is already active. This follows from Decision 9's rationale: the common case is
   "I have my corrected image, drop the raw and the intermediates", which is exactly
   `reclaimable_versions` (`app/src/storage.jl`) — the modal is that same reclaim, scoped to the
   selection and reviewable, rather than whole-project and automatic. Nothing is deleted without the
   confirm, so a pre-selection is a suggestion, not an action.
11. **Multi-version removal orders `default` LAST.** Removing `default` while other versions remain is
   safe (`remove_image_version!`'s safe-primary rule only un-imports when nothing survives), so
   removing it first is fine — but removing the others first and `default` last, in a loop that ends up
   taking all of them, is what correctly un-imports the image at the end rather than mid-loop. Assert
   the ordering in a package test.

## Architecture

**New Julia core** — one function, the only genuinely new backend piece:

```julia
# app/src/storage.jl — beside remove_image_version!, sharing its lock discipline
reset_image_analysis!(img::CciaImage; on_log) -> (freedBytes, dropped::Vector{String})
```
`rm -r`s every child of `img._dir` except the keep-list — `ccid.json` + `runlog.json`, Decisions 7 and
8 — then in ONE `commit_state!` clears the analysis registrations (`labels`, `label_props`,
`branch_labels`) while leaving `filepath` **entirely alone** (Decision 9: no store is shed here), plus
`imChannelNames`, `meta`, `attr`, `included`/`note`/`starred` and `status`. Same discipline as
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

None outstanding — the three that were open (run-log survival, whether the reset sheds versions,
multi-version ordering) are Decisions 8, 9 and 11, answered by Dominik 2026-08-04.

## Reservations

- Phase 2 removes two working affordances. If the modal's version list is wrong, the fallback (the
  task, the viewer button) is gone — hence the ordering rule in Phase 2.
- Three of the four scopes are irreversible and now operate on a *selection*; the modal must state the
  count and what survives, per `docs/UI.md` → *File operations*.
