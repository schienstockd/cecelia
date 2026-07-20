# Storage box + reclaim originals (Settings)

**Status:** built · **Branch:** `feat/storage-reclaim`

## Problem

Users were told by hand: once an image is drift/AF-corrected, delete the original imported OME-ZARR to
free space. Nothing surfaced how much space that would reclaim, or did it safely. A small Settings box
should show disk usage + the reclaimable originals and free them in one click.

## Locked decisions

- **Reclaimable = every image version EXCEPT the active one** (`reclaimable_versions`). Keep only
  what you're using: the original `default` import AND any superseded intermediates (e.g. with
  `cpCorrected` active, `default` + `afCorrected` are both freeable). Started as default-only, then
  generalised (Dom, 2026-07-20): "really you can remove every image except the active one." Channel
  names/dims (fallback to `default`) and all derived labels/measurements/gating live in `1/`, so
  dropping non-active image stores is safe. Freeing `default` is irreversible (re-import) — accepted.
- **On-demand scan.** Walking every image store (`_dir_bytes`) is expensive at TB scale, so it is NOT
  run on Settings open — a "Scan storage" button triggers it. `diskstat` (disk free/total) is cheap
  and returned by the same call.
- **One-click reclaim** with a confirm, reusing the shared removal path.

## Architecture

- `app/src/storage.jl` — the shared core:
  - `reclaimable_versions(fp)` — pure policy over the ccid `filepath` dict: all versions except
    `_active` (unit-tested).
  - `image_storage(img)` / `project_storage_summary(proj)` — walked sizes + per-image reclaimable
    version list + disk.
  - `remove_image_version!(img, value, new_default)` — the ONE deletion path, extracted from the
    `RemoveImage` task so the task and the reclaim API share it. Returns `nothing` on a missing
    version (a failure the caller propagates — the chain fault-isolation relies on this).
  - `reclaim_inactive!(proj, uids)` — free every non-active version of each image, keeping the active
    one; skips images with nothing to reclaim.
- **Safe-primary fix** (the crux): `remove_image_version!` only "un-imports" (clears `imChannelNames`
  + `SizeC/T/Z`, `status="pending"`) when the primary is removed AND no other version survives.
  Previously removing `default` ALWAYS wiped — which would break a still-active corrected variant that
  inherits channel names/dims from `default` via versioned fallback. Now reclaiming the original keeps
  the corrected variant fully working.
- `api/src/storage_api.jl` — `GET /api/storage/summary?projectUid`, `POST /api/storage/reclaim`
  `{projectUid, imageUids}`. Wired in `server.jl`.
- Frontend: `frontend/src/utils/storage.ts` (`formatBytes` + REST wrappers, unit-tested) + a Storage
  section in `SettingsModule.vue` (scan → disk/project/reclaimable list → confirm → "Free up space").

## Tests

- `app/test/runtests.jl` "Storage reclaim": `reclaimable_versions` policy (all-except-active, incl.
  leftover variant when active is `default`); safe-primary rule; `reclaim_inactive!` frees all
  non-active versions and keeps names/dims/status + the active store.
- `api/test/runtests.jl` "API: storage": param validation (400s).
- `frontend/src/utils/storage.test.ts`: `formatBytes` golden values.

## Reservations / follow-ups

- **Behaviour change to `RemoveImage`:** removing `default` while other versions exist no longer
  un-imports the image (it did before). This is a fix, but it IS a semantics change for that task's
  UI path.
- **Reclaim is a synchronous API** (not a scheduler task): a very large delete blocks that request.
  Fine for a settings action with a spinner; revisit if it feels slow.
- **Scan cost:** `project_storage_summary` walks every version of every image; on a huge project the
  scan takes a few seconds. On-demand (button) keeps it off the Settings-open path.
- Not yet exercised in a running browser/app — logic is unit-tested across all four layers.
