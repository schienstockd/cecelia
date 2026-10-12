# Mask ↔ channels decoupling in the viewer

Status: **P1–P4 built** on `fix/mask-decouple` (2026-10-12); awaits a visual pass in the browser. Durable summary: `docs/ARCHITECTURE.md` → *Mask vs channels in the viewer*.

## Goal

Toggling or switching the segmentation mask in the viewer must not throw away the image channels
already on the GPU. Today, after playing a movie through with mask + channels resident, unticking the
segmentation drops every resident frame and the next scrub reloads every channel.

## Why it happens (as of `origin/main` a60dfd67)

1. **Every mask change is a full reallocate.** `ViewerWindow.vue` watches `labelName`,
   `previewLabelsKey` and `liveLabelsKey` and calls `reallocate()` → `renderer.setImage(...)`, which
   drops the whole cache (brick: `dropAtlas`; flat: `dropAll`). A live run's progress re-stamps did the
   same on every tick.
2. **Brick renderer: mask allocation is a `setImage` argument** (`withLabels`), and `setBrickSource`
   treats any label-field change as a full source switch (`pageTable.clear()`).
3. **Brick renderer: mask bytes only arrive with their intensity twin** (`kickLabelFetch` fires from
   the intensity landing), so there is no way to load the mask onto already-resident bricks.
4. **Flat renderer: the mask is budgeted and allocated per timepoint slot at `setImage`**, and fetched
   in the same job as the channels.

The 2D tile path already does it right: `tileRenderer` stamps each slot's mask layer with a source id
(`setLabelSource` / `needsLabel` / `uploadLabel`) and refetches the mask alone. This plan copies that
shape into the other two renderers.

## Decisions (2026-10-12)

1. **One mask-source id for all three paths** — `tileMaskSourceId` (`utils/tileViewer.ts`), computed
   with `hidden: false` for the flat/brick paths (hiding there stays an opacity-0 redraw). A change of
   that id is a *mask* change, never a reallocate (except Decision 4's one case).
2. **Per-slot mask stamp.** Each renderer slot records which mask source (flat: per timepoint slot) or
   which brick key under the current source (brick: per atlas slot) its mask bytes belong to. A
   mismatch means "draw this slot without a mask and fetch the mask alone".
3. **Brick: a mask-source change replaces only the label textures.** Turning the mask off destroys the
   label atlases and binds the placeholder; turning it on or switching allocates fresh (zeroed) label
   atlases — zeroed so a brick whose new mask has not landed draws no outline rather than the old
   mask's. The intensity atlas, page table and residency are untouched. No shader change.
4. **Flat: reserve mask room when the image has any segmentation.** The flat cache sizes its capacity
   from the per-timepoint cost, which includes the mask. Budgeting for it whenever `meta.labelNames`
   is non-empty (or a preview/live mask is showing) means toggling never changes capacity. The one
   remaining reallocate: a preview or live mask appearing on an image that had no reservation.
5. **Backfill order.** Brick: bricks at the displayed timepoint first, then every other resident
   brick by distance from it, a few label fetches at a time. Flat: the prefetch walk treats a resident
   frame with a stale mask as "needs work" and fetches the mask alone.
6. **A mask fetch that fails is not retried for that source** (per brick / per frame), same as
   `tileLabelFailed` — otherwise an unservable mask (fewer levels, 409) refetches on every tick.

## Phases

- **P1 — brick renderer**: `setBrickSource` splits image identity from mask identity; label textures
  managed independently; per-slot `labelOf`; tick-driven label backfill. Unit-test the pure pieces.
- **P2 — flat renderer**: per-slot `labelSrc`; `setLabelSource`, `needsLabel`, `uploadLabel`; mask
  reservation decoupled from the current mask.
- **P3 — ViewerWindow**: one watcher on the mask-source id replaces the three reallocating watchers;
  flat `fetchTimepoint` grows a mask-only branch.
- **P4 — docs**: fold the durable parts into the viewer docs; mark this plan built.

## Known limits (not in scope)

- A brick slot reused by a different brick keeps the previous brick's mask bytes until the new
  brick's mask lands (pre-existing; same source only — a source change starts zeroed).
- The brick label atlas is still outside the atlas VRAM budget (pre-existing).
