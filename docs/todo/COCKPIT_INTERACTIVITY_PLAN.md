# Correction cockpit — interactivity + follow-up correctness

> **SUPERSEDED 2026-09-10** by [`CORRECTION_DIRECT_PLAN.md`](CORRECTION_DIRECT_PLAN.md).
> The chip-strip / queue-and-apply direction is retired. Direct manipulation on the viewer
> (napari-paint parity + vizsla-parity track ops) with immediate autosave and a user-triggered
> recompute button replaces both this plan and the never-committed `CORRECTION_BRUSH_PLAN.md`.
> An intermediate 2026-09-08 rewrite of this file (vizsla-outline + NucleoSegment worklist over
> the same queue-and-apply substrate) was drafted but never committed — the queue itself is the
> problem, not what wraps it. Retained below as a historical record of the ranked-affordance
> analysis; nothing here should be built.

**Status:** planning (2026-09-08). Uncommitted scratch in the `feat/correction-cockpit-review-brush`
worktree; commit into the first-phase PR when work starts.
**Origin:** Dominik ran PRs #850 (obs carry-over), #852 (invalidation surface), #855 (Review pager
+ `label.split`) tonight and said the Labels + Review tabs feel *silent* — a count of picked labels,
no visible list, no per-focus feedback — compared to what he built himself pre-cecelia (napari-vizsla
and the older PyQt **NucleoSegment**). This plan is the follow-up: (a) the interactivity affordances
those two projects have and we don't, ranked by how much UX we get per line-of-code; (b) six
correctness findings surfaced by an adversarial review of the tonight-shipped code.

## Goal

Turn the cockpit from "silent count" into "you can SEE what you picked, which one you're on, what it
looks like across planes, and which one to review first" — matching the interactivity floor Dominik
already shipped in his own earlier surfaces. Fix the six correctness bugs at the same time.

---

## Locked design decisions

1. **`/Pick selection` stays the single source of truth for picked labels.** Both Labels and Review
   read it. The cockpit adds VISIBLE affordances around it (chip strip, outline, montage) rather
   than a second selection registry — anything else drifts.
2. **Review's focused label gets its own transient pop `/Review focus`** rather than sharing colour
   with `/Pick selection`. One pop per semantic role.
3. **The outline overlay renders on the browser viewer, not in the cockpit panel.** The panel shows
   the *list*; the viewer shows the *shape*. Same split NucleoSegment made
   (nuc_select.py → panel, DRAW canvas → shape).
4. **Every interactive affordance ships behind pure logic in `frontend/src/utils/*.ts`.** Cockpit
   `.vue` stays the reactivity layer, never the algorithm — matches the `reviewPager.ts` split
   already shipped.
5. **No new pop-map JSON shape.** `/Review focus` uses the existing transient-pop machinery
   (`_set_pick_selection!` sibling in `gating_api.jl`), so the frontend and the pop-manager reader
   don't need a schema change.
6. **Keybinds are opt-in and scoped to the cockpit panel** — document-level listener attached on
   mount, removed on unmount. Never global.
7. **No new op kinds.** The six affordances are viewer / cockpit / sort / display work. Split's
   raster brush and any new op kinds stay separately scoped in CORRECTION_PLAN.md.

---

## The interactivity gap — ranked

Rankings cross-check napari-vizsla (Dominik's tracks-editing plugin) and NucleoSegment (Dominik's
pre-cecelia nucleus corrector). "Both voted" = both plugins do this; strongest signal to port.

### Rank 1 — Chip strip of picked labels **(both voted)**

Both plugins let the user *see* what's picked and jump to any of them by clicking:
- Vizsla: napari's own selection state on the Shapes layer (free).
- NucleoSegment: 2 rows × N chip-buttons around the cursor with `nID + criterion value + mask
  thumbnail` (`nuc_select.py:333-349`).

Cecelia today: `pickedLabels.value.length` rendered as text — `3 labels picked @ frame 12`.

**Port seed:** in `CorrectionCockpit.vue`, render `pickedLabels.value` as a `ChipSelect` strip (the
existing frontend primitive). Click a chip → drop from pick (writes back to `/Pick selection`).
Hover a chip → flash it in the viewer (temporary colour bump on the pop, cheap CSS/state).

**Why highest:** smallest change, biggest silence-killer. No new backend, no new plot.

### Rank 2 — Per-object Details montage in Review

NucleoSegment: for the focused nucleus, a **grid of (lamin × labels × nuclei-mask) across every
z-plane, side by side** (`nuc_select.py:show_nucleus_planes:1755` → `frontend/figures/plot.py:160`).
The 3D shape becomes legible in one glance.

Cecelia today: focus card is text only (`Label 12 @ frame 3 / Centroid (…)`).

**Port seed:** new `frontend/src/components/correction/ReviewMontage.vue` — for the focused label,
fetch a bounding-box tile at ±k t (live imaging) or ±k z (3D) via the existing tile route
(`api/src/frame_overlays.jl` — check for a `/api/labels/bbox` or extend if needed), render as a
small grid using the primitives the CorrectionPlan cards already use.

**This is not new scope** — Phase 3 in `CORRECTION_PLAN.md` explicitly promises "per-object Details
montage". Owed functionality.

### Rank 3 — Pick outline overlay on the viewer (vizsla's biggest visual affordance)

Vizsla paints a **sharp red polygon on a dedicated overlay layer** for the clicked cell —
`skimage.measure.find_contours` on the label's bbox slice (`_widget.py:280-304`), cached per (label,
t) in `self._polys` so time-scrubbing is cheap.

Cecelia today: the `/Pick selection` pop is rendered as a colored point via the existing overlay
pipeline, but the *cell boundary* is not drawn — a user sees a dot, not "this cell".

**Port seed:** extend `frontend/src/utils/viewerOverlays.ts` with a `pickOutlineLayer` that pulls
the label mask for the picked ids at current t, rasterises the contour, and renders as a WebGPU
line strip. Cache `Map<label, Map<t, ContourPath>>` in the cockpit store, invalidate on Apply.

**Prereq for Rank 4** — the alpha-ramp needs the outline to ramp.

### Rank 4 — Alpha-ramp for temporal proximity (vizsla)

Vizsla ramps the outline opacity 1.0 at `t == label's home t`, 0.5 otherwise (`_widget.py:409-413`)
— as the user scrubs time, the outline fades away from its "home", so the user *sees* which t a
pick belongs to.

Cecelia today: nothing. A Review focus at t=12 looks identical whether scrolled to t=0 or t=12.

**Port seed:** wire an `alphaWeight = f(currentT, focusedT)` into the pick-outline layer (from #3).
Trivial once #3 exists.

### Rank 5 — Sort criterion + histogram

NucleoSegment: **QComboBox over `filter_criteria_nuclei` + a histogram of the current sort's
distribution** (`nuc_select.py:258, 315`). Sort by area, intensity, whatever — see where in the
tail you are.

Cecelia today: `sortLabels(labels, 'id-asc' | 'id-desc')` in `reviewPager.ts`.

**Port seed:** extend `reviewPager.ts` with `SortCriterion` union — id, area, mean_intensity_0,
live.cell.speed, area_ratio, etc. Values come from `/api/viewer/overlays.values` (already fetched
for Review mode). Render a tiny inline sparkline histogram via Observable Plot (project standard,
`docs/PLOTS.md`).

Turns Review from "walk in order" to "walk worst-first".

### Rank 6 — Colour picked labels by {sort criterion} on the viewer

NucleoSegment: **the whole image recolours by whatever param the pager sorts by** (`RBG_COLOUR:43`,
`get_draw_nuclei_cur_layer:970-999`, `coolwarm` for continuous). The viewer and the pager speak
the same language.

Cecelia today: viewer has `colourBy=…` on `/api/viewer/overlays`, but the cockpit doesn't drive it.

**Port seed:** one chip in Review — `Colour by: {criterion}` — writes the same param to the viewer
state via existing `useViewerStore.colourBy`. Pure wiring.

**Cross-vote with vizsla's alpha-ramp** — same "visually distinguish quality" intent.

### Rank 7 — Keyboard shortcuts **(both voted)**

Vizsla: `b` break, `l` link, `s`/`t`/`h` toggles (`_widget.py:97-101, 449-481`).
NucleoSegment: `.`/`,` next/prev, `M`/`/` shift, `X` keep-toggle, `V` save, `B` apply, `Q..F`
select-chip (`nuc_select.py:125-171`).

Cecelia today: none.

**Port seed:** document-level listener on `CorrectionCockpit.vue` mount:
- `,` / `.` — prev / next in Review
- `M` / `R` — Merge / Remove (Labels mode)
- `S` — Split ↔ on Review focus
- `Enter` — Apply queued ops
- `Backspace` — Undo last op

Cheap; punches above its weight for power users.

### Rank 8 (follow-up, not this plan) — RF-scored worklist

NucleoSegment's `classifier.py:113` trains an RF on prior corrections and backfills `nuc_proba`
per nucleus so the user can sort by "most-likely-bad first". This is the label analog of
`find_track_issues` — a `find_label_issues` route.

**Deferred:** own worktree, own plan. Bigger than an evening, needs a training story + a feature
set + a labelling loop. Note as **P4a-for-segmentation**.

---

## Correctness findings — six bugs the adversarial review surfaced

Fix these in the same follow-up sweep, ideally as an early phase so we don't ship interactivity on
top of broken foundations.

**F1 — Review drops picks on tab switch.** `CorrectionCockpit.vue:reloadPicked()` guards
`if (mode.value !== 'labels' … ) pickedLabels.value = []`. Switching Labels → Review clears the
pick set Review is supposed to page over. Fix: `mode.value === 'labels' || mode.value === 'review'`.

**F2 — Split runner drops multi-t ops when T isn't axis 0.** `correct_run.py:t_idx != 0` branch
reads `src[:]` once and applies only `ops_by_t.get(0, [])`. Ops at t≥1 are validated, journalled
with `perOpPixels[i]=0`, and silently lost. Fix: iterate t on the transposed axis, or refuse the
run with a clear error when the labels store isn't (T,Z,Y,X). Add a test for a `(Z,T,Y,X)` store.

**F3 — `labels_before/after` miscounted when T isn't axis 0.** Same branch as F2 — one whole-array
read, early break, so `nLabelsAfter` under-reports impact. Fix falls out of F2.

**F4 — First Review focus race.** `watch(reviewFocused, target => …)` fires focusReviewLabel on
mount before the viewer publishes viewState. `current` is falsy → early return; no re-fire when
viewer catches up. The first pager position never lights up until the user hits Next. Fix: watch
`viewerStore.viewState` too, or defer initial focus until `viewState` is present.

**F5 — Dead `keep` in `_apply_split_inplace`.** `keep = (labeled == largest) | cut & mask` is
computed and discarded (`_ = keep`). The comment says "cut pixels return to the largest fragment"
but no assignment enforces it. Today cut pixels remain `id_` by accident (never overwritten); a
future op re-ordering could silently orphan them. Fix: either assign explicitly
(`frame2d[cut & mask] = id_`) or delete the dead code and remove the misleading comment.

**F6 — carry-over `_is_categorical` breaks on nullable Int64 / bool / string dtype.** A boolean
gating obs (`pop.gate`) or a nullable `track_id` (`pd.Int64Dtype`) hits `_serialise_numeric` →
`np.asarray(series, dtype=float)`. Nullable Int64 with `pd.NA` raises; bool silently casts to
float and breaks downstream `.astype(bool)` predicates. Fix: extend `_is_categorical` to catch
`pd.BooleanDtype`, `pd.Int64Dtype`, `pd.StringDtype`; add tests for each.

---

## Phases

Sizes are wall-clock estimates for one focused session. Every phase ships its own PR — the
correctness half is separable from the interactivity half.

### Phase 0 — Correctness sweep (F1–F6). **~half a day.** — **BUILT, PR pending review** (2026-09-08)
One PR fixing all six findings + tests. No feature work. Ship first so interactivity lands on
solid foundations.

Landed on `feat/correction-correctness-sweep`:
- **F1**: `reloadPicked` guard extended to `mode === 'labels' || mode === 'review'` so Review reads
  the same `/Pick selection` set Labels does. No test — pure guard fix on cockpit reactive path.
- **F2+F3**: Correction runner refuses labels stores where T is not on axis 0 (the only layout the
  frame loop can safely iterate). Foreign-pipeline (Z,T,Y,X) stores raise a clear
  `RuntimeError` instead of silently truncating ops at t≥1. Unit tests: `_t_axis` returns 0 for
  canonical (T,C,Z,Y,X), 1 for (Z,T,Y,X) (guard-triggering), and None for still images.
- **F4**: Added a `viewerStore.viewState` null→present watcher that retries the initial fly-to when
  the viewer publishes state after the cockpit mounts. Subsequent viewState changes never re-fly
  (would yank the user away mid-pan).
- **F5**: Explicitly re-stamps cut pixels to `id_` in `_apply_split_inplace` instead of relying on
  the accidental invariant that they were never zeroed. Test pins that the cut row's inner pixels
  read as `id_` after a horizontal split.
- **F6**: Extended `_is_categorical` for `pd.StringDtype`; extended `_serialise_numeric` to use
  `.to_numpy(dtype=float, na_value=np.nan)` on extension numeric dtypes (`pd.Int64Dtype`,
  `pd.BooleanDtype`, `pd.Float64Dtype`) so nullable columns with `pd.NA` no longer crash the
  snapshotter. Test covers all three dtype classes with `pd.NA` present.

Verified: `pixi run test-pkg` (7222 pass), `pixi run test-py` (1072 pass), `pixi run test-frontend`
(3156 pass), `npm run typecheck` clean.

### Phase A — Chip strip + focus pop **(Ranks 1 + partial 3). ~half a day.**
- Chip strip of `pickedLabels` in `CorrectionCockpit.vue`.
- `/Review focus` transient pop written on `reviewIndex` change (Julia gating_api.jl mirrors
  `_set_pick_selection!`).
- Hover-chip → flash pop colour intensity.

Ships the "you can see what you picked" fix — the single biggest complaint.

### Phase B — Details montage **(Rank 2). ~1 day.**
- `ReviewMontage.vue` — per-focus grid of label crops over ±k t (live) or ±k z (3D).
- New route `/api/labels/bbox?…&t=&label=` if the tile route doesn't already handle labels — check
  first.

Delivers the plan doc's own Phase-3 promise.

### Phase C — Pick outline overlay + alpha-ramp **(Ranks 3 + 4). ~1–1.5 days.**
- `pickOutlineLayer` in `viewerOverlays.ts` — rasterise contour on client from mask bbox.
- Contour cache keyed by (label, t).
- Alpha-weight = `f(currentT, focusedT)`.

Requires understanding the browser viewer's overlay pipeline; scope with a spike first.

### Phase D — Sort + histogram + colour-by-criterion **(Ranks 5 + 6). ~1 day.**
- `SortCriterion` union in `reviewPager.ts`; values from `/api/viewer/overlays`.
- Inline sparkline histogram of current sort's distribution.
- One chip drives `useViewerStore.colourBy` to match.

### Phase E — Keybinds **(Rank 7). ~2 hours.**
- Document-level listener on `CorrectionCockpit.vue` mount.
- `,`/`.`, `M`/`R`/`S`, `Enter`, `Backspace`.
- Show a `?` tooltip listing all binds when panel focused.

---

## Non-goals / do-not-port

Both source projects have shapes that would REGRESS cecelia if adopted:

- **Six-pickle correction persistence** (NucleoSegment `correction.py:283-350`). Cecelia's queued-
  ops + append-only journal + one Apply is strictly stronger.
- **In-place graph mutation with no journal** (vizsla `_widget.py:507, 549`). Same reason.
- **Global widget-state bag** (NucleoSegment `storage.config`). Cecelia's settings store + view
  state is the right shape.
- **Napari-native idioms** — top-layer pinning (`_widget.py:626-635`), on_layer_inserted combo
  swap (`:608-623`), one-fat-Shapes-layer + `features` DataFrame per shape (`:86-96`). None map
  onto WebGPU cleanly.
- **Five-tab dialog with per-tab independent state** (NucleoSegment). Cecelia's three modes
  sharing `/Pick selection` is the better shape.
- **RF-scored worklist** — deferred to a separate plan (see Rank 8).
- **Split's raster brush** (Draw / Erase / Fill / Pick) — deferred to its own worktree per
  CORRECTION_PLAN.md P4 note. Not in scope here.

---

## References

- `docs/todo/CORRECTION_PLAN.md` — the parent plan; this is a follow-up scoped to interactivity
  + correctness only, no new op kinds.
- napari-vizsla — https://github.com/tlnagy/napari-vizsla (`_widget.py` at 684 LOC, review dated
  2026-09-08).
- NucleoSegment — Dominik's pre-cecelia PyQt project at
  `/media/dominik/QUACK2/LONDON/LAPTOP_FILES/PycharmProjects/NucleoSegment` (~14k LOC; the review
  focused on `frontend/gui/nuc_select.py` + `processing/correction.py`).
- Current cockpit code — `frontend/src/components/correction/CorrectionCockpit.vue`,
  `frontend/src/lib/labelCorrection.ts`, `frontend/src/utils/reviewPager.ts`,
  `app/src/label_correction.jl`, `app/src/tasks/segment/correct_run.py`,
  `app/src/tasks/segment/carry_over_run.py`.
