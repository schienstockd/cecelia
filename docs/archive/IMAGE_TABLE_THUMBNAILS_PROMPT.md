> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. The
> thumbnail column as built is `frontend/src/components/ImageTable.vue` → *Row thumbnails*,
> `frontend/src/utils/imageThumbs.ts` and `api/src/thumbnails_api.jl` (#2312, #2352).
>
> **Outcome (2026-11-25):** partly built, in the PR that archived this brief. **Shipped:** item 1
> (a fixed 80 px square slot on the media ground with a border — `components/ThumbSlot.vue`; 80, not
> 56, to keep the size the column already had), item 3 (hover preview, through the app's `v-tooltip`:
> a 320 px render, name, channels, Z/T — it opens at once like every tooltip, not after 300 ms, and not
> on keyboard focus, since `v-tooltip` doesn't), item 7 (loading and missing fill the same slot), and
> the lazy-loading constraint (rows fetch as they near the viewport). **Not built, deliberately** —
> four premises did not hold against `origin/main` 3912568b:
>
> 1. **The per-row icon button is not a duplicate of the thumbnail click.** It toggles that row's
>    thumbnail on and off (a per-row set in `profileStorage`, `cc.imageThumbs:<project>`); clicking
>    the thumbnail already opens the viewer. Removing it (item 2) and the Off/Small/Medium cycle (item 4)
>    would drop per-row choice, not noise — kept.
> 2. **Colour differences are by design, not inconsistency.** The table thumbnail is drawn the way the
>    image's viewer shows it (saved colours + contrast, else per-channel percentile), so one green row
>    means that image's viewer is set green. A same-rule render already exists, server-side only, for
>    the set montage (`contrast=shared`: pooled per channel NAME over the set). Item 6 would need a
>    backend change and set-level pooling, and it conflicts with item 5's dots "in the colours the
>    viewer uses" — a choice between faithful and comparable rows, not made.
> 3. **Item 5's channel dots have no data source.** `GET /api/images/thumbnail` returns only
>    `{png, width, height}` — no channel list or colours.
> 4. **The column hide is being built elsewhere.** `feat/image-table-columns` adds a Columns popover
>    that can hide the `thumb` column — item 8's Off state.
>
> The "working mock" in the originating chat is not in the repo. Before/after screenshots in a PR would
> be lab data, so the PR has none.

# IMAGE_TABLE_THUMBNAILS_PROMPT

## Goal

Redesign how thumbnails appear in the image table so the table is cleaner and clearer. Implement it, test it, and open a PR.

## Problem (current state)

- Thumbnails vary in size and aspect ratio, so rows are ragged.
- Each row repeats the same icon button beside its thumbnail. It adds noise and no information.
- Black image backgrounds bleed into the table and blur row edges.
- Thumbnails differ in colour (e.g. one green, the rest red) with no hint why.

## Target design

1. **Fixed thumbnail slot.** Every row uses the same square box. Medium is 56px, Small is 40px. Image uses `object-fit: contain`, centred. Box has a uniform dark background, a 1px border and a small radius (4px).
2. **Thumbnail is the click target.** Remove the per-row icon button. Pointer cursor, plus a subtle expand overlay on hover. Click keeps whatever the old icon button did (open viewer / image).
3. **Hover preview.** After ~300 ms hover, show a popover with a larger render (~240-320px), image name, dimensions and channel count. Hide on mouse leave. Don't let it trap pointer events.
4. **Single cycle toggle in the header (decided).** One button replaces the header icon. Each click cycles **Off → Small → Medium → Off**. The button shows the current level: a small photo icon, three level bars (filled up to the current level, none filled for Off) and a text label (Off / Small / Medium). No Large size, no menu. Persist the choice across sessions (follow however the app persists other table preferences).
5. **Channel hint.** Tiny corner badge on each thumbnail: one coloured dot per composited channel, using the channel colours the viewer uses.
6. **Consistent rendering.** All thumbnails use the same auto-contrast and composite rule, so colour differences reflect the data.
7. **Loading state.** A skeleton of identical size while a thumbnail loads, so layout doesn't jump. A missing thumbnail shows a neutral placeholder in the same slot.
8. **Off state.** The thumbnail column collapses entirely and the table gets denser.

A working mock exists in the chat that produced this prompt. Treat it as a behaviour reference, not code to copy.

## Method

1. Find the image table component and its thumbnail rendering, the header icon, and where thumbnails are generated and cached. Read how the table persists other UI preferences.
2. Before changing anything, write a short plan: files to touch, whether thumbnail generation needs changes (e.g. consistent contrast, channel list), and risks.
3. Implement in small commits. Prefer CSS and component changes. Only change backend thumbnail generation if consistent rendering (item 6) can't be done client-side.
4. Follow existing conventions: styling tokens, component patterns, i18n/label handling, accessibility.

## Constraints

- Keep the table fast with many rows: lazy-load thumbnails, no layout shift.
- Keyboard accessible: the toggle and the thumbnail click target are focusable with visible focus; hover preview also opens on focus.
- Works in light and dark mode.
- Don't change table columns, sorting or selection behaviour.
- Don't add a new dependency.

## Tests

- Toggle cycles Off → Small → Medium → Off and the label and bars update.
- Choice persists across reload.
- Slot size is identical for portrait, landscape and very wide images.
- Off state removes the column and its space.
- Existing image table tests still pass; add tests proportionate to the change (see TEST_VALUE_AUDIT guidance: no tests that only restate the implementation).

## Output

- Branch and PR with a short description, before/after screenshots (Off, Small, Medium, hover preview) and any open questions.
- If a decision above turns out to be wrong for the codebase, say so in the PR rather than working around it silently.
