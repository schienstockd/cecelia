> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

> **Outcome (2026-08-24).** Shipped, with three departures the brief could not have known about.
> (1) **`sequence` is a role, not a modifier.** A `VisCell` carries scalars, and no existing shape
> function draws a field of values, so "call the existing role's shape function once per frame" had
> nothing to call. It landed as `grid` — one new shape, and the only one in the figure that is a band
> rather than an inline 16px glyph. (2) **The float was extracted first.** The button and panel lived
> inside `ParamRenderer`'s repeatable-group branch and smoothing has no group; `ParamFigure.vue` now
> mounts anywhere, and a param names its figure in the task JSON (`"figure": "smoothMethod"`).
> (3) **The `result` row animates too**, rather than showing one smoothed frame — same role, no extra
> machinery, and the median's smearing is only legible while the spot is moving.
>
> Two of the brief's asks were dropped on evidence. The **cost row** is computed from the user's own
> `z x t x channels` at the measured 0.12 s/plane, not #554's "~7 min/channel" — that figure is a
> property of one acquisition, which is the same objection the brief itself raised against the
> amplitude chart. The **note** the brief asked for is gone — its condition (a high zero-voxel
> fraction) is computed by the runner and does not exist before the run, and what it would have said,
> that `gated` is unproven on photon-limited data, contradicted the option help beside it. A different
> line took the slot: the figure states WHICH ONE TO PICK at the user's current settings, because at
> the default window the two grids agree and that agreement is the answer only to a reader who already
> knows it is. Measurement replaced the rest: see `docs/todo/SMOOTHING_PLAN.md` → *2026-08-24*.

# Extend VisualAid with an animated row; ship it as smoothing's median vs gated aid

`cleanupImages.smooth` offers `temporalStat: "median" | "mean" | "gated"`
(#554) but nothing shows what the choice costs before you run it. Segmentation
already has a figure for this shape of problem — `VisualAid` (#629) draws one
column per option, each row typed by a role (`diameter`, `blur`, `distance`,
`area`, `fraction`, `text`). Reuse that component for smoothing; don't build a
second float. But its rows are all instantaneous values — a diameter is a
diameter forever — and what makes median vs gated legible is watching a bright
spot move through a noisy window and seeing which output keeps it. A static
circle can't show that, so this is two changes landing together: `VisualAid`
gains the row type that can animate, and smoothing is its first consumer.

## Part 1 — the new role: `sequence`

A `sequence` row supplies N small frames (a tiny 2D value grid, or a
pre-rendered shape per frame — whichever `VisColumns` already uses for its
existing per-row payload) and plays them in a loop. Same rule as every other
role: `VisualAid` doesn't know it's frames of a movie versus frames of
anything else — it cycles an array on a timer and draws whichever shape
function the row's *other* role would already use for a single frame. So
`sequence` is a modifier — `{ role: "diameter", sequence: Frame[] }` — not a
sixth independent shape. Don't re-derive circle/ring/span/disc/track drawing
a second time inside a separate sequence branch; call the existing role's
shape function once per frame, swapped on an interval.

Constraints, matching the rest of #629:

- **Respect `prefers-reduced-motion`.** Everything else in this float is a
  still image; this is the first thing in it that moves on its own. Gate the
  interval on the media query, same as any other animation in the app.
- **Pause when the panel isn't visible.** `FloatingPanel` is off by default
  and closeable — don't run a `setInterval` for a float nobody has open.
  Start the loop on open, clear it on close.
- **No fake controls.** #629 already learned this lesson once (the
  rail-plus-handle that looked draggable but wasn't). If there's no
  play/pause affordance, the frame counter shouldn't look clickable either —
  a caption, not a control.
- **Geometry stays pure and tested.** What each frame's grid looks like is
  computed the same way every other row's value is computed: in the task's
  `paramVis`-equivalent, tested without mounting anything. `VisualAid` only
  owns the interval and the draw call.

## Part 2 — first consumer: median vs gated

Two columns, floating in the smoothing group's header (same placement pattern
as segmentation: off by default, behind a button, not inlined above the
form).

| row | role | value |
|---|---|---|
| motion | `sequence` (small grid per frame) | the moving-spot-plus-noise sequence, computed once and shared by both columns — same input, so the comparison is fair. Sits above the two columns below it, not duplicated per column. |
| method | `text` | `"Median"` / `"Gated"` |
| window | `distance` | current `temporalFrames`, same value both columns — the comparison is the method, not the window |
| cost | `text` | `"~free"` / `"~7 min / channel"` (measured, #554) |
| result | whichever role fits a single smoothed frame | median output / gated output at the guide timepoint, recomputed live from the shared motion sequence |

Do not invent a `diameter` or `area` row for the gate's internal weighting —
`noise_sigma` is deliberately not a user-facing knob (auto-estimated once per
acquisition, #554). A figure implying a tunable knob where none exists is
worse than no figure.

**The note.** Segmentation's `InlineNote` fires when growth settings match
across passes — the thing worth catching before a long run rather than after.
Here: `gated` is only measured on one 30 s intravital regime and is untested
on photon-starved data (86–95% zero voxels) — the regime this task was
originally built for. Fire the note whenever `temporalStat === "gated"` and
the active dataset's zero-voxel fraction is high (reuse whatever the task
already computes for the photon-limited path; a static warning is fine as a
first pass otherwise). Same severity tier as the growth-settings note — not
an error, `gated` isn't wrong here, just unproven.

## Explicitly not in scope

- No scrubber/seek control on the sequence — loop only.
- No amplitude/sharpness bar chart pulling #554's fixed measured numbers into
  the UI — those are one dataset's results, not a property of the method.
  If a live per-run amplitude-kept metric gets surfaced later, that's a
  `fraction` row added then, not blocking this one.
- No reuse of `sequence` for segmentation's existing rows yet. If a future
  segmentation param benefits from it (e.g. a pass growing over iterations),
  that's a separate call once this lands and is proven out here.
- No change to the `median` default.
