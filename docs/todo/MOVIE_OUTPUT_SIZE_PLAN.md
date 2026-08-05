# Movie output size: an explicit X/Y, not a magic multiplier

**Status:** planning, no branch. The **removal** of the old `res` control has shipped (see *What was
removed* below); this plan is the replacement, and nothing depends on it — a movie today comes out at
the napari canvas size, which is a defensible default.

## Goal

Let the user say how big the movie should be, in pixels, for the three surfaces that render one
(ViewerPanel timelapse, BatchMoviesPanel, AnimationModule). Two number fields, X and Y.

## Why the old control had to go first

`MovieOutputControls.vue` carried a `res` slider, 1–3×, tooltipped "Resolution supersample (2× =
double resolution)". It was not a supersample. napari-animation applies it like this
(`frame_sequence.py::iter_frames`):

```python
frame = state.render(viewer, canvas_only=canvas_only)
if scale_factor not in (None, 1):
    from scipy import ndimage as ndi
    frame = ndi.zoom(frame, (scale_factor, scale_factor, 1))
    frame = frame.astype(np.uint8)
```

The canvas is screenshotted at canvas size and then **interpolated up** with `scipy.ndimage.zoom`. So
`res=2` produced a file with 4× the pixels and **no additional detail** — a blurrier movie, plus a
zoom per frame and 4× the encode. The control could not do the thing its own tooltip claimed.

It was also never a deliberate offering: the viewer and batch panels had it, AnimationModule did not,
and unifying the three controls spread it to all three rather than asking whether it earned a place.

Two further reasons a multiplier is the wrong shape even when implemented correctly:

* **It is not reproducible.** The base is the live canvas, which depends on window size and monitor.
  The same "2×" gives a different movie on a laptop and a desktop, and the number recorded in the
  per-set prefs does not describe the output.
* **Publication figures are specified in absolute terms.** A journal asks for pixel or physical
  dimensions, never for a multiple of whatever window somebody had open.

## The mechanism that makes this real

**napari already supports it; napari-animation is what does not.**

```python
napari.Viewer.screenshot(path=None, *, size=None, scale=None, canvas_only=True, flash=False)
```

`size=(height, width)` re-renders the canvas at that size — a genuine render, not an upscale. But
napari-animation's `iter_frames` calls `state.render(viewer, canvas_only=...)` with no `size`, and
exposes only `scale_factor`. So the knob we want exists one layer below the one we call.

## Decisions

1. **Two integer fields, X and Y, in pixels.** Not a preset list ("1080p") — the canvas aspect is
   whatever the data and layout give, and a preset that does not match it either letterboxes or
   distorts. Show the current canvas size as the placeholder/default so the honest default is visible.
2. **Own the frame loop; do not pass `size` through napari-animation.** `animate()` would need
   `size` threaded through `iter_frames` → `state.render`, i.e. an upstream change or a monkeypatch.
   We already stream frames one at a time, and the loop is ~10 lines (`imageio.get_writer` +
   `viewer.screenshot(size=..., canvas_only=...)` per interpolated state). Replacing `animate()` with
   our own writer keeps the streaming property, removes the scipy zoom, and puts `size` where it
   belongs. Keep using napari-animation for **interpolation** (`Animation` / keyframes) — that is the
   part worth having.
3. **Empty = canvas size.** No size stored means "whatever the window is", which is today's
   behaviour and stays the default. The field is opt-in.
4. **Persist next to `fps`**, in the same per-set `movie` config and per-project animation state that
   `fps` already uses. Do NOT reintroduce the key `scale` — an older prefs file may still carry the
   removed one, and reusing the name would make a stale 1–3 multiplier read as a pixel width.
5. **Clamp, and say why.** A screenshot at a size the GL context cannot allocate fails at render
   time; cap at something defensible (e.g. 4096 per axis) and surface the clamp rather than letting
   ffmpeg receive a truncated frame. Also force **even** dimensions — libx264 rejects odd ones, which
   is a confusing ffmpeg error rather than an obvious validation message.
6. **The title card must follow.** `title_card.py::render_card_frame` takes explicit width/height and
   `prepend_title_to_movie` reads them from the recorded movie, so it inherits the new size for free —
   but the assertion belongs in a test, because a card sized to the old canvas would be silently
   rescaled by ffmpeg on concat.

## Phases

1. **Own the frame loop.** Replace `anim.animate(...)` in `record_timelapse` / `record_keyframes`
   (`python/cecelia/utils/napari_utils.py`) with a streaming writer over
   `FrameSequence.iter_frames`-equivalent states, still one frame at a time. No user-visible change,
   no `size` yet — this phase is a pure refactor and should leave byte-comparable output at canvas
   size. Test: frame count and per-frame shape unchanged.
2. **Thread `size` through.** `record_*(size=None)` → `viewer.screenshot(size=...)`; the two
   `napari_api.jl` routes and `run_batch_movies` read `sizeX`/`sizeY`; `sockets.jl` passes them.
   Clamp + even-dimension coercion in ONE helper, unit-tested, not at each call site.
3. **The control.** X/Y number fields in `MovieOutputControls.vue` (canvas size as placeholder), wired
   through the three surfaces and persisted beside `fps` in `settings.ts` / `animation.ts`.
4. **Title-card assertion.** A test that a recorded movie and its prepended card agree on dimensions.

## What was removed

Commit "Drop the movie resolution multiplier" deleted the `res` control and its 14 touchpoints:
`MovieOutputControls.vue` (the slider), its three renderers (`ViewerPanel.vue`,
`batchmovies/BatchMoviesPanel.vue`, `AnimationModule.vue`), both stores (`settings.ts` `movie.scale`,
`animation.ts` `scale`), both `napari_api.jl` routes plus `run_batch_movies`, `sockets.jl`, and the
`scale`/`scale_factor` parameters on `record_timelapse` / `record_keyframes`.

Persisted prefs written before that commit may still contain a `scale` key. It is unread, not
migrated — deliberately, since the value described a multiplier of a canvas that no longer bounds
anything.
