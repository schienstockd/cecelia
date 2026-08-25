"""One writer and one size policy for every `.mp4` Cecelia produces.

Two paths write movie frames — the napari recorders (`napari_utils.record_timelapse`/`record_keyframes`)
and the title-card prepend (`title_card.prepend_title_to_movie`) — and they must agree on the encode,
because the card is concatenated onto the recording. They did not: the recorders went through
napari-animation's `animate()`, which takes imageio's defaults (quality 5, and **macro_block_size 16**,
which silently rescales every frame up to the next multiple of 16), while the card wrote
libx264/yuv420p/quality 8/macro_block_size 1. So a movie "at the canvas size" was never quite the canvas
size, and the two halves of the same file were encoded differently.

`movie_writer` is now the only writer, so the encode is one decision. `coerce_movie_size` is the only
place a requested output size is validated. Full rationale: docs/NAPARI.md → *Movie output size*.

`stitch_movies` is the third producer: it composes several finished recordings into one frame
(side-by-side version comparison — docs/todo/MOVIE_COMPARE_PLAN.md). It lives here, and not in
`napari_utils`, because it needs no viewer at all — and because a composed file must encode exactly
like the recordings it is made of.
"""
import contextlib
import os

# Per-axis ceiling for a requested movie size. A canvas screenshot renders through the GL context, so a
# size the driver cannot allocate is not a clean failure — on some drivers it comes back as a blank
# frame, which would encode happily into a black movie. Render cost also scales with pixel count, so a
# few thousand frames at 8K is a hang, not a figure. 4096 is comfortably inside every GL max-viewport
# we would ship against.
MAX_MOVIE_AXIS = 4096

# libx264 with yuv420p subsamples chroma 2×2, so odd dimensions are rejected outright — and because we
# write with macro_block_size=1 (exact frames, no silent rescale), imageio will not paper over it.
_EVEN = 2


class RecordCancelled(Exception):
    """A recording or a stitch stopped because the user cancelled it. Carries the frames written
    before the stop.

    An exception rather than a return value so no caller can mistake a cancelled render for a
    finished one — the title card must not be prepended, and the staged file must not be promoted.

    Defined here rather than in `napari_utils` (which re-exports it, and where it used to live)
    because both frame loops raise it and `napari_utils` imports THIS module, not the other way
    round. One exception type for "the user stopped it", whichever loop was running."""

    def __init__(self, frames):
        super().__init__(f"recording cancelled after {frames} frame(s)")
        self.frames = frames


def coerce_movie_size(size):
    """Validate a requested movie size. Returns ``((height, width), notes)``, or ``(None, notes)``.

    ``size`` is ``(height, width)`` — **napari's** order, since it goes straight to
    ``Viewer.screenshot(size=…)``. The UI and the Julia routes speak X/Y (width/height) and swap at the
    bridge boundary; do the swap once, there.

    ``None`` (or a zero/blank axis) means "the napari canvas size", which is the default and what every
    movie was before this existed.

    ``notes`` are human-readable strings for the log — a clamp or an odd-axis fix must be VISIBLE, not
    silent, or the user believes they got the size they typed.
    """
    notes = []
    if size is None:
        return None, notes
    try:
        h, w = (int(v) if v is not None else 0 for v in size)
    except (TypeError, ValueError):
        return None, [f"movie size {size!r} is not a pair of numbers — using the canvas size"]
    if h <= 0 or w <= 0:
        return None, notes                      # blank/zero = "canvas size", not an error

    out = []
    for axis, v in (("height", h), ("width", w)):
        if v > MAX_MOVIE_AXIS:
            notes.append(f"movie {axis} {v} exceeds {MAX_MOVIE_AXIS} px — clamped to {MAX_MOVIE_AXIS}")
            v = MAX_MOVIE_AXIS
        if v % _EVEN:
            notes.append(f"movie {axis} {v} is odd — using {v - 1} (h.264 needs even dimensions)")
            v -= 1
        out.append(max(_EVEN, v))
    return (out[0], out[1]), notes


def size_from_xy(x, y):
    """``(height, width)`` for a requested X/Y (width/height) pair, or None if either is blank.

    The axis-order boundary, kept separate from `coerce_movie_size` (which validates): X/Y is how the UI,
    the Julia routes and the bridge command speak, ``(height, width)`` is how napari does. Flip once,
    here, so no other layer has to remember which way round it is.
    """
    try:
        w = int(x) if x not in (None, "") else 0
        h = int(y) if y not in (None, "") else 0
    except (TypeError, ValueError):
        return None
    return (h, w) if h > 0 and w > 0 else None


def crop_to_even(frame):
    """Drop a trailing row/column so the frame has even dimensions. A cheap view, no copy.

    Needed even when the request was even: napari divides the requested size by the display's
    ``devicePixelRatio`` and truncates to int before resizing the canvas, so on a HiDPI screen the
    frame that comes back can be a pixel off the request. The writer's frame size is taken from the
    first frame, so every frame gets the same treatment and they stay consistent.
    """
    h, w = frame.shape[:2]
    return frame[: h - h % _EVEN, : w - w % _EVEN]


@contextlib.contextmanager
def movie_writer(path, fps):
    """The one imageio writer for a Cecelia `.mp4`. Use as a context manager.

    ``macro_block_size=1`` keeps frames at their exact dimensions — imageio's default of 16 rescales
    anything not divisible by 16, which is why "canvas size" used to be approximate. That is why
    `coerce_movie_size`/`crop_to_even` own the even-dimension rule instead: nothing downstream will fix
    an odd frame for us.

    **It is a context manager of our own, rather than the raw writer, so the file is closed on the way
    out of a FAILED block too.** imageio's ``__exit__`` closes only when the block exits cleanly
    (``if value is None: self.close()``), so a cancel or an error left the writer open; ffmpeg then
    finalised the staged ``.tmp.mp4`` moments later — *after* the caller's cleanup had already looked
    for it and found nothing. Every cancelled render therefore leaked a staged temp that only
    ``napari_utils._clear_stale_staging`` would collect, an hour later. Closing here makes the staged
    file real before the caller's ``except`` runs, which is what makes "a cancel leaves nothing
    behind" true rather than aspirational.
    """
    import imageio.v2 as imageio   # local: keeps the import off any path that only needs the helpers

    writer = imageio.get_writer(str(path), fps=float(fps), codec="libx264", quality=8,
                                macro_block_size=1, pixelformat="yuv420p")
    try:
        yield writer
    finally:
        writer.close()


def encode_raw_frames(raw_path, out_path, *, width, height, frames, fps, log=None):
    """Encode a file of raw RGB24 frames to an mp4 through :func:`movie_writer`.

    The frames come from Julia's compositor (``api/src/movie_render.jl``, renderer C) — the offline
    half of the browser-viewer split. They arrive as ONE file of ``width * height * 3`` bytes per
    frame, top row first, rather than as a PNG each: a PNG per frame pays an encode and a decode for
    bytes that were already in memory, and PNG encoding is half of that renderer's warm frame.

    Reads one frame at a time. ``np.fromfile`` on the whole file would hold the entire movie in memory
    (~600 MB for a 181-frame recording of a real timecourse) to hand the writer one frame at a time
    anyway.
    """
    import numpy as np
    import os

    expect = int(width) * int(height) * 3
    n = int(frames)
    have = os.path.getsize(raw_path)
    if have < expect * n:
        # Checked BEFORE the writer opens, so a truncated render never creates a partial mp4 that
        # looks like a finished one. The per-frame check below stays as well: this one cannot tell a
        # short file from a wrong `width`.
        raise ValueError(
            f'{raw_path} holds {have} bytes, expected {expect * n} for {n} frames of '
            f'{width}x{height} RGB24 — the render was truncated')
    with open(raw_path, 'rb') as fh, movie_writer(out_path, fps) as writer:
        for i in range(n):
            buf = fh.read(expect)
            if len(buf) != expect:
                # A short read is a truncated render, and encoding what arrived would produce a movie
                # that looks complete and is not. Fail with the frame number, which says how far it got.
                raise ValueError(
                    f'raw frame {i} is {len(buf)} bytes, expected {expect} '
                    f'({width}x{height} RGB24) — the render was truncated')
            writer.append_data(np.frombuffer(buf, dtype=np.uint8).reshape(int(height), int(width), 3))
            if log is not None and (i + 1) % 25 == 0:
                log.log(f'[PROGRESS] {i + 1}/{n}')
    return n


# ── Side-by-side composition (version comparison) ────────────────────────────
# One movie per image VERSION is recorded by the normal path, then the finished files are composed
# here into a single frame each. Composing at the frame level (rather than loading several versions
# into one napari canvas) is what keeps every recording invariant intact — see
# docs/todo/MOVIE_COMPARE_PLAN.md D1/D2.

#: Height of a column's caption strip, as a fraction of the tile height (floored at 14 px so it is
#: legible on a small canvas). Big enough to read a version name, small enough not to be the figure.
_CAPTION_FRAC = 0.055
_CAPTION_MIN_PX = 14

#: A divider between tiles, in px, and its colour. Not decoration: the comparisons this exists for are
#: often SLIGHT (raw vs a correction that moved the intensities a little), and two dark fluorescence
#: frames butted together read as one wide frame — you cannot see where one version ends. Mid-grey
#: rather than black or white so the line stays visible against a dark fluorescence tile and a bright
#: brightfield one alike.
_SEPARATOR_PX = 2
_SEPARATOR_RGB = (90, 90, 100)


def _frame_count(reader):
    """Frame count of an open imageio reader, or None when it cannot be known cheaply. Only used to
    give the progress bar a total — never to drive the loop, which reads until the inputs run out."""
    try:
        n = int(reader.count_frames())
    except Exception:
        return None
    return n if n > 0 else None


def _rgb(frame, np):
    """A frame as (H, W, 3) uint8. Sources are our own mp4s (always RGB), so this only guards the
    degenerate cases rather than converting colour spaces."""
    a = np.asarray(frame)
    if a.ndim == 2:
        a = np.repeat(a[:, :, None], 3, axis=2)
    elif a.shape[2] > 3:
        a = a[:, :, :3]
    return a.astype(np.uint8, copy=False)


def _pad_to(frame, height, width, np):
    """`frame` centred on a black (height, width, 3) canvas. Normally a no-op: every column is
    screenshotted at the same requested/canvas size, so the tiles already match. It is the safety net
    for the case they don't (see MOVIE_COMPARE_PLAN.md D10) — a padded column is readable, a crashed
    stitch after an hour of rendering is not."""
    out = np.zeros((height, width, 3), dtype=np.uint8)
    if frame is None:
        return out
    f = _rgb(frame, np)
    h, w = min(f.shape[0], height), min(f.shape[1], width)
    top, left = (height - h) // 2, (width - w) // 2
    out[top:top + h, left:left + w] = f[:h, :w]
    return out


def stitch_movies(paths, out_path, *, fps, labels=None, layout="row",
                  on_progress=None, should_cancel=None):
    """Compose several .mp4s into one, tile by tile, and write it to ``out_path``. Returns the number
    of frames written.

    ``layout`` is ``'row'`` (tiles left to right — the default) or ``'column'`` (top to bottom), with
    a thin divider between tiles (see ``_SEPARATOR_PX``).
    ``labels``, when given, must be one caption per input and is drawn as a strip under each tile
    (`title_card.caption_band` — one font stack and one truncation rule for all text on a movie
    frame). ``on_progress(i, total)`` is called per frame and ``should_cancel()`` polled per frame;
    a true reading raises `RecordCancelled` — the same contract as
    ``napari_utils._render_animation``, because from the user's side this is the tail of the same
    render.

    Inputs of unequal length hold their last frame until the longest one ends, rather than truncating
    to the shortest: a comparison that silently loses the end of a timecourse is worse than one that
    freezes visibly. Inputs of unequal frame size are centred on the largest tile.

    **Staged**: frames go to a ``.tmp.mp4`` sibling and are promoted onto ``out_path`` only once the
    last one is written, so a cancel or a crash never replaces a good movie with a file that has no
    moov atom. Same scheme (and the same reason for keeping the ``.mp4`` extension on the temp) as
    ``napari_utils._render_animation`` and ``title_card.prepend_title_to_movie``.
    """
    import imageio.v2 as imageio   # local: keeps the import off any path that only needs the helpers
    import numpy as np

    paths = [str(p) for p in paths]
    if not paths:
        raise ValueError("stitch_movies needs at least one input movie")
    if layout not in ("row", "column"):
        raise ValueError(f"stitch_movies: layout must be 'row' or 'column', got {layout!r}")
    if labels is not None and len(labels) != len(paths):
        raise ValueError(f"stitch_movies: {len(labels)} label(s) for {len(paths)} movie(s)")

    readers = []
    staging = f"{out_path}.tmp.mp4"
    written = 0
    try:
        for p in paths:
            readers.append(imageio.get_reader(p))
        counts = [_frame_count(r) for r in readers]
        total = max([c for c in counts if c], default=0)

        # Pull frame 0 from each up front: it sizes the tile, and taking it from the iterator (rather
        # than get_data(0)) means no seek — these readers stream.
        iters = [iter(r) for r in readers]
        cur, alive = [], []
        for it in iters:
            try:
                cur.append(np.asarray(next(it)))
                alive.append(True)
            except StopIteration:
                cur.append(None)
                alive.append(False)
        if not any(alive):
            raise ValueError("stitch_movies: none of the input movies has any frames")

        tile_h = max(f.shape[0] for f in cur if f is not None)
        tile_w = max(f.shape[1] for f in cur if f is not None)

        # Captions are static, so render them once and reuse the arrays for every frame.
        bands = None
        if labels:
            from cecelia.utils import title_card
            band_h = max(_CAPTION_MIN_PX, int(round(tile_h * _CAPTION_FRAC)))
            bands = [title_card.caption_band(tile_w, band_h, t) for t in labels]

        axis = 1 if layout == "row" else 0
        separator = None
        if len(paths) > 1:
            band_px = bands[0].shape[0] if bands is not None else 0
            shape = ((tile_h + band_px, _SEPARATOR_PX) if axis == 1
                     else (_SEPARATOR_PX, tile_w))
            separator = np.broadcast_to(np.array(_SEPARATOR_RGB, dtype=np.uint8),
                                        (*shape, 3)).copy()

        def compose():
            tiles = [_pad_to(f, tile_h, tile_w, np) for f in cur]
            if bands is not None:
                tiles = [np.concatenate((t, b), axis=0) for t, b in zip(tiles, bands)]
            if separator is not None:
                spaced = [tiles[0]]
                for t in tiles[1:]:
                    spaced += [separator, t]
                tiles = spaced
            return np.concatenate(tiles, axis=axis)

        with movie_writer(staging, fps) as out:
            while True:
                if should_cancel is not None and should_cancel():
                    raise RecordCancelled(written)
                out.append_data(crop_to_even(compose()))
                written += 1
                if on_progress is not None:
                    on_progress(written, total or written)
                for i, it in enumerate(iters):
                    if not alive[i]:
                        continue                     # exhausted: hold its last frame (see docstring)
                    try:
                        cur[i] = np.asarray(next(it))
                    except StopIteration:
                        alive[i] = False
                if not any(alive):
                    break
    except BaseException:
        try:
            os.remove(staging)
        except OSError:
            pass
        raise
    finally:
        for r in readers:
            try:
                r.close()
            except Exception:
                pass
    os.replace(staging, out_path)                    # promote: whole file or none
    return written
