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
"""

# Per-axis ceiling for a requested movie size. A canvas screenshot renders through the GL context, so a
# size the driver cannot allocate is not a clean failure — on some drivers it comes back as a blank
# frame, which would encode happily into a black movie. Render cost also scales with pixel count, so a
# few thousand frames at 8K is a hang, not a figure. 4096 is comfortably inside every GL max-viewport
# we would ship against.
MAX_MOVIE_AXIS = 4096

# libx264 with yuv420p subsamples chroma 2×2, so odd dimensions are rejected outright — and because we
# write with macro_block_size=1 (exact frames, no silent rescale), imageio will not paper over it.
_EVEN = 2


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


def movie_writer(path, fps):
    """The one imageio writer for a Cecelia `.mp4`. Use as a context manager.

    ``macro_block_size=1`` keeps frames at their exact dimensions — imageio's default of 16 rescales
    anything not divisible by 16, which is why "canvas size" used to be approximate. That is why
    `coerce_movie_size`/`crop_to_even` own the even-dimension rule instead: nothing downstream will fix
    an odd frame for us.
    """
    import imageio.v2 as imageio   # local: keeps the import off any path that only needs the helpers

    return imageio.get_writer(str(path), fps=float(fps), codec="libx264", quality=8,
                              macro_block_size=1, pixelformat="yuv420p")
