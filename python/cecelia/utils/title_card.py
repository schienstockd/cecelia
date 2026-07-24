"""Movie title card — render a description slide and prepend it to a recorded .mp4.

Part of the animation title-card feature (docs/todo/ANIMATION_PLAN.md → Phase H). A recorded movie is
written by napari-animation's ``Animation.animate()`` (frames not exposed), so the card is composited
as a POST-record step here: render N = ``duration × fps`` still frames of a dark slide (image name +
attributes, channel/population/colour-by legend rows with colour swatches, an optional note) at the
movie's exact resolution, then rewrite the file with those frames prepended. The rewrite re-encodes
the clip once (acceptable for short movies).

Content is passed in as a plain dict (assembled by the caller from the CANONICAL legend source — see
Phase H, decision 4; channels are added by the recorder from the live viewer, decision 5):

    content = {
      "title":    "MERTK — mouse 1 — location B",     # image name + attrs
      "note":     "15s intravital, day 3",             # optional free-text line ("" to omit)
      "sections": [                                     # legend blocks, in display order
        {"heading": "Channels",  "items": [{"label": "gBT", "colour": "#00ff00"}, …]},
        {"heading": "Tracks",    "items": [{"label": "T cells", "colour": "#00bfff"}]},
        {"heading": "Colour by", "items": [{"label": "Directed", "colour": "#ff1493"}, …]},
      ],
    }

Pure + testable without napari (only PIL + numpy for rendering; imageio only for the prepend, both
already in the env via scikit-image / imageio-ffmpeg). See python/cecelia/tests/test_title_card.py.
"""
import os

import numpy as np
from PIL import Image, ImageDraw, ImageFont

# Dark palette matched to the app surface (not exact tokens — this renders to pixels, not CSS).
_BG        = (11, 11, 18)
_FG_TITLE  = (240, 240, 245)
_FG_HEAD   = (150, 150, 160)
_FG_LABEL  = (222, 222, 228)
_FG_NOTE   = (170, 170, 180)
_SWATCH_BORDER = (255, 255, 255)


def _font(size):
    """A font at ``size`` px. Prefers the scalable built-in (Pillow ≥ 10), then a bundled TrueType,
    then the fixed default — so we never hard-depend on a font file being present."""
    size = max(8, int(size))
    try:
        return ImageFont.load_default(size=size)          # Pillow ≥ 10 scales the built-in
    except TypeError:
        pass
    for name in ("DejaVuSans.ttf", "Arial.ttf", "LiberationSans-Regular.ttf"):
        try:
            return ImageFont.truetype(name, size)
        except OSError:
            continue
    return ImageFont.load_default()


def _hex_rgb(value):
    """'#00ff00' / '00ff00' / 'f00' → (r, g, b); None for missing/invalid (→ no swatch drawn)."""
    if not value:
        return None
    s = str(value).lstrip("#")
    if len(s) == 3:
        s = "".join(c * 2 for c in s)
    if len(s) != 6:
        return None
    try:
        return (int(s[0:2], 16), int(s[2:4], 16), int(s[4:6], 16))
    except ValueError:
        return None


def title_frame_count(fps, duration_sec):
    """Number of still frames for the card = fps × duration, at least 1."""
    return max(1, int(round(float(fps or 0) * float(duration_sec or 0))))


def _fit_prefix(draw, s, font, max_w):
    """Largest prefix length of `s` (>= 1) whose rendered width fits `max_w`."""
    if draw.textlength(s, font=font) <= max_w:
        return len(s)
    lo, hi = 1, len(s)
    while lo < hi:
        mid = (lo + hi + 1) // 2
        if draw.textlength(s[:mid], font=font) <= max_w:
            lo = mid
        else:
            hi = mid - 1
    return lo


def _wrap_lines(draw, text, font, max_w):
    """Word-wrap `text` to fit `max_w`. A single word wider than the line (e.g. a long image name with
    no spaces) is hard-broken — preferring a break just after a '-' or '_' within the fitting prefix, so
    names like ``M1a-MERTK_KAT-…-res_0001`` split at separators rather than mid-token. Never ellipsises,
    so the whole title is always shown."""
    lines, cur = [], ""
    for w in str(text).split():
        trial = w if not cur else cur + " " + w
        if draw.textlength(trial, font=font) <= max_w:
            cur = trial
            continue
        if cur:
            lines.append(cur)
            cur = ""
        while draw.textlength(w, font=font) > max_w:
            n = _fit_prefix(draw, w, font, max_w)
            sep = max(w.rfind("-", 0, n), w.rfind("_", 0, n))   # prefer a separator break
            cut = sep + 1 if sep > 0 else n
            lines.append(w[:cut])
            w = w[cut:]
        cur = w
    if cur:
        lines.append(cur)
    return lines


def render_card_frame(content, width, height):
    """Render the title card once as an (H, W, 3) uint8 RGB array. The title word-wraps so the whole
    image name shows; content that overflows the height is clipped (movies are tall enough for the
    small legend in practice) and long legend labels are ellipsised."""
    width = max(2, int(width))
    height = max(2, int(height))
    img = Image.new("RGB", (width, height), _BG)
    d = ImageDraw.Draw(img)
    margin = max(16, int(width * 0.045))
    x = margin
    y = margin
    max_w = width - 2 * margin

    def text_h(font, sample="Ag"):
        b = d.textbbox((0, 0), sample, font=font)
        return b[3] - b[1]

    def clip(s, font):
        if d.textlength(s, font=font) <= max_w:
            return s
        while s and d.textlength(s + "…", font=font) > max_w:
            s = s[:-1]
        return (s + "…") if s else s

    def line(px, py, s, font, fill):
        d.text((px, py), s, font=font, fill=fill)
        return int(text_h(font) * 1.35)

    title = str(content.get("title") or "").strip()
    if title:
        tf = _font(height * 0.05)                     # a bit smaller so long image names fit
        for ln in _wrap_lines(d, title, tf, max_w):   # wrap (never clip) so the whole name shows
            y += line(x, y, ln, tf, _FG_TITLE)
        y += int(height * 0.02)

    head_f = _font(height * 0.032)
    label_f = _font(height * 0.030)
    row_h = int(text_h(label_f) * 1.5)
    swatch = int(text_h(label_f))

    for section in (content.get("sections") or []):
        items = section.get("items") or []
        if not items:
            continue
        heading = str(section.get("heading") or "").strip()
        if heading:
            y += line(x, y, heading, head_f, _FG_HEAD) + int(height * 0.006)
        for it in items:
            rgb = _hex_rgb(it.get("colour"))
            label_x = x
            if rgb is not None:
                d.rectangle([x, y + 2, x + swatch, y + 2 + swatch], fill=rgb, outline=_SWATCH_BORDER)
                label_x = x + swatch + int(swatch * 0.5)
            d.text((label_x, y), clip(str(it.get("label") or ""), label_f), font=label_f, fill=_FG_LABEL)
            y += row_h
        y += int(height * 0.015)

    note = str(content.get("note") or "").strip()
    if note:
        nf = _font(height * 0.028)
        y += int(height * 0.01)
        d.text((x, y), clip(note, nf), font=nf, fill=_FG_NOTE)

    return np.asarray(img, dtype=np.uint8)


def prepend_title_to_movie(movie_path, content, *, duration_sec=3.0):
    """Prepend the rendered title card to an existing .mp4, rewriting it in place. Reads the movie's
    fps + frame size (so the card matches exactly), writes card frames then the movie's frames to a
    temp file, and atomically replaces the original. Returns the number of card frames prepended.
    Re-encodes the clip once (the source frames are read back and re-written)."""
    import imageio.v2 as imageio   # local import: only the prepend step needs it, keeps render_* light

    movie_path = str(movie_path)
    with imageio.get_reader(movie_path) as r:
        meta = r.get_meta_data()
        fps = float(meta.get("fps") or 15)
        frame0 = r.get_data(0)
    height, width = int(frame0.shape[0]), int(frame0.shape[1])

    card = render_card_frame(content, width, height)
    n = title_frame_count(fps, duration_sec)

    tmp = movie_path + ".tmp.mp4"
    # macro_block_size=1 keeps the card + source frames at their exact (even) dimensions — no resize,
    # so the appended source frames always match the writer's frame size.
    with imageio.get_writer(tmp, fps=fps, codec="libx264", quality=8,
                            macro_block_size=1, pixelformat="yuv420p") as out, \
            imageio.get_reader(movie_path) as r2:
        for _ in range(n):
            out.append_data(card)
        for frame in r2:
            out.append_data(frame)
    os.replace(tmp, movie_path)
    return n
