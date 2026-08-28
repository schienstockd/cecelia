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


#: A real TrueType is tried BEFORE Pillow's built-in, and the order matters. `load_default(size=…)`
#: succeeds on every Pillow >= 10, so putting it first made the TrueType branch below dead code — and
#: the built-in (Aileron) covers little more than ASCII. Titles are `name — attr — attr` (EM DASH,
#: U+2014, from `_title_card_content` in api/src/napari_api.jl), so every real title card rendered
#: `.notdef` boxes where the separators were: Aileron's mask for `—` is bitmap-identical to its mask
#: for `中`, which is how a missing glyph looks. Notes and attribute values are user text and can hold
#: anything (µm, °, accents), so ASCII-only was never enough here.
#:
#: matplotlib's bundled copy is listed first because it is an in-env absolute path — present on every
#: platform the app ships to, unlike a bare name, which needs the OS font dirs to hold it. The bare
#: names stay as a fallback for a slim env without matplotlib.
def _font_candidates():
    try:
        import matplotlib
        from pathlib import Path
        yield str(Path(matplotlib.__file__).parent / "mpl-data" / "fonts" / "ttf" / "DejaVuSans.ttf")
    except Exception:
        pass
    yield from ("DejaVuSans.ttf", "LiberationSans-Regular.ttf", "Arial.ttf")


def _font(size):
    """A font at ``size`` px. Prefers a real TrueType with non-ASCII coverage, then the scalable
    built-in (Pillow >= 10), then the fixed default — so we never hard-depend on a font file."""
    size = max(8, int(size))
    for name in _font_candidates():
        try:
            return ImageFont.truetype(name, size)
        except OSError:
            continue
    try:
        return ImageFont.load_default(size=size)          # Pillow >= 10 scales the built-in
    except TypeError:
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


def _clip(draw, s, font, max_w):
    """`s` ellipsised to fit `max_w`. The ONE truncation rule for text drawn onto a movie frame —
    shared by the card's legend rows and the comparison movie's column captions."""
    if draw.textlength(s, font=font) <= max_w:
        return s
    while s and draw.textlength(s + "…", font=font) > max_w:
        s = s[:-1]
    return (s + "…") if s else s


def _wrap_lines(draw, text, font, max_w):
    """Word-wrap `text` to fit `max_w`. A single word wider than the line (e.g. a long image name with
    no spaces) is hard-broken — preferring a break just after a '-' or '_' within the fitting prefix, so
    names like ``M1a-MERTK_KAT-…-res_0001`` split at separators rather than mid-token. Never ellipsises,
    so the whole title is always shown.

    ``max_w`` is clamped to at least 1px and the hard-break loop is guarded on ``w`` being non-empty.
    Both are termination guards, not cosmetics: ``render_card_frame`` derives ``max_w`` as
    ``width - 2*margin`` with a 16px floor on the margin, so any frame under ~32px wide made it
    NEGATIVE — every prefix then "doesn't fit", the loop appended an empty string forever and the
    render hung, holding the recording open with it. Reachable from a legitimate size request: the
    movie size policy clamps the top end at 4096 but the bottom end at 2."""
    max_w = max(1, max_w)
    lines, cur = [], ""
    for w in str(text).split():
        trial = w if not cur else cur + " " + w
        if draw.textlength(trial, font=font) <= max_w:
            cur = trial
            continue
        if cur:
            lines.append(cur)
            cur = ""
        while w and draw.textlength(w, font=font) > max_w:
            n = _fit_prefix(draw, w, font, max_w)
            sep = max(w.rfind("-", 0, n), w.rfind("_", 0, n))   # prefer a separator break
            cut = sep + 1 if sep > 0 else n
            lines.append(w[:cut])
            w = w[cut:]
        cur = w
    if cur:
        lines.append(cur)
    return lines


def _font_scale(width, height):
    """The reference dimension the card's fonts are sized against.

    Type used to scale off ``height`` alone while every other measurement on the card — the margin, the
    wrap width, the ellipsis point — is driven by ``width``. The card therefore rendered at a size that
    depended on the frame's ASPECT RATIO: a 500x500 batch recording drew its title at 5.0% of the frame
    width where a 1200x800 viewer recording drew it at 3.33%, i.e. half again as large for the same
    card. Capping the reference at ``width * 2/3`` (a 3:2 frame) leaves every landscape recording
    exactly as it was and only pulls square/portrait ones back into line.
    """
    return min(max(2, int(height)), (max(2, int(width)) * 2) / 3)


def render_card_frame(content, width, height):
    """Render the title card once as an (H, W, 3) uint8 RGB array. The title word-wraps so the whole
    image name shows; content that overflows the height is clipped (movies are tall enough for the
    small legend in practice) and long legend labels are ellipsised.

    Text is sized off ``_font_scale`` rather than the raw height — see there for why."""
    width = max(2, int(width))
    height = max(2, int(height))
    scale = _font_scale(width, height)
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
        return _clip(d, s, font, max_w)

    def line(px, py, s, font, fill):
        d.text((px, py), s, font=font, fill=fill)
        return int(text_h(font) * 1.35)

    title = str(content.get("title") or "").strip()
    if title:
        tf = _font(scale * 0.05)                     # a bit smaller so long image names fit
        for ln in _wrap_lines(d, title, tf, max_w):   # wrap (never clip) so the whole name shows
            y += line(x, y, ln, tf, _FG_TITLE)
        y += int(scale * 0.02)

    head_f = _font(scale * 0.032)
    label_f = _font(scale * 0.030)
    row_h = int(text_h(label_f) * 1.5)
    swatch = int(text_h(label_f))

    for section in (content.get("sections") or []):
        items = section.get("items") or []
        if not items:
            continue
        heading = str(section.get("heading") or "").strip()
        if heading:
            y += line(x, y, heading, head_f, _FG_HEAD) + int(scale * 0.006)
        for it in items:
            rgb = _hex_rgb(it.get("colour"))
            label_x = x
            if rgb is not None:
                d.rectangle([x, y + 2, x + swatch, y + 2 + swatch], fill=rgb, outline=_SWATCH_BORDER)
                label_x = x + swatch + int(swatch * 0.5)
            d.text((label_x, y), clip(str(it.get("label") or ""), label_f), font=label_f, fill=_FG_LABEL)
            y += row_h
        y += int(scale * 0.015)

    note = str(content.get("note") or "").strip()
    if note:
        nf = _font(scale * 0.028)
        y += int(scale * 0.01)
        d.text((x, y), clip(note, nf), font=nf, fill=_FG_NOTE)

    return np.asarray(img, dtype=np.uint8)


def caption_band(width, height, text):
    """Render a single centred caption strip as an (H, W, 3) uint8 RGB array — the per-column label of
    a side-by-side comparison movie (`movie_io.stitch_movies`; docs/todo/MOVIE_COMPARE_PLAN.md D7).

    Lives here rather than in `movie_io` so there is ONE font stack, one palette and one truncation
    rule for text drawn onto movie frames — the card and the column labels of the same file must not
    disagree about either. Text too wide for the strip is ellipsised (never wrapped: the strip is one
    line by construction)."""
    width = max(2, int(width))
    height = max(2, int(height))
    img = Image.new("RGB", (width, height), _BG)
    d = ImageDraw.Draw(img)
    s = str(text or "").strip()
    if s:
        margin = max(4, int(width * 0.02))
        max_w = max(1, width - 2 * margin)
        f = _font(height * 0.62)
        s = _clip(d, s, f, max_w)
        box = d.textbbox((0, 0), s, font=f)
        d.text(((width - (box[2] - box[0])) / 2 - box[0], (height - (box[3] - box[1])) / 2 - box[1]),
               s, font=f, fill=_FG_LABEL)
    return np.asarray(img, dtype=np.uint8)


#: Overlay drawing colours + margins. Reused across every frame of a recording, so pinned here beside
#: _BG/_FG. Text is white on a translucent-black rectangle for legibility against both dark
#: fluorescence and bright brightfield; the scale bar is a solid white block above its label — same
#: convention as napari's built-in overlays.
_OVERLAY_TEXT      = (255, 255, 255)
_OVERLAY_SHADOW    = (0, 0, 0)                   # background shim under text; drawn with alpha 128
_OVERLAY_MARGIN_PX = 8


def draw_frame_overlays(frame_np, *, timestamp=None, scale_bar=None):
    """Draw per-frame overlays (timestamp + scale bar) onto an (H, W, 3) uint8 RGB frame in-place-ish.

    Returns the modified frame. ONE font stack, ONE colour palette, ONE renderer for every text glyph
    on a movie frame — same rule as ``caption_band`` and the title card. Used by
    ``encode_movie_run.py`` when the offline renderer asks for napari-parity timestamp + scale bar
    overlays that the Julia-side kernel can't draw itself (no anti-aliased text primitive in Julia).

    ``timestamp`` is the string to draw top-left (e.g. ``"1m 30s"``) or ``None``. ``scale_bar`` is
    ``{"lengthPx": int, "label": str}`` — a solid white bar at the bottom-right with its label above
    — or ``None``. Both can be present.
    """
    if timestamp is None and scale_bar is None:
        return frame_np
    img = Image.fromarray(frame_np).convert("RGB")
    d = ImageDraw.Draw(img, "RGB")
    W, H = img.size
    f = _font(max(11, int(min(H, W) * 0.045)))
    m = _OVERLAY_MARGIN_PX
    if timestamp:
        s = str(timestamp)
        box = d.textbbox((0, 0), s, font=f)
        tw = box[2] - box[0]; th = box[3] - box[1]
        # Shadow rectangle behind the text so it stays readable against a bright frame.
        pad = 4
        d.rectangle([m - pad + box[0], m - pad + box[1], m + tw + pad, m + th + pad], fill=_OVERLAY_SHADOW)
        d.text((m - box[0], m - box[1]), s, font=f, fill=_OVERLAY_TEXT)
    if scale_bar:
        length_px = max(2, int(scale_bar.get("lengthPx", 0)))
        label = str(scale_bar.get("label", ""))
        bar_h = max(3, int(min(H, W) * 0.006))
        # Bar bottom-right; label centred above the bar.
        bar_x2 = W - m
        bar_x1 = bar_x2 - length_px
        bar_y2 = H - m
        bar_y1 = bar_y2 - bar_h
        if bar_x1 >= 0:
            d.rectangle([bar_x1, bar_y1, bar_x2, bar_y2], fill=_OVERLAY_TEXT)
            if label:
                lbox = d.textbbox((0, 0), label, font=f)
                lw = lbox[2] - lbox[0]; lh = lbox[3] - lbox[1]
                lx = bar_x1 + (length_px - lw) / 2 - lbox[0]
                ly = bar_y1 - lh - 4 - lbox[1]
                pad = 4
                d.rectangle([lx - pad + lbox[0], ly - pad + lbox[1],
                             lx + lw + pad, ly + lh + pad], fill=_OVERLAY_SHADOW)
                d.text((lx, ly), label, font=f, fill=_OVERLAY_TEXT)
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

    # The temp deliberately KEEPS the `.mp4` extension — imageio infers the writer format from it, so
    # the sibling-suffix scheme `atomic_io` uses (`x.mp4.tmp.ab12`) can't be used here. That means a
    # leftover from a killed run WOULD match a naive `*.mp4` directory listing, so `/api/movies`
    # filters `.tmp.` names out. See cecelia/utils/atomic_io.py for the general rule.
    tmp = movie_path + ".tmp.mp4"
    # The SHARED writer (`movie_io.movie_writer`) — the same one the recorders use, because this file is
    # the card concatenated onto a recording and the two halves must encode identically. Its
    # macro_block_size=1 keeps the card + source frames at their exact (even) dimensions — no resize, so
    # the appended source frames always match the writer's frame size.
    from cecelia.utils.movie_io import movie_writer
    with movie_writer(tmp, fps) as out, \
            imageio.get_reader(movie_path) as r2:
        for _ in range(n):
            out.append_data(card)
        for frame in r2:
            out.append_data(frame)
    os.replace(tmp, movie_path)
    return n
