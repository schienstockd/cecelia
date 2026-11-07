"""Figures: a still, a filmstrip or a z-strip of one region, rendered from the raw image by the viewer's
own shader and laid out in one standard look. ``docs/todo/FIGURES_PLAN.md``.

Tiles go through ``render_animation_run.render_frames`` (the shared still renderer: level 0, a z slab as
max projection, per-channel LUTs) — the same pixels a movie frame of the same view gets. This module
adds only what a figure needs on top: one display range per channel shared across every tile, an
honest enlargement of native pixels, and the layout (rows, gaps, labels, legend) with ``title_card``'s
font stack.

**Spec** (params dict; camelCase like every ``run_py`` contract):
    ``zarrPath``        : image store
    ``outDir``          : where ``figure.png``, ``preview.png`` (≤ ``PREVIEW_PX``) and ``figure.json`` (the
                          resolved spec) land — a caller keeps what it needs (the app keeps figure.png)
    ``kind``            : ``sequence`` (one region over ``frames``) | ``zstack`` (one ``t``, planes ``planes``)
    ``center``          : ``[y, x]`` in level-0 pixels — the crop centre
    ``sizePx``          : crop side in level-0 pixels (square)
    ``z``               : centre plane; ``zHalf`` planes either side are max-projected (default 0)
    ``frames``          : timepoints (``sequence``); ``t`` + ``planes`` (``zstack``)
    ``rows``            : ``[{label?, channels: [{c, colour, name?}]}]`` — one row of tiles per entry;
                          ``colour`` is ``#rrggbb`` (a channel's LUT runs black → colour); ``name`` is
                          the legend's display name (default ``channelNames[c]``)
    ``display``         : ``{percentiles: [lo, hi]}`` (default ``[1, 99.8]``) measured over ALL tiles, or
                          ``{ranges: {"<c>": [lo, hi]}}`` explicitly. ``perTile: true`` opts into a range
                          per tile, and the legend then says so.
    ``tilePx``          : output tile side; default ``style.tileMm`` at ``style.dpi``
    ``interpolation``   : ``pixels`` (default: native pixels as equal blocks, crisp) | ``smooth``
                          (render 1:1, Lanczos up — soft at large enlargements)
    ``channelNames``    : names by channel index, for the legend
    ``timeIncrementS``  : seconds per timepoint (``None`` = label ``t<n>``)
    ``pixelSizeUm``     : x pixel size; ``zStepUm``: z step (``zstack`` labels); ``zAniso`` = z / x
    ``scaleBarUm``      : scale-bar length (``None`` = none)
    ``layout``          : ``strip`` (default: every frame in one line) | ``grid`` (the frames wrapped
                          into a near-square matrix — ``grid_columns`` — for a long series)
    ``style``           : overrides of ``STYLE`` (dpi, tileMm — default auto by frame count, gapMm, fontPt, scaleFontPt, barMm,
                          scaleBarOn ``first`` (default) | ``last`` | ``all``)
"""
import json
import os
import pathlib

import numpy as np
from PIL import Image, ImageDraw

from cecelia.utils import title_card, wgpu_host
from cecelia.utils.atomic_io import atomic_path, write_json_atomic
from cecelia.utils.zarr_utils import open_as_zarr, read_axes
from cecelia.writers import render_animation_run

KINDS = ('sequence', 'zstack')
LAYOUTS = ('strip', 'grid')
MAX_TILES = 64
MAX_TILE_PX = 4096
PREVIEW_PX = 1600
_DEFAULT_PERCENTILES = (1.0, 99.8)


class FigureSpecError(ValueError):
    """A spec the renderer refuses — the message names the field and what is allowed."""


def _hex(colour):
    s = str(colour).lstrip('#')
    if len(s) != 6:
        raise FigureSpecError(f'rows[].channels[].colour {colour!r}: expected #rrggbb')
    return [int(s[i:i + 2], 16) / 255.0 for i in (0, 2, 4)]


TIME_FORMATS = ('clock', 'min')


def elapsed_label(t, inc_s, fmt='clock'):
    """Elapsed time at timepoint ``t`` as a column title. ``clock`` (default): ``h:mm:ss``, the viewer's
    and the movies' timestamp (``stillOverlay.ts`` ``elapsedLabel(..., 'clock')``); ``min``: ``12.5 min``,
    switching to ``2.5 h`` from two hours. No interval → ``t<n>``."""
    if fmt not in TIME_FORMATS:
        raise FigureSpecError(f'style.timeFormat {fmt!r}: expected one of {", ".join(TIME_FORMATS)}')
    if not inc_s:
        return f't{t}'
    secs = t * float(inc_s)
    if fmt == 'clock':
        s = int(round(secs))
        return f'{s // 3600}:{(s % 3600) // 60:02d}:{s % 60:02d}'
    mins = secs / 60
    return f'{round(mins / 60, 1):g} h' if mins >= 120 else f'{round(mins, 1):g} min'


def validate(spec):
    """The resolved spec, or ``FigureSpecError`` naming the field. Caps keep a spec from running away."""
    s = dict(spec)
    kind = s.get('kind', 'sequence')
    if kind not in KINDS:
        raise FigureSpecError(f'kind {kind!r}: expected one of {", ".join(KINDS)}')
    s['kind'] = kind
    for k in ('zarrPath', 'outDir', 'center', 'sizePx', 'z', 'rows'):
        if s.get(k) is None:
            raise FigureSpecError(f'{k}: required')
    if kind == 'sequence':
        if not s.get('frames'):
            raise FigureSpecError('frames: a non-empty list of timepoints is required for kind sequence')
    elif s.get('t') is None or not s.get('planes'):
        raise FigureSpecError('t and planes: both required for kind zstack')
    n = len(s['frames'] if kind == 'sequence' else s['planes'])
    if n * len(s['rows']) > MAX_TILES:
        raise FigureSpecError(f'{n} columns x {len(s["rows"])} rows exceeds {MAX_TILES} tiles')
    if s.get('tilePx') is not None and not 16 <= int(s['tilePx']) <= MAX_TILE_PX:
        raise FigureSpecError(f'tilePx {s["tilePx"]}: expected 16..{MAX_TILE_PX}')
    s['sizePx'] = int(s['sizePx'])
    s['interpolation'] = s.get('interpolation', 'pixels')
    if s['interpolation'] not in ('smooth', 'pixels'):
        raise FigureSpecError(f'interpolation {s["interpolation"]!r}: expected smooth | pixels')
    for r in s['rows']:
        if not r.get('channels'):
            raise FigureSpecError('rows[].channels: each row needs at least one channel')
        for ch in r['channels']:
            _hex(ch.get('colour', '#ffffff'))
    s['layout'] = s.get('layout') or 'strip'
    if s['layout'] not in LAYOUTS:
        raise FigureSpecError(f'layout {s["layout"]!r}: expected one of {", ".join(LAYOUTS)}')
    s.setdefault('zHalf', 0)
    s.setdefault('display', {})
    return s


def _columns(s):
    """``(t, z_lo, z_hi, label)`` per column."""
    zh = int(s['zHalf'])
    if s['kind'] == 'sequence':
        z = int(s['z'])
        fmt = (s.get('style') or {}).get('timeFormat') or STYLE['timeFormat']
        return [(int(t), z - zh, z + zh, elapsed_label(int(t), s.get('timeIncrementS'), fmt)) for t in s['frames']]
    step = s.get('zStepUm')
    return [(int(s['t']), int(p) - zh, int(p) + zh,
             f'z{p}' + (f' ({(int(p) - int(s["z"])) * step:+g} µm)' if step else '')) for p in s['planes']]


def _crop(level0, axes, t, z_lo, z_hi, s):
    """``(c, y, x)`` — the max-projected crop at ``t``, every channel: what the column's tiles show, for
    measuring its range. Read through the renderer's own slicer (``_load_at_t``), so the range is
    measured on exactly the pixels the shader draws."""
    cy, cx = (int(round(v)) for v in s['center'])
    h = s['sizePx'] // 2
    vol = render_animation_run._load_at_t(level0, t, axes, z_slice=slice(max(0, z_lo), z_hi + 1),
                                          yx=(slice(max(0, cy - h), cy + h), slice(max(0, cx - h), cx + h)))
    return vol.astype(np.float64).max(axis=1)


def display_ranges(s, levels, axes, cols):
    """``{(c, col) : (lo, hi)}`` — one range per channel over every tile unless ``perTile``. Explicit
    ``ranges`` win. Returned with the measured values so ``figure.json`` records what was drawn."""
    disp = s['display']
    chans = sorted({int(ch['c']) for r in s['rows'] for ch in r['channels']})
    explicit = {int(k): tuple(v) for k, v in (disp.get('ranges') or {}).items()}
    p_lo, p_hi = disp.get('percentiles', _DEFAULT_PERCENTILES)
    out = {}
    by_col = ([_crop(levels[0], axes, t, zl, zh, s) for t, zl, zh, _ in cols]
              if any(c not in explicit for c in chans) else [])
    for c in chans:
        if c in explicit:
            out.update({(c, i): explicit[c] for i in range(len(cols))})
            continue
        crops = [col[c] for col in by_col]
        if disp.get('perTile'):
            for i, a in enumerate(crops):
                out[(c, i)] = (float(np.percentile(a, p_lo)), float(np.percentile(a, p_hi)))
        else:
            allv = np.concatenate([a.ravel() for a in crops])
            rng = (float(np.percentile(allv, p_lo)), float(np.percentile(allv, p_hi)))
            out.update({(c, i): rng for i in range(len(cols))})
    return out


def _states(s, row, cols, ranges, nc, canvas):
    """One renderer state per column for ``row``: a 2D head-on view of the crop, only the row's channels
    visible, each black → its colour."""
    cy, cx = (float(v) for v in s['center'])
    want = {int(ch['c']): ch.get('colour', '#ffffff') for ch in row['channels']}
    states = []
    for i, (t, zl, zh, _) in enumerate(cols):
        specs = []
        for c in range(nc):
            if c in want:
                lo, hi = ranges[(c, i)]
                specs.append({'lo': lo, 'hi': max(hi, lo + 1), 'lut': [[0, 0, 0], _hex(want[c])], 'visible': True})
            else:
                specs.append({'lo': 0, 'hi': 1, 'lut': [], 'visible': False})
        states.append({'t': t, 'ndisplay': 2, 'zRange': [max(0, zl), zh], 'snapH': canvas,
                       'camera': {'angles': [0, 0, 0], 'center': [float(s['z']), cy, cx],
                                  'zoom': canvas / s['sizePx']},
                       'specs': specs})
    return states


#: The house look, after the Cecelia paper's image panels (Nat Commun 2025, Fig. 4a): black tiles on
#: a white page with thin white gaps, plain black column titles above, a black legend band below with
#: each channel named in its own colour, a small plain scale bar. Lengths in mm / pt so text is the
#: same size in every figure whatever the tile size.
#: ONE source with the style editor and the per-project override: ``frontend/src/lib/figureStyle.json``
#: (``tileMm: None`` = auto — the strip fills ``figureWidthMm``, each tile within [tileMmMin, tileMmMax]:
#: 9 frames get 20 mm, a still 80 mm; a fixed 20 mm made a still's 7 pt text an eighth of its height).
STYLE_FILE = pathlib.Path(__file__).resolve().parents[3] / "frontend" / "src" / "lib" / "figureStyle.json"
with open(STYLE_FILE, encoding="utf-8") as _f:
    STYLE = json.load(_f)["defaults"]
_NAMED = {'#ff00ff': 'magenta', '#00ff00': 'green', '#ffffff': 'grey', '#00ffff': 'cyan',
          '#ff0000': 'red', '#0000ff': 'blue', '#ffff00': 'yellow'}


def grid_columns(n, n_rows, layout):
    """Frames per line. ``strip``: all ``n``. ``grid``: about ``sqrt(n * n_rows)`` — each line of frames
    is ``n_rows`` tiles tall, so the matrix comes out near square — then evened out so the last line is
    as full as it can be (8 frames → 3 + 3 + 2, not 3 + 3 + 3 with one hole too many)."""
    if layout != 'grid' or n <= 1:
        return n
    c = max(1, int(np.ceil(np.sqrt(n * max(1, n_rows)))))
    lines = int(np.ceil(n / c))
    return int(np.ceil(n / lines))


def _px(mm, dpi):
    return max(1, int(round(mm / 25.4 * dpi)))


def _pt(pt, dpi):
    return max(6, int(round(pt / 72.0 * dpi)))


def tile_mm(style, n_cols):
    """The tile width: ``style.tileMm``, or auto — the page width shared by ``n_cols`` tiles, clamped."""
    if style.get('tileMm'):
        return float(style['tileMm'])
    per = style['figureWidthMm'] / max(1, n_cols)
    return max(style['tileMmMin'], min(style['tileMmMax'], per))


def tile_px(s, style, n_cols=1):
    """Output tile side: ``tilePx`` if given, else ``tile_mm`` at ``dpi``. In ``pixels`` mode it snaps to a
    whole multiple of the crop so every native pixel is the same size block."""
    want = int(s.get('tilePx') or _px(tile_mm(style, n_cols), style['dpi']))
    if s['interpolation'] == 'pixels':
        k = max(1, int(round(want / s['sizePx'])))
        return k * s['sizePx']
    return want


def _legend_parts(s, row):
    """``[(text, rgb)]`` — each channel's name in its own colour (grey drawn white)."""
    names = s.get('channelNames') or []
    if row.get('label'):
        return [(row['label'], (255, 255, 255))]
    parts = []
    for ch in row['channels']:
        c = int(ch['c'])
        rgb = tuple(int(v * 255) for v in _hex(ch.get('colour', '#ffffff')))
        name = ch.get('name') or (names[c] if c < len(names) else f'C{c}')
        parts.append((name, rgb))
    if s['display'].get('perTile'):
        parts.append(('(contrast per tile)', (255, 255, 255)))
    return parts


SCALE_BAR_ON = ('first', 'last', 'all')


def _shows_bar(where, ri, ci, n_rows, n_cols):
    """Which tile carries the scale bar: ``first`` = the first tile of the figure (default — every tile
    shares one scale), ``last`` = the bottom-right tile, ``all`` = every tile."""
    if where not in SCALE_BAR_ON:
        raise FigureSpecError(f'style.scaleBarOn {where!r}: expected one of {", ".join(SCALE_BAR_ON)}')
    return (where == 'all' or (where == 'first' and ri == 0 and ci == 0)
            or (where == 'last' and ri == n_rows - 1 and ci == n_cols - 1))


def _draw_scale_bar(img, bar, style):
    """Plain white bar bottom-right, its label centred above — no outline, no backing box."""
    if not bar:
        return
    d = ImageDraw.Draw(img)
    W, H = img.size
    m = _px(1.0, style['dpi'])
    bh = _px(style['barMm'], style['dpi'])
    x2, y2 = W - m, H - m
    x1 = x2 - bar['lengthPx']
    if x1 < 0:
        return
    d.rectangle([x1, y2 - bh, x2, y2], fill=(255, 255, 255))
    f = title_card.figure_font(_pt(style['scaleFontPt'], style['dpi']))
    b = d.textbbox((0, 0), bar['label'], font=f)
    d.text((x1 + (bar['lengthPx'] - (b[2] - b[0])) / 2 - b[0], y2 - bh - m // 2 - (b[3] - b[1]) - b[1]),
           bar['label'], font=f, fill=(255, 255, 255))


def render_figure(spec, host=None, log=None):
    """Render ``spec`` into ``outDir``: ``figure.png`` (the layout, at ``dpi``), ``preview.png`` and
    ``figure.json`` (the spec with measured ranges). Returns the paths."""
    s = validate(spec)
    style = dict(STYLE, **(s.get('style') or {}))
    log = log or _PrintLog()
    host = host or wgpu_host.MipHost()
    levels, _ = open_as_zarr(s['zarrPath'])
    axes = read_axes(s['zarrPath'])
    nc = dict(zip([a.lower() for a in axes], levels[0].shape)).get('c', 1)
    cols = _columns(s)
    ranges = display_ranges(s, levels, axes, cols)
    per_line = grid_columns(len(cols), len(s['rows']), s['layout'])
    # Auto tile width from the FRAME count, whatever the layout: a grid is the strip's tiles rearranged,
    # so its text (fixed pt) keeps the strip's size against the tiles. Sized by its column count, eight
    # frames got 60 mm tiles instead of 22.5 mm and the labels read tiny.
    tile = tile_px(s, style, len(cols))
    style = dict(style, tileMm=tile_mm(style, len(cols)))     # figure.json records what auto picked
    canvas = s['sizePx'] if s['interpolation'] == 'smooth' else tile
    params = {'zarrPath': s['zarrPath'], 'canvasH': canvas, 'canvasW': canvas,
              'zAniso': float(s.get('zAniso', 1.0)), 'renderQuality': 'high'}
    os.makedirs(s['outDir'], exist_ok=True)
    bar = None
    if s.get('scaleBarUm') and s.get('pixelSizeUm'):
        um = float(s['scaleBarUm'])
        bar = {'lengthPx': int(round(um / float(s['pixelSizeUm']) * tile / s['sizePx'])), 'label': f'{um:g} µm'}
    rows_px = []
    for ri, row in enumerate(s['rows']):
        params['states'] = _states(s, row, cols, ranges, nc, canvas)
        tiles = []
        for ci, frame in enumerate(render_animation_run.render_frames(params, host, log)):
            img = Image.fromarray(frame).convert('RGB')
            if img.size != (tile, tile):
                img = img.resize((tile, tile), Image.LANCZOS if s['interpolation'] == 'smooth' else Image.NEAREST)
            if _shows_bar(style['scaleBarOn'], ri, ci, len(s['rows']), len(cols)):   # first / last FRAME
                _draw_scale_bar(img, bar, style)
            tiles.append(img)
        rows_px.append((tiles, _legend_parts(s, row)))
    out = _compose(rows_px, [c[3] for c in cols], tile, style, per_line)
    fig_path = os.path.join(s['outDir'], 'figure.png')
    with atomic_path(fig_path) as tmp:
        out.save(tmp, format='PNG', dpi=(style['dpi'], style['dpi']))
    # What a caller LOOKS at (Claude's dry run, a list thumbnail): the same figure, no side over
    # PREVIEW_PX, so a 64-tile print figure cannot flood a model's context.
    prev = out.copy()
    prev.thumbnail((PREVIEW_PX, PREVIEW_PX), Image.LANCZOS)
    with atomic_path(os.path.join(s['outDir'], 'preview.png')) as tmp:
        prev.save(tmp, format='PNG')
    resolved = dict(s, style=style, tilePxResolved=tile,
                    resolvedRanges={f'{c}:{i}': list(v) for (c, i), v in ranges.items()})
    write_json_atomic(os.path.join(s['outDir'], 'figure.json'), resolved)
    return {'figure': fig_path, 'spec': os.path.join(s['outDir'], 'figure.json')}


def _compose(rows_px, col_labels, tile, style, per_line=None):
    """White page: column titles above, then per row its tiles (white gaps) and a black legend band
    with each channel named in its own colour.

    ``per_line`` < the frame count wraps the frames into blocks (``layout: grid``): each block repeats
    the titles over its own frames and all the rows; the legend bands follow the last block only when
    there is one row (a merge — one legend says it all), under every row of every block when the
    figure is split (a grey row can't be read without its name)."""
    dpi = style['dpi']
    gap = _px(style['gapMm'], dpi)
    f = title_card.figure_font(_pt(style['fontPt'], dpi))
    probe = ImageDraw.Draw(Image.new('RGB', (1, 1)))
    line_h = probe.textbbox((0, 0), 'Hg0', font=f)
    line_h = line_h[3] - line_h[1]
    pad = max(2, line_h // 3)
    n = len(col_labels)
    per = max(1, min(per_line or n, n))
    blocks = [list(range(b, min(n, b + per))) for b in range(0, n, per)]
    n_rows = len(rows_px)
    W = per * tile + (per - 1) * gap
    head_h = line_h + 2 * pad
    band_h = line_h + 2 * pad

    def bands_in(bi):
        return n_rows > 1 or bi == len(blocks) - 1

    H = 0
    for bi in range(len(blocks)):
        H += head_h + n_rows * tile + (n_rows - 1) * gap + (n_rows * band_h if bands_in(bi) else 0)
    H += (len(blocks) - 1) * gap
    page = Image.new('RGB', (W, H), (255, 255, 255))
    d = ImageDraw.Draw(page)
    space = d.textlength('  ', font=f)
    y = 0
    for bi, idx in enumerate(blocks):
        for k, i in enumerate(idx):
            b = d.textbbox((0, 0), col_labels[i], font=f)
            x = k * (tile + gap) + (tile - (b[2] - b[0])) / 2 - b[0]
            d.text((x, y + pad - b[1]), col_labels[i], font=f, fill=(0, 0, 0))
        y += head_h
        for ri, (tiles, parts) in enumerate(rows_px):
            for k, i in enumerate(idx):
                page.paste(tiles[i], (k * (tile + gap), y))
            y += tile
            if bands_in(bi):
                d.rectangle([0, y, W - 1, y + band_h - 1], fill=(0, 0, 0))
                total = sum(d.textlength(t, font=f) for t, _ in parts) + space * (len(parts) - 1)
                x = (W - total) / 2
                for t, rgb in parts:
                    b = d.textbbox((0, 0), t, font=f)
                    d.text((x, y + pad - b[1]), t, font=f, fill=rgb)
                    x += d.textlength(t, font=f) + space
                y += band_h
            if ri < n_rows - 1:
                y += gap
        y += gap
    return page


class _PrintLog:
    def log(self, msg):
        print(msg, flush=True)


def run(params):
    """``run_py`` entry: ``params`` is the spec."""
    from cecelia.utils import script_utils
    log = script_utils.get_logfile_utils(params)
    paths = render_figure(params, log=log)
    log.log(f'[INFO] figure written: {paths["figure"]}')


def main():
    from cecelia.utils import script_utils
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    try:
        run(params)
    except FigureSpecError as e:
        # The message names the field — printed as the run's error so a caller (or Claude) can fix it.
        print(f'[ERROR] figure spec: {e}', flush=True)
        raise SystemExit(2)


if __name__ == '__main__':
    main()
