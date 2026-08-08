"""
Generic napari display helpers — add images, labels and tracks to a viewer from plain arrays.

These are the SHARED, project-agnostic layer builders: they take arrays + a per-axis ``scale`` (µm),
with NO disk paths and NO project/pipeline state. cecelia's napari bridge
(``napari/napari_bridge.py``) keeps all of its brain — disk load of label zarr / label-props HDF5,
populations, per-layer reconciliation + signature caching, colour-by columns, timestamp, scale-bar —
and delegates only the final ``viewer.add_*`` calls here, so the display conventions live in ONE
documented place.

The sibling ``coastal`` project **imports these helpers** — its ``coastal/napari_viz.py`` delegates its
``add_*`` calls here and keeps only coastal-specific orchestration (viewer setup, array unpacking, its
µm→pixel track matrix). coastal already installs cecelia editable and uses cecelia's IO helpers, so the
napari layer conventions are single-sourced here rather than duplicated — coastal renders identically
by SHARING this code, not mirroring it. See ``docs/todo/CECELIA_NAPARI_UPSTREAM_PLAN.md``.

Conventions (kept consistent across EVERY layer so napari's unit-aware rendering stays enabled — a
mismatch makes napari warn "Inconsistent units across layers" and disable it for ALL layers):
  - ``scale``: per-axis µm/pixel from the image's pixel resolution, e.g. a [T, Z, Y, X] array →
    ``(1, z, y, x)``; pass the SAME scale to images, labels AND tracks.
  - ``units``: set consistently across layers (cecelia reads them from OME-XML).
  - images: one layer per channel (``channel_axis``), per-channel colormaps, ``blending='additive'``,
    contrast from a middle sample.
  - labels: ``opacity=0.7``.
  - tracks: ``[track_id, t, (z), y, x]`` matrix in PIXEL coords (``scale`` supplies the µm
    conversion); ``color_by='track_id'`` → turbo; categorical colour-by → Okabe–Ito (via
    ``colormaps_dict``); continuous → viridis; ``tail_width=4``, ``tail_length=30``.

napari is a heavy ENVIRONMENT dependency (the pixi env ships it; ``pip install cecelia`` does not), so
it is imported LAZILY inside the functions — this module imports only numpy at load time, like
cecelia's other heavy helpers.
"""

import contextlib

import numpy as np

# "The user cancelled it" is ONE exception, raised by both frame loops — this module's recorder and
# `movie_io.stitch_movies`. It lives in `movie_io` (which this module imports, not the reverse) and is
# re-exported here under its original name, so `napari_utils.RecordCancelled` — what the bridge
# catches and what the tests assert on — stays the same object rather than becoming a second,
# incompatible class. movie_io is import-light by design, so this costs nothing at load.
from cecelia.utils.movie_io import RecordCancelled  # noqa: F401  (re-export — see above)

# Default per-channel colormaps (extend if a movie has > 4 channels).
CHANNEL_COLORMAPS = ['red', 'green', 'blue', 'yellow']


def require_napari():
  """Return the ``napari`` module, or raise a clear message. The ONE place napari is imported — callers
  (cecelia's bridge, coastal's viz) go through here + ``new_viewer`` so they never import napari
  directly (napari is an environment dep, not in the ``pip install cecelia`` light tier)."""
  try:
    import napari
  except ImportError as e:  # pragma: no cover - environment-dependent
    raise ImportError(
      "napari is required here — it is an environment dependency (the pixi env ships napari + pyqt); "
      "`pip install cecelia` does not include it."
    ) from e
  return napari


def new_viewer(**kwargs):
  """Create a napari ``Viewer`` — so callers never import napari directly to make one."""
  return require_napari().Viewer(**kwargs)


def image_layer_name(channel_names, channel_axis):
  """napari rejects a LIST ``name`` when there is no channel axis (a single layer). Collapse a list to
  its first element in that case; otherwise pass it through. Pure (no napari)."""
  if isinstance(channel_names, list) and channel_axis is None:
    return channel_names[0] if channel_names else None
  return channel_names


def set_contrast_from_sample(layer, low_pct=1.0, high_pct=99.9, min_valid=100):
  """Set a layer's contrast limits from a middle sample of its data.

  Reads a mid-position slice (middle index along every axis except the last two Y/X, coarsest pyramid
  level if multiscale) and uses the ``[low_pct, high_pct]`` percentiles of the positive values. Avoids
  napari's auto-contrast, which can silently set ``[0, dtype_max]`` for dask arrays that haven't been
  computed yet. Falls back to ``reset_contrast_limits()`` if sampling fails. Pure w.r.t. napari — takes
  a duck-typed layer (``.data``, ``.contrast_limits``, ``.reset_contrast_limits``)."""
  try:
    raw = layer.data
    if isinstance(raw, list):
      raw = raw[-1]  # coarsest scale level — a contrast SAMPLE only, so read the smallest pyramid
                     # level (orders of magnitude less I/O than full-res); runs once per visible layer
    ndim = raw.ndim
    # index that selects the middle position along all axes except the last two (y, x)
    idx = tuple(n // 2 if ax < ndim - 2 else slice(None) for ax, n in enumerate(raw.shape))
    sample = np.asarray(raw[idx]).ravel()
    valid = sample[sample > 0]
    if len(valid) > min_valid:
      cmin = float(np.percentile(valid, low_pct))
      cmax = float(np.percentile(valid, high_pct))
      if cmax > cmin:
        layer.contrast_limits = [cmin, cmax]
        return
  except Exception:
    pass
  try:
    layer.reset_contrast_limits()
  except Exception:
    pass


def add_image(viewer, data, *, scale, units=None, channel_axis=None, channel_names=None,
              colormaps=None, contrast=True, blending='additive', visible=True,
              name=None, axes=None, image_axes=None, image_shape=None, cache=None):
  """Add a (possibly multi-channel) image — one layer per channel via ``channel_axis``, per-channel
  ``colormaps``, additive ``blending``, and (optionally) contrast from a middle sample. Returns the
  added layer, or the list of per-channel layers when ``channel_axis`` is set.

  ``axes``/``image_axes``/``image_shape`` align a layer whose axes are a SUBSET of the viewer's, exactly
  as ``add_labels`` does and for the same reason — napari reinterprets a short layer's axes as the
  viewer's trailing ones, which silently renders time as Z. The task preview needs this: its block is
  channel-less ``[T, Z, Y, X]`` while the image layers it sits beside include C. See ``expand_to_axes``.

  ``name`` overrides the derived layer name, for a layer that is not simply "the image" — a corrected
  channel from an AF preview, say, which has to be distinguishable from the raw channel beside it.
  ``cache=False`` forces every slice to re-read, which matters when consecutive layers of the same
  shape hold DIFFERENT data (see the note on ``add_labels``).
  """
  require_napari()
  if axes is not None or image_axes is not None:
    data, aligned = expand_to_axes(data, axes, image_axes, viewer_shape=image_shape)
    if not aligned:
      nd = layer_ndim(data)
      scale = align_axis_vector(scale, nd)
      if units is not None:
        units = align_axis_vector(units, nd)
  kw = dict(channel_axis=channel_axis,
            name=name if name is not None else image_layer_name(channel_names, channel_axis),
            colormap=colormaps, scale=scale, visible=visible)
  if units is not None:
    kw['units'] = units
  if blending is not None:
    kw['blending'] = blending
  if cache is not None:
    kw['cache'] = bool(cache)
  result = viewer.add_image(data, **kw)
  if contrast:
    for layer in (result if isinstance(result, list) else [result]):
      if getattr(layer, 'visible', True):
        set_contrast_from_sample(layer)
  return result


def preview_region_from_corners(corners, factors, axes, ndisplay=2, current_step=None):
    """A viewer's visible extent → the task-preview region contract, in **level-0 pixels**.

    ``corners``: napari's ``layer.corner_pixels``, shape (2, ndim) — ``[min, max]`` in the layer's own
    DATA coordinates at its current ``data_level``. ``factors``: that level's row of
    ``layer.downsample_factors``. ``axes``: the layer's axis letters in order (channel already
    dropped), e.g. ``['t','z','y','x']``. ``current_step``: ``viewer.dims.current_step``, aligned with
    ``axes`` — the DATA index per dimension.

    Returns ``{"xy": {"X": [lo, hi], "Y": [lo, hi]}, "z": int|None, "t": int|None, "ndisplay": int}``.

    **Two different sources, deliberately, and this is the part that bit us:**

    * The visible XY box comes from ``corner_pixels``.
    * The current plane/timepoint comes from ``current_step``. It CANNOT come from corner_pixels: for a
      dimension that isn't being displayed, napari leaves corner_pixels at ``[0, 0]`` rather than the
      slider position. An earlier version of this function read z/t off corner_pixels on the assumption
      that min == max == the current index; on a live viewer sitting at t=44, z=9 it returned t=0, z=0
      and previewed drift-correction padding. (The unit test that "confirmed" the assumption built a
      fresh layer, whose step is 0 anyway — 0 == 0 proved nothing. Hence the live-viewer test below.)

    Two conversions on the XY box are also silently wrong if fumbled, and invisible in the result:

    * corner_pixels is at ``data_level``, **not level 0** — a zoomed-out viewer reports small numbers
      that must be scaled up, or the preview segments the top-left corner of the image.
    * corner_pixels bounds are **inclusive**; the contract is half-open — so +1 before scaling, or the
      preview quietly drops its last row and column.
    """
    corners = np.asarray(corners)
    factors = np.asarray(factors, dtype=float)
    if corners.ndim != 2 or corners.shape[0] != 2:
        raise ValueError(f"corner_pixels must be (2, ndim), got {corners.shape}")
    if corners.shape[1] != len(axes) or len(factors) != len(axes):
        raise ValueError(
            f"axes {list(axes)} do not match corners {corners.shape} / factors {len(factors)}")
    step = list(current_step) if current_step is not None else None
    if step is not None and len(step) != len(axes):
        raise ValueError(f"current_step {step} does not match axes {list(axes)}")

    out = {"xy": {}, "z": None, "t": None, "ndisplay": int(ndisplay)}
    for i, ax in enumerate(axes):
        ax = str(ax).upper()
        if ax in ("Y", "X"):
            lo = int(np.floor(corners[0][i] * factors[i]))
            hi = int(np.ceil((corners[1][i] + 1) * factors[i]))   # inclusive → half-open
            out["xy"][ax] = [max(0, lo), hi]
        elif ax in ("Z", "T"):
            # the slider, NOT corner_pixels (see above). No step given → fall back to the corner, which
            # is right only for a viewer that has never been moved.
            idx = step[i] if step is not None else corners[0][i]
            out[ax.lower()] = max(0, int(idx))
    return out


def layer_ndim(data):
  """Dimensionality of a layer's data, accepting a multiscale LIST (level 0 decides) or one array."""
  arr = data[0] if isinstance(data, (list, tuple)) and len(data) else data
  return len(getattr(arr, 'shape', ()) or ())


def align_axis_vector(vec, ndim):
  """Trim/pad a per-axis vector (``scale``, ``units``) to ``ndim`` entries, keeping the TRAILING axes.

  napari requires ``len(scale) == layer.ndim`` and raises
  ``could not broadcast input array from shape (N,) into shape (M,)`` otherwise.

  This is the LAST RESORT, used only when the axis NAMES are unknown or untrustworthy — prefer
  ``expand_to_axes``, which aligns by name. Trailing-axis trimming assumes the array's axes are the
  image's last N, which is true for a (z,y,x) volume of a (t,z,y,x) timelapse and **false** for a
  (t,y,x) Z-projection of one: there it hands the Z scale to the T axis. Padding with 1.0 (a no-op
  scale) handles the reverse case rather than raising. ``None`` passes through unchanged.
  """
  if vec is None or not ndim:
    return vec
  v = list(vec)
  if len(v) == ndim:
    return vec
  if len(v) > ndim:
    return type(vec)(v[-ndim:]) if isinstance(vec, tuple) else v[-ndim:]
  pad = [1.0] * (ndim - len(v))
  out = pad + v
  return type(vec)(out) if isinstance(vec, tuple) else out


def _insert_singleton_axes(arr, positions):
  """Insert length-1 axes at ``positions`` (ascending, in OUTPUT coordinates). Indexing with ``None``
  works identically on numpy, dask and anything else that supports basic indexing, so this needs no
  per-backend branch and stays lazy for a dask-backed store."""
  at = set(positions)
  n_out = len(getattr(arr, 'shape', ())) + len(at)
  return arr[tuple(None if i in at else slice(None) for i in range(n_out))]


def _broadcast_axes(arr, positions, viewer_shape):
  """Stretch the length-1 axes at ``positions`` to the viewer's extent. ``np.broadcast_to`` dispatches
  through ``__array_function__``, so a dask-backed store stays LAZY and costs nothing — the expansion is
  a view, not data (a 201x20x544x548 uint32 curtain would be 4.8 GB if it were ever materialised; napari
  reads one plane at a time, so it never is)."""
  if viewer_shape is None:
    return arr
  shape = list(getattr(arr, 'shape', ()))
  at = set(positions)
  for i in at:
    if i < len(shape) and i < len(viewer_shape) and viewer_shape[i]:
      shape[i] = int(viewer_shape[i])          # only the INSERTED axes; Y/X keep this level's own size
  return np.broadcast_to(arr, tuple(shape)) if tuple(shape) != tuple(getattr(arr, 'shape', ())) else arr


def expand_to_axes(data, layer_axes, viewer_axes, viewer_shape=None):
  """Give a layer array every axis the viewer has, in the viewer's order, by inserting length-1 axes.

  **napari aligns a layer's dimensions against the viewer's from the RIGHT.** A layer with fewer
  dimensions than the viewer is therefore not "missing its leading axes" — its axes are *reinterpreted*
  as the viewer's trailing ones. So a Z-projected timelapse skeleton stored as (t,y,x), added to a
  (t,z,y,x) viewer, has its TIME axis rendered as Z: every timepoint stacked into one volume, a tower
  of frames standing on the image. That is what `segment.branching` produced for a 3D+t SHG store
  (``branchLabels/SHG.zarr`` = (201,544,548) over a 4-axis image), and no amount of fixing up ``scale``
  can correct it — the dimensions themselves are misassigned.

  Aligning by NAME is the fix: insert a singleton for each viewer axis the layer lacks, and the layer's
  own axes land where they belong. A Z-projection then renders on a single Z plane, which is what it
  physically is, and time steps with the slider.

  ``viewer_shape`` (optional, aligned with ``viewer_axes``) additionally **stretches** each inserted axis
  to the viewer's extent, so a Z-PROJECTED store shows on every Z plane instead of on one. That is what a
  projection means — the skeleton was computed on the MIP, so it belongs to the whole volume, not to
  z=0 — and it is what the old R version did, by writing the MIP onto every Z plane *before*
  skeletonising (`create_branching.py`: *"this will propagate the 2D image into 3D — otherwise the
  following steps will be a bit confusing"*). Here it is a lazy broadcast instead of duplicated bytes, so
  the store stays honest about having no Z. In 3D rendering the result is an extruded curtain; in 2D the
  overlay follows the slider through z. Omit it to keep the single plane.

  ``layer_axes``/``viewer_axes`` are axis-name sequences (case-insensitive, e.g. ``['t','y','x']`` and
  ``['t','z','y','x']``); ``viewer_axes`` must already EXCLUDE the channel axis (napari splits channels
  into separate layers). Accepts a multiscale list or a single array.

  Returns ``(data, ok)``. ``ok`` is False — and ``data`` comes back untouched — when the names can't be
  trusted or used: either list missing, a rank/name mismatch (a store whose ``.zattrs`` axes don't match
  the array it holds is exactly the case that must NOT be acted on), a layer axis absent from the
  viewer, or axes out of the viewer's relative order. The caller then falls back to
  ``align_axis_vector``.
  """
  if not layer_axes or not viewer_axes:
    return data, False
  lay = [str(a).lower() for a in layer_axes]
  view = [str(a).lower() for a in viewer_axes]
  if len(set(lay)) != len(lay) or len(set(view)) != len(view):
    return data, False
  if layer_ndim(data) != len(lay):
    return data, False          # metadata does not describe this array — do not act on it
  if len(lay) > len(view):
    return data, False
  try:
    pos = [view.index(a) for a in lay]
  except ValueError:
    return data, False          # the layer has an axis the viewer doesn't
  if pos != sorted(pos):
    return data, False          # transposed relative to the viewer — an insert can't fix that
  if len(lay) == len(view):
    return data, True           # already aligned; nothing to insert
  missing = [i for i in range(len(view)) if i not in set(pos)]
  grow = lambda a: _broadcast_axes(_insert_singleton_axes(a, missing), missing, viewer_shape)
  if isinstance(data, (list, tuple)):
    return [grow(a) for a in data], True
  return grow(data), True


def add_labels(viewer, labels, *, scale, units=None, opacity=0.7, name='Labels', visible=True,
               cache=False, contour=0, axes=None, image_axes=None, image_shape=None):
  """Add an instance/label layer (0 = background) at ``opacity`` (0.7 by default). Returns the layer.

  ``cache=False`` by default (napari's own default is True). napari's Labels layer, with a
  dask-backed input, wires ``configure_dask(data, cache=True)`` — which enables the napari-global
  ``resize_dask_cache()`` (a ``cachey.Cache`` in ``napari.utils._dask_utils``). Dask task names
  from ``da.from_zarr(path)`` are DETERMINISTIC per path/shape/dtype (empirically same across
  separate opens; probe: ``scratchpad/reopen_probe.py``). So after a user re-runs segmentation
  to the same output name, a fresh ``show_labels`` → ``_remove_layer`` → new
  ``da.from_zarr(labels_path)`` → new Labels layer → napari slices → cachey ``HIT`` on the OLD
  task name → the STALE label bytes render. cache-off forces every slice to re-read from disk
  (cheap: uint32 masks, high compression, OS page cache still helps); the correctness cost of
  cache-on is catastrophic in the primary segmentation-iteration workflow. Callers can flip
  ``cache=True`` when they know labels won't be regenerated (e.g. static viewing sessions).

  ``axes``/``image_axes`` are the label store's and the viewer's axis names. Pass both whenever they
  are known: a derived label store can have FEWER axes than the image (a Z-projected skeleton of a
  timelapse), and napari reinterprets a short layer's axes as the viewer's trailing ones, which
  silently renders time as Z. See ``expand_to_axes``; without the names we can only fix up ``scale``,
  which does not fix the dimension assignment. ``image_shape`` (the viewer's extent, channel axis
  excluded) additionally stretches a projected store across the axis it collapsed — a MIP-derived
  skeleton belongs to the whole volume, so it renders on every plane rather than only the first.

  ``contour`` draws each label as an outline of that many pixels instead of a filled region (0, the
  napari default, keeps it filled) — an outline lets the channel signal under the mask stay readable.
  Set at ADD time as well as being a captured view prop, because the paths that re-add a layer without
  then loading the props file — the movie recorder swapping masks between cells — would otherwise come
  back filled."""
  require_napari()
  # ONE place every labels layer goes through — align here, not at each call site.
  labels, aligned = expand_to_axes(labels, axes, image_axes, viewer_shape=image_shape)
  nd = layer_ndim(labels)
  kw = dict(name=name, scale=scale if aligned else align_axis_vector(scale, nd),
            opacity=opacity, visible=visible, cache=bool(cache))
  if units is not None:
    kw['units'] = units if aligned else align_axis_vector(units, nd)
  layer = viewer.add_labels(labels, **kw)
  # `contour` is a settable PROPERTY, not a constructor argument — `Labels.__init__` does not accept it
  # (checked on napari 0.7.1), so passing it in `kw` raises TypeError and takes every mask layer with
  # it. Set after the add, and only when asked, so the default path is byte-for-byte unchanged.
  wanted = max(0, int(contour or 0))
  if wanted:
    layer.contour = wanted
  return layer


def add_tracks(viewer, tracks, *, scale, units=None, color_by='track_id', colormap='turbo',
               colormaps_dict=None, tail_width=4, tail_length=30, properties=None,
               blending='additive', visible=True, name='Tracks'):
  """Add a tracks layer from a ``[track_id, t, (z), y, x]`` matrix in PIXEL coords (``scale`` supplies
  the µm conversion, matching the image/labels layers). ``color_by='track_id'`` → the named
  ``colormap`` (turbo); pass ``colormaps_dict`` (per-value RGBA) for a categorical colour-by (Okabe–Ito)
  and it wins over ``colormap``. Returns the layer."""
  require_napari()
  kw = dict(name=name, scale=scale, color_by=color_by, tail_width=tail_width,
            tail_length=tail_length, blending=blending, visible=visible)
  if units is not None:
    kw['units'] = units
  if properties is not None:
    kw['properties'] = properties
  if colormaps_dict is not None:
    kw['colormaps_dict'] = colormaps_dict
  else:
    kw['colormap'] = colormap
  return viewer.add_tracks(tracks, **kw)


# ── Colour conversion (hex ↔ RGBA float) ──────────────────────────────────────
# Shared so every napari colour path parses hex ONE way — labels/tracks colormaps, points
# face_color, and the solid track colormap below. cecelia's bridge imports these; coastal can too.

def hex_to_rgba(hex_colour):
  """``'#rrggbb'`` → ``(r, g, b, 1.0)`` floats in 0..1. None / blank / malformed → None."""
  if not hex_colour:
    return None
  s = str(hex_colour).lstrip('#')
  if len(s) != 6:
    return None
  try:
    return (int(s[0:2], 16) / 255., int(s[2:4], 16) / 255., int(s[4:6], 16) / 255., 1.0)
  except ValueError:
    return None


def rgba_to_hex(rgba):
  """``(r, g, b, a)`` floats in 0..1 → ``'#rrggbb'`` (alpha dropped, channels clamped to 0..255)."""
  return '#{:02x}{:02x}{:02x}'.format(
    *(max(0, min(255, int(round(c * 255)))) for c in rgba[:3]))


def solid_track_colormap(hex_colour, name='cc_pop'):
  """A **black → colour** two-stop colormap for flat-colouring a Tracks layer in a single population
  colour — the idiom the old R viewer used (``cmap_single(['#000000', colour])`` in
  ``show_tracks(split_tracks=…)``). A napari Tracks layer colours ONLY via ``color_by`` + a colormap:
  colouring by ``track_id`` keeps napari's built-in turbo (a custom colormap there is ignored), and a
  same-colour "solid" map doesn't take either. The working recipe: colour by a helper property whose
  value is a NONZERO constant, mapped through this black→colour map — the value lands at the colour end
  (0 = black is never hit), so every track in the pop renders in ``hex_colour``. See old-R
  ``inst/py/napari_utils.py::show_tracks``."""
  napari = require_napari()
  rgba = hex_to_rgba(hex_colour) or (1.0, 1.0, 1.0, 1.0)   # malformed colour → white (never crash a render)
  return napari.utils.Colormap([(0.0, 0.0, 0.0, 1.0), rgba], name=name)


# ── 3D crop (axis-aligned clipping planes) ────────────────────────────────────

def axis_aligned_clip_planes(layer, world_box, display_axes):
  """Build napari ``experimental_clipping_planes`` for an axis-aligned crop box, expressed in the
  LAYER's own data coordinates.

  ``world_box`` maps a spatial axis label (``'z'``/``'y'``/``'x'``) → ``(lo, hi)`` in WORLD (µm)
  coordinates; ``display_axes`` is the viewer's non-channel axis order (e.g. ``['t','z','y','x']``).
  Layers are right-aligned to the viewer dims, so a layer with fewer dims (e.g. no ``t``) still maps
  correctly. Each axis contributes two planes — keep ``>= lo`` (normal ``+axis``) and ``<= hi`` (normal
  ``-axis``); napari intersects all enabled planes, so the kept region is the box. Returns ``[]`` when
  no requested axis lands inside the layer.

  napari clipping planes are **3-D constructs**: ``position``/``normal`` are always 3-vectors over the
  DISPLAYED spatial dims (the layer's last 3 = z,y,x), regardless of the layer's ndim. Emitting
  ndim-length vectors for a 4-D (t,z,y,x) layer makes napari reject them silently → nothing clips.
  Returns ``[]`` for a layer with < 3 dims (clipping is a volume-render feature).

  Pure geometry (no napari import): unit-testable with a lightweight stand-in exposing
  ``ndim``/``scale``/``translate``. World→data uses the layer's own ``scale``/``translate`` (each layer
  may differ — image, labels, tracks and points can carry distinct scales), so a single world box clips
  them all consistently."""
  nd = int(layer.ndim)
  if nd < 3:
    return []                                # clipping planes are 3-D; a 2-D layer has no volume to clip
  n_disp = 3                                  # position/normal are 3-vectors over the displayed z,y,x
  disp_dims = list(range(nd - n_disp, nd))    # the last 3 layer dims = the displayed spatial axes
  scale = np.asarray(layer.scale, dtype=float)
  translate = np.asarray(layer.translate, dtype=float)
  offset = len(display_axes) - nd            # dims are trailing-aligned to the viewer axes
  planes = []
  for ax, bounds in world_box.items():
    if ax not in display_axes:
      continue
    ldim = display_axes.index(ax) - offset   # this axis's index in the layer's own dims
    if ldim not in disp_dims:
      continue                               # not a displayed spatial dim (e.g. t) → can't clip on it
    vi = disp_dims.index(ldim)               # its slot in the 3-vector
    s = scale[ldim] if ldim < len(scale) and scale[ldim] else 1.0
    tr = translate[ldim] if ldim < len(translate) else 0.0
    lo_d = (float(bounds[0]) - tr) / s
    hi_d = (float(bounds[1]) - tr) / s
    for coord, sign in ((lo_d, 1.0), (hi_d, -1.0)):
      pos = [0.0] * n_disp; pos[vi] = coord
      nrm = [0.0] * n_disp; nrm[vi] = sign
      planes.append({"position": tuple(pos), "normal": tuple(nrm), "enabled": True})
  return planes


# ── Categorical vs numeric obs column (the shared colour-by rule) ──────────────
# Up to this many distinct integer levels reads as a categorical code set; above that an integer
# column is a numeric count. MUST match Julia ``_MAX_CATEGORICAL_LEVELS``
# (app/src/tracking/track_props.jl) so napari colour-by, the plots and the pop manager all agree.
MAX_CATEGORICAL_LEVELS = 20


def is_categorical_column(column, uniq):
  """Whether an obs ``column`` (with distinct finite numeric values ``uniq``) is a categorical code
  set. Mirrors Julia ``_is_categorical_col`` (app/src/tracking/track_props.jl) — the ONE rule, shared
  so tracks/labels colour-by matches the rest of the app:

    • ``clusters`` / ``clusters.*`` → always categorical (name-rule: a high-resolution run can exceed
      the level cap, but cluster codes are never a count);
    • otherwise an all-integer column with ≤ ``MAX_CATEGORICAL_LEVELS`` distinct levels
      (e.g. ``hmm.state`` ∈ {1,2,3}).

  Non-numeric columns (strings / anndata categoricals) are categorical too, but the caller detects
  those before reaching here (they factorise to codes); this handles the numeric case."""
  if column == 'clusters' or column.startswith('clusters.'):
    return True
  uniq = np.asarray(uniq)
  return len(uniq) <= MAX_CATEGORICAL_LEVELS and np.allclose(uniq, np.round(uniq))


def broadcast_track_to_cells(cell_track_ids, track_labels, track_values):
  """Map each cell's ``track_id`` to its track's value (``track_labels[i] -> track_values[i]``) so a
  TRACK-level obs column (e.g. ``clusters.*`` from clustTracks) can colour cells + track vertices by
  the cell's track cluster/population. Cells with no / zero / absent track_id → ``np.nan`` (untracked
  → grey). Returns an object array aligned to ``cell_track_ids``. Ports R ``split_tracks``' colour-by
  -cluster; the bridge's colour-by uses it when a column is absent from the cell table."""
  tmap = {int(l): v for l, v in zip(track_labels, track_values)}
  out = np.empty(len(cell_track_ids), dtype=object)
  for i, t in enumerate(cell_track_ids):
    ok = t is not None and not (isinstance(t, float) and t != t) and int(t) > 0   # t != t → NaN
    out[i] = tmap.get(int(t), np.nan) if ok else np.nan
  return out


# ── View snapshot (the "view state" atom) ─────────────────────────────────────
# A durable, JSON-safe description of a viewer: camera + dims (incl. the T/Z slider) + each layer's
# display props, all as SETTABLE SCALAR values (colormap by NAME, enums as strings, arrays as lists).
# We own this schema rather than persisting napari's own ViewerState objects, whose captured dicts hold
# napari enums / pint Units / ColorArrays that tie stored data to napari's internal types across
# versions. Storing settable scalars keeps a snapshot durable, human-readable, GUI-editable, and
# re-applied by plain setattr. See docs/todo/ANIMATION_PLAN.md (Decision 1). Reused by the bridge
# (capture at screenshot / zoom-to-source) and available to coastal.

_VIEW_CAMERA_KEYS = ('center', 'zoom', 'angles', 'perspective')
# per-layer display props to capture/restore; guarded — a layer type lacking one is simply skipped.
# `contour` is Labels-only (0 = filled, N = an N-px outline), which costs an image layer nothing here
# and means an outline the user set survives a re-open: both the props file and the movie recorder's
# per-cell view apply go through these keys.
_VIEW_LAYER_KEYS = ('visible', 'opacity', 'blending', 'gamma', 'contrast_limits', 'colormap',
                    'rendering', 'interpolation2d', 'interpolation3d', 'depiction', 'contour')


def _json_scalar(v):
  """Coerce a napari attribute to a JSON-safe scalar/list: ndarray/tuple → list, numpy number →
  python number, Enum → its string value; everything else (str/int/float/bool/None) passes through."""
  import enum
  if isinstance(v, np.ndarray):
    return v.tolist()
  if isinstance(v, np.floating):
    return float(v)
  if isinstance(v, np.integer):
    return int(v)
  if isinstance(v, enum.Enum):
    return v.value
  if isinstance(v, (list, tuple)):
    return [_json_scalar(x) for x in v]
  if v is None or isinstance(v, (str, int, float, bool)):
    return v
  return str(v)  # last resort: never let an unexpected type break json.dumps of a snapshot


def capture_layer_props(layer):
  """One layer's display props as JSON-safe scalars — the per-layer half of ``capture_view_state``.

  Separate because two callers need exactly this and neither wants the rest of a view snapshot: the
  task preview re-applies a layer's props across a re-preview (it removes and re-adds its layers, which
  would otherwise reset the contrast the user just set), and it must NOT restore the camera or the T/Z
  position — a re-preview happens *because* those moved.
  """
  props = {}
  for k in _VIEW_LAYER_KEYS:
    if not hasattr(layer, k):
      continue
    val = getattr(layer, k)
    if val is None:
      continue
    if k == 'colormap':
      val = getattr(val, 'name', None)  # store the settable NAME, not the ColorArray object
      if val is None:
        continue
    props[k] = _json_scalar(val)
  return props


def apply_layer_props(layer, props):
  """Set display props back onto ``layer`` — the per-layer half of ``apply_view_state``.

  Every ``setattr`` is guarded: a prop can be unsettable on this layer type, and a restored
  ``contrast_limits`` can fall outside the new data's range (exactly the task-preview case, where each
  re-preview brings a differently-scaled block). Skipping beats raising — the layer is already on
  screen and a lost contrast window is a smaller loss than a failed preview.
  """
  for k, v in (props or {}).items():
    if v is None:
      continue
    try:
      setattr(layer, k, v)
    except Exception:
      pass


def capture_view_state(viewer):
  """Capture a JSON-safe view snapshot from ``viewer`` — camera, dims (incl. the T/Z slider position),
  and each layer's display props (colormap by NAME, contrast, visibility, …). Duck-typed: reads only
  public napari attributes and returns a plain dict ready for ``json.dumps``. Robust to missing
  attributes (each read is guarded). See docs/todo/ANIMATION_PLAN.md Phase A."""
  camera = {}
  for k in _VIEW_CAMERA_KEYS:
    try:
      camera[k] = _json_scalar(getattr(viewer.camera, k))
    except Exception:
      pass
  dims = {}
  for k in ('ndisplay', 'order', 'current_step', 'point'):
    try:
      dims[k] = _json_scalar(getattr(viewer.dims, k))
    except Exception:
      pass
  layers = {layer.name: capture_layer_props(layer) for layer in viewer.layers}
  return {'camera': camera, 'dims': dims, 'layers': layers}


def apply_view_state(viewer, snapshot):
  """Re-apply a snapshot from ``capture_view_state`` to ``viewer``: camera, dims (the T/Z position,
  clamped to this image's extent), and each PRESENT layer's display props. Missing layers and
  unsettable attributes are skipped silently (every ``setattr`` is guarded), so a snapshot degrades
  gracefully when the reopened image has fewer layers than when it was captured. Returns True."""
  snapshot = snapshot or {}
  for k, v in (snapshot.get('camera') or {}).items():
    if k in _VIEW_CAMERA_KEYS:
      try:
        setattr(viewer.camera, k, v)
      except Exception:
        pass
  dims = snapshot.get('dims') or {}
  for k in ('ndisplay', 'order'):
    if k in dims:
      try:
        setattr(viewer.dims, k, dims[k] if k == 'ndisplay' else tuple(dims[k]))
      except Exception:
        pass
  step = dims.get('current_step')
  if step is not None:
    try:
      cur = list(viewer.dims.current_step)
      nsteps = viewer.dims.nsteps
      for i in range(len(cur)):
        if i < len(step) and i < len(nsteps):
          cur[i] = max(0, min(int(step[i]), int(nsteps[i]) - 1))
      viewer.dims.current_step = tuple(cur)
    except Exception:
      pass
  layers = getattr(viewer, 'layers', None)
  for name, props in (snapshot.get('layers') or {}).items():
    if layers is None or name not in layers:
      continue
    apply_layer_props(layers[name], props)
  return True


# ── Movie recording (timelapse T-sweep) ───────────────────────────────────────
# Record a viewer's time axis to an .mp4 by interpolating the dims T slider between two keyframes.
# The batch-movie ("generateMovies") building block: apply a view (channels/pops/colour-by) then call
# this to sweep T → one attr-named file per image. Uses napari-animation (PyPI) + imageio-ffmpeg (both
# in the pixi env); kept here (not the bridge) so it's a shared, testable primitive. See
# docs/todo/ANIMATION_PLAN.md (Phase F1) and docs/NAPARI.md.

# ── Title card (Phase H) — prepend a description slide to a recorded movie ─────
def _visible_channel_legend(viewer):
  """``[{label, colour}]`` for each visible Image layer: channel name + its colormap's max colour as
  hex. Channel colour lives ONLY in the napari layer state (ANIMATION_PLAN.md Phase H, decision 5), so
  the title card reads it here rather than duplicating a colormap→hex table. (A grayscale ramp maps its
  max to white; single-hue channels give their hue — the common case.)"""
  try:
    from napari.layers import Image as _ImageLayer
  except Exception:
    _ImageLayer = None
  out = []
  for layer in getattr(viewer, "layers", []):
    if _ImageLayer is not None and not isinstance(layer, _ImageLayer):
      continue
    if not getattr(layer, "visible", False):
      continue
    colour = None
    try:
      rgba = layer.colormap.map(np.array([1.0]))[0]
      colour = "#{:02x}{:02x}{:02x}".format(
        int(round(float(rgba[0]) * 255)), int(round(float(rgba[1]) * 255)), int(round(float(rgba[2]) * 255)))
    except Exception:
      pass
    out.append({"label": str(getattr(layer, "name", "")), "colour": colour})
  return out


def canvas_size(viewer):
  """The size a canvas-only screenshot of ``viewer`` comes out at, as ``(height, width)`` in OUTPUT
  pixels — i.e. the size a movie is recorded at when no explicit size is asked for. None if it can't be
  read (a headless/duck-typed viewer).

  Read off the CANVAS widget, not ``viewer._canvas_size``: that model field is only refreshed by a
  resize event, so before the first one it still holds napari's ``(800, 600)`` default — which is
  ``(width, height)`` while everything else here is ``(height, width)``. Trusting it reports a
  TRANSPOSED size for a viewer that hasn't been resized yet, and a transposed placeholder is worse than
  none. ``VispyCanvas.size`` is documented ``(height, width)`` and always current.

  Then multiply by the device-pixel ratio: the canvas size is in LOGICAL pixels, while a screenshot
  renders the GL framebuffer, which is ``logical × devicePixelRatio`` (the same arithmetic napari's own
  ``resize_canvas`` does in reverse). On a HiDPI display those differ by 2×, so the logical size would be
  wrong on exactly the machines people edit figures on.
  """
  try:
    qt = viewer.window._qt_viewer                    # no public accessor for the canvas widget
    h, w = (int(v) for v in qt.canvas.size)          # (height, width), as screenshot(size=) takes
  except Exception:
    return None
  if h <= 0 or w <= 0:
    return None
  ratio = 1.0
  for get in (qt.devicePixelRatio, _qt_screen_ratio):
    try:
      r = float(get())
      if r > 0:
        ratio = r
        break
    except Exception:
      continue
  return (int(round(h * ratio)), int(round(w * ratio)))


def _qt_screen_ratio():
  """The primary screen's device-pixel ratio — the fallback when the viewer's own widget can't say."""
  from qtpy.QtGui import QGuiApplication
  return QGuiApplication.primaryScreen().devicePixelRatio()


@contextlib.contextmanager
def overlays_hidden(viewer, scale_bar=False, timestamp=False):
  """Temporarily hide napari's BAKED overlays — the scale bar and the elapsed-time text — restoring
  whatever they were on the way out. Both are drawn into the canvas, so they are burnt into every
  screenshot and every movie frame; this is the only way to leave them out of the output without
  changing what the user sees in the window afterwards.

  One helper for both consumers — the publication still (`save_screenshot(clean=True)`) and the movie
  recorders — because they are the same three lines of get/set/restore, and the still had them inline
  first. Each flag is independent: a figure often wants the timestamp burnt in and the scale bar added
  as vector art later, or the other way round.

  Every access is guarded: a duck-typed or headless viewer simply has no `scale_bar`/`text_overlay`,
  and a missing overlay must not fail a render that is otherwise fine."""
  saved = {}
  for name, hide in (("scale_bar", scale_bar), ("text_overlay", timestamp)):
    if not hide:
      continue
    try:
      overlay = getattr(viewer, name)
      saved[name] = overlay.visible
      overlay.visible = False
    except Exception:
      pass
  try:
    yield
  finally:
    for name, was in saved.items():
      try:
        getattr(viewer, name).visible = was
      except Exception:
        pass


_STALE_STAGING_S = 3600


def _clear_stale_staging(path, older_than_s=_STALE_STAGING_S):
  """Delete leftover `*.mp4.tmp.mp4` files beside ``path``. Best-effort, and it announces what it removes.

  A cancelled or failed record cleans up after itself. This is for the case that cannot: the bridge
  process being KILLED mid-render (napari force-quit, a machine going down), which runs no cleanup.
  Such a file is invisible — `/api/movies` filters `.tmp.` out of the listing — and nothing sweeps it:
  the `store-debris` patch walks DIRECTORIES in store locations, so a stray file in `movies/` is
  outside it. Recording is serial on the one viewer, so no other record can own one of these; the age
  guard is belt-and-braces.
  """
  import os
  import time as _time
  folder = os.path.dirname(path) or "."
  try:
    names = os.listdir(folder)
  except OSError:
    return
  cutoff = _time.time() - older_than_s
  for name in names:
    if not name.endswith(".mp4.tmp.mp4"):
      continue
    full = os.path.join(folder, name)
    try:
      if os.path.getmtime(full) < cutoff:
        os.remove(full)
        print(f"[record] removed a leftover partial from a killed run: {name}", flush=True)
    except OSError:
      pass


def _render_animation(viewer, anim, path, *, fps, canvas_only, size=None,
                      on_progress=None, should_cancel=None):
  """Write ``anim``'s interpolated frames to ``path`` (mp4), one frame at a time. Returns the frame count.

  This is the loop napari-animation's ``animate()`` used to run for us. We own it for one reason: the
  output size. ``animate()`` exposes only ``scale_factor``, which ``ndi.zoom``s the finished screenshot —
  pixels without detail. napari's own ``Viewer.screenshot(size=…)`` re-renders the canvas at a requested
  size, but ``FrameSequence.iter_frames`` never passes one, so the knob we want sits one layer below the
  one we called. We keep napari-animation for the part worth having — keyframe INTERPOLATION — and do the
  rendering ourselves. See docs/NAPARI.md.

  ``size`` is ``(height, width)`` or None for the canvas size. **Apply the state first, screenshot
  second** — that ordering is the whole feature. vispy holds the camera's world rect across a canvas
  resize, so applying the keyframe at the live canvas size and THEN screenshotting at ``size`` keeps the
  framing and raises the resolution. Resize the canvas first and each keyframe's ``camera.zoom`` is
  reinterpreted against the bigger canvas: same magnification, wider field, black margins — a different
  movie, and the failure would look like a bug in the keyframes.

  ``on_progress(i, total)`` is called per frame (throttle in the caller — this loop does not) and
  ``should_cancel()`` is polled per frame; a true reading raises ``RecordCancelled``. Together they are
  what lets a single record behave like a batch on the task rail: a progress bar and a working Cancel.

  **Staged**: frames go to a ``.tmp.mp4`` sibling, promoted onto ``path`` only once the last one is
  written. A movie is named after the IMAGE, so a re-record targets the path of the previous one — write
  in place and a cancel (or a crash) replaces a good movie with a file that has no moov atom and plays
  nowhere. `/api/movies` already hides `.tmp.` names, and nothing sweeps them: the `store-debris` patch
  walks directories in store locations only. Same scheme as ``title_card.prepend_title_to_movie``.
  """
  import os
  from cecelia.utils.movie_io import coerce_movie_size, crop_to_even, movie_writer

  states = _frame_sequence(anim)
  hw, notes = coerce_movie_size(size)
  for n in notes:
    print(f"[WARN] {n}", flush=True)
  if hw is not None and not canvas_only:
    # napari applies `size` to the canvas render only; with the viewer chrome included it is ignored
    # outright, so say so rather than writing a window-sized movie that claims to be the asked-for one.
    print("[WARN] a movie size applies to canvas-only recordings — recording at the window size",
          flush=True)
    hw = None
  shot = {"size": hw} if hw is not None else {}
  total = len(states)
  print(f"[record] {total} frames at {'x'.join(map(str, reversed(hw))) if hw else 'canvas size'}",
        flush=True)

  # The temp KEEPS the .mp4 extension — imageio infers the writer format from it, so `atomic_io`'s
  # `x.mp4.tmp.<uid>` scheme can't be used (same reason, and same name, as the title-card prepend).
  staging = f"{path}.tmp.mp4"
  _clear_stale_staging(path)
  written = 0
  try:
    with movie_writer(staging, fps) as out:
      for i, state in enumerate(states):
        if should_cancel is not None and should_cancel():
          raise RecordCancelled(written)
        state.apply(viewer)                     # the keyframe's framing, at the live canvas size
        frame = viewer.screenshot(canvas_only=canvas_only, flash=False, **shot)
        out.append_data(crop_to_even(frame))
        written += 1
        if on_progress is not None:
          on_progress(written, total)
        if written % 25 == 0:                   # the render can run for minutes; don't go silent
          print(f"[record] {written}/{total}", flush=True)
  except BaseException:
    # Cancelled, or the render/encode died. Either way the staged file is unplayable and the previous
    # movie at `path` is still intact — leave it that way.
    try:
      os.remove(staging)
    except OSError:
      pass
    raise
  os.replace(staging, path)                     # promote: the movie appears whole or not at all
  return written


def _maybe_prepend_title(viewer, path, title_card):
  """If a title card is enabled, prepend it to the just-recorded movie: add a Channels section read from
  the live viewer (unless the payload ALREADY carries one — the animation page supplies a union across
  all keyframes, which the single live view can't reconstruct), prepend it to the caller's sections
  (populations / colour-by, …), and composite via ``cecelia.utils.title_card``. Best-effort — a failure
  logs and leaves the movie untouched; it never fails the recording."""
  if not title_card or not title_card.get("enabled"):
    return
  try:
    from cecelia.utils import title_card as _tc
    sections = list(title_card.get("sections") or [])
    has_channels = any((s.get("heading") or "").strip().lower() == "channels" for s in sections)
    if not has_channels:
      channels = _visible_channel_legend(viewer)
      if channels:
        sections = [{"heading": "Channels", "items": channels}] + sections
    content = {"title": title_card.get("title", ""), "note": title_card.get("note", ""), "sections": sections}
    _tc.prepend_title_to_movie(path, content, duration_sec=float(title_card.get("durationSec", 3.0)))
  except Exception as e:
    print(f"[WARN] title card skipped: {e}")


def record_timelapse(viewer, path, *, t_axis_index, n_timepoints, fps=15,
                     canvas_only=True, size=None, t_start=0, t_end=None, title_card=None,
                     on_progress=None, should_cancel=None):
  """Record ``viewer``'s T-sweep (dims slider index ``t_axis_index``) from ``t_start``..``t_end``
  (default the full ``n_timepoints`` range) to ``path`` (an ``.mp4``), one frame per timepoint, at
  ``fps``. ``canvas_only`` excludes the napari UI chrome. ``size`` is ``(height, width)`` in pixels, or
  None for the napari canvas size (the default) — see ``_render_animation`` and
  docs/NAPARI.md.
  ``title_card`` (Phase H) optionally prepends a description slide after recording. ``on_progress(i,
  total)``/``should_cancel()`` drive the task rail (see ``_render_animation``); a cancel raises
  ``RecordCancelled`` and leaves the previous movie untouched. Returns the number of frames written.
  Raises ``ValueError`` for a single-timepoint stack. Ports the old R
  ``generateMovies`` T-playback: two keyframes (first/last T) + linear slider interpolation."""
  n = int(n_timepoints)
  if n <= 1:
    raise ValueError("record_timelapse needs a stack with >1 timepoint (no time axis to sweep)")
  t0 = max(0, int(t_start))
  t1 = (n - 1) if t_end is None else min(int(t_end), n - 1)
  if t1 <= t0:
    raise ValueError(f"record_timelapse: empty T range [{t0}, {t1}]")
  Animation = _require_napari_animation()

  def _set_t(t):
    step = list(viewer.dims.current_step)
    step[t_axis_index] = int(t)
    viewer.dims.current_step = tuple(step)

  anim = Animation(viewer)
  _set_t(t0); anim.capture_keyframe()
  _set_t(t1); anim.capture_keyframe(steps=(t1 - t0))   # one interpolated frame per timepoint between
  frames = _render_animation(viewer, anim, path, fps=int(fps), canvas_only=canvas_only, size=size,
                             on_progress=on_progress, should_cancel=should_cancel)
  _maybe_prepend_title(viewer, path, title_card)        # Phase H: optional description slide
  return frames


def record_keyframes(viewer, path, keyframes, *, fps=15, canvas_only=True, size=None, title_card=None,
                     on_progress=None, should_cancel=None):
  """Render an interpolated keyframe animation to ``path`` (mp4). Each keyframe carries a saved view
  state (``{"viewState": {...}, "steps": N}``); we apply it to ``viewer`` and capture it as a
  napari-animation keyframe with ``steps`` interpolated frames FROM the previous keyframe — so the
  movie tweens between views (camera pans/zooms, contrast/colour fades, T scrubbing). The first
  keyframe just starts the sequence (its ``steps`` is ignored). Needs ≥ 2 keyframes. The "super-simple
  OpenShot" render path; see docs/todo/ANIMATION_PLAN.md (F2/H4). ``size`` is ``(height, width)`` or None
  for the canvas size, as for ``record_timelapse`` (docs/NAPARI.md).
  ``title_card`` (Phase H4) optionally prepends a description slide. Returns the frame count."""
  if len(keyframes) < 2:
    raise ValueError("record_keyframes needs at least 2 keyframes")
  Animation = _require_napari_animation()
  anim = Animation(viewer)
  for i, kf in enumerate(keyframes):
    apply_view_state(viewer, kf.get("viewState") or {})
    steps = 15 if i == 0 else max(1, int(kf.get("steps", 15)))   # first keyframe: no in-transition
    anim.capture_keyframe(steps=steps)
  frames = _render_animation(viewer, anim, path, fps=int(fps), canvas_only=canvas_only, size=size,
                             on_progress=on_progress, should_cancel=should_cancel)
  # Phase H4: the animation card carries its OWN Channels section (a union across all keyframes, built
  # by the frontend), so _maybe_prepend_title uses that and does not read the live viewer here.
  _maybe_prepend_title(viewer, path, title_card)
  return frames


def _require_napari_animation():
  """Return napari-animation's ``Animation`` class, or raise a clear message (it's a PyPI env dep)."""
  try:
    from napari_animation import Animation
  except ImportError as e:  # pragma: no cover - environment-dependent
    raise ImportError(
      "napari-animation is required to record movies — it ships in the pixi env (PyPI, not conda-forge); "
      "`pip install cecelia` does not include it."
    ) from e
  return Animation


def _frame_sequence(anim):
  """The interpolated frame states for ``anim`` — napari-animation's own interpolation, which is the
  part of it we still want (see ``_render_animation`` for the part we replaced).

  Built from the PUBLIC keyframe list rather than reaching for ``anim._frames``: same sequence, no
  private attribute. States are interpolated lazily on indexing, so iterating stays one frame at a time.
  A seam of its own (like ``_require_napari_animation``) so tests can stub it without importing napari."""
  from napari_animation.frame_sequence import FrameSequence
  return FrameSequence(anim.key_frames)
