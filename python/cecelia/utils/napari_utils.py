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

import numpy as np

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
              colormaps=None, contrast=True, blending='additive', visible=True):
  """Add a (possibly multi-channel) image — one layer per channel via ``channel_axis``, per-channel
  ``colormaps``, additive ``blending``, and (optionally) contrast from a middle sample. Returns the
  added layer, or the list of per-channel layers when ``channel_axis`` is set."""
  require_napari()
  kw = dict(channel_axis=channel_axis, name=image_layer_name(channel_names, channel_axis),
            colormap=colormaps, scale=scale, visible=visible)
  if units is not None:
    kw['units'] = units
  if blending is not None:
    kw['blending'] = blending
  result = viewer.add_image(data, **kw)
  if contrast:
    for layer in (result if isinstance(result, list) else [result]):
      if getattr(layer, 'visible', True):
        set_contrast_from_sample(layer)
  return result


def add_labels(viewer, labels, *, scale, units=None, opacity=0.7, name='Labels', visible=True):
  """Add an instance/label layer (0 = background) at ``opacity`` (0.7 by default). Returns the layer."""
  require_napari()
  kw = dict(name=name, scale=scale, opacity=opacity, visible=visible)
  if units is not None:
    kw['units'] = units
  return viewer.add_labels(labels, **kw)


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


def solid_track_colormap(hex_colour, name='cc_pop'):
  """A **black → colour** two-stop colormap for flat-colouring a Tracks layer in a single population
  colour — the idiom the old R viewer used (``cmap_single(['#000000', colour])`` in
  ``show_tracks(split_tracks=…)``). A napari Tracks layer colours ONLY via ``color_by`` + a colormap:
  colouring by ``track_id`` keeps napari's built-in turbo (a custom colormap there is ignored), and a
  same-colour "solid" map doesn't take either. The working recipe: colour by a helper property whose
  value is a NONZERO constant, mapped through this black→colour map — the value lands at the colour end
  (0 = black is never hit), so every track in the pop renders in ``hex_colour``. See old-R
  ``inst/py/napari_utils.py::show_tracks``."""
  require_napari()
  import napari
  s = str(hex_colour).lstrip('#')
  rgba = (int(s[0:2], 16) / 255., int(s[2:4], 16) / 255., int(s[4:6], 16) / 255., 1.0)
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
# per-layer display props to capture/restore; guarded — a layer type lacking one is simply skipped
_VIEW_LAYER_KEYS = ('visible', 'opacity', 'blending', 'gamma', 'contrast_limits', 'colormap',
                    'rendering', 'interpolation2d', 'interpolation3d', 'depiction')


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
  layers = {}
  for layer in viewer.layers:
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
    layers[layer.name] = props
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
    layer = layers[name]
    for k, v in props.items():
      if v is None:
        continue
      try:
        setattr(layer, k, v)
      except Exception:
        pass
  return True


# ── Movie recording (timelapse T-sweep) ───────────────────────────────────────
# Record a viewer's time axis to an .mp4 by interpolating the dims T slider between two keyframes.
# The batch-movie ("generateMovies") building block: apply a view (channels/pops/colour-by) then call
# this to sweep T → one attr-named file per image. Uses napari-animation (PyPI) + imageio-ffmpeg (both
# in the pixi env); kept here (not the bridge) so it's a shared, testable primitive. See
# docs/todo/ANIMATION_PLAN.md (Phase F1) and docs/NAPARI.md.

def record_timelapse(viewer, path, *, t_axis_index, n_timepoints, fps=15,
                     canvas_only=True, scale=1, t_start=0, t_end=None):
  """Record ``viewer``'s T-sweep (dims slider index ``t_axis_index``) from ``t_start``..``t_end``
  (default the full ``n_timepoints`` range) to ``path`` (an ``.mp4``), one frame per timepoint, at
  ``fps``. ``canvas_only`` excludes the napari UI chrome; ``scale`` supersamples (2 = 2× resolution).
  Returns the number of frames written. Raises ``ValueError`` for a single-timepoint stack. Ports the
  old R ``generateMovies`` T-playback: two keyframes (first/last T) + linear slider interpolation."""
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
  anim.animate(path, fps=int(fps), canvas_only=canvas_only, scale_factor=scale)
  return (t1 - t0) + 1


def record_keyframes(viewer, path, keyframes, *, fps=15, canvas_only=True):
  """Render an interpolated keyframe animation to ``path`` (mp4). Each keyframe carries a saved view
  state (``{"viewState": {...}, "steps": N}``); we apply it to ``viewer`` and capture it as a
  napari-animation keyframe with ``steps`` interpolated frames FROM the previous keyframe — so the
  movie tweens between views (camera pans/zooms, contrast/colour fades, T scrubbing). The first
  keyframe just starts the sequence (its ``steps`` is ignored). Needs ≥ 2 keyframes. The "super-simple
  OpenShot" render path; see docs/todo/ANIMATION_PLAN.md (F2). Returns the frame count."""
  if len(keyframes) < 2:
    raise ValueError("record_keyframes needs at least 2 keyframes")
  Animation = _require_napari_animation()
  anim = Animation(viewer)
  for i, kf in enumerate(keyframes):
    apply_view_state(viewer, kf.get("viewState") or {})
    steps = 15 if i == 0 else max(1, int(kf.get("steps", 15)))   # first keyframe: no in-transition
    anim.capture_keyframe(steps=steps)
  anim.animate(path, fps=int(fps), canvas_only=canvas_only)
  return sum(max(1, int(kf.get("steps", 15))) for kf in keyframes[1:]) + 1


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
