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
