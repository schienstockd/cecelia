"""Headless WebGPU host for the viewer's MIP shader — the movie side of the shared renderer.

Runs ``shaders/mip.wgsl`` and its two overlay passes (``mip_points.wgsl``, ``mip_segments.wgsl``),
expanded by ``wgsl_utils``, through ``wgpu-py`` on Vulkan / Metal / DX12, or on a software adapter
where there is no GPU. It only UPLOADS: the volume, the LUT, the label palette, the overlay
instances and the uniform block, in the layouts ``volumeRenderer.ts`` uses. The drawing is the
viewer's shader (``docs/todo/SHARED_RENDERER_PLAN.md``).

What the viewer computes on the CPU before upload is mirrored here, and each mirror is pinned to
the viewer by ``shaders/golden.json``, which ``shaderSource.test.ts`` runs against the TS:
  - ``view_camera``: a view state's camera → yaw/pitch/dist/pan (``applyViewStateToBrowser``).
  - ``lut_rows``: LUT stops → the LUT texture (``lutTextureBytes``).
  - ``label_palette``: ``shaders/label_palette.json``, which the TS test checks against
    ``labelPaletteBytes``.

The target is ``rgba8unorm-srgb``, the format the viewer's canvas draws through
(``canvasFormat.ts``): the shader writes linear colour and the view encodes it.
"""
import json
import math
from typing import Mapping, Optional, Sequence

import numpy as np

from cecelia.utils import wgsl_utils

TARGET_FORMAT = "rgba8unorm-srgb"
#: floats per overlay instance — ``POINT_STRIDE`` / ``SEG_STRIDE`` in ``utils/viewerOverlays.ts``.
POINT_STRIDE, SEG_STRIDE = 7, 10


def _native_arm_processor() -> None:
    """Make ``platform.processor()`` agree with the process on Apple Silicon, before wgpu loads.

    wgpu's Metal backend imports ``rubicon-objc``, which picks its message-send symbols from
    ``platform.processor()`` — and that shells out to ``uname -p``. Under a parent running in Rosetta
    (an Intel Julia on an Apple Silicon Mac) the child ``uname`` runs translated too and says
    ``i386``, while this Python is native arm64: rubicon then binds the Intel-only
    ``objc_msgSendSuper_stret`` and the import dies (``dlsym … symbol not found``). The kernel's
    answer for THIS process (``os.uname().machine``) is the one to trust."""
    import os
    import platform
    import sys
    if sys.platform != "darwin" or os.uname().machine != "arm64":
        return
    if not platform.processor().startswith("arm"):
        platform.processor = lambda: "arm"


def _wgpu():
    _native_arm_processor()
    import wgpu
    return wgpu


def request_adapter(prefer: str = "high-performance"):
    """The best adapter wgpu offers, else the software fallback; ``None`` if there is neither."""
    wgpu = _wgpu()
    adapter = wgpu.gpu.request_adapter_sync(power_preference=prefer)
    if adapter is None:
        adapter = wgpu.gpu.request_adapter_sync(force_fallback_adapter=True)
    return adapter


def view_camera(camera: Mapping, snap_h: Optional[float], n_x: int, n_y: int,
                voxel_um: Sequence[float], canvas_h: int, half_angle: Optional[float] = None) -> dict:
    """A view state's ``camera`` ({zoom, center: [z, y, x], angles: [rx°, ry°, rz°]}) → the viewer's
    orbit camera ``{yaw, pitch, dist, panX, panY}`` (radians, µm). ``applyViewStateToBrowser`` in
    ``utils/viewer/viewState.ts``, line for line.

    - The zoom was measured against the snapshot's canvas (``snap_h``), so the output shows the image
      height the viewer showed, whatever the movie's own size. No snapshot canvas → ``canvas_h``.
    - ``center`` is the image L0 pixel the camera looks at; absent → the image centre (a batch camera
      rotates each image about its own midpoint).
    """
    if half_angle is None:
        half_angle = float(wgsl_utils.shader_constants()["VIEW_HALF_ANGLE"])
    um_x = float(voxel_um[0] or 1)
    um_y = float(voxel_um[1] or 1)
    captured_h = float(snap_h) if snap_h and snap_h > 0 else float(canvas_h)
    visible_l0_h = captured_h / max(1e-6, float(camera.get("zoom") or 1))
    dist = visible_l0_h * um_y / (2 * max(half_angle, 1e-6))
    centre = camera.get("center")
    cy = float(centre[1]) if centre is not None else n_y / 2
    cx = float(centre[2]) if centre is not None else n_x / 2
    angles = camera.get("angles") or [0, 0, 0]
    return {"dist": dist,
            "panX": (cx - (n_x or 1) / 2) * um_x,
            "panY": -((cy - (n_y or 1) / 2) * um_y),
            "yaw": float(angles[1] or 0) * math.pi / 180,
            "pitch": float(angles[0] or 0) * math.pi / 180}


def plane_level(zoom: float, n_levels: int) -> int:
    """The pyramid level the viewer's 2D view shows at ``zoom`` (L0 pixels per screen pixel):
    ``pickTileLevel`` in ``utils/volumeViewer.ts`` on a first pick (a movie has no previous level
    to hold), pinned by ``shaders/golden.json`` → ``planeLevel``. Assumes ×2 levels, as it does."""
    if n_levels <= 1 or not math.isfinite(zoom) or zoom <= 1:
        return 0
    return max(0, min(n_levels - 1, math.floor(math.log2(zoom))))


def _js_round(v: np.ndarray) -> np.ndarray:
    """``Math.round``: half rounds UP (NumPy's ``round`` is half-to-even)."""
    return np.floor(v + 0.5)


def lut_rows(luts: Sequence[Sequence[Sequence[float]]]) -> np.ndarray:
    """Per-channel LUT stops (RGB in 0..1) → the ``(MAX_CHANNELS, LUT_STOPS, 4)`` LUT texture.
    ``lutTextureBytes`` + ``sampleLut``: linear interpolation across the stops, rows past the last
    channel and channels with no stops left all-zero (black adds nothing)."""
    consts = wgsl_utils.shader_constants()
    max_c, stops_n = int(consts["MAX_CHANNELS"]), int(consts["LUT_STOPS"])
    out = np.zeros((max_c, stops_n, 4), np.uint8)
    n = np.arange(stops_n) / (stops_n - 1)
    for c, stops in enumerate(list(luts)[:max_c]):
        s = np.asarray(stops, dtype=np.float64).reshape(-1, 3)
        k = len(s)
        if k == 0:
            continue
        if k == 1:
            rgb = np.repeat(s[:1], stops_n, axis=0)
        else:
            p = np.clip(n, 0, 1) * (k - 1)
            i = np.minimum(np.floor(p).astype(int), k - 2)
            f = (p - i)[:, None]
            rgb = s[i] + f * (s[i + 1] - s[i])
        out[c, :, :3] = np.clip(_js_round(rgb * 255), 0, 255)
        out[c, :, 3] = 255
    return out


def label_palette() -> np.ndarray:
    """``shaders/label_palette.json`` as ``(rows, 4)`` uint8 — the viewer's 3D label colours."""
    with open(wgsl_utils.SHADER_DIR / "label_palette.json", encoding="utf-8") as f:
        rgb = np.asarray(json.load(f)["rgb"], dtype=np.uint8)
    return np.concatenate([rgb, np.full((len(rgb), 1), 255, np.uint8)], axis=1)


#: Width of a label colour table (``label_table``): ids wrap onto rows of this many texels. Any 2D
#: texture holds 4096 per side, so a table addresses 16.7 M ids before the last row runs out.
LABEL_TABLE_W = 4096


def label_table(ids: Sequence[int], rgb: Sequence[Sequence[float]]) -> np.ndarray:
    """Per-label colours → the shader's colour TABLE (``labColour`` with a negative row count):
    ``(rows, LABEL_TABLE_W, 4)`` uint8, id at ``(id // W, id % W)``, alpha 255 = drawn, 0 = not.
    ``rgb`` in 0..1, the same linear values the palette carries. Pass ``-LABEL_TABLE_W`` as
    ``lab.paletteRows``."""
    ids = np.asarray(ids, np.int64).reshape(-1)
    rows = int(ids.max()) // LABEL_TABLE_W + 1 if len(ids) else 1
    out = np.zeros((rows * LABEL_TABLE_W, 4), np.uint8)
    keep = ids > 0
    if keep.any():
        out[ids[keep], :3] = _js_round(np.clip(np.asarray(rgb, np.float64).reshape(-1, 3)[keep], 0, 1) * 255)
        out[ids[keep], 3] = 255
    return out.reshape(rows, LABEL_TABLE_W, 4)


class MipHost:
    """One device + the MIP pipeline and its overlay passes. Set the inputs, then ``render`` a frame.

    Inputs, in the viewer's layouts:
      - ``volume``: ``(c, z, y, x)`` uint16 — channels stacked along z in ONE ``r16uint`` 3D texture.
      - ``lut``: ``(MAX_CHANNELS, LUT_STOPS, 4)`` uint8 — one ramp row per channel (``lut_rows``).
      - ``palette``: ``(rows, 4)`` uint8 — label colours, ``id % rows`` (``label_palette``); or a
        ``(rows, LABEL_TABLE_W, 4)`` colour table (``label_table``).
      - ``labels``: ``(z, y, x)`` uint32 for the same planes, or ``None`` (labels off).
      - ``points``: ``(n, 7)`` float32 — centre xyz (image µm), rgb, z plane (``buildPointBuffer``).
      - ``segments``: ``(n, 10)`` float32 — from xyz, to xyz, rgb, end plane (``buildTrackBuffer``).
    """

    def __init__(self, adapter=None):
        wgpu = _wgpu()
        self._wgpu = wgpu
        self.adapter = adapter or request_adapter()
        if self.adapter is None:
            raise RuntimeError("wgpu: no adapter (no GPU and no software fallback)")
        # The defaults cap a 3D texture at 2048 per side; ask for what the adapter really has, so a
        # deep or wide volume uploads at full resolution where the hardware allows it.
        lim = self.adapter.limits
        self.device = self.adapter.request_device_sync(required_limits={
            "max-texture-dimension-3d": lim["max-texture-dimension-3d"],
            "max-buffer-size": lim["max-buffer-size"],
        })
        self.max_texture_3d = int(lim["max-texture-dimension-3d"])
        self.layout = wgsl_utils.UniformLayout("mip")
        consts = wgsl_utils.shader_constants()
        self.max_channels, self.lut_stops = int(consts["MAX_CHANNELS"]), int(consts["LUT_STOPS"])
        self.pick_binding = int(consts["MIP_PICK_BINDING"])
        self.pipeline = self._pipeline("mip.wgsl")
        self._points_pipe = self._pipeline("mip_points.wgsl", [
            ("float32x3", 0), ("float32x3", 12), ("float32", 24)], POINT_STRIDE)
        self._seg_pipe = self._pipeline("mip_segments.wgsl", [
            ("float32x3", 0), ("float32x3", 12), ("float32x3", 24), ("float32", 36)], SEG_STRIDE)
        BU = wgpu.BufferUsage
        self._ubuf = self.device.create_buffer(size=self.layout.bytes, usage=BU.UNIFORM | BU.COPY_DST)
        # Pick highlight is the correction cockpit's; a movie never draws it. Zeroed = no focus,
        # no contour, empty set (`viewerLabels.ts` → PICK_BUFFER_BYTES).
        pick_words = 4 + int(consts["PICK_BITSET_WORDS"])
        self._pick = self.device.create_buffer_with_data(data=bytes(pick_words * 4), usage=BU.STORAGE)
        self._vol = self._lut = self._pal = None
        self._lab = self._texture((1, 1, 1), "r32uint", "3d", np.zeros(1, np.uint32), 4)
        self._points = self._segments = None
        self._n_points = self._n_segments = 0
        self._target = None
        self._bind_group = None
        self._ov_groups = None

    def _pipeline(self, shader: str, attributes=None, stride: int = 0):
        """A pipeline for one of the viewer's passes. Overlays get the viewer's instance layout and
        its alpha blend, and draw over the MIP in the same pass (``volumeRenderer.ts``)."""
        module = self.device.create_shader_module(code=wgsl_utils.expand_wgsl(shader))
        target = {"format": TARGET_FORMAT}
        vertex = {"module": module, "entry_point": "vs"}
        if attributes:
            vertex["buffers"] = [{
                "array_stride": stride * 4, "step_mode": "instance",
                "attributes": [{"shader_location": i, "offset": off, "format": fmt}
                               for i, (fmt, off) in enumerate(attributes)]}]
            target["blend"] = {
                "color": {"src_factor": "src-alpha", "dst_factor": "one-minus-src-alpha", "operation": "add"},
                "alpha": {"src_factor": "one", "dst_factor": "one-minus-src-alpha", "operation": "add"}}
        return self.device.create_render_pipeline(
            layout="auto", vertex=vertex,
            fragment={"module": module, "entry_point": "fs", "targets": [target]},
            primitive={"topology": "triangle-list"})

    @property
    def adapter_info(self) -> dict:
        return dict(self.adapter.info)

    def _texture(self, size, fmt, dim, data: np.ndarray, bytes_per_texel: int):
        TU = self._wgpu.TextureUsage
        tex = self.device.create_texture(size=size, dimension=dim, format=fmt,
                                         usage=TU.TEXTURE_BINDING | TU.COPY_DST)
        self.device.queue.write_texture(
            {"texture": tex}, np.ascontiguousarray(data).tobytes(),
            {"bytes_per_row": size[0] * bytes_per_texel, "rows_per_image": size[1]}, size)
        return tex

    def fits(self, shape_czyx: Sequence[int]) -> bool:
        """Whether a ``(c, z, y, x)`` volume fits one 3D texture on this device."""
        nc, nz, ny, nx = shape_czyx
        return max(nx, ny, nz * nc) <= self.max_texture_3d

    def set_volume(self, volume: np.ndarray) -> None:
        if volume.dtype != np.uint16 or volume.ndim != 4:
            raise ValueError(f"volume must be (c, z, y, x) uint16, got {volume.dtype} {volume.shape}")
        nc, nz, ny, nx = volume.shape
        # (c, z, y, x) in C order IS the texture: x fastest, channel c at z offset c * nz.
        self._vol = self._texture((nx, ny, nz * nc), "r16uint", "3d", volume, 2)
        self._bind_group = None

    def set_lut(self, lut: np.ndarray) -> None:
        want = (self.max_channels, self.lut_stops, 4)
        if lut.shape != want or lut.dtype != np.uint8:
            raise ValueError(f"lut must be {want} uint8, got {lut.dtype} {lut.shape}")
        self._lut = self._texture((self.lut_stops, self.max_channels, 1), "rgba8unorm", "2d", lut, 4)
        self._bind_group = None

    def set_palette(self, palette: np.ndarray) -> None:
        if palette.dtype != np.uint8 or palette.shape[-1] != 4 or palette.ndim not in (2, 3):
            raise ValueError(f"palette must be (rows, 4) or (rows, w, 4) uint8, got {palette.dtype} {palette.shape}")
        size = (palette.shape[0], 1, 1) if palette.ndim == 2 else (palette.shape[1], palette.shape[0], 1)
        self._pal = self._texture(size, "rgba8unorm", "2d", palette, 4)
        self._bind_group = None

    def set_labels(self, labels: Optional[np.ndarray]) -> None:
        if labels is None:
            labels = np.zeros((1, 1, 1), np.uint32)
        if labels.dtype != np.uint32 or labels.ndim != 3:
            raise ValueError(f"labels must be (z, y, x) uint32, got {labels.dtype} {labels.shape}")
        nz, ny, nx = labels.shape
        self._lab = self._texture((nx, ny, nz), "r32uint", "3d", labels, 4)
        self._bind_group = None

    def _instances(self, data: Optional[np.ndarray], stride: int):
        if data is None or len(data) == 0:
            return None, 0
        arr = np.ascontiguousarray(data, dtype=np.float32)
        if arr.ndim != 2 or arr.shape[1] != stride:
            raise ValueError(f"overlay instances must be (n, {stride}) float32, got {arr.shape}")
        buf = self.device.create_buffer_with_data(data=arr.tobytes(), usage=self._wgpu.BufferUsage.VERTEX)
        return buf, len(arr)

    def set_points(self, points: Optional[np.ndarray]) -> None:
        """Population points for the next frames; ``None`` = none."""
        self._points, self._n_points = self._instances(points, POINT_STRIDE)

    def set_segments(self, segments: Optional[np.ndarray]) -> None:
        """Track-tail segments for the next frames; ``None`` = none."""
        self._segments, self._n_segments = self._instances(segments, SEG_STRIDE)

    def _bind(self):
        if None in (self._vol, self._lut, self._pal):
            raise RuntimeError("set_volume, set_lut and set_palette before render")
        if self._bind_group is None:
            self._bind_group = self.device.create_bind_group(
                layout=self.pipeline.get_bind_group_layout(0), entries=[
                    {"binding": 0, "resource": {"buffer": self._ubuf}},
                    {"binding": 1, "resource": self._vol.create_view()},
                    {"binding": 2, "resource": self._lut.create_view()},
                    {"binding": 3, "resource": self._lab.create_view()},
                    {"binding": 4, "resource": self._pal.create_view()},
                    {"binding": self.pick_binding, "resource": {"buffer": self._pick}},
                ])
        if self._ov_groups is None:
            # The overlay passes read only the uniform block, the SAME buffer the raycast reads, so
            # they share its camera and cannot drift from the pixels.
            self._ov_groups = tuple(self.device.create_bind_group(
                layout=p.get_bind_group_layout(0),
                entries=[{"binding": 0, "resource": {"buffer": self._ubuf}}])
                for p in (self._seg_pipe, self._points_pipe))
        return self._bind_group

    def render(self, width: int, height: int, uniforms: Mapping[str, float]) -> np.ndarray:
        """One frame → ``(height, width, 4)`` uint8, sRGB-encoded. ``uniforms`` are lane names
        (``uniforms.json`` → "mip"); ``vp.canvasW`` / ``vp.canvasH`` are filled from the size.
        Draw order is the viewer's: the MIP, then track tails, then points."""
        TU = self._wgpu.TextureUsage
        if self._target is None or self._target.size[:2] != (width, height):
            self._target = self.device.create_texture(size=(width, height, 1), format=TARGET_FORMAT,
                                                      usage=TU.RENDER_ATTACHMENT | TU.COPY_SRC)
        values = dict(uniforms)
        values["vp.canvasW"], values["vp.canvasH"] = width, height
        q = self.device.queue
        q.write_buffer(self._ubuf, 0, self.layout.pack(values).tobytes())
        mip_group = self._bind()
        enc = self.device.create_command_encoder()
        rp = enc.begin_render_pass(color_attachments=[{
            "view": self._target.create_view(), "load_op": "clear", "store_op": "store",
            "clear_value": (0, 0, 0, 1)}])
        rp.set_pipeline(self.pipeline)
        rp.set_bind_group(0, mip_group)
        rp.draw(3)
        for pipe, group, buf, n in ((self._seg_pipe, self._ov_groups[0], self._segments, self._n_segments),
                                    (self._points_pipe, self._ov_groups[1], self._points, self._n_points)):
            if n:
                rp.set_pipeline(pipe)
                rp.set_bind_group(0, group)
                rp.set_vertex_buffer(0, buf)
                rp.draw(6, n)
        rp.end()
        q.submit([enc.finish()])
        raw = q.read_texture({"texture": self._target}, {"bytes_per_row": width * 4}, (width, height, 1))
        return np.frombuffer(raw, dtype=np.uint8).reshape(height, width, 4)
