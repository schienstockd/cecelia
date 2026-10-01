"""Headless WebGPU host for the viewer's MIP shader — the movie side of the shared renderer.

Runs ``shaders/mip.wgsl`` (expanded by ``wgsl_utils``) through ``wgpu-py`` on Vulkan / Metal / DX12,
or on a software adapter where there is no GPU. It only UPLOADS: the volume, the LUT, the label
palette and the uniform block, in the same formats ``volumeRenderer.ts`` uses — the drawing is the
viewer's shader (``docs/todo/SHARED_RENDERER_PLAN.md``).

The target is ``rgba8unorm-srgb``, the format the viewer's canvas draws through
(``canvasFormat.ts``): the shader writes linear colour and the view encodes it, which is the step
the torch renderer once skipped (movies came out about 2x darker in mid-tones).
"""
from typing import Mapping, Optional

import numpy as np

from cecelia.utils import wgsl_utils

TARGET_FORMAT = "rgba8unorm-srgb"


def request_adapter(prefer: str = "high-performance"):
    """The best adapter wgpu offers, else the software fallback; ``None`` if there is neither."""
    import wgpu
    adapter = wgpu.gpu.request_adapter_sync(power_preference=prefer)
    if adapter is None:
        adapter = wgpu.gpu.request_adapter_sync(force_fallback_adapter=True)
    return adapter


class MipHost:
    """One device + the MIP pipeline. Set the inputs, then ``render`` a frame per uniform block.

    Inputs, in the viewer's layouts:
      - ``volume``: ``(c, z, y, x)`` uint16 — channels stacked along z in ONE ``r16uint`` 3D texture.
      - ``lut``: ``(MAX_CHANNELS, LUT_STOPS, 4)`` uint8 — one ramp row per channel.
      - ``palette``: ``(rows, 4)`` uint8 — label colours, ``id % rows``.
      - ``labels``: ``(z, y, x)`` uint32 for the same planes, or ``None`` (labels off).
    """

    def __init__(self, adapter=None):
        import wgpu
        self._wgpu = wgpu
        self.adapter = adapter or request_adapter()
        if self.adapter is None:
            raise RuntimeError("wgpu: no adapter (no GPU and no software fallback)")
        self.device = self.adapter.request_device_sync()
        self.layout = wgsl_utils.UniformLayout("mip")
        consts = wgsl_utils.shader_constants()
        self.max_channels, self.lut_stops = int(consts["MAX_CHANNELS"]), int(consts["LUT_STOPS"])
        self.pick_binding = int(consts["MIP_PICK_BINDING"])
        module = self.device.create_shader_module(code=wgsl_utils.expand_wgsl("mip.wgsl"))
        self.pipeline = self.device.create_render_pipeline(
            layout="auto",
            vertex={"module": module, "entry_point": "vs"},
            fragment={"module": module, "entry_point": "fs", "targets": [{"format": TARGET_FORMAT}]},
            primitive={"topology": "triangle-list"})
        self._ubuf = self.device.create_buffer(size=self.layout.bytes,
                                               usage=wgpu.BufferUsage.UNIFORM | wgpu.BufferUsage.COPY_DST)
        # Pick highlight is the correction cockpit's; a movie never draws it. Zeroed = no focus,
        # no contour, empty set (`viewerLabels.ts` → PICK_BUFFER_BYTES).
        pick_words = 4 + int(consts["PICK_BITSET_WORDS"])
        self._pick = self.device.create_buffer_with_data(data=bytes(pick_words * 4),
                                                         usage=wgpu.BufferUsage.STORAGE)
        self._vol = self._lut = self._pal = None
        self._lab = self._texture((1, 1, 1), "r32uint", "3d", np.zeros(1, np.uint32), 4)
        self._target = None
        self._bind_group = None

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
        if palette.ndim != 2 or palette.shape[1] != 4 or palette.dtype != np.uint8:
            raise ValueError(f"palette must be (rows, 4) uint8, got {palette.dtype} {palette.shape}")
        self._pal = self._texture((palette.shape[0], 1, 1), "rgba8unorm", "2d", palette, 4)
        self._bind_group = None

    def set_labels(self, labels: Optional[np.ndarray]) -> None:
        if labels is None:
            labels = np.zeros((1, 1, 1), np.uint32)
        if labels.dtype != np.uint32 or labels.ndim != 3:
            raise ValueError(f"labels must be (z, y, x) uint32, got {labels.dtype} {labels.shape}")
        nz, ny, nx = labels.shape
        self._lab = self._texture((nx, ny, nz), "r32uint", "3d", labels, 4)
        self._bind_group = None

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
        return self._bind_group

    def render(self, width: int, height: int, uniforms: Mapping[str, float]) -> np.ndarray:
        """One frame → ``(height, width, 4)`` uint8, sRGB-encoded. ``uniforms`` are lane names
        (``uniforms.json`` → "mip"); ``vp.canvasW`` / ``vp.canvasH`` are filled from the size."""
        TU = self._wgpu.TextureUsage
        if self._target is None or self._target.size[:2] != (width, height):
            self._target = self.device.create_texture(size=(width, height, 1), format=TARGET_FORMAT,
                                                      usage=TU.RENDER_ATTACHMENT | TU.COPY_SRC)
        values = dict(uniforms)
        values["vp.canvasW"], values["vp.canvasH"] = width, height
        q = self.device.queue
        q.write_buffer(self._ubuf, 0, self.layout.pack(values).tobytes())
        enc = self.device.create_command_encoder()
        rp = enc.begin_render_pass(color_attachments=[{
            "view": self._target.create_view(), "load_op": "clear", "store_op": "store",
            "clear_value": (0, 0, 0, 1)}])
        rp.set_pipeline(self.pipeline)
        rp.set_bind_group(0, self._bind())
        rp.draw(3)
        rp.end()
        q.submit([enc.finish()])
        raw = q.read_texture({"texture": self._target}, {"bytes_per_row": width * 4}, (width, height, 1))
        return np.frombuffer(raw, dtype=np.uint8).reshape(height, width, 4)
