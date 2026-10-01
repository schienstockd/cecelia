"""SHARED_RENDERER_PLAN Phase 0: render ONE viewer frame headlessly with wgpu-py, running the viewer's
own MIP shader (`shaders/mip.wgsl`, via `mipShader.ts`) on the uniforms/LUT/palette that `export_inputs.test.ts` produces from
the viewer's own TS. Nothing here re-implements drawing; it only uploads.

    python render_frame.py prep   <project_dir> <image_uid> <movie_key> <t> <out_dir>
    (then run export_inputs.test.ts — see its header)
    python render_frame.py render <out_dir> [nvidia|llvmpipe|intel] [frames]
"""
import base64
import json
import os
import sys
import time

import numpy as np

# The target format. The viewer draws through an sRGB view (canvasFormat.ts); SPIKE_FMT=rgba8unorm
# writes the shader's linear output as-is, to check the transfer.
FMT = os.environ.get('SPIKE_FMT', 'rgba8unorm-srgb')


def prep(project_dir, image_uid, movie_key, t, out_dir):
    from cecelia.utils import vn_versioning, zarr_utils
    movies = json.load(open(os.path.join(project_dir, 'settings', 'movies.json'), encoding='utf-8'))
    vs = movies[movie_key]['config']['keyframes'][0]['viewState']
    vs['dims']['current_step'][0] = t
    vs['dims']['point'][0] = t
    img_dir = next(os.path.join(project_dir, s, image_uid) for s in sorted(os.listdir(project_dir))
                   if os.path.isfile(os.path.join(project_dir, s, image_uid, 'ccid.json')))
    ccid = json.load(open(os.path.join(img_dir, 'ccid.json'), encoding='utf-8'))
    vn = movies[movie_key]['config']['valueNames'][0]
    # The zarr itself sits in the sibling dir that holds the store (the project splits ccid + data).
    store = vn_versioning.versioned_get_field_at(ccid, 'filepath', vn)
    zpath = next(os.path.join(project_dir, s, image_uid, store) for s in sorted(os.listdir(project_dir))
                 if os.path.isdir(os.path.join(project_dir, s, image_uid, store)))
    names = ccid['imChannelNames']['default']
    axes, scale = zarr_utils.read_axes(zpath), zarr_utils.read_scale(zpath)
    assert axes == ['t', 'c', 'z', 'y', 'x'], axes
    lvl0 = zarr_utils.open_as_zarr(zpath)[0][0]         # (levels, …) → level 0
    vol = np.ascontiguousarray(np.asarray(lvl0[t]), dtype=np.uint16)        # (c, z, y, x)
    nc, nz, ny, nx = vol.shape
    # (c, z, y, x) in C order IS the viewer's texture: x fastest, channels stacked along z.
    vol.tofile(os.path.join(out_dir, 'volume.u16'))
    json.dump({'viewState': vs, 'names': names, 'nX': nx, 'nY': ny, 'nZ': nz,
               'voxelUm': [scale[4], scale[3], scale[2]], 'steps': 256,
               'width': vs['canvas']['width'], 'height': vs['canvas']['height']},
              open(os.path.join(out_dir, 'inputs.json'), 'w', encoding='utf-8'))
    print(f'prep: {zpath} t={t} vol={vol.shape}')


def pick_adapter(wgpu, want):
    for a in wgpu.gpu.enumerate_adapters_sync():
        if want.lower() in a.info['device'].lower() and a.info['backend_type'] == 'Vulkan':
            return a
    raise SystemExit(f'no Vulkan adapter matching {want!r}')


def render(out_dir, want='nvidia', frames=20):
    import wgpu
    inp = json.load(open(os.path.join(out_dir, 'inputs.json'), encoding='utf-8'))
    gi = json.load(open(os.path.join(out_dir, 'gpu_inputs.json'), encoding='utf-8'))
    nx, ny, nz, nc = inp['nX'], inp['nY'], inp['nZ'], len(inp['names'])
    w, h = inp['width'], inp['height']
    vol = np.fromfile(os.path.join(out_dir, 'volume.u16'), dtype=np.uint16)

    adapter = pick_adapter(wgpu, want)
    dev = adapter.request_device_sync()
    q = dev.queue
    TU = wgpu.TextureUsage

    def tex(size, fmt, dim='2d', usage=TU.TEXTURE_BINDING | TU.COPY_DST):
        return dev.create_texture(size=size, dimension=dim, format=fmt, usage=usage)

    vtex = tex((nx, ny, nz * nc), 'r16uint', '3d')
    q.write_texture({'texture': vtex}, vol.tobytes(),
                    {'bytes_per_row': nx * 2, 'rows_per_image': ny}, (nx, ny, nz * nc))
    lut = tex(tuple(gi['lutSize']), 'rgba8unorm')
    q.write_texture({'texture': lut}, base64.b64decode(gi['lut']),
                    {'bytes_per_row': gi['lutSize'][0] * 4}, (*gi['lutSize'], 1))
    pal = tex(tuple(gi['palSize']), 'rgba8unorm')
    q.write_texture({'texture': pal}, base64.b64decode(gi['pal']),
                    {'bytes_per_row': gi['palSize'][0] * 4}, (*gi['palSize'], 1))
    nolab = tex((1, 1, 1), 'r32uint', '3d')
    q.write_texture({'texture': nolab}, bytes(4), {'bytes_per_row': 4, 'rows_per_image': 1}, (1, 1, 1))
    ubuf = dev.create_buffer_with_data(data=np.asarray(gi['uniforms'], dtype=np.float32).tobytes(),
                                       usage=wgpu.BufferUsage.UNIFORM)
    pick = dev.create_buffer_with_data(data=bytes(gi['pickBytes']), usage=wgpu.BufferUsage.STORAGE)

    # The canvas's sRGB view (canvasFormat.ts): the shader writes linear, the view encodes.
    target = tex((w, h, 1), FMT, usage=TU.RENDER_ATTACHMENT | TU.COPY_SRC)
    t0 = time.perf_counter()
    mod = dev.create_shader_module(code=gi['wgsl'])
    pipe = dev.create_render_pipeline(
        layout='auto', vertex={'module': mod, 'entry_point': 'vs'},
        fragment={'module': mod, 'entry_point': 'fs', 'targets': [{'format': FMT}]},
        primitive={'topology': 'triangle-list'})
    compile_ms = (time.perf_counter() - t0) * 1e3
    bg = dev.create_bind_group(layout=pipe.get_bind_group_layout(0), entries=[
        {'binding': 0, 'resource': {'buffer': ubuf}},
        {'binding': 1, 'resource': vtex.create_view()},
        {'binding': 2, 'resource': lut.create_view()},
        {'binding': 3, 'resource': nolab.create_view()},
        {'binding': 4, 'resource': pal.create_view()},
        {'binding': 5, 'resource': {'buffer': pick}},
    ])

    def draw():
        enc = dev.create_command_encoder()
        rp = enc.begin_render_pass(color_attachments=[{
            'view': target.create_view(), 'load_op': 'clear', 'store_op': 'store',
            'clear_value': (0, 0, 0, 1)}])
        rp.set_pipeline(pipe)
        rp.set_bind_group(0, bg)
        rp.draw(3)
        rp.end()
        q.submit([enc.finish()])
        return np.frombuffer(q.read_texture({'texture': target}, {'bytes_per_row': w * 4},
                                            (w, h, 1)), dtype=np.uint8).reshape(h, w, 4)

    img = draw()                                         # warm: first submit + readback
    t0 = time.perf_counter()
    for _ in range(frames):
        img = draw()
    per_frame = (time.perf_counter() - t0) * 1e3 / frames
    import imageio.v3 as iio
    out = os.path.join(out_dir, f'wgpu_{want}_{FMT}.png')
    iio.imwrite(out, img[..., :3])
    res = {'adapter': adapter.info['device'], 'wgpu': wgpu.__version__, 'size': [w, h],
           'compile_ms': round(compile_ms, 1), 'ms_per_frame_incl_readback': round(per_frame, 2),
           'mean_rgb': [round(float(x), 2) for x in img[..., :3].reshape(-1, 3).mean(0)], 'png': out}
    print(json.dumps(res))
    json.dump(res, open(os.path.join(out_dir, f'result_{want}.json'), 'w', encoding='utf-8'))


if __name__ == '__main__':
    cmd, *a = sys.argv[1:]
    if cmd == 'prep':
        prep(a[0], a[1], a[2], int(a[3]), a[4])
    else:
        render(a[0], *(a[1:2] or ['nvidia']), *([int(a[2])] if len(a) > 2 else []))
