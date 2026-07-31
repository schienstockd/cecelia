"""Move one dense array block between two of our Python processes over the JSON WS protocol.

The task preview needs the mask it just computed to reach the napari bridge — two separate processes
(`preview/preview_worker.py` → `napari/napari_bridge.py`, via the Julia orchestrator) that only speak
one-JSON-message-per-request. The obvious alternative, writing a scratch zarr store and having the
bridge open it, is what this replaced: it put throwaway bytes in the user's project tree, needed its
own staging/lifecycle/sweep machinery, and made a preview reachable by anything that lists stores.
A preview is a picture, not data, so it never touches the disk.

ONE codec, used by both ends — a hand-rolled encode on one side and decode on the other is exactly
the drift this repo keeps paying for. `dtype` carries byte order, so a big-endian producer does not
silently become garbage on a little-endian consumer.

Label masks are repetitive, so compression matters more than the level. Measured on a 590×590 uint32
plane holding 676 cells (1.39 MB raw): **zlib-1 → 66 KB in 2.8 ms; zlib-6 → 29 KB in 7.9 ms.** Level 6
is genuinely 2.3× smaller — the point is that both are irrelevant beside the 140–380 ms of inference
they follow, so this takes the cheaper one and the payload is small either way.
"""
import base64
import zlib

import numpy as np

# zlib level — see the module docstring for the measurement. Level 1 because the encode sits in an
# interactive loop and 66 KB is already nothing; not because higher levels don't compress better.
_LEVEL = 1

# Cap on the DECODED size of one block, as a guard against a malformed/hostile payload turning into a
# multi-GB allocation. Comfortably above a full-frame uint32 mask of a large image (a 4096×4096 plane
# is 67 MB), and far below anything that would trouble the machine.
MAX_DECODED_BYTES = 512 * 1024 * 1024


def encode_block(arr):
    """A dense ndarray → a JSON-safe dict (`shape`/`dtype`/`data`)."""
    arr = np.ascontiguousarray(arr)
    return {
        'shape': [int(s) for s in arr.shape],
        'dtype': arr.dtype.str,
        'data': base64.b64encode(zlib.compress(arr.tobytes(), _LEVEL)).decode('ascii'),
    }


def decode_block(payload):
    """The dict from `encode_block` → a WRITABLE ndarray of the original shape and dtype.

    Writable on purpose: `np.frombuffer` over `bytes` returns a read-only view, and a read-only array
    that only fails once something downstream tries to assign into it is a trap. The `bytearray` makes
    it writable without a second copy of the data.
    """
    shape = [int(s) for s in payload['shape']]
    dtype = np.dtype(payload['dtype'])
    expected = int(np.prod(shape)) * dtype.itemsize if shape else dtype.itemsize
    if expected > MAX_DECODED_BYTES:
        raise ValueError(f'block too large to decode: {expected} bytes > {MAX_DECODED_BYTES}')
    raw = bytearray(zlib.decompress(base64.b64decode(payload['data'])))
    if len(raw) != expected:
        # a truncated/mismatched payload would otherwise reshape into a plausible-looking wrong array
        raise ValueError(f'block payload is {len(raw)} bytes, shape/dtype imply {expected}')
    return np.frombuffer(raw, dtype=dtype).reshape(shape)


# Axes chunked to 1 in the lazy full-shape array. A viewer slices ONE plane at a time, and a chunk is
# the atomic unit of computation — so a chunk spanning all of T and Z would materialise the entire
# volume (4.8 GB for a 201×21×544×548 uint32 movie) to draw a single plane.
_PLANE_AXES = ('T', 'Z')


def place_block_lazy(block, full_shape, axes, region, fill=0):
    """A block placed at `region` inside a LAZY array of the full label extent.

    Returns a dask array of `full_shape`: `fill` everywhere, `block` in the region it was computed
    for. Nothing but the block is ever materialised, so this costs the block's bytes rather than the
    full extent's — the point being that a preview layer can be full-image-shaped, and therefore line
    up with the image with no `translate`, without allocating the image.

    `axes` names each of `full_shape`'s axes; `region` maps an axis name to `[lo, hi)`. An axis absent
    from `region` must be covered by the block in full.

    Built as zeros-plus-assignment rather than `dask.array.pad`: measured on the shape above, pad
    yields a 93k-task graph against 8.4k here, and it does not let us choose the chunking — which is
    the part that actually matters (see `_PLANE_AXES`).
    """
    import dask.array as da

    full_shape = [int(s) for s in full_shape]
    chunks = tuple(1 if ax in _PLANE_AXES else int(s) for ax, s in zip(axes, full_shape))
    out = da.full(tuple(full_shape), fill, dtype=block.dtype, chunks=chunks)

    sl = []
    for ax, full, n in zip(axes, full_shape, block.shape):
        lo = int(region[ax][0]) if ax in region else 0
        if lo + n > full:
            raise ValueError(f'block exceeds the {ax} extent: {lo}+{n} > {full}')
        if ax not in region and n != full:
            raise ValueError(f'axis {ax} is not in the region, so the block must cover it: {n} != {full}')
        sl.append(slice(lo, lo + n))
    out[tuple(sl)] = block
    return out
