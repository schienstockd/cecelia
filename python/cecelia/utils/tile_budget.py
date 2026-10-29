"""Auto tile size for segmentation: the largest XY tile the machine can hold, up to the whole frame.

Nobody can answer "what tile size?" from the image alone; it depends on the engine, the model's
rescaling, the number of planes and how much RAM and GPU memory is free WHEN THE RUN STARTS (a
browser viewer on the same laptop can hold most of an 8 GB card). So `blockSize = auto` is resolved
here, in the Python runner, from:

- the engine's declared cost per tile pixel (`TileCost`, from `SegmentationUtils._tile_cost`);
- free RAM (`cpu_utils.available_memory_bytes`) minus what the run already holds (the frame and its
  label buffers);
- free VRAM (`gpu_utils.free_vram_gb`) minus the engine's fixed GPU cost.

A bigger tile is not faster per pixel (measured flat from 1024 px up), it only has fewer seams, so
the rule is "whole frame if it fits, otherwise the largest that fits", with a safety share kept back.
Measurements: `docs/SEGMENTATION.md` → *Auto tile size and overlap*; the plan and the patchworks
comparison: `docs/todo/PATCHWORKS_IDEAS_PLAN.md`.
"""

import math
import typing

AUTO = 'auto'

# Share of free memory a run may plan to use. The rest absorbs allocator overhead, the caching
# allocator's reserve (torch reserved 7.5 of 8 GB at a tile whose allocations were 5.7) and whatever
# else starts on the machine meanwhile.
RAM_SHARE = 0.7
VRAM_SHARE = 0.8

# Auto never picks a tile smaller than this, nor rounds to a finer step. Below it the overlap is a
# large share of every tile and the per-tile overhead dominates.
MIN_AUTO_BLOCK = 256
AUTO_BLOCK_STEP = 256


class TileCost(typing.NamedTuple):
    """What one tile pixel costs an engine.

    ram_per_px:  host bytes per XY pixel of the (padded) tile, all planes included.
    vram_per_px: GPU bytes per XY pixel of ONE plane — engines that take a Z stack plane by plane
                 hold one plane's worth on the GPU at a time. 0 for a CPU-only engine.
    vram_fixed:  GPU bytes held whatever the tile size (model, context, the allocator's floor).
    """
    ram_per_px: float
    vram_per_px: float = 0.0
    vram_fixed: float = 0.0


def parse_tiling_param(value, allow_zero=False):
    """`'auto'` (or missing) → None; otherwise a whole number of pixels.

    Saved params and chain templates hold the old ints (`4096`), the select holds strings
    (`'4096'`); both mean the same size.
    """
    if value is None or str(value).strip().lower() == AUTO:
        return None
    px = int(float(value))
    if px < 0 or (px == 0 and not allow_zero):
        raise ValueError(f'a tile size must be a positive number of pixels or "auto", got {value!r}')
    return px


def auto_block_size(cost, height, width, overlap, held_bytes=0, free_ram=None, free_vram=None):
    """The block size to tile at, and one line saying why — for the log.

    `free_ram` / `free_vram` are bytes, None when the machine cannot say (Windows RAM; no CUDA). A
    limit that cannot be read is skipped, never guessed; with neither known the whole frame is used.
    """
    frame = max(int(height), int(width))
    if cost is None:
        return frame, 'whole frame (this engine declares no tile cost)'

    limits = []
    if free_ram is not None and cost.ram_per_px > 0:
        limits.append(('RAM', (free_ram * RAM_SHARE - held_bytes) / cost.ram_per_px))
    if free_vram is not None and cost.vram_per_px > 0:
        limits.append(('GPU memory', (free_vram * VRAM_SHARE - cost.vram_fixed) / cost.vram_per_px))
    if not limits:
        return frame, 'whole frame (free memory unknown on this machine)'

    what, max_px = min(limits, key=lambda lim: lim[1])
    side = int(math.sqrt(max(max_px, 0.0))) - 2 * int(overlap)
    if side >= frame:
        return frame, f'whole frame ({frame} px fits in {what.lower()})'
    block = max(MIN_AUTO_BLOCK, side // AUTO_BLOCK_STEP * AUTO_BLOCK_STEP)
    return block, f'{block} px tiles (limited by free {what.lower()})'


def auto_overlap(object_size_px, fallback=64):
    """Overlap = 1.5 cell sizes, so a cell cut by a seam is whole in both tiles' predictions.

    That is what the seam stitch matches on (`SegmentationUtils._stitch_tile_seams`): each tile's
    prediction of the band either side of the seam, `overlap` px wide. A band narrower than a cell
    holds only fragments of it, and fragments of the same cell predicted from different context do
    not overlap well. `object_size_px` None (the engine cannot say) → `fallback`.
    """
    if not object_size_px or object_size_px <= 0:
        return fallback
    return int(math.ceil(1.5 * object_size_px))


def out_of_memory_message(exc, block_size):
    """One actionable line when `exc` is an out-of-memory error, else None.

    Auto plans with a safety share, but another process can take the memory mid-run. The run then
    stops (the models and caches go with the process) and says what to set, instead of a CUDA stack.
    """
    name = type(exc).__name__
    if not (isinstance(exc, MemoryError) or name in ('OutOfMemoryError', 'CUDAOutOfMemoryError')
            or 'out of memory' in str(exc).lower()):
        return None
    where = 'GPU memory' if ('cuda' in str(exc).lower() or 'OutOfMemory' in name) else 'memory'
    if block_size and block_size > MIN_AUTO_BLOCK:
        smaller = max(MIN_AUTO_BLOCK, block_size // 2)
        return (f'Ran out of {where} at {block_size} px tiles. '
                f'Set Tile size to {smaller} px (Image tiling) and run again.')
    return f'Ran out of {where} even at {block_size} px tiles. Free {where} and run again.'
