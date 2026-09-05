"""GPU device detection + VRAM pre-flight for torch-backed tasks.

One canonical place so the CUDA → MPS → CPU fallback and the "does this fit in memory" check
aren't duplicated per task. Cellpose, coastal and denoise all read from here.
"""

import torch


def torch_device():
    """Auto-detect the compute device: prefer CUDA, fall back to MPS (Apple
    Silicon), then CPU.

    Returns:
        (use_gpu: bool, device: torch.device | None)
    """
    if torch.cuda.is_available():
        return True, torch.device('cuda')
    if hasattr(torch.backends, 'mps') and torch.backends.mps.is_available():
        return True, torch.device('mps')
    return False, None


def free_vram_gb(device=None):
    """Free VRAM in GB on the active CUDA device, or `None` when not applicable.

    Only CUDA reports a queryable free/total pair (`torch.cuda.mem_get_info`); MPS + CPU return
    `None` so callers know a numeric budget is not available on those backends.
    """
    if device is None:
        _, device = torch_device()
    if device is None or device.type != 'cuda':
        return None
    free, _ = torch.cuda.mem_get_info(device)
    return free / 1e9


def require_free_vram_gb(min_gb, task_label, log=None, device=None):
    """Refuse the run cleanly if the active CUDA device has less than `min_gb` free.

    Raises `SystemExit(1)` on insufficient VRAM after logging an actionable message — the trainer's
    OOM otherwise surfaces as an opaque CUDA stack halfway through epoch 1. On MPS/CPU the check is
    a no-op (no free/total query available); the task will still hit any hard-cap the driver has.

    Args:
        min_gb: minimum free VRAM in GB needed for the run to succeed.
        task_label: user-facing name of what is being sized, quoted verbatim in the error.
        log: object with `.log(msg)` (script_utils.LogfileUtils shape). Prints to stdout if None.
        device: override for the target CUDA device; defaults to `torch_device()`'s pick.
    """
    def _emit(msg):
        (log.log if log is not None else print)(msg)

    free_gb = free_vram_gb(device)
    if free_gb is None:
        return  # nothing to check on MPS/CPU — the task's own error is the only signal available
    if free_gb < min_gb:
        _emit(f'[ERROR] Not enough free VRAM for {task_label}: {free_gb:.1f} GB free, ~{min_gb} GB needed.')
        _emit(f'[ERROR] Try a smaller model size, or close other GPU processes and retry.')
        raise SystemExit(1)
