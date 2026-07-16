"""GPU device detection for torch-backed tasks (cellpose segment + denoise).

One canonical detector so the CUDA → MPS → CPU fallback isn't duplicated per task.
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
