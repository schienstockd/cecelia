"""CPU thread budgeting for BLAS-backed work. The CPU-side sibling of `gpu_utils`.

**Why this exists.** A numpy/scipy call that lands in BLAS gets ALL cores by default, and every
Python task here runs inside one `cpu` pool slot — so `n` concurrent tasks ask for `n × cores`
threads. That is bad enough on its own; what makes it worth a helper is that on the workload this
was measured against, all-cores is slower even with the machine to itself.

Measured on `kSUFux/mkh3Tu` (180 frames, 8×512×512), drift estimation, 32-core box — identical
result (residual 0.329) at every setting, so this is pure overhead, not accuracy:

| BLAS threads | one task | four concurrent |
|--------------|----------|-----------------|
| 32 (default) |   56.3 s |         309.7 s |
| 16           |   42.8 s |                 |
| 8            |   32.5 s |                 |
| 4            |   31.8 s |          70.7 s |
| 2            |   38.8 s |                 |
| 1            |   51.3 s |                 |

1.8× faster alone, 4.4× faster contended. Note the four-concurrent uncapped case (309.7 s) is worse
than running them one after another would be (4 × 56.3 = 225 s) — past a point the threads are
fighting for cache rather than working.

The cause is the *shape* of the work, not its size: phase correlation's sub-pixel refinement is many
SMALL matmuls, and OpenBLAS spends more on fanning threads out and syncing them than on arithmetic.

**The DEFAULT lives in the launcher, not here.** `run_py` sets `OPENBLAS_NUM_THREADS`
(`BLAS_THREADS_PER_TASK`) on every Python task it spawns — that is the only layer that can, since the
variable is read when the child imports numpy, and it has no hole this context manager does: a
`threadpool_limits` block only bounds the pools already LOADED when it is entered, and clustering
loads a second BLAS after the first is capped. See `docs/SCHEDULER.md` → *Thread budgets*.

**So use this only to DEVIATE**, in two situations: code running outside `run_py` (a REPL session, a
test, an external consumer importing `cecelia` directly — none of which get the launcher's env), or a
region that has been MEASURED to want a different number. Do not raise it on a hunch; the table above
is what "obviously wants all cores" actually looks like when measured.
"""

import contextlib
import os

# ── Task-internal WORKER threads (not BLAS) ───────────────────────────────────────────────────────
#
# `BLAS_THREADS_PER_TASK` above bounds the pools numpy/scipy open underneath you. This bounds the
# pools a task opens ITSELF — coastal maps its z-planes over a `ThreadPoolExecutor`, and that number
# was a constant chosen on one 32-core laptop. On a 4-core machine it oversubscribes; on a 128-core
# one it leaves the machine idle.
#
# The value travels as an env var set by `run_py` (`CECELIA_TASK_WORKERS`), for the same reason the
# BLAS budget does: it is a property of the MACHINE and the scheduler, not of any one task's params,
# and putting it in every task's param list would be a knob per task that all had to agree.
#
# The default assumes roughly `_ASSUMED_ACTIVE_TASKS` tasks are actually computing at once — the
# `cpu` pool allows 20, but a limit is not an expectation, and sizing for the worst case would leave
# a single running task on one core. It matches how `BLAS_THREADS_PER_TASK` was arrived at.
TASK_WORKERS_ENV = 'CECELIA_TASK_WORKERS'
# Set by `run_py` (app/src/py_runner.jl). `WIDEN` is already the AND of the user's flag and "the
# budget was derived, not typed" — Julia owns that decision because it owns the config; this side
# only asks whether it may. `USABLE_CPUS` is `Cecelia.usable_cpus()`: the affinity mask and cgroup
# quota applied, which is why it is passed in rather than re-derived from `os.cpu_count()`.
TASK_WORKERS_WIDEN_ENV = 'CECELIA_TASK_WORKERS_WIDEN'
USABLE_CPUS_ENV = 'CECELIA_USABLE_CPUS'
_ASSUMED_ACTIVE_TASKS = 4
_MAX_DEFAULT_WORKERS = 16
# A floor, because the divisor alone turns a small machine SERIAL: 4 cores / 4 is one thread, and
# a 4-core laptop is not going to be running four heavy tasks anyway. Still never wider than the box.
_MIN_DEFAULT_WORKERS = 2


def default_task_workers(n_cpus=None):
    """The worker budget for one task when nothing has been configured — derived, not hardcoded."""
    n = max(1, int(n_cpus if n_cpus is not None else (os.cpu_count() or 1)))
    share = min(_MAX_DEFAULT_WORKERS, max(_MIN_DEFAULT_WORKERS, n // _ASSUMED_ACTIVE_TASKS))
    return max(1, min(share, n))


def task_workers(cap=None, env=None, scales_linearly=False):
    """How many threads this task may run its own work on.

    `cap` is an ALGORITHMIC ceiling, not a preference: a stage that has been measured to stop
    scaling — or to get worse — past some width passes it, and the budget is then the smaller of the
    two. Coastal's region growing is the case that motivated it (it peaks at 4 threads and is slower
    at 8), and no amount of machine says otherwise, so that number belongs next to the measurement
    rather than in a config file.

    `scales_linearly=True` is the OPPOSITE claim, and only a measurement earns it: this stage keeps
    getting faster with width, so where the budget was DERIVED rather than chosen it may take the
    usable CPU count instead. Coastal's flow metrics are the case (14x at 32 threads against 6.8x at
    8 - docs/SCHEDULER.md), and the budget's own divisor assumes four tasks are computing at once,
    which is wrong by ~2x for a lone run. Gated by `CECELIA_TASK_WORKERS_WIDEN`, which Julia sets to
    1 only when the user asked for it AND nobody typed a thread count - see `task_workers_widen`.

    `cap` still applies on top and always wins: an algorithmic ceiling is not a budget, so the two
    arguments are not contradictory even when both are passed. Widening never goes BELOW the budget
    either, so a configured value can only be exceeded by this path, never undercut.

    An unparseable or non-positive setting falls back to the derived default rather than raising:
    a typo in a config file should not stop a run, and every value here is a performance choice.
    """
    environ = env if env is not None else os.environ
    raw = environ.get(TASK_WORKERS_ENV)
    try:
        workers = int(str(raw).strip())
    except (TypeError, ValueError):
        workers = 0
    if workers < 1:
        workers = default_task_workers()
    if scales_linearly and _widen_allowed(environ):
        workers = max(workers, _usable_cpus(environ))
    if cap is not None:
        workers = min(workers, max(1, int(cap)))
    return max(1, workers)


def _widen_allowed(environ):
    """Whether Julia said a linearly-scaling stage may take the box. Absent = no."""
    return str(environ.get(TASK_WORKERS_WIDEN_ENV, '')).strip() in ('1', 'true', 'True')


def _usable_cpus(environ):
    """`Cecelia.usable_cpus()` as passed in, falling back to this process's own count.

    The fallback matters for a runner invoked OUTSIDE `run_py` - a REPL session, a test, an external
    consumer of the package - where the variable is absent. `os.cpu_count()` is the machine rather
    than the affinity mask, so it is a weaker answer; it is only reached when nothing better was
    supplied, and never when the widen flag is off (which is the default).
    """
    try:
        n = int(str(environ.get(USABLE_CPUS_ENV, '')).strip())
    except (TypeError, ValueError):
        n = 0
    return max(1, n if n > 0 else (os.cpu_count() or 1))


# Best measured for the many-small-matmuls case above. The curve is flat between 4 and 8, so this
# is not a knife edge; both are ~1.8x better than uncapped and ~1.6x better than 1 thread.
BLAS_THREADS_SMALL_MATMUL = 4


@contextlib.contextmanager
def limit_blas_threads(n_threads=BLAS_THREADS_SMALL_MATMUL):
    """Cap the BLAS thread pool for the duration of the block, then restore it.

    Guarantees only that no BLAS pool exceeds `n_threads` while the block runs. It does not
    guarantee the observed number IS `n_threads`: some backends clamp to their own maximum (a pool
    stayed at 2 when asked for 4), others set the limit outright (a pool went from 3 to 4). Both
    were CI failures from asserting something stronger. On a machine with fewer cores than the
    budget this is effectively a no-op, which is the right degradation — the problem it solves only
    exists when there are more cores than the work can use.

    A no-op (rather than an error) when the thread pool cannot be introspected, because doing the
    work slowly is always better than not doing it — `threadpoolctl` is declared in
    `python/pyproject.toml`, but an external consumer with an unusual BLAS build should still run.
    """
    try:
        import threadpoolctl
    except ImportError:      # pragma: no cover - declared dependency, defensive only
        yield
        return
    with threadpoolctl.threadpool_limits(limits=int(n_threads), user_api='blas'):
        yield


# ── Memory-bounded concurrency ────────────────────────────────────────────────────────────────────
#
# Some stages are bounded by RAM, not cores: coastal's flow metrics hold every plane of a sequence as
# float32 while they are computed, so running N sequences at once costs N times that. The training
# runner used to run exactly one at a time for this reason — a correct call when a sequence was a
# whole 1046x1104 movie (~1.55 GB), and a waste once sequences became 256x256 crops (~0.24 GB).
#
# So the bound is DERIVED rather than picked: measure what one unit actually cost, then divide the
# memory we may use by it. That way the decision follows the crop size, the frame count and the
# machine instead of a constant that is right for one dataset.

def available_memory_bytes():
    """Memory that could be allocated now without swapping, or `None` if we cannot tell.

    Linux: `MemAvailable` from `/proc/meminfo` — the kernel's own estimate, which counts reclaimable
    page cache. NOT `SC_AVPHYS_PAGES`, which excludes it and understates badly (3 GiB vs 10 GiB on the
    box this was written on) — budgeting from that number would keep a stage serial on a machine with
    plenty of room.

    macOS falls back to `SC_AVPHYS_PAGES`, which is the best stdlib answer there. Windows has none, and
    `None` is the honest reply — the caller must then not pretend to know.
    """
    try:
        if os.path.exists('/proc/meminfo'):
            with open('/proc/meminfo', encoding='utf-8') as fh:
                for line in fh:
                    if line.startswith('MemAvailable:'):
                        return int(line.split()[1]) * 1024        # reported in kB
        return os.sysconf('SC_AVPHYS_PAGES') * os.sysconf('SC_PAGE_SIZE')
    except Exception:
        return None


def concurrency_for_memory(per_unit_bytes, available_bytes, cap, reserve=0.5, unknown=2):
    """How many units of `per_unit_bytes` to run at once. Pure — the caller measures.

    `reserve` keeps HALF the available memory back by default, because `per_unit_bytes` is one
    observation of a transient peak and the next unit may be larger: a run that is 2x faster and
    occasionally killed is worse than a run that is 2x faster and never is.

    `available_bytes=None` means we could not measure (Windows), and the answer is `unknown` — a
    conservative step up rather than either extreme. Staying at 1 would make the platform gratuitously
    slower; taking `cap` would be guessing with someone else's RAM.

    Never returns less than 1: the work has to happen even if the estimate says there is no room.
    """
    cap = max(int(cap), 1)
    if available_bytes is None:
        return max(1, min(int(unknown), cap))
    if per_unit_bytes is None or per_unit_bytes <= 0:
        return cap
    fits = int((available_bytes * reserve) // per_unit_bytes)
    return max(1, min(fits, cap))


def rss_bytes():
    """Current resident set size, or `None` where it cannot be read (non-Linux without `resource`)."""
    try:
        if os.path.exists('/proc/self/statm'):
            with open('/proc/self/statm', encoding='utf-8') as fh:
                return int(fh.read().split()[1]) * os.sysconf('SC_PAGE_SIZE')
    except Exception:
        pass
    try:
        import resource, sys
        ru = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
        # Linux reports kB, macOS bytes — and this is the PEAK, not the current value
        return ru if sys.platform == 'darwin' else ru * 1024
    except Exception:
        return None


def _ru_maxrss_bytes(who):
    """`getrusage(who).ru_maxrss` in bytes (Linux reports kB, macOS bytes), or `None` without `resource`."""
    try:
        import resource, sys
        ru = resource.getrusage(who).ru_maxrss
        return ru if sys.platform == 'darwin' else ru * 1024
    except Exception:
        return None


def _windows_peak_working_set():
    """`PeakWorkingSetSize` of this process (Windows' peak resident memory), or `None`.

    Through `GetProcessMemoryInfo` with `ctypes` — the call psutil makes for `memory_info().peak_wset`,
    without psutil, which is only a transitive dependency of the default env and absent from others.
    """
    try:
        import ctypes
        from ctypes import wintypes

        class _Counters(ctypes.Structure):          # PROCESS_MEMORY_COUNTERS (psapi.h)
            _fields_ = [('cb', wintypes.DWORD), ('PageFaultCount', wintypes.DWORD),
                        ('PeakWorkingSetSize', ctypes.c_size_t), ('WorkingSetSize', ctypes.c_size_t),
                        ('QuotaPeakPagedPoolUsage', ctypes.c_size_t), ('QuotaPagedPoolUsage', ctypes.c_size_t),
                        ('QuotaPeakNonPagedPoolUsage', ctypes.c_size_t),
                        ('QuotaNonPagedPoolUsage', ctypes.c_size_t),
                        ('PagefileUsage', ctypes.c_size_t), ('PeakPagefileUsage', ctypes.c_size_t)]

        k32, psapi = ctypes.WinDLL('kernel32'), ctypes.WinDLL('psapi')
        k32.GetCurrentProcess.restype = wintypes.HANDLE
        psapi.GetProcessMemoryInfo.argtypes = [wintypes.HANDLE, ctypes.POINTER(_Counters), wintypes.DWORD]
        psapi.GetProcessMemoryInfo.restype = wintypes.BOOL
        c = _Counters()
        c.cb = ctypes.sizeof(_Counters)
        if not psapi.GetProcessMemoryInfo(k32.GetCurrentProcess(), ctypes.byref(c), c.cb):
            return None
        return int(c.PeakWorkingSetSize)
    except Exception:
        return None


def _linux_vm_hwm():
    """`VmHWM` from `/proc/self/status` in bytes — the peak RSS of THIS program's address space — or `None`."""
    try:
        with open('/proc/self/status', encoding='utf-8') as fh:
            for line in fh:
                if line.startswith('VmHWM:'):
                    return int(line.split()[1]) * 1024            # reported in kB
    except Exception:
        pass
    return None


def peak_rss_bytes():
    """The process high-water mark, or `None`. Used to price a transient peak that is already over.

    Linux: `VmHWM`, NOT `getrusage(RUSAGE_SELF).ru_maxrss`. The kernel carries `ru_maxrss` across
    `execve`, so a runner forked from the Julia backend starts with the backend's RSS as its peak —
    measured: a 12 MB Python child reporting 1.07 GB under a Julia parent holding a 0.8 GB array.
    `VmHWM` belongs to the new address space and starts from zero at exec. macOS: `ru_maxrss`. Windows:
    the peak working set.
    """
    if os.name == 'nt':
        return _windows_peak_working_set()
    hwm = _linux_vm_hwm()
    if hwm is not None:
        return hwm
    try:
        import resource
    except Exception:
        return None
    return _ru_maxrss_bytes(resource.RUSAGE_SELF)


def children_peak_rss_bytes():
    """The high-water mark of the LARGEST child process this one has waited for, or `None`.

    `getrusage(RUSAGE_CHILDREN).ru_maxrss` — the biggest single child, not the sum of them, and only
    children already reaped. `None` on Windows (no equivalent) and when no child was ever waited for.
    On Linux a forked child's figure includes this process's RSS at the fork (see `peak_rss_bytes`), so
    it never reads above this process's own peak for that reason alone.
    """
    if os.name == 'nt':
        return None
    try:
        import resource
    except Exception:
        return None
    v = _ru_maxrss_bytes(resource.RUSAGE_CHILDREN)
    return v if v else None
