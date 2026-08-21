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


def task_workers(cap=None, env=None):
    """How many threads this task may run its own work on.

    `cap` is an ALGORITHMIC ceiling, not a preference: a stage that has been measured to stop
    scaling — or to get worse — past some width passes it, and the budget is then the smaller of the
    two. Coastal's region growing is the case that motivated it (it peaks at 4 threads and is slower
    at 8), and no amount of machine says otherwise, so that number belongs next to the measurement
    rather than in a config file.

    An unparseable or non-positive setting falls back to the derived default rather than raising:
    a typo in a config file should not stop a run, and every value here is a performance choice.
    """
    raw = (env if env is not None else os.environ).get(TASK_WORKERS_ENV)
    try:
        workers = int(str(raw).strip())
    except (TypeError, ValueError):
        workers = 0
    if workers < 1:
        workers = default_task_workers()
    if cap is not None:
        workers = min(workers, max(1, int(cap)))
    return max(1, workers)


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


def peak_rss_bytes():
    """The process high-water mark, or `None`. Used to price a transient peak that is already over."""
    try:
        import resource, sys
        ru = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
        return ru if sys.platform == 'darwin' else ru * 1024
    except Exception:
        return None
