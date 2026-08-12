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

