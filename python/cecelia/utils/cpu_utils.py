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

**This is opt-in per call site, and that is a WEAKNESS, not the design.** One helper with no single
point of application is the shape this codebase treats as a bug elsewhere: the next runner silently
gets the unbounded default and nothing catches it. The structurally right home is `run_py`, the one
place a Python task's environment is built — and the only layer that *can* set it, since
`OPENBLAS_NUM_THREADS` is read before numpy imports. It is not there yet only because the right
number is a property of the workload and just this one has been measured; a task doing one genuinely
large matmul (scanpy PCA/UMAP) plausibly wants every core, and a blanket cap would trade one
unmeasured default for another. See `docs/TODO.md` → *BLAS threads are bounded per call site, not
per task*. Until then: MEASURE, then wrap the small-matmul region.
"""

import contextlib

# Best measured for the many-small-matmuls case above. The curve is flat between 4 and 8, so this
# is not a knife edge; both are ~1.8x better than uncapped and ~1.6x better than 1 thread.
BLAS_THREADS_SMALL_MATMUL = 4


@contextlib.contextmanager
def limit_blas_threads(n_threads=BLAS_THREADS_SMALL_MATMUL):
    """Cap the BLAS thread pool for the duration of the block, then restore it.

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

