// Coalesce concurrent async calls that share a key into a single in-flight operation. Extracted from
// GatePlotPanel.vue (gate-outline fetching) so the dedup logic is unit-testable without a mounted
// component or a mocked fetch — the .vue side is then just "build the key, assign the result".
//
// While a call for `key` is running, later calls with the SAME key return that same promise instead
// of starting new work; once it settles (resolve OR reject) the next call starts fresh. A call with a
// DIFFERENT key always starts its own work. Used so an outline re-fetch triggered from two places in
// the same tick (confirmGate + the childGateSig watcher) issues one request, not two.

export function coalesceByKey<T>(work: (key: string) => Promise<T>): (key: string) => Promise<T> {
  let inflight: { key: string; p: Promise<T> } | null = null
  return (key: string) => {
    if (inflight?.key === key) return inflight.p
    const p = work(key)
    inflight = { key, p }
    // clear once settled so a later call with the same key re-runs (only clear if still the latest).
    // then(clear, clear) — not finally — so this monitoring chain settles cleanly on rejection too and
    // doesn't surface as an unhandled rejection; the caller still gets the original `p` to handle.
    const clear = () => { if (inflight?.p === p) inflight = null }
    void p.then(clear, clear)
    return p
  }
}
