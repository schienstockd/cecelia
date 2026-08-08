// Wait for a set of panels to stop loading before capturing them.
//
// The board export (PDF / board SVG) reads each slot's rendered chart out of the DOM. It used to wait
// exactly one animation frame — enough for the drag-grip to hide, and nothing else — so a slot that was
// still fetching or drawing exported blank or half-drawn into the finished document. The failure is
// silent: you get a PDF, just with a hole in it.
//
// Deliberately POLLED rather than promise-chained: "is this panel busy" is a reactive boolean owned by
// each panel, panels come and go with the board, and several kinds (summary, cluster, interactive) each
// track it their own way. A poll needs nothing from them but the boolean.
//
// The timeout is the important part. A genuinely stuck panel must NOT hang the export — a document with
// one bad plot beats a button that never returns — so this resolves `false` on expiry and the caller
// captures anyway, exactly as it did before.

export interface AwaitIdleOptions {
  /** Give up after this long and let the caller proceed. */
  timeoutMs?: number
  /** How often to re-check. */
  intervalMs?: number
  /** Stay idle for this long before believing it — a panel can report idle between two fetches. */
  settleMs?: number
  now?: () => number
  sleep?: (ms: number) => Promise<void>
}

const defaultSleep = (ms: number) => new Promise<void>(r => setTimeout(r, ms))

/**
 * Resolve once `isBusy()` has been false continuously for `settleMs`, or `false` if `timeoutMs`
 * elapses first. Resolves `true` when it settled legitimately.
 */
export async function awaitIdle(
  isBusy: () => boolean,
  opts: AwaitIdleOptions = {},
): Promise<boolean> {
  const timeoutMs = opts.timeoutMs ?? 10_000
  const intervalMs = opts.intervalMs ?? 50
  const settleMs = opts.settleMs ?? 100
  const now = opts.now ?? (() => Date.now())
  const sleep = opts.sleep ?? defaultSleep

  const deadline = now() + timeoutMs
  let idleSince: number | null = null
  // check BEFORE sleeping, so an already-idle board costs one call and no delay
  for (;;) {
    if (!isBusy()) {
      if (idleSince === null) idleSince = now()
      if (now() - idleSince >= settleMs) return true
    } else {
      idleSince = null
    }
    if (now() >= deadline) return false
    await sleep(intervalMs)
  }
}

/** True if ANY panel reports busy. A panel with no `isBusy` is treated as idle (it can't tell us). */
export function anyBusy(panels: Iterable<{ isBusy?: () => boolean } | undefined>): boolean {
  for (const p of panels) {
    try {
      if (p?.isBusy?.()) return true
    } catch {
      // a panel mid-teardown must not break the export
    }
  }
  return false
}
