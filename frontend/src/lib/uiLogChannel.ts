// Console lines from the app's OTHER windows.
//
// A pop-out is a second full app instance with its own Pinia (`lib/popout.ts`), so a `logStore.info()`
// call lands only in the store of the window that made it. That is invisible for the two popouts that
// render no console — the volume viewer and the Task Manager — and it is why the viewer's diagnostics
// had nowhere to go but a Debug panel you have to be in that window to read. Backend lines do not have
// this problem: they arrive over each window's own WS from one ring, which is why viewer prints
// everywhere already.
//
// **`localStorage` + the `storage` event**, the same transport as `lib/openProjectChannel.ts`, for the
// same reason: it is the one cross-window mechanism this app already relies on, and the event fires in
// every OTHER window of the origin but never in the one that wrote — so a publisher cannot hear itself
// and there is no echo to filter. Two keys share this transport: `cc.uiLog` is the LIVE event (an
// overwrite is fine — a listening window gets it, a window not open yet does not); `cc.uiLogRing` is
// the small persisted HISTORY the console popout hydrates from at mount, so a popout opened AFTER a
// line was said still sees it. Without the ring the live channel alone leaves a fresh popout blank of
// everything before it opened — which is the reported bug and the whole reason there IS a ring.
//
// The `n` counter is load-bearing. `storage` fires only when the value CHANGES, so two identical
// messages in a row — "GPU: Draw failed", every frame — would deliver once and then go silent.
import type { LogLevel } from '../utils/logFilter'

const KEY = 'cc.uiLog'
const RING_KEY = 'cc.uiLogRing'
/** The ring's cap. 200 covers the longest UI storm we see in practice (viewer errors during a lost
 *  device) with room for prior activity — the docked view already keeps 3000, so this is enough
 *  history to hydrate a popout with, not a second store. */
const RING_CAP = 200

/** One console line, as it crosses between windows. `ts` travels so a line is filed where it happened
 *  rather than where it was received. */
export interface UiLogLine {
  level: LogLevel
  message: string
  detail?: string
  /** Which surface said it — `'viewer'`, `'tasks'`. Unlisted sources group under the UI's own chip. */
  source: string
  /** ISO 8601. */
  ts: string
}

let seq = 0

/** The stored value for a line. Pure, and separate from the write, so the counter that makes a repeated
 *  message deliver twice can be asserted without a DOM. */
export function uiLogPayload(line: Omit<UiLogLine, 'ts'> & { ts?: string }, n: number,
                            now = () => new Date().toISOString()): string {
  return JSON.stringify({ ...line, ts: line.ts ?? now(), n })
}

/** Publish a line to the app's other windows and append it to the persisted ring. Never throws:
 *  private mode disables storage, and a console line is not worth taking a window down for. */
export function publishUiLog(line: Omit<UiLogLine, 'ts'> & { ts?: string }): void {
  const resolved: UiLogLine = {
    level: line.level,
    message: line.message,
    detail: line.detail,
    source: line.source,
    ts: line.ts ?? new Date().toISOString(),
  }
  try { localStorage.setItem(KEY, uiLogPayload(resolved, ++seq)) }
  catch { /* storage disabled — the other windows just won't hear it */ }
  appendToRing(resolved)
}

/**
 * What a stored ring value means, as a pure decision — the untypeable half of `readUiLogRing`, split
 * out so a hand-edited or half-upgraded value can be asserted without a DOM. Silent-drops the entries
 * that fail the shape check rather than the whole ring, because one bad row shouldn't hide the rest.
 */
export function parseUiLogRing(raw: string | null): UiLogLine[] {
  if (!raw) return []
  let parsed: unknown
  try { parsed = JSON.parse(raw) } catch { return [] }
  if (!Array.isArray(parsed)) return []
  const out: UiLogLine[] = []
  for (const v of parsed) {
    if (!v || typeof v !== 'object') continue
    const rec = v as Partial<UiLogLine>
    if (typeof rec.message !== 'string' || !rec.message) continue
    const level: LogLevel = rec.level === 'error' || rec.level === 'warn' ? rec.level : 'info'
    out.push({
      level,
      message: rec.message,
      detail: typeof rec.detail === 'string' ? rec.detail : undefined,
      source: typeof rec.source === 'string' && rec.source ? rec.source : 'app',
      ts: typeof rec.ts === 'string' ? rec.ts : new Date().toISOString(),
    })
  }
  return out
}

/** Cap-and-serialise for the ring. Pure so the eviction rule can be asserted directly. Newest at the
 *  end (chronological), so a hydrating popout replays lines in order. */
export function serialiseUiLogRing(current: UiLogLine[], next: UiLogLine, cap: number = RING_CAP): string {
  const merged = current.concat(next)
  const capped = merged.length > cap ? merged.slice(merged.length - cap) : merged
  return JSON.stringify(capped)
}

/** The persisted history a popout hydrates from at mount. Bounded, so a long-running session cannot
 *  fill the ~5 MB quota with log lines and start rejecting other stores' writes. */
export function readUiLogRing(): UiLogLine[] {
  try { return parseUiLogRing(localStorage.getItem(RING_KEY)) }
  catch { return [] }                       // private mode disables `getItem`
}

/** Wipe the shared history. Called by the console's Clear button so clearing one window's console
 *  clears what a popout opened next would hydrate — otherwise Clear looks broken across windows. */
export function clearUiLogRing(): void {
  try { localStorage.removeItem(RING_KEY) } catch { /* private mode */ }
}

function appendToRing(line: UiLogLine): void {
  try { localStorage.setItem(RING_KEY, serialiseUiLogRing(readUiLogRing(), line)) }
  catch { /* storage disabled or quota exceeded — history is best-effort */ }
}

/**
 * What a `storage` event means, as a pure decision: the line to push, or `null` for nothing.
 *
 * Separated from the listener because the listener is the untestable half. The ignores are all real: an
 * event about any other key (the settings store writes a dozen), a `clear()` — `key === null` — and a
 * value that is not a line this version wrote, which is what a half-upgraded second window looks like
 * during a dev reload.
 */
export function uiLogFromStorageEvent(e: Pick<StorageEvent, 'key' | 'newValue'>): UiLogLine | null {
  if (e.key !== KEY || !e.newValue) return null
  try {
    const v = JSON.parse(e.newValue) as Partial<UiLogLine>
    if (typeof v.message !== 'string' || !v.message) return null
    const level: LogLevel = v.level === 'error' || v.level === 'warn' ? v.level : 'info'
    return {
      level, message: v.message,
      detail: typeof v.detail === 'string' ? v.detail : undefined,
      source: typeof v.source === 'string' && v.source ? v.source : 'app',
      ts: typeof v.ts === 'string' ? v.ts : new Date().toISOString(),
    }
  } catch { return null }                    // a hand-edited key, or a value from another app
}

/** Subscribe to lines published by the app's other windows. Returns the unsubscribe. */
export function onUiLog(cb: (line: UiLogLine) => void): () => void {
  const handler = (e: StorageEvent) => { const line = uiLogFromStorageEvent(e); if (line) cb(line) }
  window.addEventListener('storage', handler)
  return () => window.removeEventListener('storage', handler)
}
