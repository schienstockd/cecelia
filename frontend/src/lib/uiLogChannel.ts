// Console lines from the app's OTHER windows.
//
// A pop-out is a second full app instance with its own Pinia (`lib/popout.ts`), so a `logStore.info()`
// call lands only in the store of the window that made it. That is invisible for the two popouts that
// render no console — the volume viewer and the Task Manager — and it is why the viewer's diagnostics
// had nowhere to go but a Debug panel you have to be in that window to read. Backend lines do not have
// this problem: they arrive over each window's own WS from one ring, which is why napari prints
// everywhere already.
//
// **`localStorage` + the `storage` event**, the same transport as `lib/openProjectChannel.ts`, for the
// same reason: it is the one cross-window mechanism this app already relies on, and the event fires in
// every OTHER window of the origin but never in the one that wrote — so a publisher cannot hear itself
// and there is no echo to filter. Unlike the open project this carries an EVENT rather than state, so
// the value is overwritten freely and nothing reads the key at mount; a line published to no listening
// window is simply lost, which is the correct behaviour for a log line and not for a project uid.
//
// The `n` counter is load-bearing. `storage` fires only when the value CHANGES, so two identical
// messages in a row — "GPU: Draw failed", every frame — would deliver once and then go silent.
import type { LogLevel } from '../utils/logFilter'

const KEY = 'cc.uiLog'

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

/** Publish a line to the app's other windows. Never throws: private mode disables storage, and a
 *  console line is not worth taking a window down for. */
export function publishUiLog(line: Omit<UiLogLine, 'ts'> & { ts?: string }): void {
  try { localStorage.setItem(KEY, uiLogPayload(line, ++seq)) }
  catch { /* storage disabled — the other windows just won't hear it */ }
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
