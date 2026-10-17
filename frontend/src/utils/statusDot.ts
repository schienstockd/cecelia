// Tones for the `.cc-dot` primitive (style.css) — the one small round status light. A surface maps
// its own state to a tone here (or in its own util) and renders `:class="dotClass(tone)"`; nothing
// re-declares the dot's size or colours. docs/ui/PRIMITIVES.md → Dot.
import type { LogLevel } from './logFilter'
import type { WsStatus } from '../stores/ws'
import type { Severity } from '../lib/severity'

/** The severity tones, plus `idle` (stopped, not wrong) and `info` (a log line that is neither). */
export type DotTone = Severity | 'idle' | 'info'

/** The modifier class for a tone; `''` when there is none (the dot stays the dim default). */
export const dotClass = (tone: DotTone | null | undefined): string => (tone ? `cc-dot-${tone}` : '')

/** The console's last-entry light. */
export const LOG_LEVEL_TONE: Record<LogLevel, DotTone> = { error: 'fail', warn: 'warn', info: 'info' }

/** The header's backend-connection light. */
export const WS_STATUS_TONE: Record<WsStatus, DotTone> = {
  connected: 'ok', connecting: 'warn', disconnected: 'idle', error: 'fail', signedOut: 'fail',
}
