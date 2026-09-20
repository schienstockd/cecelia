// Cross-window seek — Kiwi (main window) → pop-out viewer (docs/todo/BIDIR_CONTEXT_PLAN.md Kiwi
// PR B). Clicking a capture row in Kiwi should jump the pop-out viewer to that capture's t / z
// so the user can see the LIVE frame alongside the shared frozen one and iterate with Claude.
//
// Two same-origin windows, and the send needs to be fire-and-forget (Kiwi has no handle on the
// pop-out — the pop-out may not even be open). `BroadcastChannel` is exactly this — no backend
// round-trip, no `postMessage` handshake, no window-handle bookkeeping. Falls back to a no-op if
// the browser doesn't have it (older environments); the caller doesn't get a receipt either way.
//
// This module is pure logic (message shape + wrapper). The Vue consumers (KiwiCockpit publishes,
// ViewerWindow subscribes) live in the SFC where they belong. See `utils/viewerBagChannel.ts` for
// the same shape used for viewer-bag sync via `localStorage` — same "main writes → viewer picks up"
// pattern, different transport (BroadcastChannel doesn't survive a reload, but a seek doesn't
// need to; the pop-out reads what's fresh, not the last-known state).
//
// BIDIR Part 4 follow-up: the message optionally carries `marks` + `captureId` so a blackboard
// attachment click can also restore the annotation overlay onto the live viewer — same channel,
// additive fields (a seek without marks is still a valid seek). The viewer mounts `MarksOverlay`
// while marks are active and drops it (with the chip) when the user leaves the capture's t/z.

import type { OverlayMark } from './captureAddress'

/** Channel name. Kept as a constant so the sender and the receiver can't drift. */
export const VIEWER_SEEK_CHANNEL = 'cecelia:viewer:seek'

/** One seek request. `imageUid` is required so a Kiwi click for image A doesn't move a viewer
 *  currently showing image B — the receiver ignores mismatches. `t` / `z` are the target
 *  timepoint / z-plane; either may be absent (no move on that axis). `projectUid` is included so
 *  a future multi-project session can filter by it too; today's viewer already implicitly filters
 *  via `imageUid`. `marks` + `captureId` are set when a blackboard attachment is the source —
 *  the viewer paints the marks over the canvas until the user seeks away or dismisses the chip. */
export interface ViewerSeekMessage {
  projectUid: string
  imageUid: string
  t?: number
  z?: number
  marks?: OverlayMark[]
  captureId?: string
}

/** Broadcast a seek to any listening viewer. No-op on browsers without `BroadcastChannel`; the
 *  caller doesn't need the guarantee — a seek is fire-and-forget. */
export function publishViewerSeek(msg: ViewerSeekMessage): void {
  if (typeof BroadcastChannel === 'undefined') return
  if (!msg.projectUid || !msg.imageUid) return
  try {
    const ch = new BroadcastChannel(VIEWER_SEEK_CHANNEL)
    ch.postMessage(msg)
    ch.close()
  } catch { /* fall through — a seek that doesn't land is not a fatal error */ }
}

/** Subscribe. Returns an `unsubscribe` handle the caller invokes on unmount. `filter` runs on
 *  every message; return `true` to hand it to `onSeek`. A caller that only cares about ONE image
 *  passes `msg.imageUid === myUid`. Unrecognised payload shape ⇒ dropped, no throw. */
export function subscribeViewerSeek(
  onSeek: (msg: ViewerSeekMessage) => void,
  filter: (msg: ViewerSeekMessage) => boolean = () => true,
): () => void {
  if (typeof BroadcastChannel === 'undefined') return () => {}
  let ch: BroadcastChannel
  try {
    ch = new BroadcastChannel(VIEWER_SEEK_CHANNEL)
  } catch {
    return () => {}
  }
  ch.onmessage = (ev) => {
    const parsed = parseSeekMessage(ev.data)
    if (parsed && filter(parsed)) onSeek(parsed)
  }
  return () => { try { ch.close() } catch { /* already closed */ } }
}

/** Shape-check an incoming message. Anything that isn't a plausible seek ⇒ `null` so a garbage
 *  postMessage from an unrelated channel-name collision can't crash the viewer. Exported for the
 *  unit test — a pure function is easier to hit than a `BroadcastChannel` round-trip. */
export function parseSeekMessage(raw: unknown): ViewerSeekMessage | null {
  if (!raw || typeof raw !== 'object') return null
  const r = raw as Record<string, unknown>
  const projectUid = typeof r.projectUid === 'string' ? r.projectUid : ''
  const imageUid   = typeof r.imageUid   === 'string' ? r.imageUid   : ''
  if (!projectUid || !imageUid) return null
  const out: ViewerSeekMessage = { projectUid, imageUid }
  if (typeof r.t === 'number' && Number.isFinite(r.t) && r.t >= 0) out.t = Math.floor(r.t)
  if (typeof r.z === 'number' && Number.isFinite(r.z) && r.z >= 0) out.z = Math.floor(r.z)
  if (typeof r.captureId === 'string' && r.captureId) out.captureId = r.captureId
  // Marks: an array (from `capture.overlay`), each element is `{kind, geom, label?, color?}`.
  // We don't re-validate the mark shape here — the viewer's overlay component is defensive against
  // a missing `geom.pts` / bad kind. Dropping the array on any non-array keeps the seek valid.
  if (Array.isArray(r.marks)) out.marks = r.marks as OverlayMark[]
  return out
}
