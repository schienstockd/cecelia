// Not opening the viewer straight back into the crash it just died in.
//
// The volume viewer talks to the graphics driver, and a driver can take the whole browser down rather
// than raise anything catchable: Mesa's `iris` segfaulted in `libgallium` on opening an image
// (Dominik, 2026-08-25, SIGSEGV at 0x8). Nothing in the page runs after that — no error handler, no
// `device.lost`, no console line — and the window reopens on the same URL, so the next click is the
// same crash. That is the failure this exists for, and it is the one failure the WebGPU probe cannot
// see: the adapter answers, the format check passes, and the driver dies later anyway.
//
// The mechanism is a BREADCRUMB, not a prediction. A mark is written before the device is created and
// cleared when a frame is actually on screen; a mark still there at the next open means the last
// attempt never got that far. It cannot say why — a crash, a force-quit and a closed tab all look the
// same — so the offer is "try again", not a refusal.
//
// Per image, because the fault usually is: one image's shape is what pushed the driver over. A guard
// keyed on the viewer as a whole would lock you out of every image because of one.

const KEY = 'cc.vw.attempt'

/** A viewer start that has not yet reached a frame. `at` is ISO 8601, for the message. */
export interface ViewerAttempt {
  imageUid: string
  at: string
}

/**
 * Whether `raw` (the stored value) is an unfinished attempt at `imageUid`. Pure — the whole decision,
 * so it can be tested without a DOM or a driver that crashes on demand.
 *
 * Deliberately NOT expired: a mark can only survive if the page never reached a frame, and that is as
 * true an hour later as a second later. An expiry would hide exactly the case this is for — you close
 * the crashed browser, make coffee, and come back.
 */
export function isUnfinishedAttempt(raw: string | null, imageUid: string): boolean {
  if (!raw || !imageUid) return false
  try {
    const v = JSON.parse(raw) as Partial<ViewerAttempt>
    return v.imageUid === imageUid
  } catch { return false }              // a hand-edited key reads as "no previous attempt"
}

/** The value to store for an attempt. Separate from the write so the shape is pinned by a test. */
export function attemptPayload(imageUid: string, at: string): string {
  return JSON.stringify({ imageUid, at } satisfies ViewerAttempt)
}

/** Mark a start. Never throws: storage is disabled in private mode, and the guard is a courtesy. */
export function markViewerAttempt(imageUid: string, now = () => new Date().toISOString()): void {
  try { localStorage.setItem(KEY, attemptPayload(imageUid, now())) } catch { /* no guard, then */ }
}

/** Clear it — call when a frame is actually on screen, not when the device is created. */
export function clearViewerAttempt(): void {
  try { localStorage.removeItem(KEY) } catch { /* nothing to clear */ }
}

/** Did the last attempt at this image never reach a frame? */
export function viewerCrashedLastTime(imageUid: string): boolean {
  try { return isUnfinishedAttempt(localStorage.getItem(KEY), imageUid) }
  catch { return false }
}
