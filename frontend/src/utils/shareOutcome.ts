// Post-Save capture outcome — the ONE place that decides "did the bidir push land or do we fall
// back to the clipboard prompt?" and builds the toast line the host surface renders.
//
// Two callers today, and they must stay in lockstep — the viewer's Save (`ViewerWindow.onDrawSave`)
// and the plot canvas's Save (`SummaryCanvas.onAnnotateSave`). Before this helper, only the viewer
// ran the `push === 'sent'` branch + clipboard fallback + toast; the plot canvas seeded its reshow
// silently, so an unpaired session got neither a Claude push nor a clipboard prompt — a "successful"
// Save signalled nothing. Centralising the branch here means a third surface (a future
// Blackboard/Kiwi Save, an image-strip capture) gets the same behaviour for free.
//
// Pure logic — the toast UI + timer is the host's, we only produce the message.

import { copyText } from './clipboard'

/** Prompt the user's Claude session sees when the frontend falls back to the clipboard path. Prefix
 *  is constant so a user (or future prompt matcher) can spot it; notes append verbatim so the
 *  session picks up the same context the push path would have carried. */
const CLIPBOARD_PROMPT_PREFIX = 'Read my shared frame in cecelia.'

/** Build the clipboard prompt. `notes` optional — an empty share still gets the "read my frame"
 *  line so Claude has a signal to react to. */
export function shareClipboardPrompt(notes: string | undefined): string {
  return notes ? `${CLIPBOARD_PROMPT_PREFIX}\nUser said: ${notes}` : CLIPBOARD_PROMPT_PREFIX
}

export type ShareOutcome = { kind: 'ok' | 'fail'; message: string }

/**
 * Announce a successful capture POST. `pushOutcome` is the backend's `push` field —
 * `'sent' | 'fallback' | 'not_paired'` in practice, but treated as an opaque string so a future
 * value doesn't need a code change here (anything not `'sent'` takes the clipboard path). On
 * `'sent'` no clipboard round-trip happens; the toast just tells the user delivery landed.
 * Otherwise we copy the prompt so the user can paste it into their Claude session, and the toast
 * reports whether the copy itself worked.
 *
 * Return-only — never touches DOM outside the clipboard call, so the host renders the chip its
 * own way (viewer's `.vw-status-chip`, plot canvas's `.sc-status-chip`).
 */
export async function announceShareOutcome(
  pushOutcome: string,
  notes: string | undefined,
): Promise<ShareOutcome> {
  if (pushOutcome === 'sent') {
    return { kind: 'ok', message: 'Sent to your Claude session — check for the incoming message.' }
  }
  const prompt = shareClipboardPrompt(notes)
  const copied = await copyText(prompt)
  return {
    kind: 'ok',
    message: copied
      ? 'Capture saved — prompt in your clipboard. Switch to Claude and paste.'
      : `Capture saved — copy manually: "${prompt}"`,
  }
}

/** Message for the fail toast — a wrapped error from the POST or a thrown Error. Takes an unknown
 *  because both hosts catch on `unknown` (strict TS). */
export function shareFailMessage(err: unknown): string {
  return err instanceof Error ? err.message : String(err)
}
