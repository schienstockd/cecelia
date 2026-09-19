// Bidir Part 5 — push pairing (BIDIR_PUSH_PLAN PR #1).
//
// Pure helper for reading the /api/push/target GET response and deriving the "paired ✓ / not
// paired" state the ViewerPanel chip renders. NO writes here — pairing is written by the MCP
// client's auto-pair middleware or the explicit `register_push_target` MCP tool; the frontend
// only observes.
//
// The GET never returns the token — it's a delivery credential that stays on disk and only PR
// #2's Julia writer reads it. So this helper deliberately has no token field on `PairedState`.

export type PairedState =
  | { paired: false }
  | { paired: true, sessionLabel: string, socketPath: string, pairedAt: string, pairedFromPid: string }

/** Coerce a `GET /api/push/target` response into the state the chip renders. */
export function parsePushTarget(raw: unknown): PairedState {
  if (!raw || typeof raw !== 'object') return { paired: false }
  const r = raw as Record<string, unknown>
  if (!r.paired) return { paired: false }
  return {
    paired: true,
    sessionLabel:  typeof r.sessionLabel  === 'string' ? r.sessionLabel  : '',
    socketPath:    typeof r.socketPath    === 'string' ? r.socketPath    : '',
    pairedAt:      typeof r.pairedAt      === 'string' ? r.pairedAt      : '',
    pairedFromPid: typeof r.pairedFromPid === 'string' ? r.pairedFromPid : '',
  }
}

/** Fetch + parse. Any network failure ⇒ `paired: false` (self-heal via next auto-pair). */
export async function fetchPushTarget(projectUid: string, apiBase = ''): Promise<PairedState> {
  if (!projectUid) return { paired: false }
  try {
    const res = await fetch(`${apiBase}/api/push/target?projectUid=${encodeURIComponent(projectUid)}`)
    if (!res.ok) return { paired: false }
    return parsePushTarget(await res.json())
  } catch {
    return { paired: false }
  }
}

/** Short label for the chip. `"paired ✓ probe-t"` when a session label is set, else `"paired ✓"`. */
export function pushChipLabel(state: PairedState): string {
  if (!state.paired) return 'not paired'
  return state.sessionLabel ? `paired ✓ ${state.sessionLabel}` : 'paired ✓'
}

/** Manual unpair for Kiwi (docs/todo/KIWI_PLAN.md PR #3). Deletes the pairing record; the next
 *  auto-pair from any MCP tool call rewrites it. Idempotent. Any network failure ⇒ `false` so
 *  the caller can show a small error without throwing — same discipline as `fetchPushTarget`. */
export async function clearPushTarget(projectUid: string, apiBase = ''): Promise<boolean> {
  if (!projectUid) return false
  try {
    const res = await fetch(`${apiBase}/api/push/target/clear`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid }),
    })
    return res.ok
  } catch {
    return false
  }
}
