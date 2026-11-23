// "Reuse unchanged steps" on the Chain page (backend: app/src/tasks/chain/reuse.jl; docs/SCHEDULER.md →
// *Reuse unchanged steps*). Opt-in per run, and never silent: a reused node is `done` with the run-log
// task id of the past run it kept, and the Live view says `reused` instead of `done`.

/** Per-profile key the Run panel's toggle is remembered under (`profileStorage`). */
export const REUSE_PREF_KEY = 'cc.chain.reuseUnchanged'

/** The toggle's stored value: on only when it was saved as on. */
export function readReusePref(raw: string | null): boolean {
  return raw === 'true'
}

/** A fresh `chain:run` payload with the reuse flag — sent only when on, so a run without it is unchanged. */
export function withReuse<T extends Record<string, unknown>>(payload: T, reuse: boolean): T & { reuse?: true } {
  return reuse ? { ...payload, reuse: true } : payload
}

/** The word a Live node's status bar shows: `reused` for a done node that reused a past run. */
export function liveStatusText(status: string, reused?: string): string {
  return status === 'done' && reused ? 'reused' : status
}

/** The Live node's tooltip for a reused node — which past run it kept. */
export function reusedTip(reused?: string): string | undefined {
  return reused ? `Unchanged — reused run ${reused}` : undefined
}

/** `GET /api/chains/run`'s `reused` map (uid → node → task id), looked up for one node on one image. */
export function reusedOf(map: Record<string, Record<string, string>> | undefined,
                         imageUid: string, nodeId: string): string | undefined {
  return map?.[imageUid]?.[nodeId] || undefined
}
