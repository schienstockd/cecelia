// Manual chain steps in the whiteboard Live view: which steps are parked, the `chain:continue` frame
// that continues them, and what to tell the user when a continue is refused. The backend half is
// app/src/tasks/chain/manual.jl; behaviour in docs/SCHEDULER.md → Manual steps.

/** The minimum a Live-view row carries for this — `LiveTaskLike` in ChainModule satisfies it. */
export interface ManualRow {
  chainNodeId: string
  imageUid: string
  status: string
}

/** Parked images per chain node, in row order. Empty when nothing waits. */
export function waitingByNode(rows: readonly ManualRow[]): Map<string, string[]> {
  const out = new Map<string, string[]>()
  for (const r of rows) {
    if (r.status !== 'waiting') continue
    const uids = out.get(r.chainNodeId) ?? out.set(r.chainNodeId, []).get(r.chainNodeId)!
    if (!uids.includes(r.imageUid)) uids.push(r.imageUid)
  }
  return out
}

/**
 * The `chain:continue` frame. `imageUid` continues one image of an image-scope step; omitted, every
 * parked image of the node goes on — which is also the only meaning a set-scope step has.
 */
export function continueFrame(projectUid: string, runId: string, nodeId: string, imageUid?: string) {
  return {
    type: 'chain:continue' as const,
    projectUid, runId, nodeId,
    ...(imageUid ? { imageUids: [imageUid] } : {}),
  }
}

/** One line per refused image (or set-scope node), from a `chain:continue:result` frame. */
export function refusalLines(result: { refused?: Record<string, string>; error?: string },
                             nameOf: (uid: string) => string = u => u): string[] {
  if (result.error) return [result.error]
  return Object.entries(result.refused ?? {}).map(([uid, why]) => `${nameOf(uid)}: ${why}`)
}
