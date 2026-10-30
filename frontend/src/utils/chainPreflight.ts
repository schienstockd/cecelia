// What a chain run will hit on each image, before it starts (`POST /api/chains/preflight`,
// app/src/tasks/chain/preflight.jl). The whiteboard's Run asks first and shows these grouped per image.

export interface PreflightProblem {
  imageUid: string; nodeId: string; fn: string; level: 'fail' | 'warn'; message: string
}

/** Problems grouped per image, images with a failure first, then by name. */
export function groupByImage(problems: readonly PreflightProblem[], nameOf: (uid: string) => string = u => u) {
  const by = new Map<string, PreflightProblem[]>()
  for (const p of problems) (by.get(p.imageUid) ?? by.set(p.imageUid, []).get(p.imageUid)!).push(p)
  return [...by.entries()]
    .map(([uid, ps]) => ({ uid, name: nameOf(uid), problems: ps, fails: ps.some(p => p.level === 'fail') }))
    .sort((a, b) => Number(b.fails) - Number(a.fails) || a.name.localeCompare(b.name))
}
