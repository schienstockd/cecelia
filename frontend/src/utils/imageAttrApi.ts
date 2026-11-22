// Writing image attributes: the `/api/images/attr/*` routes, one function each. Each throws with
// the server's message on failure; the caller decides how to say so.
//
// `setImageAttr` returns what was STORED: the route trims values, so a caller updates its state from
// the reply, never from what it sent (`docs/API.md` → `attr/*`).

async function post(path: string, body: unknown): Promise<{ values?: Record<string, string> }> {
  const res = await fetch(path, {
    method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body),
  })
  const reply = await res.json().catch(() => ({})) as { error?: string; values?: Record<string, string> }
  if (!res.ok) throw new Error(reply.error ?? `HTTP ${res.status}`)
  return reply
}

/** Add attribute `attrName` (empty) to every image in `imageUids` — the column then exists. */
export async function createImageAttr(projectUid: string, imageUids: string[], attrName: string): Promise<void> {
  await post('/api/images/attr/create', { projectUid, imageUids, attrName })
}

/** Remove attribute `attrName` from every image in `imageUids`. */
export async function deleteImageAttr(projectUid: string, imageUids: string[], attrName: string): Promise<void> {
  await post('/api/images/attr/delete', { projectUid, imageUids, attrName })
}

/** Set `attrName` per image (`values`: uid → value); returns the values as stored. */
export async function setImageAttr(projectUid: string, attrName: string,
                                   values: Record<string, string>): Promise<Record<string, string>> {
  return (await post('/api/images/attr/set', { projectUid, attrName, values })).values ?? values
}

/**
 * Rename attribute `from` to `to` on every image in `imageUids`; returns uid → value as stored under
 * the new name. Refused (throws) when an image already has `to`. With `setUid`, the set's experiment
 * design follows the rename.
 */
export async function renameImageAttr(projectUid: string, imageUids: string[], from: string, to: string,
                                      setUid?: string): Promise<Record<string, string>> {
  return (await post('/api/images/attr/rename', { projectUid, imageUids, from, to, setUid })).values ?? {}
}

/**
 * Write a table import's plan (`utils/attrTable.ts` → `planImport`): create its new attribute columns on
 * every image in `imageUids`, then one `attr/set` per attribute with the changed cells. `store` hears
 * each step (the project store's `addAttrKey` / `setAttrValues`), fed from what was STORED. Returns the
 * number of values written. Shared by the Metadata page's table dialog and the design dialog's confirm.
 */
export async function writeImportPlan(
  projectUid: string, imageUids: string[],
  plan: { newAttrs: string[]; writes: Record<string, Record<string, string>> },
  store: { addAttrKey: (attr: string) => void; setAttrValues: (attr: string, values: Record<string, string>) => void },
): Promise<number> {
  for (const a of plan.newAttrs) {
    await createImageAttr(projectUid, imageUids, a)
    store.addAttrKey(a)
  }
  let n = 0
  for (const [a, values] of Object.entries(plan.writes)) {
    if (!Object.keys(values).length) continue
    store.setAttrValues(a, await setImageAttr(projectUid, a, values))
    n += Object.keys(values).length
  }
  return n
}
