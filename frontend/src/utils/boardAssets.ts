// Board sidecar-asset helpers for the Analysis board. A slot's persisted `state` may reference sidecar
// PNGs (settings/board-assets/<id>.png) by an `assetId` string — filmstrip cells (an array of cells),
// an image slot, or any future nesting. Duplicating a board must COPY each asset to a fresh id (so the
// two boards are independent); closing a board must DELETE its assets (so they don't orphan). Both walk
// the same reference graph, so the walker lives here as one pure, testable helper.

// Visit every `assetId` reference anywhere under `o` (recurses objects + arrays). `visit` receives the
// holding object + key + id, and MAY mutate the holder in place (e.g. rewrite the id to a copy).
export function walkAssetRefs(
  o: unknown,
  visit: (holder: Record<string, unknown>, key: string, id: string) => void,
): void {
  if (!o || typeof o !== 'object') return
  if (Array.isArray(o)) { for (const x of o) walkAssetRefs(x, visit); return }
  const rec = o as Record<string, unknown>
  for (const [k, v] of Object.entries(rec)) {
    if (k === 'assetId' && typeof v === 'string' && v) visit(rec, k, v)
    else walkAssetRefs(v, visit)
  }
}

// Every asset id referenced under `state`, in walk order.
export function collectAssetIds(state: unknown): string[] {
  const ids: string[] = []
  walkAssetRefs(state, (_h, _k, id) => ids.push(id))
  return ids
}
