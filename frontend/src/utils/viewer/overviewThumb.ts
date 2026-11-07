/**
 * When the 2D mini map's tissue thumbnail needs (re)fetching.
 *
 * The thumbnail is one fetch per image version, but it has to happen whenever the mini map can be
 * SEEN without one — not once at open. Fetching only at open, and only when the window opened in
 * 2D, left the mini map black for a window that opened in 3D and switched to 2D, and for good
 * after a single failed fetch. `loadedKey` is the key of the thumbnail held or in flight (`''`
 * after a failure, so the next trigger retries); `key` names the version on screen.
 */
export function overviewNeedsLoad(o: {
  mode: 'plane' | 'volume'
  shown: boolean
  starting: boolean
  hasMeta: boolean
  key: string
  loadedKey: string
}): boolean {
  return o.mode === 'plane' && o.shown && !o.starting && o.hasMeta && o.key !== o.loadedKey
}

/** The version a thumbnail was fetched for: the store and the cache-clear revision. */
export function overviewKey(valueName: string, cacheClearRev: number | string): string {
  return `${valueName}|${cacheClearRev}`
}
