// Movie-player (/movies) pure helpers — kept out of the .vue SFC so they're unit-testable. The player
// lists the project's rendered .mp4s (GET /api/movies) and streams them via the range-capable serve
// route (GET /api/movies/file). See api/src/server.jl → try_serve_movie / api_movies_list.

export interface MovieEntry {
  name: string     // file name, e.g. "myImage_animation.mp4"
  size: number     // bytes
  mtime: number    // unix seconds (Float)
}

// URL the <video> element points at. The name is a server-sanitised basename ([A-Za-z0-9._-]+.mp4),
// but encode it anyway so it survives as a query param regardless.
export function movieStreamUrl(projectUid: string, name: string): string {
  return `/api/movies/file?projectUid=${encodeURIComponent(projectUid)}&name=${encodeURIComponent(name)}`
}

// Display label for a movie: drop the .mp4 extension. Everything else (uid suffix, attr parts) is left
// as-is — that's how the recorder named it, and the user chose those file attrs.
export function movieDisplayName(name: string): string {
  return name.replace(/\.mp4$/i, '')
}

// Newest-first, then by name for stable ordering of same-mtime files. The backend already sorts by
// mtime desc, but re-sorting here keeps the UI correct if the list is ever assembled client-side.
export function sortMovies(movies: MovieEntry[]): MovieEntry[] {
  return [...movies].sort((a, b) => (b.mtime - a.mtime) || a.name.localeCompare(b.name))
}

export interface Box { w: number; h: number }

// Mouse/centre-anchored zoom for the player's scroll viewport. Given the content box BEFORE and AFTER
// a zoom, the viewport size, the focal point (in viewport coords) and the current scroll, return the
// scroll that keeps the same content point under the focal point — so zooming grows about the cursor
// (or the viewport centre for keyboard zoom), not the top-left corner. Handles the `margin:auto`
// centring offset that applies while the content is smaller than the viewport (no scroll in that axis).
// Pure → unit-tested (movies.test.ts). Mirrors the intent of the plot canvas's zoom but with real
// scrollbars (layout-based box, not a CSS transform).
export function anchoredScroll(
  before: Box, after: Box, vp: { w: number; h: number },
  focal: { x: number; y: number }, scroll: { left: number; top: number },
): { left: number; top: number } {
  const axis = (b: number, a: number, v: number, f: number, s: number): number => {
    if (a <= v) return 0                                   // fits after zoom → centred, no scroll
    const off0 = b <= v ? (v - b) / 2 : -s                 // viewport-x of content pixel 0, before
    const frac = b > 0 ? (f - off0) / b : 0.5              // content fraction under the focal point
    return Math.max(0, Math.min(a - v, frac * a - f))      // keep that fraction under the focal point
  }
  return {
    left: axis(before.w, after.w, vp.w, focal.x, scroll.left),
    top:  axis(before.h, after.h, vp.h, focal.y, scroll.top),
  }
}
