/**
 * Movie output size, frontend half. The pixel-level rules (clamp, even axes) live in Python
 * (`cecelia/utils/movie_io.py`) because that is where the frame is rendered and where h.264 rejects an
 * odd dimension — this only turns what the user typed into "a size" or "blank = the canvas size".
 */

/** The per-axis ceiling; mirrors `movie_io.MAX_MOVIE_AXIS` so the field can't ask for a clamp. */
export const MAX_MOVIE_AXIS = 4096

/**
 * A typed axis value → a positive integer, or null for "unset" (blank, zero, junk).
 * Null is not an error state: an empty field means "record at the viewer's canvas size", which is the
 * default and what every movie was before these fields existed.
 */
export function parseMovieAxis(value: string | number | null | undefined): number | null {
  if (value === null || value === undefined || value === '') return null
  const n = Math.floor(Number(value))
  if (!Number.isFinite(n) || n <= 0) return null
  return Math.min(n, MAX_MOVIE_AXIS)
}

/**
 * What to show in an empty size field: the browser viewer's current canvas size when we have one,
 * else a word. The point is that the default is VISIBLE — a movie records at the canvas size, and a
 * blank field that says nothing makes that look like "no size at all".
 */
export function movieAxisPlaceholder(canvasAxis: number | null | undefined): string {
  return canvasAxis && canvasAxis > 0 ? String(canvasAxis) : 'canvas'
}

/**
 * Both axes, as sent to the API — `{}` when either is unset, since a single axis cannot describe a
 * frame and half a size would silently letterbox. One rule, so all three surfaces agree.
 */
export function movieSizeParams(sizeX: number | null, sizeY: number | null): { sizeX?: number, sizeY?: number } {
  const x = parseMovieAxis(sizeX)
  const y = parseMovieAxis(sizeY)
  return x && y ? { sizeX: x, sizeY: y } : {}
}
