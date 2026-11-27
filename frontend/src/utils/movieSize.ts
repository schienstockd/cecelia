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
 * What a 2D viewer recording with blank size fields comes out at: `screen` the viewer's canvas size,
 * `full` one movie pixel per image pixel of the visible region (up to MAX_MOVIE_AXIS). The backend
 * reads it as `resolution` (`_record_max_px`, api/src/movie_rail.jl).
 */
export const MOVIE_RESOLUTIONS = ['screen', 'full'] as const
export type MovieResolution = typeof MOVIE_RESOLUTIONS[number]
export const MOVIE_RESOLUTION_DEFAULT: MovieResolution = 'screen'
/** The `res` control's three states: the two resolutions, or typed sizes (`custom`). */
export type MovieSizeMode = MovieResolution | 'custom'

/** Typed sizes are `custom`, whatever resolution is stored; else the stored resolution. */
export function movieSizeMode(sizeX: number | null, sizeY: number | null,
                              resolution: MovieResolution | null | undefined): MovieSizeMode {
  return parseMovieAxis(sizeX) && parseMovieAxis(sizeY) ? 'custom' : (resolution ?? MOVIE_RESOLUTION_DEFAULT)
}

/**
 * What picking a `res` mode writes. A resolution clears the sizes (typed sizes would win over it);
 * `custom` starts the fields at the canvas size, so the first movie matches what `screen` gave.
 */
export function movieSizePick(mode: MovieSizeMode, canvasX: number | null | undefined,
                              canvasY: number | null | undefined):
  { sizeX: number | null, sizeY: number | null, resolution?: MovieResolution } {
  if (mode !== 'custom') return { sizeX: null, sizeY: null, resolution: mode }
  return { sizeX: parseMovieAxis(canvasX) ?? 1024, sizeY: parseMovieAxis(canvasY) ?? 1024 }
}

/**
 * Both axes, as sent to the API — `{}` when either is unset, since a single axis cannot describe a
 * frame and half a size would silently letterbox. One rule, so all three surfaces agree. With no sizes,
 * `resolution: 'full'` rides along when asked for (`screen` is the backend's default, so it is omitted).
 */
export function movieSizeParams(sizeX: number | null, sizeY: number | null,
                                resolution?: MovieResolution | null):
  { sizeX?: number, sizeY?: number, resolution?: MovieResolution } {
  const x = parseMovieAxis(sizeX)
  const y = parseMovieAxis(sizeY)
  if (x && y) return { sizeX: x, sizeY: y }
  return resolution === 'full' ? { resolution } : {}
}
