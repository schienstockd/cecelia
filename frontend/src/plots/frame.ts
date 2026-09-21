/**
 * Normalised 0..1 coord frame for a plot's DRAWN AREA — the axis rect on a scatter, the image
 * sub-rect inside `object-fit: contain`, the centred dot cloud inside a UMAP square panel. NOT
 * the outer container: `(0.5, 0.5)` on a padded scatter's outer rect lands in the axis gutter.
 *
 * Consumed by (soon) the Claude point-out subscription: `{family, plotId, coords: {u, v}}` →
 * each family renders a marker at `fromNorm(u, v)`. Consumed also by the reverse — a user's
 * click can be turned into `{u, v}` via `toNorm` and shared back. Kept coord-frame-only here
 * because a family may render its marker any way it likes (halo, ring, tile outline, pin) —
 * the Frame just answers "which pixel does this normalised point sit on".
 *
 * A Frame is LIVE, not a snapshot: `toNorm` / `fromNorm` read the current DOM rect on each
 * call, so a resize / scroll between two calls is fine (each returns coords in the current
 * frame). Returns `null` when the reference DOM element hasn't mounted / is unmounted /
 * isn't measurable — never throws; callers decide how to react.
 *
 * A Frame's coord basis is CLIENT SPACE (viewport coords, same frame `getBoundingClientRect`
 * returns) so an overlay component mounted anywhere in the tree can project a point without
 * knowing where the plot lives in the DOM.
 */
export interface Frame {
  /** Convert a client-space point to 0..1 in the drawn area. Returns `null` when the frame
   *  isn't measurable OR the point is outside the drawn area. */
  toNorm(clientX: number, clientY: number): NormPoint | null
  /** Convert 0..1 to client-space. `null` when the frame isn't measurable. Does NOT clamp — a
   *  caller can pass `(-0.1, 1.2)` to get an off-frame anchor for e.g. a leader line. */
  fromNorm(u: number, v: number): ClientPoint | null
  /** For multi-cell families (image strip cells, pairs matrix, faceted UMAP): each cell exposes
   *  its own Frame keyed by a stable id. Absent (or empty array) = single-cell plot. */
  subFrames?(): FrameCell[]
}

export interface NormPoint { u: number; v: number }
export interface ClientPoint { clientX: number; clientY: number }

export interface FrameCell {
  /** Stable identifier for the sub-frame — a point-out envelope like
   *  `{family: 'imageStrip', plotId, cell: '3'}` can address it. Shape depends on the family
   *  (`row=2,col=3`, `cell=7`, `facet=speed`); readers should treat it as opaque. */
  key: string
  /** Optional human-readable label if the key alone isn't clear (`'Speed vs Angle'` for a
   *  facet). Used for a tooltip / accessible label, not for addressing. */
  label?: string
  frame: Frame
}

/** The DOMRect fields Frame implementations depend on — nothing else. Narrow so tests can hand
 *  in plain objects without constructing a real DOMRect. */
export interface FrameRect {
  left: number
  top: number
  width: number
  height: number
}

/**
 * Frame whose drawn area is the FULL container rect — no letterbox, no padding. Used by
 * families where the container IS the axis rect (gating scatter's `.plot-capture`, the base
 * case for a tile whose source already matches the cell aspect).
 */
export function rectFrame(getRect: () => FrameRect | null): Frame {
  return {
    toNorm(clientX, clientY) {
      const r = getRect()
      if (!r || r.width <= 0 || r.height <= 0) return null
      const u = (clientX - r.left) / r.width
      const v = (clientY - r.top) / r.height
      if (u < 0 || u > 1 || v < 0 || v > 1) return null
      return { u, v }
    },
    fromNorm(u, v) {
      const r = getRect()
      if (!r || r.width <= 0 || r.height <= 0) return null
      return { clientX: r.left + u * r.width, clientY: r.top + v * r.height }
    },
  }
}

/**
 * Frame whose drawn area is a CENTRED SUB-RECT of the container with a given natural aspect
 * (naturalWidth / naturalHeight). Matches how the browser lays out `object-fit: contain` on
 * an `<img>` and how `preserveAspectRatio="xMidYMid meet"` lays out an SVG viewBox — so a
 * mark projected here lands on the same pixel a matching-viewBox SVG would render it at.
 *
 * `naturalAspect` is a number OR a getter (aspect may not be known until the source loads —
 * an `<img>` fires `@load` with `naturalWidth`/`naturalHeight` on both first paint and any src
 * swap). A getter is re-read on every `toNorm`/`fromNorm` call, so the frame updates live.
 *
 * `naturalAspect <= 0` (missing / not-yet-loaded natural size) collapses to `rectFrame`
 * semantics — the container itself. Callers that only need the letterboxed path can gate on
 * a positive aspect before calling.
 */
export function letterboxFrame(
  getRect: () => FrameRect | null,
  naturalAspect: number | (() => number),
): Frame {
  const aspect = () => (typeof naturalAspect === 'function' ? naturalAspect() : naturalAspect)
  const drawn = (r: FrameRect | null): FrameRect | null => {
    if (!r || r.width <= 0 || r.height <= 0) return null
    const a = aspect()
    if (!(a > 0)) return r
    const containerAspect = r.width / r.height
    let w: number, h: number
    if (a > containerAspect) {
      // wider than the container → letterbox top / bottom
      w = r.width
      h = r.width / a
    } else {
      // taller / narrower → letterbox left / right
      h = r.height
      w = r.height * a
    }
    return {
      left: r.left + (r.width - w) / 2,
      top: r.top + (r.height - h) / 2,
      width: w,
      height: h,
    }
  }
  return {
    toNorm(clientX, clientY) {
      const d = drawn(getRect())
      if (!d) return null
      const u = (clientX - d.left) / d.width
      const v = (clientY - d.top) / d.height
      if (u < 0 || u > 1 || v < 0 || v > 1) return null
      return { u, v }
    },
    fromNorm(u, v) {
      const d = drawn(getRect())
      if (!d) return null
      return { clientX: d.left + u * d.width, clientY: d.top + v * d.height }
    },
  }
}
