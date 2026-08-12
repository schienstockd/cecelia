// THE positioner for anything that floats beside an anchor element: given the anchor's rect, the
// box's size and the viewport, return the fixed `top`/`left` to render at plus the side it ended up
// on (so the caller can point an arrow the right way).
//
// Pure and DOM-free on purpose — the callers do the measuring (`getBoundingClientRect`, `offsetWidth`)
// and this does the arithmetic, so the arithmetic is testable. Two consumers:
//   - `TeleportPopover.vue` — dropdowns/menus, `bottom-start`/`bottom-end`
//   - `GuideBubble.vue`     — guide bubbles, all four sides + centred alignment
// Extracted FROM TeleportPopover (whose `reposition()` was exactly this, minus two sides). Do not add
// a third copy of "clamp a floating box into the viewport" — extend the placement grammar here.
//
// The rules, in order:
//   1. place on the requested side, `gap` px from the anchor edge;
//   2. if it would overflow that side, FLIP to the opposite side — but only if the opposite side has
//      more room, so a box taller than the viewport doesn't ping-pong;
//   3. clamp the cross axis into the viewport (never off-screen, `margin` px minimum);
//   4. clamp the main axis too, as a last resort — a box with nowhere to go is still readable.

export type Side = 'top' | 'bottom' | 'left' | 'right'
export type Align = 'start' | 'center' | 'end'

// `<side>` alone means centred on that edge; `-start`/`-end` align to the anchor's leading/trailing
// edge (left/right for top+bottom, top/bottom for left+right).
export type Placement =
  | 'top' | 'top-start' | 'top-end'
  | 'bottom' | 'bottom-start' | 'bottom-end'
  | 'left' | 'left-start' | 'left-end'
  | 'right' | 'right-start' | 'right-end'

export interface AnchorRect { top: number; left: number; width: number; height: number }
export interface BoxSize { width: number; height: number }
export interface ViewportSize { width: number; height: number }

export interface PlaceOpts {
  anchor: AnchorRect
  box: BoxSize
  viewport: ViewportSize
  placement?: Placement
  gap?: number        // px between the anchor edge and the box (default 4)
  margin?: number     // px minimum from the viewport edge (default 4)
}

export interface Placed {
  top: number
  left: number
  side: Side          // where it ACTUALLY landed (may differ from the request — see `flipped`)
  flipped: boolean
}

// 'bottom-start' → { side: 'bottom', align: 'start' }; 'left' → { side: 'left', align: 'center' }
export function parsePlacement(p: Placement): { side: Side; align: Align } {
  const [side, align] = p.split('-') as [Side, Align | undefined]
  return { side, align: align ?? 'center' }
}

const OPPOSITE: Record<Side, Side> = { top: 'bottom', bottom: 'top', left: 'right', right: 'left' }

function clamp(v: number, lo: number, hi: number): number {
  // lo wins when the box is bigger than the space (hi < lo) — better pinned to the near edge than
  // pushed off the far one.
  return Math.max(lo, Math.min(v, hi))
}

// How much room is there between the anchor and the viewport edge on `side`?
function roomOn(side: Side, a: AnchorRect, vp: ViewportSize): number {
  switch (side) {
    case 'top':    return a.top
    case 'bottom': return vp.height - (a.top + a.height)
    case 'left':   return a.left
    case 'right':  return vp.width - (a.left + a.width)
  }
}

// Main-axis coordinate (the one the side dictates) for a box on `side`.
function mainCoord(side: Side, a: AnchorRect, box: BoxSize, gap: number): number {
  switch (side) {
    case 'top':    return a.top - box.height - gap
    case 'bottom': return a.top + a.height + gap
    case 'left':   return a.left - box.width - gap
    case 'right':  return a.left + a.width + gap
  }
}

// Cross-axis coordinate: `start` aligns leading edges, `end` trailing, `center` centres.
function crossCoord(side: Side, align: Align, a: AnchorRect, box: BoxSize): number {
  const vertical = side === 'top' || side === 'bottom'
  const anchorStart = vertical ? a.left : a.top
  const anchorSize  = vertical ? a.width : a.height
  const boxSize     = vertical ? box.width : box.height
  if (align === 'start') return anchorStart
  if (align === 'end')   return anchorStart + anchorSize - boxSize
  return anchorStart + anchorSize / 2 - boxSize / 2
}

export function placeBox(opts: PlaceOpts): Placed {
  const { anchor: a, box, viewport: vp } = opts
  const gap = opts.gap ?? 4
  const margin = opts.margin ?? 4
  const { side: want, align } = parsePlacement(opts.placement ?? 'bottom-start')

  // 1-2. does the requested side fit? if not, flip — but only when the opposite side is roomier.
  const needed = (side: Side) => (side === 'top' || side === 'bottom' ? box.height : box.width) + gap
  let side = want
  let flipped = false
  if (roomOn(want, a, vp) < needed(want) + margin) {
    const other = OPPOSITE[want]
    if (roomOn(other, a, vp) > roomOn(want, a, vp)) { side = other; flipped = true }
  }

  const vertical = side === 'top' || side === 'bottom'
  let main  = mainCoord(side, a, box, gap)
  let cross = crossCoord(side, align, a, box)

  // 3-4. clamp both axes into the viewport.
  main  = clamp(main,  margin, (vertical ? vp.height - box.height : vp.width  - box.width)  - margin)
  cross = clamp(cross, margin, (vertical ? vp.width  - box.width  : vp.height - box.height) - margin)

  return vertical
    ? { top: Math.round(main),  left: Math.round(cross), side, flipped }
    : { top: Math.round(cross), left: Math.round(main),  side, flipped }
}

// Where the arrow sits ALONG the box's anchored edge, as a px offset from the box's leading corner —
// so an arrow points at the anchor's centre even after the cross axis was clamped. Callers clamp it
// away from the box's rounded corners themselves (`inset`).
export function arrowOffset(placed: Placed, a: AnchorRect, box: BoxSize, inset = 10): number {
  const vertical = placed.side === 'top' || placed.side === 'bottom'
  const anchorMid = vertical ? a.left + a.width / 2 : a.top + a.height / 2
  const boxStart  = vertical ? placed.left : placed.top
  const boxSize   = vertical ? box.width : box.height
  return clamp(anchorMid - boxStart, inset, boxSize - inset)
}
