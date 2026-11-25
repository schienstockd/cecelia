// Place centred label chips (cluster-map centroid labels) so each sits whole inside the plot box and
// they don't overlap one another — the geom_label_repel idea without the physics. Each chip starts on
// its anchor, is clamped into the box, then overlapping pairs are pushed apart along the axis of least
// overlap (usually vertical: chips are wide + short) and re-clamped, a few dozen rounds. Pure: the
// on-screen HTML overlay and the SVG export call it with the same sizes, so they place identically.

export interface Chip { x: number; y: number; w: number; h: number }   // centre + size, px
export interface Inset { left?: number; right?: number; top?: number; bottom?: number }

export function placeChips(chips: Chip[], W: number, H: number, inset: Inset = {}, gap = 2): { x: number; y: number }[] {
  const l = inset.left ?? 0, r = W - (inset.right ?? 0), t = inset.top ?? 0, b = H - (inset.bottom ?? 0)
  // a chip wider than the box centres in it rather than clamping to a negative span
  const clampX = (x: number, w: number) => (r - l <= w ? (l + r) / 2 : Math.min(Math.max(x, l + w / 2), r - w / 2))
  const clampY = (y: number, h: number) => (b - t <= h ? (t + b) / 2 : Math.min(Math.max(y, t + h / 2), b - h / 2))
  const p = chips.map(c => ({ x: clampX(c.x, c.w), y: clampY(c.y, c.h) }))
  for (let round = 0; round < 60; round++) {
    let moved = false
    for (let i = 0; i < p.length; i++) for (let j = i + 1; j < p.length; j++) {
      const ox = (chips[i].w + chips[j].w) / 2 + gap - Math.abs(p[i].x - p[j].x)
      const oy = (chips[i].h + chips[j].h) / 2 + gap - Math.abs(p[i].y - p[j].y)
      if (ox <= 0 || oy <= 0) continue
      moved = true
      // push along the axis of least overlap — unless the box has no room to part them that way (two
      // wide labels side by side: a sideways push would only pin them to the walls, still overlapping)
      const roomX = (r - l) - (chips[i].w + chips[j].w) / 2 >= (chips[i].w + chips[j].w) / 2 + gap
      const roomY = (b - t) - (chips[i].h + chips[j].h) / 2 >= (chips[i].h + chips[j].h) / 2 + gap
      // split the push between the pair; ties (identical anchors) break by index so they still part
      if ((oy <= ox && roomY) || !roomX) {
        const s = (p[i].y < p[j].y || (p[i].y === p[j].y && i < j)) ? -1 : 1
        p[i].y = clampY(p[i].y + s * oy / 2, chips[i].h); p[j].y = clampY(p[j].y - s * oy / 2, chips[j].h)
      } else {
        const s = (p[i].x < p[j].x || (p[i].x === p[j].x && i < j)) ? -1 : 1
        p[i].x = clampX(p[i].x + s * ox / 2, chips[i].w); p[j].x = clampX(p[j].x - s * ox / 2, chips[j].w)
      }
    }
    if (!moved) break
  }
  return p
}
