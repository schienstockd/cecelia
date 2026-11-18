// What a "+ Plot…" card draws: the SUBJECT of a plot — the cell, track, image or states it measures —
// not its chart shape. The shape is the same for every plot of a kind (every "Compare groups" plot
// opens as a box plot), so the picker shows it once, on the kind's tile (PlotKindSketch.vue), and the
// cards use this to tell the plots of one kind apart.
//
// Built from a few shared SUBJECTS (cell, track, image, states, …) plus a modifier (× 2 images, ×
// frames, → card), so a new plot is usually a reuse, not a new drawing. Everything is plain shapes on
// a 96×60 box in theme classes; PlotSketch.vue only renders the list. Paths use absolute M/L/Q/C/Z
// only, so `placed` can move and scale a subject into a frame and a test can check nothing leaves
// the box.

export type SketchClass =
  | 'ink' | 'dim' | 'acc' | 'dash' | 'thick' | 'lane' | 'frame' | 'fill' | 'fill-dim' | 'fill-acc' | 'heat'

export type SketchShape =
  | { el: 'circle'; x: number; y: number; r: number; cls: string }
  | { el: 'ellipse'; x: number; y: number; rx: number; ry: number; cls: string }
  | { el: 'rect'; x: number; y: number; w: number; h: number; cls: string; op?: number }
  | { el: 'path'; d: string; cls: string }

export const SKETCH_W = 96
export const SKETCH_H = 60

export const PLOT_SKETCH_KINDS = [
  'cell', 'track', 'popImage', 'cellDistance', 'contactImages', 'spreadImages', 'popOverTime',
  'states', 'motif', 'movementPatterns', 'contacts', 'stateSignature', 'transitions', 'clusters', 'umap',
  'gate', 'image', 'tracksStar', 'trackLanes', 'trackCard', 'motifCard', 'statesCard', 'masks',
  'stateCount', 'trackCheck', 'planes', 'probability', 'curve', 'generic',
] as const
export type PlotSketchKind = typeof PLOT_SKETCH_KINDS[number]
export const isPlotSketchKind = (v: unknown): v is PlotSketchKind =>
  typeof v === 'string' && (PLOT_SKETCH_KINDS as readonly string[]).includes(v)

// ---- primitives ----
const dot = (x: number, y: number, r: number, cls: string): SketchShape => ({ el: 'circle', x, y, r, cls })
const path = (cls: string, d: string): SketchShape => ({ el: 'path', d, cls })
const frame = (x: number, y: number, w: number, h: number): SketchShape => ({ el: 'rect', x, y, w, h, cls: 'frame' })
const group = (x: number, y: number, rx: number, ry: number): SketchShape => ({ el: 'ellipse', x, y, rx, ry, cls: 'acc dash' })
const dots = (pts: number[][], r: number, cls: string) => pts.map(([x, y]) => dot(x!, y!, r, cls))

/** An arrowhead at (x, y) pointing along (dx, dy). */
function head(x: number, y: number, dx: number, dy: number, cls: string): SketchShape {
  const a = Math.atan2(dy, dx), h = 5
  const p = (s: number) => `${(x - h * Math.cos(a + s)).toFixed(1)} ${(y - h * Math.sin(a + s)).toFixed(1)}`
  return path(cls, `M${p(-0.45)} L${x} ${y} L${p(0.45)}`)
}
const arrow = (x1: number, y1: number, x2: number, y2: number, cls = 'ink') =>
  [path(cls, `M${x1} ${y1} L${x2} ${y2}`), head(x2, y2, x2 - x1, y2 - y1, cls)]
const curvedArrow = (x1: number, y1: number, cx: number, cy: number, x2: number, y2: number, cls: string) =>
  [path(cls, `M${x1} ${y1} Q${cx} ${cy} ${x2} ${y2}`), head(x2, y2, x2 - cx, y2 - cy, cls)]

/** Move and scale shapes (a subject placed into a frame or a card). */
export function placed(shapes: SketchShape[], dx: number, dy: number, s: number): SketchShape[] {
  const X = (v: number) => +(dx + v * s).toFixed(2), Y = (v: number) => +(dy + v * s).toFixed(2)
  return shapes.map(sh => {
    switch (sh.el) {
      case 'circle': return { ...sh, x: X(sh.x), y: Y(sh.y), r: +(sh.r * s).toFixed(2) }
      case 'ellipse': return { ...sh, x: X(sh.x), y: Y(sh.y), rx: sh.rx * s, ry: sh.ry * s }
      case 'rect': return { ...sh, x: X(sh.x), y: Y(sh.y), w: sh.w * s, h: sh.h * s }
      case 'path': {
        let i = 0
        return { ...sh, d: sh.d.replace(/-?\d+(\.\d+)?/g, n => String(i++ % 2 ? Y(+n) : X(+n))) }
      }
    }
  })
}

// ---- subjects ----
const BLOB = 'M30 30 Q33 16 46 19 Q58 22 60 30 Q70 33 67 42 Q63 50 53 48 Q44 54 35 48 Q26 43 30 30 Z'
const TRACK = 'M8 50 L18 40 L24 45 L34 31 L44 36 L52 23 L64 28 L74 15 L88 11'

const cell = (): SketchShape[] => [
  ...placed([path('dim dash', BLOB)], -10, 4, 1), path('acc fill-acc', BLOB), ...arrow(46, 32, 84, 14)]

const track = (): SketchShape[] => [
  path('ink', TRACK), path('acc dash', 'M8 50 L88 11'), dot(8, 50, 2.5, 'dim fill'), dot(88, 11, 2.5, 'acc fill')]

const popImage = (): SketchShape[] => [frame(14, 6, 68, 48),
  ...dots([[28, 18], [52, 40], [66, 20], [36, 42]], 4, 'acc fill'),
  ...dots([[44, 16], [24, 32], [58, 30], [70, 42], [42, 29], [30, 46]], 4, 'fill-dim')]

const STATE_PARTS: [string, string][] = [
  ['acc', 'M6 46 L18 40 L30 42'], ['ink', 'M30 42 L38 28 L50 30'],
  ['dim', 'M50 30 L56 25 L62 31 L68 25'], ['acc', 'M68 25 L78 18 L90 12']]
const states = (): SketchShape[] => STATE_PARTS.map(([c, d]) => path(`${c} thick`, d))

const motif = (): SketchShape[] => [
  path('dim', 'M6 46 L18 40 L30 42 L38 28 L50 30 L56 25 L62 31 L68 25 L78 18 L90 12'),
  path('ink thick', 'M38 28 L50 30 L56 25 L62 31'), { el: 'rect', x: 34, y: 18, w: 32, h: 18, cls: 'acc dash' }]

const transitions = (): SketchShape[] => [
  dot(22, 32, 10, 'acc'), dot(74, 32, 10, 'ink'),
  ...curvedArrow(30, 25, 48, 12, 65, 25, 'ink'), ...curvedArrow(66, 39, 48, 52, 31, 39, 'dim'),
  path('acc', 'M15 24 C4 10 24 6 21 21'), head(21, 21, -1, 5, 'acc')]

const clusters = (): SketchShape[] => [
  ...dots([[12, 18], [20, 26], [14, 34], [24, 14]], 3, 'ink'),
  ...dots([[46, 40], [54, 48], [42, 50], [56, 36]], 3, 'dim'),
  ...dots([[76, 16], [86, 22], [80, 28]], 3, 'ink'),
  group(18, 24, 15, 16), group(49, 43, 14, 13), group(81, 22, 12, 12)]

const image = (): SketchShape[] => [frame(6, 6, 84, 48),
  ...placed([path('acc fill-acc', BLOB)], -14, -6, 0.8), ...placed([path('ink', BLOB)], 30, 8, 0.7)]

/** Two images side by side, a subject in each. */
const twoImages = (a: SketchShape[], b: SketchShape[]): SketchShape[] =>
  [frame(2, 6, 44, 48), ...a, frame(50, 6, 44, 48), ...b]

/** A stack of cards with the subject on the front one. */
const card = (inner: SketchShape[]): SketchShape[] =>
  [frame(20, 2, 70, 48), frame(15, 6, 70, 48), frame(10, 10, 70, 48), ...placed(inner, 12, 14, 0.66)]

/** Deterministic pseudo-random (a sketch must draw the same every time). */
function seeded(seed: number) {
  let s = seed
  return () => { s = (s * 16807) % 2147483647; return (s - 1) / 2147483646 }
}

function tracksStar(): SketchShape[] {
  const rnd = seeded(7), out: SketchShape[] = []
  for (let i = 0; i < 11; i++) {
    const a = (2 * Math.PI * i) / 11 + (rnd() - 0.5) * 0.4, len = 14 + rnd() * 12
    let x = 48, y = 30
    const pts = [[x, y]]
    for (let k = 0; k < 5; k++) {
      const aa = a + (rnd() - 0.5) * 1.2
      x += (len / 5) * Math.cos(aa) * 1.35; y += (len / 5) * Math.sin(aa); pts.push([x, y])
    }
    out.push(path(i % 2 ? 'acc' : 'ink', 'M' + pts.map(([px, py]) => `${px!.toFixed(1)} ${py!.toFixed(1)}`).join(' L')))
  }
  return [...out, dot(48, 30, 2, 'dim fill')]
}

const lane = (cls: string, x1: number, x2: number, y: number) => path(`${cls} lane`, `M${x1} ${y} L${x2} ${y}`)
const MASKS: [number, number, number][] = [[26, 24, 8], [48, 38, 9], [70, 22, 8]]

const ART: Record<PlotSketchKind, () => SketchShape[]> = {
  cell,
  track,
  popImage,
  cellDistance: () => [dot(22, 36, 9, 'acc fill'), dot(76, 24, 9, 'fill-dim'),
    path('ink', 'M32 33 L66 27 M32 29 L32 37 M66 23 L66 31')],
  contactImages: () => twoImages(
    [dot(17, 30, 8, 'acc fill'), dot(32, 30, 7, 'ink'), path('acc thick', 'M24.5 23 L24.5 37')],
    [dot(62, 40, 8, 'acc fill'), dot(82, 18, 7, 'ink')]),
  spreadImages: () => twoImages(
    [...dots([[12, 18], [16, 15], [15, 21], [19, 19], [11, 23]], 2.4, 'acc fill'),
      ...dots([[34, 16], [30, 42], [12, 44], [38, 30], [24, 36]], 2.4, 'fill-dim')],
    [...dots([[58, 14], [86, 18], [70, 30], [58, 44], [86, 44]], 2.4, 'acc fill'),
      ...dots([[72, 16], [62, 30], [82, 32], [72, 46]], 2.4, 'fill-dim')]),
  popOverTime: () => [4, 35, 66].flatMap((ox, f) => [frame(ox, 8, 26, 44),
    ...[[8, 10], [20, 30], [12, 40], [22, 14], [16, 22]].map(([x, y], i) =>
      dot(ox + x!, 8 + y!, 3, i < [1, 3, 5][f]! ? 'acc fill' : 'fill-dim'))]),
  states,
  motif,
  movementPatterns: () => [group(16, 30, 13, 20), path('ink', 'M10 42 L22 18'),
    group(48, 30, 13, 20), path('ink', 'M38 38 L42 28 L46 38 L50 28 L54 38'),
    group(80, 30, 13, 20), path('ink', 'M72 46 C70 36 90 34 86 26 C82 18 72 22 76 30 C79 36 88 30 84 22 L82 14')],
  contacts: () => [dot(20, 20, 6, 'acc fill'), dot(32, 24, 6, 'ink'), dot(60, 16, 6, 'fill-dim'),
    dot(72, 40, 6, 'acc fill'), dot(62, 46, 6, 'ink'), dot(26, 46, 6, 'fill-dim'), dot(86, 20, 6, 'ink'),
    path('acc', 'M25.5 18 L25.5 26 M67 38 L67 46')],
  stateSignature: () => [path('dim', 'M6 52 L20 44 L30 46'), path('acc thick', 'M30 46 L40 34 L50 36'),
    path('dim', 'M50 36 L64 26 L76 24'), path('dim dash', 'M42 33 L60 18'),
    ...placed([path('acc fill-acc', BLOB)], 38, -8, 0.55)],
  transitions,
  clusters,
  umap: () => [path('dim', 'M4 4 L4 56 L92 56'),
    ...dots([[18, 18], [22, 14], [26, 20], [20, 24], [28, 15], [15, 22], [24, 26], [30, 21], [17, 13]], 1.8, 'acc fill'),
    ...dots([[52, 40], [58, 44], [56, 36], [62, 40], [50, 46], [60, 48], [64, 35], [55, 50], [48, 38]], 1.8, 'ink fill'),
    ...dots([[74, 14], [80, 18], [78, 10], [84, 14], [72, 20], [86, 22], [80, 24], [76, 26]], 1.8, 'dim fill')],
  gate: () => [...dots([[14, 44], [20, 40], [18, 48], [26, 46], [54, 20], [60, 26], [66, 18], [62, 22], [70, 28],
    [58, 14], [74, 20]], 2, 'dim fill'), { el: 'rect', x: 48, y: 8, w: 34, h: 26, cls: 'acc fill-acc' }],
  image,
  tracksStar,
  trackLanes: () => [lane('acc', 8, 44, 12), lane('ink', 28, 86, 24), lane('acc', 14, 40, 36),
    lane('ink', 46, 88, 36), lane('acc', 6, 64, 48)],
  trackCard: () => card(track()),
  motifCard: () => card(motif()),
  statesCard: () => card(states()),
  masks: () => [frame(6, 6, 84, 48),
    ...MASKS.map(([x, y, r]): SketchShape => ({ el: 'ellipse', x, y, rx: r, ry: r - 1, cls: 'fill-dim' })),
    ...MASKS.map(([x, y, r]): SketchShape => ({ el: 'ellipse', x, y, rx: r + 1, ry: r, cls: 'acc' }))],
  stateCount: () => [
    lane('acc', 8, 44, 14), lane('ink', 46, 86, 14),
    lane('acc', 8, 32, 30), lane('ink', 34, 60, 30), lane('dim', 62, 86, 30),
    lane('acc', 8, 26, 46), lane('ink', 28, 46, 46), lane('dim', 48, 66, 46), lane('acc', 68, 86, 46)],
  trackCheck: () => [...track(), path('acc thick', 'M58 48 L64 54 L78 40')],
  planes: () => [frame(18, 4, 60, 40), frame(12, 10, 60, 40), frame(6, 16, 60, 40)],
  probability: () => [frame(4, 10, 40, 40), ...placed(image(), 6, 12, 0.38), ...arrow(46, 30, 52, 30),
    frame(54, 10, 40, 40),
    ...Array.from({ length: 16 }, (_, k): SketchShape => {
      const r = Math.floor(k / 4), c = k % 4
      return { el: 'rect', x: 56 + c * 9, y: 12 + r * 9, w: 8.6, h: 8.6, cls: 'heat', op: 0.1 + (((r * 5 + c * 3) % 7) / 7) * 0.8 }
    })],
  curve: () => [path('dim', 'M6 6 L6 54 L90 54'), path('ink', 'M10 12 C20 40 40 46 88 48')],
  // a plugin plot that names no sketch: deliberately plain, not a blank card
  generic: () => [path('dim', 'M6 6 L6 54 L90 54'), path('dim dash', 'M10 46 L86 14'),
    ...dots([[20, 40], [34, 34], [48, 30], [62, 24], [76, 20]], 2.6, 'acc fill')],
}

export const plotSketchArt = (kind: PlotSketchKind): SketchShape[] => ART[kind]()
