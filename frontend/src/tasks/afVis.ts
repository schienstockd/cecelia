/**
 * What AF correction does with the channels the user picked — the figure offered beside
 * `correctChannels`.
 *
 * **Why this exists.** The form asks what each channel IS — a marker to correct, an AF-only channel,
 * another competing signal — and the roles do very different things to the same pixels. Markers and
 * competing channels take the share of each voxel they outshine; an AF channel zeroes the markers
 * wherever it is clearly bright. The one mistake worth preventing is naming an AF channel as
 * competing: AF granules are about as bright in the marker as in the AF channel, so the marker "wins"
 * them and keeps much of the AF. The verdict line says how much would stay if that happened.
 *
 * **Built from the selection.** One input column per picked channel (markers, then AF channels, then
 * competing), one output column per corrected marker — so two markers show them competing with each
 * other, and "different cell types" off adds a double-positive cell for them to split. Capped
 * (`MAX_MARKERS` etc.) so the float stays readable.
 *
 * **The scene is a schematic, not the user's data**, drawn the way `driftVis` draws one — soft
 * objects on a continuous low noise floor, deterministic (no `Math.random`) — but shaped like the
 * intravital T cells it is about: irregular amoeboid bodies with flat interiors, small AF puncta
 * scattered through the field and some sitting on cells. It carries the two properties the real data
 * showed: AF puncta ~0.6× as bright in a marker as in the AF channel and well above its noise, and each marker leaking ~9%
 * into the AF channel under bright cells. Cells cover most of the bright area, as they do in the real
 * data.
 *
 * **The outputs run the real arithmetic at small scale**, as `smoothVis` and `ridgesVis` do:
 *   - share: `b_t · b_t² / Σ b_i²` over the target, the other markers and the competing channels
 *     (`correction_utils.af_correct_frame` step (b), p = 2)
 *   - AF channel: smooth, subtract each marker's leak into it (`af_reference_leaks` /
 *     `af_reference_field`), threshold at median + 6·MAD-σ with a 2σ ramp (`AF_MASK_K`,
 *     `AF_MASK_RAMP`); several AF channels take the larger mask. One scale here; the run uses two.
 * The numbers and the verdict are read back off the frames being drawn.
 */
import type { VisCell, VisColumns, VisFrame, VisRow } from './paramVis'

const N = 32

/** Same constants as `correction_utils` — the figure claims to be that arithmetic. */
const MASK_K = 6
const MASK_RAMP = 2
const WEIGHT_P = 2

/** How much of each marker leaks into an AF channel in the scene (measured ~0.08-0.09 on real data). */
const LEAK = 0.09

/** Columns beyond these stop being readable at the float's width. */
export const MAX_MARKERS = 3
export const MAX_AF = 2
export const MAX_COMPETING = 2

function mulberry32(seed: number): () => number {
  let a = seed >>> 0
  return () => {
    a = (a + 0x6D2B79F5) >>> 0
    let t = Math.imul(a ^ (a >>> 15), 1 | a)
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296
  }
}

const zeros = (): VisFrame => Array.from({ length: N }, () => new Array<number>(N).fill(0))
const falses = (): boolean[][] => Array.from({ length: N }, () => new Array<boolean>(N).fill(false))

/** Separable gaussian, edges clamped — the schematic's stand-in for scipy's `gaussian_filter`. */
export function blur(f: VisFrame, sigma: number): VisFrame {
  const r = Math.max(1, Math.ceil(4 * sigma))
  const k: number[] = []
  for (let i = -r; i <= r; i++) k.push(Math.exp(-(i * i) / (2 * sigma * sigma)))
  const s = k.reduce((a, b) => a + b, 0)
  const w = k.map(v => v / s)
  const at = (i: number) => Math.min(N - 1, Math.max(0, i))
  const h = f.map(row => row.map((_, x) => w.reduce((acc, wk, d) => acc + wk * row[at(x + d - r)], 0)))
  return h.map((row, y) => row.map((_, x) => w.reduce((acc, wk, d) => acc + wk * h[at(y + d - r)][x], 0)))
}

/** Add a soft gaussian punctum of `amp` at (cy, cx) to `f`; returns its core (≥ half max). */
function addBlob(f: VisFrame, cy: number, cx: number, sigma: number, amp: number): boolean[][] {
  const core = falses()
  for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) {
    const g = Math.exp(-((y - cy) ** 2 + (x - cx) ** 2) / (2 * sigma * sigma))
    f[y][x] += amp * g
    if (g >= 0.5) core[y][x] = true
  }
  return core
}

/**
 * An amoeboid cell body: three or four elongated lobes at random angles around (cy, cx), summed and
 * passed through a soft step so the interior is flat and the edge is soft — the look of a migrating
 * T cell rather than a gaussian spot. Returns its core (body ≥ half).
 */
function addCell(f: VisFrame, cy: number, cx: number, amp: number, rnd: () => number): boolean[][] {
  const lobes = Array.from({ length: 3 + Math.floor(rnd() * 2) }, () => {
    const th = rnd() * Math.PI, d = 0.8 + rnd() * 2.2
    return { y: cy + d * Math.sin(th * 2), x: cx + d * Math.cos(th * 2), th,
             a: 2.0 + rnd() * 1.6, b: 1.0 + rnd() * 0.6 }
  })
  const core = falses()
  for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) {
    let g = 0
    for (const l of lobes) {
      const dy = y - l.y, dx = x - l.x
      const u = dx * Math.cos(l.th) + dy * Math.sin(l.th), v = -dx * Math.sin(l.th) + dy * Math.cos(l.th)
      g += Math.exp(-(u * u / (2 * l.a * l.a) + v * v / (2 * l.b * l.b)))
    }
    const body = 1 / (1 + Math.exp(-10 * (g - 0.45)))
    f[y][x] += amp * body * (0.85 + 0.25 * rnd())                    // a little interior texture
    if (body >= 0.5) core[y][x] = true
  }
  return core
}

/**
 * Cell slots across the field, with a brightness each — a packed field of cells of mixed brightness,
 * like an intravital frame. Dealt out to the markers in turn, so one marker gets all of them and two
 * markers get alternating cells.
 */
const SLOTS: Array<[number, number, number]> = [
  [6, 7, 1.0], [5, 18, 0.55], [8, 28, 0.8], [15, 4, 0.5], [13, 13, 0.9], [16, 23, 0.6],
  [24, 8, 0.75], [23, 18, 1.0], [27, 27, 0.45], [29, 13, 0.6],
]
/** Where the AF puncta sit: three on cells (slots 0, 4, 7), the rest in the tissue between them. */
const GRANULES: Array<[number, number]> = [[7, 8], [13, 14], [23, 19], [10, 21], [19, 11], [20, 29],
                                           [2, 12], [27, 3], [11, 2], [30, 21]]
/** Objects of each competing channel — placed to overlap the edges of some marker cells. */
const OTHER: Array<Array<[number, number]>> = [[[9, 12], [26, 22], [18, 2]], [[3, 26], [16, 18], [29, 9]]]

/** A double-positive cell for markers 0 and 1, when the user says cells can carry two markers. */
const DOUBLE: [number, number] = [19, 24]

export interface AfRolesInput {
  markers: string[]
  afs: string[]
  competing: string[]
  /** "different cell types" — off adds a cell carrying the first two markers */
  exclusive: boolean
}

export interface AfScene {
  markers: VisFrame[]
  afs: VisFrame[]
  competing: VisFrame[]
  /** AF granule cores */
  granule: boolean[][]
  /** per marker: its own cell cores, away from any granule */
  cells: boolean[][][]
}

export function afScene(inp: AfRolesInput, seed = 5): AfScene {
  const rnd = mulberry32(seed)
  const nm = Math.max(1, Math.min(MAX_MARKERS, inp.markers.length))
  const na = Math.min(MAX_AF, inp.afs.length)
  const nc = Math.min(MAX_COMPETING, inp.competing.length)
  const markers = Array.from({ length: nm }, zeros)
  const afs = Array.from({ length: na }, zeros)
  const competing = Array.from({ length: nc }, zeros)

  const granule = falses()
  for (const [gy, gx] of GRANULES) {
    // a granule is AF: full in every AF channel, ~0.6x in every marker
    const core = addBlob(zeros(), gy, gx, 0.7, 1)
    for (const a of afs) addBlob(a, gy, gx, 0.7, 1.2)
    for (const m of markers) addBlob(m, gy, gx, 0.7, 0.7)
    core.forEach((row, y) => row.forEach((v, x) => { if (v) granule[y][x] = true }))
  }
  const cells = markers.map(() => falses())
  SLOTS.forEach(([cy, cx, amp], k) => {
    const i = k % nm
    const core = addCell(markers[i], cy, cx, amp, rnd)
    core.forEach((row, y) => row.forEach((v, x) => { if (v && !granule[y][x]) cells[i][y][x] = true }))
  })
  if (!inp.exclusive && nm >= 2) {
    const shape = mulberry32(seed + 1)
    addCell(markers[0], DOUBLE[0], DOUBLE[1], 0.6, shape)
    addCell(markers[1], DOUBLE[0], DOUBLE[1], 0.6, mulberry32(seed + 1))
  }
  // a competing channel is another signal with objects of its own, some overlapping the markers'
  // cells — where it is brighter it takes the voxel, where the marker is brighter the marker keeps it
  competing.forEach((c, j) => {
    for (const [cy, cx] of OTHER[j]) addCell(c, cy, cx, 0.9, rnd)
  })
  // each marker leaks ~9% into every AF channel, then the continuous noise floor `driftVis` uses
  for (const a of afs) for (const m of markers)
    for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) a[y][x] += LEAK * m[y][x]
  for (const f of [...markers, ...afs, ...competing])
    for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) f[y][x] += 0.04 + 0.06 * rnd()
  // the AF channel is photon-starved: mostly empty, with single-photon speckle (the magenta specks
  // of a real CART+AF overlay). It is also what sets the mask's noise σ, and so how far a mask spreads.
  for (const a of afs)
    for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) if (rnd() < 0.2) a[y][x] += 0.15
  return { markers, afs, competing, granule, cells }
}

/** Background floor — the schematic's stand-in for the derived triangle threshold (noise ≤ 0.1). */
const FLOOR = 0.1
const sub = (f: VisFrame) => f.map(row => row.map(v => Math.max(0, v - FLOOR)))

/** Step (b): marker `t`'s share against `others` (background-subtracted fields). */
export function share(t: VisFrame, others: VisFrame[]): VisFrame {
  return t.map((row, y) => row.map((v, x) => {
    const num = v ** WEIGHT_P
    const den = others.reduce((acc, o) => acc + o[y][x] ** WEIGHT_P, num)
    return den > 0 ? v * num / den : 0
  }))
}

function median(v: number[]): number {
  const s = [...v].sort((a, b) => a - b), m = s.length >> 1
  return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2
}

/** Step (c): the fraction removed per pixel by one AF channel, its leak from the markers taken out. */
export function afMask(af: VisFrame, markers: VisFrame[], sigma = 1): VisFrame {
  let field = blur(af, sigma)
  for (const m of markers) {
    // The run FITS this leak (`af_reference_leaks`, 0.080 on `RFtn9f/jrNNl2`); the schematic uses the
    // value it was built with. At 1024 pixels one cluster of puncta can be half of a brightness bin,
    // a small-sample problem a movie of millions of voxels does not have.
    const bm = blur(m, sigma)
    field = field.map((row, y) => row.map((v, x) => v - LEAK * bm[y][x]))
  }
  const vals = field.flat()
  const level = median(vals)
  let sd = 1.4826 * median(vals.map(v => Math.abs(v - level)))
  if (sd <= 0) {
    const mean = vals.reduce((a, b) => a + b, 0) / vals.length
    sd = Math.sqrt(vals.reduce((a, b) => a + (b - mean) ** 2, 0) / vals.length)
  }
  const lo = level + MASK_K * sd
  return field.map(row => row.map(v => sd > 0 ? Math.min(1, Math.max(0, (v - lo) / (MASK_RAMP * sd))) : 0))
}

/** Each corrected marker, as the run would write it. `afAsCompeting` is the what-if for the verdict. */
export function correctAll(scene: AfScene, afAsCompeting = false): VisFrame[] {
  const bms = scene.markers.map(sub), bcs = scene.competing.map(sub), bas = scene.afs.map(sub)
  const masks = afAsCompeting ? [] : scene.afs.map(a => afMask(a, scene.markers))
  return bms.map((bm, i) => {
    const others = [...bms.filter((_, j) => j !== i), ...bcs, ...(afAsCompeting ? bas : [])]
    const out = share(bm, others)
    return out.map((row, y) => row.map((v, x) =>
      v * (1 - masks.reduce((mx, m) => Math.max(mx, m[y][x]), 0))))
  })
}

/** Share of a marker (above background) left on `where`, as a percentage. */
export function keptPct(out: VisFrame, marker: VisFrame, where: boolean[][]): number {
  const bm = sub(marker)
  let kept = 0, total = 0
  for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) {
    if (!where[y][x]) continue
    kept += out[y][x]; total += bm[y][x]
  }
  return total > 0 ? Math.round(100 * kept / total) : 0
}

function normTo01(frames: VisFrame[]): VisFrame[] {
  let peak = 0
  for (const f of frames) for (const row of f) for (const v of row) if (v > peak) peak = v
  const k = peak > 0 ? 1 / peak : 0
  return frames.map(f => f.map(row => row.map(v => Math.min(1, Math.max(0, v * k)))))
}

function cell(text: string, frames?: VisFrame[]): VisCell {
  return { value: 0, px: null, r: 0, at: 0, text, pxText: '', frames }
}

/** The names the figure draws — the selection, capped, with generic names when nothing is picked. */
export function figureChannels(inp: AfRolesInput): AfRolesInput {
  // `channelRoles` gives each channel one role; dedupe anyway, so a REPL-built input reads the same
  const markers = inp.markers.length ? [...new Set(inp.markers)].slice(0, MAX_MARKERS) : ['Marker']
  const afs = [...new Set(inp.afs)].filter(c => !markers.includes(c)).slice(0, MAX_AF)
  const competing = [...new Set(inp.competing)]
    .filter(c => !markers.includes(c) && !afs.includes(c)).slice(0, MAX_COMPETING)
  return { markers, afs, competing, exclusive: inp.exclusive }
}

export interface AfRolesFigure {
  vis: VisColumns
  headings: string[]
  note: string
}

export function afRolesFigure(input: AfRolesInput): AfRolesFigure {
  const inp = figureChannels(input)
  const scene = afScene(inp)
  const outs = correctAll(scene)
  const inputs = [...scene.markers, ...scene.afs, ...scene.competing]
  // ONE peak for every column, or the dimming the figure exists to show gets rescaled away
  const frames = normTo01([...inputs.map(sub), ...outs])

  const roles = [
    ...inp.markers.map(() => 'marker'), ...inp.afs.map(() => 'AF only'),
    ...inp.competing.map(() => 'competing'), ...inp.markers.map(() => 'output'),
  ]
  const nIn = inputs.length
  const leftOnAf = outs.map((o, i) => keptPct(o, scene.markers[i], scene.granule))
  const cellsKept = outs.map((o, i) => keptPct(o, scene.markers[i], scene.cells[i]))
  const blank = Array.from({ length: nIn }, () => cell(''))
  const rows: VisRow[] = [
    { key: 'result', label: 'Simulated', role: 'grid', uniform: false,
      cells: frames.map(f => cell('', [f])) },
    { key: 'role', label: 'Role', role: 'text', uniform: false, cells: roles.map(r => cell(r)) },
    { key: 'keptAf', label: 'Marker left on AF', role: 'text', uniform: false,
      cells: [...blank, ...leftOnAf.map(p => cell(`${p}%`))] },
    { key: 'keptCell', label: 'Own cell kept', role: 'text', uniform: false,
      cells: [...blank, ...cellsKept.map(p => cell(`${p}%`))] },
  ]
  const headings = [
    ...inp.markers, ...inp.afs, ...inp.competing, ...inp.markers.map(m => `${m} corrected`),
  ]
  const columns = headings.map((_, i) => `c${i}`)

  const alt = scene.afs.length
    ? keptPct(correctAll(scene, true)[0], scene.markers[0], scene.granule) : null
  const note = afRolesVerdict(leftOnAf[0], alt)
  return { vis: { columns, rows, pxSize: null, uniformKeys: [] }, headings, note }
}

/** One line, read off the numbers the figure draws. `alt` = the same AF listed as competing. */
export function afRolesVerdict(leftOnAf: number, alt: number | null): string {
  if (alt === null) return 'No AF channel: AF stays wherever a marker outshines its competitors'
  return `AF channels leave ${leftOnAf}% of the marker on AF; listed as competing, ${alt}% would stay`
}
