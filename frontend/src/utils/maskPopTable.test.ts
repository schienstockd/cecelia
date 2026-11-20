import { describe, it, expect } from 'vitest'
import { effectiveMaskColour,
  maskPopTable, trackclustMaskLayer, colourByMaskLayer, maskColourKeys, maskLayerShowsAny, isMaskColour, LABEL_TABLE_W, LABEL_TABLE_MAX_ROWS, MASK_PARENT_PALE, MASK_UNGATED_GREY,
  type LabelTable, type MaskPopLayer,
} from './maskPopTable'
import { NO_VALUE_HEX, colourByValue, type OverlayPayload } from './viewerOverlays'
import { mix, parseHex } from './colour'

// The shader is the ground truth for where an id's colour lives, so the reader below is checked against
// the WGSL source itself rather than re-derived from the builder.
const WGSL = Object.values(import.meta.glob('/src/lib/webgpu/shaders/label_colour.wgsl',
  { query: '?raw', import: 'default', eager: true }) as Record<string, string>)[0] ?? ''

/** `labColour` on a table (`rows < 0`): texel `(id % w, id / w)` of a texture written row-major at
 *  `bytesPerRow = w * 4`, and transparent past the last row. */
function shaderRead(t: LabelTable, id: number): [number, number, number, number] {
  const w = t.width, row = Math.floor(id / w)
  if (row >= t.rows) return [0, 0, 0, 0]
  const o = (row * w + (id % w)) * 4
  return [t.bytes[o], t.bytes[o + 1], t.bytes[o + 2], t.bytes[o + 3]]
}

const rgb255 = (hex: string) => parseHex(hex)
const pale255 = (hex: string) => parseHex(mix(hex, '#ffffff', MASK_PARENT_PALE))
const colourOf = (t: LabelTable, id: number) => shaderRead(t, id).slice(0, 3)

const layer = (cells: number[], pops: { path: string; colour: string; labels: number[] }[],
               hidden: string[] = []): MaskPopLayer => ({ payload: { cells: { label: cells }, pops },
                                                          hidden: new Set(hidden) })

describe('the shader formula this reader stands in for', () => {
  it('found the WGSL', () => { expect(WGSL).toContain('fn labColour') })
  it('addresses (id % W, id / W) and clears past the last row', () => {
    expect(WGSL).toMatch(/let w = u32\(-rows\);/)
    expect(WGSL).toMatch(/let row = id \/ w;/)
    expect(WGSL).toMatch(/if \(row >= textureDimensions\(pal\)\.y\) \{ return vec4\(0\.0\); \}/)
    expect(WGSL).toMatch(/textureLoad\(pal, vec2<i32>\(i32\(id % w\), i32\(row\)\), 0\)/)
  })
  it('hides a label by alpha 0', () => { expect(WGSL).toMatch(/labColour\(id\)\.a > 0\.0/) })
})

describe('maskPopTable', () => {
  const RED = '#ff0000', BLUE = '#0000ff', GREEN = '#00ff00'

  it('is null with no cells — the caller keeps the palette', () => {
    expect(maskPopTable([])).toBeNull()
    expect(maskPopTable([layer([], [])])).toBeNull()
  })

  it('is LABEL_TABLE_W wide, the width the movie host uses too', () => {
    const t = maskPopTable([layer([1, 2, 3], [])])!
    expect(t.width).toBe(LABEL_TABLE_W)
    expect(LABEL_TABLE_W).toBe(4096)
    expect(t.bytes.length).toBe(t.width * t.rows * 4)
  })

  it('draws cells in no pop grey, and ids outside the cell table not at all', () => {
    const t = maskPopTable([layer([1, 5], [{ path: '/A', colour: RED, labels: [1] }])])!
    expect(colourOf(t, 5)).toEqual(rgb255(MASK_UNGATED_GREY))
    expect(shaderRead(t, 5)[3]).toBe(255)
    expect(shaderRead(t, 3)[3]).toBe(0)       // in range, not a cell
    expect(shaderRead(t, 0)[3]).toBe(0)       // background
  })

  it('lets the deepest pop win, whatever order the payload lists them in', () => {
    const pops = [
      { path: '/A/B/C', colour: GREEN, labels: [3] },
      { path: '/A', colour: RED, labels: [1, 2, 3] },
      { path: '/A/B', colour: BLUE, labels: [2, 3] },
    ]
    const t = maskPopTable([layer([1, 2, 3], pops)])!
    expect(colourOf(t, 3)).toEqual(rgb255(GREEN))
    expect(colourOf(t, 2)).toEqual(pale255(BLUE))   // /A/B is a parent of a shown pop
    expect(pale255(BLUE)).toEqual([153, 153, 255])
    expect(colourOf(t, 1)).toEqual(pale255(RED))
  })

  it('breaks a depth tie by payload order, later winning — as the movie does', () => {
    const t = maskPopTable([layer([1], [
      { path: '/A', colour: RED, labels: [1] }, { path: '/B', colour: BLUE, labels: [1] },
    ])])!
    expect(colourOf(t, 1)).toEqual(rgb255(BLUE))
  })

  it('draws a parent solid once its only child is hidden, and a hidden pop not at all', () => {
    const pops = [{ path: '/A', colour: RED, labels: [1, 2] }, { path: '/A/B', colour: BLUE, labels: [2] }]
    const t = maskPopTable([layer([1, 2], pops, ['/A/B'])])!
    expect(colourOf(t, 1)).toEqual(rgb255(RED))
    expect(colourOf(t, 2)).toEqual(rgb255(RED))
    const none = maskPopTable([layer([1, 2], pops, ['/A', '/A/B'])])!
    expect(colourOf(none, 2)).toEqual(rgb255(MASK_UNGATED_GREY))
  })

  it('does not take a sibling sharing a name prefix for a child', () => {
    const t = maskPopTable([layer([1, 2], [
      { path: '/A', colour: RED, labels: [1] }, { path: '/AB', colour: BLUE, labels: [2] },
    ])])!
    expect(colourOf(t, 1)).toEqual(rgb255(RED))
  })

  it('keeps pale within a pop type — another layer\'s /A/B is not a child', () => {
    const t = maskPopTable([
      layer([1, 2], [{ path: '/A', colour: RED, labels: [1] }]),
      layer([1, 2], [{ path: '/A/B', colour: BLUE, labels: [2] }]),
    ])!
    expect(colourOf(t, 1)).toEqual(rgb255(RED))
    expect(colourOf(t, 2)).toEqual(rgb255(BLUE))
  })

  it('places ids past the first row where the shader reads them', () => {
    const big = 3 * LABEL_TABLE_W + 17
    const t = maskPopTable([layer([1, big], [{ path: '/A', colour: BLUE, labels: [big] }])])!
    expect(t.rows).toBe(4)
    expect(colourOf(t, big)).toEqual(rgb255(BLUE))
    expect(shaderRead(t, big - 1)[3]).toBe(0)
    expect(shaderRead(t, big + LABEL_TABLE_W)[3]).toBe(0)   // past the last row
    expect(t.truncated).toBe(false)
  })

  it('caps the rows and says so — a huge id costs a bounded table', () => {
    const huge = 2 ** 31
    const t = maskPopTable([layer([1, huge], [{ path: '/A', colour: RED, labels: [1, huge] }])], 16, 4)!
    expect(t.rows).toBe(4)
    expect(t.bytes.length).toBe(16 * 4 * 4)
    expect(t.truncated).toBe(true)
    expect(colourOf(t, 1)).toEqual(rgb255(RED))
    expect(shaderRead(t, huge)[3]).toBe(0)
    expect(LABEL_TABLE_MAX_ROWS).toBe(8192)   // WebGPU's guaranteed maxTextureDimension2D
  })
})

// The movie draws the same rule in Julia (`mask_pop_colours`, `api/src/overlay_author.jl`); both sides
// are held to this one fixture, so neither can drift alone.
describe('trackclustMaskLayer', () => {
  const RED = '#ff0000', BLUE = '#0000ff'
  it('colours each tracked cell by its track cluster, hiding the pops the manager hides', () => {
    const l = trackclustMaskLayer({ cells: { label: [1, 2, 3] }, pops: [
      { path: '/c1', colour: RED, labels: [1], show: true },
      { path: '/c2', colour: BLUE, labels: [2], show: false },
    ] })
    expect([...l.hidden!]).toEqual(['/c2'])
    const t = maskPopTable([l])!
    expect(colourOf(t, 1)).toEqual(rgb255(RED))
    expect(colourOf(t, 2)).toEqual(rgb255(MASK_UNGATED_GREY))   // its cluster is hidden
    expect(colourOf(t, 3)).toEqual(rgb255(MASK_UNGATED_GREY))   // untracked
  })
  it('hides by the eye when given one — the viewer draws track clusters by their eyes, not show', () => {
    const pops = [{ path: '/c1', colour: RED, labels: [1], show: true },
                  { path: '/c2', colour: BLUE, labels: [2], show: false }]
    const eyes: Record<string, boolean> = { '/c1': false, '/c2': true }
    const l = trackclustMaskLayer({ cells: { label: [1, 2] }, pops }, (path, show) => eyes[path] ?? show)
    expect([...l.hidden!]).toEqual(['/c1'])
    const t = maskPopTable([l])!
    expect(colourOf(t, 1)).toEqual(rgb255(MASK_UNGATED_GREY))
    expect(colourOf(t, 2)).toEqual(rgb255(BLUE))
  })
  it('maskLayerShowsAny reads the eyes the layer draws by, not show', () => {
    const pops = [{ path: '/c1', colour: RED, labels: [1], show: true }]
    const off = trackclustMaskLayer({ cells: { label: [1] }, pops }, () => false)
    expect(maskLayerShowsAny(off)).toBe(false)              // every eye off: the mask is all grey
    const hidden = [{ path: '/c1', colour: RED, labels: [1], show: false }]
    expect(maskLayerShowsAny(trackclustMaskLayer({ cells: { label: [1] }, pops: hidden }, () => true))).toBe(true)
    expect(maskLayerShowsAny(trackclustMaskLayer({ cells: { label: [1] }, pops: [] }))).toBe(false)
  })
  it('with no track clusters draws every cell grey, as the movie does', () => {
    const t = maskPopTable([trackclustMaskLayer({ cells: { label: [1, 2] }, pops: [] })])!
    expect(colourOf(t, 2)).toEqual(rgb255(MASK_UNGATED_GREY))
  })
})

describe('track clusters with a colour-by (`trackclustMaskLayer` + values) — clusters select, the column shades', () => {
  // A lens score sets the colour-by; on *Colour: Track clusters* the mask ignored it, so only the dots
  // took the scores. The movie's population mask does the same with `colour_by` (`mask_id_colours`:
  // the pops' cells by value, the rest "no value").
  const RED = '#ff0000', BLUE = '#0000ff'
  const tc = (values: (number | null)[] | null): OverlayPayload => ({
    nCells: 4, nDropped: 0, axes: [], hasT: false, cells: { label: [1, 2, 3, 4] },
    pops: [{ path: '/c1', name: 'c1', colour: RED, labels: [1, 2], show: true },
           { path: '/c2', name: 'c2', colour: BLUE, labels: [3], show: false }],
    colourColumns: ['s'], colourBy: values ? 's' : null, valueKind: 'numeric', valueRange: [0, 5],
    values: values ?? undefined,
  } as unknown as OverlayPayload)
  it('a shown cluster\'s cells by the column; a hidden cluster, an untracked cell or no value draw dim', () => {
    const p = tc([0, 5, 5, null])
    const t = maskPopTable([trackclustMaskLayer(p, undefined, ['#ffffff'])])!
    const byValue = colourByValue(p)!
    expect(colourOf(t, 1)).toEqual(byValue(0).map(v => Math.round(v * 255)))
    expect(colourOf(t, 2)).toEqual(byValue(1).map(v => Math.round(v * 255)))
    expect(colourOf(t, 3)).toEqual(rgb255(NO_VALUE_HEX))   // its cluster is hidden
    expect(colourOf(t, 4)).toEqual(rgb255(NO_VALUE_HEX))   // untracked, no value
  })
  it('no colour-by: the cluster colours, as before', () => {
    const t = maskPopTable([trackclustMaskLayer(tc(null), undefined, ['#ffffff'])])!
    expect(colourOf(t, 1)).toEqual(rgb255(RED))
    expect(colourOf(t, 4)).toEqual(rgb255(MASK_UNGATED_GREY))
  })
})

describe('isMaskColour', () => {
  it('accepts the three sources and nothing else', () => {
    expect(['id', 'pops', 'trackclust'].every(isMaskColour)).toBe(true)
    expect(isMaskColour('true')).toBe(false)
    expect(isMaskColour(null)).toBe(false)
  })
})

import PARITY_JSON from './maskPopTable.parity.json'

interface ParityCase {
  name: string
  layers: { cells: number[]; pops: { path: string; colour: string; labels: number[] }[]; hidden?: string[] }[]
  expected: Record<string, string>
}
const PARITY = PARITY_JSON as unknown as { cases: ParityCase[] }

describe('maskPopTable ↔ mask_pop_colours parity fixture', () => {
  for (const c of PARITY.cases) {
    it(c.name, () => {
      const t = maskPopTable(c.layers.map(l => layer(l.cells, l.pops, l.hidden ?? [])))!
      const expected = c.expected
      const maxId = Math.max(...Object.keys(expected).map(Number)) + 2
      for (let id = 0; id <= maxId; id++) {
        const hex = expected[String(id)]
        if (hex) {
          expect(shaderRead(t, id)[3], `id ${id} drawn`).toBe(255)
          expect(colourOf(t, id), `id ${id}`).toEqual(rgb255(hex))
        } else {
          expect(shaderRead(t, id)[3], `id ${id} not drawn`).toBe(0)
        }
      }
    })
  }
})

describe('the mask on Colour by (`colourByMaskLayer`) — every cell by the column, as the movie (`mask_id_colours`)', () => {
  const payload = (values: (number | string | null)[], kind: 'categorical' | 'numeric', extra = {}) => ({
    nCells: values.length, nDropped: 0, axes: [], hasT: false,
    cells: { label: values.map((_, i) => i + 1) }, pops: [], colourColumns: ['c'], colourBy: 'c',
    valueKind: kind, values, ...extra })
  it('a category its palette slot by sorted level; a cell with no value draws dim (no data)', () => {
    const p = payload(['b', 'a', null, 'a'], 'categorical', { valueLevels: ['b', 'a'] })
    const t = maskPopTable([colourByMaskLayer(p, ['#ff0000', '#00ff00'])])!
    // levels sort to [a, b]: a → slot 0 (red), b → slot 1 (green)
    expect(colourOf(t, 2)).toEqual(rgb255('#ff0000'))
    expect(colourOf(t, 4)).toEqual(rgb255('#ff0000'))
    expect(colourOf(t, 1)).toEqual(rgb255('#00ff00'))
    expect(colourOf(t, 3)).toEqual(rgb255(NO_VALUE_HEX))
  })
  it('a number on the ramp; NaN draws dim; no column = no pops, so the caller keeps the palette', () => {
    const p = payload([0, 10, Number.NaN], 'numeric', { valueRange: [0, 10] })
    const t = maskPopTable([colourByMaskLayer(p)])!
    expect(colourOf(t, 1)).not.toEqual(colourOf(t, 2))
    expect(colourOf(t, 3)).toEqual(rgb255(NO_VALUE_HEX))
    const none = maskPopTable([colourByMaskLayer({ ...p, colourBy: null })])!
    expect(colourByMaskLayer({ ...p, colourBy: null }).payload.pops).toEqual([])
    expect(colourOf(none, 3)).toEqual(rgb255(MASK_UNGATED_GREY))   // no column: nothing to call "no value"
  })
  it('the movie keys of each choice — one translation for the viewer look and a batch', () => {
    expect(maskColourKeys('id', true, 'c')).toEqual({ maskAllCells: true, maskPopTypes: [], colourLabels: false })
    expect(maskColourKeys('pops', true, '')).toEqual({ maskAllCells: false, maskPopTypes: [], colourLabels: false })
    expect(maskColourKeys('pops', false, '')).toEqual({ maskAllCells: true, maskPopTypes: [], colourLabels: false })
    expect(maskColourKeys('trackclust', false, '')).toEqual({ maskAllCells: false, maskPopTypes: ['trackclust'], colourLabels: false })
    // track clusters with a colour-by: the clusters select, the column shades (`mask_id_colours`' gated)
    expect(maskColourKeys('trackclust', false, 'c')).toEqual({ maskAllCells: false, maskPopTypes: ['trackclust'], colourLabels: true })
    // Pops keeps its pop colours whatever the colour-by — only the points take it
    expect(maskColourKeys('pops', true, 'c')).toEqual({ maskAllCells: false, maskPopTypes: [], colourLabels: false })
    expect(maskColourKeys('colourBy', false, 'c')).toEqual({ maskAllCells: true, maskPopTypes: [], colourLabels: true })
    expect(maskColourKeys('colourBy', false, '')).toEqual({ maskAllCells: true, maskPopTypes: [], colourLabels: false })
    expect(isMaskColour('colourBy')).toBe(true)
  })
})

describe('effectiveMaskColour', () => {
  it('a stored Track clusters draws Pops on a still, as stored on a time-lapse; others untouched', () => {
    expect(effectiveMaskColour('trackclust', false)).toBe('pops')
    expect(effectiveMaskColour('trackclust', true)).toBe('trackclust')
    expect(effectiveMaskColour('colourBy', false)).toBe('colourBy')
  })
})
