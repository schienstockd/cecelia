import { describe, it, expect } from 'vitest'
import {
  maskPopTable, LABEL_TABLE_W, LABEL_TABLE_MAX_ROWS, MASK_PARENT_PALE, MASK_UNGATED_GREY,
  type LabelTable, type MaskPopLayer,
} from './maskPopTable'
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
