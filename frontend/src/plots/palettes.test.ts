// VIEWER_PARITY phases 1 + 2: the palettes.json this test imports IS the source of truth for the
// browser look AND the Julia offline renderer (`api/src/overlay_author.jl` reads the same file).
// This test pins that the browser wrappers (`PALETTES` in plot.ts, `BLUE_HEAT_ANCHORS` in
// flowColors.ts, `TRACK_COLOR_MODES` in viewerOverlays.ts) match the JSON — a mirror on the Julia
// side lives in `api/test/runtests.jl`.
// See docs/todo/VIEWER_PARITY_PLAN.md.
import { describe, it, expect } from 'vitest'
import palettesJson from './palettes.json'
import { PALETTES } from './plot'
import { BLUE_HEAT_ANCHORS } from './flowColors'
import { TRACK_COLOR_MODES } from '../utils/viewerOverlays'

describe('palettes.json parity wrappers', () => {
  it('PALETTES equals palettesJson.palettes', () => {
    expect(PALETTES).toStrictEqual(palettesJson.palettes)
  })

  it('cecelia is 12 colours, all valid hex', () => {
    expect(PALETTES.cecelia).toHaveLength(12)
    for (const c of PALETTES.cecelia) expect(c).toMatch(/^#[0-9a-fA-F]{6}$/)
  })

  it('BLUE_HEAT_ANCHORS is the JSON heatRamp — five stops', () => {
    expect(BLUE_HEAT_ANCHORS).toStrictEqual(palettesJson.heatRamp)
    expect(BLUE_HEAT_ANCHORS).toHaveLength(5)
  })

  it('TRACK_COLOR_MODES is the JSON trackColorModes — exact three names, in order', () => {
    expect(TRACK_COLOR_MODES).toStrictEqual(palettesJson.trackColorModes)
    expect(TRACK_COLOR_MODES).toStrictEqual(['track', 'speed', 'solid'])
  })
})
