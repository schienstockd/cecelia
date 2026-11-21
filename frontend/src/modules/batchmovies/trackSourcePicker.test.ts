import { describe, it, expect } from 'vitest'
import { PALETTES } from '../../plots/plot'
import { defaultTrackSourceColour } from '../../utils/batchMovie'

// A track source's default colour is the cecelia track palette by position (`defaultTrackSourceColour`,
// and the viewer / movie preview draw tracks from the same set). Its ColourPicker must offer that palette:
// ColourPicker's own default is the population palette, which neither contains the starting colour nor
// keeps a re-pick on the track set.
const SFC = import.meta.glob(
  ['/src/modules/batchmovies/BatchMoviesPanel.vue', '/src/components/viewerWindow/VwTracksSection.vue'],
  { query: '?raw', import: 'default', eager: true }) as Record<string, string>

/** Every `<ColourPicker …>` opening tag in a source that sets a track source's colour. */
function trackSourcePickers(src: string): string[] {
  return [...src.matchAll(/<ColourPicker\b[\s\S]*?\/>/g)].map(m => m[0])
    .filter(tag => /trackSourceColour|sourceColour/.test(tag))
}

describe('track-source colour pickers', () => {
  it('the default colour is one of the cecelia chips', () => {
    for (let i = 0; i < 20; i++) expect(PALETTES.cecelia).toContain(defaultTrackSourceColour(i))
  })

  it('every track-source picker offers the cecelia palette', () => {
    const files = Object.keys(SFC)
    expect(files).toHaveLength(2)
    for (const [path, src] of Object.entries(SFC)) {
      const tags = trackSourcePickers(src)
      expect(tags.length, path).toBeGreaterThan(0)
      for (const tag of tags) expect(tag, path).toMatch(/:palette="PALETTES\.cecelia"/)
    }
  })
})
