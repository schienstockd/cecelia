import { describe, it, expect } from 'vitest'
import { DEFAULT_COLOUR_SCHEME, applyColourScheme, parseColourScheme, nextColourScheme, colourSchemeLabel, COLOUR_SCHEMES } from './colourScheme'

describe('colour scheme', () => {
  it('reads anything unknown as the default, light', () => {
    expect(DEFAULT_COLOUR_SCHEME).toBe('light')
    expect(parseColourScheme('light')).toBe('light')
    expect(parseColourScheme('dark')).toBe('dark')
    expect(parseColourScheme(null)).toBe('light')
    expect(parseColourScheme('sepia')).toBe('light')
  })

  it('sets data-theme for light and removes it for dark', () => {
    const root = { dataset: {} as DOMStringMap }
    applyColourScheme('light', root)
    expect(root.dataset.theme).toBe('light')
    applyColourScheme('dark', root)
    expect(root.dataset.theme).toBeUndefined()
  })

  it('switches to the other scheme, labelled by where it goes', () => {
    expect(nextColourScheme('dark')).toBe('light')
    expect(nextColourScheme('light')).toBe('dark')
    expect(colourSchemeLabel(nextColourScheme('dark'))).toBe('Light colours')
  })

  // Every non-default scheme needs its token block, or picking it changes nothing.
  it('has a style.css block for every non-dark scheme', () => {
    const css = Object.values(import.meta.glob('/src/style.css', { query: '?raw', import: 'default', eager: true }))[0] as string
    for (const s of COLOUR_SCHEMES.filter(s => s.value !== 'dark')) {
      expect(css, s.value).toContain(`:root[data-theme="${s.value}"] {`)
    }
  })
})
