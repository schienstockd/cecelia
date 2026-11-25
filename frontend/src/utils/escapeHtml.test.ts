import { describe, expect, it } from 'vitest'
import { escapeHtml } from './escapeHtml'

describe('escapeHtml', () => {
  it('escapes the five markup characters and leaves the rest', () => {
    expect(escapeHtml(`<a href="x">Tom & Jerry's</a>`))
      .toBe('&lt;a href=&quot;x&quot;&gt;Tom &amp; Jerry&#39;s&lt;/a&gt;')
    expect(escapeHtml('CD8 µm — ok')).toBe('CD8 µm — ok')
  })

  it('escapes an ampersand once, not an already-escaped entity twice over', () => {
    expect(escapeHtml('&lt;')).toBe('&amp;lt;')
  })
})
