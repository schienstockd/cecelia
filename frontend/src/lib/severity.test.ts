import { describe, it, expect } from 'vitest'
import { SEVERITY, worstSeverity, severityFor, type Severity } from './severity'

describe('severity model', () => {
  it('every level ships a shape-distinct icon + emoji + label (colour never alone)', () => {
    const icons = new Set<string>()
    const emojis = new Set<string>()
    for (const k of ['ok', 'warn', 'fail'] as Severity[]) {
      expect(SEVERITY[k].icon).toMatch(/^pi-/)
      expect(SEVERITY[k].label.length).toBeGreaterThan(0)
      icons.add(SEVERITY[k].icon); emojis.add(SEVERITY[k].emoji)
    }
    expect(icons.size).toBe(3)     // distinct shapes, not one shape × three hues
    expect(emojis.size).toBe(3)
    // the classic trap: same-shape circles must NOT be used
    for (const k of ['ok', 'warn', 'fail'] as Severity[])
      expect(['🟢', '🟡', '🔴']).not.toContain(SEVERITY[k].emoji)
  })

  it('worstSeverity reduces to the most severe', () => {
    expect(worstSeverity([])).toBe('ok')
    expect(worstSeverity(['ok', 'ok'])).toBe('ok')
    expect(worstSeverity(['ok', 'warn'])).toBe('warn')
    expect(worstSeverity(['warn', 'fail', 'ok'])).toBe('fail')
  })

  it('severityFor: failure→fail, any warn→warn, info/none→ok', () => {
    expect(severityFor({ failed: true, levels: ['info'] })).toBe('fail')   // failure wins
    expect(severityFor({ levels: ['info', 'warn'] })).toBe('warn')
    expect(severityFor({ levels: ['info', 'info'] })).toBe('ok')            // info never raises
    expect(severityFor({})).toBe('ok')
  })
})
