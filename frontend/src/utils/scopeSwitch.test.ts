import { describe, expect, it } from 'vitest'
import { scopeCopy, scopeSetter, type PlotScope } from './scopeSwitch'

describe('scopeSetter', () => {
  it('global → local seeds every plot from the global values before flipping', () => {
    const scope = { value: 'global' as PlotScope }
    const g = { sel: ['a', 'b'], vis: { size: 3 } }
    const plots = [{ sel: [] as string[], vis: { size: 1 } }, { sel: ['x'], vis: { size: 9 } }]
    let seenScope: PlotScope | null = null
    const set = scopeSetter(scope, () => {
      seenScope = scope.value
      for (const p of plots) { p.sel = scopeCopy(g.sel); p.vis = scopeCopy(g.vis) }
    })
    set('local')
    expect(seenScope).toBe('global')
    expect(scope.value).toBe('local')
    expect(plots).toEqual([{ sel: ['a', 'b'], vis: { size: 3 } }, { sel: ['a', 'b'], vis: { size: 3 } }])
    // copies, not aliases: editing one plot leaves global and the other plot alone
    plots[0]!.sel.push('c'); plots[0]!.vis.size = 5
    expect(g).toEqual({ sel: ['a', 'b'], vis: { size: 3 } })
    expect(plots[1]).toEqual({ sel: ['a', 'b'], vis: { size: 3 } })
  })

  it('local → global and same-scope sets leave the local values alone', () => {
    let seeded = 0
    const scope = { value: 'local' as PlotScope }
    const set = scopeSetter(scope, () => { seeded++ })
    set('global'); expect(scope.value).toBe('global')
    set('global')
    expect(seeded).toBe(0)
  })

  it('scopeCopy passes undefined through', () => {
    expect(scopeCopy(undefined)).toBeUndefined()
  })
})
