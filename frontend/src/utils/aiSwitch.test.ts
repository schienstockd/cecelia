import { describe, it, expect } from 'vitest'
import { aiStateFromStatus, aiCanTurnOn, aiOffLine, showKiwiLauncher, guideGroupsFor, AI_OFF_COPY, AI_ONLY_GUIDES } from './aiSwitch'
import { isTooLong } from './uiCopy'

describe('aiSwitch', () => {
  it('reads the server state, and treats a backend without the switch as on', () => {
    expect(aiStateFromStatus({ ai: { enabled: false, setBy: 'workstation' } })).toEqual({ enabled: false, setBy: 'workstation' })
    expect(aiStateFromStatus({ ai: { enabled: true, setBy: null } })).toEqual({ enabled: true, setBy: null })
    expect(aiStateFromStatus({})).toEqual({ enabled: true, setBy: null })
    expect(aiStateFromStatus(null)).toEqual({ enabled: true, setBy: null })
    // off with an unknown reason: the person's own switch, the one they can change
    expect(aiStateFromStatus({ ai: { enabled: false, setBy: 'gremlin' } })).toEqual({ enabled: false, setBy: 'profile' })
  })

  it('only the profile layer can be turned on from here', () => {
    expect(aiCanTurnOn({ enabled: false, setBy: 'profile' })).toBe(true)
    expect(aiCanTurnOn({ enabled: false, setBy: 'workstation' })).toBe(false)
    expect(aiCanTurnOn({ enabled: true, setBy: null })).toBe(false)
    expect(aiOffLine({ enabled: false, setBy: 'workstation' })).toBe(AI_OFF_COPY.workstation)
  })

  it('the launcher hides only while off and hidden', () => {
    expect(showKiwiLauncher({ enabled: true, setBy: null }, true)).toBe(true)
    expect(showKiwiLauncher({ enabled: false, setBy: 'profile' }, false)).toBe(true)
    expect(showKiwiLauncher({ enabled: false, setBy: 'profile' }, true)).toBe(false)
  })

  it('copy stays short', () => {
    for (const s of Object.values(AI_OFF_COPY)) expect(isTooLong(s)).toBe(false)
  })
})

describe('guideGroupsFor', () => {
  const groups = [{ group: 'Analysis', guides: [{ id: 'assist-with-kiwi' }, { id: 'gating' }] },
                  { group: 'AI', guides: [{ id: 'assist-with-kiwi' }] }]
  it('drops AI-only guides and emptied groups only while AI is off', () => {
    expect(guideGroupsFor(groups, true)).toBe(groups)
    expect(guideGroupsFor(groups, false)).toEqual([{ group: 'Analysis', guides: [{ id: 'gating' }] }])
  })
})

describe('tipsFor', () => {
  it('drops the AI-only tips while AI is off, and they exist', async () => {
    const { TIPS, AI_ONLY_TIPS, tipsFor } = await import('../lib/tips')
    for (const id of AI_ONLY_TIPS) expect(TIPS.map(t => t.id)).toContain(id)
    expect(tipsFor(true)).toBe(TIPS)
    expect(tipsFor(false).map(t => t.id).some(id => AI_ONLY_TIPS.includes(id))).toBe(false)
    expect(tipsFor(false).length).toBe(TIPS.length - AI_ONLY_TIPS.length)
  })
})

describe('AI_ONLY_GUIDES', () => {
  it('names real guides', async () => {
    const { guideById } = await import('../lib/guides')
    for (const id of AI_ONLY_GUIDES) expect(guideById(id)).toBeTruthy()
  })
})
