import { describe, it, expect } from 'vitest'
import { buildTurnSave, buildClaimSave, snapshotForRef } from './kiwiTurnSave'
import type { KiwiTurn, KiwiClaim, KiwiClaimRef } from './kiwiTurn'
import type { KiwiRef } from './kiwiRef'

const NOW = new Date('2026-09-25T12:00:00Z')

function claim(kind: KiwiClaim['kind'], text: string, refs: KiwiClaimRef[] = []): KiwiClaim {
  return { kind, text, refs }
}
function rr(ref: KiwiRef): KiwiClaimRef {
  return { ref, result: { ok: true, check: 'exists', label: 'x', error: '' }, seen: true }
}
function turn(over: Partial<KiwiTurn>): KiwiTurn {
  return {
    turnId: 't-1', projectUid: 'P', prompt: 'why the drift?', refs: [], reasoning: false,
    model: 'sonnet', engine: 'claude', status: 'done', startedAt: '2026-09-25T11:59:00Z',
    steps: [], reply: { ok: true, abstain: false, claims: [], errors: [], reasked: false,
      reasoning: '', usage: { input: 0, output: 0 }, toolCalls: 0, seconds: 1 },
    ...over,
  }
}

const cb = {
  imageName: (u: string) => u === 'IMG1' ? 'img_005' : u,
  plotSummary: (id: string) => id === 'p1' ? 'chart: boxplot\nseries…\nN=7' : '',
}

describe('snapshotForRef', () => {
  it('carries a sidecar for the six fragile kinds', () => {
    const savedAt = '2026-09-25T12:00:00.000Z'
    const kinds: KiwiRef[] = [
      { kind: 'population', imageUid: 'IMG1', valueName: 'default', popPath: '/foo' },
      { kind: 'cells',      imageUid: 'IMG1', valueName: 'default', labelIds: [1, 2, 3] },
      { kind: 'tracks',     imageUid: 'IMG1', valueName: 'default', trackIds: [7] },
      { kind: 'plot',       plotId: 'p1' },
      { kind: 'tile',       imageUid: 'IMG1', valueName: 'default', cellId: 'c42' },
      { kind: 'ui',         anchor: 'nav:analysis' },
    ]
    for (const r of kinds) {
      const s = snapshotForRef(r, cb, savedAt)
      expect(s, `${r.kind} should sidecar`).not.toBeNull()
      expect(s!.snapshot?.kind).toBe(r.kind)
      expect(s!.savedAt).toBe(savedAt)
    }
  })
  it('returns null for the six stable kinds', () => {
    const stable: KiwiRef[] = [
      { kind: 'project' },
      { kind: 'set', setUid: 'S' },
      { kind: 'image', imageUid: 'IMG1' },
      { kind: 'viewer', imageUid: 'IMG1', t: 3 },
      { kind: 'task', funName: 'x' },
      { kind: 'blackboard', entryId: 'bb-1' },
      { kind: 'capture', captureId: 'cap-1' },
      { kind: 'proposedPlot', plot: 'x' },
    ]
    for (const r of stable) {
      expect(snapshotForRef(r, cb, '2026-09-25T12:00:00.000Z'),
        `${r.kind} must not sidecar`).toBeNull()
    }
  })
  it('freezes the plot summary text verbatim', () => {
    const s = snapshotForRef({ kind: 'plot', plotId: 'p1' }, cb, 'x')
    expect(s!.snapshot).toMatchObject({ kind: 'plot', plotSummary: 'chart: boxplot\nseries…\nN=7' })
  })
})

describe('buildTurnSave', () => {
  it('builds markdown + sidecar + attachments for a whole turn', () => {
    const plot: KiwiRef = { kind: 'plot', plotId: 'p1' }
    const image: KiwiRef = { kind: 'image', imageUid: 'IMG1' }
    const cap: KiwiRef = { kind: 'capture', captureId: 'cap-1' }
    const t = turn({
      refs: [{ ref: image, result: { ok: true, check: 'exists', label: 'img', error: '' } }],
      reply: { ok: true, abstain: false, claims: [
        claim('observation', 'the drift shrinks after frame 40', [rr(plot), rr(cap)]),
        claim('interpretation', 'flow register is compensating rigid motion'),
      ], errors: [], reasked: false, reasoning: '', usage: { input: 0, output: 0 }, toolCalls: 0, seconds: 1 },
    })
    const out = buildTurnSave(t, cb, NOW)
    expect(out.title).toBe('why the drift?')
    // Markdown carries the claim kinds, the question, the attachments header, and the ref labels.
    expect(out.content).toContain('# Kiwi turn')
    expect(out.content).toContain('## Question\nwhy the drift?')
    expect(out.content).toContain('## Attachments')
    expect(out.content).toContain('**observation** — the drift shrinks')
    expect(out.content).toContain('**interpretation** — flow register')
    // The plot ref is fragile → sidecar + snapshot; the image + capture refs don't sidecar.
    const keys = Object.keys(out.kiwiRefs)
    expect(keys.length).toBe(1)
    expect(out.kiwiRefs[keys[0]].snapshot?.kind).toBe('plot')
    // Capture id lifted to attachments so the entry's chip strip surfaces it.
    expect(out.attachments).toEqual(['cap-1'])
  })
  it('escapes claim text that leads with a markdown-active character', () => {
    const t = turn({
      reply: { ok: true, abstain: false, claims: [
        claim('observation', '# this is not a header'),
      ], errors: [], reasked: false, reasoning: '', usage: { input: 0, output: 0 }, toolCalls: 0, seconds: 1 },
    })
    const md = buildTurnSave(t, cb, NOW).content
    expect(md).toContain('**observation** — \\# this is not a header')
  })
  it('renders an abstain-only turn as an abstain note under Claims', () => {
    const t = turn({
      reply: { ok: true, abstain: true, claims: [], errors: [], reasked: false, reasoning: '',
        usage: { input: 0, output: 0 }, toolCalls: 0, seconds: 1 },
    })
    expect(buildTurnSave(t, cb, NOW).content).toContain('Kiwi abstained')
  })
})

describe('buildClaimSave', () => {
  it('scopes the entry to one claim + only its refs', () => {
    const plotA: KiwiRef = { kind: 'plot', plotId: 'p1' }
    const plotB: KiwiRef = { kind: 'plot', plotId: 'p-other' }
    const t = turn({
      reply: { ok: true, abstain: false, claims: [
        claim('observation', 'kept', [rr(plotA)]),
        claim('interpretation', 'dropped', [rr(plotB)]),
      ], errors: [], reasked: false, reasoning: '', usage: { input: 0, output: 0 }, toolCalls: 0, seconds: 1 },
    })
    const kept = t.reply!.claims[0]
    const out = buildClaimSave(t, kept, cb, NOW)
    expect(out.content).toContain('kept')
    expect(out.content).not.toContain('dropped')
    // Only the kept claim's plot appears in the sidecar.
    const keys = Object.keys(out.kiwiRefs)
    expect(keys.length).toBe(1)
    expect(out.kiwiRefs[keys[0]].ref).toEqual(plotA)
  })
})
