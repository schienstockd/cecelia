import { describe, it, expect } from 'vitest'
import { qcState, qcSummary, qcFindings, isMetadataCode, groupByTask, qcTooltipHtml } from './qc'
import type { CciaImage } from '../stores/project'

// Only the QC-relevant fields matter here; the rest of CciaImage is irrelevant to these readers.
const img = (qc?: CciaImage['qc']): CciaImage =>
  ({ uid: 'u1', name: 'img', qc } as unknown as CciaImage)

const finding = (level: 'info' | 'warn', code: string) =>
  ({ level, code, short: `${code} short`, long: `${code} long` })

describe('qcState — the image table slot', () => {
  // The whole reason this exists next to `qcSummary`: that one returns null for BOTH of these, so the
  // table could not tell "we checked and it is fine" from "nothing has ever looked at this".
  it('separates never-ran from ran-clean, which qcSummary cannot', () => {
    expect(qcState(img(undefined))).toBe('none')
    expect(qcState(img({}))).toBe('none')
    expect(qcState(img({ 'segment.cellpose': { findings: [] } }))).toBe('clean')
    expect(qcSummary(img({ 'segment.cellpose': { findings: [] } }))).toBeNull()
    expect(qcSummary(img(undefined))).toBeNull()
  })

  it('reports the worst level across every sidecar', () => {
    expect(qcState(img({ a: { findings: [finding('info', 'drift.x')] } }))).toBe('info')
    expect(qcState(img({
      a: { findings: [finding('info', 'drift.x')] },
      b: { findings: [finding('warn', 'segment.tiny')] },
    }))).toBe('warn')
  })

  // Calibration findings have their own click-to-fix icon, so counting them here would show the same
  // problem twice — and, worse, would make a perfectly clean segmentation read as flagged.
  it('ignores calibration findings, which have their own affordance', () => {
    expect(isMetadataCode('metadata.no_physical_size')).toBe(true)
    const only = img({ a: { findings: [finding('warn', 'metadata.no_physical_size')] } })
    expect(qcState(only)).toBe('clean')       // a sidecar exists, and nothing it says belongs here
    expect(qcSummary(only)).toBeNull()
  })

  it('is clean when a sidecar carries no findings key at all', () => {
    expect(qcState(img({ a: {} }))).toBe('clean')
  })

  it('agrees with qcFindings about what it counted', () => {
    const two = img({
      a: { findings: [finding('warn', 'x.a'), finding('info', 'metadata.b')] },
    })
    expect(qcFindings(two)).toHaveLength(2)   // raw count keeps both
    expect(qcState(two)).toBe('warn')         // …the slot only weighs the non-calibration one
    expect(qcSummary(two)!.count).toBe(1)
  })
})

// Provenance. The real case that motivated this: WIaUjL/p6t4mC carries one finding each from import,
// drift and AF, and the flat list rendered three problems with no way to tell which step raised them.
describe('finding provenance', () => {
  const p6t4mC = img({
    'importImages.omezarr/default':       { funName: 'importImages.omezarr',      findings: [finding('warn', 'import.channel_saturated')] },
    'cleanupImages.driftCorrect/drifted': { funName: 'cleanupImages.driftCorrect', findings: [finding('warn', 'drift.unregistered_frames')] },
    'cleanupImages.afCorrect/afCorr':     { funName: 'cleanupImages.afCorrect',    findings: [finding('warn', 'af.bleedthrough')] },
    'cleanupImages.smooth/smoothed':      { funName: 'cleanupImages.smooth',       findings: [] },
  })

  it('tags every finding with the task that raised it', () => {
    expect(qcFindings(p6t4mC).map(f => f.funName)).toEqual([
      'importImages.omezarr', 'cleanupImages.driftCorrect', 'cleanupImages.afCorrect',
    ])
  })

  // A sidecar banked before qc.jl wrote `funName` into the doc still has it in the key, which is the
  // same string — so an old image is badged rather than silently unattributed.
  it('falls back to the sidecar key when the doc carries no funName', () => {
    const old = img({ 'segment.cellpose/base': { findings: [finding('warn', 'segment.no_cells')] } })
    expect(qcFindings(old)[0].funName).toBe('segment.cellpose')
  })

  it('groups per task in first-seen order, badging a task once for several findings', () => {
    const twoFromOne = qcFindings(img({
      'importImages.omezarr/default': {
        funName: 'importImages.omezarr',
        findings: [finding('warn', 'import.channel_saturated'), finding('warn', 'import.other')],
      },
      'cleanupImages.afCorrect/a': { funName: 'cleanupImages.afCorrect', findings: [finding('warn', 'af.bleedthrough')] },
    }))
    const gs = groupByTask(twoFromOne)
    expect(gs.map(g => [g.funName, g.findings.length])).toEqual([
      ['importImages.omezarr', 2], ['cleanupImages.afCorrect', 1],
    ])
    // one tag for the two-finding task, not two
    const html = qcTooltipHtml(gs, fn => fn, () => ({}))
    expect(html.match(/cc-module-tag"/g)).toHaveLength(2)
  })
})

describe('qcTooltipHtml', () => {
  const label = (fn: string) => ({ 'cleanupImages.afCorrect': 'AF correction' }[fn] ?? fn)

  it('tags each group with the shared module pill and pairs problem with action', () => {
    const html = qcTooltipHtml(
      groupByTask(qcFindings(img({ 'cleanupImages.afCorrect/a': {
        funName: 'cleanupImages.afCorrect', findings: [finding('warn', 'af.bleedthrough')] } }))),
      label, () => ({ background: '#06564622', color: '#065646', borderColor: '#06564655' }))
    // the SHARED pill (style.css .cc-module-tag), not a private one — same visual as the task
    // manager's row pill and the image table's run tag, tinted from the one MODULE_COLORS palette
    expect(html).toContain('class="cc-module-tag" style="background:#06564622;color:#065646;border-color:#06564655"')
    // the label sits DIRECTLY in the tag — no `-fun` part, see the dimming test below
    expect(html).toContain('border-color:#06564655">AF correction</span>')
    expect(html).toContain('af.bleedthrough short')
    expect(html).toContain('<div class="qcf-a">af.bleedthrough long</div>')
  })

  // The whole point of escaping: this is rendered with PrimeVue `escape: false`, so a finding whose
  // text ever interpolates a filename or channel name must not be able to inject markup.
  it('escapes every interpolated string, label included', () => {
    const html = qcTooltipHtml(
      [{ funName: 'x', findings: [{ level: 'warn', code: 'x', short: '<img src=x>', long: 'a & b' }] }],
      () => '<b>t</b>', () => ({ background: 'javascript:alert(1)' }))
    expect(html).toContain('&lt;img src=x&gt;')
    expect(html).toContain('a &amp; b')
    expect(html).toContain('&lt;b&gt;t&lt;/b&gt;')
    expect(html).not.toContain('<img')
    expect(html).not.toContain('<b>')
    // and the style attribute takes hex only, so a non-colour can never reach it
    expect(html).not.toContain('javascript:')
    expect(html).toContain('style=""')
  })

  it('is empty for no groups, so the tooltip falls back to the plain string', () => {
    expect(qcTooltipHtml([], fn => fn, () => ({}))).toBe('')
  })

  // The bug this closes, caught by Dominik looking at a mock: the label first shipped wearing
  // `.cc-module-tag-fun`, whose `opacity: .85` means "secondary to the bold module id". Here the label
  // is the only thing in the pill, and the dimming took it from 4.55:1 to 3.70:1 — back under AA,
  // undoing the lift `moduleTagStyle` exists to apply, while still looking plausible.
  //
  // Asserted against the REAL stylesheet rather than a hardcoded class list, so adding an opacity to a
  // `.cc-module-tag*` part fails here instead of quietly dimming this label.
  it('wears no pill part that style.css dims', async () => {
    const css = (await import('../style.css?raw')).default as string
    const dimmed = [...css.matchAll(/(\.cc-module-tag[\w-]*)\s*\{([^}]*)\}/g)]
      .filter(m => {
        const o = /opacity:\s*([\d.]+)/.exec(m[2])
        return !!o && parseFloat(o[1]) < 1
      })
      .map(m => m[1].slice(1))
    expect(dimmed, 'the detector found no dimmed part — has the CSS moved?').toContain('cc-module-tag-fun')

    const html = qcTooltipHtml(
      [{ funName: 'x', findings: [{ level: 'warn', code: 'c', short: 's', long: 'l' }] }],
      () => 'AF correction', () => ({ color: '#659c8c' }))
    expect(html).toContain('class="cc-module-tag"')
    for (const cls of dimmed) expect(html, cls).not.toContain(cls)
  })
})
