import { describe, it, expect } from 'vitest'
import { problemReportUrl, issueOsLabel, REPORT_URL_MAX, CECELIA_REPO_URL } from './links'

// The "Something wrong?" report (FEATURE_MATURITY_PLAN → P2): what it carries, and that task settings
// go to public GitHub only when the user ticked for them.

const field = (url: string, k: string) => new URL(url).searchParams.get(k)
const BASE = {
  feature: 'Phenotype', tier: 'Experimental', page: '/phenotype', version: 'v0.1.9',
  platform: 'linux-64', lastTask: 'segment.cellposeMeasure — failed',
}
const PARAMS = { modelPath: '/home/someone/secret-project/model', diameter: 12 }

describe('problemReportUrl', () => {
  it('opens the bug form with feature, tier, page, version, OS and last task', () => {
    const url = problemReportUrl(BASE)
    expect(url.startsWith(`${CECELIA_REPO_URL}/issues/new?`)).toBe(true)
    expect(field(url, 'template')).toBe('bug_report.yml')
    expect(field(url, 'title')).toBe('Phenotype: ')
    expect(field(url, 'version')).toBe('v0.1.9')
    expect(field(url, 'os')).toBe('Linux')
    const body = field(url, 'what-happened')!
    for (const s of ['Feature: Phenotype (Experimental)', 'Page: /phenotype', 'Cecelia: v0.1.9',
                     'OS: Linux (linux-64)', 'Last task: segment.cellposeMeasure — failed']) expect(body).toContain(s)
  })
  it('carries no task settings unless they are passed (the opt-in)', () => {
    for (const url of [problemReportUrl(BASE), problemReportUrl({ ...BASE, params: null })]) {
      expect(url).not.toContain('secret-project')
      expect(field(url, 'what-happened')).not.toContain('settings')
    }
    const opted = problemReportUrl({ ...BASE, params: PARAMS })
    expect(field(opted, 'what-happened')).toContain('/home/someone/secret-project/model')
  })
  it('leaves out what it does not know', () => {
    const body = field(problemReportUrl({ feature: 'Gating', tier: 'Beta' }), 'what-happened')!
    expect(body).toContain('Feature: Gating (Beta)')
    expect(body).not.toMatch(/Page:|Cecelia:|OS:|Last task:/)
    expect(field(problemReportUrl({ feature: 'Gating', tier: 'Beta' }), 'version')).toBeNull()
  })
  it('stays under GitHub’s URL limit: long settings are cut, then dropped', () => {
    const big = Object.fromEntries(Array.from({ length: 400 }, (_, i) => [`param_${i}`, `"value ${i}" & more`]))
    const url = problemReportUrl({ ...BASE, params: big })
    expect(url.length).toBeLessThanOrEqual(REPORT_URL_MAX)
    const body = field(url, 'what-happened')!
    expect(body.includes('… (cut)') || body.includes('Settings left out')).toBe(true)
    const mid = problemReportUrl({ ...BASE, params: Object.fromEntries(Array.from({ length: 60 }, (_, i) => [`p${i}`, i])) })
    expect(mid.length).toBeLessThanOrEqual(REPORT_URL_MAX)
    expect(field(mid, 'what-happened')).toContain('p59')
  })
})

describe('issueOsLabel', () => {
  it('maps pixi platforms to the bug form’s OS words', () => {
    expect(issueOsLabel('linux-64')).toBe('Linux')
    expect(issueOsLabel('osx-arm64')).toBe('macOS')
    expect(issueOsLabel('win-64')).toBe('Windows')
    expect(issueOsLabel('unknown')).toBe('')
    expect(issueOsLabel(undefined)).toBe('')
  })
})
