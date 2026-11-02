/**
 * The Call-for-Datasets registry and its URL builder — tested without mounting anything, the same
 * rule the vis-aid producers follow.
 *
 * The claims worth pinning are the ones a reader of the ISSUE would take away: the title says what
 * the ask is, the body fronts the two questions we cannot guess (what the imaging looks like and a
 * cloud link to a sample), and the labels are present so triage can find it.
 */
import { describe, expect, it } from 'vitest'
import { CALL_FOR_DATASETS, asksOfKind, datasetAskUrl, findAsk } from './callForDatasets'
import { CECELIA_NEW_ISSUE_URL } from './links'

describe('CALL_FOR_DATASETS registry', () => {
  it('has at least the two seed entries the plan names', () => {
    // The plan (docs/todo/CALL_FOR_DATASETS_PLAN.md → Decision 6) locks two seeds:
    //   sitk-rigid-3d-full  — full 6-DOF 3D rigid drift correction
    //   static-3d-registration — 3D extension of editImages.register
    // These are the current cited callers; removing one silently is how a chip on a task-param
    // vis-aid becomes a broken link.
    const ids = CALL_FOR_DATASETS.map(a => a.id)
    expect(ids).toContain('sitk-rigid-3d-full')
    expect(ids).toContain('static-3d-registration')
  })

  it('every entry names a plan the reader can find', () => {
    // A card is not the last word — a plan is. A card without a plan pointer is a marketing line
    // that nobody can trace to a design.
    for (const a of CALL_FOR_DATASETS) {
      expect(a.planPath.length).toBeGreaterThan(0)
      expect(a.title.length).toBeGreaterThan(0)
      expect(a.oneLiner.length).toBeGreaterThan(0)
      expect(a.blurb.length).toBeGreaterThan(0)
      expect(a.sceneryHint.length).toBeGreaterThan(0)
    }
  })

  it('every entry carries the call-for-datasets label so triage can find it', () => {
    // The label is what makes a stray issue (opened via a raw new-issue URL rather than through
    // this modal) still land in the tracker with a matching entry. Any entry that drops it
    // becomes silently invisible to the label filter.
    for (const a of CALL_FOR_DATASETS) {
      expect(a.labels).toContain('call-for-datasets')
    }
  })

  it('ids are unique — a duplicate would race the deep-link scroll', () => {
    const ids = CALL_FOR_DATASETS.map(a => a.id)
    expect(new Set(ids).size).toBe(ids.length)
  })

  it('every entry is a build or a test ask, and the two sections hold every entry once', () => {
    // The modal renders asksOfKind('build') then asksOfKind('test'); an entry of any other kind
    // would be in the registry and on no card.
    for (const a of CALL_FOR_DATASETS) expect(['build', 'test'], a.id).toContain(a.kind)
    const shown = [...asksOfKind('build'), ...asksOfKind('test')].map(a => a.id)
    expect(shown.sort()).toEqual(CALL_FOR_DATASETS.map(a => a.id).sort())
    expect(asksOfKind('build').length).toBeGreaterThan(0)
    expect(asksOfKind('test').length).toBeGreaterThan(0)
  })

  it('a test ask carries the test-data label and keeps its card lines short', () => {
    // docs/ui/COPY.md: the one-liner is one line; the blurb two sentences at most.
    for (const a of asksOfKind('test')) {
      expect(a.labels, a.id).toContain('test-data')
      expect(a.oneLiner.length, a.id).toBeLessThanOrEqual(90)
      expect(a.blurb.split(/(?<=\.) /).length, a.id).toBeLessThanOrEqual(2)
    }
  })
})

describe('datasetAskUrl', () => {
  it('routes to the repo new-issue endpoint with the ask s title, labels and body', () => {
    const ask = CALL_FOR_DATASETS[0]
    const url = datasetAskUrl(ask)
    expect(url.startsWith(CECELIA_NEW_ISSUE_URL + '?')).toBe(true)
    const q = new URLSearchParams(url.split('?')[1])
    expect(q.get('title')).toBe(ask.issueTitle)
    expect(q.get('labels')).toBe(ask.labels.join(','))
    // The body fronts the two questions we cannot guess — a submission missing them is a triage
    // round trip nobody wants to run.
    const body = q.get('body') ?? ''
    expect(body.toLowerCase()).toContain('what does the imaging look like')
    expect(body.toLowerCase()).toContain('cloud link')
    expect(body).toContain(ask.planPath)
  })
})

describe('datasetAskUrl for a test ask', () => {
  it('names what the data would test rather than a capability to build', () => {
    const body = new URLSearchParams(datasetAskUrl(asksOfKind('test')[0]).split('?')[1]).get('body') ?? ''
    expect(body).toContain('## What it would test')
    expect(body).not.toContain('## Capability')
  })
})

describe('findAsk', () => {
  it('resolves a known id, and returns null for anything else', () => {
    expect(findAsk('sitk-rigid-3d-full')?.id).toBe('sitk-rigid-3d-full')
    expect(findAsk('does-not-exist')).toBeNull()
    // The two shapes a `?ask=` query yields when the value is absent — matched here so the App.vue
    // handler and the modal's scroll can trust the same input contract.
    expect(findAsk(null)).toBeNull()
    expect(findAsk(undefined)).toBeNull()
  })
})
