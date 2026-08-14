import { describe, it, expect } from 'vitest'
import { taskInScope, taskProjectLabel, frameTargetsOpenProject } from './taskScope'

const row = (projectUid: string) => ({ projectUid })
const nameOf = (uid: string) => ({ pA: 'zolIMa', pB: 'exciDR' }[uid])

describe('taskInScope', () => {
  // The report: after switching projects, the manager still listed the previous one's runs, with
  // nothing on a row to say so.
  it('hides another project\'s rows when scoped', () => {
    expect(taskInScope(row('pB'), 'pA', true)).toBe(false)
    expect(taskInScope(row('pA'), 'pA', true)).toBe(true)
  })

  it('shows everything with the scope off', () => {
    expect(taskInScope(row('pB'), 'pA', false)).toBe(true)
  })

  // A project import has no project yet — it is what creates one. Hiding its progress would be the
  // one row a user is certainly watching.
  it('never hides a row with no project', () => {
    expect(taskInScope(row(''), 'pA', true)).toBe(true)
  })

  // Scoping to nothing would empty the list rather than answer the question.
  it('shows everything when no project is open', () => {
    expect(taskInScope(row('pB'), '', true)).toBe(true)
    expect(taskInScope(row('pB'), undefined, true)).toBe(true)
    expect(taskInScope(row('pB'), null, true)).toBe(true)
  })
})

describe('taskProjectLabel', () => {
  it('names a foreign project when the list can mix', () => {
    expect(taskProjectLabel(row('pB'), 'pA', false, nameOf)).toBe('exciDR')
  })

  it('says nothing when every row is the open project\'s', () => {
    expect(taskProjectLabel(row('pB'), 'pA', true, nameOf)).toBe('')
    expect(taskProjectLabel(row('pA'), 'pA', false, nameOf)).toBe('')
  })

  it('does not label a row that has no project — there is nothing to name', () => {
    expect(taskProjectLabel(row(''), 'pA', false, nameOf)).toBe('')
  })

  // The project may have been deleted since the row was made; a uid is still a lead.
  it('falls back to the uid when the project is not in the recent list', () => {
    expect(taskProjectLabel(row('pGone'), 'pA', false, nameOf)).toBe('pGone')
  })
})

describe('frameTargetsOpenProject', () => {
  // The traced bug: a crop/copy finishing in the project you just left folded its set and image into
  // the project you just opened (`ws.ts` → task:result → ensureSet + addImagesFromApi).
  it('refuses a frame from another project', () => {
    expect(frameTargetsOpenProject('pB', 'pA')).toBe(false)
  })

  it('allows the open project\'s own frames', () => {
    expect(frameTargetsOpenProject('pA', 'pA')).toBe(true)
  })

  // Unattributable → behave as before, rather than dropping legitimate updates.
  it('allows a frame that names no project', () => {
    expect(frameTargetsOpenProject('', 'pA')).toBe(true)
    expect(frameTargetsOpenProject(undefined, 'pA')).toBe(true)
  })

  it('allows anything when no project is open', () => {
    expect(frameTargetsOpenProject('pB', '')).toBe(true)
    expect(frameTargetsOpenProject('pB', null)).toBe(true)
  })
})
