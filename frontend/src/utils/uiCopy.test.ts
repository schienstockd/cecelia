import { describe, it, expect } from 'vitest'
import {
  COPY_MAX, normalise, isMultiSentence, isTooLong, tooltipStrings, hintStrings,
} from './uiCopy'

describe('normalise', () => {
  it('collapses whitespace so a wrapped literal measures the same as a joined one', () => {
    expect(normalise('a\n   b')).toBe('a b')
  })

  it('unescapes the quote form Vue attributes use', () => {
    expect(normalise("napari\\'s scale bar")).toBe("napari's scale bar")
  })
})

describe('isMultiSentence', () => {
  it('flags a second sentence', () => {
    expect(isMultiSentence('Remove this image. The original file is not deleted.')).toBe(true)
    expect(isMultiSentence('Render on the GPU. Restarts napari. Linux only.')).toBe(true)
  })

  it('does not flag an abbreviation', () => {
    expect(isMultiSentence('Colour by a cell property (e.g. HMM state)')).toBe(false)
    expect(isMultiSentence('Second attribute to combine (e.g. Treatment × Mouse)')).toBe(false)
    expect(isMultiSentence('Regex over the name; a (group) if present, e.g. M(\\d+)→4')).toBe(false)
  })

  it('does not flag one sentence, however it is punctuated', () => {
    expect(isMultiSentence('Delete the set and all its images — cannot be undone')).toBe(false)
    expect(isMultiSentence('all = ignore failures, require_all = abort')).toBe(false)
    expect(isMultiSentence('Output suffix → clusters.<suffix>.')).toBe(false)
  })

  it('does not treat a decimal or a version as a sentence break', () => {
    expect(isMultiSentence('Lower it to 99.9 if a hot pixel pins the max')).toBe(false)
  })
})

describe('isTooLong', () => {
  it('measures the normalised string against the budget', () => {
    expect(isTooLong('x'.repeat(COPY_MAX))).toBe(false)
    expect(isTooLong('x'.repeat(COPY_MAX + 1))).toBe(true)
    expect(isTooLong('x'.repeat(COPY_MAX + 1).replace(/x/g, 'x '))).toBe(true)
  })
})

describe('tooltipStrings', () => {
  it('reads the literals a user sees, not the binding expression', () => {
    // The regression this whole module exists for: measuring the expression makes this one
    // 95-character violation, when neither branch is close to the budget.
    const src = `<i v-tooltip="flaggedActive ? 'Deselect flagged images' : 'Select all flagged images'" />`
    expect(tooltipStrings(src)).toEqual(['Deselect flagged images', 'Select all flagged images'])
    expect(tooltipStrings(src).some(isTooLong)).toBe(false)
  })

  it('handles every placement modifier and both attribute quote styles', () => {
    expect(tooltipStrings(`<i v-tooltip.right="'a'" />`)).toEqual(['a'])
    expect(tooltipStrings(`<i v-tooltip.top='"unused" + \`b\`' />`)).toEqual(['b'])
  })

  it('drops ${…} rather than guessing its rendered width', () => {
    expect(tooltipStrings('<i v-tooltip="`Apply to ${n} image(s)`" />')).toEqual(['Apply to image(s)'])
  })

  it('joins a literal split across lines', () => {
    expect(tooltipStrings(`<i v-tooltip="'one\n         two'" />`)).toEqual(['one two'])
  })

  it('ignores a binding with no literal — the text is checked where it is built', () => {
    expect(tooltipStrings('<i v-tooltip="someLabel" />')).toEqual([])
  })

  it('does not run past the end of one binding into the next', () => {
    expect(tooltipStrings(`<i v-tooltip="'a'" /><i v-tooltip="'b'" />`)).toEqual(['a', 'b'])
  })
})

describe('hintStrings', () => {
  it('reads the static hint props', () => {
    expect(hintStrings('<ModuleLayout hint-key="segment" hint="Detect cells here." />'))
      .toEqual(['Detect cells here.'])
    expect(hintStrings('<X no-set-hint="Create a set first." />')).toEqual(['Create a set first.'])
  })

  it('ignores a bound hint, and does not match hint-key', () => {
    expect(hintStrings('<X :hint="dynamicHint" hint-key="segment" />')).toEqual([])
  })
})

// ── The ratchet: every rendered string in the app is inside budget ────────────────────────────────
//
// An exact allow-list, not a count — the `cssScenarios` lesson. A count-based baseline silently
// permits swapping one violation for another inside the same file, and stops meaning anything once
// it reaches zero. Both surfaces were swept to zero, so the bar is simply: this list, and nothing
// else. Adding an entry should require the argument below, not a bumped number.
//
// Before adding: is the removed explanation in `docs/`? That was true of every one of the ~100
// strings shortened in the two sweeps — pseudoreplication in PLOTS.md, chain failure policy in
// SCHEDULER.md, resource pools in MODULES.md, import staging in IMPORT_RESCALE_PLAN.md. If the fact
// matters, it belongs there, and the UI can then be short without losing it.
const ALLOWED_LONG: string[] = []

// Multi-sentence copy is allowed only where the second sentence is a call to ACTION rather than an
// explanation — a notification that tells you what to do about it.
const ALLOWED_MULTI: string[] = [
  'Update available — . Open Settings to install.',   // status + what to do; `${version}` stripped
]

const SFC = import.meta.glob('/src/**/*.vue', { query: '?raw', import: 'default', eager: true }) as
  Record<string, string>


describe('UI copy stays short (docs/UI.md → UI copy — keep it short)', () => {
  const sfcs = Object.entries(SFC)

  it('found the sources it is meant to police', () => {
    expect(sfcs.length).toBeGreaterThan(50)      // the glob resolved
  })

  it('no tooltip or hint is over the budget', () => {
    const over: string[] = []
    for (const [path, src] of sfcs) {
      for (const s of [...tooltipStrings(src), ...hintStrings(src)]) {
        if (isTooLong(s) && !ALLOWED_LONG.includes(s)) over.push(`${path}: [${s.length}] ${s}`)
      }
    }
    expect(over).toEqual([])
  })

  it('nothing explains itself in a second sentence', () => {
    const multi: string[] = []
    for (const [path, src] of sfcs) {
      for (const s of [...tooltipStrings(src), ...hintStrings(src)]) {
        if (isMultiSentence(s) && !ALLOWED_MULTI.includes(s)) multi.push(`${path}: ${s}`)
      }
    }
    expect(multi).toEqual([])
  })
})
