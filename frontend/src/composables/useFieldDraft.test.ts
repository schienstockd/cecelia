import { describe, it, expect } from 'vitest'
import { ref, nextTick } from 'vue'
import { useFieldDraft } from './useFieldDraft'

describe('useFieldDraft', () => {
  it('seeds from the committed value', () => {
    const src = ref('cpCorrected')
    expect(useFieldDraft(() => src.value).value).toBe('cpCorrected')
  })

  it('typing is not undone by the committed value staying put', async () => {
    // the reported bug: the field commits on blur, so while typing the draft and the source disagree.
    // A re-render must not resolve that disagreement in the source's favour.
    const src = ref('cpCorrected')
    const draft = useFieldDraft(() => src.value)
    draft.value = 'my movie'          // user types
    await nextTick()                  // …and the panel re-renders (poll, task frame, whatever)
    expect(draft.value).toBe('my movie')
  })

  it('re-seeds when the committed value genuinely changes', async () => {
    const src = ref('cpCorrected')
    const draft = useFieldDraft(() => src.value)
    draft.value = 'half typed'
    src.value = 'AF'                  // e.g. the prefill follows the version now shown
    await nextTick()
    expect(draft.value).toBe('AF')
  })

  it('a commit that normalises the value shows the normalised form', async () => {
    const src = ref<number | null>(null)
    const draft = useFieldDraft(() => src.value)
    draft.value = '801'
    src.value = 802                   // parseMovieAxis rounds to an even width
    await nextTick()
    expect(draft.value).toBe('802')
  })

  it('null and undefined read as an empty field, not "null"', async () => {
    const src = ref<number | null | undefined>(null)
    const draft = useFieldDraft(() => src.value)
    expect(draft.value).toBe('')
    src.value = undefined
    await nextTick()
    expect(draft.value).toBe('')
  })

  it('a cleared field is a real value, not an absent one', async () => {
    // '' means "the user deliberately cleared the suffix", which must survive a re-seed
    const src = ref<string | null>('AF')
    const draft = useFieldDraft(() => src.value)
    src.value = ''
    await nextTick()
    expect(draft.value).toBe('')
  })
})
