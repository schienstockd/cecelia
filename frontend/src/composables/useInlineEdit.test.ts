import { describe, it, expect, vi } from 'vitest'
import { useInlineEdit } from './useInlineEdit'

describe('useInlineEdit', () => {
  it('opens on a key with the current value, and closes on cancel', () => {
    const e = useInlineEdit()
    expect(e.isEditing('a')).toBe(false)
    e.start('a', 'name')
    expect(e.editing.value).toBe('a')
    expect(e.draft.value).toBe('name')
    expect(e.isEditing('a')).toBe(true)
    e.cancel()
    expect(e.editing.value).toBeNull()
  })

  it('saves a changed value, trimmed', async () => {
    const e = useInlineEdit()
    const save = vi.fn()
    e.start('a', 'old')
    e.draft.value = '  new  '
    await e.commit('a', 'old', save)
    expect(save).toHaveBeenCalledWith('new')
    expect(e.editing.value).toBeNull()
  })

  // Enter clears the field, and the resulting blur calls commit a SECOND time. Without the guard the
  // save runs twice — PopulationManager had both handlers wired straight through.
  it('does not save twice when blur follows Enter', async () => {
    const e = useInlineEdit()
    const save = vi.fn()
    e.start('a', 'old')
    e.draft.value = 'new'
    await e.commit('a', 'old', save)     // Enter
    await e.commit('a', 'old', save)     // the blur that follows
    expect(save).toHaveBeenCalledTimes(1)
  })

  it('ignores a commit for a row that is not the open one', async () => {
    const e = useInlineEdit()
    const save = vi.fn()
    e.start('a', 'old')
    await e.commit('b', 'other', save)
    expect(save).not.toHaveBeenCalled()
    expect(e.editing.value).toBe('a')    // and the open edit is left alone
  })

  it('does not save an unchanged value, whitespace included', async () => {
    const e = useInlineEdit()
    const save = vi.fn()
    e.start('a', 'name')
    e.draft.value = '  name '
    await e.commit('a', 'name', save)
    expect(save).not.toHaveBeenCalled()
  })

  it('leaves emptiness to the caller — clearing a note is legitimate', async () => {
    const e = useInlineEdit()
    const save = vi.fn()
    e.start('a', 'note')
    e.draft.value = ''
    await e.commit('a', 'note', save)
    expect(save).toHaveBeenCalledWith('')
  })

  it('treats a null current as empty rather than throwing', async () => {
    const e = useInlineEdit()
    const save = vi.fn()
    e.start('a', null as unknown as string)
    expect(e.draft.value).toBe('')
    await e.commit('a', null as unknown as string, save)
    expect(save).not.toHaveBeenCalled()
  })

  it('awaits an async save before the next edit can start', async () => {
    const e = useInlineEdit()
    let resolved = false
    e.start('a', 'old')
    e.draft.value = 'new'
    await e.commit('a', 'old', async () => { await Promise.resolve(); resolved = true })
    expect(resolved).toBe(true)
  })
})
