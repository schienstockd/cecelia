import { ref } from 'vue'

/**
 * Edit-in-place on a row: click a name, type, Enter to save / Escape to abandon.
 *
 * Four components had written this — `ImageTable` (notes + channel names), `NotebookTable`,
 * `PopulationManager` and `FlowModelVault` — and the shape was identical every time: a key naming
 * what is being edited, a draft string, and a commit that has to make three decisions. This is
 * `ImageTable`'s version, which was already the general one, lifted out rather than reinvented.
 *
 * The three decisions, and why each is here rather than left to the caller:
 *
 * 1. **Is this still the cell being edited?** Enter fires `commit`, which clears the field, and the
 *    resulting blur fires `commit` AGAIN. Without the guard the save runs twice — harmless for a
 *    rename that is now a no-op, not harmless for a save with a side effect. `PopulationManager`
 *    was missing it; both its handlers (`@keyup.enter` and `@blur`) called straight through.
 * 2. **Did anything change?** An unchanged value must not hit the network, or every accidental
 *    click-away writes.
 * 3. **Trim.** A trailing space in a name is invisible and breaks every later comparison.
 *
 * Emptiness is deliberately NOT decided here: clearing an image note is a legitimate edit, clearing
 * a model name is not. The `save` callback rejects what it cannot accept.
 */
export function useInlineEdit() {
  /** Key of the row/cell being edited — `null` when nothing is. Caller's choice of key. */
  const editing = ref<string | null>(null)
  /** The in-progress text, bound with `v-model`. */
  const draft = ref('')

  const isEditing = (key: string) => editing.value === key

  function start(key: string, current: string) {
    editing.value = key
    draft.value = current ?? ''
  }

  function cancel() { editing.value = null }

  /**
   * Finish the edit: `save(value)` runs only if this is still the open cell and the trimmed value
   * differs from `current`.
   */
  async function commit(key: string, current: string,
                        save: (value: string) => void | Promise<void>): Promise<void> {
    if (editing.value !== key) return
    editing.value = null
    const value = draft.value.trim()
    if ((current ?? '') === value) return
    await save(value)
  }

  /**
   * Focus the input as it mounts, without stealing focus it already has.
   *
   * Lives here because a template has no `document`, and every one of these fields wants it: the
   * field appears because you clicked to edit, so anything but focused is a second click.
   */
  function focusInput(el: unknown) {
    const input = el as HTMLInputElement | null
    if (input && input !== document.activeElement) input.focus()
  }

  return { editing, draft, isEditing, start, cancel, commit, focusInput }
}
