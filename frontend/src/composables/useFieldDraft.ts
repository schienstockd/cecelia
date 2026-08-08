import { ref, watch, type Ref } from 'vue'

/**
 * A text field's in-progress text, for a field that commits on `@change` (blur / Enter) rather than on
 * every keystroke.
 *
 * The bug this exists to stop: `<input :value="model" @change="commit">` looks like an ordinary
 * controlled field and is not. The model only catches up on blur, so while you are typing the DOM is
 * AHEAD of the binding — and Vue force-patches `value` on every element patch, comparing against the
 * DOM's current text rather than the previous binding:
 *
 *     if (next !== prev || key === "value") hostPatchProp(el, key, ...)   // runtime-core, patchElement
 *     const oldValue = el.value                                           // runtime-dom, patchDOMProp
 *     if (oldValue !== newValue) el.value = newValue
 *
 * So ANY re-render of the component while the field has focus silently replaces what the user typed
 * with the bound value. On a busy panel — one that polls, or re-renders on task/WS traffic — that is
 * "I typed a name and it jumped back to the placeholder", which is exactly how it was reported for the
 * movie filename on both the single-shot and batch surfaces.
 *
 * `v-model="draft"` keeps the DOM and the binding in lockstep, so there is nothing to clobber, while
 * `commit on @change` semantics are preserved — which matters for the numeric fields, where parsing a
 * half-typed number per keystroke would fight the user ("8" clamped to the minimum before they reach
 * "800").
 *
 * The re-seed is a `watch` on the SOURCE, not an effect on every render: a re-render with an unchanged
 * source leaves the draft alone, and a genuine external change (the prefill following the version you
 * picked) still lands. Same reasoning as `@input` vs a rendered binding — track the value, not the
 * render.
 *
 * @param source  the committed value, as a getter (`() => props.suffix`)
 * @param format  how it reads in the field; defaults to `String`, with null/undefined as empty
 */
export function useFieldDraft<T>(
  source: () => T,
  format: (v: T) => string = v => (v == null ? '' : String(v)),
): Ref<string> {
  const draft = ref(format(source()))
  watch(source, v => { draft.value = format(v) })
  return draft
}
