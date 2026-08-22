<!--
  Type freely, with what you ALREADY USE offered as you type. The one control for a field whose value
  is user-invented but usually a repeat: a segmentation's output name, an image attribute's value, a
  movie tag, a file suffix. Its opposite is a `<select>` — right when the value must already exist,
  wrong here because you could never enter a new one.

  This replaced a native <datalist>, which is the obvious way to do it and is unusable: that popup is
  browser CHROME, so it renders at the browser's own UI font (~16px), ignores every `--cc-*` token,
  and dwarfs a 0.82rem input. No selector reaches it. Built on TeleportPopover — the canonical popover
  (teleport, positioning, theme, outside-click, Escape) — so this owns only the list and the keys.
  See docs/UI.md → *Suggesting what you already use*.

  Opens on FOCUS, showing everything already in use, and narrows as you type. It opened on typing
  only at first, which gets the question backwards: "what did I call the other one?" is exactly what
  you cannot answer if the list appears only once you can already spell it.

  `separator` makes it a MULTI-value field (tags): suggestions then complete the token at the caret
  instead of the whole box, so accepting one does not wipe the tags already typed — and a tag already
  in the box drops off the list, so the same one cannot be added twice from it.
-->
<script setup lang="ts">
import { ref, computed, nextTick } from 'vue'
import TeleportPopover from './TeleportPopover.vue'
import {
  filterSuggestions, moveHighlight, isExistingOption, activeToken, replaceActiveToken, withoutChosen,
} from '../utils/suggestInput'

// Two root nodes (the input and the teleported popover), so a caller's attrs — `@blur`,
// `@keyup.enter`, `@click.stop`, `v-tooltip` — cannot auto-inherit onto a fragment. Route them all
// to the INPUT, which is the element every caller means. Same split as MovieOptionsButton.
defineOptions({ inheritAttrs: false })

const props = defineProps<{
  modelValue: string
  options: string[]            // what already exists (may be empty — then this is a plain input)
  placeholder?: string
  tip?: string
  separator?: string           // set for a multi-value field, e.g. "," for tags
  inputClass?: string          // the caller's own input styling; each surface has its own
  markExisting?: boolean       // accent the field when the value names something that EXISTS
}>()
const emit = defineEmits<{ 'update:modelValue': [string] }>()

const input = ref<HTMLInputElement | null>(null)
const open = ref(false)
const highlight = ref(-1)

const query = computed(() => activeToken(props.modelValue ?? '', props.separator))
// What is still on offer — a multi-value field drops the tags already in the box (no-op otherwise).
const available = computed(() =>
  withoutChosen(props.options, props.modelValue ?? '', props.separator))
// Focus offers EVERYTHING — a field holding `Tcell` would otherwise open filtered to `Tcell`, hiding
// the one name the user opened it to find. The first keystroke narrows.
const showAll = ref(false)
const matches = computed(() =>
  showAll.value ? available.value : filterSuggestions(available.value, query.value))
/** Whether what is typed names something that EXISTS — the reuse vs create distinction. */
const existing = computed(() =>
  props.markExisting === true && isExistingOption(props.options, query.value))

// The popover is positioned from the input's rect but sized by its content, so a one-word suggestion
// under a wide field looks detached. Match the input's width instead.
const width = computed(() => (input.value ? `${input.value.offsetWidth}px` : undefined))

function onInput(e: Event) {
  emit('update:modelValue', (e.target as HTMLInputElement).value)
  highlight.value = -1        // typing means "something new" until an arrow key says otherwise
  showAll.value = false       // …and narrows the list to what is typed
  nextTick(() => { open.value = matches.value.length > 0 })
}

// Focus AND click: focus alone would never re-open the list after Escape, since the input still has
// it. Nothing to show = nothing to open, so a field with no history behaves like a plain input.
function onOpen() {
  showAll.value = true
  open.value = available.value.length > 0   // every tag already added = nothing left to offer
}

function accept(choice: string) {
  emit('update:modelValue', replaceActiveToken(props.modelValue ?? '', choice, props.separator))
  open.value = false
  highlight.value = -1
  showAll.value = false
  nextTick(() => {
    input.value?.focus()
    // Restoring focus fires `onOpen` when focus had actually left (a mouse pick returns it), which
    // would re-open the list on the value just chosen. Accepting is a decision, not a re-ask.
    open.value = false
    // Accepting a suggestion sets the value programmatically, so the browser fires NO `change` — and
    // a caller that commits on `@change` (MovieOutputControls emits `update:suffix` there, the way a
    // plain <input> works) would silently never hear it. Dispatch one so this component stays a
    // drop-in for the <input> it replaced. After the tick, so the DOM value is the new one.
    input.value?.dispatchEvent(new Event('change', { bubbles: true }))
  })
}

function onKeydown(e: KeyboardEvent) {
  if (e.key === 'Escape') {
    // dismiss the suggestions WITHOUT touching what was typed; only swallow the key if we actually
    // closed something, so Escape still reaches a surrounding dialog when there is no list
    if (open.value) { e.stopPropagation(); open.value = false; highlight.value = -1 }
    return
  }
  if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
    if (!matches.value.length) return
    e.preventDefault()                            // don't move the caret while picking
    open.value = true
    highlight.value = moveHighlight(highlight.value, e.key === 'ArrowDown' ? 1 : -1, matches.value.length)
    return
  }
  if (e.key === 'Enter' && open.value && highlight.value >= 0) {
    e.preventDefault()                   // accepting a suggestion is not submitting the form
    e.stopImmediatePropagation()         // …nor triggering the caller's own @keydown.enter, which is
                                         // a SIBLING listener on this input (MetadataPanel assigns
                                         // the value on it), so stopPropagation would not stop it
    accept(matches.value[highlight.value])
    swallowNextEnterUp.value = true
  }
}

// Enter that ACCEPTED a suggestion must not also fire the caller's `@keyup.enter` — several callers
// commit on it (the movie tag cell saves and closes the editor), so one keypress would both pick a
// tag and end the edit. keydown and keyup are separate events, so stopping the keydown does not stop
// the keyup: it has to be swallowed explicitly, once. A second Enter then commits, which is what a
// combobox is expected to do.
// A caller holding a `ref` to this component gets a component instance, not the <input> — so an
// inline-edit helper doing `el.focus()` would silently do nothing and the field would never take
// focus. Expose it so the component is a drop-in for a bare <input> ref.
defineExpose({ focus: () => input.value?.focus() })

const swallowNextEnterUp = ref(false)
function onKeyup(e: KeyboardEvent) {
  if (e.key !== 'Enter') return
  if (swallowNextEnterUp.value) {
    swallowNextEnterUp.value = false
    e.stopImmediatePropagation()   // a SIBLING listener on this same input, not an ancestor
  }
}
</script>

<template>
  <!-- `v-bind="$attrs"` LAST is deliberate. Vue merges same-event listeners into an array in
       attribute order, so ours run FIRST and can `stopImmediatePropagation` to keep a caller's
       `@keyup.enter` from also firing — `stopPropagation` alone would not, since that stops
       ancestors, not siblings on the same element. -->
  <input
    ref="input"
    type="text"
    :class="[inputClass ?? 'text-input', { 'si-existing': existing }]"
    :value="modelValue"
    :placeholder="placeholder"
    role="combobox"
    aria-autocomplete="list"
    :aria-expanded="open"
    @input="onInput"
    @keydown="onKeydown"
    @keyup="onKeyup"
    @focus="onOpen"
    @click="onOpen"
    @blur="open = false; showAll = false"
    v-tooltip.bottom="tip"
    v-bind="$attrs"
  />
  <TeleportPopover v-model="open" :anchor="input" placement="bottom-start" flush>
    <div class="si-menu" :style="{ minWidth: width }" role="listbox">
      <!-- mousedown, not click: the input's @blur fires first and would close the popover before a
           click ever lands. preventDefault keeps focus in the input so accept() can restore it. -->
      <button
        v-for="(name, i) in matches" :key="name"
        class="si-item" :class="{ 'si-hot': i === highlight }"
        role="option" :aria-selected="i === highlight"
        @mousedown.prevent="accept(name)"
        @mouseenter="highlight = i"
      >{{ name }}</button>
    </div>
  </TeleportPopover>
</template>

<style scoped>
.si-menu { display: flex; flex-direction: column; }   /* padding: TeleportPopover (flush) */
.si-item {
  display: block; width: 100%; padding: 0.3rem 0.55rem;
  border: none; border-radius: var(--cc-radius-xs);
  background: none; color: var(--cc-text);
  font-size: var(--cc-fs-md); text-align: left; cursor: pointer;
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
}
.si-item:hover, .si-hot { background: var(--cc-surface-2); }
/* what is typed already exists — this REUSES it rather than creating something new */
.si-existing { border-color: var(--cc-accent, var(--cc-border)); }
</style>
