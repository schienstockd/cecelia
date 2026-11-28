<!--
  A single-pick dropdown with a filter box — the `<select>` for a list too long to scroll: a gate axis
  on a 40-channel multiplex panel. Closed, it looks like a native select (`.cc-select` shares the global
  `select` styling); open, the filter box has focus, so typing narrows the list at once. Arrows move,
  Enter picks, Escape closes. Built on TeleportPopover (positioning, outside-click, Escape); the
  filtering is `utils/searchSelect.ts` (tested).

  Use a plain `<select>` for a short, fixed list (a transform, a mode): a filter box there is noise.
-->
<script setup lang="ts">
import { ref, computed, watch, nextTick } from 'vue'
import TeleportPopover from './TeleportPopover.vue'
import { moveHighlight } from '../utils/suggestInput'
import { filterGroups, flatOptions, labelFor, optionLabel, type SearchGroup } from '../utils/searchSelect'

const props = defineProps<{
  modelValue: string
  groups: readonly SearchGroup[]
  ariaLabel?: string
  disabled?: boolean
  /** shown closed when `modelValue` is none of the options — an "add one" picker */
  placeholder?: string
}>()
const emit = defineEmits<{ 'update:modelValue': [string] }>()

const trigger = ref<HTMLButtonElement | null>(null)
const filterEl = ref<HTMLInputElement | null>(null)
const listEl = ref<HTMLElement | null>(null)
const open = ref(false)
const query = ref('')
const highlight = ref(-1)

const shown = computed(() => filterGroups(props.groups, query.value))
const flat = computed(() => flatOptions(shown.value))
const label = computed(() => labelFor(props.groups, props.modelValue))
// the open list is at least the control's width, and grows to its longest name (capped by `.ss-menu`):
// a narrow panel's control must not cut `flow.cell.edge_dist#regions.loc` to `flow.cell.edge_dist#re…`
const minWidth = computed(() => (trigger.value ? `${Math.max(trigger.value.offsetWidth, 160)}px` : undefined))

function scrollToHot() {
  nextTick(() => listEl.value?.querySelector('.ss-hot')?.scrollIntoView({ block: 'nearest' }))
}

watch(open, (o) => {
  if (!o) return
  query.value = ''
  highlight.value = flat.value.findIndex(x => x.value === props.modelValue)
  nextTick(() => { filterEl.value?.focus(); scrollToHot() })
})
// Typing narrows; the first match is then the one Enter takes.
watch(query, () => { highlight.value = flat.value.length ? 0 : -1 })

function pick(value: string) {
  open.value = false
  if (value !== props.modelValue) emit('update:modelValue', value)
  trigger.value?.focus()
}

function onKeydown(e: KeyboardEvent) {
  if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
    e.preventDefault()
    highlight.value = moveHighlight(highlight.value, e.key === 'ArrowDown' ? 1 : -1, flat.value.length)
    scrollToHot()
  } else if (e.key === 'Enter') {
    e.preventDefault()
    const o = flat.value[highlight.value]
    if (o) pick(o.value)
  } else if (e.key === 'Escape') {
    trigger.value?.focus()   // TeleportPopover closes on it; focus returns to the control
  }
}
</script>

<template>
  <span class="ss">
    <button ref="trigger" type="button" class="cc-select ss-trigger" :disabled="disabled"
            :aria-label="ariaLabel" aria-haspopup="listbox" :aria-expanded="open"
            @click="open = !open"
            @keydown.down.prevent="open = true">{{ label || placeholder }}</button>
    <TeleportPopover v-model="open" :anchor="trigger" placement="bottom-start" flush>
      <div class="ss-menu" :style="{ minWidth }">
        <input ref="filterEl" v-model="query" type="search" class="cc-input-xs ss-filter"
               placeholder="Search" aria-label="Search the list"
               v-tooltip.top="'Type to narrow; Enter picks'" @keydown="onKeydown" />
        <div ref="listEl" class="ss-list" role="listbox">
          <template v-for="(g, gi) in shown" :key="gi">
            <div v-if="g.title" class="ss-group cc-fs-2xs cc-muted">{{ g.title }}</div>
            <button v-for="o in g.options" :key="o.value" type="button"
                    :class="['ss-item', { 'ss-hot': flat[highlight] === o, 'ss-on': o.value === modelValue }]"
                    role="option" :aria-selected="o.value === modelValue"
                    @mousedown.prevent="pick(o.value)"
                    @mouseenter="highlight = flat.indexOf(o)">{{ optionLabel(o) }}</button>
          </template>
          <div v-if="!flat.length" class="cc-empty cc-empty-inline">No match</div>
        </div>
      </div>
    </TeleportPopover>
  </span>
</template>

<style scoped>
.ss { display: inline-flex; min-width: 0; }
.ss-trigger { flex: 1; min-width: 0; text-align: left;
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.ss-menu { display: flex; flex-direction: column; gap: 0.25rem;      /* padding: TeleportPopover (flush) */
  width: max-content; max-width: min(34rem, calc(100vw - 2rem)); }
.ss-filter { width: 100%; box-sizing: border-box; }
/* Block flow, not a flex column: flex items with overflow:hidden shrink to 0 to fit max-height, so a
   40-channel list collapsed into overlapping slivers. */
.ss-list { max-height: 18rem; overflow-y: auto; }
.ss-group { padding: 0.3rem 0.55rem 0.1rem; font-weight: 600; }
.ss-item {
  display: block; width: 100%; padding: 0.25rem 0.55rem 0.25rem 0.9rem;
  border: none; border-radius: var(--cc-radius-xs);
  background: none; color: var(--cc-text);
  font-size: var(--cc-fs-sm); text-align: left; cursor: pointer;
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
}
.ss-hot { background: var(--cc-surface-2); }
.ss-on { color: var(--cc-accent); font-weight: 600; }
</style>
