<!--
  A tiny dropdown that shows a colour SWATCH next to each option (native <select> can't render swatches
  reliably across browsers). Used by the batch-movie channel-colormap picker; reusable for any
  {value,label,hex} colour choice. Swatch style matches the app's `.cby-swatch` idiom; hex comes from
  the caller (single source of truth — see utils/napariColormap). An option with `hex: null` renders a
  hatched "none/hidden" swatch.
-->
<script setup lang="ts">
import { ref, computed, onMounted, onBeforeUnmount, useTemplateRef } from 'vue'

export interface SwatchOption { value: string; label: string; hex: string | null }

const props = defineProps<{ modelValue: string; options: SwatchOption[]; placeholder?: string }>()
const emit = defineEmits<{ 'update:modelValue': [string] }>()

const open = ref(false)
const root = useTemplateRef<HTMLElement>('root')
const current = computed(() => props.options.find(o => o.value === props.modelValue))

function pick(v: string) { emit('update:modelValue', v); open.value = false }
function onDoc(e: MouseEvent) { if (root.value && !root.value.contains(e.target as Node)) open.value = false }
onMounted(() => document.addEventListener('mousedown', onDoc))
onBeforeUnmount(() => document.removeEventListener('mousedown', onDoc))
</script>

<template>
  <div ref="root" class="ss" :class="{ open }">
    <button type="button" class="ss-btn" @click="open = !open">
      <span class="ss-sw" :class="{ none: !current?.hex }" :style="current?.hex ? { background: current.hex } : undefined" />
      <span class="ss-lbl">{{ current?.label ?? placeholder ?? 'Select…' }}</span>
      <i class="pi pi-chevron-down ss-caret" />
    </button>
    <ul v-if="open" class="ss-menu">
      <li v-for="o in options" :key="o.value" class="ss-opt" :class="{ sel: o.value === modelValue }" @click="pick(o.value)">
        <span class="ss-sw" :class="{ none: !o.hex }" :style="o.hex ? { background: o.hex } : undefined" />
        <span class="ss-lbl">{{ o.label }}</span>
        <i v-if="o.value === modelValue" class="pi pi-check ss-check" />
      </li>
    </ul>
  </div>
</template>

<style scoped>
.ss { position: relative; min-width: 150px; }
.ss-btn { display: flex; align-items: center; gap: 6px; width: 100%; text-align: left;
  font-size: 0.8rem; color: var(--cc-text); background: var(--cc-surface-2);
  border: 1px solid var(--cc-border); border-radius: 0.4rem; padding: 0.32rem 0.5rem; cursor: pointer; }
.ss-btn:hover { border-color: #484f58; }
.ss.open .ss-btn { border-color: var(--cc-accent); }
.ss-lbl { flex: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.ss-caret { font-size: 0.6rem; opacity: 0.6; }
/* swatch — same size/shape as .cby-swatch */
.ss-sw { width: 0.8rem; height: 0.8rem; border-radius: 2px; flex-shrink: 0; border: 1px solid var(--cc-border); }
.ss-sw.none { background: repeating-linear-gradient(45deg, transparent, transparent 2px, var(--cc-text-dim) 2px, var(--cc-text-dim) 3px); }
.ss-menu { position: absolute; z-index: 40; left: 0; right: 0; top: calc(100% + 2px); margin: 0; padding: 3px;
  list-style: none; max-height: 220px; overflow: auto; background: var(--cc-surface-1);
  border: 1px solid var(--cc-border); border-radius: 0.4rem; box-shadow: 0 6px 18px rgba(0,0,0,0.4); }
.ss-opt { display: flex; align-items: center; gap: 6px; padding: 0.28rem 0.4rem; border-radius: 0.3rem;
  font-size: 0.8rem; cursor: pointer; }
.ss-opt:hover { background: var(--cc-surface-2); }
.ss-opt.sel { color: var(--cc-text); }
.ss-check { font-size: 0.6rem; color: var(--cc-accent); }
</style>
