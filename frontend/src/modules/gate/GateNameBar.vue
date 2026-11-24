<!--
  The naming bar for a drawn gate, over the top-left of its gating plot (GatePlotPanel). One row that
  never wraps, so it fits a narrow plot: the draw tool's icon stands for the kind ("new quadrant" in its
  tooltip), then the name box — or, for a quadrant, the number of populations it adds — then Add / ×.
  The name box shrinks before anything wraps; the whole of what was typed stays in it. A name error
  goes on its own line underneath, wrapped to the row's width.

  Esc is not handled here: GateOverlay owns it while a gate is pending (`cancel`), so one key press
  cannot both drop the gate and disarm the tool.
-->
<script setup lang="ts">
import CcToggle from '../../components/CcToggle.vue'

defineProps<{
  kind: string                 // the pending gate's kind
  icon: string                 // its draw tool's icon classes ('pi pi-…')
  count?: number | null        // a quadrant: how many populations it adds (shown instead of a name box)
  nameTip: string              // tooltip on the name box
  invalid?: boolean            // the typed name is rejected
  error?: string | null        // why — shown under the row
  addDisabled?: boolean
  addTip?: string              // Add's tooltip (a split: the region names it creates)
}>()
const name = defineModel<string>({ default: '' })
const rule = defineModel<boolean>('rule', { default: false })     // a bisector: follow the density valley
const emit = defineEmits<{ add: []; cancel: [] }>()
</script>

<template>
  <div class="gate-name" data-guide="gate.name">
    <div class="gate-name-row">
      <i :class="[icon, 'gate-name-kind']" v-tooltip.top="`New ${kind}`" />
      <span v-if="count != null" class="gate-name-count cc-muted">{{ count }} pops</span>
      <input v-else v-model="name" class="gate-name-input" placeholder="name…" autofocus
             :aria-label="`Name for the new ${kind}`" v-tooltip.top="nameTip"
             :class="{ 'name-invalid': invalid }" @keyup.enter="emit('add')" />
      <CcToggle v-if="kind === 'bisector'" v-model="rule" label="valley"
                v-tooltip.top="'Cut at the density valley, found per image'" />
      <button class="cc-btn cc-btn-primary" :disabled="addDisabled" @click="emit('add')"
              v-tooltip.top="addTip ?? ''">Add</button>
      <button class="cc-btn cc-btn-ghost cc-btn-icon" aria-label="Cancel" v-tooltip.top="'Cancel'"
              @click="emit('cancel')"><i class="pi pi-times" /></button>
    </div>
    <span v-if="error" class="name-hint">{{ error }}</span>
  </div>
</template>

<style scoped>
/* never wider than the plot (4px inset each side); the row never wraps — the name box gives way */
.gate-name { position: absolute; top: 4px; left: 4px; max-width: calc(100% - 8px); box-sizing: border-box;
  display: flex; flex-direction: column; gap: 4px;
  background: var(--cc-surface-1); border: 1px solid var(--cc-accent); border-radius: var(--cc-radius-xs);
  padding: 3px 4px; font-size: var(--cc-fs-xs); }
.gate-name-row { display: flex; flex-wrap: nowrap; align-items: center; gap: 4px; min-width: 0; }
.gate-name-row > * { flex-shrink: 0; white-space: nowrap; }
.gate-name-kind { color: var(--cc-accent); font-size: var(--cc-fs-xs); }
.gate-name-row > .gate-name-input { flex: 1 1 6rem; width: 6rem; min-width: 2.5rem;
  background: var(--cc-bg); border-radius: var(--cc-radius-xs); padding: 1px 5px; }
.gate-name-input.name-invalid { border-color: var(--cc-sev-fail); }
/* own row under the controls; width:0 + min-width:100% keeps the message from widening the box —
   it wraps to the controls row's width instead */
.name-hint { color: var(--cc-sev-fail); font-size: var(--cc-fs-2xs); line-height: 1.3; width: 0; min-width: 100%; }
</style>
