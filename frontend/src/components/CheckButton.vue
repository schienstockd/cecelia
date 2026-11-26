<script setup lang="ts">
// One check in the action bar's Checks cluster (ChecksCluster.vue): the state icon, the check's name and
// its open count. The state is drawn by `lib/checks.ts` → checkLook — a severity icon in its
// `--cc-sev-*` colour, so the shape carries the meaning, never the colour alone. `pressed` marks a
// check whose click is a toggle that is on (the QC row filter). Inside a collapsed cluster the name moves
// into the tooltip and only the icon + count stay.
import { computed, inject, ref } from 'vue'
import { checkLook, CHECKS_COLLAPSED, type CheckState } from '../lib/checks'

const props = defineProps<{
  label: string
  state: CheckState
  count?: number
  tip: string
  pressed?: boolean
  disabled?: boolean
}>()
defineEmits<{ click: [] }>()

const look = computed(() => checkLook(props.state))
const collapsed = inject(CHECKS_COLLAPSED, ref(false))
const tipText = computed(() => collapsed.value ? `${props.label}: ${props.tip}` : props.tip)
const el = ref<HTMLButtonElement | null>(null)
defineExpose({ el })   // a host anchors its popover to the button
</script>

<template>
  <button ref="el" class="cc-btn cc-btn-ghost cc-btn-dense cc-fs-sm check-btn" :class="{ 'cc-btn-on': pressed }"
          :disabled="disabled" :aria-pressed="pressed ?? undefined" v-tooltip.bottom="tipText" :aria-label="collapsed ? `${label}${count ? ` · ${count}` : ''}` : undefined" @click="$emit('click')">
    <i class="pi" :class="look.icon" :style="look.color ? { color: look.color } : undefined" />
    <template v-if="!collapsed">{{ label }}<template v-if="count"> · {{ count }}</template></template>
    <template v-else-if="count">{{ count }}</template>
  </button>
</template>

<style scoped>
.check-btn { gap: var(--cc-space-2xs); }
</style>
