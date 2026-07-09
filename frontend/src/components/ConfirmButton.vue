<!--
  Inline "arm → confirm" button — the ONE destructive-action confirm for the app. Replaces native
  browser dialogs (`window.confirm`), which are discouraged: they look out of place, block the thread,
  and can't be styled (see docs/UI.md → "No native browser dialogs"). First click arms (the button
  morphs to Confirm + Cancel in place); the `confirm` event only fires on the second click. Auto-disarms
  on an outside click or after a timeout, so a stray arm doesn't linger.

  Styling is passed through (`triggerClass`/`confirmClass`/`cancelClass`) so each host keeps its own
  look; trigger/confirm inner content can be a slot (icon-only buttons) or the icon/label props. Set
  `needsConfirm=false` to fire immediately with no arm step (e.g. closing an already-empty board).
-->
<script setup lang="ts">
import { ref, onMounted, onBeforeUnmount, useTemplateRef } from 'vue'

const props = withDefaults(defineProps<{
  triggerClass?: string; triggerIcon?: string; triggerLabel?: string; triggerTooltip?: string
  confirmClass?: string; confirmLabel?: string; confirmTooltip?: string
  cancelClass?: string; cancelLabel?: string
  disabled?: boolean
  needsConfirm?: boolean          // false → click fires `confirm` immediately (no arm step)
  autoDismissMs?: number          // armed state auto-cancels after this (0 = never)
}>(), { confirmLabel: 'Confirm', cancelLabel: 'Cancel', needsConfirm: true, autoDismissMs: 4000 })
const emit = defineEmits<{ confirm: [] }>()

const armed = ref(false)
const rootEl = useTemplateRef<HTMLElement>('rootEl')
let timer: ReturnType<typeof setTimeout> | null = null
const clearTimer = () => { if (timer) { clearTimeout(timer); timer = null } }
function disarm() { armed.value = false; clearTimer() }
function onTrigger() {
  if (props.disabled) return
  if (!props.needsConfirm) { emit('confirm'); return }
  armed.value = true
  clearTimer()
  if (props.autoDismissMs > 0) timer = setTimeout(disarm, props.autoDismissMs)
}
function doConfirm() { emit('confirm'); disarm() }
// clicking anywhere outside disarms (so an armed button doesn't stay hot across the UI)
function onDocClick(e: MouseEvent) { if (armed.value && rootEl.value && !rootEl.value.contains(e.target as Node)) disarm() }
onMounted(() => document.addEventListener('mousedown', onDocClick))
onBeforeUnmount(() => { document.removeEventListener('mousedown', onDocClick); clearTimer() })
</script>

<template>
  <span ref="rootEl" class="confirm-btn">
    <button v-if="!armed" :class="triggerClass" :disabled="disabled" type="button"
            v-tooltip.bottom="triggerTooltip" @click="onTrigger">
      <slot><i v-if="triggerIcon" :class="triggerIcon" /><span v-if="triggerLabel">{{ triggerLabel }}</span></slot>
    </button>
    <template v-else>
      <button :class="confirmClass ?? triggerClass" type="button" v-tooltip.bottom="confirmTooltip" @click="doConfirm">
        <slot name="confirm">{{ confirmLabel }}</slot>
      </button>
      <button :class="cancelClass ?? 'cb-cancel'" type="button" @click="disarm"><slot name="cancel">{{ cancelLabel }}</slot></button>
    </template>
  </span>
</template>

<style scoped>
.confirm-btn { display: inline-flex; align-items: center; gap: 6px; }
/* fallback cancel look when the host doesn't pass a cancelClass */
.cb-cancel { background: var(--cc-surface-2); color: var(--cc-text-dim); border: 1px solid var(--cc-border);
  border-radius: 5px; padding: 4px 10px; cursor: pointer; font-size: 12px; }
.cb-cancel:hover { color: var(--cc-text); }
</style>
