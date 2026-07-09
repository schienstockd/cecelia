<!--
  Logic-only "arm → confirm" wrapper for destructive actions — the one replacement for native browser
  dialogs (`window.confirm`), which are discouraged (out of place, unstyleable; see docs/UI.md → "No
  native browser dialogs"). It renders NONE of its own chrome: the HOST provides the buttons via the
  default scoped slot, so the host's own (scoped) CSS styles them. This matters — a child component's
  rendered DOM can't receive a parent's scoped styles, so passing host classes into a child that renders
  the button leaves it unstyled. Here the buttons live in the host template, styled by the host.

  Slot props: `{ armed, arm, confirm, cancel }`. Show the trigger button when `!armed` (calls `arm`);
  when `armed`, show Confirm (calls `confirm`) + Cancel (calls `cancel`). `arm` fires `@confirm`
  immediately when `needsConfirm=false` (e.g. an already-empty board). Auto-disarms on an outside click
  or after `autoDismissMs`. The wrapper span is `display:contents` so it doesn't disturb the host's
  layout (the buttons lay out as if direct children); it stays in the DOM for outside-click detection.

  <ConfirmButton @confirm="doQuit" v-slot="{ armed, arm, confirm, cancel }">
    <button v-if="!armed" class="footer-btn danger" @click="arm"><i class="pi pi-power-off" /></button>
    <template v-else>
      <button class="footer-btn danger" @click="confirm"><i class="pi pi-check" /></button>
      <button class="footer-btn" @click="cancel"><i class="pi pi-times" /></button>
    </template>
  </ConfirmButton>
-->
<script setup lang="ts">
import { ref, onMounted, onBeforeUnmount, useTemplateRef } from 'vue'

const props = withDefaults(defineProps<{
  needsConfirm?: boolean          // false → arm() fires `confirm` immediately (no arm step)
  autoDismissMs?: number          // armed state auto-cancels after this (0 = never)
}>(), { needsConfirm: true, autoDismissMs: 4000 })
const emit = defineEmits<{ confirm: [] }>()

const armed = ref(false)
const rootEl = useTemplateRef<HTMLElement>('rootEl')
let timer: ReturnType<typeof setTimeout> | null = null
const clearTimer = () => { if (timer) { clearTimeout(timer); timer = null } }
function cancel() { armed.value = false; clearTimer() }
function arm() {
  if (!props.needsConfirm) { emit('confirm'); return }
  armed.value = true
  clearTimer()
  if (props.autoDismissMs > 0) timer = setTimeout(cancel, props.autoDismissMs)
}
function confirm() { emit('confirm'); cancel() }
// clicking anywhere outside disarms (so an armed control doesn't stay hot across the UI)
function onDocClick(e: MouseEvent) { if (armed.value && rootEl.value && !rootEl.value.contains(e.target as Node)) cancel() }
onMounted(() => document.addEventListener('mousedown', onDocClick))
onBeforeUnmount(() => { document.removeEventListener('mousedown', onDocClick); clearTimer() })
</script>

<template>
  <span ref="rootEl" class="confirm-btn"><slot :armed="armed" :arm="arm" :confirm="confirm" :cancel="cancel" /></span>
</template>

<style scoped>
/* layout-transparent: the host's buttons lay out as if direct children (so a footer/toolbar flex treats
   them exactly like sibling buttons). Still a real DOM node for outside-click detection. */
.confirm-btn { display: contents; }
</style>
