<!--
  Logic-only "arm → confirm" wrapper for destructive actions — the one replacement for native browser
  dialogs (`window.confirm`), which are discouraged (out of place, unstyleable; see docs/UI.md → "No
  native browser dialogs"). It renders NONE of its own chrome: the HOST provides the buttons via the
  default scoped slot, so the host's own (scoped) CSS styles them. This matters — a child component's
  rendered DOM can't receive a parent's scoped styles, so passing host classes into a child that renders
  the button leaves it unstyled. Here the buttons live in the host template, styled by the host.

  Slot props: `{ armed, arm, confirm, press }`. ONE button, flipped in place: it calls `press` (arm when
  idle, confirm when armed) and changes only its LOOK on `armed` — danger fill and the
  `pi-exclamation-triangle` "click again" glyph. The label stays put so the button never changes width
  under the cursor; what the second click does goes in the armed tooltip. There is
  deliberately NO Cancel button: a Confirm + Cancel pair sprouting next to the trigger read as two new
  choices and confused users; walking away (outside click / `autoDismissMs`) is the cancel. `arm` /
  `confirm` stay exposed for hosts that do work on arming (Quit counts running tasks, Prune dry-runs).
  `arm` fires `@confirm` immediately when `needsConfirm=false` (e.g. an already-empty board). The
  wrapper span is `display:contents` so it doesn't disturb the host's layout; it stays in the DOM for
  outside-click detection.

  <ConfirmButton @confirm="doQuit" v-slot="{ armed, press }">
    <button class="cc-btn" :class="armed ? 'cc-btn-danger' : 'cc-btn-ghost'" @click="press">
      <i class="pi" :class="armed ? 'pi-exclamation-triangle' : 'pi-power-off'" /> Quit
    </button>
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
function press() { if (armed.value) confirm(); else arm() }
// clicking anywhere outside disarms (so an armed control doesn't stay hot across the UI)
function onDocClick(e: MouseEvent) { if (armed.value && rootEl.value && !rootEl.value.contains(e.target as Node)) cancel() }
onMounted(() => document.addEventListener('mousedown', onDocClick))
onBeforeUnmount(() => { document.removeEventListener('mousedown', onDocClick); clearTimer() })
</script>

<template>
  <span ref="rootEl" class="confirm-btn"><slot :armed="armed" :arm="arm" :confirm="confirm" :press="press" /></span>
</template>

<style scoped>
/* layout-transparent: the host's buttons lay out as if direct children (so a footer/toolbar flex treats
   them exactly like sibling buttons). Still a real DOM node for outside-click detection. */
.confirm-btn { display: contents; }
</style>
