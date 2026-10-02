<!--
  The one toast stack (docs/UI.md → *Toast notifications*), mounted once in App.vue. Renders
  composables/useToast.ts's queue bottom-right, newest at the bottom. Severity is the traffic-light
  scale, carried by the icon AND the edge colour (colour is never the only cue). Hovering a toast
  pauses its life. A toast with an `onClick` is clickable as a whole: clicking runs the action
  and closes it.
-->
<script setup lang="ts">
import { useToast } from '../composables/useToast'
import { TOAST_STYLE } from '../utils/toastQueue'

const { toasts, remove, pause, resume } = useToast()

function activate(id: number, onClick: (() => void) | null) {
  if (!onClick) return
  remove(id)
  onClick()
}
</script>

<template>
  <div class="cc-toast-stack">
    <TransitionGroup name="cc-toast">
      <div v-for="t in toasts" :key="t.id"
           class="cc-toast" :class="{ 'cc-toast-action': t.onClick }"
           :style="{ '--sev': TOAST_STYLE[t.severity].color }"
           :role="t.severity === 'error' ? 'alert' : 'status'"
           @mouseenter="pause(t.id)" @mouseleave="resume(t.id)"
           @click="activate(t.id, t.onClick)">
        <i class="pi cc-toast-icon" :class="TOAST_STYLE[t.severity].icon" aria-hidden="true" />
        <div class="cc-toast-text">
          <div v-if="t.summary" class="cc-toast-summary">{{ t.summary }}</div>
          <div v-if="t.detail" class="cc-toast-detail">{{ t.detail }}</div>
        </div>
        <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" aria-label="Close"
                v-tooltip.left="'Close'" @click.stop="remove(t.id)">
          <i class="pi pi-times" />
        </button>
      </div>
    </TransitionGroup>
  </div>
</template>

<style scoped>
/* above modals (500) and popovers (1000), below the pointer/guide bubbles (1400+) */
.cc-toast-stack {
  position: fixed; right: 1rem; bottom: 1rem; z-index: 1300;
  display: flex; flex-direction: column; gap: 0.5rem;
  width: 22rem; max-width: calc(100vw - 2rem);
  pointer-events: none;
}
.cc-toast {
  pointer-events: auto;
  display: flex; align-items: flex-start; gap: 0.6rem;
  padding: 0.6rem 0.6rem 0.6rem 0.75rem;
  background: var(--cc-surface-2); color: var(--cc-text);
  border: 1px solid var(--cc-border); border-left: 3px solid var(--sev);
  border-radius: var(--cc-radius-md);
  box-shadow: 0 6px 20px rgba(0, 0, 0, 0.45);
  font-size: var(--cc-fs-md);
}
.cc-toast-action { cursor: pointer; }
.cc-toast-action:hover { background: var(--cc-surface-1); }
.cc-toast-icon { color: var(--sev); font-size: 1.05rem; margin-top: 0.05rem; }
.cc-toast-text { flex: 1; min-width: 0; overflow-wrap: anywhere; }
.cc-toast-summary { font-weight: 600; }
.cc-toast-detail { color: var(--cc-text-dim); margin-top: 0.15rem; }

.cc-toast-enter-active, .cc-toast-leave-active { transition: opacity 0.18s, transform 0.18s; }
.cc-toast-enter-from, .cc-toast-leave-to { opacity: 0; transform: translateX(1rem); }
</style>
