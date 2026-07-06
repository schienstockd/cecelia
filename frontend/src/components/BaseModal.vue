<!--
  BaseModal — THE modal/dialog shell for this app. One hand-rolled convention (we don't use PrimeVue
  Dialog): a fixed dimmed overlay, a centred surface box, a header (icon + title + close), and slots
  for optional toolbar / body / footer. Closes on the ✕ button, on click-outside (overlay), and on
  Escape. See docs/UI.md → "Modals & dialogs" for how to add a new one.

  Every centred dialog should build on this instead of re-hand-rolling an overlay — the previous
  copy-paste (pp-*/fb-* shells across ProjectPanel, PhysicalSizeDialog, FileBrowser, …) is exactly
  the divergent re-implementation to avoid.

  Slots:  #title (override the icon+text), #toolbar (fixed row under the header), default (scrolling
          body), #footer (fixed action row). Props size the box; body scrolls, header/footer are pinned.
-->
<script setup lang="ts">
import { onMounted, onBeforeUnmount, computed } from 'vue'

const props = withDefaults(defineProps<{
  title?: string
  icon?: string        // a PrimeIcons class, e.g. 'pi-box' (rendered as <i class="pi pi-box">)
  width?: string       // CSS width, e.g. '480px'
  height?: string      // optional fixed CSS height; omit to size to content (capped at max-height)
}>(), { title: '', icon: '', width: '480px', height: '' })

const emit = defineEmits<{ (e: 'close'): void }>()

const boxStyle = computed(() => ({ width: props.width, ...(props.height ? { height: props.height } : {}) }))

function onKey(e: KeyboardEvent) { if (e.key === 'Escape') emit('close') }
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
</script>

<template>
  <div class="cc-modal-overlay" @click.self="emit('close')">
    <div class="cc-modal" :style="boxStyle">
      <div class="cc-modal-header">
        <span class="cc-modal-title">
          <slot name="title"><i v-if="icon" :class="['pi', icon]" /> {{ title }}</slot>
        </span>
        <button class="cc-modal-close" @click="emit('close')" v-tooltip.left="'Close'">
          <i class="pi pi-times" />
        </button>
      </div>

      <slot name="toolbar" />

      <div class="cc-modal-body"><slot /></div>

      <div v-if="$slots.footer" class="cc-modal-footer"><slot name="footer" /></div>
    </div>
  </div>
</template>

<style scoped>
.cc-modal-overlay {
  position: fixed; inset: 0;
  background: rgba(0,0,0,0.65);
  display: flex; align-items: center; justify-content: center;
  z-index: 500;
}
.cc-modal {
  background: var(--cc-surface-1);
  border: 1px solid var(--cc-border);
  border-radius: 0.6rem;
  max-width: 96vw; max-height: 90vh;
  display: flex; flex-direction: column;
  overflow: hidden;
  box-shadow: 0 24px 48px rgba(0,0,0,0.5);
}
.cc-modal-header {
  display: flex; align-items: center;
  padding: 0.75rem 1rem;
  border-bottom: 1px solid var(--cc-border);
  flex-shrink: 0;
}
.cc-modal-title {
  flex: 1; font-size: 0.9rem; font-weight: 600; color: var(--cc-text);
  display: flex; gap: 0.45rem; align-items: center;
}
.cc-modal-close {
  background: none; border: none; cursor: pointer;
  color: var(--cc-text-dim); font-size: 0.8rem;
  padding: 0.2rem 0.4rem; border-radius: 0.25rem;
}
.cc-modal-close:hover { background: var(--cc-surface-2); color: var(--cc-text); }

.cc-modal-body { flex: 1; overflow-y: auto; min-height: 0; }

.cc-modal-footer {
  display: flex; align-items: center; gap: 0.4rem;
  padding: 0.65rem 1rem;
  border-top: 1px solid var(--cc-border);
  background: var(--cc-surface-1);
  flex-shrink: 0;
}
</style>
