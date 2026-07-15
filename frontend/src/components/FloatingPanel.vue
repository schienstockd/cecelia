<script setup lang="ts">
// A generic floating, draggable, resizable, collapsible panel that floats above the app content
// (position: fixed). Position/size/collapsed persist per `storageKey` so it reopens where you left it.
// The parent owns visibility (v-if) and handles @close; the panel owns everything else. Reusable —
// not viewer-specific — so any tool that wants a floating box uses this one component.
import { reactive, onMounted, onUnmounted, watch } from 'vue'

const props = withDefaults(defineProps<{
  title: string
  storageKey: string            // localStorage namespace: cc.floating.<storageKey>
  icon?: string                 // optional PrimeIcons class (e.g. 'pi-eye')
  accent?: string               // optional highlight colour applied to the panel border + header icon
  defaultX?: number
  defaultY?: number
  defaultW?: number
  defaultH?: number
}>(), { icon: '', accent: '', defaultX: 240, defaultY: 84, defaultW: 290, defaultH: 460 })

const emit = defineEmits<{ (e: 'close'): void }>()

interface PanelState { x: number; y: number; w: number; h: number; collapsed: boolean }
const LSKEY = `cc.floating.${props.storageKey}`
function load(): PanelState {
  try {
    const s = JSON.parse(localStorage.getItem(LSKEY) ?? 'null') as PanelState | null
    if (s && typeof s.x === 'number') return s
  } catch { /* corrupt / absent → defaults */ }
  return { x: props.defaultX, y: props.defaultY, w: props.defaultW, h: props.defaultH, collapsed: false }
}
const st = reactive(load())
watch(st, () => localStorage.setItem(LSKEY, JSON.stringify(st)), { deep: true })

// keep the panel reachable: clamp its top-left into the viewport (a smaller window / a stale saved
// position could otherwise leave it fully off-screen with no way to grab it).
function clampIntoView() {
  st.x = Math.min(Math.max(st.x, 0), Math.max(0, window.innerWidth - 60))
  st.y = Math.min(Math.max(st.y, 0), Math.max(0, window.innerHeight - 40))
}
onMounted(() => { clampIntoView(); window.addEventListener('resize', clampIntoView) })
onUnmounted(() => { window.removeEventListener('resize', clampIntoView); endGesture() })

// ── drag (by header) / resize (bottom-right handle) — one pointer-move loop for both ──
let mode: 'drag' | 'resize' | null = null
let offX = 0, offY = 0
function onHeaderDown(e: PointerEvent) {
  if ((e.target as HTMLElement).closest('.fp-btn')) return   // header buttons aren't drag handles
  mode = 'drag'; offX = e.clientX - st.x; offY = e.clientY - st.y; beginGesture(e)
}
function onResizeDown(e: PointerEvent) {
  mode = 'resize'; offX = e.clientX - st.w; offY = e.clientY - st.h; beginGesture(e); e.stopPropagation()
}
function beginGesture(e: PointerEvent) {
  window.addEventListener('pointermove', onMove)
  window.addEventListener('pointerup', endGesture)
  e.preventDefault()
}
function onMove(e: PointerEvent) {
  if (mode === 'drag') {
    st.x = Math.min(Math.max(e.clientX - offX, 0), window.innerWidth - 60)
    st.y = Math.min(Math.max(e.clientY - offY, 0), window.innerHeight - 40)
  } else if (mode === 'resize') {
    st.w = Math.max(220, Math.min(e.clientX - offX, window.innerWidth - st.x))
    st.h = Math.max(140, Math.min(e.clientY - offY, window.innerHeight - st.y))
  }
}
function endGesture() {
  mode = null
  window.removeEventListener('pointermove', onMove)
  window.removeEventListener('pointerup', endGesture)
}
</script>

<template>
  <div class="fp" :style="{ left: st.x + 'px', top: st.y + 'px', width: st.w + 'px',
                            height: st.collapsed ? 'auto' : st.h + 'px',
                            ...(accent ? { borderColor: accent } : {}) }">
    <div class="fp-header" @pointerdown="onHeaderDown">
      <i v-if="icon" :class="['pi', icon, 'fp-icon']" :style="accent ? { color: accent } : undefined" />
      <span class="fp-title">{{ title }}</span>
      <button class="fp-btn" @click="st.collapsed = !st.collapsed"
              v-tooltip.bottom="st.collapsed ? 'Expand' : 'Collapse'">
        <i :class="['pi', st.collapsed ? 'pi-window-maximize' : 'pi-window-minimize']" />
      </button>
      <button class="fp-btn" @click="emit('close')" v-tooltip.bottom="'Close'">
        <i class="pi pi-times" />
      </button>
    </div>
    <div v-show="!st.collapsed" class="fp-body"><slot /></div>
    <div v-show="!st.collapsed" class="fp-resize" @pointerdown="onResizeDown"
         v-tooltip.left="'Drag to resize'" />
  </div>
</template>

<style scoped>
.fp {
  position: fixed;
  z-index: 60;                     /* above content + right panel, below modals/console */
  display: flex;
  flex-direction: column;
  background: var(--cc-surface-1);          /* solid — floats over content, must not be see-through */
  border: 1px solid var(--cc-border);
  border-radius: 0.5rem;
  box-shadow: 0 8px 28px rgba(0, 0, 0, 0.45);
  overflow: hidden;
  min-width: 220px;
}
.fp-header {
  display: flex;
  align-items: center;
  gap: 0.4rem;
  padding: 0.3rem 0.4rem 0.3rem 0.6rem;
  background: var(--cc-surface-2);
  border-bottom: 1px solid var(--cc-border);
  cursor: move;
  user-select: none;
  flex-shrink: 0;
}
.fp-icon { font-size: 0.8rem; color: var(--cc-accent); flex-shrink: 0; }
.fp-title {
  flex: 1;
  font-size: 0.72rem;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  color: var(--cc-text);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}
.fp-btn {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 1.4rem;
  height: 1.4rem;
  border: none;
  background: none;
  color: var(--cc-text-dim);
  cursor: pointer;
  border-radius: 0.25rem;
  font-size: 0.72rem;
  flex-shrink: 0;
}
.fp-btn:hover { color: var(--cc-text); background: var(--cc-surface-3, rgba(255, 255, 255, 0.06)); }
.fp-body { flex: 1; overflow: auto; min-height: 0; }
.fp-resize {
  position: absolute;
  right: 0;
  bottom: 0;
  width: 14px;
  height: 14px;
  cursor: nwse-resize;
  /* corner grip lines */
  background:
    linear-gradient(135deg, transparent 0 6px, var(--cc-border) 6px 7px, transparent 7px 9px,
                    var(--cc-border) 9px 10px, transparent 10px);
}
</style>
