<script setup lang="ts">
/**
 * The task-preview control: run this task's real compute over the region napari is showing, so params
 * can be judged before committing to a full run. Sits beside Run because that is the choice it informs.
 *
 * All state and timing live in `stores/taskPreview` (which delegates to the tested
 * `utils/taskPreview` + `utils/debouncedLatest`); this file only renders it. In particular the preview
 * re-runs on view change by itself — the store subscribes to `napari:view-changed` — so there is
 * nothing to drive from here.
 */
import { computed, onMounted, watch } from 'vue'
import { useTaskPreviewStore } from '../stores/taskPreview'
import { SEVERITY } from '../lib/severity'
import type { PreviewContext } from '../utils/taskPreview'

const props = defineProps<{
  projectUid: string
  imageUid: string
  valueName: string
  funName: string
  params: Record<string, unknown> | null
  /** false for tasks with no preview support — the control hides rather than offering a dead toggle */
  previewable?: boolean
}>()

const preview = useTaskPreviewStore()

// Keep the store's context current: a parameter edit IS a re-preview trigger (debounced in the store).
const ctx = computed<PreviewContext>(() => ({
  projectUid: props.projectUid, imageUid: props.imageUid,
  valueName: props.valueName, funName: props.funName, params: props.params,
}))
watch(ctx, c => preview.setContext(c), { deep: true, immediate: true })
onMounted(() => { void preview.refreshStatus() })

const label = computed(() => {
  if (preview.starting) return 'Starting…'
  if (preview.runState !== 'idle') return 'Previewing…'
  return preview.summary.text
})
</script>

<template>
  <!-- `display: contents` so the buttons join the parent's `.run-row` flex directly: they can then
       stretch to the Run button's height, while the status line below carries `flex-basis: 100%` and
       wraps to its own row instead of squeezing Run. -->
  <div v-if="previewable !== false" class="tp">
    <button
      class="tp-btn cc-btn cc-btn-ghost cc-btn-icon"
      :class="{ 'cc-btn-on cc-btn-on-tint': preview.enabled }"
      :disabled="preview.busy && !preview.enabled"
      @click="preview.toggle()"
      v-tooltip.left="preview.enabled
        ? 'Stop previewing (frees the GPU model)'
        : 'Preview these params on the region napari is showing'"
    >
      <i class="pi" :class="preview.busy ? 'pi-spinner pi-spin' : 'pi-bolt'" />
    </button>

    <template v-if="preview.enabled">
      <!-- pin: stop chasing the view, so a result you want to compare against stays put -->
      <button
        class="tp-btn cc-btn cc-btn-ghost cc-btn-icon"
        :class="{ 'cc-btn-on cc-btn-on-tint': preview.pinned }"
        @click="preview.pinned = !preview.pinned"
        v-tooltip.left="preview.pinned
          ? 'Unpin — follow the view again'
          : 'Pin — stop following the view'"
      >
        <i class="pi" :class="preview.pinned ? 'pi-lock' : 'pi-lock-open'" />
      </button>

    </template>

    <div v-if="preview.enabled" class="tp-status">
      <span v-if="label" class="cc-readout cc-fs-2xs">{{ label }}</span>
      <!-- why there is no fresh preview. A mismatch between what the viewer shows and what the task
           reads is amber, not muted: it looks exactly like a working preview of the wrong pixels, so
           it has to be as loud as the other warnings. `utils/taskPreview.previewNotice` decides. -->
      <span v-if="preview.notice.short"
        :class="preview.notice.warn ? 'preview-warn cc-fs-2xs' : 'cc-muted cc-fs-2xs'"
        v-tooltip.left="preview.notice.detail || undefined">
        <i v-if="preview.notice.warn" class="pi" :class="SEVERITY.warn.icon" />
        {{ preview.notice.short }}
      </span>
      <!-- Every "the run will not look exactly like this" caveat: 2D fallback / no signal, base model
           only, run-would-tile, composite steps not previewed. Collected in the store (`warnings`) —
           they are the same kind of statement and rendered identically, so one loop rather than a span
           each. All go through the severity model: shape-distinct icon + text, never colour alone
           (lib/severity.ts, WCAG 1.4.1). -->
      <span v-for="w in preview.warnings" :key="w.short" class="preview-warn cc-fs-2xs"
        v-tooltip.left="w.detail">
        <i class="pi" :class="SEVERITY.warn.icon" />
        {{ w.short }}
      </span>
    </div>
  </div>
</template>

<style scoped>
/* children participate in the parent's `.run-row` flex, not in a box of our own */
.tp { display: contents; }
/* stretch, not a hardcoded height — Run's padding stays the single source of the row's height */
.tp-btn { align-self: stretch; height: auto; min-width: 2.4rem; }
/* full basis → wraps onto its own line under Run, so a long warning never narrows the Run button */
.tp-status {
  flex: 1 0 100%;
  display: flex;
  align-items: center;
  gap: 0.35rem;
  flex-wrap: wrap;
}
.preview-warn { color: var(--cc-sev-warn); display: inline-flex; align-items: center; gap: 0.2rem; }
</style>
