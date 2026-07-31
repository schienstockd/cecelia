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
  params: Record<string, unknown> | null
  /** false for tasks with no preview support — the control hides rather than offering a dead toggle */
  previewable?: boolean
}>()

const preview = useTaskPreviewStore()

// Keep the store's context current: a parameter edit IS a re-preview trigger (debounced in the store).
const ctx = computed<PreviewContext>(() => ({
  projectUid: props.projectUid, imageUid: props.imageUid,
  valueName: props.valueName, params: props.params,
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
  <div v-if="previewable !== false" class="preview-row">
    <button
      class="cc-btn cc-btn-ghost cc-btn-icon"
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
        class="cc-btn cc-btn-ghost cc-btn-icon"
        :class="{ 'cc-btn-on cc-btn-on-tint': preview.pinned }"
        @click="preview.pinned = !preview.pinned"
        v-tooltip.left="preview.pinned
          ? 'Unpin — follow the view again'
          : 'Pin this result — stop re-previewing when the view moves'"
      >
        <i class="pi" :class="preview.pinned ? 'pi-lock' : 'pi-lock-open'" />
      </button>

      <span v-if="label" class="cc-readout cc-fs-2xs">{{ label }}</span>
      <span v-if="preview.hint" class="cc-muted cc-fs-2xs">{{ preview.hint }}</span>
      <!-- the 2D fallback is a real warning, so it goes through the severity model: shape-distinct
           icon + text, never colour alone (lib/severity.ts, WCAG 1.4.1) -->
      <span v-if="preview.summary.warn" class="preview-warn cc-fs-2xs"
        v-tooltip.left="preview.summary.warnDetail">
        <i class="pi" :class="SEVERITY.warn.icon" />
        {{ preview.summary.warn }}
      </span>
    </template>
  </div>
</template>

<style scoped>
.preview-row { display: flex; align-items: center; gap: 0.35rem; flex-wrap: wrap; }
.preview-warn { color: var(--cc-sev-warn); display: inline-flex; align-items: center; gap: 0.2rem; }
</style>
