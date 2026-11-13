<!--
  The viewer window's "On the image" overlay (docs/todo/VIEWER_ON_THE_IMAGE_PLAN.md, Decisions 2–3):
  WHAT is drawn, floating top left of the image and folding like the play bar. Every control here picks
  something from the project — a segmentation, a population type, a ribbon family. HOW it looks (eyes per row, size,
  opacity, colour) stays in the right sidebar's sections; a slider or a colour does not belong here.

  The switches write the same settings the main window's Viewer panel writes (per image: label, track
  and label-file visibility; per set: `popVisible`, `showGatedTracks`), so movies and stills still match (`drawSpec.ts` RECORDED). The window watches
  those settings and refetches; this component owns only the placement, the rows and the fold.

  Folded (per profile, `cc.vw.onImage`) it is a pill naming what is on.
-->
<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import CcToggle from '../CcToggle.vue'
import ChipSelect from '../ChipSelect.vue'
import type { LabelsDimMismatch } from '../../stores/viewer'
import { segRows, segRowActive, flipMaskVisibility, flipTracks, mismatchTip, shortLevelsTip } from '../../utils/viewer/segRows'
import { useSettingsStore } from '../../stores/settings'
import { useStoredSectionOpen } from '../../utils/sectionOpen'
import { CELL_POP_TYPES } from '../../utils/overlayAutoShow'
import { popTypeInfo } from '../../plots/popTypeRegistry'

const props = defineProps<{
  /** the image's set — every switch here is per set. '' = no set context: nothing to switch */
  setUid: string
  /** px to keep clear at the top: the timestamp sits in this corner (`StillOverlay`, 8 px down) */
  clearTop?: number
  imageUid: string
  /** segmentations with mask pixels / with a cell table (tracks) — from the viewer's meta */
  labelNames: string[]
  trackNames: string[]
  /** each segmentation's derived files (halo, nuc), outlined on its mask */
  labelFiles: Record<string, string[]>
  /** masks that do not fit the shown image version (the window publishes it) */
  dims: LabelsDimMismatch | null
}>()
const settings = useSettingsStore()
const open = useStoredSectionOpen('cc.vw.onImage', true)

const label = (popType: string) => popTypeInfo(popType)?.label ?? popType
const popOn = (popType: string) => !!props.setUid && settings.getPopVisible(props.setUid, popType)
const setPop = (popType: string, v: boolean) => { if (props.setUid) settings.setPopVisible(props.setUid, popType, v) }
const ribbonsOn = computed(() => !!props.setUid && settings.getShowGatedTracks(props.setUid))
const setRibbons = (v: boolean) => { if (props.setUid) settings.setShowGatedTracks(props.setUid, v) }

// ── Segmentations: one mask at a time, tracks per segmentation, the mask's derived files ──
const labelVis = computed(() => props.imageUid ? settings.getLabelVisibility(props.imageUid, props.labelNames) : {})
const trackVis = computed(() => props.imageUid ? settings.getTrackVisibility(props.imageUid, props.trackNames) : {})
const rows = computed(() => segRows(props.labelNames, props.trackNames, props.dims))
const activeRows = computed(() => rows.value.filter(r => segRowActive(r, labelVis.value, trackVis.value)))
// Rows in use only, the rest one click away: seven segmentations is an ordinary image, and a row each
// would make this taller than the image. Per image, like the panel's fold.
const segsExpanded = ref(false)
watch(() => props.imageUid, () => { segsExpanded.value = false })
const shownRows = computed(() => segsExpanded.value ? rows.value : activeRows.value)
const foldedCount = computed(() => rows.value.length - activeRows.value.length)
function flipMask(vn: string) {
  if (props.imageUid) settings.setLabelVisibility(props.imageUid, flipMaskVisibility(props.labelNames, labelVis.value, vn))
}
function flipTrack(vn: string) {
  if (props.imageUid) settings.setTrackVisibility(props.imageUid, flipTracks(trackVis.value, vn))
}
const filesOf = (vn: string) => props.labelFiles[vn] ?? []
const shownFilesOf = (vn: string) => {
  const hidden = props.imageUid ? settings.getLabelFilesHidden(props.imageUid, vn) : new Set<string>()
  return filesOf(vn).filter(f => !hidden.has(f))
}
// stored as what is HIDDEN (`settings.getLabelFilesHidden`)
function setShownFiles(vn: string, shown: string[]) {
  if (props.imageUid) settings.setLabelFilesHidden(props.imageUid, vn, new Set(filesOf(vn).filter(f => !shown.includes(f))))
}

/** the folded pill's summary: the population types and ribbon families that are on */
const summary = computed(() => [
  ...props.labelNames.filter(vn => labelVis.value[vn]),
  ...CELL_POP_TYPES.filter(popOn).map(label),
  ...(ribbonsOn.value ? ['cell-track ribbons'] : []),
  ...(popOn('trackclust') ? ['track-cluster ribbons'] : []),
].join(' · ') || 'nothing')
</script>

<template>
  <div v-if="setUid" class="cc-card vw-onimg" :class="{ 'vw-onimg-folded': !open }" data-guide="viewer.onImage"
       :style="{ top: `calc(0.75rem + ${clearTop ?? 0}px)` }">
    <div class="vw-onimg-head">
      <span class="cc-eyebrow cc-fs-2xs">On the image</span>
      <span v-if="!open" class="cc-fs-2xs vw-onimg-summary">{{ summary }}</span>
      <button class="cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense" :aria-expanded="open"
              :aria-label="open ? 'Fold what is on the image' : 'Show what is on the image'"
              v-tooltip.bottom="open ? 'Fold to a summary' : 'Select what is drawn on the image'"
              @click="open = !open">
        <i class="pi" :class="open ? 'pi-chevron-left' : 'pi-chevron-right'" />
      </button>
    </div>
    <template v-if="open">
      <div v-if="rows.length" class="vw-onimg-group">
        <div class="cc-eyebrow cc-fs-3xs" v-tooltip.bottom="'One mask at a time; tracks per segmentation'">Segmentation</div>
        <template v-for="r in shownRows" :key="r.valueName">
          <div class="vw-onimg-seg">
            <span class="cc-fs-2xs vw-onimg-seg-name" :title="r.valueName">{{ r.valueName }}</span>
            <i v-if="r.dimMismatch" class="pi pi-exclamation-triangle cc-muted-warn" v-tooltip.bottom="mismatchTip(dims, r.valueName)" />
            <i v-else-if="r.shortLevels" class="pi pi-exclamation-triangle cc-muted-warn" v-tooltip.bottom="shortLevelsTip(dims, r.valueName)" />
            <button class="cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense" :class="{ 'cc-btn-on cc-btn-on-tint': trackVis[r.valueName] }"
                    :aria-pressed="!!trackVis[r.valueName]" :aria-label="`Tracks of ${r.valueName}`"
                    v-tooltip.bottom="trackVis[r.valueName] ? 'Hide its tracks' : 'Show its tracks'"
                    @click="flipTrack(r.valueName)"><i class="pi pi-share-alt" /></button>
            <button v-if="r.masked" class="cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense"
                    :class="{ 'cc-btn-on cc-btn-on-tint': labelVis[r.valueName] }"
                    :aria-pressed="!!labelVis[r.valueName]" :aria-label="`Mask of ${r.valueName}`"
                    v-tooltip.bottom="labelVis[r.valueName] ? 'Hide its mask' : 'Show its mask'"
                    @click="flipMask(r.valueName)"><i class="pi pi-eye" /></button>
          </div>
          <ChipSelect v-if="r.masked && labelVis[r.valueName] && filesOf(r.valueName).length"
                      class="cc-fs-2xs" multiple :collapse-above="0"
                      :options="filesOf(r.valueName).map(f => ({ value: f, tip: `Outline the ${f}` }))"
                      :model-value="shownFilesOf(r.valueName)" :aria-label="`${r.valueName} label files`"
                      @update:model-value="v => setShownFiles(r.valueName, v as string[])" />
        </template>
        <button v-if="foldedCount > 0" class="cc-btn cc-btn-ghost cc-btn-dense cc-fs-2xs vw-onimg-more"
                v-tooltip.bottom="segsExpanded ? 'Show only the segmentations in use' : 'Show every segmentation on this image'"
                @click="segsExpanded = !segsExpanded">{{ segsExpanded ? 'Show fewer' : `${foldedCount} more` }}</button>
      </div>
      <div class="vw-onimg-group">
        <div class="cc-eyebrow cc-fs-3xs" v-tooltip.bottom="'Population types drawn as points'">Populations</div>
        <CcToggle v-for="pt in CELL_POP_TYPES" :key="pt" class="cc-fs-2xs" :label="label(pt)"
                  :model-value="popOn(pt)" @update:model-value="v => setPop(pt, v)" />
      </div>
      <div class="vw-onimg-group">
        <div class="cc-eyebrow cc-fs-3xs" v-tooltip.bottom="'Ribbons along the tracks of shown populations'">Tracks</div>
        <CcToggle class="cc-fs-2xs" label="Cell-track ribbons" :model-value="ribbonsOn" @update:model-value="setRibbons" />
        <CcToggle class="cc-fs-2xs" label="Track-cluster ribbons" :model-value="popOn('trackclust')"
                  @update:model-value="v => setPop('trackclust', v)" />
      </div>
    </template>
  </div>
</template>

<style scoped>
.vw-onimg {
  position: absolute; top: 0.75rem; left: 0.75rem; z-index: 2;
  min-width: 11rem; max-width: min(18rem, calc(100% - 1.5rem));
  padding: 0.3rem 0.5rem 0.45rem;
  box-shadow: 0 2px 10px var(--cc-shadow);
}
.vw-onimg-folded { min-width: 0; padding-bottom: 0.3rem; border-radius: var(--cc-radius-pill); }
.vw-onimg-head { display: flex; align-items: center; gap: 0.4rem; }
.vw-onimg-head .cc-eyebrow { flex: 1; white-space: nowrap; }
.vw-onimg-summary { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; color: var(--cc-accent); }
.vw-onimg-group { display: flex; flex-direction: column; gap: 0.2rem; padding-top: 0.35rem;
  border-top: 1px solid var(--cc-border); margin-top: 0.3rem; }
.vw-onimg-seg { display: flex; align-items: center; gap: 0.2rem; }
.vw-onimg-seg-name { flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.vw-onimg-more { align-self: flex-start; }
</style>
