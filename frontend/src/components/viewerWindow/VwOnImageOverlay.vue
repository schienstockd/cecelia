<!--
  The viewer window's "On the image" overlay (docs/todo/VIEWER_ON_THE_IMAGE_PLAN.md, Decisions 2–3):
  WHAT is drawn, floating top left of the image and folding like the play bar. Every control here picks
  something from the project — a population type, a ribbon family. HOW it looks (eyes per row, size,
  opacity, colour) stays in the right sidebar's sections; a slider or a colour does not belong here.

  The switches write the same per-set settings the main window's Viewer panel writes (`popVisible`,
  `showGatedTracks`), so movies and stills still match (`drawSpec.ts` RECORDED). The window watches
  those settings and refetches; this component owns only the placement, the rows and the fold.

  Folded (per profile, `cc.vw.onImage`) it is a pill naming what is on.
-->
<script setup lang="ts">
import { computed } from 'vue'
import CcToggle from '../CcToggle.vue'
import { useSettingsStore } from '../../stores/settings'
import { useStoredSectionOpen } from '../../utils/sectionOpen'
import { CELL_POP_TYPES } from '../../utils/overlayAutoShow'
import { popTypeInfo } from '../../plots/popTypeRegistry'

const props = defineProps<{
  /** the image's set — every switch here is per set. '' = no set context: nothing to switch */
  setUid: string
  /** px to keep clear at the top: the timestamp sits in this corner (`StillOverlay`, 8 px down) */
  clearTop?: number
}>()
const settings = useSettingsStore()
const open = useStoredSectionOpen('cc.vw.onImage', true)

const label = (popType: string) => popTypeInfo(popType)?.label ?? popType
const popOn = (popType: string) => !!props.setUid && settings.getPopVisible(props.setUid, popType)
const setPop = (popType: string, v: boolean) => { if (props.setUid) settings.setPopVisible(props.setUid, popType, v) }
const ribbonsOn = computed(() => !!props.setUid && settings.getShowGatedTracks(props.setUid))
const setRibbons = (v: boolean) => { if (props.setUid) settings.setShowGatedTracks(props.setUid, v) }

/** the folded pill's summary: the population types and ribbon families that are on */
const summary = computed(() => [
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
</style>
