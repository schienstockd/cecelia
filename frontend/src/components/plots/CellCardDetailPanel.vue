<!--
  Per-card detail floater — opened by clicking a card in CellCardsView. Reuses <StripCell> for the
  full-length filmstrip and the same card colour ring; the stats footer shows every measure the
  server returned (not just the top five the grid squeezes in). One floater open at a time; the
  storage key is shared so opening a new card reuses the same window position.

  Uses <FloatingPanel> (top-level fixed window, drag + resize + collapse + persist) rather than a
  canvas panel — comparison stays in the grid, individual arrangement happens on demand here.
-->
<script setup lang="ts">
import { ref } from 'vue'
import FloatingPanel from '../FloatingPanel.vue'
import StripCell from './StripCell.vue'
import StatBox from './StatBox.vue'
import { openViewerWindow } from '../../utils/viewerWindow'
import { useProjectStore } from '../../stores/project'
import { useViewerStore } from '../../stores/viewer'
import { showTracksInViewer } from '../../utils/viewer/showTracksInViewer'
import type { Card } from './cellCards'

const props = defineProps<{
  card: Card
  projectUid: string
  /** Same statScales passed to the grid — the detail draws against the same per-measure shared scale
      so a box's fill/position matches what the grid shows for that measure. */
  statScales?: Record<string, [number, number]>
}>()
defineEmits<{ (e: 'close'): void }>()

const viewerErr = ref('')
const project = useProjectStore()
const viewerStore = useViewerStore()

// "Show in viewer" — the SAME zoom-to-track mechanism the tracks module uses (TrackSchemeView →
// `showTracksInViewer`, extracted for reuse by the correction cockpit). Publishes the medoid track
// as the viewer's highlight and flies the camera to it. Difference from those callers: the shared
// helper bails out when no viewer is open; for the analysis board a card is more likely opened
// with NO popup up yet, so we publish the highlight via localStorage first (the popup viewer's
// storage listener picks it up on mount) and force-open the window ourselves.
async function showInViewer() {
  viewerErr.value = ''
  const uid = props.card.medoid?.uid
  const vn  = props.card.medoid?.value_name
  const tid = props.card.medoid?.track_id
  if (!props.projectUid || !uid || !vn || tid == null) {
    viewerErr.value = 'No medoid track on this card.'
    return
  }
  // Publish first: setTrackHighlight persists to localStorage, so the popup viewer picks it up on
  // mount even if it isn't up yet. Ordering matches the shared helper's docstring — highlight
  // ahead of overlays tick so the popup's next rebuild sees the new ids.
  viewerStore.setTrackHighlight({ imageUid: uid, valueName: vn, trackIds: [tid] })
  // Force-open the viewer on the medoid image (idempotent — a same-target open is a re-focus).
  openViewerWindow({ projectUid: props.projectUid, imageUid: uid, valueName: vn })
  // If the viewer was already published on this image, run the shared helper for its camera fit
  // + track-visibility bookkeeping. If we just opened it, that fit will need a follow-up click
  // once the popup registers (the popup's mount reads the highlight from localStorage, but the
  // camera-fit path here needs `viewerStore.viewState` to be populated by the popup's first tick).
  if (project.openImageUid === uid) {
    await showTracksInViewer(props.projectUid, uid, vn, [tid], 'cell-cards')
  }
}

function frameSrc(assetId: string): string {
  return `/api/board-assets?projectUid=${encodeURIComponent(props.projectUid)}&assetId=${encodeURIComponent(assetId)}`
}

function scaleFor(stat: { name: string; min: number; max: number }): [number, number] {
  const s = props.statScales?.[stat.name]
  return s && s.length === 2 ? [s[0], s[1]] : [stat.min, stat.max]
}

// Same formatter as `CellCardsView` so the detail header shows the ACTUAL timescale (mm:ss / Ns)
// when the medoid image records a TimeIncrement, or the raw frame index otherwise. Kept local since
// it's a 6-line function; extract if a third card view needs it.
function timeLabel(t: number, t_s?: number): string {
  if (typeof t_s !== 'number' || !isFinite(t_s)) return `t=${t}`
  const s = Math.round(t_s)
  if (s < 60) return `${s}s`
  const m = Math.floor(s / 60); const rem = s - m * 60
  return `${m}:${String(rem).padStart(2, '0')}`
}

</script>

<template>
  <FloatingPanel :title="`Card · ${card.name}`" storage-key="cellCards-detail"
                 :accent="card.colour" :default-w="520" :default-h="620"
                 @close="$emit('close')">
    <div class="ccd">
      <div class="ccd-head">
        <span class="ccd-name" :style="{ color: card.colour }">{{ card.name }}</span>
        <span class="cc-muted cc-fs-xs ccd-meta">
          n={{ card.n }} · medoid track {{ card.medoid.track_id }}
          <span v-if="card.medoid.uid && card.medoid.value_name">
            ({{ card.medoid.uid }} / {{ card.medoid.value_name }})
          </span>
        </span>
        <!-- Zoom-to-source parity with ImageStripView: open the medoid's image in the viewer. -->
        <button class="ccd-viewer-btn cc-btn cc-btn-ghost cc-btn-dense" type="button"
                @click.stop="showInViewer"
                v-tooltip.top="'Open the medoid track image in the viewer'">
          <i class="pi pi-directions" /> viewer
        </button>
      </div>
      <p v-if="viewerErr" class="cc-muted-warn cc-fs-xs">{{ viewerErr }}</p>
      <!-- Backend emits a single frame per card (the medoid's last t) — the detail panel is that
           one image plus its own timestamp caption. If a future backend change re-adds a filmstrip
           we iterate it; for now cards.filmstrip is a 1-element list on the interactive path. -->
      <div v-if="card.filmstrip.length" class="ccd-strip">
        <div v-for="f in card.filmstrip" :key="f.asset_id"
             class="ccd-frame" :style="{ borderColor: card.colour }">
          <StripCell class="ccd-cell"
                     :src="frameSrc(f.asset_id)" :alt="timeLabel(f.t, f.t_s)" />
          <div class="ccd-time cc-fs-2xs">{{ timeLabel(f.t, f.t_s) }}</div>
        </div>
      </div>
      <p v-else class="cc-muted">No filmstrip PNGs — the medoid's image has no OME-Zarr on disk.</p>
      <!-- Full stats: every measure on the same shared scale as the grid, laid out one per row like
           the card footer. Taller row here so shape and whisker length read at a glance. -->
      <table class="ccd-stats">
        <tr v-for="s in card.stats" :key="s.name">
          <td class="ccd-stat-name cc-muted cc-fs-xs">{{ s.name.replace(/^live\.track\./, '') }}</td>
          <td class="ccd-plot"><StatBox :stat="s" :scale="scaleFor(s)" :w="360" :h="22" /></td>
        </tr>
      </table>
    </div>
  </FloatingPanel>
</template>

<style scoped>
.ccd { padding: 8px 10px; display: flex; flex-direction: column; gap: 8px; height: 100%;
  overflow: auto; }
.ccd-head { display: flex; align-items: center; gap: 8px; }
.ccd-name { font-weight: 700; font-size: var(--cc-fs-md); }
.ccd-meta { flex: 1; min-width: 0; }
.ccd-viewer-btn { display: inline-flex; align-items: center; gap: 4px; flex: none; }
.ccd-strip { display: flex; gap: 4px; overflow-x: auto; min-height: 140px; }
.ccd-frame { flex: 1 1 0; min-width: 140px; min-height: 140px; display: flex; flex-direction: column;
  overflow: hidden; border: 2px solid transparent; border-radius: var(--cc-radius-xs); position: relative; }
.ccd-cell { flex: 1; }
.ccd-frame :deep(.strip-cell) { min-height: 0; }
/* timestamp caption anchored bottom-left of the frame, white-on-dark chip so it reads on any image */
.ccd-time { position: absolute; bottom: 4px; left: 4px; padding: 1px 5px; border-radius: 3px;
  background: rgba(0,0,0,0.55); color: #fff; font-weight: 600; }
.ccd-stats { width: 100%; border-collapse: collapse; }
.ccd-stats td { padding: 2px 6px; vertical-align: middle; border-bottom: 1px solid var(--cc-border); }
.ccd-stat-name { text-align: left; white-space: nowrap; }
.ccd-plot { width: 100%; }
.ccd-plot :deep(svg) { display: block; width: 100%; }
</style>
