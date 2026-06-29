<!--
  Read-only population picker for the SUMMARY canvas — the cross-image / cross-segmentation series
  selector. Unlike the gating PopulationManager (single tree, mutation: rename/delete/gate), this
  lists the populations AVAILABLE across the selected images, GROUPED BY SEGMENTATION (value_name),
  and the user eye-selects which ones to plot. Each selected (segmentation, pop) is a plot series —
  so populations from DIFFERENT segmentations can be overlaid on one plot (docs/UI.md).

  Source data comes from GET /api/plots/populations (the union across the selected images). The
  host owns the selection set (keyed by `tkey`) and the highlight scope; this component is purely
  presentational + emits toggles. Draggable by its header (shared useFloatingPanel), collapsible.
-->
<script setup lang="ts">
import { ref, computed, onMounted, useTemplateRef } from 'vue'
import { useFloatingPanel } from '../../composables/useFloatingPanel'
import { tkey } from '../../plots/series'
import type { VisProps } from '../../plots/plot'
import type { SegmentationPops } from '../../plots/types'

const props = defineProps<{
  groups: SegmentationPops[]          // populations available, grouped by segmentation
  selected: string[]                  // selected target keys (tkey), in the current scope
  scope: 'global' | 'local'
  vis: VisProps                       // visual properties for the current scope
}>()
const emit = defineEmits<{
  toggle: [valueName: string, pop: string, popType: string]
  'update:scope': ['global' | 'local']
  'update:vis': [patch: Partial<VisProps>]
}>()

const collapsed = ref(false)
// each option sub-group has its own collapsible (ported from the old R plotCharts adjustments)
const open = ref<Record<string, boolean>>({ layout: false, points: false, colours: false, labels: false })
const set = (patch: Partial<VisProps>) => emit('update:vis', patch)
const total = computed(() => props.groups.reduce((n, g) => n + g.populations.length, 0))
const isLit = (vn: string, pop: string, pt: string) => props.selected.includes(tkey(pt, vn, pop))
// hierarchy: indent a pop by its tree depth (a path's "/" count − 1), mirroring the gating manager.
const depthOf = (path: string) => Math.max(0, path.split('/').length - 2)

// drag-to-move, clamped to the workspace (same behaviour as the population manager)
const panel = useTemplateRef<HTMLElement>('panel')
const { pos, startDrag } = useFloatingPanel(panel)
onMounted(() => {
  const par = panel.value?.offsetParent as HTMLElement | null
  if (par) pos.value = { x: Math.max(16, par.clientWidth - (panel.value!.offsetWidth || 300) - 16), y: 16 }
})
</script>

<template>
  <div ref="panel" class="pop-manager" :style="{ left: pos.x + 'px', top: pos.y + 'px' }">
    <div class="pm-header" @mousedown.prevent="startDrag">
      <i class="pi pi-sitemap" />
      <span class="pm-title">Populations</span>
      <span class="pm-count">{{ total }}</span>
      <button class="pm-icon" v-tooltip.left="collapsed ? 'Expand' : 'Collapse'"
              @click.stop="collapsed = !collapsed">
        <i :class="collapsed ? 'pi pi-chevron-down' : 'pi pi-chevron-up'" />
      </button>
    </div>

    <div v-show="!collapsed" class="pm-body">
      <div v-if="!total" class="pm-empty">No populations in the selected segmentations.</div>

      <template v-for="grp in groups" :key="grp.valueName">
        <div v-if="grp.populations.length" class="pm-group-head">{{ grp.valueName }}</div>
        <div v-for="p in grp.populations" :key="p.popType + grp.valueName + p.path"
             class="pm-row" :class="{ active: isLit(grp.valueName, p.path, p.popType) }"
             :style="{ paddingLeft: 12 + depthOf(p.path) * 14 + 'px' }"
             @click="emit('toggle', grp.valueName, p.path, p.popType)">
          <span class="pm-swatch" :style="{ background: p.colour }" />
          <span class="pm-name">{{ p.name }}</span>
          <span v-if="p.popType !== 'live'" class="pm-tag" v-tooltip.left="'Gated on per-track properties'">{{ p.popType }}</span>
          <button class="pm-icon" :class="{ lit: isLit(grp.valueName, p.path, p.popType) }"
                  v-tooltip.left="isLit(grp.valueName, p.path, p.popType) ? 'Remove from plots' : 'Plot this population'"
                  @click.stop="emit('toggle', grp.valueName, p.path, p.popType)">
            <i :class="isLit(grp.valueName, p.path, p.popType) ? 'pi pi-eye' : 'pi pi-eye-slash'" />
          </button>
        </div>
      </template>
    </div>

    <!-- visual properties (obey the global/local scope, like the gating manager's plot options).
         Ported from the old R plotCharts adjustments, grouped into collapsible sub-sections. -->
    <div v-show="!collapsed" class="pm-opts">
      <!-- Layout / scale -->
      <button class="pm-opts-toggle" @click="open.layout = !open.layout">
        <i :class="open.layout ? 'pi pi-chevron-down' : 'pi pi-chevron-right'" /><span>Layout</span>
      </button>
      <div v-show="open.layout" class="pm-opts-body">
        <label class="pm-opt-row"><span>Legend</span>
          <input type="checkbox" :checked="vis.legend" @change="set({ legend: ($event.target as HTMLInputElement).checked })" /></label>
        <label class="pm-opt-row" v-tooltip.left="'Log scale on the measure axis'"><span>Log scale</span>
          <input type="checkbox" :checked="vis.logScale" @change="set({ logScale: ($event.target as HTMLInputElement).checked })" /></label>
        <label class="pm-opt-row"><span>Gridlines</span>
          <input type="checkbox" :checked="vis.grid" @change="set({ grid: ($event.target as HTMLInputElement).checked })" /></label>
        <label class="pm-opt-row" v-tooltip.left="'Rotate x tick labels 45°'"><span>Rotate X labels</span>
          <input type="checkbox" :checked="vis.rotateXLabel" @change="set({ rotateXLabel: ($event.target as HTMLInputElement).checked })" /></label>
        <label class="pm-opt-row" v-tooltip.left="'Flip 90° — measure on X, series labels on Y (R coord_flip)'"><span>Rotate 90°</span>
          <input type="checkbox" :checked="vis.rotate"
                 @change="set({ rotate: ($event.target as HTMLInputElement).checked, ...(($event.target as HTMLInputElement).checked ? { facet: false } : {}) })" /></label>
        <label class="pm-opt-row" v-tooltip.left="'One small-multiple panel per series (mutually exclusive with rotate)'"><span>Facet</span>
          <input type="checkbox" :checked="vis.facet"
                 @change="set({ facet: ($event.target as HTMLInputElement).checked, ...(($event.target as HTMLInputElement).checked ? { rotate: false } : {}) })" /></label>
        <label class="pm-opt-row"><span>Dark theme</span>
          <input type="checkbox" :checked="vis.darkTheme" @change="set({ darkTheme: ($event.target as HTMLInputElement).checked })" /></label>
        <label class="pm-opt-row" v-tooltip.left="'Measure-axis range (blank = auto)'"><span>Y min</span>
          <input class="pm-opt-txt" type="text" :value="vis.yMin" @change="set({ yMin: ($event.target as HTMLInputElement).value })" /></label>
        <label class="pm-opt-row"><span>Y max</span>
          <input class="pm-opt-txt" type="text" :value="vis.yMax" @change="set({ yMax: ($event.target as HTMLInputElement).value })" /></label>
      </div>

      <!-- Points / data -->
      <button class="pm-opts-toggle" @click="open.points = !open.points">
        <i :class="open.points ? 'pi pi-chevron-down' : 'pi pi-chevron-right'" /><span>Points</span>
      </button>
      <div v-show="open.points" class="pm-opts-body">
        <label class="pm-opt-row" v-tooltip.left="'Data offset (beeswarm / random / none)'"><span>Offset</span>
          <select class="pm-opt-sel" :value="vis.jitter" @change="set({ jitter: ($event.target as HTMLSelectElement).value as VisProps['jitter'] })">
            <option value="beeswarm">beeswarm</option><option value="random">random</option><option value="none">none</option>
          </select></label>
        <label class="pm-opt-row" v-tooltip.left="'Colour points by series (else grey)'"><span>Colour data</span>
          <input type="checkbox" :checked="vis.colorData" @change="set({ colorData: ($event.target as HTMLInputElement).checked })" /></label>
        <label class="pm-opt-row"><span>Point size</span>
          <input type="range" min="0.5" max="8" step="0.5" :value="vis.pointSize"
                 @input="set({ pointSize: Number(($event.target as HTMLInputElement).value) })" />
          <span class="pm-opt-val">{{ vis.pointSize }}</span></label>
        <label class="pm-opt-row"><span>Point opacity</span>
          <input type="range" min="0.05" max="1" step="0.05" :value="vis.pointOpacity"
                 @input="set({ pointOpacity: Number(($event.target as HTMLInputElement).value) })" />
          <span class="pm-opt-val">{{ vis.pointOpacity.toFixed(2) }}</span></label>
      </div>

      <!-- Colours -->
      <button class="pm-opts-toggle" @click="open.colours = !open.colours">
        <i :class="open.colours ? 'pi pi-chevron-down' : 'pi pi-chevron-right'" /><span>Colours</span>
      </button>
      <div v-show="open.colours" class="pm-opts-body">
        <label class="pm-opt-row"><span>Palette</span>
          <select class="pm-opt-sel" :value="vis.palette" @change="set({ palette: ($event.target as HTMLSelectElement).value as VisProps['palette'] })">
            <option value="standard">standard (population)</option><option value="distinct">distinct</option>
            <option value="okabe-ito">Okabe-Ito</option>
            <option value="tol-bright">Tol bright</option><option value="tol-muted">Tol muted</option>
            <option value="tol-light">Tol light</option><option value="user">user</option>
          </select></label>
        <label v-if="vis.palette === 'user'" class="pm-opt-row pm-opt-col" v-tooltip.left="'Comma-separated colours/hex, in series order'">
          <span>Colours</span>
          <input class="pm-opt-txt wide" type="text" :value="vis.userColors" placeholder="#4477AA,#EE6677,…"
                 @change="set({ userColors: ($event.target as HTMLInputElement).value })" /></label>
      </div>

      <!-- Labels / captions -->
      <button class="pm-opts-toggle" @click="open.labels = !open.labels">
        <i :class="open.labels ? 'pi pi-chevron-down' : 'pi pi-chevron-right'" /><span>Labels</span>
      </button>
      <div v-show="open.labels" class="pm-opts-body">
        <label class="pm-opt-row pm-opt-col"><span>Title</span>
          <input class="pm-opt-txt wide" type="text" :value="vis.title" @change="set({ title: ($event.target as HTMLInputElement).value })" /></label>
        <label class="pm-opt-row pm-opt-col"><span>X label</span>
          <input class="pm-opt-txt wide" type="text" :value="vis.labX" @change="set({ labX: ($event.target as HTMLInputElement).value })" /></label>
        <label class="pm-opt-row pm-opt-col"><span>Y label</span>
          <input class="pm-opt-txt wide" type="text" :value="vis.labY" @change="set({ labY: ($event.target as HTMLInputElement).value })" /></label>
        <label class="pm-opt-row"><span>Font size</span>
          <input type="range" min="8" max="20" step="1" :value="vis.fontSize"
                 @input="set({ fontSize: Number(($event.target as HTMLInputElement).value) })" />
          <span class="pm-opt-val">{{ vis.fontSize }}</span></label>
      </div>
    </div>

    <!-- scope (global = every plot / local = active plot only): icons only, at the very bottom.
         Governs BOTH the eye-selection and the Options above. -->
    <div v-show="!collapsed" class="pm-footer">
      <div class="pm-seg">
        <button class="seg-btn" :class="{ active: scope === 'global' }"
                v-tooltip.top="'Global — selection + options apply to every plot'"
                @click="emit('update:scope', 'global')"><i class="pi pi-globe" /></button>
        <button class="seg-btn" :class="{ active: scope === 'local' }"
                v-tooltip.top="'Local — selection + options apply to the active plot only'"
                @click="emit('update:scope', 'local')"><i class="pi pi-map-marker" /></button>
      </div>
    </div>
  </div>
</template>

<style scoped>
.pop-manager {
  position: absolute; z-index: 20; width: 300px;
  background: var(--cc-surface-1); border: 1px solid var(--cc-border);
  border-radius: 6px; box-shadow: 0 6px 24px rgba(0,0,0,0.4);
  font-size: 12px; color: var(--cc-text); user-select: none;
}
.pm-header {
  display: flex; align-items: center; gap: 6px; padding: 6px 8px;
  cursor: move; border-bottom: 1px solid var(--cc-border); background: var(--cc-surface-2);
  border-radius: 6px 6px 0 0;
}
.pm-title { font-weight: 600; }
.pm-count { color: var(--cc-text-dim); margin-left: auto; }
.pm-body { max-height: 60vh; overflow-y: auto; }
.pm-empty { padding: 12px; color: var(--cc-text-dim); }
.pm-group-head {
  padding: 5px 8px; background: var(--cc-surface-2); color: var(--cc-text-dim);
  font-size: 10px; text-transform: uppercase; letter-spacing: 0.06em;
  border-bottom: 1px solid var(--cc-border); position: sticky; top: 0; z-index: 1;
}
.pm-row { display: flex; align-items: center; gap: 6px; padding: 4px 8px 4px 12px; cursor: pointer; border-bottom: 1px solid var(--cc-border); }
.pm-row:hover { background: var(--cc-surface-2); }
.pm-row.active { background: color-mix(in srgb, var(--cc-accent) 22%, transparent); }
.pm-swatch { width: 14px; height: 14px; border-radius: 3px; flex-shrink: 0; border: 1px solid rgba(255,255,255,0.2); }
.pm-name { flex: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.pm-tag { font-size: 9px; text-transform: uppercase; letter-spacing: 0.04em; color: var(--cc-text-dim);
  border: 1px solid var(--cc-border); border-radius: 3px; padding: 0 3px; flex-shrink: 0; }
.pm-icon { background: none; border: none; color: var(--cc-text-dim); cursor: pointer; padding: 2px; }
.pm-icon:hover { color: var(--cc-text); }
.pm-icon.lit { color: var(--cc-accent); }

/* visual-property options box */
.pm-opts { border-top: 1px solid var(--cc-border); }
.pm-opts-toggle { display: flex; align-items: center; gap: 6px; width: 100%; background: none; border: none;
  color: var(--cc-text-dim); cursor: pointer; padding: 6px 8px; font-size: 11px; text-transform: uppercase; letter-spacing: 0.05em; }
.pm-opts-toggle:hover { color: var(--cc-text); }
.pm-opts-body { padding: 4px 10px 10px; display: flex; flex-direction: column; gap: 8px; }
.pm-opt-row { display: flex; align-items: center; gap: 8px; color: var(--cc-text-dim); font-size: 11px; }
.pm-opt-row > span:first-child { flex: 1; }
.pm-opt-row input[type="range"] { flex: 1; max-width: 110px; }
.pm-opt-val { width: 2.2rem; text-align: right; font-variant-numeric: tabular-nums; }
.pm-opt-sel { font-size: 11px; max-width: 7rem; }
.pm-opt-txt { font-size: 11px; width: 4rem; padding: 1px 4px; }
.pm-opt-txt.wide { width: 100%; }
.pm-opt-col { flex-direction: column; align-items: stretch; gap: 3px; }
.pm-opt-col > span:first-child { flex: none; }

.pm-footer { display: flex; align-items: center; padding: 6px 8px; border-top: 1px solid var(--cc-border); background: var(--cc-surface-2); border-radius: 0 0 6px 6px; }
.pm-seg { display: inline-flex; gap: 4px; margin-left: auto; }
.seg-btn {
  display: inline-flex; align-items: center; justify-content: center;
  width: 1.8rem; height: 1.8rem; border-radius: 0.3rem;
  border: 1px solid var(--cc-border); background: var(--cc-surface-1);
  color: var(--cc-text-dim); cursor: pointer; font-size: 0.8rem; transition: background 0.1s, color 0.1s, border-color 0.1s;
}
.seg-btn:hover { color: var(--cc-text); border-color: #484f58; }
.seg-btn.active { background: #2d1b69; border-color: #7c3aed; color: #c4b5fd; }
</style>
