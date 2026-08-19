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
import { computed } from 'vue'
import { tkey } from '../../plots/series'
import type { VisProps } from '../../plots/plot'
import type { SegmentationPops } from '../../plots/types'
import CanvasSidePanel from './CanvasSidePanel.vue'
import type { PlotReadout } from '../../plots/plotReadout'

const props = defineProps<{
  groups: SegmentationPops[]          // populations available, grouped by segmentation
  selected: string[]                  // selected target keys (tkey), in the current scope
  scope: 'global' | 'local'
  // The shared plot-styling block, OPT-IN as `CanvasSidePanel` documents it: omit the bag and the
  // block does not render. The Track canvas's rail is the case that needs the omission — its track
  // panels read no `vis`, so offering layout/points/colours there would be five controls wired to
  // nothing, which is the dead-chrome failure the rail plan exists to avoid.
  vis?: VisProps
  // header identity, when this picker is not "Populations" — the Track canvas swaps this box in for the
  // gating tree, and two boxes that differ only in their rows are two boxes a user cannot tell apart.
  title?: string
  icon?: string
  readout?: PlotReadout               // active plot's last render: stats test + auto-overridden settings
  docked?: boolean                    // render in a fixed rail (Analysis board) instead of floating
  // the active plot doesn't use this selection — either it is PRECOMPUTED (plots/popTypes.ts
  // isPrecomputedSpec, populations fixed by the run it reads) or it is self-contained (`rail: 'none'`,
  // it picks its own data in-panel). Say so instead of offering dead toggles; the panel stays for its
  // styling block + scope footer.
  selectionUnused?: boolean
  unusedNote?: string
}>()
const emit = defineEmits<{
  toggle: [valueName: string, pop: string, popType: string]
  'update:scope': ['global' | 'local']
  'update:vis': [patch: Partial<VisProps>]
}>()

const total = computed(() => props.groups.reduce((n, g) => n + g.populations.length, 0))
const isLit = (vn: string, pop: string, pt: string) => props.selected.includes(tkey(pt, vn, pop))
// hierarchy: indent a pop by its tree depth (a path's "/" count − 1), mirroring the gating manager.
const depthOf = (path: string) => Math.max(0, path.split('/').length - 2)
</script>

<template>
  <CanvasSidePanel :count="total" :scope="scope" :vis="vis" :docked="docked" :readout="readout"
                        v-bind="{ ...(title ? { title } : {}), ...(icon ? { icon } : {}) }"
                        :options-sections="['layout', 'points', 'colours', 'labels', 'stats']"
                        @update:scope="emit('update:scope', $event)" @update:vis="emit('update:vis', $event)">
    <div v-if="selectionUnused" class="pick-empty cc-muted">
      {{ unusedNote ?? "This plot's populations come from its run." }}
    </div>
    <div v-else-if="!total" class="pick-empty cc-muted">No populations in the selected segmentations.</div>
    <template v-for="grp in (selectionUnused ? [] : groups)" :key="grp.valueName">
      <div v-if="grp.populations.length" class="pick-group-head">{{ grp.valueName }}</div>
      <div v-for="p in grp.populations" :key="p.popType + grp.valueName + p.path"
           class="pick-row" :class="{ active: isLit(grp.valueName, p.path, p.popType) }"
           :style="{ paddingLeft: 12 + depthOf(p.path) * 14 + 'px' }"
           @click="emit('toggle', grp.valueName, p.path, p.popType)">
        <span class="pick-swatch" :style="{ background: p.colour }" />
        <span class="pick-name">{{ p.name }}</span>
        <span v-if="p.popType !== 'live'" class="pick-tag" v-tooltip.left="'Gated on per-track properties'">{{ p.popType }}</span>
        <button class="pick-eye cc-btn cc-btn-bare cc-btn-icon" :class="{ lit: isLit(grp.valueName, p.path, p.popType) }"
                v-tooltip.left="isLit(grp.valueName, p.path, p.popType) ? 'Remove from plots' : 'Plot this population'"
                @click.stop="emit('toggle', grp.valueName, p.path, p.popType)">
          <i :class="isLit(grp.valueName, p.path, p.popType) ? 'pi pi-eye' : 'pi pi-eye-slash'" />
        </button>
      </div>
    </template>
  </CanvasSidePanel>
</template>

<style scoped>
/* row styles — applied to the population list rendered into CanvasSidePanel's default slot
   (slotted content keeps THIS component's scoped styles; the chrome lives in the shell, prefixed
   `csp-`). `pick-` is this component's own prefix: it was `pm-` from when the shell was the
   population manager, which made two different components look like one. */
.pick-empty { padding: 12px; }   /* + .cc-muted */
.pick-group-head {
  padding: 5px 8px; background: var(--cc-surface-2); color: var(--cc-text-dim);
  font-size: var(--cc-fs-2xs); text-transform: uppercase; letter-spacing: 0.06em;
  border-bottom: 1px solid var(--cc-border); position: sticky; top: 0; z-index: 1;
}
.pick-row { display: flex; align-items: center; gap: 6px; padding: 4px 8px 4px 12px; cursor: pointer; border-bottom: 1px solid var(--cc-border); }
.pick-row:hover { background: var(--cc-surface-2); }
.pick-row.active { background: color-mix(in srgb, var(--cc-accent) 22%, transparent); }
.pick-swatch { width: 14px; height: 14px; border-radius: var(--cc-radius-xs); flex-shrink: 0; border: 1px solid rgba(255,255,255,0.2); }
.pick-name { flex: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.pick-tag { font-size: var(--cc-fs-3xs); text-transform: uppercase; letter-spacing: 0.04em; color: var(--cc-text-dim);
  border: 1px solid var(--cc-border); border-radius: var(--cc-radius-xs); padding: 0 3px; flex-shrink: 0; }
/* the eye is `cc-btn cc-btn-bare cc-btn-icon`; the only rule it needs of its own is the LIT state —
   its old `:hover` was byte-identical to `.cc-btn-bare:hover` and is gone. */
.pick-eye.lit { color: var(--cc-accent); }
</style>
