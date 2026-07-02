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
import PopulationPanelShell from './PopulationPanelShell.vue'

const props = defineProps<{
  groups: SegmentationPops[]          // populations available, grouped by segmentation
  selected: string[]                  // selected target keys (tkey), in the current scope
  scope: 'global' | 'local'
  vis: VisProps                       // visual properties for the current scope
  docked?: boolean                    // render in a fixed rail (Analysis board) instead of floating
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
  <PopulationPanelShell :count="total" :scope="scope" :vis="vis" :docked="docked"
                        @update:scope="emit('update:scope', $event)" @update:vis="emit('update:vis', $event)">
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
  </PopulationPanelShell>
</template>

<style scoped>
/* row styles — applied to the population list rendered into PopulationPanelShell's default slot
   (slotted content keeps THIS component's scoped styles; the chrome lives in the shell). */
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
</style>
