<!--
  ImageMetadataDialog — read-only "everything we know about this image" view, opened from the info
  icon on each ImageTable row. Its headline is the original source file location (oriPath): the raw
  file this image was converted from, which is otherwise not visible anywhere in the UI. Built on
  BaseModal like every other dialog. Editing physical size / timing / channels lives elsewhere
  (PhysicalSizeDialog, Metadata page) — this one only shows.
-->
<script setup lang="ts">
import { computed, ref, onMounted } from 'vue'
import BaseModal from './BaseModal.vue'
import type { CciaImage } from '../stores/project'
import { useCopyFlash } from '../composables/useCopyFlash'
import { useProjectMetaStore } from '../stores/projectMeta'
import { formatBytes } from '../utils/storage'
import { storeFormatFacts, storeFormatTitle, storeLevelRows,
         type StoreEncoding } from '../utils/storeFormat'
import { formatPhysicalSize, fmtNum } from '../utils/physicalSize'
import { buildLineageForest, flattenLineage } from '../utils/versionLineage'
import { useTaskDefsStore } from '../stores/taskDefs'

const props = defineProps<{ image: CciaImage }>()
defineEmits<{ (e: 'close'): void }>()

const img = computed(() => props.image)
const projectMeta = useProjectMetaStore()

// "12 × 512 × 512" style formatting is overkill here — we show each dimension as its own row so a
// missing one reads as "—" rather than a silently-absent factor.
function num(v: number | null | undefined): string {
  return v === null || v === undefined ? '—' : String(v)
}

// Calibration readouts share the ImageTable's formatting (`0.346 µm`, not `0.3459441507762987
// micrometer`) — one shared helper, `formatPhysicalSize`, so the two surfaces stay in sync.
const physical = computed(() => {
  const i = img.value
  const u = i.physicalSizeUnit
  return {
    x: formatPhysicalSize(i.physicalSizeX, u),
    y: formatPhysicalSize(i.physicalSizeY, u),
    z: formatPhysicalSize(i.physicalSizeZ, u),
  }
})

const timeStr = computed(() => {
  const i = img.value
  if (i.timeIncrement === null || i.timeIncrement === undefined) return '—'
  // Time is not a physical-size (unit is 's' or 'ms', not µm) — normalise the number the same way
  // but keep the unit as-recorded rather than routing it through the µm-shortening path.
  return `${fmtNum(i.timeIncrement)} ${i.timeIncrementUnit ?? 's'}`
})

const channels = computed(() => img.value.channelNames?.filter(c => c && c.length) ?? [])
// valueName → filename, active first. The active version is the zarr the app currently reads.
const versions = computed(() => Object.entries(img.value.filepaths ?? {}))

// Version lineage — one row per stored version, indented under whichever version was used as
// INPUT to produce it (derived from the automatic run log; see utils/versionLineage.ts). Only
// meaningful when there are two or more versions AND at least one was produced from another; a lone
// `default` (or a set with no run-log edges) collapses to nothing rather than a "flow" of a single
// leaf.
const taskDefs = useTaskDefsStore()
const lineageRows = computed(() => {
  const names = versions.value.map(([vn]) => vn)
  const rows = flattenLineage(buildLineageForest(names, img.value.runLog))
  return rows
})
const hasLineageEdges = computed(() => lineageRows.value.some(r => r.node.edge))
// The label the caption shows for a producing task ('cleanupImages.driftCorrect' → 'Drift correct').
// Task defs load asynchronously, so labelFor falls back to the fun's last segment while it warms.
const funLabel = (fun: string) => taskDefs.labelFor(fun)

// What each stored version IS on disk: how its pixels are ENCODED, and how much space it takes.
// Fetched rather than stored: the codec is a property of the store (which can be re-landed on a
// different one by rechunk_zarr.py without anything in ccid.json changing), and the size can only be
// known by walking it. Awaited AFTER the modal is on screen, never before — the walk is the expensive
// part (a few hundred ms per store warm, seconds cold on a multi-GB version), so the sizes fill in a
// moment later rather than delaying the dialog. Failure is silent — this dialog is read-only
// information and must still open if a store is missing.
type StoreInfo = { bytes: number, label?: string } & StoreEncoding
const stores = ref<{ versions: Record<string, StoreInfo | null>, labels: Record<string, StoreInfo> }>(
  { versions: {}, labels: {} })
onMounted(async () => {
  // Warm the task-defs cache so lineage captions carry the pretty task label ('Drift correct')
  // rather than the fun's last segment ('driftCorrect'). Awaited alongside the store walk since
  // both are display-only enrichments the modal must still open without.
  taskDefs.ensureLoaded()
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  try {
    const res = await fetch(`/api/images/stores?projectUid=${encodeURIComponent(projectUid)}`
                            + `&imageUid=${encodeURIComponent(img.value.uid)}`)
    if (res.ok) {
      const j = await res.json()
      stores.value = { versions: j.versions ?? {}, labels: j.labels ?? {} }
    }
  } catch { /* display-only */ }
})
// 0 bytes means "not measured yet / store missing", not an empty store — show it as unknown.
const size = (s: StoreInfo | null | undefined) => (s && s.bytes > 0 ? formatBytes(s.bytes) : '—')
const labels = computed(() => Object.entries(img.value.labels ?? {}))
const attrs = computed(() => Object.entries(img.value.attr ?? {}).filter(([, v]) => v && v.length))
const extra = computed(() => Object.entries(img.value.extraMeta ?? {}))

// copy-to-clipboard for path-like values — shared helper, keyed per field
const { isCopied, copy: copyValue } = useCopyFlash()
const copy = (key: string, value: string) => copyValue(value, key)

// Which stored versions have their Pyramid section expanded. Default collapsed — a project with N
// stored versions (default + drift-corrected + AF-corrected + …) would otherwise show N per-level
// tables the moment the modal opens, and only one is usually of interest. Local ref, not persisted:
// the modal reopens fresh from an info-icon click, so persisting a per-open toggle would over-index
// on a decision the user makes once per look.
const expandedLevels = ref<Set<string>>(new Set())
const togglePyramid = (vn: string) => {
  const s = new Set(expandedLevels.value)
  s.has(vn) ? s.delete(vn) : s.add(vn)
  expandedLevels.value = s
}
const levelsFor = (vn: string) => storeLevelRows(stores.value.versions[vn])
</script>

<template>
  <BaseModal width="560px" @close="$emit('close')">
    <template #title>
      <i class="pi pi-info-circle" /> Image metadata
    </template>

    <div class="md-body">
      <p class="md-name">{{ img.name }}</p>

      <!-- headline: the original source file this image was converted from -->
      <section class="md-section">
        <h4 class="md-h cc-eyebrow">Original file</h4>
        <div v-if="img.oriPath" class="md-path">
          <code class="md-code">{{ img.oriPath }}</code>
          <button class="md-copy cc-btn cc-btn-bare cc-btn-icon" @click="copy('ori', img.oriPath!)"
            v-tooltip.left="isCopied('ori') ? 'Copied!' : 'Copy path'">
            <i :class="isCopied('ori') ? 'pi pi-check' : 'pi pi-copy'" />
          </button>
        </div>
        <p v-else class="md-none cc-muted">Not recorded — imported before source paths were tracked, or created in-app.</p>
      </section>

      <section class="md-section">
        <h4 class="md-h cc-eyebrow">Identity</h4>
        <div class="md-grid">
          <span class="md-k">UID</span><span class="md-v md-mono">{{ img.uid }}</span>
          <span class="md-k">Status</span><span class="md-v">{{ img.status }}</span>
        </div>
      </section>

      <section class="md-section">
        <h4 class="md-h cc-eyebrow">Dimensions &amp; calibration</h4>
        <div class="md-grid">
          <span class="md-k">Channels (C)</span><span class="md-v">{{ num(img.sizeC) }}</span>
          <span class="md-k">Z-slices (Z)</span><span class="md-v">{{ num(img.sizeZ) }}</span>
          <span class="md-k">Frames (T)</span><span class="md-v">{{ num(img.sizeT) }}</span>
          <span class="md-k">Pixel size X</span><span class="md-v">{{ physical.x }}</span>
          <span class="md-k">Pixel size Y</span><span class="md-v">{{ physical.y }}</span>
          <span class="md-k">Voxel depth Z</span><span class="md-v">{{ physical.z }}</span>
          <span class="md-k">Frame interval</span><span class="md-v">{{ timeStr }}</span>
        </div>
      </section>

      <section v-if="channels.length" class="md-section">
        <h4 class="md-h cc-eyebrow">Channels</h4>
        <ol class="md-chips cc-row cc-row-tight">
          <li v-for="(c, i) in channels" :key="i" class="md-chip">{{ c }}</li>
        </ol>
      </section>

      <section class="md-section">
        <h4 class="md-h cc-eyebrow">Stored files</h4>
        <div class="md-grid">
          <span class="md-k">Active version</span><span class="md-v">{{ img.activeValueName || '—' }}</span>
        </div>
        <!-- Provenance flow: which version was used as INPUT to produce which. Rebuilt from the
             per-image run log (see utils/versionLineage.ts). Only rendered when there is at least
             one edge — for a project with a lone `default` (or a set with no run-log entries) a
             "flow" of one node is noise. Compact tree: each version is one line; a producing task
             sits on the line ABOVE its output as "└─ Drift correct →" so the read order is
             input-then-task-then-output, matching a data-flow diagram left→right/top-down. -->
        <div v-if="hasLineageEdges" class="md-lineage cc-card">
          <ol class="md-flow">
            <li v-for="row in lineageRows" :key="row.node.version"
                class="md-flow-row" :style="{ '--depth': row.depth }">
              <span v-if="row.node.edge" class="md-flow-via cc-muted cc-fs-xs">
                <i class="pi pi-arrow-down md-flow-arrow" />
                {{ funLabel(row.node.edge.fun) }}
              </span>
              <span class="md-flow-vn"
                    :class="{ 'md-flow-active': row.node.version === img.activeValueName }">
                {{ row.node.version }}
                <span v-if="!row.node.edge" class="cc-muted cc-fs-2xs">imported</span>
              </span>
            </li>
          </ol>
        </div>
        <!-- One CARD per stored thing, not one grid of rows. Each version carries six technical facts
             (codec, zarr format, NGFF version, chunk, shard, chunk keys) on top of its name, filename
             and size — as columns that is unreadably dense, and as one appended sentence it is a
             run-on; both were tried. A card gives each entry its own block: what it IS on the head row
             (name · format · size), what it is MADE OF underneath. Sizes still compare down the page —
             every card is full width, so the right-aligned size lands in the same column. -->
        <ul class="md-stores">
          <li v-for="[vn, fn] in versions" :key="'v-' + vn" class="md-store cc-card">
            <div class="md-store-head">
              <span class="md-store-vn">{{ vn }}</span>
              <!-- The format sits with the NAME, not among the facts below: it is what this entry IS,
                   and it is what differs between two versions of one image. It also keeps the facts to
                   a single uncrowded line. NGFF bracketed, and absent when the store declares none. -->
              <span v-if="storeFormatTitle(stores.versions[vn])" class="cc-muted cc-fs-xs">
                {{ storeFormatTitle(stores.versions[vn]) }}
              </span>
              <span class="md-size cc-readout cc-fs-xs">{{ size(stores.versions[vn]) }}</span>
            </div>
            <code class="md-code">{{ fn }}</code>
            <!-- Chunking and codec: answerable here rather than by reading `zarr.json` in a terminal,
                 since v2 and v3 stores coexist permanently (no converter). An em dash when the store
                 could not be read at all — a different answer from "unsharded". -->
            <div v-if="storeFormatFacts(stores.versions[vn]).length" class="cc-row cc-row-loose">
              <span v-for="f in storeFormatFacts(stores.versions[vn])" :key="f.k" class="cc-row-group">
                <span class="cc-muted cc-fs-xs">{{ f.k }}</span>
                <span class="md-fact-v">{{ f.v }}</span>
              </span>
            </div>
            <span v-else class="cc-muted cc-fs-xs">—</span>
            <!-- Pyramid layout: per-level XY shape, XY chunk, and tile grid at that level. Collapsed by
                 default (see `expandedLevels` — one card per stored version could otherwise mean N tables
                 open on the first look). One row is a valid answer for a store with `nscales=1` (drift/AF
                 corrections default to that), which is exactly the state the toggle is here to surface. -->
            <template v-if="levelsFor(vn).length">
              <button class="md-pyr-toggle cc-section-toggle"
                      @click="togglePyramid(vn)"
                      v-tooltip.top="expandedLevels.has(vn) ? 'Hide pyramid layout' : 'Show pyramid layout'">
                <i :class="['pi', expandedLevels.has(vn) ? 'pi-chevron-down' : 'pi-chevron-right']" />
                <span class="cc-eyebrow cc-fs-2xs">Pyramid</span>
                <span class="cc-muted cc-fs-2xs">{{ levelsFor(vn).length }} level{{ levelsFor(vn).length === 1 ? '' : 's' }}</span>
              </button>
              <table v-if="expandedLevels.has(vn)" class="md-pyr">
                <thead>
                  <tr>
                    <th class="cc-muted cc-fs-2xs">level</th>
                    <th class="cc-muted cc-fs-2xs">XY</th>
                    <th class="cc-muted cc-fs-2xs">chunk XY</th>
                    <th class="cc-muted cc-fs-2xs">grid</th>
                  </tr>
                </thead>
                <tbody>
                  <tr v-for="row in levelsFor(vn)" :key="row.level">
                    <td class="md-pyr-lvl">{{ row.level }}</td>
                    <td class="md-fact-v">{{ row.xy }}</td>
                    <td class="md-fact-v">{{ row.chunk }}</td>
                    <td class="md-fact-v">{{ row.grid }}</td>
                  </tr>
                </tbody>
              </table>
            </template>
          </li>
          <!-- A label set's size is the sum of its files (base + nuc). No codec facts: it is written
               with the fixed `labels` compressor, which nothing here asks the user to choose. -->
          <li v-for="[vn, fns] in labels" :key="'l-' + vn" class="md-store cc-card">
            <div class="md-store-head">
              <span class="md-store-vn">labels · {{ vn }}</span>
              <span class="md-size cc-readout cc-fs-xs">{{ size(stores.labels[vn]) }}</span>
            </div>
            <code class="md-code">{{ fns.join(', ') }}</code>
          </li>
        </ul>
      </section>

      <section v-if="attrs.length" class="md-section">
        <h4 class="md-h cc-eyebrow">Attributes</h4>
        <div class="md-grid">
          <template v-for="[k, v] in attrs" :key="'a-' + k">
            <span class="md-k">{{ k }}</span><span class="md-v">{{ v }}</span>
          </template>
        </div>
      </section>

      <section v-if="extra.length" class="md-section">
        <h4 class="md-h cc-eyebrow">Other metadata</h4>
        <div class="md-grid">
          <template v-for="[k, v] in extra" :key="'e-' + k">
            <span class="md-k">{{ k }}</span><span class="md-v">{{ v }}</span>
          </template>
        </div>
      </section>

      <section v-if="img.note" class="md-section">
        <h4 class="md-h cc-eyebrow">Note</h4>
        <p class="md-note-text">{{ img.note }}</p>
      </section>
    </div>
  </BaseModal>
</template>

<style scoped>
/* Shell (overlay/box/header/footer) lives in BaseModal; only dialog-specific styles here. */
.md-body { display: flex; flex-direction: column; gap: 1rem; }   /* padding from BaseModal */
.md-name { margin: 0; font-size: var(--cc-fs-md); font-weight: 600; color: var(--cc-text); }

.md-section { display: flex; flex-direction: column; gap: 0.4rem; }
.md-h { margin: 0; }

.md-grid {
  display: grid; grid-template-columns: max-content 1fr;
  gap: 0.25rem 1rem; font-size: var(--cc-fs-md); align-items: baseline;
}
.md-k { color: var(--cc-text-dim); white-space: nowrap; }
.md-v { color: var(--cc-text); word-break: break-word; }
.md-mono, .md-v.md-mono { font-family: var(--cc-mono); font-size: var(--cc-fs-sm); }

.md-path { display: flex; align-items: flex-start; gap: 0.4rem; }
.md-code {
  flex: 1; min-width: 0; font-family: var(--cc-mono); font-size: var(--cc-fs-sm);
  color: var(--cc-text); background: var(--cc-surface-2); border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-sm); padding: 0.3rem 0.45rem; word-break: break-all;
}
/* .md-copy → cc-btn cc-btn-bare cc-btn-icon */
.md-copy:hover { color: var(--cc-text); background: var(--cc-surface-2); }
.md-none { margin: 0; font-style: italic; }

.md-chips { list-style: none; margin: 0; padding: 0; counter-reset: ch; }
.md-chip {
  font-size: var(--cc-fs-sm); color: var(--cc-text);
  background: var(--cc-surface-2); border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-pill); padding: 0.12rem 0.5rem;
}
.md-chip::before { counter-increment: ch; content: counter(ch) '· '; color: var(--cc-text-dim); }

/* One card per stored version / label set (surface + border + radius from .cc-card). */
.md-stores { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 0.4rem; }
.md-store { display: flex; flex-direction: column; gap: 0.3rem; padding: 0.45rem 0.5rem; }
.md-store-head { display: flex; align-items: baseline; gap: 0.5rem; }
/* The value name is the entry's title — the one thing you scan the list by, so it is text, not dim. */
.md-store-vn { color: var(--cc-text); font-weight: 600; font-size: var(--cc-fs-sm); overflow-wrap: anywhere; }
/* pushed right (rather than space-between, which would centre the format) so the numbers line up down
   the page — the only way sizes compare at a glance */
.md-size { white-space: nowrap; margin-left: auto; }
/* Values in mono: these are shapes and identifiers, and `1×1×1×1024×1024` only lines up in mono. */
.md-fact-v { font-family: var(--cc-mono); font-size: var(--cc-fs-sm); color: var(--cc-text); }

.md-note-text { margin: 0; font-size: var(--cc-fs-md); color: var(--cc-text); white-space: pre-wrap; }

/* Pyramid section — bare inline toggle inside a store card. `.cc-section-toggle` gives us the row
   affordance without CollapsibleSection's panel-bar chrome (which would fight the card border). */
.md-pyr-toggle { padding: 0.15rem 0; gap: 0.4rem; }
.md-pyr {
  border-collapse: collapse; margin-top: 0.15rem;
  font-size: var(--cc-fs-sm);
}
.md-pyr th, .md-pyr td { text-align: left; padding: 0.1rem 0.65rem 0.1rem 0; }
.md-pyr td { font-family: var(--cc-mono); }
.md-pyr th { font-weight: 400; }
.md-pyr-lvl { color: var(--cc-text-dim); font-family: var(--cc-mono); }

/* Version lineage — a compact indented tree. Each row is one stored version; its inbound "via"
   caption sits above it on the same row (arrow + task label). Depth is a CSS variable so nested
   children inherit their parent's indent without a nested <ul>, which would break the top-to-bottom
   read order the tree relies on. */
.md-lineage { padding: 0.45rem 0.6rem; }
.md-flow { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 0.15rem; }
.md-flow-row {
  display: flex; flex-direction: column; gap: 0.05rem;
  padding-left: calc(var(--depth, 0) * 1rem);
}
.md-flow-via { display: inline-flex; align-items: center; gap: 0.3rem; }
/* colour inherits from the .cc-muted wrapper via currentColor */
.md-flow-arrow { font-size: var(--cc-fs-2xs); }
.md-flow-vn { color: var(--cc-text); font-family: var(--cc-mono); font-size: var(--cc-fs-sm); }
.md-flow-active { font-weight: 600; }
</style>
