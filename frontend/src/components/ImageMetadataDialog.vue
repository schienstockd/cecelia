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

const props = defineProps<{ image: CciaImage }>()
defineEmits<{ (e: 'close'): void }>()

const img = computed(() => props.image)
const projectMeta = useProjectMetaStore()

// "12 × 512 × 512" style formatting is overkill here — we show each dimension as its own row so a
// missing one reads as "—" rather than a silently-absent factor.
function num(v: number | null | undefined): string {
  return v === null || v === undefined ? '—' : String(v)
}

const physical = computed(() => {
  const i = img.value
  const unit = i.physicalSizeUnit ?? 'px'
  const fmt = (v: number | null | undefined) => (v === null || v === undefined ? '—' : `${v} ${unit}`)
  return { x: fmt(i.physicalSizeX), y: fmt(i.physicalSizeY), z: fmt(i.physicalSizeZ) }
})

const timeStr = computed(() => {
  const i = img.value
  if (i.timeIncrement === null || i.timeIncrement === undefined) return '—'
  return `${i.timeIncrement} ${i.timeIncrementUnit ?? 's'}`
})

const channels = computed(() => img.value.channelNames?.filter(c => c && c.length) ?? [])
// valueName → filename, active first. The active version is the zarr the app currently reads.
const versions = computed(() => Object.entries(img.value.filepaths ?? {}))

// What each stored version IS on disk: how its pixels are ENCODED, and how much space it takes.
// Fetched rather than stored: the codec is a property of the store (which can be re-landed on a
// different one by rechunk_zarr.py without anything in ccid.json changing), and the size can only be
// known by walking it. Awaited AFTER the modal is on screen, never before — the walk is the expensive
// part (a few hundred ms per store warm, seconds cold on a multi-GB version), so the sizes fill in a
// moment later rather than delaying the dialog. Failure is silent — this dialog is read-only
// information and must still open if a store is missing.
type StoreInfo = { bytes: number, label?: string }
const stores = ref<{ versions: Record<string, StoreInfo | null>, labels: Record<string, StoreInfo> }>(
  { versions: {}, labels: {} })
onMounted(async () => {
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
        <!-- ONE grid for versions + labels: the codec and size columns are as wide as the longest
             entry across all rows, so a row-to-row difference in either can't resize the filename
             box. A label row's size is the sum of its files (base + nuc); it shows no codec. -->
        <div class="md-files">
          <template v-for="[vn, fn] in versions" :key="'v-' + vn">
            <span class="md-file-vn cc-muted">{{ vn }}</span>
            <code class="md-code">{{ fn }}</code>
            <span class="md-codec cc-muted cc-fs-xs">{{ stores.versions[vn]?.label ?? '—' }}</span>
            <span class="md-size cc-muted cc-fs-xs">{{ size(stores.versions[vn]) }}</span>
          </template>
          <template v-for="[vn, fns] in labels" :key="'l-' + vn">
            <span class="md-file-vn cc-muted">labels · {{ vn }}</span>
            <code class="md-code">{{ fns.join(', ') }}</code>
            <span class="md-codec" />
            <span class="md-size cc-muted cc-fs-xs">{{ size(stores.labels[vn]) }}</span>
          </template>
        </div>
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

/* Version column is 9rem, not 6rem: `temporalSmoothed` (16 chars) wrapped to two lines at 6rem, which
   misaligned it against its own filename box. Fixed rather than `max-content` on purpose — the labels
   rows below render "labels · {vn}" and a long label set would otherwise squeeze the filename. Long
   names still wrap; 9rem just moves the threshold past the value names we actually produce. */
.md-files {
  display: grid; grid-template-columns: 9rem minmax(0, 1fr) max-content max-content;
  gap: 0.25rem 0.5rem; align-items: baseline;
}
.md-codec { white-space: nowrap; }
/* right-aligned so the numbers line up column-wise, which is the only way sizes compare at a glance */
.md-size { white-space: nowrap; text-align: right; font-variant-numeric: tabular-nums; }
.md-file-vn { overflow-wrap: anywhere; }

.md-note-text { margin: 0; font-size: var(--cc-fs-md); color: var(--cc-text); white-space: pre-wrap; }
</style>
