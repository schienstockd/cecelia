<script setup lang="ts">
// Preview + register step for migrating an old R/Shiny cecelia project into the current set.
// Point at a legacy project dir → scan (read-only) → a table that states plainly, per image, what
// WILL transfer (image · segmentation · tracking) and what will NOT (clustering · gating · HMM,
// re-run in the app) → register the ticked images (UIDs preserved). The actual data transfer is the
// `importImages.migrateLegacy` task, run from the TaskRunner afterwards. See
// docs/todo/LEGACY_MIGRATION_PLAN.md.
import { ref, computed } from 'vue'
import BaseModal from './BaseModal.vue'

const props = defineProps<{ projectUid: string; setUid: string }>()
const emit = defineEmits<{ (e: 'imported', images: unknown[]): void; (e: 'close'): void }>()

interface LegacyImage {
  uid: string; name: string; kind: string
  size: Record<string, number>
  image_variant: string | null
  segmentation: string[]
  tracking: Record<string, number>
  excluded: { clustering: boolean; gating: boolean; hmm: boolean }
  warnings: string[]
}
interface Manifest {
  project_uid: string; n_images: number; images: LegacyImage[]
  transfers: string[]; not_transferred: string[]; error?: string; note?: string
}

const path = ref('')
const rscript = ref('')          // optional: path to Rscript if R isn't on the server's PATH
const scanning = ref(false)
const importing = ref(false)
const error = ref('')
const manifest = ref<Manifest | null>(null)
const selected = ref<Set<string>>(new Set())
const done = ref(0)              // >0 after a successful import → show the next-step panel

const nTracks = (im: LegacyImage) => Object.values(im.tracking).reduce((a, b) => a + b, 0)
const isTracked = (im: LegacyImage) => Object.keys(im.tracking).length > 0
const excludedList = (im: LegacyImage) =>
  [im.excluded.clustering && 'clustering', im.excluded.gating && 'gating', im.excluded.hmm && 'HMM']
    .filter(Boolean) as string[]

const summary = computed(() => {
  const imgs = manifest.value?.images ?? []
  const seg = imgs.filter(i => i.segmentation.length).length
  const tracked = imgs.filter(isTracked).length
  return { total: imgs.length, seg, tracked, picked: selected.value.size }
})

async function scan() {
  error.value = ''
  manifest.value = null
  if (!path.value.trim()) { error.value = 'Enter the path to a legacy project folder.'; return }
  scanning.value = true
  try {
    const res = await fetch('/api/import/scan-legacy', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        sourceProjectDir: path.value.trim(),
        ...(rscript.value.trim() ? { rscript: rscript.value.trim() } : {}),
      }),
    })
    const body = await res.json().catch(() => ({})) as Manifest
    if (!res.ok || body.error) throw new Error(body.error ?? `HTTP ${res.status}`)
    manifest.value = body
    selected.value = new Set(body.images.map(i => i.uid))   // all ticked by default
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  } finally {
    scanning.value = false
  }
}

function toggle(uid: string) {
  const s = new Set(selected.value)
  s.has(uid) ? s.delete(uid) : s.add(uid)
  selected.value = s
}

const allSelected = computed(() => {
  const n = manifest.value?.images.length ?? 0
  return n > 0 && selected.value.size === n
})
const someSelected = computed(() => {
  const n = manifest.value?.images.length ?? 0
  return selected.value.size > 0 && selected.value.size < n
})
function toggleAll() {
  const imgs = manifest.value?.images ?? []
  selected.value = allSelected.value ? new Set() : new Set(imgs.map(i => i.uid))
}

async function confirmImport() {
  const m = manifest.value
  if (!m || selected.value.size === 0) return
  importing.value = true
  error.value = ''
  try {
    const images = m.images.filter(i => selected.value.has(i.uid))
      .map(i => ({ uid: i.uid, name: i.name, kind: i.kind }))
    const res = await fetch('/api/import/register-legacy', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        projectUid: props.projectUid, setUid: props.setUid,
        sourceProjectDir: path.value.trim(), images,
        ...(rscript.value.trim() ? { rscript: rscript.value.trim() } : {}),
      }),
    })
    const body = await res.json().catch(() => ({})) as { images?: unknown[]; error?: string }
    if (!res.ok || body.error) throw new Error(body.error ?? `HTTP ${res.status}`)
    done.value = (body.images ?? images).length
    emit('imported', body.images ?? [])   // parent adds to the set; dialog stays open to show next step
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  } finally {
    importing.value = false
  }
}
</script>

<template>
  <BaseModal title="Migrate legacy project" width="820px" @close="$emit('close')">
    <!-- success / next-step panel -->
    <div v-if="done" class="lm lm-donebox">
      <p class="lm-doneline"><i class="pi pi-check-circle" /> Added <strong>{{ done }}</strong>
        image{{ done === 1 ? '' : 's' }} to the set.</p>
      <p class="lm-lead">
        These are placeholders — no data has moved yet. To transfer the images, segmentation and
        tracking, select them and run the <strong>“Migrate legacy image”</strong> task from the task
        panel.
      </p>
    </div>

    <div v-else class="lm">
      <p class="lm-lead">
        Import images, segmentation &amp; tracking from an old (R/Shiny) cecelia project.
        <strong>Clustering, gating and HMM aren’t transferred.</strong> The source is never modified.
      </p>

      <div class="lm-path">
        <input
          v-model="path"
          class="lm-input"
          placeholder="/path/to/old/cecelia/projects/&lt;PROJECT&gt;"
          @keyup.enter="scan"
        />
        <button class="cc-btn cc-btn-primary" :disabled="scanning" @click="scan">
          <i :class="scanning ? 'pi pi-spin pi-spinner' : 'pi pi-search'" /> Scan
        </button>
      </div>
      <input
        v-model="rscript"
        class="lm-input lm-rscript"
        placeholder="Rscript path (optional — only if R isn't found, e.g. /usr/bin/Rscript or your renv Rscript)"
      />

      <p v-if="error" class="lm-error"><i class="pi pi-exclamation-triangle" /> {{ error }}</p>

      <p v-if="manifest && !manifest.images.length && manifest.note" class="lm-warn">
        <i class="pi pi-info-circle" /> {{ manifest.note }}
      </p>

      <template v-if="manifest && manifest.images.length">
        <p class="lm-summary">
          <strong>{{ summary.total }}</strong> images —
          <strong>{{ summary.seg }}</strong> with segmentation,
          <strong>{{ summary.tracked }}</strong> tracked.
          Selected <strong>{{ summary.picked }}</strong> to import.
        </p>

        <div class="lm-tablewrap">
          <table class="lm-table">
            <thead>
              <tr>
                <th>
                  <input
                    type="checkbox"
                    :checked="allSelected"
                    :indeterminate.prop="someSelected"
                    @change="toggleAll"
                    v-tooltip.right="'Select all / none'"
                  />
                </th>
                <th>Image</th><th>Size</th>
                <th>Transfers</th><th>Not transferred</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="im in manifest.images" :key="im.uid" :class="{ off: !selected.has(im.uid) }">
                <td><input type="checkbox" :checked="selected.has(im.uid)" @change="toggle(im.uid)" /></td>
                <td>
                  <div class="lm-name">{{ im.name }}</div>
                  <div class="cc-muted">{{ im.uid }} · {{ im.kind }}</div>
                  <div v-for="w in im.warnings" :key="w" class="lm-warn"><i class="pi pi-info-circle" /> {{ w }}</div>
                </td>
                <td class="cc-muted">
                  <span v-if="im.size.SizeC">{{ im.size.SizeC }}c</span>
                  <span v-if="im.size.SizeZ && im.size.SizeZ > 1"> · {{ im.size.SizeZ }}z</span>
                  <span v-if="im.size.SizeT && im.size.SizeT > 1"> · {{ im.size.SizeT }}t</span>
                </td>
                <td>
                  <span class="lm-tag ok">image</span>
                  <span v-for="s in im.segmentation" :key="s" class="lm-tag ok">seg: {{ s }}</span>
                  <span v-if="isTracked(im)" class="lm-tag ok">tracking: {{ nTracks(im) }}</span>
                  <span v-if="!im.segmentation.length" class="cc-muted">image only</span>
                </td>
                <td>
                  <span v-for="x in excludedList(im)" :key="x" class="lm-tag off-tag">{{ x }}</span>
                  <span v-if="!excludedList(im).length" class="cc-muted">—</span>
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      </template>
    </div>

    <template #footer>
      <button v-if="done" class="cc-btn cc-btn-primary" @click="$emit('close')">Done</button>
      <template v-else>
        <button class="cc-btn" @click="$emit('close')">Cancel</button>
        <button
          v-if="manifest && manifest.images.length"
          class="cc-btn cc-btn-primary"
          :disabled="importing || selected.size === 0"
          @click="confirmImport"
        >
          <i :class="importing ? 'pi pi-spin pi-spinner' : 'pi pi-download'" />
          Import {{ selected.size }} image{{ selected.size === 1 ? '' : 's' }}
        </button>
      </template>
    </template>
  </BaseModal>
</template>

<style scoped>
.lm { display: flex; flex-direction: column; gap: 0.75rem; color: var(--cc-text); }
.lm-lead { margin: 0; font-size: 0.9rem; color: var(--cc-text-dim); }
.lm-donebox { padding: 0.5rem 0.25rem; }
.lm-doneline { margin: 0 0 0.4rem; font-size: 1rem; color: var(--cc-text); }
.lm-doneline .pi-check-circle { color: #4ade80; margin-right: 0.35rem; }
.lm-path { display: flex; gap: 0.5rem; }
.lm-input {
  flex: 1; padding: 0.4rem 0.6rem; font-size: 0.9rem;
  border: 1px solid var(--cc-border); border-radius: 6px;
  background: var(--cc-surface-1); color: var(--cc-text);
}
.lm-input::placeholder { color: var(--cc-text-dim); }
.lm-rscript { font-size: 0.8rem; }
.lm-error { color: #f87171; margin: 0; font-size: 0.85rem; }
.lm-summary { margin: 0; font-size: 0.9rem; color: var(--cc-text); }
.lm-tablewrap { max-height: 46vh; overflow: auto; border: 1px solid var(--cc-border); border-radius: 6px; }
.lm-table { width: 100%; border-collapse: collapse; font-size: 0.85rem; color: var(--cc-text); }
.lm-table th, .lm-table td { text-align: left; padding: 0.4rem 0.6rem; border-bottom: 1px solid var(--cc-border); vertical-align: top; }
.lm-table thead th { position: sticky; top: 0; background: var(--cc-surface-1); font-weight: 600; color: var(--cc-text-dim); }
.lm-table tr.off { opacity: 0.45; }
.lm-name { font-weight: 500; }
.cc-muted { color: var(--cc-text-dim); font-size: 0.78rem; }
.lm-warn { color: #fbbf24; font-size: 0.78rem; margin-top: 0.15rem; }
.lm-tag { display: inline-block; margin: 0.1rem 0.2rem 0.1rem 0; padding: 0.05rem 0.4rem; border-radius: 4px; font-size: 0.76rem; }
.lm-tag.ok { background: #16653433; color: #4ade80; }
.lm-tag.off-tag { background: var(--cc-surface-2); color: var(--cc-text-dim); text-decoration: line-through; }
</style>
