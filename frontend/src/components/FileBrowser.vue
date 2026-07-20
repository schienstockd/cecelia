<!--
  File browser modal. Calls Julia /api/fs/list to navigate the server filesystem.
  Emits 'select' with an array of ABSOLUTE paths when the user confirms. Navigates the whole
  filesystem (home + mount shortcuts) so mounted network drives / external storage are reachable.
-->
<script setup lang="ts">
import { ref, computed } from 'vue'
import BaseModal from './BaseModal.vue'
import { useLogStore } from '../stores/log'
import { fsBreadcrumbs } from '../utils/fsPath'

const emit = defineEmits<{
  (e: 'select', paths: string[]): void
  (e: 'close'): void
}>()

interface FsEntry {
  name: string
  path: string     // absolute path on the server
  isdir: boolean
  isimage: boolean
  ext: string
  size: number | null
}

interface FsShortcut { label: string; path: string }

function formatSize(bytes: number | null): string {
  if (bytes === null || bytes === undefined) return ''
  if (bytes < 1024) return `${bytes} B`
  if (bytes < 1024 ** 2) return `${(bytes / 1024).toFixed(1)} KB`
  if (bytes < 1024 ** 3) return `${(bytes / 1024 ** 2).toFixed(1)} MB`
  return `${(bytes / 1024 ** 3).toFixed(2)} GB`
}

interface FsListing {
  root: string
  current: string
  parent: string | null
  shortcuts: FsShortcut[]
  entries: FsEntry[]
}

const log      = useLogStore()
const listing  = ref<FsListing | null>(null)
const loading  = ref(false)
const selected = ref<Set<string>>(new Set())
const error    = ref<string | null>(null)

async function navigate(path: string) {
  loading.value = true
  error.value   = null
  selected.value = new Set()
  try {
    const res = await fetch(`/api/fs/list?path=${encodeURIComponent(path)}`)
    if (!res.ok) {
      const body = await res.json().catch(() => ({}))
      throw new Error(body.error ?? `HTTP ${res.status}`)
    }
    listing.value = await res.json() as FsListing
  } catch (e: unknown) {
    const msg = e instanceof Error ? e.message : String(e)
    error.value = msg
    log.error(`File browser: ${msg}`, { source: 'import' })
  } finally {
    loading.value = false
  }
}

// open at root on mount
navigate('')

function toggleSelect(entry: FsEntry) {
  if (!entry.isimage) return
  if (selected.value.has(entry.path)) selected.value.delete(entry.path)
  else selected.value.add(entry.path)
  selected.value = new Set(selected.value)
}

function selectAll() {
  const images = listing.value?.entries.filter(e => e.isimage) ?? []
  if (selected.value.size === images.length)
    selected.value = new Set()
  else
    selected.value = new Set(images.map(e => e.path))
}

function confirm() {
  if (selected.value.size === 0) return
  emit('select', [...selected.value])
}

const imageEntries  = computed(() => listing.value?.entries.filter(e => e.isimage) ?? [])
const allSelected   = computed(() =>
  imageEntries.value.length > 0 && selected.value.size === imageEntries.value.length
)
const someSelected  = computed(() =>
  selected.value.size > 0 && selected.value.size < imageEntries.value.length
)
const breadcrumbs = computed(() => fsBreadcrumbs(listing.value?.current ?? ''))
const shortcuts   = computed(() => listing.value?.shortcuts ?? [])
</script>

<template>
  <BaseModal title="Select images" width="680px" @close="$emit('close')">

    <!-- shortcuts + breadcrumbs -->
    <template #toolbar>
      <div v-if="shortcuts.length" class="fb-shortcuts">
        <i class="pi pi-bookmark fb-sc-icon" />
        <button v-for="sc in shortcuts" :key="sc.path" class="fb-shortcut"
          @click="navigate(sc.path)"
          v-tooltip.bottom="`Go to ${sc.path}`">
          {{ sc.label }}
        </button>
      </div>
      <div class="fb-breadcrumbs">
        <template v-for="(crumb, i) in breadcrumbs" :key="crumb.path">
          <span v-if="i > 0" class="crumb-sep">/</span>
          <button class="crumb" @click="navigate(crumb.path)"
            v-tooltip.bottom="`Navigate to ${crumb.label}`">
            {{ crumb.label }}
          </button>
        </template>
      </div>
    </template>

      <!-- body -->
      <div class="fb-body">
        <div v-if="loading" class="fb-state">
          <i class="pi pi-spin pi-cog" /> Loading…
        </div>

        <div v-else-if="error" class="fb-state error">
          <i class="pi pi-exclamation-triangle" /> {{ error }}
          <button class="btn-ghost btn-sm" @click="navigate('')">Back to home</button>
        </div>

        <table v-else class="fb-table">
          <thead>
            <tr>
              <th class="col-chk">
                <input type="checkbox"
                  :checked="allSelected"
                  :indeterminate="someSelected"
                  @change="selectAll"
                  :disabled="imageEntries.length === 0"
                  v-tooltip.right="'Select all image files in this directory.'"
                />
              </th>
              <th class="col-name">Name</th>
              <th class="col-type">Type</th>
              <th class="col-size">Size</th>
            </tr>
          </thead>
          <tbody>
            <!-- parent directory -->
            <tr v-if="listing?.parent !== null && listing?.parent !== undefined"
              class="fb-row dir-row"
              @click="navigate(listing!.parent!)">
              <td class="col-chk" />
              <td class="col-name">
                <i class="pi pi-arrow-up row-icon" />
                <span class="row-name">..</span>
              </td>
              <td class="col-type dim">parent</td>
              <td class="col-size" />
            </tr>

            <!-- entries -->
            <tr
              v-for="entry in listing?.entries"
              :key="entry.path"
              class="fb-row"
              :class="{
                'dir-row':      entry.isdir,
                'image-row':    entry.isimage,
                'row-selected': selected.has(entry.path),
                'non-image':    !entry.isdir && !entry.isimage,
              }"
              @click="entry.isdir ? navigate(entry.path) : toggleSelect(entry)"
              v-tooltip.right="entry.isdir
                ? `Open folder ${entry.name}`
                : entry.isimage
                  ? `Select ${entry.name}`
                  : `${entry.ext} files are not supported`"
            >
              <td class="col-chk" @click.stop>
                <input v-if="entry.isimage"
                  type="checkbox"
                  :checked="selected.has(entry.path)"
                  @change="toggleSelect(entry)"
                />
              </td>
              <td class="col-name">
                <i :class="[
                  'pi row-icon',
                  entry.isdir ? 'pi-folder' : entry.isimage ? 'pi-file' : 'pi-file-o'
                ]" />
                <span class="row-name">{{ entry.name }}</span>
              </td>
              <td class="col-type dim">{{ entry.isdir ? 'folder' : entry.ext }}</td>
              <td class="col-size dim">{{ entry.isdir ? '' : formatSize(entry.size) }}</td>
            </tr>

            <tr v-if="listing && listing.entries.length === 0">
              <td colspan="3" class="fb-empty">Empty directory</td>
            </tr>
          </tbody>
        </table>
      </div>

    <template #footer>
      <span class="sel-count" v-if="selected.size > 0">
        {{ selected.size }} file{{ selected.size > 1 ? 's' : '' }} selected
      </span>
      <span class="sel-count dim" v-else>No files selected</span>

      <div class="footer-actions">
        <button class="btn-ghost btn-sm" @click="$emit('close')"
          v-tooltip.top="'Cancel and close the file browser.'">
          Cancel
        </button>
        <button class="btn-primary btn-sm"
          :disabled="selected.size === 0"
          @click="confirm"
          v-tooltip.top="selected.size > 0
            ? `Add ${selected.size} selected file(s) to the set.`
            : 'Select at least one image file first.'">
          <i class="pi pi-plus" />
          Add {{ selected.size > 0 ? selected.size : '' }} to set
        </button>
      </div>
    </template>
  </BaseModal>
</template>

<style scoped>
/* Shell (overlay/box/header/footer) lives in BaseModal; only browser-specific styles remain here. */

/* breadcrumbs */
.fb-breadcrumbs {
  display: flex; align-items: center; gap: 0.15rem;
  padding: 0.4rem 1rem;
  border-bottom: 1px solid var(--cc-border);
  font-size: 0.75rem;
  background: var(--cc-surface-1);
  flex-shrink: 0;
  flex-wrap: wrap;
}
/* shortcuts (home + mount points) */
.fb-shortcuts {
  display: flex; align-items: center; gap: 0.3rem; flex-wrap: wrap;
  padding: 0.4rem 1rem;
  border-bottom: 1px solid var(--cc-border);
  background: var(--cc-surface-1);
}
.fb-sc-icon { font-size: 0.7rem; color: var(--cc-text-dim); margin-right: 0.1rem; }
.fb-shortcut {
  background: var(--cc-surface-2); border: 1px solid var(--cc-border); cursor: pointer;
  color: var(--cc-text); font-size: 0.72rem;
  padding: 0.12rem 0.5rem; border-radius: 0.9rem;
}
.fb-shortcut:hover { border-color: var(--cc-accent); color: var(--cc-accent); }

.crumb-sep { color: var(--cc-text-dim); }
.crumb {
  background: none; border: none; cursor: pointer;
  color: var(--cc-accent); font-size: 0.75rem;
  padding: 0.1rem 0.25rem; border-radius: 0.2rem;
}
.crumb:hover { background: var(--cc-surface-2); }

/* body — BaseModal's cc-modal-body owns the scroll; .fb-body needs no shell styling. */

.fb-state {
  display: flex; align-items: center; gap: 0.5rem;
  padding: 2rem; color: var(--cc-text-dim); font-size: 0.82rem;
}
.fb-state.error { color: #fca5a5; flex-direction: column; align-items: flex-start; }

/* table */
.fb-table { width: 100%; border-collapse: collapse; font-size: 0.82rem; }
.fb-table thead th {
  text-align: left; font-size: 0.68rem; font-weight: 600;
  text-transform: uppercase; letter-spacing: 0.06em;
  color: var(--cc-text-dim);
  padding: 0.4rem 0.75rem;
  border-bottom: 1px solid var(--cc-border);
  position: sticky; top: 0;
  background: var(--cc-surface-1);
}
.col-chk  { width: 36px; }
.col-name { flex: 1; }
.col-type { width: 70px; }
.col-size { width: 80px; text-align: right; font-variant-numeric: tabular-nums; }

.fb-row {
  border-bottom: 1px solid var(--cc-border);
  cursor: pointer;
  transition: background 0.1s;
}
.fb-row td { padding: 0.4rem 0.75rem; vertical-align: middle; }
.fb-row:hover { background: var(--cc-surface-2); }
.dir-row  { color: var(--cc-text); }
.image-row { color: var(--cc-text); }
.non-image { opacity: 0.4; cursor: default; }
.row-selected { background: #a78bfa14; }

.row-icon { font-size: 0.8rem; margin-right: 0.4rem; }
.dir-row  .row-icon { color: #fcd34d; }
.image-row .row-icon { color: var(--cc-accent); }

.col-name { display: table-cell; }
.row-name { vertical-align: middle; }
.dim { color: var(--cc-text-dim); font-size: 0.75rem; }

.fb-empty { text-align: center; padding: 2rem; color: var(--cc-text-dim); font-size: 0.8rem; }

/* footer */
.sel-count { font-size: 0.78rem; color: var(--cc-text); flex: 1; }
.footer-actions { display: flex; gap: 0.4rem; }

.btn-sm {
  display: flex; align-items: center; gap: 0.3rem;
  font-size: 0.78rem; font-weight: 500;
  padding: 0.35rem 0.75rem;
  border-radius: 0.35rem; border: 1px solid transparent;
  cursor: pointer;
}
.btn-ghost { background: var(--cc-surface-2); border-color: var(--cc-border); color: var(--cc-text-dim); }
.btn-ghost:hover { color: var(--cc-text); }
.btn-primary { background: var(--cc-accent); color: #fff; }
.btn-primary:hover:not(:disabled) { filter: brightness(1.1); }
.btn-primary:disabled { opacity: 0.35; cursor: not-allowed; background: var(--cc-surface-2); color: var(--cc-text-dim); }
</style>
