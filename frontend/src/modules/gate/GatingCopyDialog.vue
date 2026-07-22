<!--
  Copy the current gating strategy (ONE gating pop_type — flow=cells or track=tracks) from the open
  image to other images in the same set. Gates are a JSON copy (server /api/gating/copy REPLACES each
  target's gating sidecar); membership recomputes per image. Optionally also copy the canvas plot
  layout (client-side, via the canvas store). A precheck (imagesWithValueName → /api/images/value-name-check)
  flags images lacking the segmentation — those are shown separately and never copied to. Ports R "Propagate to Selected".
-->
<script setup lang="ts">
import { ref, computed, onMounted } from 'vue'
import BaseModal from '../../components/BaseModal.vue'
import { useProjectStore } from '../../stores/project'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useCanvasPanelsStore } from '../../stores/canvasPanels'
import { useLogStore } from '../../stores/log'
import { imagesWithValueName } from '../../lib/valueNames'

const props = defineProps<{
  setUid: string
  sourceUid: string
  valueName: string
  popType: string          // 'flow' | 'track' (a gating pop type)
}>()
const emit = defineEmits<{ (e: 'close'): void }>()

const project     = useProjectStore()
const projectMeta = useProjectMetaStore()
const canvas      = useCanvasPanelsStore()
const log         = useLogStore()

// other images in the same set (never the source itself)
const siblings = computed(() =>
  (project.sets.find(s => s.uid === props.setUid)?.images ?? []).filter(i => i.uid !== props.sourceUid))

// precheck: which siblings have the segmentation (copyable) vs not (flagged, never copied to)
const checking = ref(true)
const availableUids = ref<Set<string>>(new Set())
const canCopy = (uid: string) => availableUids.value.has(uid)
const available = computed(() => siblings.value.filter(i => canCopy(i.uid)))
const missing   = computed(() => siblings.value.filter(i => !canCopy(i.uid)))

onMounted(async () => {
  const projectUid = projectMeta.current?.uid
  const uids = siblings.value.map(i => i.uid)
  if (!projectUid || !uids.length) { checking.value = false; return }
  const { available } = await imagesWithValueName(projectUid, props.valueName, uids)
  availableUids.value = new Set(available)
  checking.value = false
})

const picked = ref<Set<string>>(new Set())
const copyLayout = ref(true)
const busy = ref(false)

function toggle(uid: string) { picked.value.has(uid) ? picked.value.delete(uid) : picked.value.add(uid); picked.value = new Set(picked.value) }
const allPicked = computed(() => available.value.length > 0 && available.value.every(i => picked.value.has(i.uid)))
function toggleAll() { picked.value = allPicked.value ? new Set() : new Set(available.value.map(i => i.uid)) }

const label = computed(() => props.popType === 'track' ? 'track gating' : 'cell gating')
const ckey = (uid: string) => `gate:${props.popType}:${uid}:${props.valueName}`

async function copy() {
  const toImageUids = [...picked.value].filter(canCopy)
  if (!toImageUids.length) { log.warn('Select at least one image.', { source: 'gating' }); return }
  const projectUid = projectMeta.current?.uid
  if (!projectUid) return
  busy.value = true
  try {
    const res = await fetch('/api/gating/copy', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, imageUid: props.sourceUid, valueName: props.valueName,
                             popType: props.popType, toImageUids }),
    })
    const d = await res.json().catch(() => ({})) as { copied?: string[]; error?: string }
    if (!res.ok) throw new Error(d.error ?? `HTTP ${res.status}`)
    const copied = d.copied ?? []
    if (copyLayout.value) for (const uid of copied) canvas.copyEntry(ckey(props.sourceUid), ckey(uid))
    log.info(`Copied ${label.value} to ${copied.length} image(s)` + (copyLayout.value ? ' (incl. plot layout)' : '') + '.', { source: 'gating' })
    emit('close')
  } catch (e) {
    log.error(`Copy gating: ${e instanceof Error ? e.message : String(e)}`, { source: 'gating' })
  } finally { busy.value = false }
}
</script>

<template>
  <BaseModal width="420px" @close="emit('close')">
    <template #title><i class="pi pi-copy" /> Copy {{ label }} to images</template>

    <div class="cg-body">
      <p class="cg-warn"><i class="pi pi-exclamation-triangle" />
        Overwrites the {{ label }} on each selected image ({{ valueName }}) — no undo.</p>

      <p v-if="checking" class="cg-empty">Checking images…</p>
      <template v-else>
        <p v-if="!siblings.length" class="cg-empty">No other images in this set.</p>

        <template v-if="available.length">
          <label class="cg-all"><input type="checkbox" :checked="allPicked" @change="toggleAll" /> Select all</label>
          <div class="cg-list">
            <label v-for="i in available" :key="i.uid" class="cg-item">
              <input type="checkbox" :checked="picked.has(i.uid)" @change="toggle(i.uid)" />
              <span class="cg-name">{{ i.name }}</span>
            </label>
          </div>
          <label class="cg-layout" v-tooltip.top="'Also copy the plot layout (plots, channels, parents).'">
            <input type="checkbox" v-model="copyLayout" /> Also copy plot layout
          </label>
        </template>
        <p v-else-if="siblings.length" class="cg-empty">No images have the “{{ valueName }}” segmentation.</p>

        <!-- flagged: no such segmentation → never copied to -->
        <div v-if="missing.length" class="cg-missing">
          <span class="cg-missing-head"><i class="pi pi-ban" /> No “{{ valueName }}” segmentation — skipped:</span>
          <span class="cg-missing-names">{{ missing.map(i => i.name).join(', ') }}</span>
        </div>
      </template>
    </div>

    <template #footer>
      <button class="btn-ghost btn-sm" @click="emit('close')">Cancel</button>
      <button class="btn-primary btn-sm" :disabled="busy || checking || !picked.size" @click="copy">
        <i v-if="busy" class="pi pi-spin pi-cog" /><i v-else class="pi pi-copy" />
        Copy to {{ picked.size }} image{{ picked.size === 1 ? '' : 's' }}
      </button>
    </template>
  </BaseModal>
</template>

<style scoped>
.cg-body { display: flex; flex-direction: column; gap: 0.7rem; }   /* padding from BaseModal */
.cg-warn { display: flex; align-items: center; gap: 0.4rem; margin: 0; font-size: 0.78rem; color: #fcd34d;
  background: #7c2d1244; border: 1px solid #92400e55; border-radius: 0.3rem; padding: 0.4rem 0.55rem; line-height: 1.35; }
.cg-empty { margin: 0; font-size: 0.8rem; color: var(--cc-text-dim); }
.cg-all { font-size: 0.75rem; color: var(--cc-text-dim); display: flex; align-items: center; gap: 0.4rem; cursor: pointer; }
.cg-list { display: flex; flex-direction: column; gap: 0.15rem; max-height: 240px; overflow: auto;
  border: 1px solid var(--cc-border); border-radius: 0.35rem; padding: 0.4rem 0.5rem; }
.cg-item { display: flex; align-items: center; gap: 0.5rem; font-size: 0.82rem; color: var(--cc-text); cursor: pointer; padding: 0.15rem 0; }
.cg-item input, .cg-all input, .cg-layout input { accent-color: var(--cc-accent); cursor: pointer; }
.cg-name { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.cg-layout { display: flex; align-items: center; gap: 0.4rem; font-size: 0.8rem; color: var(--cc-text); cursor: pointer; }
.cg-missing { display: flex; flex-direction: column; gap: 0.2rem; font-size: 0.72rem; color: var(--cc-text-dim);
  border-top: 1px solid var(--cc-border); padding-top: 0.5rem; }
.cg-missing-head { display: flex; align-items: center; gap: 0.35rem; color: #f59e0b; }
.cg-missing-names { padding-left: 1.1rem; word-break: break-word; }

.btn-sm { display: flex; align-items: center; gap: 0.3rem; font-size: 0.78rem; font-weight: 500;
  padding: 0.35rem 0.75rem; border-radius: 0.35rem; border: 1px solid transparent; cursor: pointer; white-space: nowrap; }
.btn-ghost { background: var(--cc-surface-2); border-color: var(--cc-border); color: var(--cc-text-dim); }
.btn-ghost:hover:not(:disabled) { color: var(--cc-text); }
.btn-primary { background: var(--cc-accent); color: #fff; }
.btn-primary:hover:not(:disabled) { filter: brightness(1.1); }
.btn-primary:disabled { opacity: 0.4; cursor: not-allowed; }
</style>
