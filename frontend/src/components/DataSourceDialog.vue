<script setup lang="ts">
// Where the set's images came from: the source the whole set carries, or the selected images' own
// (an override, e.g. a set mixing two datasets). An import suggestion (a repository ID found in the
// path) waits here to be accepted. Logic in utils/dataSource.ts; docs/todo/DATA_SOURCE_PLAN.md.
import { computed, onMounted, ref, watch } from 'vue'
import BaseModal from './BaseModal.vue'
import ChipSelect, { type ChipOption } from './ChipSelect.vue'
import InlineNote from './InlineNote.vue'
import { formFrom, formIsEmpty, pendingSuggestion, sourcePayload,
         type SourceForm, type SourceReply } from '../utils/dataSource'
import { fetchDataSource, saveDataSource } from '../utils/dataSourceApi'
import { useLogStore } from '../stores/log'

const props = defineProps<{ projectUid: string; setUid: string; selectedUids: string[] }>()
const emit = defineEmits<{ (e: 'close'): void; (e: 'saved'): void }>()
const log = useLogStore()

const reply = ref<SourceReply | null>(null)
const busy = ref(false)
const scope = ref<'set' | 'images'>('set')
const form = ref<SourceForm>(formFrom(null))

const SCOPES = computed<ChipOption[]>(() => [
  { value: 'set', label: 'Whole set', tip: 'Every image in the set, unless it has its own' },
  { value: 'images', label: `Selected (${props.selectedUids.length})`, tip: 'Only the selected images',
    disabled: props.selectedUids.length === 0 },
])

// the source being edited: the set's, or the first selected image's own
const base = computed(() => {
  if (!reply.value) return null
  if (scope.value === 'set') return reply.value.source
  return reply.value.images.find(i => i.uid === props.selectedUids[0])?.own ?? null
})
const suggestion = computed(() => pendingSuggestion(reply.value))

async function load() {
  try {
    reply.value = await fetchDataSource(props.projectUid, props.setUid)
    form.value = formFrom(base.value ?? suggestion.value?.source)
  } catch (e) { log.error(`Source: ${(e as Error).message}`, { source: 'metadata' }) }
}
onMounted(load)
watch(scope, () => { form.value = formFrom(base.value) })

async function write(source: ReturnType<typeof sourcePayload>, uids: string[] = []) {
  busy.value = true
  try {
    await saveDataSource(props.projectUid, props.setUid, source, uids)
    emit('saved')
  } catch (e) { log.error(`Source: ${(e as Error).message}`, { source: 'metadata' }); throw e }
  finally { busy.value = false }
}

/** Accept the import suggestion for the whole set, and drop the per-image copies it was stored as. */
async function acceptSuggestion() {
  const s = suggestion.value
  if (!s) return
  await write(sourcePayload(formFrom(s.source), s.source))
  await write(null, s.uids)
  await load()
}

async function save() {
  await write(sourcePayload(form.value, base.value), scope.value === 'images' ? props.selectedUids : [])
  emit('close')
}
async function clear() {
  await write(null, scope.value === 'images' ? props.selectedUids : [])
  emit('close')
}
</script>

<template>
  <BaseModal title="Source" icon="pi-book" width="520px" @close="emit('close')">
    <div class="ds-body">
      <ChipSelect v-model="scope" :options="SCOPES" variant="segmented" aria-label="Applies to" />
      <div v-if="suggestion && scope === 'set'" class="ds-suggest">
        <InlineNote :short="`From the import path: ${suggestion.source.citation}`"
                    :detail="`Found a ${suggestion.source.repository} ID in ${suggestion.uids.length} image path(s)`" />
        <button class="cc-btn cc-btn-ghost" :disabled="busy" v-tooltip.bottom="'Use it for the whole set'"
                @click="acceptSuggestion">Accept</button>
      </div>
      <label class="ds-field"><span class="cc-muted cc-fs-xs">Citation</span>
        <input v-model="form.citation" class="field-input" placeholder="e.g. repository, dataset ID and lab"
               v-tooltip.bottom="'How the data should be cited'" />
      </label>
      <label class="ds-field"><span class="cc-muted cc-fs-xs">DOI</span>
        <input v-model="form.doi" class="field-input" placeholder="10.…" v-tooltip.bottom="'The dataset DOI'" />
      </label>
      <label class="ds-field"><span class="cc-muted cc-fs-xs">URL</span>
        <input v-model="form.url" class="field-input" placeholder="https://…" v-tooltip.bottom="'Where the data is published'" />
      </label>
      <label class="ds-field"><span class="cc-muted cc-fs-xs">Licence</span>
        <input v-model="form.licence" class="field-input" placeholder="e.g. CC BY 4.0" v-tooltip.bottom="'Terms the data is shared under'" />
      </label>
    </div>
    <template #footer>
      <button class="cc-btn cc-btn-ghost" :disabled="busy" @click="emit('close')">Cancel</button>
      <button class="cc-btn cc-btn-ghost" :disabled="busy || !base"
              v-tooltip.bottom="scope === 'set' ? 'Remove the set\'s source' : 'Remove their own; the set\'s applies'"
              @click="clear">Clear</button>
      <span class="ds-spacer" />
      <button class="cc-btn cc-btn-primary" :disabled="busy || formIsEmpty(form)" @click="save">Save</button>
    </template>
  </BaseModal>
</template>

<style scoped>
.ds-body { display: flex; flex-direction: column; gap: 10px; }
.ds-field { display: flex; flex-direction: column; gap: 3px; }
.ds-suggest { display: flex; align-items: center; gap: 8px; justify-content: space-between; }
.ds-spacer { flex: 1; }
</style>
