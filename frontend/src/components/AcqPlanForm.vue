<script setup lang="ts">
// The set's plan for the acquisition check (docs/todo/ACQUISITION_TEST_CHECK_PLAN.md): which objects
// will be segmented or tracked, in which channel (by name) per label design — a design being the movies
// that share a channel-name list — and what is measured and compared. A
// form, not parsed text — the sentence is only echoed. Logic: utils/acqCheck.ts.
import { computed, ref } from 'vue'
import ChipSelect from './ChipSelect.vue'
import type { AcqDesign, AcqPlan, AcqPlanChannel, AcqStage } from '../utils/acqCheckApi'
import { ACQ_MEASURES, ACQ_STAGES, cleanPlan, planByName, planProblems, planSentence } from '../utils/acqCheck'

const props = defineProps<{ plan: AcqPlan; designs: AcqDesign[]; saving?: boolean }>()
const emit = defineEmits<{ save: [AcqPlan]; cancel: [] }>()

// a working copy: the form edits it, Save hands it back cleaned
const designOf = (id: string) => props.designs.find(d => d.id === id)
const draft = ref<AcqPlan>({ stage: 'test' as AcqStage, ...planByName(JSON.parse(JSON.stringify(props.plan)), props.designs) })
const channelOptions = (id: string) =>
  (designOf(id)?.channels ?? []).map(c => ({ value: c.name, label: `${c.name}${c.filter ? ` · ${c.filter}` : ''}` }))

function addRow(list: AcqPlanChannel[], kind?: 'cells') {
  list.push({ name: '', ...(kind ? { kind } : {}), channels: {} })
}
function setChannel(o: AcqPlanChannel, design: string, v: string) {
  if (v === '') delete o.channels[design]
  else o.channels[design] = v
}
const measureOptions = ACQ_MEASURES.map(m => ({ value: m, label: m[0].toUpperCase() + m.slice(1) }))
const compareOptions = computed(() =>
  draft.value.objects.filter(o => o.name.trim()).map(o => ({ value: o.name.trim(), label: o.name.trim() })))
const problems = computed(() => planProblems(draft.value))
const sentence = computed(() => planSentence(cleanPlan(draft.value)))
</script>

<template>
  <div class="acq-form">
    <div class="cc-row">
      <span class="cc-muted cc-fs-xs lbl">Movies</span>
      <ChipSelect :model-value="draft.stage ?? 'test'" @update:model-value="v => draft.stage = v as AcqStage" :options="ACQ_STAGES" aria-label="Where these movies stand"
                  v-tooltip.top="'Final data: no more movies, advice is for the analysis'" />
    </div>
    <div class="cc-eyebrow cc-fs-2xs">Objects you'll segment or track</div>
    <div class="grid" :style="{ '--n': draft.designs.length }">
      <span class="cc-muted cc-fs-xs">Name</span>
      <span v-for="d in draft.designs" :key="d.id" class="cc-muted cc-fs-xs ellip" v-tooltip.top="d.key">{{ d.label || d.key }}</span>
      <span class="cc-muted cc-fs-xs">Kind</span>
      <span />
      <template v-for="(o, i) in draft.objects" :key="'o' + i">
        <input v-model="o.name" placeholder="e.g. OT-I" v-tooltip.top="'What these cells are'" />
        <select v-for="d in draft.designs" :key="d.id" :value="o.channels[d.id] ?? ''"
                @change="setChannel(o, d.id, ($event.target as HTMLSelectElement).value)"
                v-tooltip.top="'Channel it is in, in this design'">
          <option value="">channel…</option>
          <option v-for="c in channelOptions(d.id)" :key="c.value" :value="c.value">{{ c.label }}</option>
        </select>
        <select v-model="o.kind" v-tooltip.top="'Points are counted, not read for noise'">
          <option value="cells">cells</option>
          <option value="points">points</option>
        </select>
        <button class="cc-btn cc-btn-bare cc-btn-icon" @click="draft.objects.splice(i, 1)" v-tooltip.top="'Remove'">
          <i class="pi pi-times" />
        </button>
      </template>
    </div>
    <button class="cc-btn cc-btn-ghost cc-btn-dense cc-fs-xs" @click="addRow(draft.objects, 'cells')">
      <i class="pi pi-plus" /> Object
    </button>

    <div class="cc-eyebrow cc-fs-2xs">Reference channels (not segmented)</div>
    <div v-if="draft.references.length" class="grid" :style="{ '--n': draft.designs.length }">
      <template v-for="(o, i) in draft.references" :key="'r' + i">
        <input v-model="o.name" placeholder="e.g. empty 570–616" v-tooltip.top="'An empty or autofluorescence channel'" />
        <select v-for="d in draft.designs" :key="d.id" :value="o.channels[d.id] ?? ''"
                @change="setChannel(o, d.id, ($event.target as HTMLSelectElement).value)"
                v-tooltip.top="'Leave unset in a design without it'">
          <option value="">none</option>
          <option v-for="c in channelOptions(d.id)" :key="c.value" :value="c.value">{{ c.label }}</option>
        </select>
        <span />
        <button class="cc-btn cc-btn-bare cc-btn-icon" @click="draft.references.splice(i, 1)" v-tooltip.top="'Remove'">
          <i class="pi pi-times" />
        </button>
      </template>
    </div>
    <button class="cc-btn cc-btn-ghost cc-btn-dense cc-fs-xs" @click="addRow(draft.references)">
      <i class="pi pi-plus" /> Reference
    </button>

    <div class="cc-row">
      <span class="cc-muted cc-fs-xs lbl">Measure</span>
      <ChipSelect v-model="draft.measure" :options="measureOptions" multiple aria-label="What you will measure"
                  v-tooltip.top="'What you will measure'" />
    </div>
    <div class="cc-row">
      <span class="cc-muted cc-fs-xs lbl">Compare</span>
      <ChipSelect v-model="draft.compare" :options="compareOptions" multiple aria-label="Objects you will compare"
                  v-tooltip.top="'Objects you will compare'" />
    </div>
    <div class="cc-row">
      <span class="cc-muted cc-fs-xs lbl">Expected</span>
      <input v-model="draft.expectation" class="expect" placeholder="optional, e.g. naive — the same"
             v-tooltip.top="'What you expect if nothing differs'" />
    </div>

    <p v-if="sentence" class="echo">{{ sentence }}</p>
    <p v-for="p in problems" :key="p" class="cc-muted-warn cc-fs-xs">{{ p }}</p>
    <div class="cc-row actions">
      <button class="cc-btn" @click="emit('cancel')">Cancel</button>
      <button class="cc-btn cc-btn-primary" :disabled="!!problems.length || saving" @click="emit('save', cleanPlan(draft))">
        <i v-if="saving" class="pi pi-spin pi-spinner" /> Check
      </button>
    </div>
  </div>
</template>

<style scoped>
.acq-form { display: flex; flex-direction: column; gap: 0.45rem; }
.grid { display: grid; grid-template-columns: 9rem repeat(var(--n), minmax(9rem, 1fr)) 5.5rem 1.75rem; gap: 0.3rem; align-items: center; }
.ellip { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.lbl { width: 4.5rem; }
.expect { flex: 1; }
.echo { margin: 0.2rem 0 0; padding: 0.35rem 0.55rem; background: var(--cc-surface-2); border-radius: var(--cc-radius-sm); font-style: italic; }
.actions { justify-content: flex-end; }
</style>
