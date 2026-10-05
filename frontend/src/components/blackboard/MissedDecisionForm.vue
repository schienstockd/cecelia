<script setup lang="ts">
// Add a decision the agent should have made and did not (AGENT_RUN_REVIEW_PLAN Decision 8): image +
// step + what it should have done + why + its cause (GUIDE_RUNS_PLAN Decision 2). The parent appends
// it as an `mNN` section marked Bad.
import { ref, computed } from 'vue'
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'
import { RUN_STEPS, type RunStep } from '../../utils/blackboardMd'
import { SECTION_CAUSE_OPTIONS, sectionVerdictReady, type SectionCause } from '../../utils/blackboardApi'

defineProps<{ images: string[]; busy?: boolean }>()
const emit = defineEmits<{ add: [step: RunStep, image: string, text: string, note: string, cause: SectionCause] }>()

const CAUSE_OPTS: ChipOption[] = [...SECTION_CAUSE_OPTIONS]

const open = ref(false)
const step = ref<RunStep>('gate')
const image = ref('')
const text = ref('')
const note = ref('')
const cause = ref<SectionCause | null>(null)
const ready = computed(() => !!text.value.trim() && sectionVerdictReady('bad', note.value, cause.value, true))
function add() {
  if (!ready.value || !cause.value) return
  emit('add', step.value, image.value, text.value.trim(), note.value.trim(), cause.value)
  open.value = false
  text.value = ''
  note.value = ''
  cause.value = null
}
</script>

<template>
  <div class="mdf">
    <button v-if="!open" class="cc-btn cc-btn-ghost cc-btn-dense" :disabled="busy" @click="open = true"
            v-tooltip.top="'Add a decision the agent should have made'">
      <i class="pi pi-plus" /> Add missed decision
    </button>
    <div v-else class="cc-row cc-row-tight mdf-row">
      <select v-model="step" class="cc-input-xs" v-tooltip.top="'Step'">
        <option v-for="s in RUN_STEPS" :key="s" :value="s">{{ s }}</option>
      </select>
      <select v-model="image" class="cc-input-xs" v-tooltip.top="'Image'">
        <option value="">all images</option>
        <option v-for="i in images" :key="i" :value="i">{{ i }}</option>
      </select>
      <input v-model="text" class="mdf-input" type="text" placeholder="What it should have done"
             v-tooltip.top="'What the agent should have done'" />
      <input v-model="note" class="mdf-input" type="text" placeholder="Why — required"
             @keydown.enter="add" v-tooltip.top="'Why it matters'" />
      <ChipSelect variant="segmented" :options="CAUSE_OPTS" :model-value="cause" aria-label="Cause"
                  @update:model-value="c => cause = (c as SectionCause) || null" />
      <button class="cc-btn cc-btn-primary cc-btn-dense" :disabled="busy || !ready"
              @click="add" v-tooltip.top="'Add it as a Bad decision'">Add</button>
      <button class="cc-btn cc-btn-ghost cc-btn-dense" @click="open = false">Cancel</button>
    </div>
  </div>
</template>

<style scoped>
.mdf { margin: 8px 0; }
.mdf-row { flex-wrap: wrap; }
.mdf-input { flex: 1; min-width: 180px; font-size: var(--cc-fs-xs); }
</style>
