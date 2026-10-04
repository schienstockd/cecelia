<script setup lang="ts">
// Add a decision the agent should have made and did not (AGENT_RUN_REVIEW_PLAN Decision 8): image +
// step + what it should have done + why. The parent appends it as an `mNN` section marked Bad.
import { ref } from 'vue'
import { RUN_STEPS, type RunStep } from '../../utils/blackboardMd'

defineProps<{ images: string[]; busy?: boolean }>()
const emit = defineEmits<{ add: [step: RunStep, image: string, text: string, note: string] }>()

const open = ref(false)
const step = ref<RunStep>('gate')
const image = ref('')
const text = ref('')
const note = ref('')
function add() {
  if (!text.value.trim() || !note.value.trim()) return
  emit('add', step.value, image.value, text.value.trim(), note.value.trim())
  open.value = false
  text.value = ''
  note.value = ''
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
      <button class="cc-btn cc-btn-primary cc-btn-dense" :disabled="busy || !text.trim() || !note.trim()"
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
