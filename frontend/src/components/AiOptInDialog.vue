<!--
  AiOptInDialog — the one place AI gets turned on (docs/todo/AI_TOGGLE_PLAN.md P3). Every "Turn on"
  opens it (`openAiOptIn()`, lib/aiOptInOpen.ts); it is mounted once in App.vue. What is sent and what you gain are read
  from the registry (`aiDialogRows()` in lib/claudeTools.ts), so a new AI surface shows up here by being
  registered. The institute line reuses CLAUDE_DATA_NOTICE; the terms are a link, never a paraphrase.
-->
<script setup lang="ts">
import { ref } from 'vue'
import BaseModal from './BaseModal.vue'
import InlineNote from './InlineNote.vue'
import { useObserverStore } from '../stores/observer'
import { aiDialogRows } from '../lib/claudeTools'
import { CLAUDE_DATA_NOTICE } from '../lib/claudeOverview'
import { AI_OPT_IN_COPY } from '../utils/aiSwitch'
import { closeAiOptIn } from '../lib/aiOptInOpen'
import { ANTHROPIC_TERMS_URL } from '../lib/links'

const observer = useObserverStore()
const rows = aiDialogRows()
const busy = ref(false)
const error = ref('')

async function turnOn() {
  if (busy.value) return
  busy.value = true
  error.value = ''
  try {
    const ok = await observer.setAiEnabled(true)
    if (!ok) { error.value = AI_OPT_IN_COPY.failed; return }
    if (!observer.ai.enabled) { error.value = AI_OPT_IN_COPY.blocked; return }   // the workstation said no
    closeAiOptIn()
  } finally { busy.value = false }
}
</script>

<template>
  <BaseModal :title="AI_OPT_IN_COPY.title" icon="pi-sparkles" width="520px" @close="closeAiOptIn">
    <p class="ai-lead cc-fs-sm">{{ AI_OPT_IN_COPY.lead }}</p>
    <div class="ai-head cc-eyebrow cc-fs-2xs">{{ AI_OPT_IN_COPY.sentHead }}</div>
    <ul class="ai-list cc-fs-sm">
      <li v-for="r in rows.filter(r => r.sends)" :key="r.id"><strong>{{ r.label }}</strong> — {{ r.sends }}</li>
    </ul>
    <div class="ai-head cc-eyebrow cc-fs-2xs">{{ AI_OPT_IN_COPY.gainHead }}</div>
    <ul class="ai-list cc-fs-sm">
      <li v-for="r in rows.filter(r => r.gain)" :key="r.id"><strong>{{ r.label }}</strong> — {{ r.gain }}</li>
    </ul>
    <div class="ai-head cc-eyebrow cc-fs-2xs" data-guide="aiOptIn.risks">{{ AI_OPT_IN_COPY.risksHead }}</div>
    <ul class="ai-list cc-fs-sm">
      <li v-for="line in AI_OPT_IN_COPY.risks" :key="line">{{ line }}</li>
    </ul>
    <InlineNote class="cc-fs-sm" severity="warn" :short="CLAUDE_DATA_NOTICE.short" :detail="CLAUDE_DATA_NOTICE.detail" />
    <p class="ai-foot cc-muted cc-fs-xs">
      <a :href="ANTHROPIC_TERMS_URL" target="_blank" rel="noopener">{{ AI_OPT_IN_COPY.terms }}</a>
      · {{ AI_OPT_IN_COPY.after }}
    </p>
    <InlineNote v-if="error" class="cc-fs-sm" severity="warn" :short="error" />
    <template #footer>
      <button class="cc-btn cc-btn-ghost" @click="closeAiOptIn">Cancel</button>
      <button class="cc-btn cc-btn-primary" data-guide="aiOptIn.turnOn" :disabled="busy" @click="turnOn">
        <i v-if="busy" class="pi pi-spin pi-spinner" /> {{ AI_OPT_IN_COPY.confirm }}
      </button>
    </template>
  </BaseModal>
</template>

<style scoped>
.ai-lead { margin: 0 0 0.6rem; }
.ai-head { margin-top: 0.6rem; }
.ai-list { margin: 0.2rem 0 0; padding-left: 1.1rem; display: flex; flex-direction: column; gap: 0.2rem; }
.ai-foot { margin: 0.6rem 0 0; }
</style>
