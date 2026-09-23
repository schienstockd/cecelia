<script setup lang="ts">
// Which model an assistant run uses — THE one picker. The lab log's "Ask Claude" had it inline; Kiwi's
// prompt box is the second site, so it lives here and both read the same allow-list (`observer.models`,
// served from `OBSERVER_MODELS` in app/src/ai/agent_runner.jl — the backend coerces anything else).
// Each caller keeps its own persisted choice (`settings.labLogObserverModel`, `settings.kiwiModel`):
// they are different jobs, and one of them may want a cheaper model than the other.
import { computed } from 'vue'
import { useObserverStore } from '../stores/observer'

defineProps<{ tip: string }>()
const model = defineModel<string>({ required: true })
const models = computed(() => useObserverStore().models)
</script>

<template>
  <select v-model="model" class="agent-model cc-fs-xs" v-tooltip.top="tip">
    <option v-for="m in models" :key="m" :value="m">{{ m }}</option>
  </select>
</template>

<style scoped>
.agent-model {
  color: var(--cc-text-dim); cursor: pointer;
  /* Longhand padding, NOT the shorthand: the global `select` rule sets padding-right: 1.6rem
     to reserve room for the caret, and a `padding:` shorthand here would clobber it — leaving
     the chevron painted on top of the model name. Same reasoning as background-color below. */
  padding-top: 0.05rem; padding-bottom: 0.05rem; padding-left: 0.2rem;
  /* background-COLOR, not the shorthand: the global `select` rule paints the custom caret via
     background-image, and a shorthand here would reset it to none (leaving an arrowless select). */
  background-color: var(--cc-surface-2); border-radius: var(--cc-radius-xs);
}
</style>
