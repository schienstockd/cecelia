<!--
  The draft Methods section for a board, a set or a chain run (`GET /api/analysis/methods`,
  app/src/ai/methods_report.jl), read-only: the server's Markdown as it is, with Copy, a Markdown
  download and the parameter table as CSV. docs/ANALYSIS.md → *Methods report*.
-->
<script setup lang="ts">
import { ref, onMounted } from 'vue'
import BaseModal from './BaseModal.vue'
import { useCopyFlash } from '../composables/useCopyFlash'
import { downloadText } from '../plots/export'
import { historyParamsCsv } from '../utils/chainFromHistory'
import { methodsReportUrl, methodsFileBase, type MethodsReport, type MethodsScope } from '../utils/methodsReport'

const props = defineProps<{ projectUid: string; scope: MethodsScope; name: string }>()
const emit = defineEmits<{ (e: 'close'): void }>()

const report = ref<MethodsReport | null>(null)
const error = ref('')
const loading = ref(true)
const { copy, isCopied } = useCopyFlash()

onMounted(async () => {
  try {
    const res = await fetch(methodsReportUrl(props.projectUid, props.scope))
    const d = await res.json() as MethodsReport & { error?: string }
    if (!res.ok) throw new Error(d.error ?? res.statusText)
    report.value = d
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  } finally { loading.value = false }
})

const base = () => methodsFileBase(props.name)
</script>

<template>
  <BaseModal title="Methods (draft)" width="760px" @close="emit('close')">
    <div v-if="loading" class="cc-empty"><i class="pi pi-spin pi-spinner" /></div>
    <div v-else-if="error" class="cc-empty-inline cc-muted-error">{{ error }}</div>
    <pre v-else-if="report" class="mr-text cc-fs-xs">{{ report.markdown }}</pre>
    <template #footer>
      <button class="cc-btn cc-btn-ghost" type="button" :disabled="!report?.table.length"
              v-tooltip.top="'The parameter table as CSV'"
              @click="report && downloadText(`${base()}_parameters.csv`, historyParamsCsv(report.table), 'text/csv')">
        <i class="pi pi-download" /> CSV
      </button>
      <button class="cc-btn cc-btn-ghost" type="button" :disabled="!report" v-tooltip.top="'Save as Markdown'"
              @click="report && downloadText(`${base()}.md`, report.markdown, 'text/markdown')">
        <i class="pi pi-download" /> .md
      </button>
      <span class="mr-spacer" />
      <button class="cc-btn cc-btn-primary" type="button" :disabled="!report" @click="report && copy(report.markdown)">
        <i :class="['pi', isCopied() ? 'pi-check' : 'pi-copy']" /> {{ isCopied() ? 'Copied' : 'Copy' }}
      </button>
    </template>
  </BaseModal>
</template>

<style scoped>
.mr-text { margin: 0; white-space: pre-wrap; word-break: break-word; font-family: var(--cc-mono); line-height: 1.5; }
.mr-spacer { flex: 1; }
</style>
