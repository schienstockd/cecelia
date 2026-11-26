<!--
  The draft Methods section for a set (or a board's images) or a chain run (`GET /api/analysis/methods`,
  app/src/ai/methods_report.jl), read-only: the server's Markdown rendered (`renderMarkdown`, sanitised —
  names in it are the user's), with Copy (the Markdown), a Markdown download and the parameter table as
  CSV — and, for a chain run, its RO-Crate (`downloadRoCrate`).
  docs/ANALYSIS.md → *Methods report*, *RO-Crate export*.
-->
<script setup lang="ts">
import { ref, computed, onMounted } from 'vue'
import BaseModal from './BaseModal.vue'
import { useCopyFlash } from '../composables/useCopyFlash'
import { downloadText } from '../plots/export'
import { renderMarkdown } from '../utils/sanitizeHtml'
import { getJson } from '../utils/api'
import { methodsReportUrl, methodsFileBase, downloadRoCrate, type MethodsReport, type MethodsScope } from '../utils/methodsReport'

const props = defineProps<{ projectUid: string; scope: MethodsScope; name: string }>()
const emit = defineEmits<{ (e: 'close'): void }>()

const report = ref<MethodsReport | null>(null)
const error = ref('')
const loading = ref(true)
const { copy, isCopied } = useCopyFlash()

onMounted(async () => {
  try {
    report.value = await getJson<MethodsReport>(methodsReportUrl(props.projectUid, props.scope))
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  } finally { loading.value = false }
})

const html = computed(() => renderMarkdown(report.value?.markdown))
const base = () => methodsFileBase(props.name)

const crateBusy = ref(false)
const saveCrate = () => props.scope.kind === 'chainRun' && downloadRoCrate(props.projectUid, props.scope.runId, crateBusy)
</script>

<template>
  <BaseModal title="Methods (draft)" width="760px" @close="emit('close')">
    <div v-if="loading" class="cc-empty"><i class="pi pi-spin pi-spinner" /></div>
    <div v-else-if="error" class="cc-empty-inline cc-muted-error">{{ error }}</div>
    <div v-else-if="report" class="mr-text cc-fs-sm" v-html="html" />
    <template #footer>
      <button class="cc-btn cc-btn-ghost" type="button" :disabled="!report?.table.length"
              v-tooltip.top="'The parameter table as CSV'"
              @click="report && downloadText(`${base()}_parameters.csv`, report.csv, 'text/csv')">
        <i class="pi pi-download" /> CSV
      </button>
      <button class="cc-btn cc-btn-ghost" type="button" :disabled="!report" v-tooltip.top="'Save as Markdown'"
              @click="report && downloadText(`${base()}.md`, report.markdown, 'text/markdown')">
        <i class="pi pi-download" /> .md
      </button>
      <button v-if="scope.kind === 'chainRun'" class="cc-btn cc-btn-ghost" type="button" :disabled="!report || crateBusy"
              v-tooltip.top="'Methods, parameters and template as an RO-Crate (.zip)'" @click="saveCrate">
        <i :class="['pi', crateBusy ? 'pi-spin pi-spinner' : 'pi-download']" /> RO-Crate
      </button>
      <span class="mr-spacer" />
      <button class="cc-btn cc-btn-primary" type="button" :disabled="!report" @click="report && copy(report.markdown)">
        <i :class="['pi', isCopied() ? 'pi-check' : 'pi-copy']" /> {{ isCopied() ? 'Copied' : 'Copy' }}
      </button>
    </template>
  </BaseModal>
</template>

<style scoped>
.mr-text { line-height: 1.55; word-break: break-word; }
.mr-text :deep(h2) { margin: 0.2em 0 0.5em; font-size: 1.15em; }
.mr-text :deep(h3) { margin: 1.1em 0 0.35em; font-size: 1em; }
.mr-text :deep(p) { margin: 0 0 0.6em; }
.mr-text :deep(blockquote) { margin: 0 0 0.8em; padding: 0.35em 0.8em; border-left: 3px solid var(--cc-warn);
                             background: var(--cc-surface-2); }
.mr-text :deep(blockquote p) { margin: 0; }
.mr-text :deep(ol), .mr-text :deep(ul) { margin: 0 0 0.6em; padding-left: 1.6em; }
.mr-text :deep(li) { margin: 0.15em 0; }
.mr-text :deep(table) { border-collapse: collapse; margin: 0 0 0.8em; font-size: 0.95em; }
.mr-text :deep(th), .mr-text :deep(td) { border: 1px solid var(--cc-border); padding: 2px 6px; text-align: left; }
.mr-spacer { flex: 1; }
</style>
