<!--
  The active board's figure legends, one per filled panel (`LayoutCanvas.collectLegends`,
  utils/panelLegend.ts): rendered, with Copy (the Markdown) and a `.md` download. A legend says what its
  panel shows; how the data were made is the Methods report's. docs/ANALYSIS.md → *Board legends*.
-->
<script setup lang="ts">
import { computed } from 'vue'
import BaseModal from '../BaseModal.vue'
import { useCopyFlash } from '../../composables/useCopyFlash'
import { downloadText } from '../../plots/export'
import { renderMarkdown } from '../../utils/sanitizeHtml'
import { safeNamePart } from '../../utils/batchMovie'

const props = defineProps<{ markdown: string; name: string }>()
const emit = defineEmits<{ (e: 'close'): void }>()
const { copy, isCopied } = useCopyFlash()
const html = computed(() => renderMarkdown(props.markdown))
const file = () => `${safeNamePart(props.name) || 'board'}_legends.md`
</script>

<template>
  <BaseModal title="Legends" width="680px" @close="emit('close')">
    <div class="bl-text cc-fs-sm" v-html="html" />
    <template #footer>
      <button class="cc-btn cc-btn-ghost" type="button" v-tooltip.top="'Save as Markdown'"
              @click="downloadText(file(), markdown, 'text/markdown')">
        <i class="pi pi-download" /> .md
      </button>
      <span class="bl-spacer" />
      <button class="cc-btn cc-btn-primary" type="button" @click="copy(markdown)">
        <i :class="['pi', isCopied() ? 'pi-check' : 'pi-copy']" /> {{ isCopied() ? 'Copied' : 'Copy' }}
      </button>
    </template>
  </BaseModal>
</template>

<style scoped>
.bl-text { line-height: 1.55; word-break: break-word; }
.bl-text :deep(h2) { margin: 0.2em 0 0.6em; font-size: 1.1em; }
.bl-text :deep(p) { margin: 0 0 0.7em; }
.bl-spacer { flex: 1; }
</style>
