<!--
  Everything a trained denoise model records about how it was trained.

  Read-only, and deliberately a full dump rather than a curated subset: the manifest IS the contract
  inference configures itself from (`denoise_run._build_model` reads `arch.*` verbatim), so anything
  in it can change what a model does. A field this component doesn't know about still shows, under
  **Other** — a manifest key added by a later training run must not become invisible just because the
  modal is older than it.

  The loss curve is NOT here. "Did it converge?" is a plot question, not a thumbnail — the QC sidecar
  the trainer writes carries `epochLosses` and the training-canvas Training-convergence plot renders
  it there.
-->
<script setup lang="ts">
import { computed } from 'vue'
import BaseModal from '../../components/BaseModal.vue'
import { denoiseModelDetailGroups, type DenoiseManifest } from '../../utils/denoiseManifest'

const props = defineProps<{ name: string; manifest: DenoiseManifest; path?: string }>()
defineEmits<{ (e: 'close'): void }>()

const groups = computed(() => denoiseModelDetailGroups(props.manifest))
</script>

<template>
  <BaseModal :title="name" icon="pi-database" width="560px" @close="$emit('close')">
    <p v-if="!groups.length" class="cc-muted">
      No manifest — inference cannot reconstruct this model. Re-train it to record one.
    </p>

    <section v-for="g in groups" :key="g.label" class="fmd-group">
      <div class="cc-eyebrow cc-fs-2xs">{{ g.label }}</div>
      <dl class="fmd-list">
        <template v-for="f in g.fields" :key="f.label">
          <dt class="cc-muted">{{ f.label }}</dt>
          <dd :class="{ 'fmd-mono': f.mono }">{{ f.value }}</dd>
        </template>
      </dl>
    </section>

    <p v-if="path" class="cc-muted cc-fs-xs fmd-path" v-tooltip.top="path">{{ path }}</p>
  </BaseModal>
</template>

<style scoped>
.fmd-group + .fmd-group { margin-top: 0.9rem; }
.fmd-list { display: grid; grid-template-columns: 11rem 1fr; gap: 0.25rem 0.8rem; margin: 0.35rem 0 0; }
.fmd-list dt { margin: 0; }
.fmd-list dd { margin: 0; overflow-wrap: anywhere; }
.fmd-mono { font-family: var(--cc-mono); font-size: var(--cc-fs-xs); }
.fmd-path { margin: 0.9rem 0 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
</style>
