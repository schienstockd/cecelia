<!--
  Everything a trained model records about how it was trained.

  The vault used to carry a one-line "Trained on" summary — channel, scales, metric count, frames —
  which was the worst of both: wide enough to force the panel to be resizable, and still too thin to
  answer anything. The full manifest belongs behind an ⓘ, and the vault goes back to a name, a date
  and a size.

  Read-only, and deliberately a dump rather than a curated subset: the manifest IS the contract
  inference configures itself from (`CoastalUtils._manifest`), so anything in it can change what a
  model does. A field this component doesn't know about still shows, under **Other** — a manifest key
  added by a later training run must not become invisible just because the modal is older than it.

  The loss curves are NOT here. "Did it converge" is a plot question and it is one — Training
  convergence, on the flow canvas — so it gets axes, zoom and CSV/PNG/SVG export instead of a
  thumbnail in a dialog.
-->
<script setup lang="ts">
import { computed } from 'vue'
import BaseModal from '../../components/BaseModal.vue'
import { modelDetailGroups, type FlowManifest } from '../../utils/flowManifest'

const props = defineProps<{ name: string; manifest: FlowManifest; path?: string }>()
defineEmits<{ (e: 'close'): void }>()

const groups = computed(() => modelDetailGroups(props.manifest))
</script>

<template>
  <BaseModal :title="name" icon="pi-database" width="560px" @close="$emit('close')">
    <p v-if="!groups.length" class="cc-muted">
      No manifest — this model assumes coastal’s default metric set. Re-train it to record one.
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
