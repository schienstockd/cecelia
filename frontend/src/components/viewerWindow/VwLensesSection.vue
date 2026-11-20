<!--
  The viewer window's Lenses section: the lens figures whose scores are on this image (Find similar saves
  one per run to Movies & figures — `composables/useVwLensQuery.ts`), newest first. Per lens: Show scores
  (colour the cells by its column), Go to (pin it again with the look it was found in, opening its image),
  delete (the figure and its score column). Rename and tags are on Movies & figures. Hidden when the
  image has none (docs/todo/LENS_QUERY_PLAN.md → P2).
-->
<script setup lang="ts">
import CollapsibleSection from '../CollapsibleSection.vue'
import CcToggle from '../CcToggle.vue'
import ConfirmDeleteButton from '../ConfirmDeleteButton.vue'
import type { LensFigureRow } from '../../utils/viewer/lensQuery'
import { useInlineEdit } from '../../composables/useInlineEdit'

const props = defineProps<{ lenses: LensFigureRow[]; shownColumn: string; error?: string }>()
const emit = defineEmits<{ showScores: [LensFigureRow, boolean]; goTo: [LensFigureRow]; remove: [LensFigureRow]
                         rename: [LensFigureRow, string] }>()
// a click on the name renames it — its Movies & figures display name, as there
const { draft, start, cancel, commit, focusInput, isEditing } = useInlineEdit()
const save = (l: LensFigureRow) => commit(l.name, l.displayName, v => emit('rename', l, v))
</script>

<template>
  <CollapsibleSection label="Lenses" tip="Lenses you ran Find similar on"
                      accordion="cc.vw.section" section="lenses" fill="content">
    <div v-for="l in props.lenses" :key="l.name" class="cc-row cc-row-tight">
      <input v-if="isEditing(l.name)" :ref="focusInput" v-model="draft" class="cc-input-2xs vw-grow"
             :aria-label="`Rename ${l.label}`" v-tooltip.bottom="'Enter to save, Esc to cancel'"
             @keyup.enter="save(l)" @keyup.esc="cancel" @blur="save(l)" />
      <span v-else class="vw-lens-name cc-fs-2xs vw-grow" v-tooltip.bottom="'Click to rename'"
            @click="start(l.name, l.displayName || l.label)">{{ l.label }}</span>
      <CcToggle :model-value="props.shownColumn === l.lens.column" :aria-label="`Show scores of ${l.label}`"
                v-tooltip.bottom="'Show scores — colour the cells by how like this lens they are'"
                @update:model-value="(on: boolean) => emit('showScores', l, on)" />
      <button class="cc-btn cc-btn-ghost cc-btn-icon" :aria-label="`Go to ${l.label}`" @click="emit('goTo', l)"
              v-tooltip.bottom="'Go to the lens'">
        <i class="pi pi-map-marker" />
      </button>
      <ConfirmDeleteButton title="Delete the lens and its scores" @confirm="emit('remove', l)" />
    </div>
    <div v-if="props.error" class="cc-muted-warn">{{ props.error }}</div>
  </CollapsibleSection>
</template>

<style scoped>
.vw-grow { flex: 1; min-width: 0; }
.vw-lens-name { cursor: text; font-weight: 600; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
</style>
