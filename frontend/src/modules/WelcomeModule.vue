<script setup lang="ts">
/*
  The app's home page at `/` (docs/todo/GUI_CLARITY_PLAN.md: audit finding 1, Decisions 13 and 16).

  It answers "where do I start?": with no project open, the recent projects; with one open, the active
  set's recipe (its own, or the one its axes suggest, picked here and stored per set) and one button
  to the recipe's first page. Nothing here runs or selects anything; the Start button only navigates.

  `/` stays its own page, not a redirect: a record's `redirect` resolves before any router guard, so a
  profile-derived landing page bounced on a cold boot. No page is privileged as "the start" — the
  recipe is the user's pick.
*/
import { computed, onMounted, ref } from 'vue'
import { useRouter } from 'vue-router'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSetRecipe } from '../composables/useSetRecipe'
import { WRITTEN_RECIPES, firstStepOf } from '../utils/setRecipe'
import { formatWhen } from '../utils/formatWhen'
import SelectionTable, { type SelectionColumn } from '../components/SelectionTable.vue'
import ProjectPanel from '../components/ProjectPanel.vue'

const router = useRouter()
const projectMeta = useProjectMetaStore()
const { set, own, suggestion, busy, save } = useSetRecipe()
const showPanel = ref(false)

onMounted(() => { if (projectMeta.recent.length === 0) void projectMeta.fetchRecent() })

// `/api/projects` already orders the list (the profile's own opens first, newest first).
const plural = (n: number, w: string) => `${n} ${w}${n === 1 ? '' : 's'}`
const recent = computed(() => projectMeta.recent.slice(0, 6).map(p => ({
  uid: p.uid, name: p.name,
  size: p.nSets == null ? '' : `${plural(p.nSets, 'set')}, ${plural(p.nImages ?? 0, 'image')}`,
  opened: p.lastOpenedAt ? formatWhen(new Date(p.lastOpenedAt)) : '—',
})))
const RECENT_COLUMNS: SelectionColumn[] = [
  { key: 'name', label: 'Project' },
  { key: 'size', label: 'Holds', width: 150 },
  { key: 'opened', label: 'Last opened', width: 110 },
]
// "Open T cells": the most recent project, one click (mock frame 3)
const topRecent = computed(() => projectMeta.current ? null : projectMeta.recent[0] ?? null)

// The suggested recipe is pre-highlighted but not stored until the user picks it (Decision 16).
const picked = computed(() => own.value?.id ?? suggestion.value?.recipe.id ?? '')
const recipeRows = computed(() => WRITTEN_RECIPES.map(r => ({
  id: r.id,
  title: r.id === suggestion.value?.recipe.id ? `${r.title} · suggested (${suggestion.value.reason})` : r.title,
  when: r.whenThisIsYou,
})))
const RECIPE_COLUMNS: SelectionColumn[] = [
  { key: 'title', label: 'Recipe' },
  { key: 'when', label: 'Is this you?', ellipsis: true },
]
const start = computed(() => {
  const r = own.value ?? suggestion.value?.recipe
  return r ? firstStepOf(r) : null
})
</script>

<template>
  <div class="welcome">
    <!-- Two columns (mock frame 3): where you were on the left, what this set is on the right -->
    <div class="welcome-col">
      <div class="welcome-lockup" aria-hidden="true">
        <img class="welcome-mark" src="/feijoa.svg" alt="" width="48" height="48" />
        <span class="welcome-word">Cecelia</span>
      </div>
      <h1 v-if="projectMeta.current" class="welcome-title">{{ projectMeta.current.name }}</h1>
      <SelectionTable v-if="recent.length" :columns="RECENT_COLUMNS" :rows="recent" id-key="uid"
        selection-mode="none" fit="fill" @row-click="r => projectMeta.openProject(String(r.uid))" />
      <div class="welcome-actions cc-row">
        <button v-if="topRecent" class="cc-btn cc-btn-primary" @click="projectMeta.openProject(topRecent.uid)">
          Open {{ topRecent.name }}
        </button>
        <button class="cc-btn" :class="topRecent ? 'cc-btn-ghost' : 'cc-btn-primary'" @click="showPanel = true">
          <i class="pi pi-folder-open" /> Open or create {{ topRecent || projectMeta.current ? 'another' : 'a project' }}…
        </button>
        <button v-if="projectMeta.current && !set" class="cc-btn cc-btn-primary" @click="router.push('/manage-images')">
          <i class="pi pi-images" /> Import images
        </button>
      </div>
    </div>

    <div v-if="projectMeta.current && set" class="welcome-col">
      <span class="cc-eyebrow">What is set “{{ set.name }}”?</span>
      <span class="cc-muted">It lights the pages you’ll need. Nothing is hidden.</span>
      <SelectionTable :columns="RECIPE_COLUMNS" :rows="recipeRows" id-key="id" fit="fill"
        :model-value="picked" :disabled="busy" @update:model-value="id => save(id)" />
      <div class="welcome-actions cc-row">
        <button v-if="start" class="cc-btn cc-btn-next" @click="router.push(start.route)"
          v-tooltip.top="start.why">
          Start: {{ start.label }} <i class="pi pi-arrow-right" />
        </button>
        <span v-if="!own && suggestion" class="cc-muted cc-fs-xs">Pick a recipe to keep it for this set.</span>
      </div>
    </div>
    <ProjectPanel v-if="showPanel" @close="showPanel = false" />
  </div>
</template>

<style scoped>
/* `min-height`, NOT `flex: 1` — `.cc-main` (App.vue) is a flex ITEM with `overflow-y: auto`, not a
   flex container, so `flex` on a child does nothing. */
.welcome {
  min-height: 100%;
  display: flex;
  flex-wrap: wrap;
  justify-content: center;
  align-items: flex-start;
  gap: var(--cc-space-2xl);
  padding: var(--cc-space-2xl) var(--cc-space-xl);
}
.welcome-col {
  width: min(36rem, 100%);
  display: flex;
  flex-direction: column;
  gap: var(--cc-space-lg);
}
.welcome-lockup { display: flex; align-items: center; gap: var(--cc-space-sm); user-select: none; }
/* The brand mark in grey, so it reads as chrome rather than content. */
.welcome-mark { filter: grayscale(1); opacity: 0.6; }
/* Weight 800 + tight tracking is the logo's own lettering, in the body font the app already ships. */
.welcome-word { font-size: 1.6rem; font-weight: 800; letter-spacing: -0.02em; color: var(--cc-text); }
.welcome-title { margin: 0; font-size: 1.3rem; font-weight: 600; }
.welcome-actions { gap: var(--cc-space-md); align-items: center; }
</style>
