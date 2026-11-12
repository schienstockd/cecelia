<script setup lang="ts">
/*
  The app's home page at `/` (docs/todo/GUI_CLARITY_PLAN.md: audit finding 1, Decisions 13 and 16).

  It answers "where do I start?", one step at a time: with no project open (or after "Projects"), the
  recent projects, with a spinner while one opens; with one open, the active set's recipe (its own, or the one its axes suggest, picked here and stored per set) and one button
  to the recipe's first page. Nothing here runs or selects anything; the Start button only navigates.

  `/` stays its own page, not a redirect: a record's `redirect` resolves before any router guard, so a
  profile-derived landing page bounced on a cold boot. No page is privileged as "the start" — the
  recipe is the user's pick.
*/
import { computed, onMounted, ref, watch } from 'vue'
import { useRouter } from 'vue-router'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSetRecipe } from '../composables/useSetRecipe'
import { WRITTEN_RECIPES, resumeStepOf, ranPagesOf } from '../utils/setRecipe'
import { formatWhen } from '../utils/formatWhen'
import SelectionTable, { type SelectionColumn } from '../components/SelectionTable.vue'
import ProjectPanel from '../components/ProjectPanel.vue'

const router = useRouter()
const projectMeta = useProjectMetaStore()
const { set, own, suggestion, busy, save } = useSetRecipe()
const showPanel = ref(false)

// One step at a time: the project list, then (once a project is open) the set's recipe and the way in.
// "Projects" goes back to the list; the open project stays open until another one is clicked.
const choosingProject = ref(false)
const showList = computed(() => !projectMeta.current || choosingProject.value)
// a project opened anywhere else (the Open or create panel, the header) lands on its step too
watch(() => projectMeta.current?.uid, () => { choosingProject.value = false })
// A project takes a moment to load — name it with a spinner while it does.
const opening = ref('')
async function open(uid: string, name: string) {
  if (opening.value) return
  opening.value = name
  try { if (await projectMeta.openProject(uid)) choosingProject.value = false }
  finally { opening.value = '' }
}

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
// Opening the project that is already open just goes back to it.
function openRow(uid: string, name: string) {
  if (uid === projectMeta.current?.uid) choosingProject.value = false
  else void open(uid, name)
}

// Only a STORED recipe is ticked. The suggestion is named on its row and taken with "Use …" — a
// pre-ticked suggestion could not be clicked into a stored one (clicking the ticked row changes
// nothing), so the question came back on every visit (Decision 16: suggested, never applied silently).
const picked = computed(() => own.value?.id ?? '')
// A set that has its recipe shows one line; the table comes back on "Change".
const changing = ref(false)
async function pick(id: string) { await save(id); changing.value = false }
const recipeRows = computed(() => WRITTEN_RECIPES.map(r => ({
  id: r.id,
  title: r.id === suggestion.value?.recipe.id ? `${r.title} · suggested (${suggestion.value.reason})` : r.title,
  when: r.whenThisIsYou,
})))
const RECIPE_COLUMNS: SelectionColumn[] = [
  { key: 'title', label: 'Recipe' },
  { key: 'when', label: 'Is this you?', ellipsis: true },
]
// Pick the set up where it is: the first recipe step whose page has not run on it yet.
const start = computed(() => {
  const r = own.value ?? suggestion.value?.recipe
  return r && set.value ? resumeStepOf(r, ranPagesOf(set.value.images)) : null
})
</script>

<template>
  <div class="welcome">
    <div class="welcome-col">
      <div class="welcome-lockup" aria-hidden="true">
        <img class="welcome-mark" src="/feijoa.svg" alt="" width="48" height="48" />
        <span class="welcome-word">Cecelia</span>
      </div>

      <!-- Step 1: which project -->
      <template v-if="showList">
        <div v-if="opening" class="cc-empty-inline" role="status">
          <i class="pi pi-spin pi-spinner" /> Opening {{ opening }}…
        </div>
        <template v-else>
          <SelectionTable v-if="recent.length" :columns="RECENT_COLUMNS" :rows="recent" id-key="uid"
            selection-mode="none" fit="fill" @row-click="r => openRow(String(r.uid), String(r.name))" />
          <div class="welcome-actions cc-row">
            <button v-if="projectMeta.current" class="cc-btn cc-btn-ghost" @click="choosingProject = false"
              v-tooltip.top="'Back to the open project'">
              <i class="pi pi-arrow-left" /> {{ projectMeta.current.name }}
            </button>
            <button v-if="topRecent" class="cc-btn cc-btn-primary" @click="open(topRecent.uid, topRecent.name)">
              Open {{ topRecent.name }}
            </button>
            <button class="cc-btn" :class="topRecent ? 'cc-btn-ghost' : 'cc-btn-primary'" @click="showPanel = true">
              <i class="pi pi-folder-open" /> Open or create {{ topRecent || projectMeta.current ? 'another' : 'a project' }}…
            </button>
          </div>
        </template>
      </template>

      <!-- Step 2: what the active set is, and where to pick it up -->
      <template v-else-if="projectMeta.current">
        <div class="cc-row welcome-head">
          <button class="cc-btn cc-btn-ghost cc-btn-dense" @click="choosingProject = true"
            v-tooltip.top="'Switch to another project'">
            <i class="pi pi-arrow-left" /> Projects
          </button>
          <h1 class="welcome-title">{{ projectMeta.current.name }}</h1>
        </div>
        <div v-if="!set" class="welcome-actions cc-row">
          <button class="cc-btn cc-btn-primary" @click="router.push('/manage-images')">
            <i class="pi pi-images" /> Import images
          </button>
        </div>
        <template v-else>
          <template v-if="own && !changing">
            <span class="cc-eyebrow">Set “{{ set.name }}”</span>
            <div class="cc-row welcome-actions">
              <span>Follows <b>{{ own.title }}</b></span>
              <button class="cc-btn cc-btn-ghost cc-btn-dense" @click="changing = true"
                v-tooltip.top="'Select another recipe for this set'">Change</button>
            </div>
          </template>
          <template v-else>
            <span class="cc-eyebrow">What is set “{{ set.name }}”?</span>
            <span class="cc-muted">It lights the pages you’ll need. Nothing is hidden.</span>
            <SelectionTable :columns="RECIPE_COLUMNS" :rows="recipeRows" id-key="id" fit="fill"
              :model-value="picked" :disabled="busy" @update:model-value="id => pick(String(id))" />
          </template>
          <div class="welcome-actions cc-row">
            <button v-if="!own && suggestion" class="cc-btn cc-btn-ghost" :disabled="busy"
              @click="pick(suggestion.recipe.id)" v-tooltip.top="'Keep it for this set'">
              Use {{ suggestion.recipe.title }}
            </button>
            <button v-if="start" class="cc-btn cc-btn-next" @click="router.push(start.route)"
              v-tooltip.top="start.why">
              {{ own ? 'Continue' : 'Start' }}: {{ start.label }} <i class="pi pi-arrow-right" />
            </button>
          </div>
        </template>
      </template>
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
.welcome-head { gap: var(--cc-space-md); align-items: center; }
</style>
