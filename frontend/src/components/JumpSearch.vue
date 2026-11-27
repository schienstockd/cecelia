<!--
  The header's jump search (docs/todo/GUI_LAYOUT_PLAN.md, M8; mock frame 1): "Jump to page, task, guide
  or image…", Ctrl K / ⌘ K from anywhere. A page opens it; a guide starts it, so "capture movie" shows
  the controls (docs/todo/HELP_SEARCH_PLAN.md); a task function opens its page with the
  function picked (the same `cc-fn:<module>` pre-selection TaskRunner's "When to use" links use); an
  image switches to its set and opens it in the viewer. Nothing found → one "Ask Kiwi" row that puts the
  text in Kiwi's question box, unsent (H2; with a project open and AI on). Matching + ranking: utils/jumpSearch.ts.
-->
<script setup lang="ts">
import { computed, nextTick, ref, watch } from 'vue'
import { useWindowListener } from '../composables/useKeepAlive'
import { useRouter } from 'vue-router'
import TeleportPopover from './TeleportPopover.vue'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useTaskDefsStore } from '../stores/taskDefs'
import { useCustomModulesStore } from '../stores/customModules'
import { allNavGroups } from '../lib/navGroups'
import { mentionTargets } from '../utils/kiwiTurn'
import { profileStorage } from '../utils/profileStorage'
import { openViewerWindow } from '../utils/viewerWindow'
import { jumpHits, isJumpShortcut, type JumpHit } from '../utils/jumpSearch'
import { useGuideStore } from '../stores/guide'
import { useObserverStore } from '../stores/observer'
import { guidesByGroup } from '../lib/guides'
import { guideGroupsFor } from '../utils/aiSwitch'
import { useKiwiStore } from '../stores/kiwi'
import { moveHighlight } from '../utils/suggestInput'

const router = useRouter()
const project = useProjectStore()
const projectMeta = useProjectMetaStore()
const taskDefs = useTaskDefsStore()
const customModules = useCustomModulesStore()
const guide = useGuideStore()
const observer = useObserverStore()
const kiwi = useKiwiStore()

const query = ref('')
const open = ref(false)
const active = ref(0)
const box = ref<HTMLElement | null>(null)
const input = ref<HTMLInputElement | null>(null)

const hits = computed<JumpHit[]>(() => jumpHits(query.value, {
  pages: allNavGroups(customModules.categories).flatMap(g => g.items.map(i => ({
    to: i.to, label: i.label, group: g.heading, tip: i.tip,
    disabled: i.disabled || (i.requiresProject && !projectMeta.hasProject),
  }))),
  // the same guides the Guides dialog lists for this AI state, with its count of unmet prerequisites
  guides: guideGroupsFor(guidesByGroup(), observer.ai.enabled).flatMap(gr => gr.guides).map(g => ({
    id: g.id, title: g.title, group: g.group, keywords: g.keywords,
    missing: guide.prereqState(g).filter(p => !p.met).length,
  })),
  functions: projectMeta.hasProject ? mentionTargets(taskDefs.all()) : [],
  images: project.sets.flatMap(s => s.images.map(i => ({ uid: i.uid, name: i.name, setUid: s.uid, setName: s.name }))),
}))
watch(hits, () => { active.value = 0 })
watch(query, q => { open.value = !!q.trim() })

const KIND_ICON: Record<JumpHit['kind'], string> = { page: 'pi-arrow-right', guide: 'pi-compass', function: 'pi-cog', image: 'pi-image' }

// nothing matched: offer the question to Kiwi (no model call until the user sends it there)
const canAsk = computed(() => !hits.value.length && projectMeta.hasProject && observer.ai.enabled)
function close() {
  query.value = ''
  open.value = false
  input.value?.blur()
}
function askKiwi() {
  if (kiwi.askText(query.value)) close()
}

function go(h: JumpHit | undefined) {
  if (!h) { if (canAsk.value) askKiwi(); return }
  if (h.kind === 'page' && h.route) void router.push(h.route)
  else if (h.kind === 'guide' && h.guideId) guide.start(h.guideId)
  else if (h.kind === 'function' && h.fn) {
    profileStorage.setItem(`cc-fn:${h.fn.module}`, h.fn.task)
    void router.push(h.fn.path)
  } else if (h.kind === 'image' && h.image) {
    project.activeSetUid = h.image.setUid
    openViewerWindow({ projectUid: projectMeta.current?.uid ?? '', imageUid: h.image.uid })
  }
  close()
}

function onKey(e: KeyboardEvent) {
  // the first hit starts highlighted (Enter takes the best match); arrows wrap, as SuggestInput's do
  if (e.key === 'ArrowDown') { e.preventDefault(); active.value = moveHighlight(active.value, 1, hits.value.length) }
  else if (e.key === 'ArrowUp') { e.preventDefault(); active.value = moveHighlight(active.value, -1, hits.value.length) }
  else if (e.key === 'Enter') { e.preventDefault(); go(hits.value[active.value]) }
  // Esc is consumed here, so the maximised canvas's lowest-priority Esc does not also fire
  else if (e.key === 'Escape') { e.preventDefault(); close() }
}

function onGlobalKey(e: KeyboardEvent) {
  if (!isJumpShortcut(e)) return
  e.preventDefault()
  void taskDefs.ensureLoaded()
  void nextTick(() => { input.value?.focus(); input.value?.select() })
}
useWindowListener('keydown', onGlobalKey)
</script>

<template>
  <div ref="box" class="jump" data-guide="header.search">
    <i class="pi pi-search jump-icon" aria-hidden="true" />
    <input ref="input" v-model="query" class="jump-input" type="search" spellcheck="false"
           placeholder="Jump to page, task, guide or image…" aria-label="Jump to a page, task, guide or image"
           v-tooltip.bottom="'Pages, guides, task functions and images (Ctrl K)'"
           @focus="taskDefs.ensureLoaded(); open = !!query.trim()" @keydown="onKey" />
    <kbd class="jump-kbd cc-muted cc-fs-2xs" aria-hidden="true">Ctrl K</kbd>
  </div>
  <TeleportPopover v-model="open" :anchor="box" placement="bottom-start" flush>
    <div class="cc-actions-menu jump-list" role="listbox">
      <button v-for="(h, i) in hits" :key="h.kind + (h.route ?? h.guideId ?? h.fn?.task ?? h.image?.uid ?? h.label)"
              class="cc-actions-item" :class="{ 'jump-on': i === active }" role="option"
              :aria-selected="i === active" @mouseenter="active = i" @click="go(h)">
        <i :class="['pi', KIND_ICON[h.kind]]" />
        <span class="jump-label">{{ h.label }}</span>
        <span class="jump-sub cc-muted cc-fs-xs">{{ h.sub }}</span>
      </button>
      <button v-if="canAsk" class="cc-actions-item jump-on" role="option" aria-selected="true" @click="askKiwi()">
        <i class="pi pi-comments" />
        <span class="jump-label">Ask Kiwi: “{{ query.trim() }}”</span>
      </button>
      <div v-else-if="!hits.length" class="cc-actions-head">Nothing matches</div>
    </div>
  </TeleportPopover>
</template>

<style scoped>
.jump {
  display: flex; align-items: center; gap: 0.4rem; width: 22rem; max-width: 30vw; height: 26px;
  padding: 0 0.5rem; border: 1px solid var(--cc-border); border-radius: var(--cc-radius-sm);
  background: var(--cc-bg);
}
.jump:focus-within { border-color: var(--cc-accent); }
.jump-icon { color: var(--cc-text-dim); font-size: var(--cc-fs-xs); }
.jump-input { flex: 1; min-width: 0; border: none; background: none; padding: 0; height: 100%; }
.jump-kbd { border: 1px solid var(--cc-border); border-radius: var(--cc-radius-xs); padding: 0 0.3rem; }
.jump-list { width: 22rem; }
.jump-on { background: var(--cc-surface-2); }
.jump-label { flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
/* may shrink: a guide's "Guide · 2 missing" must not squeeze the label out */
.jump-sub { flex-shrink: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
</style>
