<!--
  "Which step?" — the Guides panel's view of every task's purpose, use-when and not-when, grouped by
  the page it runs on (docs/todo/TASK_DISCOVERY_PLAN.md Decision 8). Generated from the task specs via
  `utils/taskDiscovery.ts`; nothing here is hand-written per task. A task's name opens its page with
  that function selected (the same `cc-fn:<module>` key TaskRunner remembers).

  Collapsed by default: ~50 tasks × 3 lines would push every guide below the fold, and the panel opens
  for guides first.
-->
<script setup lang="ts">
import { computed, onMounted, ref } from 'vue'
import { useRouter } from 'vue-router'
import { useTaskDefsStore } from '../stores/taskDefs'
import { whichStepGroups, type WhichStepTask } from '../utils/taskDiscovery'
import { profileStorage } from '../utils/profileStorage'
import { closeGuides } from '../lib/guideOpen'

const defs = useTaskDefsStore()
const router = useRouter()
onMounted(() => { void defs.ensureLoaded() })

const open = ref(false)
const groups = computed(() => whichStepGroups(defs.all()))

async function openTask(t: WhichStepTask) {
  profileStorage.setItem(`cc-fn:${t.module}`, t.task)
  closeGuides()
  await router.push(t.path)
}
</script>

<template>
  <section class="gd-group ws">
    <button class="ws-toggle cc-section-toggle" @click="open = !open"
            v-tooltip.top="'What each step is for, and when to use another'">
      <i :class="['pi', open ? 'pi-chevron-down' : 'pi-chevron-right']" />
      <span class="cc-eyebrow cc-fs-2xs">Which step?</span>
    </button>

    <template v-if="open">
      <p v-if="!groups.length" class="cc-empty cc-fs-xs">No task descriptions loaded</p>
      <div v-for="g in groups" :key="g.path" class="ws-page">
        <h4 class="ws-page-head cc-eyebrow cc-fs-2xs">{{ g.page }}</h4>
        <div v-for="t in g.tasks" :key="t.funName" class="ws-task">
          <button class="ws-name" @click="openTask(t)" v-tooltip.top="`Open ${g.page} with this task`">
            {{ t.label }}
          </button>
          <span class="cc-muted cc-fs-xs">{{ t.purpose }}</span>
          <p v-for="l in t.useWhen" :key="'u' + l" class="ws-line ws-use cc-fs-xs">{{ l }}</p>
          <p v-for="l in t.notWhen" :key="'n' + l" class="ws-line ws-not cc-fs-xs">{{ l }}</p>
        </div>
      </div>
    </template>
  </section>
</template>

<style scoped>
.ws-toggle { padding: 0.3rem 0.2rem; }
.ws-page { margin: 0.35rem 0 0 1.1rem; }
.ws-page-head { margin: 0.3rem 0 0.1rem; }
.ws-task { padding: 0.3rem 0.2rem; border-top: 1px solid var(--cc-border); }
/* a link, not a button: same as the guide picker's "X first" link — it leaves the dialog */
.ws-name {
  background: none; border: none; padding: 0; margin-right: 0.4rem;
  font: inherit; font-weight: 600; color: var(--cc-accent-soft); cursor: pointer; text-decoration: underline;
}
.ws-name:hover { color: var(--cc-accent); }
.ws-line { margin: 0.1rem 0 0 0.6rem; line-height: 1.35; }
/* the glyph and the colour both change, so colour is never the only cue (docs/UI.md → Severity) */
.ws-use::before { content: '✓ '; color: var(--cc-sev-ok); }
.ws-not { color: var(--cc-text-dim); }
.ws-not::before { content: '✗ '; color: var(--cc-sev-warn); }
</style>
