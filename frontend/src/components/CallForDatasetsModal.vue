<script setup lang="ts">
/**
 * The "Call for Datasets" modal — a discoverable list of capabilities Cecelia can build once a
 * real dataset lands.
 *
 * Reachable from three places, all named in `docs/todo/CALL_FOR_DATASETS_PLAN.md`:
 * - the icon in `AppHeader`'s help row (a browse-first entry),
 * - a `?ask=<id>` query-string deep link from a task-param vis-aid placeholder,
 * - and — later — a small chip on the drift-rigid vis aid's fourth column.
 *
 * NOT a wish-list or a public roadmap — see the header of `lib/callForDatasets.ts`. Entries are
 * capabilities whose engineering decision is unblocked as soon as a validation dataset arrives.
 * Each card's primary action opens a prefilled GitHub issue asking the two things we cannot guess
 * (what the imaging looks like, and a cloud link to a sample); the modal does not host files.
 */
import { computed, nextTick, onMounted, ref, watch } from 'vue'
import BaseModal from './BaseModal.vue'
import { CALL_FOR_DATASETS, datasetAskUrl, type CapabilityAsk } from '../lib/callForDatasets'
import { callForDatasetsFocusId, closeCallForDatasets } from '../lib/callForDatasetsOpen'

/**
 * The card DOM refs, keyed by ask id — the deep-link scroll target on open. Kept as a plain
 * `Record` (not `useTemplateRef`) because we build the keys from data rather than declaring one
 * per card, which the template-ref helper does not support directly.
 */
const cardRefs = ref<Record<string, HTMLElement | null>>({})
function bindCard(id: string, el: Element | null) {
  cardRefs.value[id] = el instanceof HTMLElement ? el : null
}

/**
 * Scroll the focused card into view once the DOM has painted. `smooth` behaviour rather than
 * instant: the visible slide is what makes the deep-link legible — otherwise a user who clicked a
 * chip on the drift vis aid sees the modal open with no confirmation that it landed anywhere in
 * particular.
 */
async function scrollFocus() {
  const id = callForDatasetsFocusId.value
  if (!id) return
  await nextTick()
  cardRefs.value[id]?.scrollIntoView({ behavior: 'smooth', block: 'start' })
}
onMounted(scrollFocus)
watch(callForDatasetsFocusId, scrollFocus)

/** True when this card is the deep-link target — used to draw the "you came here for this" mark
 *  in the card header. Purely visual; the scroll does the actual work. */
function isFocused(a: CapabilityAsk): boolean {
  return callForDatasetsFocusId.value === a.id
}

const asks = computed<CapabilityAsk[]>(() => CALL_FOR_DATASETS)
</script>

<template>
  <BaseModal title="Call for Datasets" icon="pi-database" width="640px" height="80vh"
             @close="closeCallForDatasets">
    <div class="cfd-body">
      <p class="cfd-intro cc-muted cc-fs-xs">
        These are capabilities Cecelia can build once a real dataset lands. If your imaging matches
        one of these, opening an issue with a cloud link to a sample is what moves it.
      </p>

      <section v-for="a in asks" :key="a.id"
               :ref="el => bindCard(a.id, el as Element | null)"
               class="cfd-card" :class="{ 'is-focused': isFocused(a) }">
        <header class="cfd-head">
          <h3 class="cfd-title">{{ a.title }}</h3>
          <p class="cfd-one cc-muted cc-fs-sm">{{ a.oneLiner }}</p>
        </header>
        <p class="cfd-blurb cc-fs-sm">{{ a.blurb }}</p>
        <div class="cfd-scenery cc-fs-xs">
          <span class="cc-eyebrow cc-fs-2xs">What we need</span>
          <p>{{ a.sceneryHint }}</p>
        </div>
        <footer class="cfd-actions">
          <a class="cc-btn cc-btn-primary cc-btn-sm"
             :href="datasetAskUrl(a)" target="_blank" rel="noopener">
            <i class="pi pi-github" /> Open a GitHub issue
          </a>
          <span class="cc-muted cc-fs-2xs cfd-plan">Plan: {{ a.planPath }}</span>
        </footer>
      </section>
    </div>
  </BaseModal>
</template>

<style scoped>
.cfd-body { display: flex; flex-direction: column; gap: 1rem; padding: 0.6rem 0.2rem 0.4rem; }
.cfd-intro { margin: 0 0.2rem; }

/* Cards read as separate cards without a heavy chrome: a border, a rest state, and a soft focus
   ring on the deep-link target. */
.cfd-card {
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-md);
  padding: 0.75rem 0.9rem;
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  background: var(--cc-surface-1);
  scroll-margin-top: 0.4rem;
}
.cfd-card.is-focused {
  border-color: var(--cc-accent);
  box-shadow: 0 0 0 1px var(--cc-accent) inset;
}

.cfd-head { display: flex; flex-direction: column; gap: 0.15rem; }
.cfd-title { margin: 0; font-size: 0.95rem; font-weight: 600; }
.cfd-one { margin: 0; }
.cfd-blurb { margin: 0; line-height: 1.4; }

/* "What we need" is the operational half of the card — a soft-tinted panel so it reads as a form
   the reader can answer, not another line of blurb. */
.cfd-scenery {
  background: var(--cc-surface-2);
  border-radius: var(--cc-radius-sm);
  padding: 0.5rem 0.7rem;
  display: flex;
  flex-direction: column;
  gap: 0.2rem;
}
.cfd-scenery p { margin: 0; line-height: 1.35; }

/* Primary action on the left, plan pointer on the right — a footer, not a floating chip on the
   card. Keeps the action tied to the card that offered it. */
.cfd-actions {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 0.5rem;
  flex-wrap: wrap;
}
.cfd-plan { min-width: 0; overflow-wrap: anywhere; }
</style>
