<script setup lang="ts">
// A model review's points on a Blackboard entry (utils/modelReview.ts), grouped by category: each point
// an InlineNote (a question is a question, an observation a warning), what to do after an arrow, its refs
// as clickable chips; then a failed last run, "nothing beyond the checks", and the closing interpretation
// (what the record would support, or the acquisition verdict). Used by the claim popover (ClaimStrip) and
// the acquisition check (AcqCheckDialog). Marking a point Good / Bad / Unsure happens on the Blackboard.
import { computed } from 'vue'
import InlineNote from './InlineNote.vue'
import KiwiRefChip from './kiwi/KiwiRefChip.vue'
import { refKey } from '../utils/kiwiTurn'
import { reviewGroups, type ModelReview } from '../utils/modelReview'

// supportsLabel heads the closing interpretation: what a claim's record would support, or an acquisition verdict.
// supportsLabel '' leaves the interpretation to the caller. shown(text) is how a point's text reads — the acquisition check swaps image names for uids with it.
const props = withDefaults(defineProps<{ review: ModelReview; categoryLabels: Record<string, string>; supportsLabel?: string;
                                         shown?: (text: string) => string }>(),
  { supportsLabel: 'Would support', shown: (t: string) => t })
const groups = computed(() => reviewGroups(props.review, props.categoryLabels))
</script>

<template>
  <div v-for="g in groups" :key="g.id" class="mr-cat">
    <span class="mr-cat-head">{{ g.label }}</span>
    <div class="mr-list">
      <template v-for="x in g.items" :key="x.sectionId ?? x.text">
        <InlineNote :short="shown(x.text)" :severity="x.kind === 'question' ? undefined : 'warn'"
                    :icon="x.kind === 'question' ? 'pi-question-circle' : undefined" />
        <span v-if="x.action" class="mr-action cc-muted">→ {{ shown(x.action) }}</span>
        <div v-if="x.refs.length" class="cc-row cc-row-tight mr-refs">
          <KiwiRefChip v-for="r in x.refs" :key="refKey(r.ref)" :kiwi-ref="r.ref" :result="r.result" :seen="r.seen" />
        </div>
      </template>
    </div>
  </div>
  <InlineNote v-if="review.lastError" :short="`The last run failed: ${review.lastError}`" severity="fail" />
  <InlineNote v-if="review.status === 'done' && !review.items.length" icon="pi-minus-circle"
              :short="review.abstain && review.note ? `Nothing raised — ${shown(review.note)}` : 'Nothing beyond the checks'" />
  <span v-if="review.supports && supportsLabel" class="cc-muted mr-supports">{{ supportsLabel }}: {{ shown(review.supports) }}</span>
</template>

<style scoped>
.mr-cat { display: flex; flex-direction: column; gap: 2px; }
.mr-cat-head { font-weight: 600; }
.mr-list { display: flex; flex-direction: column; gap: 3px; padding-left: 1.2rem; }
.mr-action { padding-left: 1.2rem; }
.mr-refs { padding-left: 1.2rem; }
.mr-supports { font-style: italic; }
</style>
