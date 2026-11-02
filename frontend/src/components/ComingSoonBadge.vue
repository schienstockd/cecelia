<!--
  A small "not shipped yet" pill — the ONE way to signal an incomplete surface. Made because the app
  had grown four separate ways of saying it (a task-gating string in `utils/taskGating.ts`, a
  parenthetical suffix in `AppSidebar.vue`'s nav tooltips, a `.wn-sketch` line in `WhatNewCard.vue`,
  and inline prose in `ChainModule.vue`), each a different colour and shape. Kept tiny and neutral so
  it can sit next to a title, a nav item, a guide row, or a chain node without pulling focus.

  Props: `label` (default "Coming soon") and `note` (a short qualifier appended after an em-dash,
  e.g. "0.2.1"). Wrap in a v-tooltip on the host for anything longer than that.

  `tier` makes it the feature-maturity badge (docs/todo/FEATURE_MATURITY_PLAN.md): "Coming soon" is the
  bottom of the same scale, so one pill says all of it. The label comes from the tier
  (`lib/guides/tiers.ts`); `exempt` renders nothing (no scientific result, no badge).

    <ComingSoonBadge />
    <ComingSoonBadge label="Coming soon" note="0.2.1" />
    <ComingSoonBadge label="Walkthrough in progress" />
    <ComingSoonBadge tier="experimental" />
-->
<script setup lang="ts">
import { computed } from 'vue'
import type { Tier } from '../lib/guides/types'
import { TIER_LABEL } from '../lib/guides/tiers'

const props = withDefaults(defineProps<{
  label?: string
  note?: string
  tier?: Tier
}>(), { label: 'Coming soon' })

const text = computed(() => props.tier ? TIER_LABEL[props.tier] : props.label)
</script>

<template>
  <span v-if="tier !== 'exempt'" :class="['cc-soon-badge', tier && `tier-${tier}`]"
        :aria-label="note ? `${text} — ${note}` : text">
    {{ text }}<template v-if="note"> — {{ note }}</template>
  </span>
</template>

<style scoped>
.cc-soon-badge {
  display: inline-flex; align-items: center;
  font-size: var(--cc-fs-2xs); font-weight: 500; line-height: 1;
  color: var(--cc-text-dim);
  background: var(--cc-surface-2);
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-pill);
  padding: 0.1rem 0.4rem;
  white-space: nowrap;
  letter-spacing: 0.01em;
}
/* the one tier that asks for every result to be checked stands out — by its word first, colour second */
.cc-soon-badge.tier-experimental { color: var(--cc-sev-warn); }
</style>
