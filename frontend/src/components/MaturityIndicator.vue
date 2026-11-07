<!--
  A page's or a function's maturity, and "Something wrong?" beside it — ONE look for both, so a picked
  function reads the same as the page it runs on (docs/todo/FEATURE_MATURITY_PLAN.md → P2).

    any tier                     its dot alone (`ComingSoonBadge tier dot`, the map's colour per tier;
                                 the tooltip names it), opens the map row — Beta gets one too, so an
                                 unmarked page never reads as "fine"
    beside it                    "Something wrong?" (`ReportProblemButton`)
    `maturity` null              nothing — exempt (no scientific result), no row to open

  Hosts: the app header (the route's `pageMaturity`), TaskRunner's Function heading (`taskMaturity`).
  Which tier and which row: `utils/maturity.ts` (tested).

    <MaturityIndicator :maturity="pageMaturity(route.path)" :page="route.path" />
-->
<script setup lang="ts">
import ComingSoonBadge from './ComingSoonBadge.vue'
import ReportProblemButton from './ReportProblemButton.vue'
import type { MaturityRef } from '../utils/maturity'
import { tierTip } from '../lib/guides/tiers'
import { openMaturityRow } from '../lib/guideOpen'

defineProps<{ maturity: MaturityRef | null; page: string }>()
</script>

<template>
  <span v-if="maturity" class="mi cc-row cc-row-tight">
    <button class="cc-btn cc-btn-bare cc-btn-dense" @click="openMaturityRow(maturity.rowId)"
            v-tooltip.bottom="`${tierTip(maturity.tier)} — show why`">
      <ComingSoonBadge :tier="maturity.tier" dot />
    </button>
    <ReportProblemButton :feature="maturity.title" :tier="maturity.tier" :page="page" />
  </span>
</template>

<style scoped>
.mi { gap: 0.1rem; }
</style>
