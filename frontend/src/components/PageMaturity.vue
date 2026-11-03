<!--
  The current page's maturity, in the app header (docs/todo/FEATURE_MATURITY_PLAN.md → P2). In the
  header rather than in each page: pages have no shared header of their own (ModuleLayout, ModulePage
  and the pipeline pages each draw their own top), and the header follows the route, so every page gets
  it from one mount.

    Experimental / Coming soon   the tier dot + word (`ComingSoonBadge tier`), clicking opens the map row
    any other tiered page        a quiet signal glyph that opens the same row — so a page without a
                                 badge doesn't read as "fine" (Beta: spot-check results)
    beside either                "Something wrong?" (`ReportProblemButton`)
    exempt / not a sidebar page  nothing — no scientific result, no row to open

  Which tier and which row: `utils/maturity.ts` → `pageMaturity` (tested).
-->
<script setup lang="ts">
import { computed } from 'vue'
import { useRoute } from 'vue-router'
import ComingSoonBadge from './ComingSoonBadge.vue'
import ReportProblemButton from './ReportProblemButton.vue'
import { pageMaturity, pageBadged } from '../utils/maturity'
import { tierTip } from '../lib/guides/tiers'
import { openMaturityRow } from '../lib/guideOpen'

const route = useRoute()
const m = computed(() => pageMaturity(route.path))
</script>

<template>
  <span v-if="m" class="pm cc-row cc-row-tight">
    <button v-if="pageBadged(m.tier)" class="cc-btn cc-btn-bare cc-btn-dense" @click="openMaturityRow(m.rowId)"
            v-tooltip.bottom="`${tierTip(m.tier)} — show why`">
      <ComingSoonBadge :tier="m.tier" />
    </button>
    <button v-else class="cc-btn cc-btn-bare cc-btn-icon" @click="openMaturityRow(m.rowId)"
            v-tooltip.bottom="`${tierTip(m.tier)} — show why`" aria-label="This page's maturity">
      <i class="pi pi-signal" />
    </button>
    <ReportProblemButton :feature="m.title" :tier="m.tier" :page="route.path" />
  </span>
</template>

<style scoped>
.pm { margin-left: -0.35rem; gap: 0.1rem; }
</style>
