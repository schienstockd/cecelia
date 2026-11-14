<!--
  How far to trust each feature (docs/todo/FEATURE_MATURITY_PLAN.md): the tier legend, then every feature
  as an expandable row (`MaturityMap.vue`) with its evidence, the checklist to the next tier, the data
  that would help, and "Something wrong?". Opened from any maturity dot (`MaturityIndicator.vue` →
  `openMaturityRow`, which opens that row) and from Help ▾ → Feature maturity. Open flag:
  `lib/maturityOpen.ts`.
-->
<script setup lang="ts">
import BaseModal from './BaseModal.vue'
import MaturityMap from './MaturityMap.vue'
import { TIER_LEGEND } from '../utils/guideMap'
import { tierTip } from '../lib/guides/tiers'
import { closeMaturity } from '../lib/maturityOpen'
</script>

<template>
  <BaseModal title="Feature maturity" icon="pi-verified" width="820px" height="80vh" @close="closeMaturity">
    <div class="md-legend cc-row cc-row-loose cc-muted cc-fs-2xs" aria-label="Maturity legend">
      <span v-for="l in TIER_LEGEND" :key="l.tier" class="cc-row cc-row-tight" v-tooltip.top="tierTip(l.tier)">
        <span :class="['cc-dot', l.dot]" /> {{ l.label }}
      </span>
    </div>
    <MaturityMap />
  </BaseModal>
</template>

<style scoped>
.md-legend { flex-wrap: wrap; margin-bottom: 0.6rem; }
</style>
