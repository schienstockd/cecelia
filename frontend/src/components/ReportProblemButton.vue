<!--
  "Something wrong?" — a prefilled GitHub bug report for one feature (docs/todo/FEATURE_MATURITY_PLAN.md
  → *Something wrong?*). Sits beside a page's maturity link (`MaturityIndicator.vue`), a picked task's badge
  (TaskRunner) and each expanded row of the map's Maturity section (`MaturityMap.vue`).

  Opens a small popover rather than the link itself, for ONE reason: the last task's settings go in only
  on an explicit tick. GitHub is public and settings carry file paths and project names, so the tick is
  never remembered — off every time the popover opens. Everything else the report carries (feature,
  tier, version, OS, last task name) is filled in silently; the URL is `lib/links.ts` →
  `problemReportUrl`, pure and tested there.

    <ReportProblemButton feature="Gate populations" tier="beta" page="/gate" />
    <ReportProblemButton feature="…" tier="experimental" text />   ← a worded button, for the map rows
-->
<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import TeleportPopover from './TeleportPopover.vue'
import CcToggle from './CcToggle.vue'
import { useAppControlStore } from '../stores/appControl'
import { useTaskStore } from '../stores/tasks'
import { getSystemEnvs } from '../utils/systemEnvs'
import { problemReportUrl } from '../lib/links'
import { TIER_LABEL } from '../lib/guides/tiers'
import type { Tier } from '../lib/guides/types'

const props = defineProps<{
  feature: string
  tier: Tier
  page?: string
  text?: boolean          // "Something wrong?" in words, not the bare glyph
}>()

const appCtl = useAppControlStore()
const tasks = useTaskStore()

const open = ref(false)
const anchor = ref<HTMLElement | null>(null)
const withParams = ref(false)
const platform = ref('')

// the tick is per report, never remembered (see the header); the platform is read once, cached upstream
watch(open, o => {
  if (!o) return
  withParams.value = false
  void getSystemEnvs().then(e => { platform.value = e?.platform ?? '' })
})

// this tab's last dispatch, else the newest live or session row (not a history row from the run log)
const last = computed(() => tasks.lastStarted ?? tasks.tasks.find(t => !t.history) ?? null)
const canParams = computed(() => !!last.value && !last.value.paramsUnknown && Object.keys(last.value.params).length > 0)

const url = computed(() => problemReportUrl({
  feature: props.feature,
  tier: TIER_LABEL[props.tier],
  page: props.page,
  version: appCtl.updateCurrent || undefined,
  platform: platform.value || undefined,
  lastTask: last.value ? `${last.value.funName} — ${last.value.status}` : undefined,
  params: withParams.value && canParams.value ? last.value!.params : null,
}))
</script>

<template>
  <button ref="anchor" :class="['cc-btn cc-btn-bare', text ? 'cc-btn-micro cc-fs-2xs' : 'cc-btn-icon']"
          @click="open = !open" v-tooltip.bottom="'Something wrong? Report it on GitHub'"
          aria-label="Report a problem">
    <i class="pi pi-bug" /><template v-if="text"> Something wrong?</template>
  </button>
  <TeleportPopover v-model="open" :anchor="anchor" placement="bottom-end">
    <div class="rp">
      <span class="cc-eyebrow cc-fs-2xs">Report: {{ feature }}</span>
      <span class="cc-muted cc-fs-2xs">Sends its tier, your version, OS and last task</span>
      <CcToggle v-if="canParams" v-model="withParams" label="Include the last task’s settings"
                v-tooltip.bottom="'Settings can hold file paths — GitHub is public'" />
      <a class="cc-btn cc-btn-primary cc-btn-dense cc-fs-xs" :href="url" target="_blank" rel="noopener"
         @click="open = false">
        Open on GitHub <i class="pi pi-external-link" />
      </a>
    </div>
  </TeleportPopover>
</template>

<style scoped>
.rp { display: flex; flex-direction: column; align-items: flex-start; gap: 6px; max-width: 18rem; }
</style>
