<!--
  THE notice affordance for a canvas plot — "something about THIS render you should know".

  Not an error (a failed request is `error` text on the panel) and not an auto-override (a setting the
  renderer had to substitute has its own canonical mechanism — `plots/autoOverride.ts` + the amber
  `.cc-auto-override` marker ON the control, which says far more than a note ever could). This is the
  third thing: a caution or an observation about what was just drawn — it is heavy, or it came back
  partly empty.

  It existed three ways before this component, which is why it is one now:
    · GatePairsPanel  — a `.pairs-warn` tinted banner ("Large matrix — may be slow to load")
    · SummaryPanel    — a `.sp-foot-note` chip, amber with a triangle for overrides, muted and
                        icon-less for empty series (so the two disagreed on whether a notice has an icon)
    · the Flow views  — bare `.cc-muted-warn` paragraphs
  Same concept, three shapes, three wordings. Add a notice HERE rather than a fourth span.

  TWO VARIANTS, because the two placements are genuinely different and collapsing them would be worse
  than the duplication: `chip` sits inline in the panel's chrome row (compact, right-aligned by the
  host), `banner` is a full-width tinted bar above the plot for something you should see before you
  wait for it. Tone is `warn` (amber, with the triangle) or `muted` (an observation, no icon).

  The tooltip is where the ACTION goes — the text says what, the tip says what to do about it — the
  same split `overrideTooltip` uses. Keep both to the house budget (docs/ui/COPY.md).
-->
<script setup lang="ts">
withDefaults(defineProps<{
  text: string
  tip?: string
  tone?: 'warn' | 'muted'
  variant?: 'chip' | 'banner'
}>(), { tone: 'warn', variant: 'chip', tip: '' })
</script>

<template>
  <div class="pn" :class="[`pn-${variant}`, tone === 'warn' ? 'pn-warn' : 'cc-muted']"
       v-tooltip.top="tip || undefined">
    <i v-if="tone === 'warn'" class="pi pi-exclamation-triangle" />
    <span>{{ text }}</span>
  </div>
</template>

<style scoped>
.pn { display: flex; align-items: center; gap: 6px; }
.pn[title], .pn { cursor: help; }
/* inline in a chrome row — the host positions it (e.g. margin-left:auto) */
.pn-chip { font-size: var(--cc-fs-2xs); }
.pn-chip.pn-warn { color: var(--cc-sev-warn); }
/* a bar above the plot: tinted, so it reads before the plot finishes */
.pn-banner { padding: 4px 10px; font-size: var(--cc-fs-xs); border-bottom: 1px solid var(--cc-border); }
.pn-banner.pn-warn { color: var(--cc-warn); background: color-mix(in srgb, var(--cc-warn) 12%, transparent); }
</style>
