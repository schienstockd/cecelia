<!--
  WhatNewCard — one card in the What's New modal. Used for both release-notes ("update") and
  tips. Content is driven by `frontend/src/lib/whatsNew.ts`; layout follows the ClaudeOverviewDialog
  card pattern.

  `sketchAnimation.id` resolves against feijoa's `sketches` catalogue; unknown ids fall through to
  the grey "coming soon" placeholder. `statsAnnotation` is a typed slot still awaiting content
  (STATS_ANNOTATIONS_PLAN.md).
-->
<script setup lang="ts">
import { computed } from 'vue'
import { SketchCanvas, sketches, type SketchDefinition } from 'feijoa'
import { formatCardDate, renderMarkdown, type WhatNewCard, CECELIA_ISSUES_URL } from '../lib/whatsNew'
import { useSettingsStore } from '../stores/settings'
import CcToggle from './CcToggle.vue'

const props = defineProps<{ card: WhatNewCard }>()

const kindLabel = computed(() => (
  props.card.kind === 'update' ? 'NEW' :
  props.card.kind === 'fix'    ? 'FIX' :
                                 'TIP'
))
const kindTone = computed(() => 'wn-kind-' + props.card.kind)
const dateLabel = computed(() => formatCardDate(props.card.publishedAt))
const issueUrl = computed(() => props.card.issueUrl ?? CECELIA_ISSUES_URL)
const bodyHtml = computed(() => renderMarkdown(props.card.bodyMd))
const isTip = computed(() => props.card.kind === 'tip')

// Resolve `sketchAnimation.id` → feijoa's catalogue; unknown id (or none) → null → placeholder.
const sketch = computed<SketchDefinition | null>(() => {
  const id = props.card.sketchAnimation?.id
  if (!id) return null
  return sketches[id] ?? null
})

// The "show tips on launch" opt-out is a store toggle bound to a checkbox on every tip card.
// Bound as `!tipsOnLaunch` so the checkbox reads "Don't show tips on launch" (opt-out language).
const settings = useSettingsStore()
const tipsOptOut = computed({
  get: () => !settings.tipsOnLaunch,
  set: (v: boolean) => { settings.tipsOnLaunch = !v },
})
</script>

<template>
  <article class="wn-card cc-card cc-card-2">
    <header class="wn-head">
      <span class="wn-kind" :class="kindTone">{{ kindLabel }}</span>
      <h3 class="wn-title">{{ card.title }}</h3>
      <span v-if="card.releaseVersion" class="wn-version">{{ card.releaseVersion }}</span>
    </header>

    <div v-if="dateLabel" class="wn-date cc-muted cc-fs-2xs">{{ dateLabel }}</div>

    <!-- sketchAnimation — feijoa SketchCanvas when the id resolves; grey box otherwise. -->
    <div v-if="sketch" class="wn-sketch wn-sketch-render">
      <SketchCanvas :definition="sketch" />
    </div>
    <div v-else-if="card.sketchAnimation" class="wn-sketch">Animation coming soon</div>

    <p v-if="card.description" class="wn-description">{{ card.description }}</p>

    <!-- Client-rendered markdown via marked (GFM). Trusted source: our own release bodies. -->
    <div v-if="bodyHtml" class="wn-body" v-html="bodyHtml" />

    <div v-if="card.steps?.length" class="wn-steps">
      <div class="wn-steps-label cc-muted cc-fs-xs">Try it</div>
      <ol>
        <li v-for="(step, i) in card.steps" :key="i">{{ step }}</li>
      </ol>
    </div>

    <footer class="wn-foot">
      <a v-if="card.releaseUrl" :href="card.releaseUrl" target="_blank" rel="noopener" class="wn-link cc-muted">
        View on GitHub <i class="pi pi-external-link" />
      </a>
      <CcToggle v-if="isTip" v-model="tipsOptOut" label="Don't show tips on launch" class="wn-optout cc-muted cc-fs-xs" />
      <a :href="issueUrl" target="_blank" rel="noopener" class="wn-link wn-link-right cc-muted">
        Report a problem <i class="pi pi-external-link" />
      </a>
    </footer>
  </article>
</template>

<style scoped>
.wn-card { padding: 14px 16px; }

.wn-head { display: flex; align-items: center; gap: 8px; }
.wn-kind {
  font-size: var(--cc-fs-2xs);
  font-weight: 700;
  letter-spacing: 0.05em;
  padding: 2px 6px;
  border-radius: var(--cc-radius-sm);
  color: #ffffff;
}
.wn-kind-update { background: var(--cc-accent); }
.wn-kind-tip    { background: var(--cc-sev-warn); color: #1a1a1a; }
.wn-kind-fix    { background: var(--cc-sev-ok);   color: #ffffff; }
.wn-title { flex: 1; margin: 0; font-size: var(--cc-fs-lg); font-weight: 600; color: var(--cc-text); }
.wn-version {
  font-family: ui-monospace, SFMono-Regular, 'SF Mono', Menlo, monospace;
  font-size: var(--cc-fs-xs);
  color: var(--cc-text-dim);
  padding: 2px 6px;
  background: var(--cc-surface-2);
  border-radius: var(--cc-radius-sm);
}

.wn-date { margin-top: 2px; }

.wn-sketch {
  margin-top: 10px;
  height: 170px;
  border-radius: var(--cc-radius-md);
  background: var(--cc-surface-2);
  border: 1px dashed var(--cc-border);
  display: flex;
  align-items: center;
  justify-content: center;
  color: var(--cc-text-dim);
  font-size: var(--cc-fs-sm);
}
/* Real sketch — drop the placeholder border/height, let SketchCanvas scale to the card width. */
.wn-sketch.wn-sketch-render {
  height: auto;
  border: none;
  background: transparent;
  padding: 0;
}
.wn-sketch.wn-sketch-render :deep(.feijoa-sketch) { width: 100%; height: auto; }

.wn-description { margin: 10px 0 6px; color: var(--cc-text); font-size: var(--cc-fs-md); line-height: 1.5; }

.wn-body {
  margin: 8px 0 0;
  color: var(--cc-text);
  font-size: var(--cc-fs-md);
  line-height: 1.55;
}
.wn-body :deep(h1),
.wn-body :deep(h2),
.wn-body :deep(h3) { font-size: var(--cc-fs-md); font-weight: 600; margin: 12px 0 4px; }
.wn-body :deep(p) { margin: 6px 0; }
.wn-body :deep(ul),
.wn-body :deep(ol) { margin: 6px 0; padding-left: 20px; }
.wn-body :deep(li) { margin: 2px 0; }
.wn-body :deep(a) { color: var(--cc-accent); }
.wn-body :deep(code) {
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  font-size: 0.9em;
  background: var(--cc-surface-2);
  padding: 0 4px;
  border-radius: var(--cc-radius-xs);
}
.wn-body :deep(pre) {
  background: var(--cc-surface-2);
  padding: 8px 10px;
  border-radius: var(--cc-radius-sm);
  overflow-x: auto;
}

.wn-steps { margin-top: 10px; }
.wn-steps-label { margin-bottom: 2px; }
.wn-steps ol { margin: 0; padding-left: 20px; color: var(--cc-text); font-size: var(--cc-fs-md); line-height: 1.55; }

.wn-foot {
  display: flex;
  gap: 12px;
  margin-top: 12px;
  padding-top: 8px;
  border-top: 1px solid var(--cc-border);
}
.wn-link {
  text-decoration: none;
  display: inline-flex;
  align-items: center;
  gap: 4px;
}
.wn-link:hover { color: var(--cc-accent); }
.wn-link .pi { font-size: 0.85em; }
.wn-link-right { margin-left: auto; }
</style>
