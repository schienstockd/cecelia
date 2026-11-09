<script setup lang="ts">
// A short line with its reasoning on hover — the shape four places had each hand-rolled.
//
// `icon + short text + v-tooltip(detail)` appeared in `ParamRenderer` (param advisories),
// `PhysicalSizeDialog` (the calibration warning and the re-run note) and `TaskPreviewControls`
// (the preview notice and each caveat). Same statement, four spellings, and two of them had already
// drifted: PhysicalSizeDialog hardcoded `pi-exclamation-triangle` instead of reading the severity
// model, so a change to the palette or the icon set would have missed it.
//
// Colour is NEVER the only cue (lib/severity.ts, WCAG 1.4.1) — the icon shape carries the meaning and
// the text says it outright, which is why the icon is not optional.
//
// TWO KINDS, deliberately distinct:
//   * a SEVERITY note — "we looked at your data and here is what we found". Icon and colour come
//     from the severity model.
//   * an INFO note (`severity` omitted) — static guidance that was never checked against anything,
//     e.g. what a dropdown option means. It must NOT borrow a severity: `ok` renders a green
//     check-circle, which would claim a verdict nobody reached.
import { computed } from 'vue'
import { SEVERITY, type Severity } from '../lib/severity'

// No `withDefaults` — the only prop that had one was `placement`, and it never reached PrimeVue.
const props = defineProps<{
  short: string
  detail?: string
  severity?: Severity
  /** Override the icon — for a note whose meaning is not severity (e.g. the re-run history hint). */
  icon?: string
  /** Labelled lines under the short text — a finding's "Next: …" / "These movies: …". Shown, not on
   *  hover: they are what to do, which a list of findings is read for. */
  lines?: { label: string; text: string }[]
}>()

const iconClass = computed(() =>
  props.icon ?? (props.severity ? SEVERITY[props.severity].icon : 'pi-info-circle'))
const color = computed(() => props.severity ? SEVERITY[props.severity].color : undefined)
</script>

<template>
  <span class="inline-note" :class="[severity ? `sev-${severity}` : 'cc-muted', { 'has-lines': lines?.length }]">
    <i class="pi" :class="iconClass" :style="color ? { color } : undefined" />
    <!-- the tooltip hangs off the TEXT, not the row: a host may add its own trailing control with a
         tooltip of its own, and a row-level one fires on top of it (docs/UI.md → nested tooltips)

         `.bottom` is FIXED, and there used to be a `placement` prop instead. It never worked: it
         passed `position` inside the tooltip VALUE object, and PrimeVue reads `position` only off
         `options.arg` (tooltip/index.mjs `getModifiers`), never off the value. So all seven call
         sites that set one were silently getting the bare default, `alignRight` — the one chain that
         ends by re-applying itself with no bounds check. A note annotates the control ABOVE it, so
         `.bottom` points the detail away from that control; and being fixed, it is one decision
         rather than a knob every call site re-guesses. See `docs/UI.md` → *Tooltip placement*. -->
    <span v-if="!lines?.length" v-tooltip.bottom="detail">{{ short }}</span>
    <span v-else class="note-body">
      <span v-tooltip.bottom="detail">{{ short }}</span>
      <span v-for="l in lines" :key="l.label" class="note-line cc-fs-xs"><span class="cc-muted">{{ l.label }}</span> {{ l.text }}</span>
    </span>
    <slot />
  </span>
</template>

<style scoped>
.inline-note {
  display: inline-flex;
  /* `center`, not `baseline`: matches `.param-advisory` / `.md-note`, the blocks this replaces. */
  align-items: center;
  /* literal, because there is no `--cc-gap-*` scale — 0.3rem is what every site here already used
     (a `cssTokens` test fails on a token that does not exist, which is how an invented one showed up) */
  gap: 0.3rem;
}
.inline-note > .pi { font-size: 0.85em; }
/* Let the short-text span honour `\n` so a caller can break a message into two lines when it
   carries two facts (see `paramAdvisors.ts::pyramidLevelsAdvisory`). Harmless when the caller has
   no newline — `pre-line` collapses ordinary whitespace runs the same as the default `normal`. */
.inline-note > span { white-space: pre-line; }
/* with lines: the icon sits by the first line, the lines stack under the short text in body colour */
.inline-note.has-lines { align-items: flex-start; }
.inline-note.has-lines > .pi { margin-top: 0.25em; }
.note-body { display: flex; flex-direction: column; gap: 0.1rem; }
.note-line { color: var(--cc-text); }
/* Colour follows the severity, but never alone — the icon shape already carries it. */
.inline-note.sev-warn { color: var(--cc-sev-warn); }
.inline-note.sev-fail { color: var(--cc-sev-fail); }
</style>
