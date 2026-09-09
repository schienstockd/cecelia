<!--
  Did this model converge, and is each loss term earning its place — the per-epoch loss, one line per
  term.

  Per TERM, not just the total, because the total is the one curve that cannot answer the question
  you ask a loss curve. Coastal optimises a weighted sum (intensity · temporal · variance ·
  foreground · …) and three of those weights are task params, so "is this term adding anything" is
  the reason to look at all.

  **Contribution, not the raw term, by default.** Coastal's history records each term BEFORE its
  weight; the total is the weighted sum. So a raw curve at 0.9 with weight 0 contributes exactly
  nothing while one at 0.05 with weight 2.0 contributes twice as much — reading the raw curves side
  by side would rank them backwards. `lossWeights` travels in the manifest for precisely this, and
  the Raw toggle is there for tuning a single term's own scale.

  A PLOT, not a chart inside the vault's details modal, and that is the point of it being a registry
  view: it lands on the canvas with the panel chrome, the zoom, CSV/PNG/SVG export and the board's
  PDF export already attached. A chart in a modal would need every one of those written again, worse.

  **A converged run and a stalled one look identical until you subtract the floor.** Every BCE term
  fits a SOFT target, so its minimum is that target's own entropy — a constant of the DATA. Measured
  on flow.cyto: `foreground` settles at 0.2651 against a floor of 0.2650, so the model's entire
  remaining error is 0.0001 and 85% of the plotted TOTAL is a number no model can move. Read raw,
  that run looks like it stopped learning after five epochs. `lossFloors` travels in the manifest and
  the `− floor` toggle (on by default) subtracts it, which is also what makes the zero-anchored axis
  tell the truth. Terms with no floor — the contrastive ones, whose minimum genuinely is 0 — are left
  alone.

  **One knob is on and has no line, by construction.** `foregroundBoundaryWeight` is not a loss term:
  coastal passes it into `ForegroundLoss` as `boundary_weight`, where it pinches the foreground TARGET
  at flow discontinuities. So it never gets a `history` entry and can never be a chip here — asked
  about it and it looked like a bug. What it does instead is move the target, which moves that term's
  FLOOR: measured on fXgbTl at blur 1.0, `floor_foreground` is 0.317 at weight 0 and 0.015 at weight
  1.0, a 21x drop, because a pinched target has lower entropy. Two runs at different weights therefore
  have non-comparable `foreground` curves unless the floor is subtracted — which is what the note
  under the terms row says, rather than leaving the absence to be discovered.

  (Not `lossWeights.boundary`, which IS a term with a curve — coastal's `ConfettiBoundaryLoss`, pinned
  at 0 by this task.)

  **The held-out curve is the one that answers the question.** A training loss only ever says the
  number went down, measured on the frames the weights were just fitted to. When the run had a
  `trainRatio` split, each term also carries a `val_` curve — drawn dashed in the SAME colour as its
  term, because the only thing you read off it is the GAP to its own training line, and a second
  colour would make that a legend lookup instead of a picture.

  Data is the model's own manifest, resolved via `useVaultModel(chosen)` — the composable fetches
  BOTH vaults (flow + denoise) once and dispatches on the picked name, so this plot works for either
  kind without the caller having to know which vault the model came from. Models trained before the
  curves were recorded say so rather than drawing an empty box; the run kept only `finalLoss`, and
  that is not recoverable.

  Denoise (SUPPORT) trains a SINGLE L1+L2 blend, so a pooled model's manifest carries
  `training.epochLosses` — a flat list, one series. A perChannel bundle
  (SUPPORT_PERCHANNEL_PLAN.md D3) trains one model per channel and carries `training.perChannelLosses`
  — a dict keyed by channel name — which this plot renders as one series per channel, with the chip
  row above auto-showing (>1 series) so each channel becomes a toggle. The minus-floor toggle stays
  hidden for denoise either way (SUPPORT has no per-term floors). Every other control — log Y,
  held-out (n/a here since SUPPORT is self-supervised), CSV/PNG/SVG export — is the same for both
  kinds because a loss curve is a loss curve.

  Observable Plot directly, like the cluster HMM panels. The summary `PlotChart` builds from a
  `PlotDataResponse` (server-aggregated CELL data), and its `trend` chart is a LOESS fit — the wrong
  thing for a loss curve, where you want the epochs as they happened, not a smooth through them.
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted, onBeforeUnmount, nextTick, useTemplateRef } from 'vue'
import { usePlotResize } from '../../composables/usePlotResize'
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'
import { rowsToCsv, downloadBlob, downloadDataUrl, elementToImageURL, svgOf } from '../../plots/export'
import { distinctColors } from '../../plots/plot'
import { useDataRefresh } from '../../composables/useDataRefresh'
import { useProjectStore } from '../../stores/project'
import { useVaultModel } from '../../composables/useVaultModel'
import { lossSeries, lossTable } from '../../plots/lossCurves'
import { applyPlotTheme, plotTheme } from '../../plots/overlays'
import type { FlowManifest } from '../../utils/flowManifest'
import { denoiseTrainingSeries, type DenoiseManifest, type DenoiseSeries } from '../../utils/denoiseManifest'
import { convergenceStats } from '../../utils/convergence'

interface TrainState { logY?: boolean; raw?: boolean; minusFloor?: boolean; terms?: string[]
                       model?: string
                       // Detail = log(step) sub-epoch view with initial/plateau/converged-at
                       // markers. On when available (see `hasStepTrace`); toggled off falls back
                       // to the per-epoch line for the classic look. Off by default so first-time
                       // viewers see the familiar shape, then discover Detail via the toggle.
                       detail?: boolean }

// `model` comes from the HOST — the vault owns the selection and its global/local scope, exactly as
// the population manager owns which pops the plots highlight. No picker here: two pickers for one
// thing is how a canvas ends up with panels that disagree about what they are showing.
//
// There used to be a fallback `<select>` for "a surface with no vault", which meant the Analysis
// board. That was a workaround for a missing contract, not a design: the board now declares
// `rail: 'flowModels'` and docks the vault like the flow canvas does, so both hosts supply a model
// and the second picker is gone. `state.model` survives only as the board's LOCAL-scope slot value,
// which the host writes. See docs/todo/CANVAS_MANAGER_RAIL_PLAN.md.
const props = defineProps<{ state: TrainState; model?: string }>()
const project = useProjectStore()

const state = computed(() => props.state)
const logY = computed({ get: () => state.value.logY ?? false, set: v => (state.value.logY = v) })
const raw = computed({ get: () => state.value.raw ?? false, set: v => (state.value.raw = v) })
const detail = computed({ get: () => state.value.detail ?? false,
                          set: v => (state.value.detail = v) })
// Default ON. The floor is the whole reason a converged run reads as a stalled one, so the useful
// view is the one showing what the model actually did; the unadjusted number is one click away.
// Models trained before floors were recorded have none, and `hasFloors` disables the control rather
// than leaving a toggle that silently does nothing.
const minusFloor = computed({ get: () => state.value.minusFloor ?? true,
                              set: v => (state.value.minusFloor = v) })
// the host's pick wins; `state.model` is the board's local-scope slot value, also host-written
const chosen = computed(() => props.model || state.value.model || '')
// One reactive resolver for either vault kind — the plot doesn't care which vault holds the model,
// it just needs the manifest + a kind so it knows which shape to render.
const { kind, manifest, loading, error, refresh } = useVaultModel(chosen)
const flowManifest = computed(() => kind.value === 'flow' ? (manifest.value as FlowManifest | null) : null)
const denoiseManifest = computed(() =>
  kind.value === 'denoise' ? (manifest.value as DenoiseManifest | null) : null)

const host = useTemplateRef<HTMLElement>('host')
// @observablehq/plot is loosely typed for our purposes; keep it as any (its types are large).
let Plot: any = null                                   // eslint-disable-line @typescript-eslint/no-explicit-any
let node: SVGElement | HTMLElement | null = null
const forceLight = ref(false)

const floors = computed(() => flowManifest.value?.lossFloors ?? null)
const hasFloors = computed(() => Object.keys(floors.value ?? {}).length > 0)
// One series builder per kind. Flow → multi-term via `lossSeries` (weights, floors, val). Denoise
// → either one series (pooled: `training.epochLosses`) OR one series per channel (perChannel
// bundles: `training.perChannelLosses`, keyed by channel name). The chip row above the plot
// auto-shows when there is more than one series, giving the user per-channel toggles for free.
const series = computed(() => {
  if (kind.value === 'denoise') return denoiseTrainingSeries(denoiseManifest.value)
  return lossSeries(flowManifest.value?.lossCurves, flowManifest.value?.lossWeights, raw.value,
                    floors.value, minusFloor.value && hasFloors.value)
})
const termOptions = computed<ChipOption[]>(() => series.value.map(s => ({
  value: s.term,
  label: s.term,
  // Weight 0 only reaches here in raw mode — weighted, `lossSeries` drops it, because its weighted
  // curve is the constant 0 and a flat line on the axis reads as "trained to nothing". This tip used
  // to be the only thing saying otherwise, i.e. a caption on a misleading picture.
  tip: s.weight === 0 ? 'weight 0 — off, shown because raw is on'
    : s.floored ? `weight ${s.weight}, less its target's entropy floor`
    : `weight ${s.weight}`,
})))
// Default: everything that is actually on. `undefined` means "not chosen yet", so an explicit empty
// pick is respected (docs/UI.md → Persisting view state).
//
// Recover from a stale kind: `state.terms` persists across vault-kind switches (flow ↔ denoise), so
// a panel that once held flow terms like `['temporal','intensity']` filters to empty against denoise's
// single `'loss'` series and the plot goes permanently black — with NO way to reset from the UI,
// because the chip row is hidden when there is only one term. If the persisted terms have zero
// overlap with the current series (a genuine kind mismatch, not an explicit unchecked-all), fall
// back to defaults for what is actually available.
const terms = computed<string[]>(() => {
  const persisted = state.value.terms
  const defaults = series.value.filter(s => s.weight !== 0).map(s => s.term)
  if (persisted === undefined) return defaults
  const available = new Set(series.value.map(s => s.term))
  const filtered = persisted.filter(t => available.has(t))
  // explicit empty (user unchecked everything) stays empty; a non-empty-but-stale list falls back
  return filtered.length === 0 && persisted.length > 0 ? defaults : filtered
})
const shown = computed(() => series.value.filter(s => terms.value.includes(s.term)))
const rows = computed(() => shown.value.flatMap(s =>
  s.values.map((loss, i) => ({ epoch: i + 1, term: s.term, loss }))))

// ── Detail view (log(step) sub-epoch trace) — data + convergence stats ────────────────────
// Only denoise carries step traces today (see denoiseTrainingSeries). Flow will slot in via
// the same DenoiseSeries.steps/stepValues shape if/when its trainer starts logging them.
type StepSeries = DenoiseSeries & { steps: number[]; stepValues: number[] }
function _hasSteps(s: DenoiseSeries): s is StepSeries {
  return !!(s.steps && s.stepValues && s.steps.length === s.stepValues.length
            && s.steps.length > 20)
}
// True when any shown series carries a step trace — the toggle is hidden otherwise, so we don't
// offer a control that does nothing on old models.
const hasStepTrace = computed(() =>
  kind.value === 'denoise' && (series.value as DenoiseSeries[]).some(_hasSteps))
const detailActive = computed(() => detail.value && hasStepTrace.value)

// Faint per-batch underlay — thin to at most 500 points per term so a big trace doesn't blow the
// SVG up. `stepStride` matches the runner's adaptive thinning: keep the last N ≈ 500 points.
const stepRows = computed(() => {
  if (!detailActive.value) return []
  return (shown.value as DenoiseSeries[]).filter(_hasSteps).flatMap((s) => {
    const stride = Math.max(1, Math.floor(s.steps.length / 500))
    const out = []
    for (let i = 0; i < s.steps.length; i += stride)
      out.push({ step: s.steps[i], term: s.term, loss: s.stepValues[i] })
    return out
  })
})
// Per-term convergence stats (initial/plateau/convergedAt) — one entry per shown series that has
// a step trace. Undefined when the trace is too short to summarise (falls back cleanly).
const convByTerm = computed(() => {
  const out: Record<string, ReturnType<typeof convergenceStats>> = {}
  if (!detailActive.value) return out
  for (const s of (shown.value as DenoiseSeries[]).filter(_hasSteps)) {
    out[s.term] = convergenceStats(s.steps, s.stepValues, { window: 20 })
  }
  return out
})
// Per-term early-stop info — resolved from the manifest.
//   - Pooled bundles: single stoppedEarly/stopEpoch at training level; the sole series is "loss".
//   - PerChannel bundles: perChannelStoppedEarly / perChannelStopEpoch dicts keyed by channel name.
// Returns {epoch, budget} when the term stopped early, else null. Consumed by the Detail render
// to annotate the plot ("stopped @ ep N of E").
const stopByTerm = computed(() => {
  const out: Record<string, { epoch: number; budget: number } | null> = {}
  const tr = denoiseManifest.value?.training
  if (!tr) return out
  const budget = tr.epochBudget ?? tr.epochs ?? 0
  const perChEarly = tr.perChannelStoppedEarly
  const perChStop = tr.perChannelStopEpoch
  for (const s of shown.value) {
    if (perChEarly && perChStop) {
      out[s.term] = perChEarly[s.term]
        ? { epoch: perChStop[s.term] ?? 0, budget }
        : null
    } else {
      out[s.term] = tr.stoppedEarly ? { epoch: tr.stopEpoch ?? 0, budget } : null
    }
  }
  return out
})
// Any shown series stopped early — surfaced as a small caption above the plot even in Simple
// view. Reader shouldn't need to toggle Detail to learn that patience caught the plateau.
const anyStoppedEarly = computed(() =>
  Object.values(stopByTerm.value).some(v => v !== null))

// Per-series caption for the Detail view. HTML rather than in-plot text — in-plot labels of
// three similar-shape curves collide both vertically (labels stack at similar y) and horizontally
// (left-side "converged" text meets right-side "stopped" text on narrow panels). One line per
// shown series, colour matched to the plot's stroke, wraps naturally.
interface DetailCaption { term: string; color: string; text: string }
const detailCaptions = computed<DetailCaption[]>(() => {
  if (!detailActive.value) return []
  const colors = distinctColors(shown.value.map(s => s.term).length)
  return shown.value.map((s, idx) => {
    const c = convByTerm.value[s.term]
    const stop = stopByTerm.value[s.term]
    const parts: string[] = []
    if (c?.convergedAt !== null && c) {
      const drop = isFinite(c.dropFraction) ? Math.round(c.dropFraction * 100) : 0
      parts.push(`converged step ${c.convergedAt} (${drop}% drop)`)
    }
    if (stop && stop.epoch > 0) {
      parts.push(`stopped ep ${stop.epoch}/${stop.budget}`)
    }
    return { term: s.term, color: colors[idx] || colors[0],
             text: parts.length ? parts.join(' · ') : '' }
  }).filter(cap => cap.text)
})
// Moving-avg rows, one per term — the bold line the reader reads. Same colour as the term.
const maRows = computed(() => {
  const out: { step: number; term: string; loss: number }[] = []
  if (!detailActive.value) return out
  for (const s of (shown.value as DenoiseSeries[]).filter(_hasSteps)) {
    const c = convByTerm.value[s.term]
    if (!c) continue
    for (let i = 0; i < c.ma.length; i++) out.push({ step: c.maSteps[i], term: s.term, loss: c.ma[i] })
  }
  return out
})
// Held-out curves, drawn dashed in the SAME colour as their term. The only thing anyone reads off a
// validation curve is the gap to its own training curve, so a second colour would turn the
// comparison into a legend lookup. Denoise (SUPPORT) is self-supervised, no held-out curve exists.
const valRows = computed(() => shown.value.flatMap(s =>
  ((s as { val?: number[] }).val ?? []).map((loss, i) => ({ epoch: i + 1, term: s.term, loss }))))
const hasVal = computed(() => shown.value.some(s => (s as { val?: number[] }).val?.length))
// Non-zero only. At 0 — the default, and every model trained before it existed — there is nothing to
// explain and a permanent caption would be noise. Flow-only knob (denoise has no boundary loss).
const flowBoundary = computed(() => flowManifest.value?.foregroundBoundaryWeight || 0)
// Log only when every plotted value is positive — val included. Zero is a legitimate loss, and
// log(0) would drop the point silently rather than fail.
const isLog = computed(() =>
  logY.value && [...rows.value, ...valRows.value].every(r => r.loss > 0))

// A finished training run adds a model. Training is set-scope and this panel is not bound to one
// image, so it watches every image in the project — same shared primitive, same opt-out. Calling
// `refresh` re-fetches BOTH vaults so a new model of either kind lands here.
const allUids = computed(() => project.sets.flatMap(s => s.images.map(i => i.uid)))
useDataRefresh(() => allUids.value, refresh)

async function render() {
  if (!host.value) return
  if (!Plot) Plot = await import('@observablehq/plot')
  node?.remove(); node = null
  if (!rows.value.length) return
  // Only the training rows get a tip. Two `tip: true` marks stacked on one x means both pointers
  // fire and the boxes overlap; the training curve is the one you hover to read a value, and the
  // val line's meaning is its distance from it, not its number.
  const w = Math.max(200, host.value.clientWidth || 360)
  const h = Math.max(160, host.value.clientHeight || 240)
  const dark = !forceLight.value
  const { ink: fg, ground: bg } = plotTheme(dark)
  const domain = shown.value.map(s => s.term)

  // Detail view — log(gradient step) sub-epoch trace + initial/plateau reference furniture per
  // series. The reason this exists: SUPPORT (and any shot-noise-limited self-supervised trainer)
  // plateaus inside the first ~100 steps and the per-epoch mean buries that, so a converged run
  // reads as a flat line. Same colour per term as the classic view; the marks change, not the
  // colour key. Old models (no step trace) never see this branch — the toggle stays hidden.
  if (detailActive.value) {
    const colors = distinctColors(domain.length)
    const colorOf = (term: string) => colors[domain.indexOf(term)] || colors[0]
    const marks: unknown[] = []
    // Faint per-batch underlay (honest variance) + bold moving-avg line per term.
    marks.push(Plot.line(stepRows.value, { x: 'step', y: 'loss', stroke: 'term',
                                           strokeWidth: 0.5, opacity: 0.25 }))
    marks.push(Plot.line(maRows.value,   { x: 'step', y: 'loss', stroke: 'term',
                                           strokeWidth: 1.8, tip: true }))
    // Reference line (initial) + plateau band + convergence x-marker per term. Same colour as the
    // term. Text lives in an HTML caption row above the plot (`detailCaptions`) rather than as
    // in-plot text marks — earlier attempts put per-series labels on the plot and they collided
    // both vertically (similar curves stack at the same y) AND horizontally (converged label on
    // the left touched the early-stop label on the right on any narrow panel).
    for (const s of shown.value) {
      const c = convByTerm.value[s.term]
      if (!c) continue
      const col = colorOf(s.term)
      marks.push(Plot.ruleY([c.initial], { stroke: col, strokeDasharray: '2,3', opacity: 0.6 }))
      marks.push(Plot.ruleY([c.plateau], { stroke: col, strokeWidth: 2,   opacity: 0.3  }))
      if (c.convergedAt !== null) {
        marks.push(Plot.ruleX([c.convergedAt], { stroke: col, strokeDasharray: '4,3', opacity: 0.7 }))
      }
    }
    node = Plot.plot({
      width: w, height: h, marginLeft: 58, marginRight: 12, marginTop: 12,
      style: { background: bg, color: fg, fontSize: '11px' },
      x: { label: 'gradient step (log)', type: 'log', grid: true },
      y: { label: 'loss', grid: true, type: 'linear', zero: true },
      color: { domain, range: colors, legend: false },
      marks,
    }) as SVGElement
    applyPlotTheme(node, dark)
    host.value.append(node)
    return
  }

  node = Plot.plot({
    width: w, height: h, marginLeft: 58, marginRight: 12, marginTop: 12,
    style: { background: bg, color: fg, fontSize: '11px' },
    x: { label: 'epoch', grid: true },
    // Log only when every plotted value is positive. Zero is a legitimate loss and log(0) would drop
    // the point silently rather than fail.
    // Anchored at zero on the linear scale. Plot's default domain starts at the data minimum, which
    // for a converged run means the flat tail fills the panel and every curve looks like it stopped
    // just short of the axis — a loss settling at 0.2 and one settling at 0.02 draw identically.
    // "How close to zero did it actually get" is the question, so zero has to be on screen.
    // Never on a log scale: log(0) is undefined and Plot would drop the axis.
    y: { label: (raw.value ? 'loss (raw)' : 'loss (weighted)')
              + (minusFloor.value && hasFloors.value ? ' \u2212 floor' : ''), grid: true,
         // Every plotted value, val included — a log axis chosen on the training rows alone would
         // silently drop a val point that touched zero.
         ...(isLog.value ? { type: 'log' as const } : { type: 'linear' as const, zero: true }) },
    color: { domain, range: distinctColors(domain.length), legend: false },
    marks: [
      Plot.line(rows.value, { x: 'epoch', y: 'loss', stroke: 'term', strokeWidth: 1.5, tip: true }),
      Plot.line(valRows.value, { x: 'epoch', y: 'loss', stroke: 'term', strokeWidth: 1.5,
                                 strokeDasharray: '3,3' }),
    ],
  }) as SVGElement
  // Plot fills a tip rect from `--plot-background`, which its own stylesheet sets to white — see
  // `applyPlotTheme`. Without this the hover is theme-ink text on a white box.
  applyPlotTheme(node, dark)
  host.value.append(node)
}

onMounted(() => {
  nextTick().then(render)
})
// Re-render whenever the resolved manifest changes (either kind, either vault fetch completes).
watch(manifest, () => nextTick().then(render))
// the observer's callback appends into the element it observes — usePlotResize explains why
// that loops ("ResizeObserver loop completed with undelivered notifications") and what stops it
const plotBox = usePlotResize(host, render)
onBeforeUnmount(() => { node?.remove(); node = null })
watch([chosen, logY, raw, minusFloor, detail, () => terms.value.join(','), hasVal,
       anyStoppedEarly],
      () => plotBox.redraw())

// ── export (the generic panel contract — plots/export.ts, same helpers as the cluster panels) ──
const exportFormats = ['png', 'svg', 'csv']
const stem = computed(() => `training_${chosen.value || 'model'}`.replace(/[^\w.-]+/g, '_'))
// One row per epoch, one COLUMN per term — a long/tidy dump would make the obvious spreadsheet
// question ("plot these against each other") a pivot first.
const csv = () => rows.value.length ? rowsToCsv(lossTable(shown.value)) : null
function exportAs(kind: string) {
  if (kind === 'csv') {
    const text = csv()
    if (text) downloadBlob(`${stem.value}.csv`, new Blob([text], { type: 'text/csv' }))
  } else if (kind === 'png' || kind === 'svg') {
    elementToImageURL(host.value, kind, '#1f2226')
      .then(url => url && downloadDataUrl(`${stem.value}.${kind}`, url))
  }
}
// board PDF/CSV/SVG: a plot-only LIGHT re-render, per the contract in docs/UI.md
async function exportImage(): Promise<string | null> {
  forceLight.value = true
  await nextTick(); await render()
  const url = await elementToImageURL(host.value, 'png', '#ffffff')
  forceLight.value = false; await render()
  return url
}
async function exportSvg(): Promise<string | null> {
  forceLight.value = true
  await nextTick(); await render()
  const svg = svgOf(host.value)?.outerHTML ?? null
  forceLight.value = false; await render()
  return svg
}
defineExpose({ exportFormats, exportAs, exportImage, exportSvg, getCsv: csv })
</script>

<template>
  <div class="ftv">
    <div class="ftv-ctrl cc-panel-controls">
      <div class="cc-row ftv-bar">
        <!-- `raw` and `− floor` are flow-only: SUPPORT has a single loss with no weighting or
             recorded floor. Hiding them is honest — a chip that does nothing is worse than one that
             isn't there (docs/ui/COPY.md → hover help is not optional). -->
        <label v-if="kind !== 'denoise'" class="cc-muted cc-fs-xs ftv-opt"
               v-tooltip.top="'Show each term before its weight is applied'">
          <input type="checkbox" v-model="raw" /> raw
        </label>
        <label class="cc-muted cc-fs-xs ftv-opt" v-tooltip.top="'Log scale on the loss axis'">
          <input type="checkbox" v-model="logY" /> log
        </label>
        <!-- Detail toggle only appears when the model actually carries a sub-epoch step trace
             (old denoise models don't). Shipping a control that silently does nothing is worse
             than not having it. -->
        <label v-if="hasStepTrace" class="cc-muted cc-fs-xs ftv-opt"
               v-tooltip.top="'log(step) view: sub-epoch trace with initial/plateau markers'">
          <input type="checkbox" v-model="detail" /> detail
        </label>
        <label v-if="kind !== 'denoise'" class="cc-muted cc-fs-xs ftv-opt"
               :class="{ 'ftv-off': !hasFloors }"
               v-tooltip.top="hasFloors
                 ? 'Subtract each target\'s entropy — the loss no model can beat'
                 : 'No floors recorded — re-train to get them'">
          <input type="checkbox" v-model="minusFloor" :disabled="!hasFloors" /> &minus; floor
        </label>
        <!-- A dashed line with nothing naming it is a puzzle. Only shown when there is one. -->
        <span v-if="hasVal" class="cc-muted cc-fs-2xs">dashed = held out</span>
        <!-- Surfaced in both Simple and Detail so a reader who never toggles Detail still knows
             the shorter run wasn't a failure — patience caught the plateau. -->
        <span v-if="anyStoppedEarly" class="cc-muted cc-fs-2xs"
              v-tooltip.top="'Loss stopped improving — patience cut the run short (Advanced → Early stop)'">early-stopped</span>
        <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.left="'Reload'"
                :disabled="loading" @click="refresh">
          <i class="pi pi-refresh" :class="{ 'pi-spin': loading }" />
        </button>
      </div>
      <label v-if="termOptions.length > 1" class="cc-row ftv-terms">
        <span class="cc-muted cc-fs-xs"
              v-tooltip.top="'Which loss terms to draw — a term at weight 0 is off'">terms</span>
        <ChipSelect :options="termOptions" :model-value="terms" multiple aria-label="Loss terms"
                    @update:model-value="v => state.terms = v as string[]" />
      </label>
      <!-- Short line, action in the tooltip (docs/ui/COPY.md): the fact is that a knob is on with no
           chip for it, and the useful move is to read `foreground` against its floor instead. -->
      <p v-if="flowBoundary" class="cc-muted cc-fs-2xs ftv-note"
         v-tooltip.bottom="'Not a loss term — it shapes the foreground target, so it moves ' +
                           'foreground and that term\'s floor'">
        Flow boundary {{ flowBoundary }} — no curve of its own
      </p>
    </div>

    <p v-if="error" class="cc-muted-warn">{{ error }}</p>
    <p v-else-if="!chosen && !loading" class="cc-muted">Select a model in the vault.</p>
    <p v-else-if="chosen && kind === null && !loading" class="cc-muted">
      Model "{{ chosen }}" is not in either vault — did it get renamed or deleted?
    </p>
    <p v-else-if="chosen && !series.length && !loading" class="cc-muted">
      No loss curves — {{ chosen }} was trained before they were recorded. Re-train to get them.
    </p>

    <!-- Per-series convergence/early-stop readout for the Detail view. HTML rather than in-plot
         text: three similarly-shaped curves would collide both vertically and horizontally as
         SVG marks. Empty in Simple view. -->
    <ul v-if="detailCaptions.length" class="ftv-captions cc-fs-xs">
      <li v-for="cap in detailCaptions" :key="cap.term">
        <span class="ftv-cap-swatch" :style="{ background: cap.color }" />
        <span class="ftv-cap-term">{{ cap.term }}</span>
        <span class="cc-muted">{{ cap.text }}</span>
      </li>
    </ul>

    <div ref="host" class="ftv-host" />
  </div>
</template>

<style scoped>
/* position: relative so the overlaid .ftv-ctrl (.cc-panel-controls) anchors to the plot box */
.ftv { position: relative; display: flex; flex-direction: column; height: 100%; min-height: 0; }
.ftv-ctrl { display: flex; flex-direction: column; gap: 0.4rem; padding: 4px 6px; }
.ftv-bar { flex-wrap: wrap; }
.ftv-terms { flex-wrap: wrap; gap: 0.4rem; }
.ftv-note  { margin: 0.15rem 0 0; }
.ftv-opt { display: flex; align-items: center; gap: 0.25rem; }
.ftv-off { opacity: 0.45; }
.ftv-host { flex: 1; min-height: 0; }
.ftv-captions { list-style: none; margin: 0.1rem 6px 0.2rem; padding: 0;
                display: flex; flex-direction: column; gap: 0.1rem; }
.ftv-captions li { display: flex; align-items: center; gap: 0.35rem; line-height: 1.15; }
.ftv-cap-swatch { display: inline-block; width: 8px; height: 8px; }
.ftv-cap-term  { font-weight: 600; }
</style>
