import { ref, nextTick } from 'vue'

// Board-wide "are any plots still loading?" counter. Every plot host registers its loading state here
// through `useDelayedLoading` (the one composable they all use for the spinner), so this covers every
// plot type — summary, cluster heatmap/HMM, UMAP, gating — with no per-plot wiring. The PDF/CSV export
// waits on `waitForPlotsIdle` so it captures FULLY-RENDERED plots instead of sleeping a fixed guess
// (a mid-fetch/mid-WebGL-frame plot exports blank/sparse). See TabbedCanvas.exportPdf.
export const activePlotLoads = ref(0)

export function beginPlotLoad(): void { activePlotLoads.value++ }
export function endPlotLoad(): void { activePlotLoads.value = Math.max(0, activePlotLoads.value - 1) }

/**
 * Resolve once no plot has been loading for a continuous `settleMs` window (so a fetch that kicks off a
 * beat after a tab switch still counts), capped at `timeoutMs` as a hard fallback. Ends with two RAF
 * frames so the final paint (WebGL/canvas draws after its data arrives) lands before the caller captures.
 * The `settleMs` floor also means an all-cached board waits ~settleMs, not zero — enough for late fetches
 * to register. Poll-based (not event-based) so it's robust to loads that start/stop between checks.
 */
export async function waitForPlotsIdle({ settleMs = 350, timeoutMs = 10000, pollMs = 50 } = {}): Promise<void> {
  await nextTick()
  const deadline = Date.now() + timeoutMs
  let idleStart: number | null = null
  while (Date.now() < deadline) {
    if (activePlotLoads.value > 0) {
      idleStart = null
    } else {
      if (idleStart === null) idleStart = Date.now()
      else if (Date.now() - idleStart >= settleMs) break
    }
    await new Promise(r => setTimeout(r, pollMs))
  }
  // two frames for the final paint (a WebGL scatter draws the frame AFTER its data is set)
  await new Promise(r => requestAnimationFrame(() => r(null)))
  await new Promise(r => requestAnimationFrame(() => r(null)))
}
