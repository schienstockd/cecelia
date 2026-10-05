<!--
  Headless board render — a bare page a HARNESS drives, not one a user opens. It shows one Analysis
  board of one project and hands back each slot's plot-only, light-theme PNG through the SAME export
  path the board's PDF uses (`LayoutCanvas.capturePage`, every panel's `exportImage()`), so a run
  record's pictures are the app's own plots, not a re-implementation.

  `?project=<uid>&images=<uid,…>` (images default to every image). The project is opened VIEW-ONLY
  (`projectMeta.peekProject` — no lastOpenedAt, no recent-list bump) and the board autosave is off, so
  the session writes nothing. The driver (`scripts/agent_eval/stage_boards.py`) waits for
  `window.__ccBoardRender.ready`, then awaits `window.__ccBoardRender.render(boardName)` per board.
-->
<script setup lang="ts">
import { computed, nextTick, onMounted, ref, useTemplateRef } from 'vue'
import { useRoute } from 'vue-router'
import LayoutCanvas from '../components/canvas/LayoutCanvas.vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useProjectStore } from '../stores/project'
import { useAnalysisTabsStore } from '../stores/analysisTabs'
import { useAnalysisLayoutStore } from '../stores/analysisLayout'
import { waitForPlotsIdle } from '../utils/plotReady'
import { parseBoardRenderQuery, findBoardTab, captureSignature, type RenderedSlot } from '../utils/boardRender'

const MAX_ROUNDS = 6
const SLOT_PX = 720

const route = useRoute()
const meta = useProjectMetaStore()
const project = useProjectStore()
const tabs = useAnalysisTabsStore()
const layout = useAnalysisLayoutStore()
layout.setReadOnly(true)

const query = parseBoardRenderQuery(route.query)
const groupKey = `analysis:${query.projectUid}`
const tabId = ref<number | null>(null)
const canvasKey = computed(() => tabId.value === null ? '' : `${groupKey}:tab:${tabId.value}`)
const imageUids = computed(() => query.imageUids.length ? query.imageUids
  : project.sets.flatMap(s => s.images.map(i => i.uid)))
type Capture = { slots: { png: string | null; name: string; title?: string }[] }
const canvasRef = useTemplateRef<{ capturePage: (vector?: boolean) => Promise<Capture> }>('canvasRef')

interface RenderApi {
  ready: boolean
  error?: string
  render: (board: string) => Promise<{ ok: boolean; error?: string; slots: RenderedSlot[] }>
}
const api: RenderApi = {
  ready: false,
  async render(board) {
    const id = findBoardTab(tabs.entries[groupKey]?.tabs ?? [], board)
    if (id === null) return { ok: false, error: `no board named "${board}"`, slots: [] }
    // Each slot at a figure size, not the A4 sheet's: the stored board is A4-locked (width = height ×
    // page aspect), which on a 2×2 board leaves a plot ~200px wide. In memory only — autosave is off.
    const e = layout.entries[`${groupKey}:tab:${id}`]
    if (e) { e.sheet = 'free'; e.rowHeight = SLOT_PX }
    tabId.value = id
    await nextTick()
    // A plot's loads can come in sequence with a quiet gap between (the UMAP waits on its run's
    // features, then fetches the embedding), so one idle window can end between them. Capture until two
    // captures in a row agree; the cap covers server-rendered card filmstrips.
    let page: Capture | undefined
    let prev = ''
    for (let round = 0; round < MAX_ROUNDS; round++) {
      await waitForPlotsIdle({ settleMs: 1500, timeoutMs: 180000 })
      page = await canvasRef.value?.capturePage(false)
      if (!page) return { ok: false, error: 'the board did not mount', slots: [] }
      const sig = captureSignature(page.slots)
      if (sig === prev) break
      prev = sig
    }
    if (!page) return { ok: false, error: 'the board did not mount', slots: [] }
    return { ok: true, slots: page.slots.map((s, index) => ({ index, name: s.name, title: s.title, png: s.png })) }
  },
}
;(window as unknown as { __ccBoardRender: RenderApi }).__ccBoardRender = api

onMounted(async () => {
  if (!query.projectUid) { api.error = 'no ?project='; api.ready = true; return }
  const ok = await meta.peekProject(query.projectUid)
  if (!ok) api.error = `could not open project ${query.projectUid}`
  api.ready = true
})
</script>

<template>
  <div class="board-render cc-dark">
    <LayoutCanvas v-if="canvasKey" ref="canvasRef" :key="canvasKey" :canvas-key="canvasKey"
                  :module="null" :image-uids="imageUids" />
  </div>
</template>

<style scoped>
.board-render { height: 100vh; width: 100vw; overflow: hidden; background: var(--cc-bg); }
</style>
