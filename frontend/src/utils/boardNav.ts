// Select an Analysis board tab by name once `/analysis` has mounted — ONE copy, shared by the
// `viewer:navigate` WS frame (the MCP `open_analysis_board_plot` tool) and a click on a Kiwi
// proposed plot. The tab may not be in the store yet: a cold `/analysis` is a lazy chunk that
// registers its tabs on mount, and a board Kiwi just created arrives by the `boards:changed` reload
// (a fetch). So WATCH the tab list until the name appears rather than polling a fixed second — a
// slow reload used to lose the race and leave the last-active board showing as if it were the one
// asked for. Resolves `false` on timeout so the caller can say so.
import { watch } from 'vue'
import { useAnalysisTabsStore } from '../stores/analysisTabs'

export const BOARD_TAB_WAIT_MS = 10_000

export function pickBoardTab(projectUid: string, boardName: string, timeoutMs = BOARD_TAB_WAIT_MS): Promise<boolean> {
  if (!projectUid || !boardName) return Promise.resolve(false)
  const tabs = useAnalysisTabsStore()
  const groupKey = `analysis:${projectUid}`
  const find = () => tabs.entries[groupKey]?.tabs.find(t => t.name === boardName)
  return new Promise(resolve => {
    let settled = false
    let stop: (() => void) | undefined
    const done = (ok: boolean) => { settled = true; clearTimeout(timer); stop?.(); resolve(ok) }
    const timer = setTimeout(() => done(false), timeoutMs)
    stop = watch(find, tab => {
      if (!tab) return
      tabs.setActive(groupKey, tab.id)
      done(true)
    }, { immediate: true, flush: 'sync' })
    if (settled) stop()      // found on the immediate run, before `stop` existed
  })
}
