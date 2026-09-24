// Select an Analysis board tab by name once `/analysis` has mounted — ONE copy, shared by the
// `viewer:navigate` WS frame (the MCP `open_analysis_board_plot` tool) and a click on a Kiwi
// proposed plot. The destination registers its tabs on mount (`analysisTabs.ensure()`), and a cold
// `/analysis` is a lazy chunk, so `nextTick` isn't enough: poll ~1 s, then give up quietly (the page
// then shows whatever board was last active — honest, if the name is gone).
import { useAnalysisTabsStore } from '../stores/analysisTabs'

export function pickBoardTab(projectUid: string, boardName: string): void {
  if (!projectUid || !boardName) return
  const tabs = useAnalysisTabsStore()
  const groupKey = `analysis:${projectUid}`
  let tries = 0
  const tryPick = () => {
    const tab = tabs.entries[groupKey]?.tabs.find(t => t.name === boardName)
    if (tab) { tabs.setActive(groupKey, tab.id); return }
    if (++tries < 20) setTimeout(tryPick, 50)
  }
  tryPick()
}
