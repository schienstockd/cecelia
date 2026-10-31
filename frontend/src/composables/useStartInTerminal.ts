// Start in terminal: copy the one-liner (`kiwi_terminal_command`, built server-side) that starts the
// assistant CLI under THIS profile's login with a chat starter as its first message, so one paste
// gives a session already looking at the project — it pairs on its first observer call. A missing or
// stale MCP entry is registered first: without it the session has no observer tools to pair through.
//
// One implementation for both ways in: Kiwi's Chat row (the project only) and a Kiwi turn's
// "Continue in terminal" (the project plus the Blackboard entry the turn was saved to). `key` lets a
// per-turn button flash on its own row.
import { ref } from 'vue'
import { fetchTerminalCommand } from '../utils/profileApi'
import { terminalCta } from '../utils/observerSetup'
import { useObserverStore } from '../stores/observer'
import { useCopyFlash } from './useCopyFlash'

export function useStartInTerminal() {
  const observer = useObserverStore()
  const { isCopied, copy } = useCopyFlash(2500)
  const starting = ref('')   // the key of the start in flight; '' = none

  /** Copy the start command for `prompt`. Returns whether it reached the clipboard. */
  async function start(prompt: string, key = 'chat'): Promise<boolean> {
    if (starting.value) return false
    starting.value = key
    try {
      if (terminalCta(observer.available, observer.terminalState) !== 'chat') await observer.registerMcp()
      const r = await fetchTerminalCommand({ prompt })
      return r?.command ? await copy(r.command, key) : false
    } finally { starting.value = '' }
  }

  return { start, starting, isCopied }
}
