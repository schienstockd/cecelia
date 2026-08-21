import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref } from 'vue'
import type { ParamValues } from '../tasks/types'

/**
 * "Load these params into the form" — one hand-off, from something that knows a param set to the
 * `TaskRunner` showing that function.
 *
 * The first case is the flow-model vault: a model's manifest IS the form that produced it, and the
 * scenario is "that one looks good, but I want to tweak it". The vault lives on the canvas and the
 * form lives in the module column, so they are siblings with no props between them.
 *
 * **Not the drafts store, and not a second copy of it.** A draft is what the user typed and is read
 * once, on (re)init. This is an EVENT: it arrives while the form is already mounted and showing the
 * target function, so it has to be watchable. Writing into `taskDrafts` instead would need TaskRunner
 * to watch its own draft — which it writes on every keystroke, so the watcher would re-apply the value
 * it just saved.
 *
 * **One-shot by construction.** `take` consumes, so an offer cannot be applied twice (remount,
 * selection change) and cannot fight the user's subsequent edits. `funName` is checked by the consumer
 * so an offer made while a different function is selected is not silently applied to it.
 *
 * Deliberately NOT scoped to an image or set, unlike a draft: the offer means "these settings", and
 * which images they run on is the selection the user already made.
 */
export interface ParamOffer {
  funName: string
  values: ParamValues
  /** what the params came from, for the confirmation the form shows — e.g. `model flow.cyto` */
  source: string
  /** fields the source could not supply, left at whatever the form had */
  missing?: string[]
}

export const useParamHandoffStore = defineStore('paramHandoff', () => {
  const pending = ref<ParamOffer | null>(null)

  /** Offer a param set. Replaces any un-consumed offer — the newest click is the one that meant it. */
  const offer = (o: ParamOffer) => { pending.value = o }

  /** Consume the offer if it targets `funName`, else leave it for the form that can use it. */
  function take(funName: string): ParamOffer | null {
    const o = pending.value
    if (!o || o.funName !== funName) return null
    pending.value = null
    return o
  }

  const clear = () => { pending.value = null }

  return { pending, offer, take, clear }
})

// Replace the live instance on hot-reload — see the note in `stores/customModules.ts`.
if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useParamHandoffStore, import.meta.hot))
