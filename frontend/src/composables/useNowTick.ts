import { onScopeDispose } from 'vue'
import { nowMs, subscribeNowTick } from '../utils/nowTick'

/**
 * A reactive `now` (ms) for live elapsed counters, ticking once a second.
 *
 * Use this in any component that renders "how long has this been running" — it shares ONE interval with
 * every other counter on screen (see utils/nowTick.ts) and releases it with the component's scope, so no
 * caller has to remember `clearInterval`.
 */
export function useNowTick() {
  onScopeDispose(subscribeNowTick())
  return nowMs
}
