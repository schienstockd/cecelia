/**
 * The population FAMILY a `rail: 'pops'` plot is showing — resolved once, for the request and the
 * control at the same time.
 *
 * All three track views had the same pair of computeds inline (`popTypeOptions` over the registry's
 * declared families, then `resolvePopType` against the panel's own state). That is the resolution the
 * rail also performs, so a second inline copy per view is three chances for a panel to REQUEST one
 * family while its picker SAYS another — and on the timeline the copy was read-only, so it was pinned
 * to the registry's first family with no way to move it. That is what stopped the canvas's population
 * selection from ever reaching it: the canvas offers `track` populations, the timeline asked under
 * `live`, and `filterSeriesToPopType` correctly dropped every one of them.
 *
 * Writable, because the family is the panel's own choice (docs/PLOTS.md: one family per plot) and it
 * persists in the panel's state bag like every other per-panel setting.
 */
import { computed, type ComputedRef, type WritableComputedRef } from 'vue'
import { popTypeOptions, resolvePopType, type PopTypeOption } from '../plots/popTypes'

export function usePopFamily(
  /** the families the host declared for this view (`popTypesFor(key)`) */
  declared: () => PopTypeOption[] | undefined,
  /** the panel's persisted choice, and how to write it back */
  chosen: () => string | undefined,
  setChosen: (v: string) => void,
): { options: ComputedRef<PopTypeOption[]>; popType: WritableComputedRef<string> } {
  const options = computed<PopTypeOption[]>(() => {
    const d = declared()
    return d?.length ? popTypeOptions({ dataSource: { popTypes: d } }) : []
  })
  const popType = computed<string>({
    // `'track'` rather than `'live'` as the no-families fallback: every caller is a TRACK view, and a
    // host that passes no families is a module page that has not been given the rail yet — asking for
    // cell populations there would be a silently empty plot instead of the whole segmentation.
    get: () => (options.value.length
      ? resolvePopType({ dataSource: { popTypes: options.value } }, chosen())
      : 'track'),
    set: v => setChosen(v),
  })
  return { options, popType }
}
