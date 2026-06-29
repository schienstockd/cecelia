// Series-target keys for the summary canvas. A series target is a population on a specific
// segmentation, fetched under a pop_type ({popType, valueName, pop}). We key it as
// `popType::valueName + pop`. pop_types/value_names contain no "::" and pop paths always start with
// "/", so we split on the first "::" (popType) then the first "/" (valueName | pop).
import type { SeriesTarget } from './types'

export const tkey = (popType: string, valueName: string, pop: string) => `${popType}::${valueName}${pop}`

export function parseTkey(key: string): SeriesTarget {
  const c = key.indexOf('::')
  const popType = c < 0 ? 'live' : key.slice(0, c)
  const rest = c < 0 ? key : key.slice(c + 2)
  const i = rest.indexOf('/')
  return i < 0 ? { popType, valueName: rest, pop: '' } : { popType, valueName: rest.slice(0, i), pop: rest.slice(i) }
}
