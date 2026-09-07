import { defineStore, acceptHMRUpdate } from 'pinia'
import { reactive } from 'vue'
import type { ManualAction, TrackOp } from '../lib/trackCorrection'

// The cockpit's shared per-(image, valueName) STATE — the inputs the cockpit's tools operate on.
// Everything that lives here also lives implicitly in TrackSchemeView's local state today (its
// `setSelected`, `splitAt`, `detSel`). Publishing here as well lets the app-global cockpit read
// the same selection the timeline sees, without threading a canvas prop through App.vue's floating
// panels. Phase 1c will lift TrackSchemeView's reads to this store too (making it the single source
// of truth); Phase 1b just mirrors from TSV → here so the cockpit can act on whatever the user last
// selected on the timeline.
//
// Keyed by `trackOpsKey(project, image, valueName)` — same rule as `useTrackOpsQueueStore`. A
// draft-shaped thing lives at the granularity of the thing it edits, not whichever surface
// authored it.

/** The Add action carries the same shape as a ManualAction but has no `key` — treat as its sibling. */
export interface AddAction {
  label: string
  blocked: string | null
  op: TrackOp | null
}

export interface CockpitScopeState {
  /** Track IDs currently selected — stringified so the same rendering path works in TSV + cockpit. */
  selectedTracks: string[]
  /** The frame armed for `Split` on the sole selected track. `null` when nothing is armed. */
  splitFrame: number | null
  /** An untracked frame + its labels, picked from the untracked-detection strip. `null` when nothing. */
  detSelection: { frame: number; labels: number[] } | null
  /**
   * Join / Split / Remove ops (with `blocked` reasons) as the timeline computed them from full paths
   * data. Published by TSV; consumed by the cockpit so it doesn't refetch paths just to render three
   * buttons. Empty until the timeline has published — a fresh cockpit-only surface reads no actions,
   * which is why Phase 1b requires the timeline to be open for these buttons to enable.
   */
  actions: ManualAction[]
  /** The Add (points.add) op the timeline's P3 untracked-lane strip authors. Same publish-only rule. */
  addAction: AddAction
}

const EMPTY_ADD: AddAction = { label: 'Add', blocked: 'Open the timeline and pick an untracked frame', op: null }
const EMPTY: CockpitScopeState = {
  selectedTracks: [], splitFrame: null, detSelection: null,
  actions: [], addAction: EMPTY_ADD,
}

export const useCorrectionCockpitStore = defineStore('correctionCockpit', () => {
  const scopes = reactive<Record<string, CockpitScopeState>>({})

  function state(key: string): CockpitScopeState {
    return key ? (scopes[key] ?? EMPTY) : EMPTY
  }

  function ensure(key: string): CockpitScopeState {
    if (!key) return EMPTY
    if (!scopes[key]) scopes[key] = {
      selectedTracks: [], splitFrame: null, detSelection: null,
      actions: [], addAction: EMPTY_ADD,
    }
    return scopes[key]
  }

  function setSelectedTracks(key: string, ids: string[]): void {
    if (!key) return
    ensure(key).selectedTracks = [...ids]
  }
  function setSplitFrame(key: string, frame: number | null): void {
    if (!key) return
    ensure(key).splitFrame = frame
  }
  function setDetSelection(key: string, det: { frame: number; labels: number[] } | null): void {
    if (!key) return
    ensure(key).detSelection = det ? { frame: det.frame, labels: [...det.labels] } : null
  }
  function setActions(key: string, actions: ManualAction[]): void {
    if (!key) return
    ensure(key).actions = actions.map(a => ({ ...a }))
  }
  function setAddAction(key: string, add: AddAction): void {
    if (!key) return
    ensure(key).addAction = { ...add }
  }
  function clearAll(key: string): void {
    if (!key) return
    scopes[key] = {
      selectedTracks: [], splitFrame: null, detSelection: null,
      actions: [], addAction: EMPTY_ADD,
    }
  }

  return { scopes, state, setSelectedTracks, setSplitFrame, setDetSelection,
           setActions, setAddAction, clearAll }
})

if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useCorrectionCockpitStore, import.meta.hot))
