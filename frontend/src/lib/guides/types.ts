// Types for the in-app guide system — bubble walkthroughs of the basics (docs/todo/GUIDE_SYSTEM_PLAN.md).
//
// The load-bearing rule: a guide POINTS AND OBSERVES. It never clicks, selects, navigates or runs
// anything (plan D1). So there is no action/dispatch field anywhere in here — a step can describe a
// control and detect that the user used it, and that is all. That is what makes it structurally
// impossible for a guide to start a 12-minute segmentation on the wrong image.
//
// Everything a step observes comes through `GuideCtx`, a flat snapshot of already-existing store
// state. Keeping guide definitions to pure predicates over that snapshot means the catalogue is
// testable without mounting the app or standing up Pinia.

import type { Placement } from '../../utils/anchorPosition'
import type { CciaImage } from '../../stores/project'

// A read-only snapshot of the app, rebuilt whenever the guide runtime re-evaluates. Add a field here
// rather than reaching into a store from a step predicate — a step that imports a store is a step
// that can mutate one.
export interface GuideCtx {
  route: string                              // current path, e.g. '/segment'
  hasProject: boolean
  setUid: string | null                      // the ACTIVE set, or null
  // How many sets the project has AT ALL. Distinct from `setUid`, and the distinction matters: with no
  // sets there is nothing to select, so a step saying "pick a set" has to point at "New set" instead.
  setCount: number
  images: CciaImage[]                        // images in the ACTIVE set (the ones a page shows)
  napariImageUid: string | null              // image currently open in the napari window, if any
  selection: (module: string) => string[]    // checkbox selection for a module page
  rightPanelCollapsed: boolean
  viewerPanelOpen: boolean
  // Live DOM reads against a `data-guide` anchor — for controls whose state is not in any store
  // (`TaskRunner`'s function `<select>`, a checkbox). Re-read on the runtime's poll, so a predicate
  // using these updates without the control having to report to a store.
  anchorValue: (anchorId: string) => string | null
  anchorExists: (anchorId: string) => boolean
  // In the DOM *and* on screen. The distinction is what separates "this control doesn't exist yet"
  // (pick a function and its parameters appear) from "it exists but something is hiding it" (a
  // collapsed panel, a collapsed pane half) — two different fixes, so two different bubbles.
  anchorReachable: (anchorId: string) => boolean
}

export type GatePredicate = (c: GuideCtx) => boolean

// What a step is waiting for before it lets go. Park on a long-running task (plan D3): the bubble
// moves to the task rail and becomes a spinner until a matching task finishes.
export interface AwaitTask {
  fun?: string          // match on funName, e.g. 'segment.cellpose'
  module?: string       // …or on the module page, e.g. 'segment' (use when several funs qualify)
  label: string         // what to call it while waiting: "Segmenting"
}

// A step whose target is not usable yet gets this bubble inserted ahead of it, pointing at whatever
// gets you there. See plan D5; without it a guide cheerfully points at a `display: none` button.
//
// A step may declare SEVERAL causes and the runtime shows the first whose `needed` is true — because
// one control can be unusable for unrelated reasons that need different advice. `TaskRunner`'s Run
// button is hidden both when the whole right panel is folded (fix: the panel handle) and when the
// runner's own pane half is collapsed (fix: the pane toggles), and pointing at the panel handle in the
// second case actively makes it worse.
export interface Reveal {
  needed: GatePredicate       // true ⇒ the target is currently unreachable, show this first
  text: string
  anchor?: string             // the control that reveals it (defaults to the step's own anchor)
  placement?: Placement
}

export interface GuideStep {
  // `data-guide="…"` id, or `nav:/segment` for a sidebar nav item (resolved via its href, so nav
  // needs no attributes). Omit for a step that is pure prose — it renders as a centred card.
  anchor?: string
  title?: string
  text: string                // ONE sentence (see docs/UI.md → Guide copy)
  bullets?: string[]          // at most 2-4 imperative lines
  placement?: Placement       // default 'right' — the bubble sits beside, not over, the control
  route?: string              // the page this step belongs to; off it, the runtime nudges back
  // How the step completes. All optional; with none of them it is Next-only. `Next` is ALWAYS
  // available regardless — a gate makes the bubble confirm the action, it never traps anyone (D2).
  when?: GatePredicate
  clickAnchor?: boolean       // advance when the anchor element is clicked
  awaitTask?: AwaitTask
  reveal?: Reveal | Reveal[]      // several causes ⇒ first match wins
}

export interface Prereq {
  id: string
  label: string               // short, human: "an image that finished importing"
  ok: GatePredicate
  fixGuide?: string           // the guide that gets you this, offered on a miss
}

export interface GuideDef {
  id: string
  title: string
  // Mirrors the sidebar's grouping so the picker reads in the same order as the app
  // ('Data' | 'Populations' | 'Explore' | 'Analysis' | 'Pipeline').
  group: string
  summary: string             // one line in the picker
  icon: string                // a PrimeIcons class, e.g. 'pi-th-large'
  prereqs: Prereq[]
  steps: GuideStep[]
}
