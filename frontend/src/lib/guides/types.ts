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
  // How many profiles this box has (see USER_PROFILE_PLAN). A guide about switching profiles has
  // nothing to point at on a single-profile install — the header chip is still there but Preferences
  // has no Switch action to demonstrate — so it soft-gates on this rather than blocking the guide.
  profileCount: number
  setUid: string | null                      // the ACTIVE set, or null
  // How many sets the project has AT ALL. Distinct from `setUid`, and the distinction matters: with no
  // sets there is nothing to select, so a step saying "pick a set" has to point at "New set" instead.
  setCount: number
  images: CciaImage[]                        // images in the ACTIVE set (the ones a page shows)
  viewerImageUid: string | null              // image in the viewer window, while one is open
  selection: (module: string) => string[]    // checkbox selection for a module page
  rightPanelCollapsed: boolean
  viewerOpen: boolean                        // the viewer window is open (this window opened it)
  // Live DOM reads against a `data-guide` anchor — for controls whose state is not in any store
  // (`TaskRunner`'s function `<select>`, a checkbox). Re-read on the runtime's poll, so a predicate
  // using these updates without the control having to report to a store.
  anchorValue: (anchorId: string) => string | null
  anchorExists: (anchorId: string) => boolean
  // In the DOM *and* on screen. The distinction is what separates "this control doesn't exist yet"
  // (pick a function and its parameters appear) from "it exists but something is hiding it" (a
  // collapsed panel, a collapsed pane half) — two different fixes, so two different bubbles.
  anchorReachable: (anchorId: string) => boolean
  // May this window see machine-wide settings (appControl.canManageMachine)? Steps about them carry
  // `onlyIf: c => c.canManageMachine` so other profiles' guides skip them.
  canManageMachine: boolean
  // Are AI features on for this profile (observer.ai.enabled)? Steps that present Kiwi or Claude
  // carry `onlyIf: c => c.aiEnabled`, so a tour with AI off doesn't advertise what it can't use.
  aiEnabled: boolean
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
  // The control is in the VIEWER window: the main window points at the Viewer chip and the viewer
  // window shows the step (lib/guides/remote.ts). Its gates still read the main window's state.
  window?: 'viewer'
  // Include the step at all? Read ONCE when the guide starts (`guideStepsFor`), so a step whose
  // control this window can never show is dropped instead of pointing at nothing.
  onlyIf?: GatePredicate
}

/** The guide's steps for this window: those whose `onlyIf` holds now. */
export const guideStepsFor = (steps: GuideStep[], c: GuideCtx): GuideStep[] =>
  steps.filter(s => !s.onlyIf || s.onlyIf(c))

export interface Prereq {
  id: string
  label: string               // short, human: "an image that finished importing"
  ok: GatePredicate
  fixGuide?: string           // the guide that gets you this, offered on a miss
}

// ── Feature maturity (docs/todo/FEATURE_MATURITY_PLAN.md) ─────────────────────────────────────────
// How far to trust what a feature produces. A guide teaches one feature, so the guide carries that
// feature's record; a feature with no guide gets the same record in `unguided.ts`. Each tier names
// the evidence it takes (the plan's *The scale*). Only the maintainer changes a tier (Decision 6).
//
//   comingSoon   not usable yet — NEVER written in a record: derived from the task JSON `comingSoon`
//                flag or the nav item's `soon` (one source per fact; `maturity.test.ts` enforces it)
//   experimental runs, not checked on realistic data — check every result by hand
//   beta         someone checked results by hand on real data — spot-check results
//   tested       an automated test fails when the result is wrong — `evidence` required
//   stable       reserved, unused (provenance recorded, upgrades safe)
//   exempt       produces no scientific result (blackboard, tour, …) — no badge, no map row (Decision 9)
export type Tier = 'comingSoon' | 'experimental' | 'beta' | 'tested' | 'stable' | 'exempt'

/** One test that would catch a wrong result: the file, the testset/test name, and what it catches. */
export interface Evidence {
  path: string                // repo-relative test file, e.g. 'app/test/suite/gating.jl'
  test: string                // the `@testset` name / `def test_…` / `it(…)` name, verbatim
  failsIf: string             // "fails if …" — the wrong result it catches, so a promoter can judge it
}

/** A human attestation — can't be tested and goes stale, so the map shows when it was made. */
export interface CheckedBy {
  who: string
  when: string                // 'YYYY-MM' — the age signal until a feature → code-path map exists
  version?: string            // the app version checked, when known
  data: string                // which images or dataset
}

export interface NextItem { text: string; done: boolean }

export interface Maturity {
  tier: Exclude<Tier, 'comingSoon'>
  testedOn: string            // breadth, separate from the tier: "intravital 2-photon, one lab"
  checkedBy?: CheckedBy
  handCheck?: string          // where a hand-check on real data is written down (a docs/ section)
  evidence?: Evidence[]
  next?: NextItem[]           // the graduation checklist, 1–3 items, from the hardening audit's gaps
  needs?: string              // the data that would help, in a user's words
  datasetAsk?: string         // a `lib/callForDatasets.ts` id — required whenever `needs` is set…
  noAsk?: string              // …unless this says why the need can't be a dataset ask (e.g. a cluster account)
  // The tasks (fun_names) and sidebar pages this record ALSO tiers. A guide already covers the tasks
  // it runs (`awaitTask.fun`) and its page in `mapData.ts`; list only what it teaches beyond those —
  // a hidden helper task, the correction composites. In `unguided.ts` this is the feature's key.
  covers?: string[]
}

export interface GuideDef {
  id: string
  title: string
  // Mirrors the sidebar's grouping so the picker reads in the same order as the app
  // ('Data' | 'Populations' | 'Explore' | 'Analysis' | 'Pipeline').
  group: string
  summary: string             // one line in the picker
  icon: string                // an icon class, e.g. 'pi-th-large'
  prereqs: Prereq[]
  steps: GuideStep[]
  maturity: Maturity          // the taught feature's tier — required, no silent default (Decision 5)
}
