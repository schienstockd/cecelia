// A guide that steps into the viewer window (docs/todo/VIEWER_ON_THE_IMAGE_PLAN.md, S4). The guide runs
// in the MAIN window (`stores/guide.ts`): its gates read the main window's stores. A step marked
// `window: 'viewer'` is about a control in the viewer window, so:
//
//   - the main window publishes the running guide as plain data (`GuideRemote`, localStorage → a
//     `storage` event in the viewer window) and points at the Viewer chip, which brings the viewer up;
//   - the viewer window mirrors it (`stores/guideMirror.ts`) through the same `GuideBubble`, resolving
//     the anchor in its own document, and points at its ← back when the step is in the main window;
//   - Next / Back / leave and a click on a `clickAnchor` come back as commands (`GuideCmd`).
//
// Pure: the store and the mirror own the storage reads and writes.
import type { GuideStep } from './types'
import type { Placement } from '../../utils/anchorPosition'

export const GUIDE_REMOTE_KEY = 'cc.guide.remote'
export const GUIDE_CMD_KEY = 'cc.guide.cmd'

export type GuideRemotePhase = 'step' | 'waiting' | 'failed' | 'done'

/** A step as plain data: no predicates cross the window. */
export interface RemoteStep {
  anchor?: string
  title?: string
  text: string
  bullets?: string[]
  placement?: Placement
  clickAnchor?: boolean
  window?: 'viewer'
}

export interface GuideRemote {
  title: string
  index: number
  total: number
  phase: GuideRemotePhase
  gateSatisfied: boolean
  /** the step itself, not the main window's stand-in for it */
  step: RemoteStep | null
}

export type GuideCmdName = 'next' | 'back' | 'exit' | 'click'
export interface GuideCmd { cmd: GuideCmdName; at: number }

export function remoteStep(s: GuideStep): RemoteStep {
  const { anchor, title, text, bullets, placement, clickAnchor, window } = s
  return { anchor, title, text, bullets, placement, clickAnchor, window }
}

/** What the main window shows for a viewer step: the Viewer chip, which brings the viewer up. */
export function viewerStandIn(s: GuideStep): GuideStep {
  return { anchor: 'launcher.viewer', placement: 'bottom', title: s.title,
           text: 'This step is in the viewer — bring it up here.', route: s.route }
}

/** What the viewer window shows for a main-window step: its ← back. */
export function mainStandIn(s: RemoteStep | null): RemoteStep {
  return { anchor: 'viewer.back', placement: 'bottom', title: s?.title,
           text: 'This step is in the main window — go back here.' }
}

export function parseGuideRemote(raw: string | null): GuideRemote | null {
  if (!raw) return null
  try {
    const r = JSON.parse(raw) as GuideRemote
    return r && typeof r.index === 'number' && typeof r.total === 'number' ? r : null
  } catch { return null }
}

export function parseGuideCmd(raw: string | null): GuideCmd | null {
  if (!raw) return null
  try {
    const c = JSON.parse(raw) as GuideCmd
    return c && ['next', 'back', 'exit', 'click'].includes(c.cmd) ? c : null
  } catch { return null }
}
