// The shared shape behind every "run a function on some images" guide — drift correct, segment,
// track, and whatever module page comes next (plan D8).
//
// These pages are the SAME five moves: pick the set → tick the images → choose the function in
// `TaskRunner`'s dropdown → set the parameters → Run → watch the task rail. That skeleton lives in
// `ModuleLayout` + `TaskRunner`, i.e. two files and five anchors, shared by every module page. So the
// guide is a builder, not three hand-written step lists — writing the fourth by hand is how a pattern
// becomes four diverging variants (docs/UI.md → generalise by scenario, not per widget).
//
// What a caller supplies is only what is genuinely per-guide: which page, which function, what its
// parameters mean, and what to do with the output afterwards.

import type { GuideDef, GuideStep, Prereq } from './types'
import { PREREQ } from './prereqs'

export interface ModuleTaskGuideOpts {
  id: string
  title: string
  group: string
  icon: string
  summary: string
  route: string
  navLabel: string                 // the sidebar entry's wording, so the bubble can name it
  taskKey: string                  // the `task` key in the JSON spec — what the <select> holds
  funName: string                  // 'segment.cellpose' — what a task in the rail reports
  funLabel: string                 // 'Cellpose segmentation' — the dropdown's own wording
  // ModuleLayout's `module=` prop, which is what scopes the image-table SELECTION
  // (`getImageSelection(scope, setUid)`). Deliberately NOT called `module`: a page may pass a
  // different key to ModuleLayout than to TaskRunner — BehaviourModule uses `behaviourAnalysis` for the
  // layout and `behaviour` for the runner — and reading the wrong one gives a permanently empty
  // selection, i.e. a gate that never fires and a step the user must click past. Pinned per guide by
  // `guides.test.ts` against the page's own SFC.
  selectionModule: string
  waitLabel: string                // gerund for the parked bubble: 'Segmenting'
  prereqs?: Prereq[]
  intro?: string                   // one sentence on what this function is FOR
  selectHint?: string[]            // bullets for the image-selection step
  params?: string[]                // bullets naming the parameters that matter
  after?: GuideStep[]              // what to do with the output (QC, napari, the next page)
}

// Every guide the builder has produced, for the ratchet in `guides.test.ts` — it checks each one's
// `selectionModule` against the `<ModuleLayout module="…">` in the page's own SFC. Populated as a side
// effect of building, so a new guide is covered without registering it anywhere.
export const MODULE_TASK_GUIDES: { id: string; route: string; selectionModule: string; taskKey: string }[] = []

export function moduleTaskGuide(o: ModuleTaskGuideOpts): GuideDef {
  MODULE_TASK_GUIDES.push({
    id: o.id, route: o.route, selectionModule: o.selectionModule, taskKey: o.taskKey,
  })

  // Every module page hides its functions panel behind one shared flag, so every one of these steps
  // needs the same reveal (plan D5) — declared once here rather than per guide.
  const revealPanel = {
    needed: (c: { rightPanelCollapsed: boolean }) => c.rightPanelCollapsed,
    anchor: 'layout.rightPanelHandle',
    text: 'The functions panel is folded away — open it with this handle.',
    placement: 'left' as const,
  }

  const steps: GuideStep[] = [
    {
      anchor: `nav:${o.route}`,
      placement: 'right',
      title: o.navLabel,
      text: o.intro ?? `Open the ${o.navLabel} page.`,
      clickAnchor: true,
    },
    {
      anchor: 'set.select',
      route: o.route,
      placement: 'bottom-start',
      text: 'Check the active set is the one you mean — functions only see this set.',
      when: c => c.setUid !== null,
    },
    {
      anchor: 'images.table',
      route: o.route,
      placement: 'top-start',
      title: 'Tick the images to run on',
      text: 'Selection is per page, and it is remembered when you navigate away.',
      bullets: o.selectHint ?? ['Tick as many as you like — one task is queued per image.'],
      when: c => c.selection(o.selectionModule).length > 0,
    },
    {
      anchor: 'task.fun',
      route: o.route,
      placement: 'left',
      title: 'Choose the function',
      text: `Pick "${o.funLabel}" from the dropdown.`,
      reveal: revealPanel,
      when: c => c.anchorValue('task.fun') === o.taskKey,
    },
    {
      anchor: 'task.params',
      route: o.route,
      placement: 'left',
      title: 'Set the parameters',
      text: 'These are the settings worth getting right before you run.',
      bullets: o.params,
      reveal: revealPanel,
    },
    {
      anchor: 'task.run',
      route: o.route,
      placement: 'left',
      text: 'Run it — this queues one task per selected image.',
      reveal: revealPanel,
      clickAnchor: true,
    },
    {
      anchor: 'task.list',
      route: o.route,
      placement: 'left',
      title: 'Watch it here',
      text: 'Every run appears in this rail with its live log, progress and a cancel button.',
      bullets: [
        'You can leave the page — runs are server-side, not tab-side.',
        'Click a row to read its log.',
      ],
      reveal: revealPanel,
      awaitTask: { fun: o.funName, label: o.waitLabel },
    },
    ...(o.after ?? []),
  ]

  return {
    id: o.id,
    title: o.title,
    group: o.group,
    icon: o.icon,
    summary: o.summary,
    prereqs: o.prereqs ?? [PREREQ.projectOpen, PREREQ.imageImported],
    steps,
  }
}
