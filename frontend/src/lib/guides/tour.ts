// "Find your way around" — the orientation tour, and the only guide in the catalogue with NO
// prerequisites (docs/todo/GUIDE_SYSTEM_PLAN.md).
//
// Every other guide teaches a pipeline step and therefore needs data: an open project, an imported
// image, a segmentation. This one deliberately points ONLY at chrome — header, sidebar, console,
// Settings — so it works on a first launch with an empty project, which is exactly when it runs. Do
// not add a step here that points at an image table, a set, or a plot; the moment one does, the tour
// stops working for the person it was written for.
//
// It is also the guide the What's New "about Cecelia" card hands off to (`lib/tips.ts`), and the one
// App.vue starts by itself the first time that dialog is closed — so it is the first interactive
// thing a new user sees. Everything is Next-only: no gates, nothing to get stuck behind. The two
// `clickAnchor` steps are the exceptions, and both are opt-in accelerators rather than requirements
// (Next still moves on, plan D2).

import type { GuideDef } from './types'
import { GUIDE_MATURITY } from './maturityRecords'
import { settingsClosed, settingsReveal } from './settingsSteps'

export const tourGuide: GuideDef = {
  id: 'find-your-way-around',
  maturity: GUIDE_MATURITY['find-your-way-around'],
  title: 'Find your way around',
  group: 'Start',
  icon: 'pi-map',
  summary: 'The buttons that are not on any page: menu, Help, viewer, console, Settings.',
  // Intentionally empty — see the header. This is the guide you run BEFORE you have anything.
  prereqs: [],

  steps: [
    {
      anchor: 'header.navToggle',
      placement: 'bottom-start',
      title: 'The menu',
      text: 'Every analysis step is a page in this menu, ordered the way a project runs.',
      bullets: [
        'Data first, then populations, then analysis.',
        'Collapse it when you want the width.',
      ],
    },
    {
      anchor: 'header.project',
      placement: 'bottom-start',
      title: 'Your project',
      text: 'Open, create and switch projects here — everything else on screen belongs to this one.',
    },
    {
      anchor: 'header.helpMenu',
      placement: 'bottom-end',
      title: 'Help',
      text: 'Guides, feature maturity, the icon and colour glossaries, what’s new, and where to ask.',
      bullets: [
        'Guides is how you get back here; each walks a real step on your own data.',
        'GitHub for anything broken, the Zulip chat for anything you are unsure about.',
      ],
    },
    {
      anchor: 'console.bar',
      placement: 'top-start',
      title: 'The console',
      text: 'Warnings and errors land here, newest last, whichever page you were on.',
      bullets: [
        'Click a line to expand the detail.',
        'Right end: the version, and a warning if the backend drops.',
      ],
    },
    {
      anchor: 'launcher.viewer',
      placement: 'bottom',
      title: 'The viewer',
      text: 'Opens images in the viewer, with segmentations and tracks as layers you can toggle.',
      bullets: ['Fixing a mask or a track: Correct, on the Segment and Track pages.'],
    },
    {
      anchor: 'launcher.labLog',
      placement: 'bottom',
      title: 'The lab log',
      text: 'A per-project record of what was run and what you concluded, kept as you go.',
    },
    {
      anchor: 'launcher.kiwi',
      placement: 'bottom-end',
      title: 'Kiwi — answers you can check',
      text: 'Ask about what’s on screen: every claim links to its data, and Kiwi can’t change anything.',
      bullets: ['On-demand only: nothing happens until you ask.',
                'Want Claude to build something? Start a Claude Code session from here.'],
      onlyIf: c => c.aiEnabled,
    },
    {
      // AI is off by default; the launcher stays as the way to turn it on (GUI_CLARITY_PLAN Decision 9)
      anchor: 'launcher.kiwi',
      placement: 'bottom-end',
      title: 'Kiwi — optional AI',
      text: 'Off by default. Turn it on here if your lab’s data policy allows it.',
      onlyIf: c => !c.aiEnabled,
    },
    {
      anchor: 'sidebar.settings',
      placement: 'right',
      title: 'Settings',
      text: 'Two things worth knowing live in here — click through and I will point them out.',
      clickAnchor: true,
    },
    {
      anchor: 'settings.updates',
      onlyIf: c => c.canManageMachine,      // the pane is peanut-only (SETTINGS_SCOPE_PLAN)
      placement: 'right',
      title: 'Updates',
      text: 'Your version, and a Check button — a new release installs from here.',
      reveal: settingsClosed('settings.updates'),
      bullets: ['A shared installation is updated by whoever administers it.'],
    },
    {
      anchor: 'settings.storageScan',
      placement: 'top-start',
      title: 'Disk space',
      text: 'Scan shows where the project’s bytes went, and what can be freed without losing work.',
      reveal: settingsReveal('settings.storageScan', 'settings.storage', 'Storage'),
      bullets: [
        'Corrections and crops keep the old version around — those are the reclaimable ones.',
        'Freeing them keeps each image’s active version.',
        'Worth a look before a big run.',
      ],
    },
    // Last, and deliberately the one step that does NOT invite its click — every other step describes
    // something to try, and this one points at the button that quits the app. No `clickAnchor`, and
    // the copy says when to use it rather than to use it now. The button is a `ConfirmButton`, so a
    // stray click arms rather than quits; that is the safety net, not the plan.
    //
    // This carries what the `cc.hint.shutdown` first-use callout used to say. It moved here because
    // the fact is about a button, and a bubble beside the button beats a sentence on an unrelated page.
    {
      anchor: 'header.profile',
      placement: 'bottom-end',
      title: 'Leaving',
      text: 'Closing the browser tab does not stop Cecelia — the backend keeps running until you quit from this menu.',
      bullets: [
        'Not now — this is for when you are done for the day.',
        'It stops the viewer and the notebook server too.',
      ],
    },
  ],
}
