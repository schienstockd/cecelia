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

export const tourGuide: GuideDef = {
  id: 'find-your-way-around',
  title: 'Find your way around',
  group: 'Start',
  icon: 'pi-map',
  summary: 'The buttons that are not on any page: menu, guides, connection, console, Settings.',
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
      anchor: 'sidebar.projectBlock',
      placement: 'right',
      title: 'Your project',
      text: 'Open, create and switch projects here — everything else on screen belongs to this one.',
    },
    {
      anchor: 'header.brand',
      placement: 'bottom-start',
      title: "What's new",
      text: 'The feijoa opens release notes and the tip cards you just came from.',
      bullets: ['Cycle the sketches to see what Cecelia does.'],
    },
    {
      anchor: 'header.guides',
      placement: 'bottom',
      title: 'The guides',
      text: 'This compass lists every guide, including this one — it is how you get back here.',
      bullets: [
        'Each one walks you through a real step on your own data.',
        'A guide only points and watches; it never clicks for you.',
      ],
    },
    {
      anchor: 'header.help',
      placement: 'bottom',
      title: 'Bugs and questions',
      text: 'GitHub for anything broken, the Zulip chat for anything you are unsure about.',
      bullets: [
        'Both open outside Cecelia, in a new tab.',
        'Questions in chat are welcome — no issue needed.',
      ],
    },
    {
      anchor: 'header.wsBadge',
      placement: 'bottom-end',
      title: 'The backend',
      text: 'Cecelia is a browser front end onto a Julia server — this says whether it is reachable.',
      bullets: [
        'Green: connected, and tasks will run.',
        'Anything else: the server is down, and nothing will start.',
      ],
    },
    {
      anchor: 'console.bar',
      placement: 'top-start',
      title: 'The console',
      text: 'Warnings and errors land here, newest last, whichever page you were on.',
      bullets: [
        'Click a line to expand the detail.',
        'Worth opening first when something did not happen.',
      ],
    },
    {
      anchor: 'sidebar.viewerCta',
      placement: 'right',
      title: 'The viewer',
      text: 'Opens images in napari, with segmentations and tracks as layers you can toggle.',
    },
    {
      anchor: 'sidebar.labLogCta',
      placement: 'right',
      title: 'The lab log',
      text: 'A per-project record of what was run and what you concluded, kept as you go.',
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
      route: '/settings',
      placement: 'top-start',
      title: 'Updates',
      text: 'Your version, and a Check button — a new release installs from here.',
      bullets: ['A shared installation is updated by whoever administers it.'],
    },
    {
      anchor: 'settings.storageScan',
      route: '/settings',
      placement: 'top-start',
      title: 'Disk space',
      text: 'Scan shows where the project’s bytes went, and what can be freed without losing work.',
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
      anchor: 'sidebar.quit',
      placement: 'right',
      title: 'Leaving',
      text: 'Closing the browser tab does not stop Cecelia — the backend keeps running until you quit here.',
      bullets: [
        'Not now — this is for when you are done for the day.',
        'It stops napari and the notebook server too.',
      ],
    },
  ],
}
