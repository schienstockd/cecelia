// "Create a notebook" — a service page rather than a function runner, so it has its own shape: the
// thing you wait for is a SERVER starting, not a task in the rail, and the notebook itself opens in a
// browser tab Cecelia doesn't own.
//
// That last bit is the same boundary as napari (plan R1): once the tab is open, the guide can only say
// what to expect, not point at it. So the steps front-load what matters — that the notebook comes
// pre-wired to the project — and stop at the door.

import type { GuideDef } from './types'
import { PREREQ } from './prereqs'

export const notebooksGuide: GuideDef = {
  id: 'create-a-notebook',
  title: 'Create a notebook',
  group: 'Analysis',
  icon: 'pi-book',
  summary: 'Open a Pluto notebook wired to your project for analysis the built-in plots do not cover.',
  prereqs: [PREREQ.projectOpen],

  steps: [
    {
      anchor: 'nav:/notebooks',
      placement: 'right',
      title: 'Notebooks',
      text: 'Notebooks are the escape hatch: reactive Julia on your project data, no export needed.',
      clickAnchor: true,
    },
    {
      anchor: 'notebooks.startServer',
      route: '/notebooks',
      placement: 'bottom-start',
      title: 'Start the server',
      text: 'Pluto runs as its own server — start it once per session.',
      bullets: ['The first launch precompiles and can take a minute.'],
      // The Start button is replaced by "Open Notebooks" once it's up, so the swap IS the signal.
      when: c => c.anchorExists('notebooks.openHome'),
    },
    {
      anchor: 'notebooks.addRow',
      route: '/notebooks',
      placement: 'bottom-start',
      title: 'Add a notebook',
      text: 'Name it and add it — you get a notebook already pointed at this project.',
      bullets: [
        'The project is loaded for you; no paths to wire up.',
        'Shipped examples are listed below, read-only — duplicate one to edit it.',
      ],
    },
    {
      anchor: 'notebooks.table',
      route: '/notebooks',
      placement: 'top-start',
      title: 'The registry',
      text: 'Every notebook in the project, with a snapshot history you can roll back to.',
      bullets: [
        'The ↗ button opens one in Pluto, in a new tab.',
        'Snapshot before a big edit — that is the provenance trail.',
      ],
    },
    {
      anchor: 'notebooks.openHome',
      route: '/notebooks',
      placement: 'bottom-start',
      title: 'Fast plots',
      text: 'The first plot compiles from scratch unless you build the fast-plot cache.',
      bullets: [
        'It is optional — notebooks work without it, just slow on the first plot.',
        'The build takes ~10 min in the background.',
      ],
    },
    {
      title: 'One thing to know',
      text: 'Pluto is reactive: changing a cell re-runs everything that depends on it.',
      bullets: [
        'So there is no stale-state confusion, but a slow cell re-runs more often.',
        'Ask Claude to draft a notebook for you — it can create one in this registry.',
      ],
    },
  ],
}
