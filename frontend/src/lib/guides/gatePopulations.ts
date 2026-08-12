// "Create and gate populations" — the first guide whose page is a CANVAS rather than a function
// runner, so it can't come from `moduleTask.ts` (plan D8): nothing is queued, nothing runs in the
// background, and the output is a sidecar the gating engine writes as you draw.
//
// The through-line is that a population is a NAMED SUBSET defined by a gate on two measures, and that
// gates nest — which is the one idea the FlowJo-shaped UI assumes you already have.

import type { GuideDef } from './types'
import { PREREQ } from './prereqs'

export const gatePopulationsGuide: GuideDef = {
  id: 'gate-populations',
  title: 'Create and gate populations',
  group: 'Populations',
  icon: 'pi-chart-scatter',
  summary: 'Draw gates on two measures at a time to carve your cells into named populations.',
  prereqs: [PREREQ.projectOpen, PREREQ.segmented],

  steps: [
    {
      anchor: 'nav:/gate',
      placement: 'right',
      title: 'Gate',
      text: 'Gating is FlowJo-style: pick two measures, draw round the cells you want, name them.',
      clickAnchor: true,
    },
    {
      anchor: 'images.table',
      route: '/gate',
      placement: 'top-start',
      title: 'One image at a time',
      text: 'Gating works on a single image — pick one and its cells load below.',
      bullets: ['Gates can be copied to the rest of the set once you are happy with them.'],
      when: c => c.selection('gate').length === 1,
    },
    {
      anchor: 'gate.segmentation',
      route: '/gate',
      placement: 'bottom-start',
      title: 'Which segmentation',
      text: 'You gate on the measures of one label set — pick it here if you have several.',
    },
    {
      anchor: 'gate.addPlot',
      route: '/gate',
      placement: 'bottom-start',
      text: 'Add a plot — each one is a pair of measures you can draw on.',
      clickAnchor: true,
    },
    {
      anchor: 'gate.axes',
      route: '/gate',
      placement: 'left',
      title: 'Choose the two measures',
      text: 'X and Y take any channel intensity or shape measure the segmentation carried.',
      bullets: [
        'The transform beside each axis is usually what makes a blob resolve into groups.',
        '"pop" is the population you are looking at — new gates land under it.',
      ],
    },
    {
      anchor: 'gate.drawTool',
      route: '/gate',
      placement: 'bottom-start',
      title: 'Pick a shape',
      text: 'Choose a gate shape, then drag on the plot to draw it.',
      bullets: ['Hold Shift to adjust a gate you already drew.'],
      // The name box only exists once a gate has been drawn — so its appearance IS the signal that
      // the user managed it, with no state to plumb.
      when: c => c.anchorExists('gate.name'),
    },
    {
      anchor: 'gate.name',
      route: '/gate',
      placement: 'top-start',
      title: 'Name it',
      text: 'A population is just a named gate — the name is how everything downstream refers to it.',
      bullets: [
        'Names are case-insensitively unique across the whole tree, not just among siblings.',
        'A leading underscore is reserved for tracked / clustering populations.',
      ],
    },
    {
      anchor: 'gate.popManager',
      route: '/gate',
      placement: 'bottom-end',
      text: 'The population manager shows the tree you are building.',
      clickAnchor: true,
    },
    {
      anchor: 'popmanager.row',
      route: '/gate',
      placement: 'left',
      title: 'Gates nest',
      text: 'Each population shows its cell count and its share of its parent.',
      bullets: [
        'Click one to plot it, then draw inside it to subset further.',
        'That nesting is the gating strategy — CD3+ → CD4+ → activated.',
      ],
    },
    {
      title: 'Populations are live',
      text: 'Gates are stored as definitions, not as a frozen list of cells.',
      bullets: [
        'Re-segment and your gates still apply.',
        'Populations show up in napari, in plots, and as a tracking input.',
      ],
    },
  ],
}
