// The screenshots at the top of the repository's README.md (the block between its `docshots` markers,
// written by frontend/src/utils/readmeShots.ts): the app, its data and its viewer side by side on one
// image, then "What it looks like". Lab data only, captions from the fixture's facts. Both schemes: GitHub shows the reader's.

import type { ShotSet } from './spec.ts'
import { LYMPHOID_SECTION, SPLEEN_TIMELAPSE } from './fixtures.ts'

export const README_SHOTS: ShotSet = {
  id: 'readme',
  page: '../README.md',
  themes: ['light', 'dark'],
  fixture: { projects: { spleen: SPLEEN_TIMELAPSE, lymphoid: LYMPHOID_SECTION } },
  shots: [
    {
      // The hero, after Fig. 1c of the Cecelia paper: the whole app, its data and its viewer side by side
      // on one image. The Gate page (the plots of the populations) ...
      id: 'readme-gate',
      project: 'spleen',
      image: 'k3Tx90',
      scope: 'gate',
      route: '/gate',
      actions: [{ select: 'gate.segmentation', option: 'T' }, { click: 'canvas.tile' }],
      crop: ['css:body'],
      // the images table's duration and frame interval
      stable: ['37m', '{{spleen.k3Tx90.interval}} s'],
      overview: true,
      width: 500,
      caption: 'The data and the image side by side: the Gate page and the viewer window, both on {{spleen.k3Tx90.name}}.',
    },
    {
      // ... and the viewer window on the same image, its gated populations drawn, at the Gate page's height
      // (980 px wide at the same scale as the page's 1440: 340 px beside its 500)
      id: 'readme-viewer',
      project: 'spleen',
      image: 'k3Tx90',
      viewer: { plane: 10, mask: 'T', gated: true, time: 74, channels: ['THG', 'Tcells-uGFP', 'SHG'] },
      crop: ['css:body'],
      overview: true,
      beside: 'readme-gate',
      windowWidth: 980,
      width: 340,
    },
    {
      id: 'readme-outlines',
      project: 'lymphoid',
      image: 'FtGoJO',
      viewer: { plane: 13, zoom: 5, at: [0.4, 0.75] },
      crop: ['css:canvas'],
      width: 560,
      caption: '{{lymphoid.FtGoJO.name}}: every cell outlined, on one of its {{lymphoid.FtGoJO.planes}} planes.',
    },
    {
      // A step says what it is for and when it fits, before you run it.
      id: 'readme-when',
      project: 'lymphoid',
      image: 'FtGoJO',
      scope: 'segment',
      route: '/segment',
      actions: [{ open: 'task.whenToggle' }],
      crop: ['task.fun', 'task.about'],
      expect: [{ anchor: 'task.about', text: 'Use when' }],
      caption: 'Each step says what it is for, when to use it and when not.',
    },
  ],
}
