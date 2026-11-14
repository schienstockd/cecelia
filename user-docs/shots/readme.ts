// The screenshots at the top of the repository's README.md (the block between its `docshots` markers,
// written by frontend/src/utils/readmeShots.ts): one large picture of a real result, then "What it looks
// like". Lab data only, captions from the fixture's facts. Both schemes: GitHub shows the reader's.

import type { ShotSet } from './spec.ts'
import { LYMPHOID_SECTION, SPLEEN_TIMELAPSE } from './fixtures.ts'

export const README_SHOTS: ShotSet = {
  id: 'readme',
  page: '../README.md',
  themes: ['light', 'dark'],
  fixture: { projects: { spleen: SPLEEN_TIMELAPSE, lymphoid: LYMPHOID_SECTION } },
  shots: [
    {
      // The hero: every track of both populations on the last frame, outlines off so the tracks read.
      id: 'readme-tracks',
      project: 'spleen',
      image: 'k3Tx90',
      viewer: { plane: 10, zoom: 4, at: [0.58, 0.5], outlines: false, tracks: ['T', 'B'], time: 74, tail: 75, channels: ['THG', 'Tcells-uGFP', 'SHG'] },
      crop: ['css:canvas'],
      aspect: 1.2,
      inset: 8,
      // the counts are the whole movie's (the tracking review's), the picture part of its field: said so
      caption: '{{spleen.k3Tx90.name}}, part of its field. The whole movie: {{spleen.k3Tx90.T.tracks}} T cell and {{spleen.k3Tx90.B.tracks}} B cell tracks over {{spleen.k3Tx90.timepoints}} frames, {{spleen.k3Tx90.interval}} s apart.',
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
