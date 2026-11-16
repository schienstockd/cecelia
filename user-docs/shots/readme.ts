// The screenshots at the top of the repository's README.md (the block between its `docshots` markers,
// written by frontend/src/utils/readmeShots.ts): the app, its data and its viewer side by side on one
// image. Lab data only, captions from the fixture's facts.

import type { ShotSet } from './spec.ts'
import { TCELL_TIMELAPSE } from './fixtures.ts'

// a gate plot's population picker (no anchor of its own: the label beside it says "pop")
const POP_SELECT = 'css:label.ax-row:has(span.ax-lbl:text-is("pop")) select'

export const README_SHOTS: ShotSet = {
  id: 'readme',
  page: '../README.md',
  // light only: a <picture> follows the reader's OS scheme, not GitHub's theme, so a dark image showed
  // on a light GitHub page
  themes: ['light'],
  fixture: { projects: { tcells: TCELL_TIMELAPSE } },
  shots: [
    {
      // The hero, after Fig. 1c of the Cecelia paper: the two windows side by side on a laptop screen,
      // where the main window folds its menu and functions panel — 840 px, the narrowest at which its set
      // bar (set, images, checks) keeps one row. The Gate page
      // (the plots of the populations) ...
      id: 'readme-gate',
      project: 'tcells',
      image: 'TKufLV',
      scope: 'gate',
      route: '/gate',
      // the rail hidden (in a narrow window it floats over the second plot); each plot on the population
      // its gates are drawn in: P14 × gBT on non-OTI (gBT, P14), P14 × OTI on qc (OTI)
      actions: [{ select: 'gate.segmentation', option: 'Tcells' }, { unpress: 'gate.popManager' }, { click: 'canvas.tile' },
                { select: POP_SELECT, option: '/qc/non-OTI' }, { select: `${POP_SELECT}@@1`, option: '/qc' }],
      crop: ['css:body'],
      // the images table's duration and frame interval
      stable: ['30m', '{{tcells.TKufLV.interval}} s'],
      overview: true,
      windowWidth: 840,
      width: 439,
      caption: 'Side by side: the Gate page and the viewer window on one two-photon time-lapse of three T cell subsets, its {{tcells.TKufLV.Tcells.tracks}} tracks drawn in 3D.',
    },
    {
      // ... and the viewer window on the same image in 3D, face on: outlines, the gated
      // populations and their cell-track ribbons
      id: 'readme-viewer',
      project: 'tcells',
      image: 'TKufLV',
      viewer: { volume: true, lift: 0.12, top: 99, folded: true, mask: 'Tcells', gated: true, ribbons: true, time: 120, tail: 30,
                channels: ['P14-ubiTom', 'gBT-uGFP', 'OTI-CTV'] },
      crop: ['css:body'],
      overview: true,
      beside: 'readme-gate',
      windowWidth: 768,
      width: 401,
    },
  ],
}
