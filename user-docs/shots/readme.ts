// The screenshots at the top of the repository's README.md (the block between its `docshots` markers,
// written by frontend/src/utils/readmeShots.ts): one row, fixed tissue beside live imaging. A multiplex
// section under the channel lens, then the app and its viewer side by side on one time-lapse. Captions
// from the fixture's facts, the public section's citation from its data source.

import type { ShotSet } from './spec.ts'
import { HUBMAP_INTESTINE, TCELL_TIMELAPSE } from './fixtures.ts'

// a gate plot's population picker (no anchor of its own: the label beside it says "pop")
const POP_SELECT = 'css:label.ax-row:has(span.ax-lbl:text-is("pop")) select'

export const README_SHOTS: ShotSet = {
  id: 'readme',
  page: '../README.md',
  // light only: a <picture> follows the reader's OS scheme, not GitHub's theme, so a dark image showed
  // on a light GitHub page
  themes: ['light'],
  fixture: { projects: { tcells: TCELL_TIMELAPSE, hubmap: HUBMAP_INTESTINE } },
  shots: [
    {
      // Fixed tissue: the viewer window on a PhenoCycler section, a spotlight lens pinned on a B cell
      // follicle's edge, its chip counting the cell clusters inside
      id: 'readme-lens',
      project: 'hubmap',
      image: '2dR91B',
      viewer: { channels: ['DAPI', 'CD20', 'CD3'], clusters: true, pointSize: 2, folded: true, camera: { x: 8292, y: 1479, zoom: 1.49 },
                lens: { x: 8274, y: 1450, r: 197, channels: [] } },
      crop: ['css:body'],
      overview: true,
      // the Gate window's shape, so the two figures of the row stand the same height
      windowWidth: 840,
      width: 300,
      caption: 'A spotlight lens on a PhenoCycler section of human small intestine, counting the cell clusters inside it. Data: {{hubmap.2dR91B.citation}}, doi:{{hubmap.2dR91B.doi}}.',
    },
    {
      // Live imaging, after Fig. 1c of the Cecelia paper: the two windows side by side on a laptop screen,
      // where the main window folds its menu and functions panel — 840 px, the narrowest at which its set
      // bar (set, images, checks) keeps one row. The Gate page
      // (the plots of the populations) ...
      id: 'readme-gate',
      heroRow: true,
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
      width: 300,
      caption: 'Side by side: the Gate page and the viewer window on one two-photon time-lapse of three T cell subsets, its {{tcells.TKufLV.Tcells.tracks}} tracks drawn in 3D.',
    },
    {
      // ... and the viewer window on the same image in 3D, face on: outlines, the gated
      // populations and their cell-track ribbons
      id: 'readme-viewer',
      project: 'tcells',
      image: 'TKufLV',
      viewer: { volume: true, lift: 0.12, top: 99.99, folded: true, mask: 'Tcells', gated: true, ribbons: true, time: 120, tail: 30,
                channels: ['P14-ubiTom', 'gBT-uGFP', 'OTI-CTV'] },
      crop: ['css:body'],
      overview: true,
      beside: 'readme-gate',
      // the viewer's timestamp at timepoint 120 (10 s frames): a fact of the image
      stable: ['0:20'],
      windowWidth: 768,
      width: 274,
    },
  ],
}
