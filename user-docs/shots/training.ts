// The screenshots of user-docs/training.md (the page itself: frontend/src/utils/trainingDoc.ts).
// Orientation shots, not a tutorial: each shows WHERE a step happens, small and cropped; the in-app
// guide teaches the clicks. One lab image (a confocal stack of lymphoid tissue after LCMV), copied
// with its segmentation and one recreated gate, viewed by a profile with AI off — the training does
// not depend on AI.

import type { ShotSet } from './spec.ts'
import { LYMPHOID_SECTION } from './fixtures.ts'

export const TRAINING_SHOTS: ShotSet = {
  id: 'training',
  page: 'training.md',
  themes: ['light'],
  fixture: {
    projects: { lt: {
      ...LYMPHOID_SECTION,
      // The source carries no channel names; these are set on the copy so the gate reads on named axes.
      channels: { FtGoJO: ['B220-PB', 'XCR1-Venus', 'LCMV-AF594', 'P14-CTDR', 'CD3e-AF700', 'AF'] },
      gates: [{ image: 'FtGoJO', name: 'P14', x: 'LCMV-AF594', y: 'P14-CTDR', xRange: [5, 99], yRange: [15, null] }],
    } },
    profiles: [{ name: 'trainer', ai: false }],
  },
  shots: [
    {
      // Where every guide starts.
      id: 'tr-guides',
      profile: 'trainer',
      project: 'lt',
      route: '/manage-images',
      point: { anchor: 'header.helpMenu', label: 'Help → Guides', side: 'bottom' },
      crop: ['header.helpMenu'],
      expect: [{ anchor: 'header.helpMenu', text: 'Help' }],
    },
    {
      // The set's checks above its image table; the image's channels, z-planes and pixel size in its row.
      id: 'tr-checks',
      profile: 'trainer',
      project: 'lt',
      route: '/manage-images',
      point: { anchor: 'checks.acquisition', label: 'Open the Acquisition check', side: 'bottom' },
      crop: ['images.checks', 'images.table'],
      masks: ['css:.img-uid'],
      expect: [{ anchor: 'images.checks', text: 'Acquisition' }],
    },
    {
      // The check before a plan: the plan row, and what is off across the set.
      id: 'tr-acq-check',
      profile: 'trainer',
      project: 'lt',
      route: '/manage-images',
      actions: [{ click: 'checks.acquisition' }, { waitFor: 'acqCheck.plan' }],
      point: { anchor: 'acqCheck.plan', label: 'Set a plan first', side: 'top' },
      crop: ['acqCheck.plan', 'acqCheck.setFindings'],
      expect: [{ anchor: 'acqCheck.plan', text: 'Set a plan' }],
    },
    {
      // What to judge after segmenting: the outlines on the image, close enough to see single cells.
      id: 'tr-mask',
      profile: 'trainer',
      project: 'lt',
      image: 'FtGoJO',
      viewer: { plane: 13, zoom: 5, at: [0.4, 0.75] },
      crop: ['css:canvas'],
      aspect: 2,
      width: 600,
    },
    {
      // The gate on two channels, and the population it makes: its count and share of its parent.
      id: 'tr-gate',
      profile: 'trainer',
      project: 'lt',
      image: 'FtGoJO',
      scope: 'gate',
      route: '/gate',
      height: 1200,
      actions: [{ click: 'canvas.tile' }],
      point: { anchor: 'popmanager.row', label: 'Count · share of parent', side: 'bottom' },
      crop: ['gate.plot@@1', 'popmanager.row'],
      expect: [{ anchor: 'popmanager.row', text: '{{lt.FtGoJO.P14.count}}' }],
    },
  ],
}
