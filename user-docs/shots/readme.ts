// The screenshots at the top of the repository's README.md (the block between its `docshots` markers,
// written by frontend/src/utils/readmeShots.ts): one row, fixed tissue beside live imaging, each the
// viewer window at one shape. A multiplex section under the channel lens, then a time-lapse in 3D. Captions
// from the fixture's facts, the public section's citation from its data source.

import type { ShotSet } from './spec.ts'
import { HUBMAP_INTESTINE, TCELL_TIMELAPSE } from './fixtures.ts'

export const README_SHOTS: ShotSet = {
  id: 'readme',
  page: '../README.md',
  // light only: a <picture> follows the reader's OS scheme, not GitHub's theme, so a dark image showed
  // on a light GitHub page
  themes: ['light'],
  fixture: { projects: { tcells: TCELL_TIMELAPSE, hubmap: HUBMAP_INTESTINE } },
  shots: [
    {
      // Fixed tissue: the viewer window on a PhenoCycler section, the segmentation coloured by cell
      // cluster, a spotlight lens pinned on a B cell follicle's edge, its chip counting the clusters inside
      id: 'readme-lens',
      project: 'hubmap',
      image: '2dR91B',
      viewer: { channels: ['DAPI', 'CD20', 'CD3'], clusters: true, maskColour: 'pops', pointSize: 2, folded: true, camera: { x: 8292, y: 1479, zoom: 1.49 },
                lens: { x: 8274, y: 1450, r: 197, channels: [] } },
      crop: ['css:body'],
      overview: true,
      windowWidth: 840,
      width: 300,
      caption: 'A PhenoCycler section of human small intestine, each cell coloured by its cluster, a spotlight lens counting the clusters inside it. Data: {{hubmap.2dR91B.citation}}, doi:{{hubmap.2dR91B.doi}}.',
    },
    {
      // Live imaging: the viewer window on a two-photon time-lapse in 3D, face on: outlines coloured by
      // the gated populations (ungated cells grey), their dots and cell-track ribbons. The same window shape as the section beside it, so the
      // two figures of the row stand the same size.
      id: 'readme-viewer',
      heroRow: true,
      project: 'tcells',
      image: 'TKufLV',
      viewer: { volume: true, lift: 0.12, top: 99.99, folded: true, mask: 'Tcells', gated: true, ribbons: true, time: 124, tail: 30,
                maskColour: 'pops', trackColour: 'pop', camera: { zoom: 1.7 }, channels: ['P14-ubiTom', 'gBT-uGFP', 'OTI-CTV'],
                // the source image's saved view (driftCorrected), tuned by hand: Auto leaves the background noisy
                contrast: { 'P14-ubiTom': [175, 600], 'gBT-uGFP': [230, 1000], 'OTI-CTV': [200, 1349] } },
      crop: ['css:body'],
      overview: true,
      // the viewer's timestamp at timepoint 124 (10 s frames): a fact of the image
      stable: ['0:20:40'],
      windowWidth: 840,
      width: 300,
      caption: 'A two-photon time-lapse of three T cell subsets in 3D, its {{tcells.TKufLV.Tcells.tracks}} tracks drawn as ribbons.',
    },
  ],
}
