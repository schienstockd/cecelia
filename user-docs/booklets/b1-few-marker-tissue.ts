// B1 — Few-marker tissue, as a showcase (plan → D6): what the workflow gives, then the in-app recipe
// guide for the doing. Each show quotes its recipe step (`step`); every number is the fixture's
// (`{{…}}`, spec.ts → FACT_TOKEN). Work in progress: what is still missing is a `todo` placeholder.

import type { Booklet } from './spec.ts'

export const B1_FEW_MARKER_TISSUE: Booklet = {
  id: 'b1-few-marker-tissue',
  series: 'B',
  kind: 'showcase',
  title: 'Few-marker tissue',
  subtitle: 'From a confocal stack to counted, gated cell populations',
  audience: 'Anyone with fixed tissue sections and a handful of markers, deciding whether to analyse them in Cecelia.',
  get: 'What the workflow gives you, what it needs from the microscope, and where to start it.',
  need: ['A confocal stack with its metadata (pixel size, z-step)', 'Your marker for each channel'],
  sample: 'A confocal section of mouse lymphoid tissue after LCMV infection: {{lt.FtGoJO.channels}} channels, {{lt.FtGoJO.planes}} z-planes.',
  beforeYouImage: [
    { setting: 'Pixel size (zoom)', limits: 'The smallest cell you can segment, and every distance and volume', shows: 'The image table’s **Scale** column, and the **Metadata** page' },
    { setting: 'Z-range and z-step', limits: 'Cells cut off at the top and bottom of the stack; how well volumes are measured', shows: 'The image table’s **Z** column, and the **Metadata** page' },
    { setting: 'Detector gain', limits: 'Saturated pixels clip a marker, so bright and very bright cells look alike when you gate on it', shows: '**Checks** above the image table' },
    { setting: 'Channel bleed-through', limits: 'One marker shows in another channel: false double positives', shows: 'The optional **Cleanup** step corrects it' },
  ],
  intro: [],
  steps: [],
  showcase: {
    outcome: {
      shot: 'b1-p14',
      text: 'The P14 cells, gated on their markers, shown where they sit in the tissue: {{lt.FtGoJO.P14.count}} cells, {{lt.FtGoJO.P14.pct}}% of all the cells segmented.',
    },
    time: 'To do: time it on the fixture image. A chain then repeats it over the whole set.',
    shows: [
      {
        title: 'Your images, checked as they come in',
        step: 'fix-metadata',
        text: 'Each image lists its channels, z-planes and pixel size, and **Checks** flags acquisition problems before you analyse anything.',
        shot: 'b1-images',
      },
      {
        title: 'Every cell outlined',
        step: 'segment-an-image',
        shot: 'b1-outlines',
      },
      {
        title: 'Populations gated like flow',
        step: 'gate-populations',
        text: 'Here P14 cells, on LCMV-AF594 against P14-CTDR: {{lt.FtGoJO.P14.count}} cells.',
        shot: 'b1-gate',
      },
      {
        title: 'Who touches whom',
        step: 'spatial-analysis',
        todo: 'The spatial result. Needs: the spatial analysis run on the fixture (the P14 gate is there now).',
      },
    ],
    limits: [
      'It does not choose your gates: you draw them, and Kiwi can only describe them.',
      'Counts are only as good as the segmentation: check the outlines on the image before you trust a number.',
      'Saturated or bled-through channels cannot be rescued afterwards; fix them at the microscope.',
    ],
    recipe: 'few-marker-tissue',
  },
  askAi: { prompt: 'How many cells are in each of my populations, per image?', where: 'Kiwi, with the board or the populations attached.' },
  next: 'c1-using-ai',
  fixture: {
    // Channel names and the P14 box as the first confocal draft had them (guide-screenshots trials,
    // booklet-v1 "Confocal (static)"): order and box read off that draft, to be confirmed.
    projects: { lt: {
      source: '4rNbMp', images: { FtGoJO: 'Lymphoid tissue, LCMV' }, name: 'Lymphoid tissue', results: true,
      channels: { FtGoJO: ['B220-PB', 'XCR1-Venus', 'LCMV-AF594', 'P14-CTDR', 'CD3e-AF700', 'AF'] },
      gates: [{ image: 'FtGoJO', name: 'P14', x: 'LCMV-AF594', y: 'P14-CTDR', xRange: [5, 99], yRange: [15, null] }],
    } },
  },
  shots: [
    {
      // Shows 1: the image row and the Checks above it. Uid line under the name is masked (no ids).
      id: 'b1-images',
      project: 'lt',
      route: '/manage-images',
      crop: ['images.checks', 'images.table'],
      masks: ['css:.uid-row'],
      expect: [{ anchor: 'images.checks', text: 'Checks' }],
    },
    {
      id: 'b1-gate',
      project: 'lt',
      image: 'FtGoJO',
      scope: 'gate',
      route: '/gate',
      height: 1100,
      // Plot 2 is the gate's own plot (LCMV-AF594 × P14-CTDR); Plot 1 is the page's default pair
      crop: ['gate.plot@@1'],
      // the plot draws its labels on canvas: the count the caption cites is checked on the population's row
      expect: [{ anchor: 'gate.plot@@1', text: 'P14-CTDR' }, { anchor: 'popmanager.row', text: 'P14' },
               { anchor: 'popmanager.row', text: '{{lt.FtGoJO.P14.count}}' }, { anchor: 'popmanager.row', text: '{{lt.FtGoJO.P14.pct}}%' }],
    },
    {
      // The existing segmentation on a middle plane, as the first confocal draft showed it.
      id: 'b1-outlines',
      project: 'lt',
      image: 'FtGoJO',
      viewer: { plane: 13, zoom: 4, at: [0.35, 0.65], channels: ['B220-PB', 'XCR1-Venus', 'P14-CTDR'] },
      crop: ['css:canvas'],
    },
    {
      // The result: the gated P14 cells on the tissue (the viewer's On the image → Gated).
      id: 'b1-p14',
      project: 'lt',
      image: 'FtGoJO',
      viewer: { plane: 13, outlines: false, gated: true, zoom: 4, at: [0.35, 0.65], channels: ['B220-PB', 'XCR1-Venus', 'P14-CTDR'] },
      crop: ['css:canvas'],
    },
  ],
}
