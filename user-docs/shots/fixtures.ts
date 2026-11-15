// Lab-data fixture projects more than one shot set copies (each set records its own copies). Sample
// facts a caption cites are measured on the copy, never typed: the names below are what the copies
// show, read back by doc-fixture as `{{<project>.<image>.name}}`.

import type { Fixture } from './spec.ts'

type FixtureProject = NonNullable<Fixture['projects']>[string]

/** A three-photon time-lapse of spleen (source project "unimelb 3P spleen"; channels THG, Tcells-uGFP,
 *  Bcells-ubiTom, SHG), its T and B cells segmented and tracked as `T` and `B`. Each population's `qc` gate is
 *  recreated from the source's own, so the track counts are measured. */
export const SPLEEN_TIMELAPSE: FixtureProject = {
  source: '4kS67f', images: { k3Tx90: 'Spleen, three-photon time-lapse' }, name: 'Spleen time-lapse', results: true,
  gates: [
    { image: 'k3Tx90', segmentation: 'T', name: 'qc', fromSource: true },
    { image: 'k3Tx90', segmentation: 'B', name: 'qc', fromSource: true },
  ],
}

/** A z-stack of lymphoid tissue (the name using-ai.ts gives the same image), segmented as `cells`. Its
 *  channels carry no marker names in the source, so no caption names one. */
export const LYMPHOID_SECTION: FixtureProject = {
  source: '4rNbMp', images: { FtGoJO: 'Lymphoid tissue, LCMV' }, name: 'Lymphoid tissue', results: true,
}
