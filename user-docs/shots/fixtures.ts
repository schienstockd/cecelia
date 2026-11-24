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

/** A two-photon time-lapse of three T cell subsets (source project "T cells"; channels AF, P14-ubiTom,
 *  gBT-uGFP, OTI-CTV), segmented and tracked as `Tcells`. The source's gates are recreated in its own tree:
 *  `qc`, then OTI under it, and gBT and P14 under the cells that are not OTI. */
export const TCELL_TIMELAPSE: FixtureProject = {
  source: 'RFtn9f', images: { TKufLV: 'T cell subsets, two-photon time-lapse' }, name: 'T cell subsets', results: true,
  gates: [
    { image: 'TKufLV', segmentation: 'Tcells', name: 'qc', fromSource: true },
    { image: 'TKufLV', segmentation: 'Tcells', name: 'OTI', parent: '/qc', fromSource: true },
    { image: 'TKufLV', segmentation: 'Tcells', name: 'non-OTI', parent: '/qc', fromSource: true },
    { image: 'TKufLV', segmentation: 'Tcells', name: 'gBT', parent: '/qc/non-OTI', fromSource: true },
    { image: 'TKufLV', segmentation: 'Tcells', name: 'P14', parent: '/qc/non-OTI', fromSource: true },
  ],
}

/** A PhenoCycler section of human small intestine (source project "PhenoCycler 0064520 cycle registration",
 *  set "HuMAP tissues"; 60 named markers), segmented as `cells` and clustered. Its cell clusters are
 *  recreated from the source's own (what a lens's chip counts). Public data, not the lab's: HuBMAP
 *  HBM666.MCZF.973, recorded as its set's data source, which the copy carries, so a caption cites
 *  `{{<project>.2dR91B.citation}}` and `.doi` from the record, never typed. */
export const HUBMAP_INTESTINE: FixtureProject = {
  source: 'mjphFQ', images: { '2dR91B': 'Human small intestine, PhenoCycler' }, name: 'Small intestine', results: true,
  gates: ['Epithelial', 'DEFA5-ITLN1-OLFM4 crypt', 'CD20 B', 'Lymphoid, mixed', 'CD38 plasma', 'Myeloid',
          'CD3-PGP95, CD45 low', 'Endothelial', 'Stromal', 'CD56-SYNAP-TENC', 'Low signal']
    .map(name => ({ image: '2dR91B', name, popType: 'clust' as const, fromSource: true as const })),
}
