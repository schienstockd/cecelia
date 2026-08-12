// The guides beyond the original seven (docs/todo/GUIDE_SYSTEM_PLAN.md → *The guides*), added because
// each answers a question users actually arrive with:
//
//   fix-metadata      — "the scale bar is wrong / my channels are called Channel 1". THE most common
//                       first failure, and the import guide already points at it.
//   behaviour-states  — the natural end of the tracking arc; `lib/tips.ts` already teases HMM states.
//   cluster-cells     — "how do I find cell types without gating by hand".
//   run-a-chain       — "I have forty images, am I really doing this one at a time?" The answer that
//                       makes the difference between a demo and a working pipeline.
//
// Two of the four are one `moduleTaskGuide` call each, which is the point of that builder (plan D8).

import type { GuideDef } from './types'
import { PREREQ } from './prereqs'
import { moduleTaskGuide } from './moduleTask'

// ── Metadata: not a task runner (edits apply immediately), so it gets its own steps ──────────────
export const fixMetadataGuide: GuideDef = {
  id: 'fix-metadata',
  title: 'Fix pixel size and channel names',
  group: 'Data',
  icon: 'pi-tag',
  summary: 'Correct the voxel size, frame interval and channel names an import guessed wrong.',
  prereqs: [PREREQ.projectOpen, PREREQ.setHasImages],

  steps: [
    {
      anchor: 'nav:/metadata',
      placement: 'right',
      title: 'Metadata',
      text: 'Wrong pixel size silently corrupts every µm measurement downstream — fix it here first.',
      bullets: ['Cell diameters, areas, speeds and distances all depend on it.'],
      clickAnchor: true,
    },
    {
      anchor: 'images.qcDot',
      route: '/metadata',
      placement: 'left',
      title: 'Cecelia flags the suspects',
      text: 'A flag on a row means a metadata field looks implausible — hover to see which.',
      bullets: ['Missing Z spacing and a 1-pixel-per-µm default are the usual two.'],
    },
    {
      anchor: 'metadata.physEditor',
      route: '/metadata',
      placement: 'left',
      title: 'Voxel size and timing',
      text: 'Open the editor to set the real pixel size, Z spacing and frame interval.',
      bullets: [
        'Select several images to fix them in one pass.',
        'Values come from your acquisition software, not from guesswork.',
      ],
      reveal: {
        needed: c => c.rightPanelCollapsed,
        anchor: 'layout.rightPanelHandle',
        text: 'The metadata panel is folded away — open it with this handle.',
        placement: 'left',
      },
    },
    {
      anchor: 'metadata.channels',
      route: '/metadata',
      placement: 'left',
      title: 'Name the channels',
      text: 'Real names make every downstream picker readable — "CD4", not "mean_intensity_2".',
      bullets: [
        'One name per line, in channel order.',
        'Copy from a reference image to do a whole set at once.',
      ],
      reveal: {
        needed: c => c.rightPanelCollapsed,
        anchor: 'layout.rightPanelHandle',
        text: 'The metadata panel is folded away — open it with this handle.',
        placement: 'left',
      },
    },
    {
      anchor: 'metadata.attributes',
      route: '/metadata',
      placement: 'left',
      title: 'Add your experimental groups',
      text: 'Attributes are your own columns — treatment, genotype, mouse, timepoint.',
      bullets: [
        'They are what plots group and compare by later.',
        'Fill them from the filename with the pattern builder.',
      ],
      reveal: {
        needed: c => c.rightPanelCollapsed,
        anchor: 'layout.rightPanelHandle',
        text: 'The metadata panel is folded away — open it with this handle.',
        placement: 'left',
      },
    },
    {
      title: 'Do this before you segment',
      text: 'Metadata is read at run time, so fixing it later means re-running what you already ran.',
    },
  ],
}

// ── Behaviour states: a plain task run ───────────────────────────────────────────────────────────
export const behaviourStatesGuide = moduleTaskGuide({
  id: 'behaviour-states',
  title: 'Classify behaviour states',
  group: 'Explore',
  icon: 'pi-directions',
  summary: 'Fit an HMM to track movement so each cell gets an arrested / directed / meandering state.',
  route: '/behaviour',
  navLabel: 'Behaviour',
  taskKey: 'hmm',
  funName: 'behaviour.hmm',
  funLabel: 'HMM (states + transitions)',
  selectionModule: 'behaviourAnalysis',
  waitLabel: 'Fitting states',
  prereqs: [PREREQ.projectOpen, PREREQ.tracked],
  intro: 'A Gaussian HMM turns raw track movement into a small set of named behaviour states.',
  selectHint: ['Only tracked images qualify — the states are fitted to track measures.'],
  params: [
    'Number of states — 3 is the usual starting point (arrested / meandering / directed).',
    'Which track measures to fit on — speed and angle are the standard pair.',
  ],
  after: [
    {
      anchor: 'layout.plotsSection',
      route: '/behaviour',
      placement: 'top-start',
      title: 'Read the states off the plots',
      text: 'The panel shows what each fitted state actually looks like, so you can name them.',
      bullets: [
        'A state is only meaningful once you have seen its speed profile.',
        'Fit landed oddly? Change the number of states and re-run.',
      ],
    },
    {
      anchor: 'sidebar.viewerCta',
      placement: 'right',
      title: 'Colour tracks by state',
      text: 'The Viewer panel can colour tracks by the new state column in napari.',
      bullets: ['That is the quickest sanity check that the states mean something.'],
    },
  ],
})

// ── Cluster cells: a plain task run ──────────────────────────────────────────────────────────────
export const clusterCellsGuide = moduleTaskGuide({
  id: 'cluster-cells',
  title: 'Cluster cells into phenotypes',
  group: 'Populations',
  icon: 'pi-share-alt',
  summary: 'Group cells by their whole measure profile instead of gating two channels at a time.',
  route: '/clust-cells',
  navLabel: 'Cluster cells',
  taskKey: 'clusterCells',
  funName: 'clustPops.cluster',
  funLabel: 'Cluster cells',
  selectionModule: 'clustPops',
  waitLabel: 'Clustering',
  prereqs: [PREREQ.projectOpen, PREREQ.segmented],
  intro: 'Clustering finds cell types from all measures at once — the unsupervised counterpart to gating.',
  selectHint: ['Select every image you want clustered TOGETHER — the run pools across them.'],
  params: [
    'Which measures to cluster on — channel intensities, usually.',
    'Resolution — higher splits into more, smaller clusters.',
    'Population to cluster within, if you want to sub-phenotype a gate.',
  ],
  after: [
    {
      anchor: 'layout.plotsSection',
      route: '/clust-cells',
      placement: 'top-start',
      title: 'UMAP plus heatmap',
      text: 'The UMAP shows how clusters separate; the heatmap says what each one actually expresses.',
      bullets: [
        'The heatmap is what turns "cluster 4" into "CD4 T cell".',
        'Clusters can be collected into named populations, same as gates.',
      ],
    },
  ],
})

// ── Chains: the whiteboard. Its own shape — a DAG editor, not a function list ─────────────────────
export const runChainGuide: GuideDef = {
  id: 'run-a-chain',
  title: 'Run a pipeline over a whole set',
  group: 'Pipeline',
  icon: 'pi-sitemap',
  summary: 'Wire your steps into a chain once, then run the lot across every image in a set.',
  prereqs: [PREREQ.projectOpen, PREREQ.imageImported],

  steps: [
    {
      anchor: 'nav:/chain',
      placement: 'right',
      title: 'Whiteboard',
      text: 'A chain is your pipeline drawn as a graph — correct, segment, track, cluster, in order.',
      bullets: ['Build it once and every new image goes through the same steps.'],
      clickAnchor: true,
    },
    {
      anchor: 'chain.bar',
      route: '/chain',
      placement: 'right',
      title: 'Chains are named templates',
      text: 'Make one per pipeline you use — they are saved with the project.',
    },
    {
      anchor: 'chain.palette',
      route: '/chain',
      placement: 'right',
      title: 'Drag in the steps',
      text: 'Every function from every module page is here — drag one onto the canvas.',
      bullets: [
        'Connect nodes to set the order.',
        'A node only runs if it is reachable from the start node.',
      ],
    },
    {
      anchor: 'chain.runImages',
      route: '/chain',
      placement: 'right',
      title: 'Pick the images',
      text: 'Choose the set and tick the images to push through the chain.',
      bullets: ['Excluded images are greyed out and cannot be selected.'],
    },
    {
      anchor: 'chain.run',
      route: '/chain',
      placement: 'right',
      text: 'Run it — each image walks the whole graph, and the pools cap what runs at once.',
      clickAnchor: true,
    },
    {
      anchor: 'chain.tabs',
      route: '/chain',
      placement: 'bottom-start',
      title: 'Watch it live',
      text: 'The Live tab shows the graph filling in per image, with QC as it lands.',
      bullets: [
        'A failed node stops that image, not the run.',
        'You can resume a run from a chosen node rather than starting over.',
      ],
    },
    {
      title: 'This is how you scale',
      text: 'Everything the module pages do one image at a time, a chain does for a cohort unattended.',
    },
  ],
}
