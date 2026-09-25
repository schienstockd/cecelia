// The single-topic bespoke guides — each answers a question users actually arrive with, and each has a
// shape different enough that the `moduleTaskGuide` builder does not fit:
//
//   fix-metadata        — "the scale bar is wrong / my channels are called Channel 1". THE most common
//                         first failure, and the import guide already points at it. Not a task runner
//                         (edits apply immediately) so it gets its own steps.
//   run-a-chain         — "I have forty images, am I really doing this one at a time?" The whiteboard's
//                         a DAG editor, not a function list.
//   lab-log-and-claude  — deliberately the SHORTEST guide in the catalogue. Its job is discovery: two
//                         surfaces exist that nobody finds on their own. The Claude one already has an
//                         in-app explainer behind its `?` (ClaudeOverviewDialog, content in
//                         lib/claudeOverview.ts) — duplicating that here would be a second copy to keep
//                         in step, so this guide points at it and stops. Claude is on-demand only and
//                         may not be installed at all, so nothing here promises it works; the `?`
//                         dialog is also where the setup state is reported.

import type { GuideDef } from './types'
import { PREREQ } from './prereqs'

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
        'A failure skips only what depends on it — other images and parallel branches carry on.',
        'You can resume a run from a chosen node rather than starting over.',
      ],
    },
    {
      title: 'This is how you scale',
      text: 'Everything the module pages do one image at a time, a chain does for a cohort unattended.',
    },
  ],
}

// ── Lab log + Claude: pointer-only, deliberately short ───────────────────────────────────────────
// The `?` moved from the lab-log toolbar to Kiwi on 2026-09-22 — Kiwi is the canonical cockpit for
// assistant controls, so the how-to lives beside pairing / chat handoff / observer state.
export const labLogGuide: GuideDef = {
  id: 'lab-log-and-claude',
  title: 'Lab log and Claude',
  group: 'Analysis',
  icon: 'pi-book',
  summary: 'Where your analysis notes live, and how to find out what the AI assist can do.',
  prereqs: [PREREQ.projectOpen],

  steps: [
    {
      anchor: 'sidebar.labLogCta',
      placement: 'right',
      title: 'The lab log',
      text: 'A per-project, append-only record of what was done and what you concluded.',
      bullets: [
        'Cecelia adds a daily digest of the runs that finished.',
        'Reachable from any page — it is a floating panel, not a page.',
      ],
      clickAnchor: true,
    },
    {
      anchor: 'kiwi.assistantHelp',
      placement: 'bottom-start',
      title: 'Claude, if you have it',
      text: 'This ? explains what Claude can see, suggest and create here — and whether it is set up.',
      bullets: [
        'On-demand only: it does nothing until you ask.',
        'Worth one read; nothing else in this guide.',
      ],
      clickAnchor: true,
      reveal: {
        // Kiwi is `v-if`'d in App.vue, so its rows do not exist until the panel is open.
        needed: c => !c.anchorExists('kiwi.assistantHelp'),
        anchor: 'sidebar.kiwiCta',
        text: 'Open Kiwi first — the ? lives at the top of it.',
        placement: 'right',
      },
    },
  ],
}
