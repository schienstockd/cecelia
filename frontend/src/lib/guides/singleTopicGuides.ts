// The single-topic bespoke guides — each answers a question users actually arrive with, and each has a
// shape different enough that the `moduleTaskGuide` builder does not fit:
//
//   use-a-user-profile  — Start group. Who is driving this box, and where switching / creating /
//                         logging-in a profile lives. Targets the header chip → Preferences (NOT the
//                         launch picker, which is a boot interstitial the router bounces away from
//                         mid-session — see GUIDE_ADDITIONS_PLAN D14).
//   use-a-view-profile  — Start group. The sidebar-curation surface in Settings — every step still
//                         works on a single-profile install.
//   fix-metadata        — Data group. "The scale bar is wrong / my channels are called Channel 1".
//                         THE most common first failure, and the import guide already points at it.
//                         Not a task runner (edits apply immediately) so it gets its own steps.
//   run-a-chain         — Pipeline group. "I have forty images, am I really doing this one at a
//                         time?" The whiteboard's a DAG editor, not a function list.
//   lab-log-and-claude  — Analysis group. Deliberately the SHORTEST guide in the catalogue — its job
//                         is discovery: two surfaces exist that nobody finds on their own. The
//                         Claude one has its own `?` explainer (ClaudeOverviewDialog) so this guide
//                         points at it and stops.

import type { GuideDef } from './types'
import { PREREQ } from './prereqs'

// ── User profile: the header chip is the mid-session entry point ─────────────────────────────────
// The launch picker (AppProfilePicker.vue) is a bare-route boot interstitial — the router bounces
// there when appCtl.needsProfilePick is true and away when it's false. A guide's "click here" step
// on it cannot land mid-session, so this walks the always-reachable surfaces instead: the header
// chip and the Profiles pane it opens.
export const userProfileGuide: GuideDef = {
  id: 'use-a-user-profile',
  title: 'Use a user profile',
  group: 'Start',
  icon: 'pi-user',
  summary: 'Who is driving this box — and where switching, creating or logging in a profile lives.',
  prereqs: [],

  steps: [
    {
      anchor: 'header.profileChip',
      placement: 'bottom',
      title: 'Who is driving',
      text: 'The chip in the header names the active profile — click it to open Preferences → Profiles.',
      bullets: ['One profile per person, not per experiment.'],
      clickAnchor: true,
    },
    {
      anchor: 'prefs.profilesPane',
      placement: 'right',
      title: 'The Profiles pane',
      text: 'Every profile has its own Kiwi credentials, preferences and project list — switching one over reloads the app.',
      bullets: ['No cross-profile sharing by default.'],
    },
    {
      anchor: 'prefs.newProfile',
      placement: 'top',
      title: 'Add a profile',
      text: 'New profile = fresh credentials and an empty project list.',
      bullets: ['Retire one from the row above; the record stays resolvable for old turn logs.'],
    },
    {
      anchor: 'prefs.copyLogin',
      placement: 'top',
      title: 'Log the profile in',
      text: 'Copies a terminal one-liner scoped to this profile\'s CLAUDE_CONFIG_DIR — paste and run /login.',
      bullets: ['Each profile logs Claude in separately.'],
    },
  ],
}

// ── View profile: the sidebar curator, lives in Settings ─────────────────────────────────────────
// Different from a user profile — a view profile only decides which sidebar entries this user sees;
// every hidden page is still reachable by URL. Settings → View profile is the only entry point.
export const viewProfileGuide: GuideDef = {
  id: 'use-a-view-profile',
  title: 'Use a view profile',
  group: 'Start',
  icon: 'pi-eye',
  summary: 'Curate the sidebar down to the pages you actually work on — declutter, not access control.',
  prereqs: [],

  steps: [
    {
      anchor: 'nav:/settings',
      placement: 'right',
      title: 'Settings',
      text: 'View profiles live in Settings — they curate the sidebar down to the pages this user actually works on.',
      bullets: ['This is decluttering, not access control — every page still opens by URL.'],
      clickAnchor: true,
    },
    {
      anchor: 'settings.viewProfile',
      route: '/settings',
      placement: 'left',
      title: 'Pick or edit one',
      text: 'The chip row picks which profile is active; Edit opens the builder.',
      bullets: ['"All pages" is the default — you never lose access to a page by picking a profile.'],
    },
    {
      anchor: 'viewProfile.editor',
      placement: 'bottom',
      title: 'Tick pages into a profile',
      text: 'One row per sidebar group — tick the pages this profile should surface.',
      bullets: ['Great for handing a colleague a "just the segmentation bits" cecelia.'],
      reveal: {
        // The editor only exists once the user clicks Edit in the previous step.
        needed: c => !c.anchorExists('viewProfile.editor'),
        anchor: 'settings.viewProfile',
        text: 'Click Edit above to open the view-profile builder.',
        placement: 'left',
      },
    },
  ],
}

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
