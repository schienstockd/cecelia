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
//   use-the-blackboard  — Analysis group. Shared markdown surface where conclusions land durably —
//                         list / new / edit + version / outcome tag / send to Kiwi. Captures arrive
//                         INBOUND from Kiwi (no attach-a-capture button), so the guide teaches the
//                         outbound counterpart (addToKiwi) instead.
//   assist-with-kiwi    — Analysis group. Walks the four Kiwi cockpit rows (help / pairing / chat /
//                         share) plus the lab-log sibling that most users find first. Replaced the
//                         earlier `lab-log-and-claude` two-step pointer in 2026-09 — see D13.

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

// ── Blackboard — the shared markdown surface for a project ───────────────────────────────────────
// Captures arrive INBOUND from Kiwi (no attach-a-capture button lives on the pane); the outbound
// counterpart is AddToKiwiButton on the entry pane head, so the guide teaches the outbound flow
// instead. Route /blackboard is confirmed in main.ts; prereq is projectOpen (bb-list is per project).
export const useTheBlackboardGuide: GuideDef = {
  id: 'use-the-blackboard',
  title: 'Use the blackboard',
  group: 'Analysis',
  icon: 'pi-comment',
  summary: 'Shared markdown between you and the assist — where a conclusion becomes durable, not chat scrollback.',
  prereqs: [PREREQ.projectOpen],

  steps: [
    {
      anchor: 'nav:/blackboard',
      placement: 'right',
      title: 'Blackboard',
      text: 'One markdown surface per project — every Kiwi turn about this project lands here too.',
      bullets: ['Reach it from the sidebar; the list opens straight away.'],
      clickAnchor: true,
    },
    {
      anchor: 'blackboard.new',
      route: '/blackboard',
      placement: 'bottom',
      title: 'Start an entry',
      text: 'One entry per thought you are chasing — captures land under it as they arrive from the viewer or Kiwi.',
      bullets: ['Entries are per project — open a project first.'],
    },
    {
      anchor: 'blackboard.list',
      route: '/blackboard',
      placement: 'right',
      title: 'The entry list',
      text: 'Sort by any column; the two chip rows above filter by status and outcome.',
      bullets: ['The project profile row stays pinned to the top.'],
    },
    {
      anchor: 'blackboard.edit',
      route: '/blackboard',
      placement: 'bottom',
      title: 'Edit and version',
      text: 'Edit takes a snapshot before saving — every revision is kept and previewable from the version dropdown.',
      bullets: ['Restore v3 with a click if v4 went sideways.'],
      reveal: {
        needed: c => !c.anchorExists('blackboard.edit'),
        anchor: 'blackboard.list',
        text: 'Pick an entry from the list — the edit button lives on its pane head.',
        placement: 'right',
      },
    },
    {
      anchor: 'blackboard.outcome',
      route: '/blackboard',
      placement: 'bottom',
      title: 'Tag outcome',
      text: 'Good or Bad plus a note when the thread is settled — what carries into the next project.',
      bullets: ['The note is required; it is what a future session actually needs to know.'],
    },
    {
      anchor: 'blackboard.addToKiwi',
      route: '/blackboard',
      placement: 'left',
      title: 'Send to Kiwi',
      text: 'Feed this entry (or a specific version) into a Kiwi turn — appears as context alongside your prompt.',
      bullets: ['Ships the version you are viewing.'],
    },
  ],
}

// ── Kiwi — the assist cockpit. Replaces the earlier `lab-log-and-claude` two-step pointer ────────
// The `?` moved from the lab-log toolbar to Kiwi on 2026-09-22, and Kiwi grew into the canonical
// cockpit for pairing / chat handoff / capture sharing. The old two-step guide undersold that; this
// one walks the four rows (help, pairing, chat, share) and keeps the lab-log step at the front
// because that's the sibling surface most users find first. See GUIDE_ADDITIONS_PLAN D13.
//
// Pairing / chat / share rows are `v-if`'d on `projectUid` (KiwiCockpit.vue:276-279 — no project =
// no pairing target), so the prereq is `projectOpen`. Kiwi itself is `v-if`'d in App.vue on
// `settings.kiwiOpen`, so anchor-not-found reveals point at `sidebar.kiwiCta`.
export const kiwiGuide: GuideDef = {
  id: 'assist-with-kiwi',
  title: 'Assist with Kiwi',
  group: 'Analysis',
  icon: 'pi-comments',
  summary: 'The assistant cockpit — pairing status, chat handoff, and sharing what is on screen.',
  prereqs: [PREREQ.projectOpen],

  steps: [
    {
      anchor: 'sidebar.labLogCta',
      placement: 'right',
      title: 'The lab log',
      text: 'A per-project, append-only record — Cecelia adds a digest when a run finishes, you and Kiwi append notes.',
      bullets: ['Floating panel, reachable from any page.'],
    },
    {
      anchor: 'sidebar.kiwiCta',
      placement: 'right',
      title: 'Kiwi is the assist cockpit',
      text: 'Everything Claude-adjacent lives in this panel — pairing, chat starter, and the buttons that share what is on screen.',
      bullets: ['On-demand only; nothing happens until you ask.'],
      clickAnchor: true,
    },
    {
      anchor: 'kiwi.pairing',
      placement: 'left',
      title: 'Pairing status',
      text: 'The chip shows whether a claude-code session on this machine is paired — the refresh button probes the socket.',
      bullets: ['Unpaired Kiwi still holds state; shared frames just fall back to the clipboard.'],
      reveal: {
        needed: c => !c.anchorExists('kiwi.pairing'),
        anchor: 'sidebar.kiwiCta',
        text: 'Open Kiwi first — the pairing row lives inside the panel.',
        placement: 'right',
      },
    },
    {
      anchor: 'kiwi.chat',
      placement: 'left',
      title: 'Copy a chat starter',
      text: 'Copies a one-line opener naming this project — paste it into the paired session to start a Kiwi conversation about it.',
    },
    {
      anchor: 'kiwi.share',
      placement: 'left',
      title: 'Ship what you are looking at',
      text: 'Two targets: the viewer frame or a canvas plot — both land in the same capture envelope.',
      bullets: ['Falls back to the clipboard if nothing is paired.'],
    },
    {
      anchor: 'kiwi.assistantHelp',
      placement: 'bottom-start',
      title: 'What Kiwi can see and do',
      text: 'This ? explains Ask vs Chat, captures, chains, blackboard, limits — read once.',
      bullets: ['Also reports whether the assist is actually set up.'],
      clickAnchor: true,
      reveal: {
        needed: c => !c.anchorExists('kiwi.assistantHelp'),
        anchor: 'sidebar.kiwiCta',
        text: 'Open Kiwi first — the ? sits in its header.',
        placement: 'right',
      },
    },
  ],
}
