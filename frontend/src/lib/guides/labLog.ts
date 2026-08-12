// "Lab log and Claude" — deliberately the SHORTEST guide in the catalogue. Its job is discovery, not
// instruction: two surfaces exist that nobody finds on their own, and the Claude one already has a
// perfectly good in-app explainer behind its `?` (ClaudeOverviewDialog, content in lib/claudeOverview.ts).
// Duplicating that here would be a second copy to keep in step, so this guide points at it and stops
// (Dominik, 2026-08-12: "just to say it's there, click the (?) to see what it can do").
//
// Claude is on-demand only and may not be installed at all, so nothing here promises it works — the
// `?` dialog is also where the setup state is reported.

import type { GuideDef } from './types'
import { PREREQ } from './prereqs'

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
      anchor: 'lablog.claudeHelp',
      placement: 'bottom-start',
      title: 'Claude, if you have it',
      text: 'This ? explains what Claude can see, suggest and create here — and whether it is set up.',
      bullets: [
        'On-demand only: it does nothing until you ask.',
        'Worth one read; nothing else in this guide.',
      ],
      clickAnchor: true,
      reveal: {
        // The panel is `v-if`'d in App.vue, so the toolbar does not exist until it is open.
        needed: c => !c.anchorExists('lablog.claudeHelp'),
        anchor: 'sidebar.labLogCta',
        text: 'Open the lab log first — the ? lives in its toolbar.',
        placement: 'right',
      },
    },
  ],
}
