// "Check an acquisition" — the set-level Acquisition check (AcqCheckDialog, rules in
// app/src/acq_check.jl, plan in docs/todo/ACQUISITION_TEST_CHECK_PLAN.md). Its own file because the
// shape is a modal walk, not a task run: open the check from the Checks cluster, state the plan, then
// read the set's findings, the verdict, the per-image table and (AI on) the review.
//
// The question it answers is the one a trainee brings back from the scope: "is this test acquisition
// good enough to image the rest, and if not, what do I change?" With the stage set to Final data the
// same findings are worded for the analysis instead (`verdictHeading`, `findingLines`).

import type { GuideDef } from './types'
import { PREREQ } from './prereqs'
import { GUIDE_MATURITY } from './maturityRecords'

const ROUTE = '/manage-images'
const checkOpen = (c: { anchorExists: (id: string) => boolean }) => c.anchorExists('acqCheck.plan')

export const acquisitionCheckGuide: GuideDef = {
  id: 'acquisition-check',
  maturity: GUIDE_MATURITY['acquisition-check'],
  title: 'Check an acquisition',
  group: 'Data',
  icon: 'pi-sliders-h',
  summary: 'Before imaging the rest: what to change at the scope, or what to watch in the analysis.',
  prereqs: [PREREQ.projectOpen, PREREQ.imageImported],

  steps: [
    {
      anchor: 'nav:/manage-images',
      placement: 'right',
      title: 'Manage images',
      text: 'The set’s checks sit above its image table.',
      when: c => c.route === ROUTE,
    },
    {
      anchor: 'checks.acquisition',
      route: ROUTE,
      placement: 'bottom-start',
      title: 'The Acquisition check',
      text: 'It reads the settings and signal of every image in the set and says what is worth a look.',
      bullets: ['The number on it is how many things to look at.', 'Open it.'],
      when: checkOpen,
    },
    {
      anchor: 'acqCheck.plan',
      route: ROUTE,
      placement: 'bottom',
      title: 'Say what you plan to do',
      text: 'Name the cells you will segment or track, the channel each is in, and what you will measure.',
      bullets: [
        'Without a plan only the set-wide checks run: photons, balance and shared signal need one.',
        'Test acquisition: what to change at the scope. Final data: what to watch in the analysis.',
      ],
    },
    {
      anchor: 'acqCheck.setFindings',
      route: ROUTE,
      placement: 'left',
      title: 'Across the set',
      text: 'What is off across the set, e.g. settings that differ between images or a channel too dim to measure.',
      bullets: [
        'Open a finding for the images it concerns and what to do next.',
        'Not measured yet? Measure runs in the background, about a minute per image.',
      ],
    },
    {
      anchor: 'acqCheck.verdict',
      route: ROUTE,
      placement: 'left',
      title: 'The verdict',
      text: 'The rules’ answer, in one paragraph: image the rest as they are, or change something first.',
      reveal: {
        needed: c => checkOpen(c) && !c.anchorExists('acqCheck.verdict'),
        anchor: 'acqCheck.plan',
        text: 'The verdict needs a plan — set one and press Check.',
        placement: 'bottom',
      },
    },
    {
      anchor: 'acqCheck.perImage',
      route: ROUTE,
      placement: 'left',
      title: 'Per image',
      text: 'One row per image, grouped by label design, with the numbers the findings rest on.',
      bullets: ['Click a row for that image’s findings.', 'Click its name to open its metadata.'],
    },
    {
      anchor: 'acqCheck.review',
      route: ROUTE,
      placement: 'left',
      title: 'Ask for a review',
      text: 'Kiwi’s model reads your plan, the images and acquisition knowledge, and adds one next step.',
      bullets: ['Mark each point Good, Bad or Unsure on the Blackboard.'],
      onlyIf: c => c.aiEnabled,
    },
  ],
}
