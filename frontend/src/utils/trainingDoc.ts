// user-docs/training.md — a trainer's run-sheet for the facility's training sessions (session 2 at
// the scope, session 3 analysing the trainee's own images in Cecelia), for microscopy staff who may
// not have used Cecelia. The trainee follows the in-app guides; this page adds what an expert would
// say in the room: what to check on the trainee's images, what good looks like, when to stop.
// Generated, so it cannot drift from the app: guide titles, maturity words, the acquisition check's
// stage and verdict labels and the AI notice are IMPORTED from the app's source. The rest was traced
// in the guides (lib/guides/*), AcqCheckDialog.vue and ClaimStrip.vue.
// `utils/trainingDoc.test.ts` fails when the committed page is stale. Its screenshots are the shot set
// user-docs/shots/training.ts.

import { guideById } from '../lib/guides'
import { TIER_LABEL, TIER_TIP } from '../lib/guides/tiers'
import { CLAUDE_DATA_NOTICE } from '../lib/claudeOverview'
import { ACQ_STAGES, verdictHeading } from './acqCheck'
import { TRAINING_SHOTS } from '../../../user-docs/shots/training.ts'
import { docHeader, docImage, docSteps, generatedBy, shotOf, type DocStep } from './docPage'

/** A guide's title as the picker shows it, in bold; throws on an id that is not a guide. */
function guide(id: string): string {
  const g = guideById(id)
  if (!g) throw new Error(`training page names guide ${id}, which does not exist`)
  return `guide **${g.title}**`
}
/** The maturity badge of a guide's feature, as the guide picker shows it. */
function badge(id: string): string {
  return TIER_LABEL[guideById(id)!.maturity.tier]
}
const stage = (v: 'test' | 'final') => ACQ_STAGES.find(s => s.value === v)!.label

/** A blank the facility fills in; a page with one is a draft. */
const FACILITY = (what: string) => `_[${what}: to be filled in by the facility]_`

export const SESSION_3_STEPS: DocStep[] = [
  {
    title: 'Check the data',
    badge: badge('fix-metadata'),
    shot: 'tr-checks',
    do: `Open **Manage images**. Compare each image’s **Ch**, **Z** and **Scale** with the scope software. Fix anything wrong with the ${guide('fix-metadata')}.`,
    see: 'One row per image. Every image of the set has the same pixel size and the same channel names, matching the scope.',
    ifWrong: 'A flag on a row means a field looks implausible: hover it. Fix metadata _before_ segmenting: segmentation reads it when it runs.',
  },
  {
    title: 'Check the acquisition',
    badge: badge('acquisition-check'),
    shot: 'tr-acq-check',
    do: `Follow the ${guide('acquisition-check')}. Set the plan, with **${stage('final')}** picked.`,
    see: `The verdict under **${verdictHeading('final')}**. The trainee can say what it means for their analysis.`,
    ifWrong: '“Not measured yet”: press **Measure**. It runs in the background, about a minute per image.',
  },
  {
    title: 'Segment',
    badge: badge('segment-an-image'),
    shot: 'tr-mask',
    do: `Follow the ${guide('segment-an-image')}. Preview on a small region first, then run it and open the image in the viewer with the mask on.`,
    see: 'One outline per cell, on the cell’s edge, with clumps split into single cells.',
    ifWrong: 'An odd size distribution in the QC plots often means the cell diameter was off. Preview again with another diameter: seconds, not minutes.',
    note: `Have the trainee look at the mask _on the image_, not just at the QC plots. A few wrong cells: note them in the log below. Correcting masks by hand is ${badge('correct-a-mask')}; leave it out of training.`,
  },
  {
    title: 'Gate populations',
    badge: badge('gate-populations'),
    shot: 'tr-gate',
    do: `Follow the ${guide('gate-populations')} on one image, then copy the gates to the rest of the set.`,
    see: 'A named population per cell type, each with its cell count and its share of its parent.',
    ifWrong: 'Open the population in the viewer: do the gated cells sit where that cell type should be?',
  },
  {
    title: 'Plot a measure',
    badge: badge('build-plots'),
    do: `Follow the ${guide('build-plots')}. Plot one measure of the gated populations, e.g. each population’s share of its parent.`,
    see: 'One value per image and population. With a set, group it by the experimental attribute (treatment, mouse).',
    ifWrong: 'No groups to compare by: the attributes are set on the **Metadata** page (step 1).',
  },
  {
    title: 'Check the claim (needs a set)',
    do: 'Show where it lives. With a set, a summary plot’s footer has a ✓ button: it takes the result as one sentence and the replicate (e.g. Mouse).',
    see: 'The checks, without AI: are the images there, was everything processed the same way, how many replicates, are the units calibrated. Each line is a fact, or a problem with what to do.',
    ifWrong: 'No ✓ button: the plot is not across a set. The check needs a set of processed images, a few per condition.',
    note: 'Trainees usually bring one or two images, which is enough for steps 1–5 but not for a claim. Say so: this is how they check a result once they have a set with conditions, all processed the same way. Nothing is blocked: the checks inform, they don’t decide.',
  },
]

const tier = (t: 'experimental' | 'beta' | 'tested') => `| ${TIER_LABEL[t]} | ${TIER_TIP[t]}. |`

export function renderTraining(widths: Record<string, number>): string {
  const img = (id: string, alt: string) => docImage(TRAINING_SHOTS, shotOf(TRAINING_SHOTS, id), alt, widths, { base: 'images/' })
  return [
    generatedBy('trainingDoc.ts', 'trainingDoc.test.ts'),
    '',
    ...docHeader({
      title: 'Training with Cecelia: a trainer’s run-sheet',
      tagline: 'Draft. The trainee follows the in-app guides; this sheet is what an expert would add in the room.',
      for: 'microscopy staff who train facility users. No Cecelia experience needed.',
      get: 'a path for sessions 2 and 3: what to open, what to check on the trainee’s own images, and when to stop.',
      need: 'a Cecelia workstation, and the trainee’s session-2 images imported the day before session 3.',
      ends: 'the trainee imports, checks, segments, gates and plots a measure on their own, with the guides, and knows how to check a claim once they have a set.',
    }),
    '## Before you start',
    '',
    'Every step below names an in-app guide. Guides are under **Help → Guides**, top right. Each one puts a bubble beside the real button, on the trainee’s own data. This sheet names the guide; it does not repeat its clicks.',
    '',
    img('tr-guides', 'Help menu'),
    '',
    '**Maturity badges.** Most of Cecelia is labelled Experimental or Beta. Don’t hide this from trainees; say what it means:',
    '',
    '| Badge | What it means |',
    '|---|---|',
    tier('experimental'),
    tier('beta'),
    tier('tested'),
    '',
    `**AI is off by default** and nothing here needs it. ${CLAUDE_DATA_NOTICE.short}, so turning it on is the trainee’s and their lab’s decision.`,
    '',
    `**When to stop.** A result looks wrong and the step’s “If it goes wrong” doesn’t fix it: stop, don’t work around it. Note the image, the step and what you saw, and send it to ${FACILITY('contact')}.`,
    '',
    '## Session 2: at the scope',
    '',
    `Cecelia is not on the scope PC, so the loop is: acquire a test, check it at a Cecelia workstation, go back and change what the check names.`,
    '',
    '| Step | Trainee does | You check |',
    '|---|---|---|',
    '| 1. Acquire a test | One or two images of a typical sample, with the settings they plan to use. | Saved in the scope’s own format (`.oir`, `.czi`), not exported TIFFs: the settings live in the file. |',
    `| 2. Import | ${guide('import-images')}. | The image opens in the viewer with the right number of channels. |`,
    `| 3. Fix metadata | ${guide('fix-metadata')}. | Pixel size and Z step match the scope software; channels have real names. |`,
    `| 4. Check it | ${guide('acquisition-check')}, with **${stage('test')}** picked. | The trainee can say what the verdict under **${verdictHeading('test')}** asks them to change, and why. |`,
    '| 5. Back to the scope | Change what the check named, then acquire the rest. | — |',
    '',
    'One or two images are enough for this session and for session 3. Comparing conditions and checking a claim (session 3, step 6) needs more: a set with a few images per condition, acquired with the same settings.',
    '',
    '**Settings to take back.** Click the image’s name in the Acquisition check to open its metadata, then **Acquisition → Settings card**: a printable card of that image’s settings. It shows only when Cecelia can tell from the file which kind of microscope took it.',
    '',
    '## Session 3: analysis',
    '',
    '**The day before.** Import the trainee’s session-2 images into a project for them and open one in the viewer. If import fails, you find out now, not in front of the trainee. Keep a second project with images that worked, as a fallback.',
    '',
    ...docSteps(SESSION_3_STEPS, TRAINING_SHOTS, widths),
    '## What the trainee takes home',
    '',
    `- Their project, and where it lives: ${FACILITY('data path')}.`,
    `- **Help → Guides**, which opens on their next step. To repeat the analysis on more images: ${guide('run-a-chain')}.`,
    `- Who to ask: ${FACILITY('contact')}.`,
    '',
    '## After each session',
    '',
    'Note where trainees got stuck. The “If it goes wrong” lines above come from that list; they are only as good as the cohorts behind them.',
    '',
    '| Date | Session | Step | What happened | What fixed it |',
    '|---|---|---|---|---|',
    '| | | | | |',
    '',
  ].join('\n')
}
