// C1 — Using AI in Cecelia. The one-pager that answers "how do I use AI here?".
// Every line the app itself shows is IMPORTED from the app's source, so the booklet and the screen
// cannot drift: the two mode cards and the Kiwi loop (lib/claudeOverview.ts), the opt-in dialog's
// "What is sent" rows (lib/claudeTools.ts → AI_SURFACES) and the off/on copy (utils/aiSwitch.ts).
// The labels typed here (Preferences → Kiwi → AI features, the Account row, Attached) were traced in
// PreferencesModal.vue, ProfileMenu.vue, KiwiCockpit.vue and kiwi/KiwiAsk.vue.

import type { Booklet } from './spec.ts'
import { CLAUDE_BRIDGE, CLAUDE_DATA_NOTICE, CLAUDE_PROVISIONAL, KIWI_LOOP, KIWI_MODE, TERMINAL_MODE } from '../../frontend/src/lib/claudeOverview.ts'
import { aiDialogRows } from '../../frontend/src/lib/claudeTools.ts'
import { AI_OFF_COPY, AI_OPT_IN_COPY } from '../../frontend/src/utils/aiSwitch.ts'

const card = (m: typeof KIWI_MODE, tone: 'kiwi' | 'claude') =>
  ({ title: m.title, tone, lead: m.use, lines: m.only, foot: `${m.start}. ${m.note ?? ''}`.trim() })

export const C1_USING_AI: Booklet = {
  id: 'c1-using-ai',
  series: 'C',
  title: 'Using AI in Cecelia',
  subtitle: 'Optional, off until you turn it on, and the analysis stays yours',
  audience: 'Anyone using Cecelia. No AI experience needed.',
  get: 'Which of the two AI tools to open, how to turn AI on, and what leaves your computer when you do.',
  need: ['A Claude account; each Cecelia profile logs in once'],
  endsAt: 'AI turned on, or deliberately left off, and your first Kiwi answer with its refs checked.',
  intro: [
    { kind: 'note', tone: 'warn', text: `Everything in Cecelia works with AI off. ${CLAUDE_DATA_NOTICE.short}, so turning AI on is up to you, your lab and your data policy. ${CLAUDE_DATA_NOTICE.detail}.` },
    { kind: 'cards', cards: [card(KIWI_MODE, 'kiwi'), card(TERMINAL_MODE, 'claude')] },
    { kind: 'terms', terms: [
      { term: 'Claim', means: 'One fact in a Kiwi reply, on its own row.' },
      { term: 'Ref', means: 'The clickable link on a claim to what it is about: an image, a population, a plot, a cell.' },
      { term: 'Blackboard', means: 'The project’s shared notes page. Saved Kiwi replies, notes and shared frames live there.' },
      { term: 'Chain', means: 'Steps you settled on, wired together to run over a whole set of images.' },
    ] },
    // What "checked" means, said once. kiwi_validate_reply (api/src/kiwi_turn.jl): every ref must exist
    // AND have been read by Kiwi this turn; nothing checks that the data SUPPORTS the claim
    // (KiwiRefChip.vue header), and nothing checks the biology (CLAUDE_PROVISIONAL).
    { kind: 'note', tone: 'info', text: `“Checked” means Kiwi made sure every claim’s ref exists and that it actually read it. It does not check that the data supports the claim, or the biology: that part is yours. ${CLAUDE_PROVISIONAL}.` },
    { kind: 'para', text: `${CLAUDE_BRIDGE}.` },
    { kind: 'table', head: ['When you use', 'What is sent to Anthropic'], rows: aiDialogRows().filter(r => r.sends).map(r => [r.label, r.sends]) },
  ],
  steps: [
    {
      title: 'Turn AI on',
      shot: 'c1-turn-on',
      do: 'Open your avatar menu → **Preferences** → **Kiwi**, then switch on **AI features**.',
      see: `The dialog **${AI_OPT_IN_COPY.title}** lists what is sent and what you gain. Read it, then press **${AI_OPT_IN_COPY.confirm}**. ${AI_OPT_IN_COPY.after}.`,
      ifWrong: `The switch is greyed out and says "${AI_OFF_COPY.workstation}": whoever looks after this computer turned AI off for everyone. Ask them; it is their call, not a fault.`,
    },
    {
      title: 'Log in to Claude',
      do: 'Open **Kiwi** in the sidebar. On the **Account** row, press **Log in**.',
      see: 'A Claude sign-in page opens. Once you sign in, the Account row shows your account instead of **Not logged in**.',
      ifWrong: 'If the page does not hand you back to Cecelia, copy the code it shows into **Paste the code from the sign-in page**.',
    },
    {
      title: KIWI_LOOP[0].title,
      do: 'Press **@ Add to Kiwi** on what puzzles you: an image row, a population or a plot.',
      see: 'It is listed under **Attached**, above the box Kiwi reads your question from.',
      ifWrong: 'No **@ Add to Kiwi** buttons anywhere: AI is still off for this profile (step 1).',
      more: [{ kind: 'note', tone: 'info', text: 'To ask about something in the viewer instead, use the **Share** row in Kiwi: draw on the pop-out viewer and share the frame.' }],
    },
    {
      title: KIWI_LOOP[1].title,
      do: 'Type your question in **Ask** and press Ctrl+Enter.',
      see: 'A table of claims, one fact per row, each with its refs under **Points at**. Click a claim and Cecelia shows you what it points at.',
      ifWrong: 'Kiwi answers **Can’t say from what I looked at.**: attach what the question is about and ask again. A claim number in red cites something Kiwi didn’t read; hover it for why, and don’t rely on that claim.',
    },
    {
      title: KIWI_LOOP[2].title,
      optional: true,
      do: 'Press **Save** under a reply. On the Blackboard, press **Tag outcome**, write why, then **Good** or **Bad**.',
      see: 'The entry with your tag and note. Entries say **by Kiwi** or **by Claude**; gates Claude proposed carry ✦ **Proposed by Claude**.',
      why: 'Replies expire after 50; the Blackboard keeps them. Bad tags are how Claude warns you when a failure comes back.',
      ifWrong: '**Good** and **Bad** stay greyed out until you write the note: the reason is required.',
    },
  ],
  askAi: null,
  shots: [
    {
      // The dialog, not the switch: what the reader must read before saying yes is the point. Cropped
      // from "Keep in mind" down — what is sent is already the table on page 1.
      id: 'c1-turn-on',
      actions: [{ click: 'header.profile' }, { click: 'header.preferences' }, { click: 'prefs.kiwi' },
                { click: 'prefs.aiFeatures' }, { waitFor: 'aiOptIn.turnOn' }],
      point: { anchor: 'aiOptIn.turnOn', label: 'Read it, then Turn on' },
      crop: ['aiOptIn.risks', 'aiOptIn.turnOn'],
      expect: [{ anchor: 'aiOptIn.turnOn', text: AI_OPT_IN_COPY.confirm }],
    },
  ],
}
