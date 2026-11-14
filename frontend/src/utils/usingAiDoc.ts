// user-docs/using-ai.md — "how do I use AI here?", for anyone using Cecelia and for whoever it is
// forwarded to (a PI, ethics, IT). Generated, so it cannot drift from the app: every line the app itself
// shows is IMPORTED from the app's source — the two mode cards and the Kiwi loop (lib/claudeOverview.ts),
// the opt-in dialog's "What is sent" rows (lib/claudeTools.ts → AI_SURFACES) and the off/on copy
// (utils/aiSwitch.ts). The labels typed here (Preferences → Kiwi → AI features, the Account row,
// Attached) were traced in PreferencesModal.vue, ProfileMenu.vue, KiwiCockpit.vue and kiwi/KiwiAsk.vue.
// `utils/usingAiDoc.test.ts` fails when the committed page is stale. Its screenshots are the shot set
// user-docs/shots/using-ai.ts (captured and composed by scripts/docshots/).

import { CLAUDE_BRIDGE, CLAUDE_DATA_NOTICE, CLAUDE_PROVISIONAL, KIWI_LOOP, KIWI_MODE, TERMINAL_MODE } from '../lib/claudeOverview'
import { aiDialogRows } from '../lib/claudeTools'
import { AI_OFF_COPY, AI_OPT_IN_COPY } from './aiSwitch'
import { USING_AI_SHOTS } from '../../../user-docs/shots/using-ai.ts'
import { imageFile } from '../../../user-docs/shots/spec.ts'

/** One step: Do → You'll see → If it goes wrong, plus Why for what isn't obvious. */
interface DocStep { title: string; do: string; see: string; ifWrong: string; why?: string; optional?: boolean; shot: string; note?: string }

export const USING_AI_STEPS: DocStep[] = [
  {
    title: 'Turn AI on',
    shot: 'ai-turn-on',
    do: 'Open your avatar menu → **Preferences** → **Kiwi**, then switch on **AI features**.',
    see: `The dialog **${AI_OPT_IN_COPY.title}** opens. Read its **${AI_OPT_IN_COPY.risksHead}** list, then press **${AI_OPT_IN_COPY.confirm}**. ${AI_OPT_IN_COPY.after}.`,
    ifWrong: `The switch is greyed out and says "${AI_OFF_COPY.workstation}": whoever looks after this computer turned AI off for everyone. Ask them; it is their call, not a fault.`,
  },
  {
    title: 'Log in to Claude',
    shot: 'ai-login',
    do: 'Press **Ask Kiwi** at the top right. On the **Account** row, press **Log in**.',
    see: 'A Claude sign-in page opens. Once you sign in, the Account row shows your account instead of **Not logged in**.',
    ifWrong: 'If the page does not hand you back to Cecelia, copy the code it shows into **Paste the code from the sign-in page**.',
  },
  {
    title: KIWI_LOOP[0].title,
    shot: 'ai-attach',
    do: 'Press **@** on what puzzles you: a population or a plot. For an image, pick **Add to Kiwi** in its row’s **⋯** menu.',
    see: 'Kiwi opens with it listed under **Attached**, above the box you type your question in.',
    ifWrong: 'No **@** buttons and no **Add to Kiwi** anywhere: AI is still off for this profile (step 1).',
    note: 'To ask about something in the viewer instead, use the **Share** row in Kiwi: draw on the pop-out viewer and share the frame.',
  },
  {
    title: KIWI_LOOP[1].title,
    shot: 'ai-reply',
    do: 'Type your question in **Ask** and press Ctrl+Enter.',
    see: 'A table of claims, one fact per row, each with its refs under **Points at**. Click a ref and Cecelia shows you what it points at; compare it with the claim. A row marked ? is a question back to you.',
    ifWrong: 'Kiwi answers **Can’t say from what I looked at.**: attach what the question is about and ask again. A claim number in red cites something Kiwi didn’t read; hover it for why, and don’t rely on that claim.',
  },
  {
    title: KIWI_LOOP[2].title,
    optional: true,
    shot: 'ai-keep',
    do: 'Press **Save** under a reply. On the Blackboard, press **Tag outcome**, write why, then **Good** or **Bad**.',
    see: 'The entry with your tag and note. Entries say **by Kiwi** or **by Claude**; gates Claude proposed carry ✦ **Proposed by Claude**.',
    why: 'Replies expire after 50; the Blackboard keeps them. Bad tags are how Claude warns you when a failure comes back.',
    ifWrong: '**Good** and **Bad** stay greyed out until you write the note: the reason is required.',
  },
]

const cell = (s: string) => s.replace(/\|/g, '\\|').replace(/\n/g, ' ')

/** One mode card (Kiwi, or Claude Code in a terminal) as a section. */
function mode(m: typeof KIWI_MODE): string {
  const foot = `${m.start}. ${m.note ?? ''}`.trim()
  return [`### ${m.title}`, '', `_${m.use}_`, '', ...m.only.map(l => `- ${l}`), '', /[.!?]$/.test(foot) ? foot : `${foot}.`, ''].join('\n')
}

/** A composed screenshot at the width it was composed for (`user-docs/images/images.json`, written by
 *  scripts/docshots/compose.mjs); a shot not composed yet is a visible placeholder line. */
function image(id: string, alt: string, widths: Record<string, number>): string {
  const shot = USING_AI_SHOTS.shots.find(s => s.id === id)
  return shot && widths[id] ? `<img src="images/${imageFile(shot, USING_AI_SHOTS.themes[0])}" width="${widths[id]}" alt="${alt.replace(/"/g, '&quot;')}">`
    : `_Screenshot \`${id}\` not composed yet (pixi run doc-shots, then doc-images)._`
}

export function renderUsingAi(widths: Record<string, number>): string {
  const sent = aiDialogRows().filter(r => r.sends)
  return [
    '<!-- Generated by frontend/src/utils/usingAiDoc.ts — edit there, then `npx vitest run -u src/utils/usingAiDoc.test.ts`. -->',
    '',
    '# Using AI in Cecelia',
    '',
    '_Optional, off until you turn it on, and the analysis stays yours._',
    '',
    '- **For:** anyone using Cecelia. No AI experience needed.',
    '- **You get:** which of the two AI tools to open, how to turn AI on, and what leaves your computer when you do.',
    '- **You need:** a Claude account; each Cecelia profile logs in once.',
    '- **Ends at:** AI turned on, or deliberately left off, and your first Kiwi answer with its refs checked.',
    '',
    '> [!WARNING]',
    `> Everything in Cecelia works with AI off. ${CLAUDE_DATA_NOTICE.short}, so turning AI on is up to you, your lab and your data policy. ${CLAUDE_DATA_NOTICE.detail}.`,
    '',
    '## Two ways in',
    '',
    mode(KIWI_MODE),
    mode(TERMINAL_MODE),
    '## Words used here',
    '',
    '- **Claim:** one fact in a Kiwi reply, on its own row.',
    '- **Ref:** the clickable link on a claim to what it is about: an image, a population, a plot, a cell.',
    '- **Blackboard:** the project’s shared notes page. Saved Kiwi replies, notes and shared frames live there.',
    '- **Chain:** steps you settled on, wired together to run over a whole set of images.',
    '',
    // What "checked" means, said once. kiwi_validate_reply (api/src/kiwi_turn.jl): every ref must exist
    // AND have been read by Kiwi this turn; nothing checks that the data SUPPORTS the claim
    // (KiwiRefChip.vue header), and nothing checks the biology (CLAUDE_PROVISIONAL).
    '> [!NOTE]',
    `> “Checked” means Cecelia made sure every claim’s ref exists and that Kiwi actually read it. A claim that fails shows its number in red. Whether the data supports the claim is yours to judge. ${CLAUDE_PROVISIONAL}.`,
    '',
    `${CLAUDE_BRIDGE}.`,
    '',
    '## What is sent to Anthropic',
    '',
    '| When you use | What is sent to Anthropic |',
    '|---|---|',
    ...sent.map(r => `| ${cell(r.label)} | ${cell(r.sends ?? '')} |`),
    '',
    '## Step by step',
    '',
    ...USING_AI_STEPS.flatMap((s, i) => [
      `### ${i + 1}. ${s.title}${s.optional ? ' (optional)' : ''}`,
      '',
      `**Do:** ${s.do}`,
      '',
      `**You’ll see:** ${s.see}`,
      '',
      ...(s.why ? [`**Why:** ${s.why}`, ''] : []),
      `**If it goes wrong:** ${s.ifWrong}`,
      '',
      image(s.shot, s.title, widths),
      '',
      ...(s.note ? ['> [!NOTE]', `> ${s.note}`, ''] : []),
    ]),
    '[← Start here: what’s in Cecelia](README.md)',
    '',
  ].join('\n')
}
