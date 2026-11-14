// The AI switch as the UI sees it (docs/todo/AI_TOGGLE_PLAN.md P3). The server decides — peanut's
// workstation ceiling, then the profile's own `aiEnabled` (`app/src/ai/ai_switch.jl`) — and reports it
// on GET /api/observer/status as `ai: {enabled, setBy}`. The UI only hides; the server refuses.

export type AiSetBy = 'workstation' | 'profile' | null
export interface AiState { enabled: boolean; setBy: AiSetBy }

/** The state from a status reply. A backend without the switch (older, or not restarted since the
 *  update) reports no `ai` and has no gate — on, as it always was. */
export function aiStateFromStatus(s: { ai?: { enabled?: unknown; setBy?: unknown } } | null | undefined): AiState {
  const ai = s?.ai
  if (!ai || typeof ai.enabled !== 'boolean') return { enabled: true, setBy: null }
  const setBy = ai.setBy === 'workstation' || ai.setBy === 'profile' ? ai.setBy : null
  return { enabled: ai.enabled, setBy: ai.enabled ? null : (setBy ?? 'profile') }
}

/** Can this person turn AI on themselves? Not when the workstation turned it off. */
export const aiCanTurnOn = (st: AiState): boolean => !st.enabled && st.setBy !== 'workstation'

export const AI_OFF_COPY = {
  title: 'AI features are off',
  workstation: 'Turned off for this workstation',
  profile: 'They send project data to Anthropic',
  turnOn: 'Turn on…',
  hide: 'Hide this button',
}

/** The line under the title in Kiwi's off panel. */
export const aiOffLine = (st: AiState): string =>
  st.setBy === 'workstation' ? AI_OFF_COPY.workstation : AI_OFF_COPY.profile

/** Show the Kiwi launcher? Always while AI is on; while off, unless this profile hid it. */
export const showKiwiLauncher = (st: AiState, hidden: boolean): boolean => st.enabled || !hidden

/** The opt-in dialog's fixed text (AiOptInDialog.vue). The per-surface lines come from the registry. */
export const AI_OPT_IN_COPY = {
  title: 'Turn on AI features?',
  lead: 'Off by default: they send data to Anthropic. Whether that’s OK is up to you, your lab and your data policy.',
  sentHead: 'What is sent',
  gainHead: 'What you gain',
  risksHead: 'Keep in mind',
  risks: [
    'Data leaves this machine — image crops too, not only numbers',
    'Answers can be wrong or differ each time; the analysis stays yours',
    'Use counts against your Claude account',
  ],
  terms: 'Anthropic’s terms',
  after: 'Turn it off any time in Preferences → Kiwi; everything else works the same',
  confirm: 'Turn on',
  failed: 'Could not save — is Cecelia still running?',
  blocked: 'AI is turned off for this workstation',
}

/** Guides that only teach AI features — left out of the picker while AI is off. */
export const AI_ONLY_GUIDES: readonly string[] = ['assist-with-kiwi']
/** The mirror: the way to turn AI on — left out of the picker once it is on. */
export const AI_OFF_ONLY_GUIDES: readonly string[] = ['turn-on-ai']

/** The picker's groups for the AI state: AI-only guides dropped while it is off, the turn-it-on guide
 *  once it is on (and an emptied group with them). */
export function guideGroupsFor<G extends { guides: { id: string }[] }>(groups: G[], aiEnabled: boolean): G[] {
  const drop = aiEnabled ? AI_OFF_ONLY_GUIDES : AI_ONLY_GUIDES
  return groups.map(g => ({ ...g, guides: g.guides.filter(x => !drop.includes(x.id)) }))
    .filter(g => g.guides.length > 0)
}
