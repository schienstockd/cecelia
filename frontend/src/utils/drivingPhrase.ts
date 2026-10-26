// The profile picker's heading. Plain "Who's driving?" half the time; otherwise a random
// translation or lab pun, so the joke stays a surprise rather than noise. Not localisation — the
// rest of the picker stays English, and `note` (the tooltip) says what a translation means.

export interface DrivingPhrase {
  text: string
  /** Tooltip — null for the English variants, which need no gloss. */
  note: string | null
}

export const PLAIN_PHRASE: DrivingPhrase = { text: "Who's driving?", note: null }

export const VARIANT_PHRASES: readonly DrivingPhrase[] = [
  { text: 'Wer fährt?', note: "German for “Who's driving?”" },
  { text: 'Qui conduit ?', note: "French for “Who's driving?”" },
  { text: '¿Quién maneja?', note: "Spanish for “Who's driving?”" },
  { text: 'Chi guida?', note: "Italian for “Who's driving?”" },
  { text: 'Wie rijdt er?', note: "Dutch for “Who's driving?”" },
  { text: '誰が運転する？', note: "Japanese for “Who's driving?”" },
  { text: "Who's at the wheel?", note: null },
  { text: "Who's on the scope?", note: null },
  { text: 'Whose bench is this?', note: null },
  { text: "Who's holding the pipette?", note: null },
]

/** `rand` in [0,1): the lower half picks the plain phrase, the upper half spreads over the variants. */
export function pickDrivingPhrase(rand: number = Math.random()): DrivingPhrase {
  if (rand < 0.5) return PLAIN_PHRASE
  const i = Math.floor((rand - 0.5) * 2 * VARIANT_PHRASES.length)
  return VARIANT_PHRASES[Math.min(i, VARIANT_PHRASES.length - 1)]
}
