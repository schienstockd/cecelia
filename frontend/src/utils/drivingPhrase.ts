// The profile picker's heading. Plain "Who's driving?" half the time; otherwise a random lab or
// microscopy-as-art pun, so the joke stays a surprise rather than noise. `note` (the tooltip)
// glosses the one that needs it.

export interface DrivingPhrase {
  text: string
  /** Tooltip — null when the pun needs no gloss. */
  note: string | null
}

export const PLAIN_PHRASE: DrivingPhrase = { text: "Who's driving?", note: null }

export const VARIANT_PHRASES: readonly DrivingPhrase[] = [
  // the lab
  { text: "Who's got the mouse?", note: null },
  { text: "Who's in focus?", note: null },
  { text: "Who's gating today?", note: null },
  { text: 'Who booked the scope?', note: null },
  { text: 'Whose bench is this?', note: null },
  // microscopy as art — the image competitions
  { text: "Who's behind the lens?", note: null },
  { text: "Who's painting with photons?", note: null },
  { text: "Who's holding the brush?", note: null },
  { text: 'Whose gallery is this?', note: null },
  { text: "People's Choice?", note: 'As in Art of Discovery' },
]

/** `rand` in [0,1): the lower half picks the plain phrase, the upper half spreads over the variants. */
export function pickDrivingPhrase(rand: number = Math.random()): DrivingPhrase {
  if (rand < 0.5) return PLAIN_PHRASE
  const i = Math.floor((rand - 0.5) * 2 * VARIANT_PHRASES.length)
  return VARIANT_PHRASES[Math.min(i, VARIANT_PHRASES.length - 1)]
}
