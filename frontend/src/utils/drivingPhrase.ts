// The profile picker's heading. Plain "Who's driving?" half the time; otherwise a random lab pun, so
// the joke stays a surprise rather than noise.

export const PLAIN_PHRASE = "Who's driving?"

export const VARIANT_PHRASES: readonly string[] = [
  "Who's got the mouse?",
  "Who's in focus?",
  "Who's gating today?",
  'Who booked the scope?',
  'Whose bench is this?',
]

/** `rand` in [0,1): the lower half picks the plain phrase, the upper half spreads over the variants. */
export function pickDrivingPhrase(rand: number = Math.random()): string {
  if (rand < 0.5) return PLAIN_PHRASE
  const i = Math.floor((rand - 0.5) * 2 * VARIANT_PHRASES.length)
  return VARIANT_PHRASES[Math.min(i, VARIANT_PHRASES.length - 1)]
}
