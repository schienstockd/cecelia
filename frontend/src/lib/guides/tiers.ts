// The maturity scale's words — the badge's label and its hover line (docs/todo/FEATURE_MATURITY_PLAN.md
// → *The scale*). Apart from `utils/maturity.ts` so `ComingSoonBadge` can name a tier without pulling in
// the guide catalogue.
import type { Tier } from './types'

export const TIER_LABEL: Record<Tier, string> = {
  comingSoon: 'Coming soon',
  experimental: 'Experimental',
  beta: 'Beta',
  tested: 'Tested',
  stable: 'Stable',
  exempt: '',
}

/** What the user should do at each tier. */
export const TIER_TIP: Record<Tier, string> = {
  comingSoon: 'Not usable yet',
  experimental: 'Not checked on realistic data — check every result by hand',
  beta: 'Checked by hand on real data — spot-check results',
  tested: 'A test fails when the result is wrong',
  stable: 'Safe to rely on across versions',
  exempt: '',
}

/** The badge's hover line: "Beta: checked by hand on real data — spot-check results". */
export const tierTip = (t: Tier): string => `${TIER_LABEL[t]}: ${TIER_TIP[t].toLowerCase()}`
