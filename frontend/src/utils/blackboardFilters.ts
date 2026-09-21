// Pure filter + row-flag helpers for the Blackboard entry list. Extracted from
// BlackboardModule.vue so the filter chip's behaviour is unit-testable per the frontend testing
// rule (utils/*.ts; no component mounting).
//
// PROJECT_MEMORY_PLAN Decisions 3, 11, 12 collect on the list:
//   • status chip filter — `all | open | resolved | parked`
//   • outcome chip filter — `all | untagged | good | bad`
//   • profile row is always visible regardless of the filters (Decision 2 — it's the pinned
//     "what is this project" record, not something a filter should hide).

import type { BlackboardEntrySummary } from './blackboardApi'

export type StatusChoice  = 'all' | 'open' | 'resolved' | 'parked'
export type OutcomeChoice = 'all' | 'untagged' | 'good' | 'bad'

export const PROFILE_ENTRY_ID = 'profile'

/** True if the entry matches BOTH filters, or is the profile (which never drops out of the list). */
export function entryPassesFilters(
  e: BlackboardEntrySummary,
  status: StatusChoice,
  outcome: OutcomeChoice,
): boolean {
  if (e.entryId === PROFILE_ENTRY_ID) return true
  if (status !== 'all' && e.status !== status) return false
  if (outcome === 'all')      return true
  if (outcome === 'untagged') return !e.outcome
  return !!e.outcome && e.outcome.verdict === outcome
}

/** Apply both filters over the list. Preserves input order (already newest-first from the API). */
export function filterEntries(
  entries: BlackboardEntrySummary[],
  status: StatusChoice,
  outcome: OutcomeChoice,
): BlackboardEntrySummary[] {
  return entries.filter(e => entryPassesFilters(e, status, outcome))
}
