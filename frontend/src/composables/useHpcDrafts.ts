import type { Ref } from 'vue'
import { useFieldDraft } from './useFieldDraft'
import { HPC_DRAFT_FIELDS, hpcFieldValue, type HpcSettings } from '../utils/hpcSettings'

/**
 * One `useFieldDraft` per Settings → HPC field, keyed by its dotted path (`site.cpu.queue`).
 *
 * Per FIELD, not one draft of the whole form: every save returns the whole settings object, and a
 * form-wide re-seed on that reply wiped whatever the user was still typing in another field. Each
 * draft here watches only its own stored value, so a reply re-seeds exactly the fields it changed.
 */
export function useHpcDrafts(source: () => HpcSettings | null): Record<string, Ref<string>> {
  return Object.fromEntries(
    HPC_DRAFT_FIELDS.map(p => [p, useFieldDraft(() => hpcFieldValue(source(), p))]))
}
