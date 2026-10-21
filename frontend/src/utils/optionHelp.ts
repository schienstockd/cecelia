/**
 * Which per-option help line to show under a `select`.
 *
 * A param-level `tip` describes the control; this describes the CHOICE. "Gated" is meaningless on its
 * own and what you need to know differs per option, so one tip cannot carry it — while a permanently
 * visible paragraph per option would breach the UI copy budget (docs/UI.md). Hence one short line,
 * rendered through `InlineNote`, for the option actually selected.
 *
 * NOT a param advisory (`tasks/paramAdvisors.ts`): an advisory reports on the user's DATA and carries
 * a severity. This is static guidance that consulted nothing, and `severity: 'ok'` renders a green
 * check-circle — a verdict nobody reached.
 *
 * Pure so it is testable without mounting `ParamRenderer` (docs/DEV.md → Tests).
 */
export interface HelpOption { label: string; value: string; help?: string }

export function selectedOptionHelp(
  options: readonly HelpOption[] | undefined,
  value: unknown,
): string {
  if (!options?.length) return ''
  const v = String(value ?? '')
  // Compare as strings: a select's DOM value is always a string while a spec default may be a number
  // or a bool, so a strict === would silently never match those.
  return options.find(o => String(o.value) === v)?.help ?? ''
}

/**
 * The option value a `select` picked, in the spec's own type. The DOM hands back a string, so a numeric
 * option (`logBase`: 0 / 2 / 10) would otherwise reach the backend as "10". Falls back to the string.
 */
export function selectedOptionValue<V>(
  options: readonly { value: V }[] | undefined,
  domValue: string,
): V | string {
  const hit = options?.find(o => String(o.value) === domValue)
  return hit ? hit.value : domValue
}
