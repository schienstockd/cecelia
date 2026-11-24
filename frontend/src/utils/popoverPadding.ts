// TeleportPopover owns its content's padding (the `.cc-popover` shell; `flush` tightens it). A caller
// that pads its own content root as well gets the two added together — a popover that looks roomier
// than every other one. This finds that root and checks its scoped rule. Used by the repo-wide test.
import { openingTags, templateBlock } from './continuousControls'
import { cssRules, styleBlocks } from './cssScenarios'

const classesOf = (tag: string): string[] => (tag.match(/\sclass="([^"]*)"/)?.[1] ?? '').split(/\s+/).filter(Boolean)

/** Classes on a TeleportPopover's content root whose own rule (selector `.cls`, alone or in a list)
 *  also sets padding. */
export function paddedPopoverContent(sfc: string): string[] {
  const tags = openingTags(templateBlock(sfc))
  const rules = styleBlocks(sfc).flatMap(cssRules)
  const padded = (cls: string) => rules.some(r =>
    r.selector.split(',').some(sel => sel.trim() === `.${cls}`) && /(^|[;\s])padding\s*:/.test(r.body))
  const out: string[] = []
  tags.forEach((t, i) => {
    if (!/^<TeleportPopover[\s>]/.test(t)) return
    const root = tags[i + 1]
    if (!root) return
    for (const c of classesOf(root)) if (padded(c)) out.push(c)
  })
  return out
}
