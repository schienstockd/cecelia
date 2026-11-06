// The project's outward links, in one place.
//
// There were three hardcoded `github.com/schienstockd/cecelia/…` literals across the frontend before
// this file (the What's New "Report a problem" default, its "View on GitHub" footer link, and the
// releases page) and the header was about to add two more. A repo move is one rename here, not a
// grep — and the rename is coming: `docs/SHIPPING.md` → *Repo swap* still has the legacy repo living
// at the old name.
//
// Kept deliberately dumb — string constants only, no fetching. `stores/appControl.ts` owns anything
// that ASKS GitHub something (the update check reads the releases API); this module only says where
// things are.

import type { MicroscopeRequest } from '../utils/acquisitionSettings'

export const CECELIA_REPO_URL = 'https://github.com/schienstockd/cecelia'

/** The issues LIST — "here is where problems get reported", for a browse-first entry point. */
export const CECELIA_ISSUES_URL = `${CECELIA_REPO_URL}/issues`
/** Straight to the form — for a "report this" action, where the user already knows what they hit. */
export const CECELIA_NEW_ISSUE_URL = `${CECELIA_REPO_URL}/issues/new`
export const CECELIA_RELEASES_URL = `${CECELIA_REPO_URL}/releases`

/**
 * A processing-recipe request, prefilled with the scenario the user clicked — the `Request` links in
 * `GuidesDialog` (docs/todo/WORKFLOW_RECIPES_PLAN.md D9).
 *
 * Points at the recipe FORM rather than a blank issue on purpose: the form asks the three things we
 * cannot guess — what they image, what they want out of it, and whether they can share an example
 * image — and a recipe written without those is the invented prose the guide system keeps getting
 * bitten by. Still just a string; nothing here posts anything.
 */
export function recipeRequestUrl(name: string): string {
  const q = new URLSearchParams({ template: 'recipe_request.yml', title: `Recipe: ${name}` })
  return `${CECELIA_REPO_URL}/issues/new?${q}`
}

/** The general feature-request form — what Claude hands over when Cecelia cannot do something and no
 *  Call for Datasets entry fits (served to it through `get_guide`, `guideText.ts`). */
export const FEATURE_REQUEST_URL = `${CECELIA_NEW_ISSUE_URL}?${new URLSearchParams({ template: 'feature_request.yml' })}`

// Zulip, not a GitHub Discussion: the lab already runs one, and a question people ask in chat is a
// question they would not have opened an issue for.
export const CECELIA_CHAT_URL = 'https://cecelia.zulipchat.com/'

/**
 * A "please support this file format for series picking" feature request, prefilled with the
 * extensions the user just hit. The picker only reads series from `.lif` today (readlif — pure
 * Python, no JVM); every other multi-series format falls through to a series-0 default. This turns
 * "we can't preview this" into a directed ask — the reason we bank the request is that the fastest
 * way to widen coverage is a sample file we can measure against (docs/PROVENANCE.md → *Real-data
 * visual validation*), so the prompt is what to attach.
 */
export function formatSupportRequestUrl(exts: string[]): string {
  const dedup = Array.from(new Set(exts.map(e => e.toLowerCase().replace(/^\./, ''))))
  const list  = dedup.join(', ')
  const q = new URLSearchParams({
    template: 'feature_request.yml',
    title:    `Series picker: support ${list}`,
  })
  return `${CECELIA_REPO_URL}/issues/new?${q}`
}

/** A "support my microscope" request: a settings reader for a file type (the ask is a sample file —
 *  a reader is written and tested against a real one), or the diagram and settings card for a
 *  microscope whose settings are read but which is not two-photon, prefilled with what the file names. */
export function acquisitionSupportRequestUrl(req: MicroscopeRequest): string {
  const title = req.what === 'settings'
    ? `Acquisition settings: support ${req.ext} files (microscope: …)`
    : `Light-path diagram and settings card: support ${
        [req.system, req.softwareVersion && `software ${req.softwareVersion}`].filter(Boolean).join(' · ') || '…'}`
  const q = new URLSearchParams({ template: 'feature_request.yml', title })
  return `${CECELIA_REPO_URL}/issues/new?${q}`
}

/** The bug form's OS dropdown word for a pixi platform label (`utils/systemEnvs.ts` → `platform`). */
export function issueOsLabel(platform: string | null | undefined): string {
  if (!platform) return ''
  if (platform.startsWith('linux')) return 'Linux'
  if (platform.startsWith('osx')) return 'macOS'
  if (platform.startsWith('win')) return 'Windows'
  return ''
}

/** What a "Something wrong?" report carries. `params` only when the user ticked for it. */
export interface ProblemReport {
  feature: string
  tier: string                  // the tier's word ("Experimental")
  page?: string                 // the route it was reported from
  version?: string              // `appCtl.updateCurrent`
  platform?: string             // the server's pixi platform ("linux-64")
  lastTask?: string             // "segment.cellposeMeasure — failed"
  params?: Record<string, unknown> | null
}

/** GitHub rejects very long URLs (~8 KB); stay under with room for the encoding. */
export const REPORT_URL_MAX = 7500
/** The settings block's own cap, before encoding — a long param list is cut, not dropped. */
export const REPORT_PARAMS_MAX = 2000

/**
 * A prefilled bug report for one feature — the "Something wrong?" link on a page's maturity badge and
 * on the map's Maturity rows (docs/todo/FEATURE_MATURITY_PLAN.md → *Something wrong?*).
 *
 * Goes to the bug FORM, filling its fields by id (`what-happened`, `version`, `os`). Task settings go in
 * only when passed: GitHub is public, and settings carry file paths and project names, so the caller
 * asks first. Pure — the caller reads the version, platform and last task.
 */
export function problemReportUrl(r: ProblemReport): string {
  const os = issueOsLabel(r.platform)
  const facts = [
    `Feature: ${r.feature} (${r.tier})`,
    r.page && `Page: ${r.page}`,
    r.version && `Cecelia: ${r.version}`,
    r.platform && `OS: ${os || r.platform}${os ? ` (${r.platform})` : ''}`,
    r.lastTask && `Last task: ${r.lastTask}`,
  ].filter(Boolean).join('\n')
  const head = `What went wrong?\n\n\n---\n${facts}`
  const build = (settings: string) => {
    const q = new URLSearchParams({ template: 'bug_report.yml', title: `${r.feature}: `, 'what-happened': head + settings })
    if (r.version) q.set('version', r.version)
    if (os) q.set('os', os)
    return `${CECELIA_REPO_URL}/issues/new?${q}`
  }
  if (!r.params) return build('')
  let json = JSON.stringify(r.params, null, 1)
  if (json.length > REPORT_PARAMS_MAX) json = `${json.slice(0, REPORT_PARAMS_MAX)}\n… (cut)`
  const url = build(`\n\nLast task's settings:\n\`\`\`json\n${json}\n\`\`\``)
  return url.length <= REPORT_URL_MAX ? url : build('\n\n(Settings left out: too long for a link)')
}
