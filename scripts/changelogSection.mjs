// One version's section out of CHANGELOG.md — what a GitHub Release body should be.
//
// WHY this exists. `release.yml` used `generate_release_notes: true`, so every release body was
// GitHub's auto-generated PR list (`* title by @user in #N`) — 450 lines of it for v0.1.0. That list
// is also what the in-app What's New modal renders, since `/api/update/check` passes the release body
// straight through as markdown (api/src/update_api.jl → lib/whatsNew.ts). A user opening it learned
// nothing they could act on (Dominik, 2026-08-10: "they will just say... ok. not sure what that tells
// me now"). The hand-written notes already existed one file away, in CHANGELOG.md, doing nothing.
//
// So the CHANGELOG becomes the release body, and stops being a mirror that has to be kept in step.
// The consequence is worth stating: the section must exist BEFORE the tag is pushed, and a missing one
// FAILS the release rather than quietly publishing an empty body — see docs/RELEASING.md.
//
// Usage:  node scripts/changelogSection.mjs 0.1.1 [path/to/CHANGELOG.md]
// Prints the section to stdout; exits non-zero with a readable reason if there isn't one.

import { readFileSync } from 'node:fs'

/**
 * `v0.1.1` / `0.1.1` → `0.1.1`. Tags carry the `v`, the CHANGELOG headings do not.
 *
 * Anything that is not a VERSION returns `''`, which makes the lookup below fail rather than match.
 * That is what stops `Unreleased` from being asked for and found: it is a real bracketed heading, so
 * a plain name match would happily return "changes not yet tagged in a release" as a release body.
 */
export function normaliseVersion(raw) {
  const v = String(raw ?? '').trim().replace(/^v/, '')
  return /^\d+(?:\.\d+)*(?:[-+][0-9A-Za-z.-]+)?$/.test(v) ? v : ''
}

/**
 * The body of `## [<version>] — <date>`, up to the next `## ` heading.
 *
 * Returns `null` when there is no such section — the caller decides what that means, and for a
 * release it means stop. Deliberately strict about which heading it matches:
 *
 *  - `[Unreleased]` is never a version, so asking for one cannot fall through to it. That is the
 *    failure that would hurt: `[Unreleased]` is the block that exists between releases, so a typo'd
 *    version would otherwise publish "_Changes on `main` that have not yet been tagged_" to everyone.
 *  - the trailing link-definition block (`[0.1.1]: https://…/compare/…`) is not part of any section,
 *    because those lines start at column 0 after the last `## ` — they are cut by the same
 *    next-heading rule only if a heading follows. It doesn't, so they are stripped explicitly.
 *
 * The heading's own line is dropped: GitHub already shows the tag and date above the body.
 */
export function changelogSection(md, version) {
  const v = normaliseVersion(version)
  if (!v) return null
  const lines = String(md ?? '').split('\n')
  // `## [0.1.1] — 2026-08-10`, `## [0.1.1]`, `## [0.1.1] - 2026-08-10` — the separator and the date
  // are cosmetic and have varied; the bracketed version is the identity.
  const esc = v.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')
  const head = new RegExp(`^##\\s+\\[${esc}\\]`)
  const start = lines.findIndex(l => head.test(l))
  if (start === -1) return null
  let end = lines.length
  for (let i = start + 1; i < lines.length; i++) {
    if (/^##\s/.test(lines[i])) { end = i; break }
  }
  const body = lines.slice(start + 1, end)
    // the link definitions at the foot of the file belong to no section
    .filter(l => !/^\[[^\]]+\]:\s*https?:\/\//.test(l))
    .join('\n')
    .trim()
  return body || null
}

/** The section plus a compare link back to the previous tag, when one is known. */
export function releaseBody(md, version, { repo, previousTag } = {}) {
  const body = changelogSection(md, version)
  if (!body) return null
  if (!repo || !previousTag) return body
  const tag = `v${normaliseVersion(version)}`
  return `${body}\n\n---\n\n` +
    `**Full commit log:** [\`${previousTag}…${tag}\`](https://github.com/${repo}/compare/${previousTag}...${tag})`
}

// ── CLI ──────────────────────────────────────────────────────────────────────
// `import.meta.main` is Node 24+; the argv check keeps it working if this is ever run on older.
if (process.argv[1] && process.argv[1].endsWith('changelogSection.mjs')) {
  const [, , version, path = 'CHANGELOG.md'] = process.argv
  if (!version) {
    console.error('usage: node scripts/changelogSection.mjs <version> [CHANGELOG.md]')
    process.exit(2)
  }
  const md = readFileSync(path, 'utf8')
  const out = releaseBody(md, version, {
    repo: process.env.GITHUB_REPOSITORY,
    previousTag: process.env.PREVIOUS_TAG,
  })
  if (!out) {
    console.error(
      `No CHANGELOG section for ${normaliseVersion(version)} in ${path}.\n` +
      'Add one before tagging — the release body comes from here now, so publishing without it\n' +
      'would ship an empty release. See docs/RELEASING.md → Cutting a release.')
    process.exit(1)
  }
  process.stdout.write(out + '\n')
}
