import { describe, it, expect } from 'vitest'
// The extractor lives in scripts/ because release.yml runs it, not the app — but what it produces is
// what every user reads in the What's New modal, so it is tested here with the rest of the pure logic.
// @ts-expect-error — plain .mjs, no type declarations
import { changelogSection, releaseBody, normaliseVersion } from '../../../scripts/changelogSection.mjs'

const MD = `# Changelog

Preamble that belongs to no section.

## [Unreleased]

_Changes on \`main\` that have not yet been tagged in a release._

## [0.1.1] — 2026-08-10

### Changed
- the store format moved

### Fixed
- a thing

## [0.1.0] — 2026-08-05

The first plain release.

[Unreleased]: https://github.com/schienstockd/cecelia/compare/v0.1.1...HEAD
[0.1.1]: https://github.com/schienstockd/cecelia/compare/v0.1.0...v0.1.1
`

describe('normaliseVersion', () => {
  it('drops the tag\'s v — tags carry it, CHANGELOG headings do not', () => {
    expect(normaliseVersion('v0.1.1')).toBe('0.1.1')
    expect(normaliseVersion(' 0.1.1 ')).toBe('0.1.1')
  })
})

describe('changelogSection', () => {
  it('returns the section body, without its own heading', () => {
    const s = changelogSection(MD, 'v0.1.1')
    expect(s).toContain('### Changed')
    expect(s).toContain('the store format moved')
    expect(s?.startsWith('## [')).toBe(false)   // GitHub already shows the tag and date
    expect(s?.startsWith('### Changed')).toBe(true)   // …but the inner headings are kept
  })

  it('stops at the next version, so one release does not swallow the previous', () => {
    expect(changelogSection(MD, '0.1.1')).not.toContain('The first plain release')
  })

  it('excludes the trailing link definitions, which belong to no section', () => {
    expect(changelogSection(MD, '0.1.0')).not.toContain('compare/v0.1.0...v0.1.1')
    expect(changelogSection(MD, '0.1.0')).toBe('The first plain release.')
  })

  it('NEVER falls through to [Unreleased]', () => {
    // the failure that would actually hurt: a typo'd version publishing "changes not yet tagged"
    expect(changelogSection(MD, '0.9.9')).toBeNull()
    expect(changelogSection(MD, 'Unreleased')).toBeNull()
  })

  it('is null for an empty section, so a placeholder heading cannot ship as a release', () => {
    expect(changelogSection('## [1.0.0] — 2026-01-01\n\n## [0.9.0]\n\nold\n', '1.0.0')).toBeNull()
  })

  it('tolerates a heading with no date, or an ASCII dash', () => {
    expect(changelogSection('## [2.0.0]\n\nbody\n', '2.0.0')).toBe('body')
    expect(changelogSection('## [2.0.0] - 2026-01-01\n\nbody\n', '2.0.0')).toBe('body')
  })

  it('matches the version exactly — 0.1.1 is not 0.1.10', () => {
    expect(changelogSection('## [0.1.10]\n\nten\n', '0.1.1')).toBeNull()
  })
})

describe('releaseBody', () => {
  it('appends a compare link when the previous tag is known', () => {
    const b = releaseBody(MD, 'v0.1.1', { repo: 'schienstockd/cecelia', previousTag: 'v0.1.0' })
    expect(b).toContain('compare/v0.1.0...v0.1.1')
    expect(b).toContain('### Changed')
  })

  it('is just the section when there is no previous tag (the first release)', () => {
    const b = releaseBody(MD, '0.1.1', { repo: 'schienstockd/cecelia' })
    expect(b).not.toContain('Full commit log')
  })

  it('is null when the section is missing — the caller must fail the release, not publish empty', () => {
    expect(releaseBody(MD, '9.9.9', { repo: 'x/y', previousTag: 'v0.1.0' })).toBeNull()
  })
})
