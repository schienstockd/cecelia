import { describe, expect, it } from 'vitest'
import { renderUsingAi, USING_AI_STEPS } from './usingAiDoc'
import { USING_AI_SHOTS } from '../../../user-docs/shots/using-ai.ts'

// The composed screenshots' widths, written by scripts/docshots/compose.mjs beside the images.
const META = import.meta.glob('../../../user-docs/images/images.json', { import: 'default', eager: true }) as
  Record<string, Record<string, number>>
const widths = Object.values(META)[0] ?? {}

describe('user-docs/using-ai.md', () => {
  // Every step's screenshot is a shot of the page's set, and every shot of the set is on the page.
  it('shows exactly the shots of its shot set', () => {
    expect(USING_AI_STEPS.map(s => s.shot).sort()).toEqual(USING_AI_SHOTS.shots.map(s => s.id).sort())
    expect(USING_AI_SHOTS.page).toBe('using-ai.md')
  })

  // The committed page. Stale ⇒ `npx vitest run -u src/utils/usingAiDoc.test.ts` and commit it. Edit the
  // words in utils/usingAiDoc.ts, or the app text it imports.
  it('matches the generated file', async () => {
    await expect(renderUsingAi(widths)).toMatchFileSnapshot('../../../user-docs/using-ai.md')
  })
})
