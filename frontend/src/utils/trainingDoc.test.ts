import { describe, expect, it } from 'vitest'
import { renderTraining, SESSION_3_STEPS } from './trainingDoc'
import { stepShots } from './docPage'
import { TRAINING_SHOTS } from '../../../user-docs/shots/training.ts'

// The composed screenshots' widths, written by scripts/docshots/compose.mjs beside the images.
const META = import.meta.glob('../../../user-docs/images/images.json', { import: 'default', eager: true }) as
  Record<string, Record<string, number>>
const widths = Object.values(META)[0] ?? {}

describe('user-docs/training.md', () => {
  // Every shot of the set is on the page: the steps' shots plus the Help menu in "Before you start".
  it('shows exactly the shots of its shot set', () => {
    expect([...stepShots(SESSION_3_STEPS), 'tr-guides'].sort()).toEqual(TRAINING_SHOTS.shots.map(s => s.id).sort())
    expect(TRAINING_SHOTS.page).toBe('training.md')
  })

  // The committed page. Stale ⇒ `npx vitest run -u src/utils/trainingDoc.test.ts` and commit it.
  it('matches the generated file', async () => {
    await expect(renderTraining(widths)).toMatchFileSnapshot('../../../user-docs/training.md')
  })
})
