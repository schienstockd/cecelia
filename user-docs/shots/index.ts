// Every user-docs page with screenshots: its shots and the fixture they run on (spec.ts → ShotSet).
// scripts/docshots/ captures and composes them; utils/docShots.test.ts checks them in CI.

import type { ShotSet } from './spec.ts'
import { USING_AI_SHOTS } from './using-ai.ts'

export const SHOT_SETS: ShotSet[] = [
  USING_AI_SHOTS,
]
