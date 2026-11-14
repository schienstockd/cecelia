// Every set of docs screenshots: its shots and the fixture they run on (spec.ts → ShotSet). A user-docs
// page, the repository README or the app (What's new, tips). scripts/docshots/ captures and composes
// them; utils/docShots.test.ts checks them in CI.

import type { ShotSet } from './spec.ts'
import { IN_APP_SHOTS } from './in-app.ts'
import { README_SHOTS } from './readme.ts'
import { USING_AI_SHOTS } from './using-ai.ts'

export const SHOT_SETS: ShotSet[] = [
  USING_AI_SHOTS,
  README_SHOTS,
  IN_APP_SHOTS,
]
