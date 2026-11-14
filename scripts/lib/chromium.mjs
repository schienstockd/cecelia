// The browser the repo's page scripts drive: playwright-core (a frontend devDependency) with a SYSTEM
// Chromium — no browser download. CECELIA_CHROMIUM picks the binary (scripts/agent_eval/board_render.py
// reads the same variable). Used by scripts/gui_capture.mjs and scripts/booklets/build.mjs.

import { createRequire } from 'node:module'
import { existsSync } from 'node:fs'
import { dirname, join, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

export const repo = resolve(dirname(fileURLToPath(import.meta.url)), '..', '..')
export const { chromium } = createRequire(join(repo, 'frontend', 'package.json'))('playwright-core')

export function findChromium() {
  const c = [process.env.CECELIA_CHROMIUM, '/usr/bin/chromium-browser', '/usr/bin/chromium', '/snap/bin/chromium',
    '/usr/bin/google-chrome', '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'].filter(Boolean)
  const hit = c.find(p => existsSync(p))
  if (!hit) throw new Error('no Chromium found; set CECELIA_CHROMIUM')
  return hit
}
