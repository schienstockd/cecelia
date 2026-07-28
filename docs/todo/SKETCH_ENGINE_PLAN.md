# Sketchbook (feijoa) — authoring tool for cecelia explainers

Status: wired · repo `github.com/schienstockd/feijoa` · git-dep + conditional sibling Vite alias

## What this is today

A sibling play repo where sketches are authored for cecelia's tip-of-the-day and update-notes
modals. Whether the engine holds together at all is still an open question — this is exploration,
not a commitment.

Cecelia now consumes feijoa as a git dependency (`"feijoa": "github:schienstockd/feijoa#main"` in
`frontend/package.json`). `WhatNewCard.vue` imports `SketchCanvas` + `sketches` from `feijoa` and
resolves `card.sketchAnimation.id` against the catalogue; unknown ids fall through to the grey
"coming soon" placeholder. The three seeded tips (hmm / clusters / gating) all resolve now that
`clusters.ts` shipped. The wiring pattern (git dep + conditional sibling alias) is captured below
for reference and for anyone touching either config file.

## What it could become (blue sky, later)

If the engine matures — the aesthetic works, the format holds up, sketches feel like something
people actually understand — the door opens to a user-facing sketchbook: users author schematics
for their own analyses, or cecelia auto-generates sketch-style summaries from real data
(`StatsResult` → sketch, populations → sketch). That's not on the table now; the point of the
current phase is to find out whether it's worth taking seriously.

## Wiring — when we actually consume feijoa in cecelia

The pattern is a **git dependency + conditional Vite alias** — fresh clones and CI installs
resolve feijoa via `npm install` from GitHub; sibling checkouts additionally get hot-reload from
the local source. This works for both `install.sh` channels:

- **stable install** — ships a prebuilt frontend `dist/`; feijoa is bundled at CI build time
  (CI's `npm install` pulls it from GitHub).
- **dev install** — runs `npm install && npm run build` locally; same GitHub fetch.

**Add to `frontend/package.json`**:
```jsonc
"dependencies": {
  "feijoa": "github:schienstockd/feijoa#main"   // main branch, not a tag
  // package-lock.json pins the resolved sha for reproducibility
  // `npm update feijoa` advances to the latest main commit
}
```

**Add to `frontend/vite.config.ts`** (conditional — sibling override only when present):
```ts
import { existsSync } from 'node:fs'
import { fileURLToPath, URL } from 'node:url'

// Four hops up: frontend → cecelia-feijoa → cecelia → cc-workspace → feijoa
const feijoaSibling = fileURLToPath(new URL('../../../feijoa/src/lib/index.ts', import.meta.url))
const feijoaAlias = existsSync(feijoaSibling) ? { feijoa: feijoaSibling } : {}

export default defineConfig({
  resolve: {
    alias: feijoaAlias,   // hot-reload from sibling; else falls through to node_modules
    dedupe: ['vue'],      // feijoa marks vue as peer; ensure one instance across both apps
  },
  // …
})
```

**No tsconfig `paths` needed** — `feijoa`'s `package.json` `exports` point at `src/lib/index.ts`,
so `vue-tsc` resolves it via `node_modules/feijoa/…` in the standard way.

Landed in the same commit as `WhatNewCard.vue`'s first `from 'feijoa'` import — earlier would have been unused scaffolding.

## Sketch format (in feijoa)

```ts
interface SketchDefinition { id, title, width, height, durationSec, acts: SketchAct[] }
type SketchAct =
  | { type: 'line' | 'arrow', from, to, colour?, delayMs?, drawMs? }
  | { type: 'rect' | 'circle' | 'ellipse' | 'path', ..., fill?, stroke?, ... }
  | { type: 'text', at, value, size?, weight? }
  | { type: 'cell', at, r, colour? }
  | { type: 'pause', ms }
  | { type: 'wipe' }
```

JSON-serialisable. Sketches live in `~/cc-workspace/feijoa/src/sketches/*.ts`.

## Seeded sketches

- `logo` — R Cecelia logo ported (smoke test, splash/setup wizard target).
- `hmm`, `gating`, `tracking`, `clusters` — first-cut concept sketches wired into the three
  seeded tip cards (`clusters` covers the "cluster labels → populations" tip).

## References

- `docs/prompts/sketch-engine-prompt.md` — the original Sonnet draft (superseded).
- `frontend/src/lib/whatsNew.ts` — the `WhatNewCard.sketchAnimation?` slot (unused today).
- `frontend/src/components/WhatNewCard.vue` — where the grey placeholder currently renders.
- `WHATS_NEW_PLAN.md` — the consumer (tip-of-the-day + release notes).
- `old-R-shiny-version/im/cciaLogo.png` — logo source of truth.
