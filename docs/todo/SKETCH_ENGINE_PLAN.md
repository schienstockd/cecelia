# Sketchbook (feijoa) — authoring tool for cecelia explainers

Status: seeded, blue-sky · repo `github.com/schienstockd/feijoa` · no user-facing page in cecelia **yet**

## What this is today

A sibling play repo where sketches are authored for cecelia's tip-of-the-day and update-notes
modals. Whether the engine holds together at all is still an open question — this is exploration,
not a commitment.

Sketches surface in cecelia today only via:
- Tip-of-the-day card on launch (see `WHATS_NEW_PLAN.md`) — opt-out.
- Release-notes cards in the What's New modal.

## What it could become (blue sky, later)

If the engine matures — the aesthetic works, the format holds up, sketches feel like something
people actually understand — the door opens to a user-facing sketchbook: users author schematics
for their own analyses, or cecelia auto-generates sketch-style summaries from real data
(`StatsResult` → sketch, populations → sketch). That's not on the table now; the point of the
current phase is to find out whether it's worth taking seriously.

## Layout

- **`~/cc-workspace/feijoa/`** — the play/authoring repo. Its own Vite site
  (`npm run dev` → `:5174`) where sketches are previewed while being written.
- **`cecelia-pineapple/frontend/`** — imports `SketchCanvas` + `sketchList` from feijoa via a
  Vite alias. Cards in the What's New modal instantiate `<SketchCanvas>` when their
  `sketchAnimation?` field is populated.

## Wiring — how cecelia sees feijoa

1. `frontend/vite.config.ts` → `resolve.alias.feijoa` = `../../feijoa/src/lib/index.ts` +
   `resolve.dedupe = ['vue']`.
2. `frontend/tsconfig.app.json` → `paths.feijoa` + `include` extended so `vue-tsc` sees feijoa's
   sources.
3. Cards that carry a sketch do `import { SketchCanvas } from 'feijoa'`; nothing else in cecelia
   references feijoa.
4. Vite bundles feijoa's `.ts`/`.vue` files during cecelia's dev/build.
5. Feijoa's runtime deps (roughjs, animejs) resolve via feijoa's own `node_modules/` — install
   once in feijoa, not in cecelia.
6. `vue` is a **peerDependency** in feijoa; the deduped one from cecelia is used at runtime.

Edit a sketch in feijoa → cecelia's Vite HMRs the change (as long as feijoa is on disk).

## Setup on a fresh machine (one-time)

```bash
git clone https://github.com/schienstockd/feijoa.git ~/cc-workspace/feijoa
cd ~/cc-workspace/feijoa && npm install
```

If feijoa isn't present, cecelia's build fails with an obvious `cannot resolve 'feijoa'`. That's
deliberate — better loud than silently invisible.

## Sketch format

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
- `hmm`, `gating`, `tracking` — first-cut concept sketches for tip cards to reference.

## References

- `frontend/vite.config.ts` — alias.
- `frontend/tsconfig.app.json` — paths + include.
- `WHATS_NEW_PLAN.md` — the consumer (tip-of-the-day + release notes).
- `old-R-shiny-version/im/cciaLogo.png` — logo source of truth.
