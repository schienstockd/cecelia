> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

# feijoa: Improve sketch character

Read every file in `src/sketches/` and the renderer (`src/lib/interpret.ts`,
`src/lib/types.ts`, `src/lib/palette.ts`) before changing anything.
The logo sketch is the canonical test case — all variants are built there.

## The brief

Figure drawing. Light. Loose. Gesture.

**Not**: a vector illustration with texture. **Not**: wobble effects.
**Not**: SVG filters — the current `feTurbulence` + `feDisplacementMap` paper
filter in `interpret.ts` reads as "vector with texture", not "hand drew this".
Remove it. Do not replace it with `feGaussianBlur` or any other filter on path
elements. Character comes from font, stroke weight, and timing — not from
post-processing.

The feeling is: someone drew this quickly and knew exactly what they were
doing. A confident line. A hand that lifted and repositioned between strokes.
The sketch is done in ~2 seconds and it's right.

## Approach: explore, then show

Do not commit to one direction. Build **4 distinct variants of the logo**,
each testing a genuinely different combination of the axes below. Render
them side by side in the app (a new "Logo variants" view — the deliverable
is the running comparison, not literal screenshots). D picks one; then we
iterate on that.

The four variants must be visibly different at a glance, not tone-shifts
of one idea.

## Axes to combine

Each variant picks a distinct point in this space. Font is 80% of character —
no two variants should share a font.

**Font** — Caveat, Patrick Hand, Kalam. Each has a personality: Caveat is
loose and cursive; Patrick Hand is print-like and even; Kalam has more
weight and edge. Load them via a Google Fonts `<link>` in `index.html`
(they aren't wired up today).

**Stroke-weight hierarchy** — vary `strokeWidth` *between* elements
(main form heavier, annotation lines lighter, labels thinnest). The
renderer already honours per-act `strokeWidth`; use it deliberately.

**Timing profile** — the whole logo in 1.2s (gesture), or 2.2s with pauses
between elements (deliberate), or 1.8s with staggered overlaps (staggered).
Semantic timing — background/context fast, the point being made slow-then-fast,
labels appear after a short pause.

**Endpoint character** — lines that snap cleanly vs lines that overshoot
their corners by 2-3px. Small change, big feel. SVG lines don't overshoot
by default — either compute extended coordinates in the sketch or extend
`SketchAct` with an optional `overshoot?: number`.

**Fills** — pastel fills + thin strokes vs no fills + heavier strokes vs
"only the cells are filled".

## Semantic `timing` hint (optional, back-compat)

Add an **optional** `timing?: 'fast' | 'normal' | 'deliberate'` field on
`Timed` in `src/lib/types.ts`. The interpreter maps it to a `drawMs`
multiplier (`fast`≈0.5×, `normal`=1×, `deliberate`≈1.7×) so authors declare
importance without picking exact milliseconds. Existing sketches (and the
cecelia `/sketchbook` consumer) must keep working without changes — the
field is opt-in per act.

Recommended per-act use:
- `fast` — background, scatter points, cell fields (the stage, not the story)
- `deliberate` — the point being made (gate polygon, key structural line)
- `normal` — everything else

## Animation flow (already possible — use it)

anime.js + `stroke-dashoffset` is already working. The logo variants should
animate, not just render. A canonical logo sequence:

1. Pink imaging window frame — draws itself, one line at a time.
2. Blue cell scales/draws in inside the frame.
3. Migration track curves out from the cell.
4. "Cecelia" wordmark appears.
5. Orange + chartreuse cells + their tracks arrive above the wordmark.

Total budget depends on the variant's timing profile (1.2s / 1.8s / 2.2s).

## Cross-consumer note

Feijoa is consumed by cecelia via a local `file:` dep at `/sketchbook`. Any
change to `SketchAct` / `SketchDefinition` must be **additive** and
**backwards-compatible** — the 13 existing sketches must still render, and
`import { SketchCanvas, sketches } from 'feijoa'` must still typecheck on
the cecelia side. Run `npm run typecheck` in feijoa before declaring done.

## Deliverable

- 4 named logo variants (`logo_gesture`, `logo_deliberate`, `logo_confident`,
  `logo_editorial` or similar) — each a `SketchDefinition` in
  `src/sketches/`, each with a top-of-file comment stating what it's
  testing in one line.
- A "Logo variants" view in the app that renders all four side by side
  with a replay-all control.
- Paper filter removed from the renderer.
- Fonts loaded in `index.html`.
- Optional `timing` hint on `SketchAct`.
- Typecheck clean.

D picks one variant. Then iterate.
