# Prompt 3: Pastel Sketch Animation Engine

Opus planning pass first, then Sonnet execution. This is the hardest prompt — it defines a new creative framework that the other two prompts depend on. Read `docs/UI.md` (design tokens, Observable Plot section), `INVENTORY.md`, `docs/PLOTS.md`, and the outputs of Prompt 1 (`WhatNewCard.sketchAnimation`) and Prompt 2 (`StatsResult`) before designing anything.

---

## The vision

A sketch animation system that draws scientific concepts and data-driven explanations in a hand-drawn pastel style — as if someone is sketching while explaining to you. Think: a researcher at a whiteboard, except the whiteboard is clean, soft, and animated. Not a presentation tool. Not a screensaver. A scientific communication medium.

The north star: you have analysed your data, made observations, and now you need to explain "cells in treatment A arrest more than treatment B" to someone in 10 seconds. The sketch engine takes your actual data (`StatsResult`) and a concept definition and produces an animated sketch that shows the claim with its evidence.

The first release is **pineapple** — fresh, crisp, light. The aesthetic should reflect that. Pastels. Clean lines that wobble slightly. Nothing heavy or corporate.

---

## Technology decisions (Opus to confirm or challenge)

**Rough.js** — hand-drawn SVG primitives. Lines wobble. Circles look drawn. Fills are light crosshatch or solid pastel. This is the aesthetic foundation.

**GSAP** — animation timeline sequencing. Elements draw in, pause, continue. The "someone is sketching this" effect comes from GSAP sequencing Rough.js path draws.

**D3** — data binding. When `StatsResult` drives a sketch, D3 maps data to positions. The bars in a sketch bar chart are Rough.js rectangles positioned by D3 scales.

**Vue component wrapper** — `SketchCanvas.vue` wraps all three. Takes a `SketchDefinition` object and plays it. Self-contained, no external dependencies beyond the three libraries.

Opus: challenge this stack if there's a better option. Specifically: is there a more maintained SVG animation library than GSAP for this use case? Is Rough.js still actively maintained? Any Vue 3-native alternative that avoids the GSAP license cost (GSAP Club is paid for some features)?

---

## The `SketchDefinition` format

A sketch is defined as a sequence of `SketchAct`s — each act adds something to the canvas. The definition is data (JSON-serialisable), not code.

```typescript
interface SketchDefinition {
  id: string
  title: string
  duration: number          // total seconds
  palette: SketchPalette    // colour scheme
  acts: SketchAct[]
}

type SketchAct =
  | DrawLineAct             // draw a line or arrow
  | DrawShapeAct            // circle, rect, ellipse (Rough.js)
  | DrawTextAct             // handwritten-style label
  | DrawDataBarAct          // a bar driven by a data value
  | DrawBracketAct          // significance bracket (from StatsResult)
  | DrawCellAct             // a simplified cell icon (circle + nucleus)
  | PauseAct                // hold current state
  | WipeAct                 // clear and start next scene

interface SketchPalette {
  background: string        // e.g. "#fafaf8" (near-white)
  stroke: string            // line colour
  fills: string[]           // pastel fill colours, one per data group
  accent: string            // highlight colour
  text: string              // label colour
}
```

**The pineapple palette** (first release):
- Background: `#fafaf8` (warm white)
- Stroke: `#4a4a4a` (soft charcoal, not black)
- Fills: `["#a8e6cf", "#ffd3b6", "#ff8b94", "#a8d8ea", "#fddb3a"]` (soft pastels)
- Accent: `#6c63ff` (soft violet, matches `--cc-accent`)
- Text: `#4a4a4a`

---

## `SketchCanvas.vue` component

```vue
<SketchCanvas
  :definition="mySketch"
  :autoplay="true"
  :loop="false"
  :stats="statsResult"        // optional: drives data-bound acts
  width="400"
  height="220"
/>
```

- Renders to an inline SVG
- `autoplay`: starts animation on mount
- `loop`: replays after finish
- `stats`: optional `StatsResult` — if present, `DrawDataBarAct` and `DrawBracketAct` use real values
- Exposes `play()`, `pause()`, `reset()` for manual control
- Respects `prefers-reduced-motion` — if set, shows final frame only, no animation

---

## What slots into Prompt 1 (WhatNewCard)

`WhatNewCard.sketchAnimation` is a `SketchDefinition`. The card renders `<SketchCanvas>` instead of the grey placeholder box from Prompt 1. No changes to the card API — the slot was designed for this.

---

## What slots into Prompt 2 (StatsResult)

`DrawBracketAct` and `DrawDataBarAct` accept a `StatsResult`. A data-driven sketch showing "WT vs KO cell speed" renders Rough.js bars at the correct relative heights, bracket at the top, significance stars. The bars look drawn, not printed. Real data, sketch aesthetic.

---

## The logo

The Cecelia logo in sketch style. Old logo: two cells emerging from an imaging window, "cecelia" text beside it.

New logo sketch definition:
- Two cells (circles with smaller inner circles for nuclei), drawn one at a time in pastel
- A soft rectangular frame (the "imaging window") drawn around them, slightly wobbly
- "cecelia" text in a clean but slightly informal font (not handwritten, but not corporate either)
- The pineapple palette
- Animation: frame draws first, then cells appear inside it, then text

This is a `SketchDefinition` JSON — not a static SVG. It can be animated on the splash screen / setup wizard, or shown statically as `loop=false` `autoplay=false` after the first play.

---

## The sketchbook page

A new route `/sketchbook` — a canvas page (same shell as Analysis board) where sketch animations live as panels. Three initial sketches:

1. **How HMM behaviour works** — cells moving, arrows showing state transitions (Arrested, Directed, Meandering), state bars appearing below
2. **How gating works** — scatter plot sketch, polygon gate drawing in, cells inside highlighted
3. **Cell tracking** — a cell path across frames, track drawn frame by frame

Each is a `SketchDefinition` in `lib/sketches.ts`. The sketchbook route shows them in a grid of `SketchCanvas` panels — same grid mechanism as Analysis board, same `FloatingPanel` infrastructure.

---

## Framework rules (Opus must include in the Sonnet prompt)

1. **One sketch engine** — `SketchCanvas.vue` is the only renderer. Never render Rough.js or GSAP outside it.
2. **Definitions are data** — `SketchDefinition` is JSON-serialisable. No code in definitions. This means sketches can eventually be authored in the UI or generated by Claude.
3. **Real data when available** — a sketch with `stats` prop shows real values. Never hardcode sample data in a definition that's used with real data.
4. **Palette is a first-class parameter** — never hardcode colours. Every colour reference goes through `SketchPalette`. The pineapple palette is the default but future releases can have different palettes.
5. **Respect motion preferences** — always check `prefers-reduced-motion`.
6. **Add to `INVENTORY.md`** — `SketchCanvas`, `SketchDefinition`, `lib/sketches.ts` must be documented.

---

## Build order

1. Rough.js + GSAP integration prototype — draw one shape, animate it, confirm the stack works
2. `SketchCanvas.vue` with 4-5 act types (line, shape, text, pause, wipe)
3. Pineapple palette + logo definition
4. `DrawDataBarAct` + `DrawBracketAct` connected to `StatsResult` (Prompt 2 integration)
5. `WhatNewCard.sketchAnimation` slot connected (Prompt 1 integration)
6. Three initial concept sketches for the sketchbook
7. Sketchbook page (`/sketchbook` route)

---

## Verify

- Logo renders in sketch style, plays animation, loops if set
- A tip card in What's New shows a sketch animation instead of grey placeholder
- A summary plot with stats → sketch shows data bars at correct relative heights + bracket with p-value
- Sketchbook page shows three concept sketches in panels
- `prefers-reduced-motion` shows final frame only
- `SketchDefinition` is valid JSON (serialise/deserialise round-trip)
- All three prompts' outputs work together: tip card → sketch animation → driven by real stats data

---

## Out of scope

- User-authored sketch definitions (later — UI for building SketchDefinitions)
- Export to video/GIF (later)
- Claude-generated sketch definitions (Phase 3 observer — but the JSON format is ready for it)
- 3D or particle effects (keep it flat and simple)
