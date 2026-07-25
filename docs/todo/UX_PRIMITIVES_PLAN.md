# UX primitives — unification tracker

**What this is.** The status of the frontend UX-primitive unification: which categories are held by a
machine check, which are still open, and which are deliberately left alone. It exists because "looks
shared but is re-declared per file, and drifts" (the [divergent re-implementation
trap](../../CLAUDE.md)) kept recurring across fresh context windows — a checkbox styled as a toggle in
one place and a native box in another; a `.btn-sm` that renders as a raw browser button on a page that
forgot to copy the CSS.

**The catalog is not here.** *What to use when* lives in one place — [`docs/UI.md`](../UI.md) → *UX
primitive catalog (CHECK BEFORE BUILDING)*. This file used to carry a second copy of that table and it
had already drifted from it in both directions, which is this document's own rule failing on itself:
**one canonical thing; the second copy is the bug.** Adding a primitive? UI.md + `INVENTORY.md`. Only
its *unification status* belongs here.

**This doc states no live counts.** Every number that was ever written here in prose turned out wrong —
"~80 remaining scenarios" (most muted text has no `hint`/`sub`/`val` in its class name), "~29 card
blocks" (mostly *controls*), "~15 chevron toggles" (six weren't sections), "48 ad-hoc spinners" (a
non-issue), and a font-size audit that counted `rem` and missed 149 `px` declarations of the same scale.
Counts live in the detectors below; a measurement quoted here is dated evidence for a past decision, not
a live figure. **Run `pixi run test-frontend` — the detectors are the authority.**

## The detectors — where every number in this area lives

Both modules sit in **`frontend/src/utils/`**, each with a `.test.ts` beside it. If you want a count,
read these; do not re-derive one by grep.

| Detector | In | Owns | Bar |
|---|---|---|---|
| `findReimplementedScenarios` | `cssScenarios.ts` | scoped rules re-declaring `.cc-muted`/`.cc-empty`/`.cc-eyebrow` | per-file `BASELINE` — **may shrink, must never grow** |
| `findScopedUtilityOverride` + `utilityRules` | `cssScenarios.ts` | a scoped rule whose selector *is* a global `.cc-*` utility, re-stating a property it declares | must be empty (no allow-list — composition is legal by construction) |
| `findHandRolledIconButtons` | `cssScenarios.ts` | an icon-only `<button>` not built from `.cc-btn` | exact list (2 full-height strip exemptions) |
| `findRawValues` | `cssScenarios.ts` | literal `font-size` / `border-radius`, incl. inline `style=` | exact list (1 documented exception) |
| `findRawColours` + `colourTokens` | `cssScenarios.ts` | a hex a token already holds exactly; any `var(--x, #hex)` fallback | must be empty |
| `findRestatedInputBase` + `inputBase` | `cssScenarios.ts` | form-control rules re-stating the global input base | exact list (2 survivors, each a class shared with a non-input element) |
| `findDeadTokenRefs` | `cssTokens.ts` | a reference to a `--*` property `style.css` never declares | must be empty |
| `findNonRootTokenDecls` | `cssTokens.ts` | the global scale declared anywhere but `:root` (declared ≠ *reachable*) | must be empty |

Shared plumbing at the top of `cssScenarios.ts`: `styleBlocks()` and `cssRules()` (which recurses into
`@media`). `SCENARIO_HINT` supplies the "migrate it to this" text the ratchet prints. Three things that
are easy to break:

- **`vite.config.ts` sets `test.css: true`.** Without it Vitest stubs `style.css` to `''` and both token
  guards pass vacuously — green, and checking nothing.
- **The exact-list checks fail on *improvement* too, deliberately.** An un-updated allow-list silently
  stops ratcheting, so fixing a site means lowering its number in the same change.
- **A detector that can't name the role must say so, not guess.** `.cc-readout` is `dim + size` *plus*
  `tabular-nums`, a strict superset of the `muted` matcher, so every hand-rolled numeric readout lands
  labelled `muted`. Nothing in the CSS separates them, so `SCENARIO_HINT` names both candidates rather
  than talking the next migrator out of the tabular figures.

## Open

- **The scenario backlog.** The live list is the per-file `BASELINE` in `cssScenarios.test.ts`; it may
  shrink and must never grow. Touch a file in it → migrate its rules and lower the number. Deliberately
  decoupled from "finish the sweep": new divergence fails immediately, the backlog drains as files get
  touched. The ≤2-rule tail has been drained (45 files → 20), so what's left is the concentrated
  remainder; the failure message names the offending selector and the utility it wants.
- **Per-row disclosure in a list** (`TaskList`, `ErrorConsole`) — distinct from a section header, and
  deliberately NOT extracted. Inspected, the two sites differ on **four** axes: `TaskList` uses a
  focusable `<button>` (already `.cc-btn-bare .cc-btn-icon`) with a tooltip, the icon as click target and
  multi-expand (a `Set`); `ErrorConsole` uses a non-focusable `<span>`, the whole row as click target and
  single-expand. Four differences from two samples — you discover an axis by watching sites differ on it,
  and at n=2 a real axis is indistinguishable from an accident of those two files. The cost is
  asymmetric: extract now, both adopt, then site three differs on the un-modelled axis and hand-rolls,
  leaving a primitive *and* divergence — strictly worse than divergence alone. Waiting costs two small
  scoped rules that no detector flags. There may also be nothing to extract: one is already the canonical
  icon button, the other isn't a button at all, and the only shared thing is "the chevron points up when
  open".
- **Not recommended as a sweep: a single-value range wrapper.** Verified — the global base is the whole
  treatment (`accent-color` + `cursor`) and every per-site rule is pure geometry (`width`, `flex`). There
  is nothing to extract, and some sliders commit on release rather than input.

## Deliberately bespoke — do not "fix" these without reading why

- `ImageTable`'s `.runlog-cog.on` / `.actions-btn.on` are the **hover-reveal** pattern (`opacity: 0` + a
  parent `:hover`), not the accent-border scenario.
- `GatePairsPanel`'s `.chan-btn.on` is a full-width select-*trigger* built from its own rules, not a
  `.cc-btn` — adopting the state axis means rebuilding the control first.
- `ModuleLayout`'s `.filter-toggle.active` sits between the outline and tint intensities and would change
  visually either way; it is a popover trigger, not a toggle.
- `HintCallout.vue` is a full component (icon + dismiss), not a text scenario.
- `ChainModule`'s richer empties (icons, multi-line CTAs) — adopt opportunistically, not swept.
- Accent-coloured readouts (`.pt-val` and friends) — deliberately accent rather than dim. Prominence, not
  density; `.cc-readout-strong` covers the common case.
- Per-site italic and geometry. `font-style: italic` marks provisional/advisory prose; it is emphasis,
  not a tier, and stays a one-line scoped rule (as do width/margin/flex/padding).
- `ChainPicnicNode`'s amber and `AnimationModule`'s `.tl-badge` are **identity** hues; the `.cc-del` /
  `.footer-btn .danger` / `.save-btn.danger` / `.opt-btn.danger` reds are **destructive-action** tones.
  Neither is a status, so both correctly stay on `--cc-warn`/`--cc-danger` rather than `--cc-sev-*` (the
  status-vs-not split is written up in `docs/UI.md`).

## Rules this exercise produced

The blow-by-blow is in git (`git log --oneline -- frontend/src/style.css`). What's worth carrying:

1. **Generalise by SCENARIO, not one component per widget.** A prominent pool-size count and a dim
   slider `°/s` readout look different but are one scenario at two prominences. The grain is a semantic
   utility with *one modifier axis*, not a `CcRange`/`CcEmptyState`/`CcCard` per widget.
2. **A utility that hard-codes a value on an axis where sites legitimately differ will not be adopted.**
   Every site written off as "bespoke" had this one cause. There were only three such axes — density,
   surface, layout — and giving each scenario its missing modifier turned the whole stalled list into
   mechanical migrations. **Name the modifier after the scale step it selects, not a relative amount**
   (`-xs`, not `-dense`) — a relative name can only express the steps you thought of, which is how
   `.cc-muted` ended up with no 11px step under the largest remaining cluster of hand-rolled text.
3. **A tier most sites override by hand is the wrong default.** Corollary of (2), and the more expensive
   half: the form-control base sat a step too large, so 33 controls across 24 files each hand-wrote the
   size they wanted while exactly one adopted the opt-in class. Count the overrides before adding
   another opt-in.
4. **When tokenising a value between two steps, check which side the element belongs on.** Rounding to
   the nearest step took the input base and the tooltip *up* into body size; both surfaced later as
   visible regressions. Dense chrome rounds down.
5. **Declared is not reachable, and a green guard can mean neither.** The token scale sat on
   `.cc-dark`, a `<div>` inside `<body>`, so PrimeVue's tooltip — which it appends to `document.body` —
   resolved none of it and rendered at the browser default 16px for months. `findDeadTokenRefs` was
   green and correct throughout: the tokens *were* declared. Two visible regressions were misdiagnosed
   as rounding before anyone checked the DOM. When a value refuses to change, verify the rule is
   *reaching* the element before tuning the value again — and note the sweep is what broke it, by
   turning a working literal into a token reference in a scope that has no tokens.

6. **A matcher pinned to one spelling has a blind spot, and they all look alike.** Four of them so far:
   reading only `<style>` blocks (inline `style="font-size:…"` invisible); keying on a *literal* size (so
   tokenising silently un-flagged a rule); keying on `var(--cc-text-dim)` and missing the
   `var(--cc-text-dim, #8b8ca7)` spelling; and `\b(select)\b` matching inside `.chip-select`, because a
   hyphen is a word boundary. When adding a matcher, ask which other spellings of the same declaration
   exist — token vs literal, with fallback vs without, scoped vs inline — and judge a rule by its
   **subject** compound, not any part of the selector.
7. **CSS cannot express intent, so prefer precision over recall.** A button, a card, a chip, a badge and
   an input are all `surface + 1px border + radius` — which is why the `card` matcher was dropped, and
   why `.cc-btn-icon`'s fixed square collapsed two full-height strip controls that were markup-identical
   to an icon button. A noisy check grows an allow-list, and the allow-list is where this rots. Where a
   detector can't be precise, the category gets a review-time rule in `docs/UI.md` and no number at all.
8. **A rule stated in one place while its neighbour keeps doing the old thing is the recurring tell.**
   The ratchet was first built with the largest category carved out of it; the "no counts" rule was added
   to one section while the table above it kept its stale counts; the size audit counted `rem` and
   ignored `px`. When you add a rule here, grep the whole file for what it forbids before claiming it
   holds.
