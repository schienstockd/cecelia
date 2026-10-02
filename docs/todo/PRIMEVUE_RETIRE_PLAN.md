# Retire PrimeVue

**Status:** planning (2026-10-02). P0 done (exact pins + Dependabot ignore).

## Goal

Get the frontend off PrimeVue, so the bundle doesn't depend on a library whose next major is
proprietary. The end state is no `primevue` and no `@primeuix/*` in `frontend/package.json`, with the
tooltip and toast as our own components behind the **same call-site API**. The 1,200+ `v-tooltip`
call sites and the `useToast()` callers then don't change. Icons move to a community-maintained
open-source set (Lucide), so no single owner can relicense them.

## Why

PrimeVue 4.x, `@primeuix/themes` 2.x and `primeicons` 7.x are **MIT**. Their next majors (PrimeVue 5,
themes 3, primeicons 8, plus the new `@primeui/license-manager` dependency) ship under the **PrimeUI
License**:

- compiled-only, with no reverse-engineering;
- a licence key is required, and a missing key "may cause the software to display a license notice";
- a free Community tier (non-profits qualify) that needs annual renewal;
- an OEM licence for redistribution that lets third parties develop with it.

Cecelia is GPL-3-or-later, ships as an installable bundle, and users extend it with plugins. A
key-gated proprietary UI library inside that bundle is a licence conflict and an install-time
liability. Staying on 4.x forever is safe for now, but it freezes us on a line that gets no fixes.
It also already blocks us: `@primeuix/themes` 3 needs `@primeuix/styled` 1.x, while PrimeVue 4.5
uses 0.7.x (Dependabot PR #1340, closed).

## Footprint (measured 2026-10-02, `frontend/src`)

| PrimeVue piece | Where | Size |
|---|---|---|
| `v-tooltip` directive (`primevue/tooltip`) | 143 files | 1,206 uses: `.bottom` 505, `.top` 379, `.left` 132, `.right` 124. One object-form site (`ImageTable.vue` QC badge: `escape: false` + `class: 'qc-tip'`) |
| Toast (`primevue/toast`, `toastservice`, `usetoast`) | `App.vue` + 4 `useToast()` importers | 12 `add()` calls, severities `success` and `error` only |
| Theme (`@primeuix/themes/aura`, `PrimeVue` config) | `main.ts` | `darkModeSelector: '.cc-dark'`, `cssLayer` order `theme, base, primevue` |
| `.p-tooltip*` overrides | `style.css` (*PrimeVue tooltip overrides*), `utils/uiCopy.ts` placement rules | written against PrimeVue's `align()` / `isOutOfBounds` internals |
| `--p-*` tokens | `utils/cssTokens.ts` (vendor allow-list only) | none consumed by our CSS |
| Icons (`primeicons`, `pi pi-*`) | 153 files | 1,388 class uses + 268 `icon: 'pi-…'` data entries in 37 files; about 300 distinct glyphs, all catalogued in `lib/iconLegend.ts`. No `` `pi-${…}` `` templates, but a few built-up prefixes (`pi-caret-` + dir, `pi-sort-amount-`, `pi-chevron-circle-`). Every glyph used exists in 7.0.0. **Separate package** (see Decision 2) |

Modals and dialogs are already hand-rolled (`BaseModal.vue`). No other PrimeVue component is in use.

## Decisions

1. **Same API, own implementation.** `app.directive('tooltip', …)` keeps the name, the four placement
   modifiers, the string value and the object form (`value`, `escape`, `class`). `useToast()` keeps
   `add({ severity, summary, detail, life })`. Call sites stay untouched, which is the only reason
   this is a small job and not a 143-file sweep.
2. **Icons move to Lucide (ISC), behind one `CcIcon` component.** The MIT grant on `primeicons` 7.0.0
   can't be revoked, so it is safe to keep meanwhile, but it is a frozen set of 314 glyphs from a
   single owner that has just relicensed everything after it. Lucide (like Tabler and Phosphor, both
   MIT) has thousands of icons and many contributors, which makes a relicence practically
   impossible. That is the long-term protection. Lucide is the largest of the three, closest to
   primeicons' outline style, and has first-class Vue support. The swap is mechanical because
   **`lib/iconLegend.ts` is already the catalogue**. Every rendered glyph is listed by meaning, and
   `iconLegend.test.ts` fails on any glyph missing from it. So the job is "pick the Lucide glyph for
   each meaning", and the test catches leftovers. `CcIcon` takes the name string, so the 268
   data entries keep a string, and spinner/`pi-fw` become props or classes. Rejected: vendoring 7
   permanently (it freezes the set and keeps single-owner provenance), and Iconify as a runtime
   dependency (an extra layer for one icon set).
3. **Tooltip positioning on `@floating-ui/dom` (MIT)**: `flip` + `shift` + `offset`, and `arrow` for
   `.p-tooltip-arrow`'s equivalent. That replaces the PrimeVue placement quirks the comments in
   `style.css` / `uiCopy.ts` work around ("`.top`/`.bottom` are the only placements PrimeVue clamps
   horizontally", "`.left` falls through to top"). Re-audit those rules once it lands: some become
   obsolete, and the `docs/ui/COPY.md` placement guidance may relax.
4. **Keep the tooltip's DOM contract.** It is appended to `document.body`, so it inherits no `--cc-*`
   vars from the app root (see `docs/ui/PRIMITIVES.md` and the `style.css` header). Keep `body`-level
   token declarations and the "size the root, never the text" rule. New class names are `cc-tooltip*`;
   the `.p-tooltip*` overrides move over in the same change.
5. **One notification system stays one.** The new `ToastHost` replaces `<Toast />` in `App.vue`, the
   one mount (`docs/inventory/FRONTEND.md` → *Toast*).
6. **A ratchet test ends the plan**: no `primevue` / `@primeuix` import under `frontend/src`, in the
   style of the existing convention tests.

## Phases

- **P0, done.** `primevue` 4.5.5, `@primeuix/themes` 2.0.3 and `primeicons` 7.0.0 are pinned
  **exactly** in `frontend/package.json`, because a caret would accept a patch released under the
  new licence. `.github/dependabot.yml` ignores all their updates. #1340 closed.
- **P1, Toast.** `components/ToastHost.vue` + `composables/useToast.ts` (same signature), on the
  traffic-light severity tokens. Swap the 5 importers. Vitest for the queue/expiry logic.
- **P2, Tooltip.** `directives/tooltip.ts` on `@floating-ui/dom`: show/hide delays, hide on scroll and
  on element unmount, the `escape: false` HTML path (`lib/qc.ts` already escapes every interpolation),
  and reactive value updates. Pure placement/option parsing goes in `utils/` with Vitest. **Needs
  Dominik's eyes before merging**: placement in dense panels (PopulationManager, the task panels,
  the ImageTable QC badge), dark mode, and the floating windows.
- **P3, remove.** Drop `primevue`, `@primeuix/themes`, the `PrimeVue` config + Aura preset and the
  `primevue` CSS layer. Check what Aura was still styling (base font, focus rings), add the ratchet
  test, and update `docs/inventory/FRONTEND.md`, `docs/ui/PRIMITIVES.md` and `docs/UI.md`.
- **P4, icons.** `components/CcIcon.vue` on `lucide-vue-next`, plus a mapping table (primeicons name →
  Lucide name) built from `ICON_LEGEND`. Then a codemod: `<i class="pi pi-X">` → `<CcIcon name="…">`,
  `icon: 'pi-X'` data → the new name, and the built-up prefixes by hand. `iconLegend.ts` switches to
  Lucide names, so the test keeps guarding. Drop `primeicons`, and update `docs/UI.md` → *Icons*.
  Independent of P1–P3, so it can go first. **Needs Dominik's eyes**: stroke weight vs neighbouring
  text, the busy spinner, and icon-button alignment in toolbars.

## Risks

- **Tooltip behaviour is a surface the user touches about 1,200 times.** Unit tests can't confirm
  it. P2 is gated on a visual pass, not on CI.
- **Icon swap changes the look everywhere.** Lucide's outline weight differs from primeicons, and
  `.cc-btn -icon` squares were sized against the font glyphs. One visual pass on the dense pages
  before merging.
- **Aura may be styling more than tooltip and toast** (resets, focus, scrollbars). P3 starts by
  diffing computed styles with the preset on and off on a few pages.
