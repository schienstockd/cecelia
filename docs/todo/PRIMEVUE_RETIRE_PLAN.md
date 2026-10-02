# Retire PrimeVue

**Status:** planning (2026-10-02). P0 done (majors ignored in `.github/dependabot.yml`).

## Goal

Get the frontend off PrimeVue, so the bundle doesn't depend on a library whose next major is
proprietary. The end state is no `primevue` and no `@primeuix/*` in `frontend/package.json`, with the
tooltip and toast as our own components behind the **same call-site API**. The 1,200+ `v-tooltip`
call sites and the `useToast()` callers then don't change.

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
| Icons (`primeicons`, `pi pi-*`) | 153 files | 1,388 uses, about 300 distinct icons. **Separate package** (see Decision 2) |

Modals and dialogs are already hand-rolled (`BaseModal.vue`). No other PrimeVue component is in use.

## Decisions

1. **Same API, own implementation.** `app.directive('tooltip', …)` keeps the name, the four placement
   modifiers, the string value and the object form (`value`, `escape`, `class`). `useToast()` keeps
   `add({ severity, summary, detail, life })`. Call sites stay untouched, which is the only reason
   this is a small job and not a 143-file sweep.
2. **Icons stay: `primeicons` 7.x, MIT, frozen.** The icon font is a standalone CSS + font package
   with no PrimeVue runtime dependency. Swapping it means remapping about 300 icons across 153 files
   and changing how everything looks, for no gain. 7.x stays installable, and Dependabot ignores the
   major. **If 7.x ever becomes unavailable**, vendor its MIT font + CSS (+ LICENSE) into
   `frontend/src/assets/vendor/primeicons/` with the class names unchanged. Rejected: Lucide/Tabler
   (ISC/MIT) as a replacement, because of the full remap and the visual change.
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

- **P0, done.** `.github/dependabot.yml` ignores semver-major for `primevue`, `@primeuix/*` and
  `primeicons`. #1340 closed.
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

## Risks

- **Tooltip behaviour is a surface the user touches about 1,200 times.** Unit tests can't confirm
  it. P2 is gated on a visual pass, not on CI.
- **Aura may be styling more than tooltip and toast** (resets, focus, scrollbars). P3 starts by
  diffing computed styles with the preset on and off on a few pages.
