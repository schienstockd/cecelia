# Surface the guides compass from the welcome page

> **Outcome: #533.** Two of this brief's premises turned out to be wrong, so read it as what was
> asked, not as what to build. **There is no welcome page** — `/` redirects to `/manage-images`, and
> the What's New dialog is the de-facto welcome surface — and the **first-time ring on the compass was
> rejected** in favour of an orientation tour that answers the same question directly. The design that
> shipped is `docs/todo/GUIDE_SYSTEM_PLAN.md` → D11 and `docs/UI.md` → *The orientation tour*.

PR #525 added the compass button (guide picker) beside the brand mark in the
header, but there's no path to it from the welcome page. A first-time user
lands on welcome with no signal that guides exist or where to find them.

Add a way in, without inventing a second guides entry point:

1. Find the welcome page component (whatever renders on first launch / empty
   project state) and add a direct link/CTA to the guide picker — e.g. "New
   here? Take a guided tour" — that opens the same dialog the compass button
   opens (reuse the existing open-picker action/store call, don't duplicate
   dialog logic).

2. Separately, add a first-time highlight ring on the compass button itself
   (the existing `--cc-guide` ring styling from PR #525, likely reusable via
   `GuideBubble`/`anchorPosition.ts` machinery) so the header icon draws the
   eye even for someone who skips the welcome CTA. Needs a "seen it" flag
   (localStorage or a store field) so the ring shows once and clears — check
   for an existing "first-time UI hint" pattern in the codebase (e.g. What's
   New tips) before rolling a new one.

3. Decide interaction: does clicking the welcome CTA open the picker
   immediately, or just ring the compass and let the user click it? Prefer
   opening the picker directly for the CTA (explicit ask), ring-only for the
   passive header hint (don't steal focus on every load).

4. Respect the "no demo data" design constraint from #525 — on an empty
   project only the import guide is useful, so the welcome CTA should
   probably open the picker filtered/scrolled to the import guide, or the
   picker's own prerequisite messaging already handles this (check before
   adding new filtering logic).

5. Add/extend `guides.test.ts` coverage: anchor id exists for the new welcome
   CTA, and the first-time ring state transitions correctly (shows once,
   clears on dismiss/first open).

6. Update `docs/UI.md` → Guides section to note the welcome-page entry point
   and the first-time ring behavior.

Report back: where the welcome page component lives, what "first-time hint"
precedent (if any) existed to reuse, and the final interaction decision for
each of the two entry points.
