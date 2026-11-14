# Screenshots for the user docs

How the screenshots on the `user-docs/` pages are declared, captured and composed. A page that shows
screenshots is **generated** (e.g. `frontend/src/utils/usingAiDoc.ts` → `user-docs/using-ai.md`), so
text the app already shows is imported from its source, never retyped, and a test fails when the
committed page is stale. In-app wording follows [`docs/ui/COPY.md`](../../docs/ui/COPY.md) → *House
style*. The engine's history: [`docs/todo/GUIDE_SCREENSHOTS_PLAN.md`](../../docs/todo/GUIDE_SCREENSHOTS_PLAN.md).

## The pieces

- **`spec.ts`**: `Shot` (how to reach the state: profile, project, image, route, user actions, or the
  pop-out viewer; what to point at, crop to and mask; which labels it must show), `Fixture` (what the
  instance needs before any shot) and `ShotSet` (one page's shots and their fixture).
- **`index.ts`**: every shot set. One per page, e.g. `using-ai.ts`.
- **`figure.ts`**: where a figure crops and where its pointer goes (pure, tested).
- **`scripts/docshots/`**: `fixture.mjs` → `capture.mjs` → `compose.mjs`.

## Making them

Against an **isolated** instance (own `CECELIA_DEV_DIR` and `CECELIA_PORT_SLOT`; the scripts refuse
8080/5173; setting one up is in `capture.mjs`'s header):

```
pixi run doc-fixture -- --base <url> --config-dir <dir> --source-projects <lab projects dir>
pixi run doc-shots   -- --base <url> --config-dir <dir>
pixi run doc-images
npx vitest run -u src/utils/usingAiDoc.test.ts      # in frontend/: the page picks up the new widths
```

`doc-fixture` sets up what a set's `fixture` names: lab images copied into the instance (never public
data, never the source written; `results` also brings segmentation and cell tables), channel names,
gates recreated through the app (a person's gate, not a ✦ one), profiles with AI on or off, and one
saved Kiwi reply. A shot names those by key and runs as its `profile`. `doc-images` writes
`user-docs/images/<id>.png` at 2× and their widths to `images.json`.

The pointer's wording and side live in the spec and apply at compose time; a pointer with no label is
the ring alone, for a narrow panel. The crop and the pointer are drawn from the recorded boxes, so
restyling never means recapturing.

Anchors are the guides' `data-guide` ids, so a shot and an in-app guide name the same control. A control
with no anchor gets one in the app (same change), not a CSS path.

## Rules

From the review of the first drafts ([`docs/archive/BOOKLET_RESERVATIONS.md`](../../docs/archive/BOOKLET_RESERVATIONS.md)
§8; read its banner first). Capture enforces the ones marked ✓:

- ✓ every anchor a shot names is there and visible;
- ✓ every label the page cites (`expect`) is in its anchor's text;
- ✓ no relative times or dates in the crop (mask them, or crop them out);
- ✓ a viewer shot's state (image, plane, channels, outlines, gated) is read back after it is set;
- ✓ compose warns when the spec changed a shot's crop or pointer target since its capture;
- set the page, the task function and the image explicitly, never inherited;
- capture the state *before* the action unless the result is the point;
- crop to what the text is about, tight enough to read in the page column (compose warns);
- no ids, internal names or unrelated images in the pixels;
- callouts never cover a value the text cites;
- AI output (a Kiwi reply) varies between runs: capture it from a **saved** entry on a fixed question,
  never a live turn.
- **Numbers come from the fixture, never the keyboard.** An `expect` may cite `{{project.image.planes}}`,
  `.channels`, `.timepoints`, `.interval`, `.<gate>.count` / `.pct` or `.<segmentation>.tracks`;
  `doc-fixture` measures them on the copy through the app and capture checks the pixels show the same.
