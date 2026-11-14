> **ARCHIVED — not authoritative, do not act on this.** A frozen record of a review (a chat with
> Sonnet, 2026-11-13) of the three draft booklets in
> `~/Downloads/TMP/guide-screenshots-trials/booklet-v1/`. It is not a description of how the code works
> now, and not instructions to re-run. The current plan lives in
> [`docs/todo/GUIDE_SCREENSHOTS_PLAN.md`](../todo/GUIDE_SCREENSHOTS_PLAN.md).
>
> **Outcome:** not acted on as of archiving (2026-11-13, `origin/main` ea98917e7). Read against main:
>
> 1. **The PhenoCycler grid is an app bug, not a capture bug.** The plan already traced it:
>    `chName` in `frontend/src/utils/acquisitionChannelTable.ts` looks up `names[order - 1]` in the
>    stacked name list, and a registered image's channels restart `order` at 1 per cycle. Not yet fixed
>    or filed.
> 2. **The drafts are throwaway scripts** (no pipeline code on main), so §7–§8 are input to the
>    pipeline phases P1–P4 of the plan, not fixes to existing code.
> 3. **The wrong-panel steps, end-state captures and baked-in relative times** are rules the plan's
>    "one fresh context per shot" did not cover: a step must also set its task function and capture the
>    before-state.
> 4. **PhenoCycler stopping at viewing was deliberate** (written for microscope users, plan
>    *Draft booklets*); §4's "arbitrary depth" applies to it only once an analysis booklet exists.
> 5. **Counts and colour judgements are from rendered pages** and were not re-checked (e.g. nine vs
>    ten sections).

# Cecelia booklets: reservations

Reviewed: Intravital two-photon, Confocal (static), PhenoCycler (multiplex).
These drafts are tests. Sections 1-7 are the specific findings; **section 8 generalises them into guidelines for all future booklets**, and each guideline lists the failures it comes from.

Based only on what is visible in the PDFs. Counts and colour judgements are from the rendered pages, so verify them against the app.

## 1. Must fix before shipping

**PhenoCycler step 3: the acquisition grid looks wrong.**
- Cycles 2 and 3 show the same markers as cycle 1 (DAPI (cycle 1), IL-10, IL1-B, NOD2). Cycle 3 shows NOD2 at 150 ms in the CY5 column, while cycle 1 has NOD2 at 385 ms in AF750.
- Step 2's table says cycle 2 is DAPI/NOS2/B-actin/TNFa and cycle 3 is DAPI/CCL2/REG3A.
- Either the grid has an app bug (every row reads cycle 1's metadata) or the screenshot is wrong. This is the step that claims to show "which marker was imaged in which cycle", so it can't ship as is.

**Segment step shows the wrong panel (confocal step 3, intravital step 5).**
- Text says "pick the method (Cellpose) and the channels that outline your cells". The screenshot is the **Measure labels** function. No segmentation method or channel choice is shown anywhere.
- Likely cause: the capture script leaves the tab on its last-used function. Same class of bug as intravital step 4 (Smoothing shown, driftCorrected claimed) and step 7 (Track measures shown, "run tracking" claimed).
- Fix in the spec: each step must set the function explicitly before capture, and the build should assert the function name matches the caption.

**Callouts cover the values the text cites.**
- "Add images" tooltip hides the Z value (confocal), the cycle-1 channel count (PhenoCycler, whose caption cites cycle 1's channels) and the first row's duration (intravital).
- Systematic, so fix in the callout placement rules (never over table cells).

**Captions claim things the screenshot doesn't show.**
- PhenoCycler step 2: text says "Run on the set", button says "Run on 1 image".
- Confocal step 4: caption promises "cell count and share", but the Populations panel is cropped before the numbers.
- PhenoCycler step 4: caption says "ten tissue sections". I count nine on the page. Check.
- Confocal header says "Preview: two steps so far" but five steps are listed and shown.

## 2. Screenshots captured from the end state

- PhenoCycler step 1 already contains the `_registered` image and the "EDIT Register images" tags. Confocal step 1 already shows a SEGMENT tag on the image.
- Readers see the result before doing the step. Capture each step from the state *before* it, or crop to exclude later outputs.
- Relative-time strings are baked into pixels ("finished … · 77d", "· 12h"). They will be wrong on every regeneration. Crop them out.

## 3. Readability

- Tables in confocal step 2, PhenoCycler step 2 and intravital steps 9 and 10 are unreadable at print size. Crop to the one column or row the caption is about.
- Confocal 3b: thousands of outlines, no zoom. The caption says "look for missed cells and merged clumps", which is impossible at this scale. Use a zoomed crop with one example of each failure.
- Confocal step 2: the channel text box shows only five of six names (B220-PB scrolled out), and the callout arrow overlaps the NEXT panel.
- PhenoCycler step 4: faint tile-grid artefacts around the sections look like a defect. Either explain or choose a display range that hides them.
- PhenoCycler step 5: nuclei are pale pink, so "white where aligned, coloured fringes where shifted" can't be judged. The zoom is too low, and red + cyan + orange doesn't sum to white anyway. Use R/G/B-complementary colours, zoom further, and add a deliberately misaligned example for contrast.
- PhenoCycler step 6: IL-10 and CCL2 are both green/teal and the image is dominated by one hue. Pick distinct colours; show fewer markers per image.

## 4. Consistency across booklets

- "Get images in" / "Get the image in" / "Bring in each cycle", with different crops, tooltips and table columns. Make import, channel naming and QC shared modules, written once.
- Numbering is hand-typed and inconsistent: 5b, 3b, missing step 8. Auto-number from the spec; use sub-steps deliberately or not at all.
- Each booklet stops at a different, arbitrary depth. Define what "end of workflow" means for each. PhenoCycler ends at viewing, with no segmentation, gating or quantification, which is the interesting part for multiplex.
- Confocal's NEXT panel suggests "Multiplex tissue (38 channels)", which implies a cross-booklet path. State it in the booklet.

## 5. Content gaps

- **Jargon unexplained:** P14, CTDR, logicle, MSD, Fürth, HMM, "outline 2", "qc", "alignment channel", "2D affine (MIP)", "Keep each cycle's alignment channel".
- **Why it matters:** PhenoCycler's "keep alignment channel" setting is the reason there are three DAPIs, and a reader will want to know which one to segment on later.
- **Experimental features** (Smoothing · Experimental, Register images · Exp…) are shown as defaults. Say so in the caption, or decide a policy for documenting them.
- **No guidance when it goes wrong:** only happy paths. Add a short "if you see X, do Y" per step (the intravital QC step flags six images and never says what to do).
- **Gating examples are weak:** intravital's gate sits largely outside the density cloud, and it's the showpiece. Confocal's gate is a reasonable example but has no rationale for the axes.
- **Biology not stated:** PhenoCycler's tissue is never named (the crypts suggest gut). Confocal says lymph node.

## 6. Fixture hygiene

- Other images in the set (Human_Lymph_Node_*, Human_Spleen_*, M1d with "failed drift correction") are visible in confocal and intravital screenshots.
- Internal names leak: project names ("PhenoCycler 0064520 cycle registration", "Spatial test"), sample IDs, "(the earlier tutorial's sample)", `diffmap.test` suffix, the Kiwi, Screenshots and "Board 5" tabs.
- Use one clean fixture project per booklet containing only the images the booklet uses.

## 7. Process

- Add a build check that fails when: a selector is missing, the active function doesn't match the caption, a caption number (channels, sections, cell count) doesn't match the app state, or a tooltip overlaps a table cell.
- Stamp each booklet with the app version and capture date, since screenshots will go stale.
- Keep captions hand-written and the spec declarative. Only the screenshots are generated.
- Pick an audience before the next draft (new lab member vs. expert in another modality). It decides how much of section 5 gets written.

## 8. General guidelines going forward

Every specific issue above is an instance of one of these failure classes. Fix the class, not the instance.

### Single source of truth
1. **One spec per step drives everything.** Caption, app state, target selector, crop region and assertions all come from one declarative entry. Nothing is typed twice.
   *From:* Segment step showing the wrong panel; "Run on the set" vs "Run on 1 image"; "Preview: two steps" header.
2. **Every checkable claim in a caption is asserted at build time.** Function name, button label, channel and cell counts, population names, section counts. If the app disagrees with the caption, the build fails.
   *From:* ten vs nine sections; missing cell count and share; 11 channels.
3. **Numbering, headers and cross-references are generated**, never hand-typed.
   *From:* 5b, 3b, missing step 8, stale preview note.

### Capture
4. **Set state explicitly; never inherit it.** Each step declares its preconditions (tab, function, selection, image version) and the script puts the app there before capture. Capture from the state *before* the action unless the step is about the result.
   *From:* Measure labels panel under "Segment"; registered image and SEGMENT tag already visible in the import step.
5. **Capture only what the caption is about.** Crop to the relevant control or region. Nothing else from the app should be visible.
   *From:* other images in the set; Kiwi, Screenshots and "Board 5" tabs; tiny full tables.
6. **No volatile content in pixels.** Timestamps, relative times, job banners, random IDs, internal project names. Crop them out or mask them.
   *From:* "77d", "12h", sample IDs, `diffmap.test`.
7. **Callouts never occlude data**, especially values the caption cites. Placement is a rule in the renderer, not a per-shot choice.
   *From:* "Add images" tooltip covering Z, Ch and Duration.
8. **Legible at print size.** Set a minimum rendered text height. If a crop fails it, crop tighter or zoom the app.
   *From:* step 2 tables, intravital steps 9 and 10, confocal 3b.

### Fixtures
9. **One purpose-built fixture per booklet.** Minimal, deterministic, versioned, containing only what the booklet uses.
10. **The fixture must make the step look right.** A gate on the cloud, an alignment that is visibly good, a segmentation at a zoom where outlines can be judged. A showpiece that looks wrong teaches the wrong thing.
    *From:* intravital gate off the density cloud; confocal 3b at unreadable density.
11. **Pick display settings that discriminate.** Distinct channel colours, complementary where overlap matters, zoom high enough to see the property the step checks.
    *From:* PhenoCycler steps 5 and 6.

### Content
12. **Define the audience and the endpoint per booklet** before writing. Terms they won't know get defined at first use or avoided.
    *From:* P14, CTDR, logicle, MSD, HMM, "outline 2".
13. **Say why, not just what.** Each non-obvious parameter gets one "choose this when" line.
    *From:* alignment channel, "keep each cycle's alignment channel", 2D vs 3D affine, Smoothing sigma.
14. **Show how to tell success from failure**, ideally with one bad example, plus a short "if it goes wrong" note. QC flags need an action.
    *From:* alignment check with no misaligned contrast; six QC flags with no guidance.
15. **Flag experimental and non-default features** in the caption, or decide a policy for not documenting them yet.
16. **Shared steps are modules, written once** (import, channel naming, QC, viewer basics) and parameterised per modality.
    *From:* three different "get images in" steps.

### Feedback loop to the app
17. **Treat capture anomalies as app QA.** When a screenshot disagrees with the data (PhenoCycler acquisition grid), file an app bug. Don't edit the booklet to hide it, and don't ship until it's resolved or the step is removed.
18. **Separate three defect types when triaging:** app bug, capture/spec bug, writing bug. Different owners and fixes.

### Lifecycle
19. **Stamp every booklet** with app version and capture date.
20. **Regenerate on every release** and diff the output. A changed screenshot needs a human glance; an unchanged one doesn't.
21. **Human review pass on the rendered PDF**, using the checklist below. Machines catch mismatches; people catch "this makes no sense to a newcomer".

### Pre-ship checklist
- [ ] Every caption claim is asserted in the build
- [ ] Each screenshot shows only the target, legible at print size
- [ ] No timestamps, internal names or unrelated images visible
- [ ] No callout covers a cited value
- [ ] State captured is the *before* state (or the result, if that is the point)
- [ ] Fixture result looks correct and discriminating
- [ ] Terms defined; why-lines present for non-obvious parameters
- [ ] Failure case shown or described for each check step
- [ ] Shared modules used, not copied
- [ ] Version and date stamped; any app bugs found are filed
