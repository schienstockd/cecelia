> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. No
> `docs/todo/*_PLAN.md` exists for it yet.
>
> **Outcome:** not run as of archiving (2026-11-15, `origin/main` c04f1bc34). Read against main, these
> premises had moved or need checking first:
> - **"Extends A1."** A1 is not a document any more. #2240 ended the booklets; A1 was meant to become an
>   in-app guide for the set's **Acquisition check**, but `acquisition-check` is still listed in
>   `frontend/src/lib/guides/unguided.ts` (no guide written; the PR that archives this adds it, `lib/guides/acquisitionCheck.ts`). The paper that goes to the scope is the
>   per-image acquisition card.
> - **The booklets ended on "no PDFs at all" (BOOKLETS_PLAN D9), one day before this brief.** Its reason —
>   a PDF only earns its place for a reader outside the app — is the trainee take-home case, so this brief
>   reopens that decision rather than building on it.
> - **"Under the existing image budget."** No user-docs image budget exists in code or docs; the copy
>   budgets in `docs/UI.md` / `guides.test.ts` are for text. `user-docs/images/` holds 5 images, 328 KB.
> - **9.4 "if lab images are committed".** They are not: `pixi run doc-fixture` copies images from a lab
>   projects dir on the capturing machine (`scripts/docshots/fixture.mjs`). The open question is
>   reproducibility outside the lab, not repo size.
> - **Already exists:** the workstation AI switch (`utils/aiSwitch.ts`), the Help menu (`HelpMenu.vue`,
>   Learn / Get help), plain Experimental wording (`lib/guides/tiers.ts`: "Not checked on realistic
>   data — check every result by hand"), recipe `why` + `optional` per step. Missing: per-step "what
>   good looks like" and "if it goes wrong".
> - **Facility knowledge base:** `ACQUISITION_ADVISOR_PLAN.md` keeps lab knowledge as repo files and
>   notes the Blackboard is per-project, so a workstation-level facility config would be new.

# Brief for Opus: training manuals (replacing the booklets)

Context: the booklet series was removed in PR #2240 (C1 became `user-docs/using-ai.md`; the screenshot engine stays as docshots). The booklets were started so that a **trainer has a path to follow**. That need remains, and it comes from the head of microscopy's plan to change how facility users are trained:

1. **Session 1:** a slide; how to turn the scope on and off; how to get an image.
2. **Session 2:** trainees bring their own samples and drive the scope with a trainer present. Goal: they set up the imaging settings properly.
3. **Session 3:** the trainer analyses with them in Cecelia, on the images from session 2. Goal: they understand what to do next.

Most Cecelia surfaces are currently Experimental, and we are **not** hiding them in training. We label them honestly instead.

This is a plan request. Check what exists, then report. Do not build the manuals yet.

## 1. Readers, and why that shapes the wording

| Reader | Uses it | Needs |
|---|---|---|
| **Trainer** (microscopy staff) | During the session | A path to follow: order, timings, what to say, what to check, what usually goes wrong, what to do if it breaks |
| **Trainee** | Takes it home | The same path, in words that work without the trainer: what to do, why, what good looks like, what to do if stuck |
| **Microscopy department** | Reviews before use | Something non-developers can read and mark up, and that is easy to keep correct when the app changes |

The trainee copy must not read like instructions to someone else, and the trainer copy must not be a different document that drifts.

**Proposal:** one source, two renders.
- **Core text, written to the trainee** ("you"), plain language, terms defined at first use. This is the take-home.
- **Trainer notes** in clearly marked blocks (talking point, timing, common stumbles, fallback). These appear only in the **trainer edition**, which is the take-home plus the notes.
- Both are generated from the same source, so the process text can never differ.

## 2. What the manuals are, and are not

- **Two manuals, by session, not by modality:**
  - **Session 2 manual (at the scope):** extends A1. The settings checklist, with variants per scope (LSM 780, LSM 980, FV4000), and what to save and where.
  - **Session 3 manual (analysis):** import their session-2 images, check metadata and scale, segment, check the outlines, gate, plot. Modality variants go in an appendix that points at the in-app recipes.
- **Not** a per-modality tutorial series, **not** a duplicate of the in-app Guides, **not** a showcase. That was the booklets' trap.
- **Process text comes from the recipes** (`lib/guides`, `data-guide` anchors): step titles, why-lines, optional flags, maturity. Do not retype it. The manual adds only: what good looks like, if it goes wrong, and the trainer layer.

## 3. Structure to evaluate

**Session 3 manual, roughly**
1. **Before the session** (trainer): import the trainees' images beforehand and check they load with correct scale, z-step and channel names. Keep a fallback project.
2. **Aim and exit criteria** (shared): what the trainee can do alone by the end.
3. **Run of show**, one block per recipe step: what you are doing and why; what to do; what good looks like; if it goes wrong. Trainer block: timing, talking point, usual stumbles.
4. **What the maturity badges mean** (shared): plain language. For Experimental: "may change or fail; check the result against the image."
5. **If something breaks** (trainer): fallback, and where to log it.
6. **What you take with you** (trainee): where your project lives, how to open the in-app guide again, what to try next, who to ask (set per facility, see section 5).
7. **After** (trainer): a place to log where trainees got stuck, to feed back into the app.

**Session 2 manual:** settings checklist per scope, saturation and dynamic range, pixel size and z-step, channel naming at acquisition, saving and naming files, and "check your image in Cecelia before you leave" if feasible. It should tie into the acquisition checks (Light path panel, source markers) rather than restate them.

## 4. Check before planning

- **Recipes:** do they hold why-lines, "choose this when", "if it goes wrong" and "what good looks like" today? What would adding the last one cost?
- **Docs pipeline:** how `user-docs/` is generated since #2240 (snapshot test, stamps), and whether a second render (trainer edition) is a small extension.
- **docshots:** can it produce the few images a manual needs, cropped by anchor? How many images would the two manuals need, and does that fit the image budget? Prefer fewer, smaller images.
- **A1:** what exists today, and how much of Session 2 it already covers.
- **Maturity strings:** where the badge text and tooltips come from, and whether the plain-language Experimental wording exists.
- **Import path:** how reliable import is today for files from the three scopes (metadata, scale, z-step, channel names), since session 3 depends on it. Flag known gaps, do not fix them here.
- **Fixtures:** is there a realistic trainee-like image (not a showcase image) for captures, and is it publishable in a public repo?
- **Drift protection:** which of these assertions exist for docs: control and label exist, cited numbers match, no placeholders in a release, version stamp. Which are missing?
- **Print:** can the same source render a clean print or PDF edition for take-home, without becoming a second system?

## 5. Facility-specific content

Contact names, local data paths, scope lists, booking and storage rules differ per facility. Proposal: keep these in a small **facility config** that the manuals read, not in the text. A release must fail if a required facility field is empty (no placeholders shipped). Check whether the acquisition advisor's "facility knowledge base" idea should share that config.

## 6. Guardrails (from the booklet drafts)

- One source drives text, screenshots and assertions. Nothing typed twice.
- Every checkable claim is asserted at build time.
- Capture from explicit state, cropped to the target, with no timestamps, internal names or other images visible.
- Stamp each edition with the app version and date.
- Stumbles and fallbacks come from **real cohorts**, not guesses. Ship a skeleton, run one cohort, then fill them in.
- Do not promise outcomes. State what the step does and how to check it.
- AI is off by default. Link to the C1 page and do not make the manual depend on AI.
- No repo bloat: images only where they teach something, under the existing budget.

## 7. Review with the microscopy department

They are the users of the trainer edition, so plan a review loop:
- A **review pack** they can read without installing anything (rendered page or PDF), with questions marked.
- A short **sign-off checklist**: wording a trainee will understand, steps match how sessions really run, timings are realistic, scope variants are right.
- A way to send comments back that doesn't require using GitHub.

**Questions to ask them (please draft the list; these are a start)**
- Which scopes and which sample types do most trainees use?
- Session length, group size and cohort size?
- What should a trainee be able to do alone after session 3?
- Which file formats do the scopes produce, and where are files saved?
- After session 3, can trainees run Cecelia themselves? Where (workstation, own laptop)?
- Who owns and updates the manual when the app changes?
- Preferred take-home format: print, PDF, online page?
- Local policies we must respect (data storage, naming, AI use)?
- What do trainers most often have to repeat, and where do trainees most often go wrong at the scope?

## 8. Report back with

- What exists and what is missing, per item in section 4.
- Whether one source with two renders (trainee and trainer edition) is feasible, and what it costs.
- A proposed outline for both manuals, using the real recipe steps.
- The changes needed in recipes, docs generation and docshots, with a suggested order.
- The assertion gaps to close first.
- A draft review pack plan and the question list for the microscopy department.
- Risks, and anything in this brief you disagree with.

## 9. Discussion points for Opus: where the manuals live and how they are delivered

These are positions to test, not decisions. Please agree, disagree or refine each one, with reasons based on the actual repo and app.

### 9.1 Help section for training materials
- Proposal: a **Training materials** section under Help, separate from the Guides modal. The Guides modal stays "what do I do next" and gets at most a footer link.
- Contents: trainee edition, trainer edition, and a facility setup page showing which required fields are still empty.
- The section ships as **Experimental** until a few facilities have used it.
- Question: does Help already have a natural place for this, and does a separate section cost more than it gains?

### 9.2 Facility config
- Proposal: contacts, data paths and scope list live in a **workstation-level** config (like the existing "Turned off for this workstation" AI switch), owned by whoever looks after the machine.
- A missing required field blocks print and save. No manual with blanks.
- Question: where would workstation-level settings live today, and can the config be shared with the acquisition advisor's facility knowledge base?

### 9.3 Source location and repo size
- Proposal: keep the **source in the main repo** (small markdown plus config). The step text comes from the recipes and the drift checks run in the main CI, so a separate repo would make those cross-repo and re-introduce the drift problem.
- **Do not commit generated outputs:** no PDFs, no PR captures, no capture output folders. Only the small docs images that pages actually display, under the image budget.
- A separate **facility template repo** (config and local notes only, building against a pinned Cecelia version) only if a second facility asks.
- Questions:
  - Can docshots run against a pinned app version from outside the repo? The template idea depends on it.
  - Is any text under a different licence from the code?

### 9.4 docshots and fixtures
- Proposal: docshots stays in the main repo. The engine is small text.
- The real risk is **fixture data**. How does `doc-fixture` obtain its images today? If lab images are committed, report the size. If they are large, propose hosting them as a hash-pinned release asset or one-off download, cached in CI, not stored in git.
- Also check: committed output churn (pixel-compare before rewriting), and that capture tooling (headless Chrome) is a dev dependency and not in the shipped package.

### 9.5 Delivery: on demand in the app, plus a generic build in CI
- **In the app (the main route):** render the manual from the source with the facility config and the running app version, then print or "Save as PDF" through a print stylesheet. Nothing stored, nothing committed, and the manual always matches the app in use.
- **In CI (the generic copy):** a PDF and HTML with facility fields clearly marked as placeholders, published to the docs site and as release assets, for facilities evaluating before install. It also checks that the print render still works on every release.
- Not committed to git. Please confirm that deploying from CI to the docs site and attaching release assets doesn't add to repo history.
- Questions:
  - Does the print stylesheet route give acceptable page breaks and image placement in the browsers and the desktop wrapper we support? Test it, and report what a PDF engine would add if not.
  - Is the trainee and trainer split a render flag on one source?

### 9.6 Images offline
- On-demand printing needs the manual's images at hand, possibly offline.
- Count the images the two manuals need and their total size. Options: bundle only those few, or cache on first open. Report whether the bundle cost is acceptable.

### 9.7 Report back on these
- For each point: agree, disagree or refine, with evidence from the repo and build.
- The current size of fixtures and docs images, and what the packaged build contains.
- A recommended order: what to settle before writing any manual text.
