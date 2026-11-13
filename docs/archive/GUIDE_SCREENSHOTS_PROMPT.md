> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in [`docs/todo/GUIDE_SCREENSHOTS_PLAN.md`](../todo/GUIDE_SCREENSHOTS_PLAN.md) (parked).
>
> **Outcome:** not run as of archiving (2026-11-12, `origin/main` 1ab1b9e3); the plan was written from
> this banner instead. Read against main, these premises had moved:
> - **A capture runner already exists.** `scripts/gui_capture.mjs` (`pixi run gui-capture`, #2084)
>   drives a running instance with `playwright-core` (a frontend devDependency) and a system Chromium,
>   opens a project through the app's store, visits routes through the router, closes launch pop-ups and
>   saves one PNG per page. It needs an isolated instance (own `CECELIA_DEV_DIR` + `CECELIA_PORT_SLOT`;
>   it refuses 8080/5173). Phase 0 step 1 and the "runner" slice extend this; they do not start from zero.
>   Missing from it: a per-shot manifest, element crops, device scale factor, theme switch, masking and highlight.
> - **Light-theme plot PNGs already render headless.** `scripts/agent_eval/board_render.py` loads
>   `frontend/dist` in a throwaway Chromium and returns each Analysis-board slot as a plot-only
>   light-theme PNG through the board's PDF export path. Check it before building a slide-tile path.
> - **The viewer tile has a non-browser route.** The offline renderer (`render_view_frame`, and
>   `wgpu_host` in Python, golden-pinned to the viewer's shaders) draws viewer frames headless, and the
>   pop-out's Share button already writes `<proj>/captures/<id>/frame.png`. Headless *Firefox* WebGPU
>   was tried and failed (`docs/archive/napari-webgpu-audit.md` G0); headless Chromium was not tried.
> - **"Light Experimental" is not a label in the app.** Schemes are plain `Dark` / `Light`
>   (`frontend/src/utils/colourScheme.ts`). State colours were made CVD-safe (#2157); stage and identity
>   hues still fail CVD separation and are the maintainer's call. Neither scheme had a screen review at merge.
> - **Guide steps have no image field.** `GuideStep` (`frontend/src/lib/guides/types.ts`) has anchor,
>   text, bullets, gates; the "guide link" is new work. Recipes already map the R missions
>   (`frontend/src/lib/guides/recipes.ts`, `docs/todo/WORKFLOW_RECIPES_PLAN.md`).
> - **The readthedocs source is not in this repo** (nor in the local old-R checkout), so "rebuild the
>   docs site from the same assets" has no build to hook into yet.
> - **The layout has kept moving**: GUI layout V0/V1 (#2150, #2153) landed after the clarity plan, so the
>   brief's "it will change again" held. Sample data: `test-data/immunemap/` and the Immunemap importer
>   (#2046) exist.

# GUIDE_SCREENSHOTS_PROMPT

**Model:** Opus. **Mode:** plan, plus one throwaway feasibility spike (Phase 0). No merged pipeline code in this pass.
**Output:** `docs/todo/GUIDE_SCREENSHOTS_PLAN.md` (+ spike notes and sample images in `docs/todo/guide-screenshots/`).

## Why

Cecelia has in-app guides users follow along. A few generated screenshots of the main features would make them clearer.
The layout has just changed (see `GUI_CLARITY_PLAN.md`), and it will change again, so hand-taken screenshots go stale immediately.
The goal is **screenshots generated from the real app, by script, that follow the UI**, reused for three consumers: in-app guides, the docs site, and **presentation slides**. Never drawn, mocked up, or AI-illustrated:
a picture of an interface that differs from the real one is worse than no picture.

## Basis: the existing tutorial

The previous version's docs have a tutorial with two canonical paths, **static image** and **live image**, each about 7 numbered steps
(create project, import, define channels, correct autofluorescence, segment, define populations, extract spatial interactions or behaviour),
with annotated screenshots, "why" prose, named sample images, a "main route vs. alternatives" diagram and a goal of under 1 hour
(https://cecelia.readthedocs.io/en/latest/tutorial.html, plus the walkthrough and overview pages). Adopt the **structure**; replace the **screenshots**.

- **Keep:** one canonical path per image type, mapped onto the journey stages and recipes (main route plus optional steps); the biology "why" prose; named sample data; the under-1-hour goal (measurable with a first-run walkthrough).
- **Replace:** the ~25 hand-annotated PNGs per path, obsolete after the redesign. Generate them; the capture step draws the highlight.
- **Skip for now:** the Vimeo screen recordings (they go stale and depend on a host). Short captured clips can come later.
- **Fix on the way:** parts of the old text are outdated (for example cyto2 models, now refused, and older model names, tools and the R-era parts). Tutorial prose needs rewriting alongside the images, not only new pictures. The old path also has **no check step**: add acquisition and the flagged checks, which is what users miss.
- **Docs site:** it still shows 0.1. Treat it as a second consumer of the same generated assets (see "Single source" below).

## Read first

`CLAUDE.md`, `INVENTORY.md`, `docs/UI.md`, the guides system (guide map, step definitions, the welcome-page guides entry point),
the fixtures and real-image ground-truth setup, the existing readthedocs tutorial, walkthrough and overview pages, the existing test and CI configuration, `WEB_VIEWER_PLAN.md`,
and the colour scheme work (Dark default, Light Experimental). Reference them; do not duplicate.

## Phase 0: Feasibility spike (throwaway)

Prove the hard part first, before designing anything around it.

1. Drive the running app with **Playwright and headless Chromium (new headless mode, the full browser, not the old stripped-down one)**. Open a fixture project, navigate to two pages, save two screenshots.
2. **WebGPU in headless mode:** check whether `navigator.gpu.requestAdapter()` returns an adapter in headless Chromium on the dev workstation (Ubuntu 24.04, NVIDIA RTX 2000 Ada, Vulkan). Find the working flag set. Check software fallback too, and record how it differs from the GPU render (speed, visual fidelity).
3. Open the **viewer pop-out window** and capture it. Record what breaks: window handling, WebGPU, load timing, data transfer being the known bottleneck.
4. Check the same steps on the CI runner (or the planned one). Say clearly whether CI can render the viewer, and if not, what the fallback is (capture viewer shots on the workstation only; or a GPU runner).
5. Record determinism: run each capture twice and diff. What varies (animations, timestamps, ids, scroll position, loading states, font loading, device scale factor)?

Report PASS / PARTIAL / FAIL per item with evidence. If the viewer cannot be captured headless, the plan must handle it as a separate path, not drop it.
Use confirmed flag names from the current Playwright and Chromium docs; do not guess.

## Phase 1: Design the pipeline

Propose, and justify against the spike results:

- **Two paths, one shot list each:** a static-image path and a live-image path, based on the existing tutorial. Aim for 6 to 8 shots per path (not 25), tied to the main journey: Welcome or project, Import, channels, a flagged check, a cleanup comparison, a Segment result, Gate or Track, behaviour or spatial result, a Figure, and the viewer window where it matters. Each shot: which guide step and which tutorial step it serves, which fixture state it needs, what single element is highlighted. Say which old steps are merged or dropped.
- **Shared dataset:** propose one sample dataset per path that serves as the tutorial data, the capture fixture and a test image. Where it is hosted (stable, versioned, checksummed; not a personal share link), size, licence and consent, and how the capture run fetches it. Check the Immunemap importer idea as a possible public source.
- **Single source:** the path definitions (steps, "why" text, shot ids) live in one place and feed both the in-app guides and the docs site. Propose the format, how the docs build consumes the generated images, and how the docs are regenerated at release so they never drift from the app.
- **Fixture and state:** which fixture dataset and which pre-run results produce each shot, without needing long processing at capture time. Prefer stored-answer or recorded results over running real jobs.
- **Capture script:** one declarative manifest (shot id, route, setup steps, selector to highlight, element crop, theme, window size, output scale) plus a runner. Support **element-level crops** (a plot, a panel, the checks cluster, a viewer image area) and **2 to 3x output scale**, so the same run yields guide shots and a reusable tile kit for slides. Support **Dark and Light** per shot. Fixed window size, fixed device scale factor, Dark scheme, animations off, fonts loaded, loading states settled, volatile text (timestamps, ids, paths) masked or fixed.
- **Highlight:** how the "click here" emphasis is drawn (a ring or label added by the capture step via a shared component or a CSS class, not baked by hand), in the palette's accent.
- **Alt text:** short alt text per shot, stored beside the manifest entry.
- **Storage and delivery:** where images live (repo vs. built artefact vs. release asset), how the app serves them to the guides, size budget, format and compression.
- **Guide link:** how a guide step names its screenshot, and what the app shows if the image is missing (nothing broken, no empty frame).
- **Freshness:** a build or CI check that fails when a guide references a missing shot, a manifest selector no longer matches, or the capture differs beyond a threshold from the last accepted one. Decide whether this doubles as a visual regression check and how accepted baselines are updated.
- **When it runs:** on demand, at release, and when the layout or tokens change. A release gate "screenshots regenerated" is acceptable if cheap. Never require a human to retake images.
- **Viewer path:** whichever approach the spike supports, and what happens on machines without GPU access.

## Presentation slides (consumer of the pipeline; later slice, with first-pass hooks)

The maintainer also wants one-slide summaries ("this is how you gate or track cells"): a collage of real GUI pieces with a caption.
Decisions already made:

- **Light scheme is needed.** The dark theme looks poor on a projector. Treat a reviewed Light scheme as a prerequisite for the slide kit: projector contrast (larger text, stronger ink), the CVD check the colour-token PR deferred, and a screen review. Say what remains before it can leave Experimental for this use.
- **Output is flat.** The maintainer works on Linux with LibreOffice, where PPTX renders differently, so editable PowerPoint is out. Produce **PNG (16:9, 1920x1080 or larger) and PDF**, which look the same everywhere and drop straight onto a slide. Propose composing the slide as an HTML template rendered by the same headless Chromium (vector text in the PDF, no extra toolchain). Keep the slide **spec** (title, tiles, numbered callouts, caption) as the editable source, so changing a slide means editing the spec and regenerating.
- **Reuse before building.** Check whether the analysis board and PDF export or the figure spec (`FIGURE_SPEC_PROMPT`) can already compose a slide; only add a composer if none fits.
- **Masking.** Project names, file paths and any sample data with sharing limits are masked or fixed before anything leaves the lab.
- **Single source.** A slide for "how to gate cells", the guide step and the tutorial page pull from the same step definition and shot ids.

**First slide (the easy one): "Gate cells and see the populations in the viewer."**
Tiles: the gate plot with a drawn gate, the populations list with counts, and the viewer image area with the population overlay, joined by
numbered callouts (1 draw the gate, 2 name the population, 3 see it in the viewer) and a one-line caption. Light scheme, projector-sized text.
Note for the viewer tile: the viewer window is exempt from colour tokens until the V0 split, so its chrome may not follow the Light scheme.
Prefer a **crop of the image area only** (an image is dark by nature and reads fine on a white slide), plus the overlay legend, and say if that sidesteps the dependency.
Use the shared sample dataset for the static path. A second slide ("track cells") follows the same recipe once the first works.

## Phase 2: Plan the slices

Split the work into independent, reviewable PRs, ordered. For example: manifest and runner for non-viewer pages; highlight helper;
guide integration and missing-image handling; the freshness check; the viewer path; CI wiring. Mark which need eyes on the real output before merge.
Include the presentation slice (composer, first slide) as its own later PR, after the guide screenshots work. Keep it small: wait for real use to show what else is needed, and do not build test infrastructure beyond what the plan needs.

## Output format

1. **Spike results table** (Phase 0) with PASS / PARTIAL / FAIL and evidence, including the working WebGPU flags and CI answer.
2. **Shot list** (Phase 1) with guide step, fixture state and highlight per shot.
3. **Pipeline design** in about one page: manifest shape, runner, determinism rules, storage, guide link, freshness check, and the single-source path definition shared by guides and docs.
4. **Old-to-new tutorial map:** each old tutorial step mapped to a new step, a merged step or a dropped one, with the outdated text called out.
5. **Risks**: viewer rendering, flakiness, repo size, baseline churn on every layout change, maintenance load on a single maintainer.
6. **Sliced PR plan** with effort S/M/L and which slices need human eyes.
7. **Decisions needed from D** (including where the shared sample dataset lives and whether the docs site is rebuilt from the same assets), each with your recommended answer. Include the Light scheme readiness for projector use and the first slide's viewer-tile approach.

## Rules

- Evidence over opinion: show spike output, not assumptions. Label anything inferred.
- No hand-annotated or hand-taken screenshots, and no generated or drawn illustrations of the UI. Screenshots come only from the real app.
- No hand-rolled infrastructure where an existing project tool fits; reuse what is already in the repo.
- No new video hosting dependency in this pass.
- Cecelia must work fully without Claude: guides and screenshots must not depend on any AI feature.
- Capture Dark for guides and Light for slides. Light stays Experimental until reviewed on screen and CVD-checked; say what that takes, and how the extra baseline churn is kept small (for example, only a Light set for the slide kit).
- If you cannot run the app or a headless browser in this environment, say so at the top and mark every finding that depends on it.
