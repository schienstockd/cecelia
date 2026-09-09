# Prompt: Coastal as default live-cell segmentation + Cellpose v4 upgrade + drop denoising + model vault

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

## Context
Cecelia currently offers Cellpose-based segmentation. `coastal` (self-supervised, optical-flow-based UNet segmentation) is now mature enough to become the default segmentation backend for **live/timelapse** imaging workflows. Cellpose remains available as a fallback/alternative but is being upgraded to v4, and its built-in denoising module is being dropped (superseded by coastal's motion-based approach / no longer needed).

## Discovery first
Before writing any code: search the codebase (Python segmentation module + Julia/Vue pipeline config) for every place that:
- Selects a segmentation backend/method (defaults, config schemas, UI dropdowns, pipeline node definitions)
- References Cellpose denoising (`denoise_model`, `restore_type`, or similar Cellpose API args)
- Pins the Cellpose version (requirements/environment files, lockfiles, Docker/conda envs)

Produce an inventory of touchpoints before changing anything. Do not assume a single config file controls this — check pipeline node defaults, CLI defaults, and any UI-exposed dropdown separately, since they may drift independently.

## Task 1 — Make coastal the default for live-cell segmentation
- Identify how the pipeline currently distinguishes "live/timelapse" from "fixed/static" image inputs.
- Change the default segmentation method for live-cell/timelapse workflows to `coastal`. Cellpose (v4) remains the default for static/fixed-image workflows with clean signal — v4 is stronger there than v2/v3, but empirically weaker than v2/v3 on intravital/live data, which is why coastal takes over for the live-cell case specifically.
- Cellpose must remain fully selectable (not removed) for live-cell workflows — this is a default change, not a deprecation.
- Update any UI copy/tooltips that state or imply Cellpose is the default.

## Task 2 — Upgrade Cellpose to v4
- Bump the pinned Cellpose version to v4 in all environment/dependency files found in discovery.
- Audit for breaking API changes between the currently pinned version and v4 (model loading, `eval()` signature, GPU handling, pretrained model names). Cellpose v4 introduced the `cpsam` (Cellpose-SAM) model and changed some model-loading conventions — check whether the current code assumes a pre-v4 model zoo/API and update accordingly.
- Update or add tests covering the segmentation call path to catch signature drift.

## Task 3 — Drop Cellpose denoising
- Remove the Cellpose denoising code path entirely (config options, UI toggles, any `denoise_model`/`restore_type` args passed to Cellpose).
- Remove now-unused denoising-specific dependencies if any are isolated to that feature.
- Update docs/config schema/CLAUDE.md or INVENTORY.md references that mention Cellpose denoising.

## Task 4 — Public model vault + picker for coastal
Design and scaffold a public model vault for pretrained coastal models, mirroring the existing plugin system's shape (discover → fetch → register → select).

- **Vault repo**: a GitHub repo (or release assets on the coastal repo) holding versioned model weights plus a manifest (JSON/YAML) per model — tissue type, imaging modality, channels used, training data description, metrics, checksum, download URL.
- **Picker**: manifest-driven list Cecelia fetches and caches locally, surfaced as a dropdown/gallery with metadata — not just filenames — so users pick by closest match to their data. Reuse the plugin system's discovery/fetch/registration pattern rather than building a parallel mechanism.
- **Local cache + versioning**: models are pulled into a local cache dir on first use, checksum-verified, and version-pinned in the pipeline config so runs stay reproducible even if the vault updates later.
- **"Train your own" stays first-class**: the picker's default/top option should point toward training on the user's own data — pretrained options are framed as starting points/fallbacks, not the primary path.
- **Contribution path**: even for a v1 with only your own models, define the manifest schema so third-party submissions can be added later without a breaking schema change. A lightweight validation/review step can come later.

Deliverable for this task can be a design doc + manifest schema + minimal picker scaffold — full vault population is out of scope for this prompt. Use the existing `flow.cyto` model (trained on cytoplasmic-reporter germinal center B cells) as the first entry in the vault to validate the manifest schema against a real model.

## Contract preservation
- Existing saved pipelines/configs that explicitly specify `cellpose` as the method must continue to work unchanged — only the *default* changes, not explicit user selections.
- Existing pipelines/configs that reference Cellpose denoising options should fail with a clear migration message, not silently ignore the setting.

## Deliverables
1. Discovery inventory (file list + what each touchpoint does)
2. Code changes for Tasks 1–3
3. Tests: default-selection test for live vs. static workflows, Cellpose v4 call-path test, denoising-removal regression test
4. Doc/CLAUDE.md/INVENTORY.md updates
5. Short migration note for any user-facing config changes (denoising removal)
6. Model vault design doc + manifest schema + minimal picker scaffold (Task 4)

## Check-in points
Stop and report back after the discovery inventory, and again after Task 2 (version bump + API audit), before proceeding — Cellpose v4's model-loading changes may have ripple effects worth confirming before touching the default-selection logic. Also check in after the Task 4 manifest schema draft, before scaffolding the picker, since the schema should be reviewed before it's load-bearing.
