# Settings page: WebGPU diagnostic ("glxgears for the browser")

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.
>
> **Outcome (2026-08-25):** capability report shipped as `frontend/src/components/GpuDiagnostic.vue`,
> mounted in `SettingsModule.vue` under Diagnostics; the shared adapter probe lives in
> `frontend/src/utils/webgpuProbe.ts` and backs both the diagnostic and the volume renderer.
> The "mini glxgears" live scene is DEFERRED — a first pass (`gearsRenderer.ts`) froze the browser
> on click-to-start and was removed rather than shipped as a hang; reinstate only after the freeze is
> diagnosed. Inventory line in `docs/inventory/FRONTEND.md`.

## Why

The web renderer's whole performance case depends on landing on a discrete
GPU with WebGPU + `high-performance` power preference actually honored.
This silently fails: Firefox has been observed returning the integrated
GPU from `requestAdapter({})` with `adapter.info` blanked, giving no way to
introspect what was actually granted. Users on hybrid laptops may also be
on a power-saving OS policy that routes the browser to integrated GPU
regardless of what the page requests. None of this throws an error —
it just quietly renders slower, and shows up later as vague "the viewer
feels laggy" complaints indistinguishable from a real network problem.

This is a self-serve diagnostic, not a blocking gate: report what was
detected, offer OS-specific guidance to fix it, and let the user proceed
regardless (some legitimately only have integrated GPU and that's fine
for lighter workloads).

## What to build

A small widget on the settings page with two parts: a capability report,
and a live visual/performance probe.

### Part 1 — Capability report

On load, request the adapter with `powerPreference: 'high-performance'`
and report:

- WebGPU supported at all? (feature-detect `navigator.gpu`)
- Adapter obtained, and whether `high-performance` was honored — since
  `adapter.info` cannot be trusted to self-report this (blanked in
  Firefox), use `maxTextureDimension3D > 2048` as the practical proxy the
  locked plan decisions already settled on: values at or below suggest
  integrated/low-power silicon, above suggests discrete.
- Relevant `limits` dump: `maxTextureDimension3D`, `maxBufferSize`,
  `maxStorageBufferBindingSize` — whatever else `image_render`'s WebGPU
  path actually depends on, check that code for the real requirements
  rather than guessing a limits list here.
- `r16uint` texture format support confirmed (non-filterable, per the
  locked delivery format decision) — fail loudly and specifically if not,
  since the whole render pipeline depends on it.
- One-line verdict: "Likely discrete GPU, ready" / "Likely integrated GPU —
  performance will be reduced" / "WebGPU unavailable."

If the verdict is integrated or unavailable, show OS-specific guidance
inline, collapsed by default:
- **Windows**: Settings → System → Display → Graphics → set browser to
  "High performance." Also check laptop-vendor utility (NVIDIA app,
  Lenovo Vantage, Dell/HP equivalents) and BIOS graphics-switching mode.
- **macOS**: Apple Silicon has no discrete/integrated split, nothing to
  change. Intel Macs: Battery settings → uncheck "Automatic graphics
  switching" to force discrete.
- **Linux**: distro-dependent — mention `prime-select`/`optimus-manager`
  as the common cases, not exhaustive.

Keep this copy short — a sentence or two per OS, not a support article.

### Part 2 — Live visual probe ("mini glxgears")

A small animated WebGPU scene rendered directly in the settings page,
running continuously while visible, with an on-screen FPS/frame-time
counter. Purpose: gives the user something to *look at* and confirms the
adapter isn't just detected but actually renders acceptably — glxgears'
actual value was never the gears, it was "spinning smoothly = your GPU
path works end to end."

Keep the scene itself trivial — this is a smoke test, not a benchmark
suite:
- A handful of rotating/orbiting 3D primitives (reuse whatever basic
  geometry/pipeline setup already exists from the raycaster prototype
  rather than writing a new one from scratch) is enough. No lighting
  model complexity needed, no textures required beyond confirming basic
  render-to-canvas works.
- Report instantaneous FPS and frame time (ms), updated per second, not
  per frame — a jittery per-frame number is noise, not signal.
- Optional stretch, only if cheap to add: bump primitive count or add a
  second render pass to get a rough read on headroom, since a scene that
  is trivially fast either way doesn't tell you much about margin for the
  actual raycaster workload.

This does not need to approximate the real raycaster's cost — it's a
correctness + baseline-smoothness check, not a predictor of actual
volumetric-rendering frame time. Say so explicitly in the UI copy so
nobody reads "60fps here" as "the volume viewer will hit 60fps."

## Where this plugs into the existing plan

This is diagnostic tooling, not part of the render pipeline itself — it
can be built and shipped independently of the P1–P7 phases in
`WEB_VIEWER_PLAN.md`, and should reuse whatever WebGPU device/pipeline
bootstrap code already exists from the raycaster prototype rather than
standing up a second one. If no such shared bootstrap exists yet, factor
this diagnostic's device setup so the raycaster work can reuse it later,
rather than writing two independent WebGPU init paths.

## Acceptance

- Loads and reports correctly on: a known-discrete machine (should say
  "ready"), and if available, a machine forced onto integrated/software
  rendering (should say "reduced" and show guidance) — test both, don't
  assume the happy path generalizes.
- Firefox specifically: confirm the `maxTextureDimension3D` proxy check
  behaves sensibly given `adapter.info` is blanked there.
- No crash or blank state when WebGPU is entirely unavailable (older
  browser, disabled flag) — falls through to a clear "not supported"
  message, not a silent failure.
