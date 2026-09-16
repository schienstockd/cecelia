# WebGPU multi-atlas — shader-variants unblock for P3

Status: **planning** (2026-09-16) · branch `docs/multi-atlas-shader-variants` (docs only) ·
follow-up to [`WEBGPU_MULTI_ATLAS_PLAN.md`](WEBGPU_MULTI_ATLAS_PLAN.md) → Phase 3 (blocked
on WGSL `binding_array` runtime).

## Why this exists

`WEBGPU_MULTI_ATLAS_PLAN.md` P1 + P2 shipped in PR #912: `pickAtlasLayout` returns
`AtlasLayout[]`, `createBrickAtlasTextures` allocates N atlases, page-table decode splits
a global slot ID into `(atlasIndex, localSlot)`, `writeBrick` routes bytes to the right
atlas. P3 — the shader sampling from atlas > 0 — is blocked on Chromium/Dawn shipping the
`binding_array` runtime, which today is a draft `gpuweb` proposal gated behind Dawn's
temporary `enable chromium_experimental_dynamic_binding`. No shipping Chromium exposes it;
the two-part probe in `spike/webgpu/diagnostic.html §A` returned `bindGroupAccepts=false`
on Brave 151 / Dawn Vulkan (2026-09-16) even with `--enable-unsafe-webgpu
--enable-webgpu-developer-features --use-vulkan=native`.

Result today: the runtime clamp in `brickVolumeRenderer.ts:553` forces `maxAtlases=1` on
every device we can test on. The P2 allocation machinery is dormant — a workstation with
32 GB VRAM reaches the same ~4 GiB atlas as the laptop, ~28 GB unused.

This plan closes that gap **without waiting on Chromium** by compiling one shader per
possible atlas count (N ∈ {1, 2, 3, 4}) and picking the variant at pipeline-creation time.
The variant with 4 explicit texture bindings does a static `switch(atlasIndex)` over its
own 4 bindings — no runtime-indexed binding, no `binding_array`, standard WGSL.

## Non-goals

- **Not a change to P1 + P2 machinery.** Allocation, page-table decode, writeBrick routing
  are untouched. This plan only replaces the WGSL half that P3 owned.
- **Not a way past `MAX_ATLASES = 4`.** More variants means more shader compiles and more
  pipeline objects — the ceiling is the same as P3 for the same reason. If a card ever
  needs > 4, that's a separate plan.
- **Not a `maxBufferSize` cap increase.** One atlas is still capped at Chromium/Dawn's
  ~4 GiB (u32 offsets). This plan gets you N × that, not a bigger single atlas.
- **Not a shader rewrite.** The existing sampling pipeline (page-table lookup → slot decode
  → `textureLoad` on the atlas) stays; only the atlas binding + the one `textureLoad` call
  moves inside a `switch`.
- **Not `binding_array`-instead-of.** When Chromium ships the runtime, that path becomes
  the simpler default and the variant fan-out can be retired. See *Migration to
  `binding_array` when it lands*.

## Locked decisions

### Decision 1 — Compile N ∈ {1, 2, 3, 4} shader variants at renderer construction.

Four WGSL modules, one pipeline each, compiled once at `createBrickVolumeRenderer` time.
Alternative — compile lazily on first N-atlas allocation — was rejected because a level
swap can change N mid-session (Decision 5 of the parent plan already drops all atlases on
N change), and a lazy compile there would add first-frame jank at exactly the moment the
user notices.

**Reason:** compilation is measured in ms on Dawn; 4 × that at startup is invisible.
Runtime pipeline switch is a single `setPipeline` call on level change, no measurable cost.

### Decision 2 — The variant is picked at pipeline-creation time, not per draw.

`pickAtlasLayout` returns N; the renderer stashes the matching pipeline handle in
`AtlasState.pipeline`; every `draw()` binds that pipeline unchanged. A level swap that
changes N drops the atlas (Decision 5 of the parent plan) and rebinds the pipeline
alongside — same event, no extra plumbing.

Alternative — one pipeline with 4 texture bindings and a runtime `if` — was rejected
because it forces the shader to declare 4 bindings even at N=1, wasting a bind-group slot
and running the driver's own branch cost per fragment. The N-variant approach also lets
the N=1 shader be byte-identical to today's (it just calls `textureLoad(atlas0, ...)`
directly, no switch), so the common case pays nothing.

### Decision 3 — WGSL `switch(atlasIndex)` over static binding literals, one arm per atlas.

For N=4:
```wgsl
@group(0) @binding(3) var atlas0: texture_3d<u32>;
@group(0) @binding(4) var atlas1: texture_3d<u32>;
@group(0) @binding(5) var atlas2: texture_3d<u32>;
@group(0) @binding(6) var atlas3: texture_3d<u32>;

fn sample_brick(atlasIndex: u32, coord: vec3<i32>) -> vec4<u32> {
  switch atlasIndex {
    case 0u: { return textureLoad(atlas0, coord, 0); }
    case 1u: { return textureLoad(atlas1, coord, 0); }
    case 2u: { return textureLoad(atlas2, coord, 0); }
    default: { return textureLoad(atlas3, coord, 0); }
  }
}
```

**Reason:** each `textureLoad` binds to a compile-time-known literal binding, which is what
WGSL requires today (Chromium 151 / Dawn Vulkan verified). No `binding_array`, no
runtime-indexed sampling.

**Reservation:** the shader compiles need a real Dawn parse before committing to the
workstream. Prototype in `spike/webgpu/diagnostic.html §I` (add) before the P0 workstream
starts — if for any reason a `switch` over `textureLoad`s of distinct bindings gets
rejected on any backend, the fallback is `if/else if` (same shape, wider compat), or worst
case an inlined shader per N with no branching at all.

### Decision 4 — Label atlas gets the same treatment.

The parent plan's Migration Risk table calls this out: label atlas is one-per-image today.
The single-atlas gate at `brickVolumeRenderer.ts:831` (`if (expectedSlot >=
atlas.perAtlasCapacity) return`) drops label bricks that landed in intensity `textures[1..]`
because there's nowhere to render them. This plan grows the label atlas to N as well and
removes that gate.

Bind group grows by N label textures. At N=4 the bind group is 4 image + 4 label textures
+ 1 sampler + uniform + storage = 11 bindings, well under Chromium's default
`maxBindingsPerBindGroup = 640` (already noted in the parent plan's risk table).

### Decision 5 — `viewerCacheMB` slider is the only user knob.

The atlas count derives from `viewerCacheMB / perAtlasBytes`; the user sets one number
("how much VRAM to spend on the viewer") and multi-atlas is emergent. No new
`MAX_ATLASES`-style slider — that would let the user pay for 4 shader compiles even when
their budget only asks for one atlas.

**Settings change:** raise the `viewerCacheMB` chip ceiling past the current 4 GB cap
(shipped in `CACHE_CHIP_CEILING_PLAN.md`'s "not the right question" verdict). New ceiling:
`MAX_ATLASES × maxBufferSize` = 16 GiB in practice. Add a resolved read-out beside the
slider — "12 GB → 3 atlases" — following the [[feedback_auto_shows_what_was_picked]]
pattern.

### Decision 6 — Diagnostic row shows the resolved N read-only.

Settings → WebGPU diagnostic row (already exists via `probeWebGpu`) grows a line: "Atlases:
3 of 4". Read-only. This is where a user learns *why* their slider setting materialised as
the number of atlases it did — not a knob.

## Files touched, per phase

| Phase | Files | Nature |
|-------|-------|--------|
| S0 | `docs/todo/spike/webgpu/diagnostic.html` (add §I) | Diagnostic — prove Decision 3 shader compiles on real Dawn |
| S1 | `frontend/src/lib/webgpu/brickShader.ts` (4 variants) · `frontend/src/lib/webgpu/brickVolumeRenderer.ts` (compile all 4 pipelines at construction; pick per atlas) | Shader change — replace single-atlas binding with N-variant switch |
| S2 | `frontend/src/lib/webgpu/brickVolumeRenderer.ts` (grow label atlas to N; delete `if (expectedSlot >= perAtlasCapacity) return` at line 831) | Label atlas grows to N |
| S3 | `frontend/src/lib/webgpu/brickVolumeRenderer.ts` (remove runtime clamp at line 553 — `maxAtlases = undefined` always) · `frontend/src/utils/brickAtlas.test.ts` (drop the `binding_array` clamp test) | Retire the P2 fallback — the clamp exists only because P3 was blocked |
| S4 | `frontend/src/components/settings/*` (raise `viewerCacheMB` ceiling; add resolved read-out) · `frontend/src/modules/ViewerWindow.vue` (diagnostic row grows "Atlases: N of M") | User-facing surfacing per Decisions 5 + 6 |

## Phases

### S0 — Prove the WGSL variant shape compiles on Dawn/Vulkan. **BLOCKS EVERYTHING.**

- Add `spike/webgpu/diagnostic.html §I`: four shader modules (N=1..4), each declares its
  static bindings and its `switch(atlasIndex)` arm per Decision 3. Report per-N:
  `wgslCompiles`, `pipelineCreates`, `sampleOk` (draw one fragment that reads from atlas 0
  and atlas N-1 through the switch).
- Re-run on Brave 151 / Dawn Vulkan (Dominik's device) AND on any workstation before
  merging S1. If any N fails, drop back to `if/else if` and rerun; if that fails, inline
  N shaders with no branching. Do NOT ship S1 against unverified shader shape.
- **Ship criterion:** all four variants compile and sample correctly in the diagnostic on
  at least one target device.

### S1 — Compile N variants, pick per atlas. Shader binds N textures.

- `brickShader.ts` exports a factory `makeBrickShader({ nAtlases: 1|2|3|4 })` that emits
  the right binding block + `switch` arms. Called four times at renderer construction.
- `brickVolumeRenderer.ts` builds four pipelines (`pipeline1..4`) alongside the current
  one; `atlas.pipeline` is set to the matching one at atlas construction. `draw()` binds
  `atlas.pipeline` instead of the single existing pipeline.
- Bind group grows to N image bindings. For N < 4, the unused binding slots stay absent
  (the variant's bind-group layout only declares N textures — WebGPU forbids "absent" as
  a runtime value, so each variant has its own layout).
- **Test coverage:** unit tests for `makeBrickShader` output (assert binding numbers +
  `switch` arm count per N). No new behavioural test — S0's diagnostic is the ground
  truth.
- **Ship criterion:** fXgbTl at a 4 GB budget renders identical to today (N=1 shader
  identical to current). fXgbTl at a 12 GB budget renders correctly through N=3 (visual
  smoke by Dominik; the "walking Z and comparing to N=1 at the same Z" test from parent
  plan P3).

### S2 — Label atlas grows to N. Delete the P2 orphan-brick gate.

- `brickVolumeRenderer.ts:604-610` — allocate N label textures instead of one, sized per
  layout. The shader gains N label bindings and a matching `switch(atlasIndex)` in the
  label sample path.
- Delete the `if (expectedSlot >= atlas.perAtlasCapacity) return` gate in `kickLabelFetch`
  (line 831) — with the shader able to render from atlas > 0, orphan-brick avoidance no
  longer applies.
- **Test coverage:** unit test that label writes land in the right (atlas, slot) pair;
  visual smoke on a fXgbTl label overlay at N=2 vs N=1 (must be identical).
- **Ship criterion:** label rendering at N > 1 identical to N=1 at the same Z on a
  labelled image (jFWePN, MERTK).

### S3 — Retire the runtime clamp. The `binding_array` probe becomes advisory.

- `brickVolumeRenderer.ts:553` — drop `maxAtlases = report.bindingArraySupported ? undefined : 1`
  in favour of `maxAtlases = undefined` unconditionally. The shader variants make the
  probe's ship-gate role obsolete.
- `webgpuProbe.ts:probeBindingArraySupport` stays (still useful for future migration back
  to `binding_array` if it ever ships), but the report field becomes advisory rather than
  ship-gating. Diagnostic row still shows it.
- `brickAtlas.test.ts` — drop the `clamps to N=1 when caller passes maxAtlases=1` test
  (the caller no longer clamps).
- **Ship criterion:** on Dominik's laptop, the renderer at 12 GB `viewerCacheMB` now
  allocates ~3 atlases and renders correctly (measured VRAM matches, no black holes).

### S4 — Uncap the `viewerCacheMB` slider and add the resolved read-out.

- Settings → Viewer → cache-size chip: extend the ceiling to `MAX_ATLASES × maxBufferSize`
  (~16 GiB). Rebutted in `CACHE_CHIP_CEILING_PLAN.md` under P2 constraints — now that
  multi-atlas actually renders, that decision reverses.
- Add a resolved-value line below the slider: "12 GB → 3 atlases" — computed from the
  same math `pickAtlasLayout` uses (Decision 4 of the parent plan).
- Diagnostic row (Settings → WebGPU) grows an "Atlases: N of M" line, driven by the same
  computed value. Read-only.
- **Ship criterion:** workstation user sets 16 GB, sees "4 atlases", the diagnostic
  agrees, the renderer allocates 4, VRAM headroom monitoring confirms.

## Migration risk map

| Risk | Where | Guard |
|------|-------|-------|
| WGSL `switch` over distinct-binding `textureLoad`s rejected | S0 shader parse | S0 diagnostic gates the whole workstream |
| 4× shader compile jank at renderer construction | S1 startup path | Measure in S0's diagnostic; if > 50 ms total, drop to lazy compile on first N-atlas allocation |
| Pipeline switch on level change costs | Draw loop after S1 | `setPipeline` is a single command-encoder call, no allocation; guarded by an assertion in bench harness |
| Bind-group layout mismatch across variants | S1 pipeline creation | Each variant gets its own explicit `GPUBindGroupLayout` — no shared layout, no runtime mismatch possible |
| Label atlas alloc doubles VRAM | S2 | Only when labels enabled; already the case at N=1 (one label atlas per image atlas). At N=4 the total is still under `viewerCacheMB` by construction (labels are r32uint, sized to the same slot grid) |
| A user's saved `viewerCacheMB` at the old ceiling silently becomes different behaviour | S4 | Old saved value stays valid; only the ceiling moves. Existing installs unaffected until user changes it |

## Migration to `binding_array` when it lands

If Chromium ever ships the `binding_array` runtime (probe flips `bindGroupAccepts=true`):

- Add ONE more shader variant, `nAtlases: "runtime"`, using `binding_array<texture_3d<u32>, MAX_ATLASES>`.
- Renderer picks that variant when the probe returns true; falls back to the fixed-N
  variants when false.
- After a release or two of the runtime variant being the default on supported browsers,
  retire the 1..4 fixed variants.

This plan does NOT wait for that migration — the fixed-variant path stands on its own.

## Retiring this plan

- After S3 ships, add a "Multi-atlas contract" section under `docs/ARCHITECTURE.md` →
  *WebGPU viewer* covering the shader-variant fan-out, the retired runtime clamp, and the
  `binding_array` migration hook.
- Update `WEBGPU_MULTI_ATLAS_PLAN.md`'s P3 row: `"BLOCKED (2026-09-16) — see
  WEBGPU_MULTI_ATLAS_SHADER_VARIANTS_PLAN.md — supersedes P3, ships without
  binding_array."`
- Update `WEBGPU_UPLOAD_PATH_PLAN.md`'s U5 row with the measured VRAM the renderer can now
  actually reach on Dominik's laptop and the workstation.
- Move this plan to the *closed* section of `docs/todo/README.md`.
