# WebGPU multi-atlas support — reaching VRAM past the single-buffer cap

Status: **SHIPPED** (2026-09-17) · P1+P2 shipped via PR #912 (2026-09-16) ·
P3 SUPERSEDED by [`WEBGPU_MULTI_ATLAS_SHADER_VARIANTS_PLAN.md`](WEBGPU_MULTI_ATLAS_SHADER_VARIANTS_PLAN.md)
— shader-variant fan-out unblocks P3 without `binding_array`, shipped S0–S4 via
PRs #941 / #944 / #949 / #952 / #958. Sub-plan of
[`WEBGPU_UPLOAD_PATH_PLAN.md`](WEBGPU_UPLOAD_PATH_PLAN.md) → U5. Kept as design record
for the P1/P2 refactor + slot-encoding contract that stays live. Multi-atlas contract lives
in `docs/ARCHITECTURE.md` → *Viewer* → *Multi-atlas contract*.

## Goal

Let the brick renderer use more atlas capacity than a single WebGPU texture can carry, by
allocating N atlas textures and binding them together. Two triggers, both confirmed on
Dominik's laptop (RTX 2000 Ada, Chromium/Dawn/Linux Vulkan) 2026-09-15:

1. **`maxBufferSize = 4 GiB − 4`** (Chromium/Dawn cap, hardware-independent). Even with an
   8 GB card the single-atlas ceiling is ~4 GiB — 50 %+ of the card's VRAM is architecturally
   unreachable. Confirmed by `validateAtlasLayout` rejecting every preset at 8/32 GB budgets
   in the diagnostic §B.
2. **`maxTextureDimension3D = 2048`** (Dawn/Linux Vulkan spec floor on discrete NVIDIA).
   Some layouts hit the axis cap before the byte cap. Same §B numbers.

The workstation (RTX 5000, 32 GB VRAM) will amplify the same two triggers. Not gated on
workstation numbers — laptop confirmed the shape.

## Non-goals

- **Not a rewrite of the fetch loop.** Fetch scheduling, LRU, `MAX_INFLIGHT`, `Response.arrayBuffer`
  timing — untouched. This plan only changes where a landed brick's bytes go.
- **Not a growth-past-4-atlases story.** N is a small fixed compile-time constant (see
  Decision 1). Growing past that would require WGSL runtime-sized bind groups, which don't
  exist. Revisit if a card ever needs it.
- **Not a policy change.** The atlas total budget still comes from `AUTO_CACHE_MB` /
  `viewerCacheMB` — this plan just spends it across N atlases instead of one.
- **Not multi-format.** All N atlases share `bytesPerVoxel`, `channelsPerBrick`, `brickSizeVox`
  — a level swap that changes any of them still forces a full destroy+recreate as it does
  today.

## Locked decisions

### Decision 1 — `MAX_ATLASES = 4`. Fixed at compile time.

WGSL has no runtime-sized bind groups. The shader binds `array<texture_3d<u32>, MAX_ATLASES>`
at build time, so N must be a constant. 4 is picked because:

- It covers the laptop case (2 atlases × 4 GiB = 8 GB > any real per-image working set).
- It covers the workstation case with headroom (4 × 4 GiB = 16 GiB against 32 GB VRAM —
  the other half stays for the OS, other tabs, and the `viewerCacheMB` cap most users leave
  well below 16 GB).
- Larger N inflates the shader's bind-group and adds sampler-per-array cost per fragment;
  4 keeps the fragment shader lean.
- The label atlas (r32uint, one per image) also needs `MAX_ATLASES` slots in the bind group.
  Doubling the bind-group cost at N=4 is fine; at N=8 it would start to matter.

If a card ever *does* need more, the escape hatch is a new plan — this one caps at 4.

### Decision 2 — Slot ID is a **global** integer, atlas index is derived.

`PageTable.slot` stays a single `number` covering `[0, totalCapacity)`. Atlas index is
computed on demand: `atlasIndex = slot / perAtlasCapacity`, `localSlot = slot % perAtlasCapacity`.
Reason: this keeps `PageTable`'s LRU / eviction / free-list logic byte-identical, and the
shader receives a single 32-bit slot ID (existing wire format) it can decode into (atlas,
xyz) with two divs — same cost as today's single-atlas decode.

The alternative — `PageTableEntry { atlas: number, slot: number }` — was rejected because
it doubles the page-table's per-entry memory and forces every existing caller to un-flatten
the ID.

**Contract:** all N atlases have the same `perAtlasCapacity`. Non-uniform atlas sizes would
break the global-slot encoding. See Decision 3.

### Decision 3 — N atlases are **identical** in geometry.

`pickAtlasLayout` returns `AtlasLayout[]` of length ∈ [1, MAX_ATLASES]. Every layout in the
array has the same `brickSizeVox`, `atlasSlotCounts`, `bytesPerVoxel`, `channelsPerBrick`.
The only reason there's an array (rather than a `count: number` field on a single layout)
is to keep the future door open for heterogeneous atlases without another API break — but
Phase 1 and Phase 2 both enforce homogeneity.

Reason: heterogeneous sizes would need a separate `perAtlasCapacity[i]` and destroy the
one-div slot decode (Decision 2). No user-visible win — the caller wants "more VRAM," not
"more VRAM in awkwardly-shaped pieces."

### Decision 4 — Sizing: pack until budget or axis limit, then start a new atlas.

`pickAtlasLayout(bytes, ..., budget, limits)` sizes ONE atlas at `min(budget, maxBufferSize)`
with the existing algorithm, then divides the caller's remaining budget by that atlas' byte
size to get N. Capped at `MAX_ATLASES`. Uneven remainders round down — a `perAtlas × N` that
doesn't quite fill the budget is the design (Decision 3 forbids ragged sizes).

Reason: this reuses today's sizer for the per-atlas layout — no separate multi-atlas
tie-break, no risk of a different tie-breaker choosing a worse-square layout.

### Decision 5 — Reuse across level changes: same rules per atlas.

`canReuseAtlas` becomes `canReuseAtlases(current: AtlasLayout[], next: AtlasLayout[]): boolean`
— true iff `current.length === next.length` AND every pair matches by the existing
`canReuseAtlas` rules. A level swap that changes N drops all atlases.

### Decision 6 — Shader binds a fixed array, uses `select` on atlas index.

WGSL:
```wgsl
@group(0) @binding(3) var atlases: binding_array<texture_3d<u32>, MAX_ATLASES>;
```

`binding_array` is the only mechanism that lets a runtime slot ID pick which texture to
sample. It's the `binding_array` proposal (gpuweb/gpuweb#binding-array — status
"explored", not yet in the stable spec as of 2026-09), so support needs a two-part probe,
NOT a single feature check:

1. **WGSL parse.** Can the shader compiler even accept `binding_array<T, N>`?
2. **Runtime bindGroup.** Does WebGPU's `createBindGroupLayout` accept an entry with
   `arraySize: N`, and `createBindGroup` accept `resource: [view0, view1, …]`?

Both must be true to ship P3 without a fallback. `docs/todo/spike/webgpu/diagnostic.html`
§A probes both and reports `probe.wgsl.bindingArray4.{wgslCompiles, bindGroupAccepts,
supported}` — re-run on the target device before merging P3.

**Confirmed on RTX 2000 Ada / Brave 151 / Dawn Vulkan (2026-09-16, two independent runs
with `--enable-unsafe-webgpu --enable-webgpu-developer-features --use-vulkan=native`)**:
`wgslCompiles = true`, `bindGroupAccepts = false`. Error text (verbatim, both runs):

> Failed to execute 'createBindGroup' on 'GPUDevice': Failed to read the 'entries' property
> from 'GPUBindGroupDescriptor': Failed to read the 'resource' property from
> 'GPUBindGroupEntry': Failed to read the 'buffer' property from 'GPUBufferBinding':
> Required member is undefined.

The runtime API for `resource: [view0, view1, …]` is not shipped in Chromium 151 even with
experimental WebGPU flags on. This is the "WGSL parses, runtime rejects" split the two-part
probe exists to catch. **P3 is blocked on this device.** Re-check trigger: a Chromium
version bump that flips `bindGroupAccepts` in the diagnostic, or a different device that
exposes the runtime.

The fallback is Phase 2's behaviour **with a runtime clamp**: `probeBindingArraySupport`
runs once at device acquisition (`utils/webgpuProbe.ts`), and `pickAtlasLayout` accepts a
`maxAtlases` param — the renderer passes `1` when the probe returns false so N stays clamped
to 1 regardless of budget. See P2 below. No crash, no user-visible holes, and the multi-atlas
allocation code paths stay under test coverage so the day the runtime unblocks it's a
one-line flip.

## Files touched, per phase

| Phase | Files | Nature |
|-------|-------|--------|
| P1 | `frontend/src/utils/brickAtlas.ts` (sig change) · `frontend/src/lib/webgpu/brickAtlasTexture.ts` (create/write N) · `frontend/src/utils/pageTable.ts` (perAtlasCapacity constant) · `frontend/src/lib/webgpu/brickVolumeRenderer.ts` (allocate array, index) | Refactor — no behaviour change; N=1 always |
| P2 | `frontend/src/utils/brickAtlas.ts` (`pickAtlasLayout` returns N > 1 when budget > maxBufferSize) · `frontend/src/lib/webgpu/brickAtlasTexture.ts` (`writeBrick` routes on atlasIndex) | Allocation change — enable N > 1 |
| P3 | `frontend/src/lib/webgpu/brickShader.ts` (WGSL `binding_array`) · `frontend/src/lib/webgpu/brickVolumeRenderer.ts` (bind group + `binding_array` feature check) | Shader change — sampling honours atlas index |

## Phases

### P1 — Refactor pickAtlasLayout to return AtlasLayout[]. No behaviour change; length always 1.

- `pickAtlasLayout` returns `AtlasLayout[]` — always length 1. Callers extract `[0]` where
  the shader/binding path is not yet ready.
- `createBrickAtlasTexture` becomes `createBrickAtlasTextures(layouts: AtlasLayout[])` and
  returns `BrickAtlasTexture[]` — always length 1 in this phase.
- `PageTable`'s constructor takes a single `capacity` still, but the atlas layer stores
  `perAtlasCapacity` alongside for Phase 2's slot decode.
- `brickVolumeRenderer` allocates the array, keeps `atlas.texture` accessors that pick
  `atlases[0].texture`, no bind-group change yet.
- **Test coverage:** existing `brickAtlas.test.ts` extends to assert array shape + length 1.
  No new behavioural tests — this phase is a signature refactor.
- **Ship criterion:** `pixi run test-frontend` green, `pixi run dev` renders fXgbTl same as
  today, PR-A's T1.1 clamp still works.

### P2 — Allocate N atlases when budget > `min(maxBufferSize, per-atlas cap)`. **SHIPPED 2026-09-16 in PR #912, with runtime clamp keeping N=1 until P3 unblocks.**

- `pickAtlasLayout` divides remaining budget by `atlasVramBytes(layouts[0])`, adds atlases
  up to `MAX_ATLASES`. Ragged tail rounds down.
- `writeBrick(slot, ...)` in `createBrickAtlasTextures` routes to
  `atlases[Math.floor(slot / perAtlasCapacity)].writeBrick(slot % perAtlasCapacity, ...)`.
- Page-table capacity becomes `perAtlasCapacity * atlasCount`. LRU / free-list unchanged
  (Decision 2).
- **Shader still binds atlas[0] only** — every write past `perAtlasCapacity` is a no-op on
  the visible render. That's the guard: the fetch loop can be exercised against N > 1 and
  its residency map, without the shader picking up the extra bytes yet.
- **Test coverage:** unit tests for the slot decode (globalSlot ↔ (atlasIndex, localSlot))
  and for `pickAtlasLayout` returning N > 1 at 8/32 GB budgets on a mocked device with
  4 GiB `maxBufferSize`.
- **Ship criterion:** `pixi run dev` renders the same at N=1 (existing card behaviour when
  `viewerCacheMB < 2048`) AND renders correctly-with-holes at N=2 (bricks past
  `perAtlasCapacity` render as their placeholder colour, not garbage).

### P3 — WGSL `binding_array` — shader samples the correct atlas. **SUPERSEDED (2026-09-17) — `WEBGPU_MULTI_ATLAS_SHADER_VARIANTS_PLAN.md` ships the same capability without `binding_array`.**

Original block cause: Chromium/Dawn 151 rejected the runtime bindGroup even with
`--enable-unsafe-webgpu` (Decision 6's ship criterion). Rather than wait on Chromium, the
follow-up plan compiles one shader per N ∈ 1..MAX_ATLASES with N static texture bindings and
a compile-time `switch(atlasIndex)` — standard WGSL, no `binding_array` runtime. Same slot
encoding (Decision 2), same allocation machinery (P2), same page-table decode — only the
final `textureLoad` moves inside a switch. See the follow-up plan for locked decisions and
the S0 diagnostic that verified the shape on real Dawn.

- `brickShader.ts` swaps `texture_3d<u32>` for `binding_array<texture_3d<u32>, MAX_ATLASES>`.
  Same for the label atlas (Decision 1: bind group handles both).
- Atlas index decoded in WGSL from the page-table slot: `let ai = slot / perAtlasCapacity;
  let ls = slot % perAtlasCapacity;` — same math as JS.
- `brickVolumeRenderer` bind group holds all N `GPUTextureView`s. Feature check on
  `device.features.has('binding_array')`; missing → fall back to Phase 1 mode (single
  atlas at `min(budget, maxBufferSize)`).
- **Test coverage:** none automated (this is a shader change). Real-viewer smoke: fXgbTl
  at 4 GB budget → N=1 renders the same as today. fXgbTl at 8 GB budget → N=2 (assuming
  laptop) renders both halves; regression check by walking Z and comparing to N=1 at the
  same Z.
- **Ship criterion:** real-viewer visual identical for N=1 case; visual for N=2 case
  matches N=1 case rendered at the same Z (measured by Dominik).

## Migration risk map

| Risk | Where | Guard |
|------|-------|-------|
| Global slot ID overflow | `PageTable.slot` becomes `perAtlasCapacity * MAX_ATLASES` ≤ ~1M for realistic cases; still fits `number` | Assert in Phase 1 |
| `binding_array` feature missing on some driver | Phase 3 shader change | Feature check + Phase 1 fallback |
| Label atlas mismatch | Label atlas is one per image today; must also become N | Same refactor applied to label atlas in each phase; assert atlas counts equal |
| Slot 0 special-cased somewhere | Free-list starts at slot 0 first — Phase 2 needs slots > `perAtlasCapacity` reachable | Free-list stays `capacity - 1 ... 0` ordering (unchanged) — Phase 2 test at N=2 asserts slots ≥ perAtlasCapacity get used |
| BindGroupLayout size limit | Chromium's default `maxBindingsPerBindGroup = 640` — 4 image + 4 label textures + 1 sampler + uniform + storage = 11. Fine. | No guard needed |

## Retiring this plan

- After P3 ships, add a "Multi-atlas contract" section under `docs/ARCHITECTURE.md` →
  *WebGPU viewer* covering: `MAX_ATLASES`, slot encoding (Decision 2), homogeneity
  invariant (Decision 3), fallback behaviour when `binding_array` is missing.
- Update the parent `WEBGPU_UPLOAD_PATH_PLAN.md`'s U5 phase row with the merged PR(s)
  and the measured VRAM the renderer can now actually reach on Dominik's card and the
  workstation.
- Move this plan to the *closed* section of `docs/todo/README.md`.
