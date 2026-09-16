> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current designs live in `docs/todo/WEBGPU_UPLOAD_PATH_PLAN.md` (fetch/upload path) and
> `docs/todo/WEBGPU_MULTI_ATLAS_PLAN.md` (atlas capacity beyond one buffer).
>
> **Outcome (2026-09-15): both audit hypotheses partially wrong, redesigned around the numbers.**
> Task A assumed the RTX 5000 would negotiate `maxTextureDimension3D` well above 2048 and that
> `chooseAtlasLayout` could then just use more VRAM — the diagnostic instead confirmed the laptop
> already hits Dawn/Linux Vulkan's 2048 floor AND the Chromium/Dawn `maxBufferSize = 4 GiB − 4`
> cap, so even a 32 GB card is architecturally capped at a single ~4 GiB atlas. That triggered
> `WEBGPU_MULTI_ATLAS_PLAN.md` (P1 shipped in PR #904, P2 in draft #912, P3 pending). Task B's
> "MAX_INFLIGHT may be too low" hunch is refuted by §H of the diagnostic — throughput plateaus at
> 200–280 MB/s across N = 1…32 on the local pipe; the real cost sits in `Response.arrayBuffer()`
> (6.7 ms/brick) and the HTTP/1.1 6-per-origin queue (wire climbs 4.5 → 140 ms as N grows). Those
> became U3 (staging ring, pending) and U4 (self-signed TLS + HTTP/2, draft PR #907). One prior
> lead — `writeTexture` — was measured at 7.1 ms vs `writeBuffer + copyBufferToTexture` at 3.2 ms
> (§C), shipped as U2a in PR #904.

# Audit: WebGPU atlas capacity & fetch throughput — are we underusing the GPU?

## Context

Repo: `schienstockd/cecelia`, `frontend/src/`. Brick-based volumetric viewer over WebGPU.

Two things came up while investigating the "movie playback flicks through blocks" issue
(intravital image, Dominik, 2026-09-10; see PR #893) and a follow-up hardware question
(workstation now has an RTX 5000, 32GB VRAM):

1. **Atlas texture capacity** — bricks are packed into a single `texture_3d<u32>` atlas
   (`lib/webgpu/brickAtlasTexture.ts`), sized per `utils/brickAtlas.ts`'s
   `chooseAtlasLayout`, which caps every axis at the device's `maxTextureDimension3D`.
2. **Fetch/admission throughput** — a two-tier inflight cap (`MAX_INFLIGHT = 16`,
   `MAX_INFLIGHT_BG = 8`, hardcoded in `lib/webgpu/brickVolumeRenderer.ts`) gates how many
   brick fetches are in flight at once. #893 found raising this to 32 did *not* help on a
   bench where the bottleneck was server/network (~25-27 MB/s) — but that was one dataset
   on unknown hardware/network conditions, not a general proof the cap is well-tuned.

Also already confirmed: `utils/webgpuProbe.ts` requests `requiredLimits.maxTextureDimension3D`
from `adapter.limits` (not a hardcoded 2048), so the atlas *should* size itself to whatever
the actual GPU reports (RTX 5000 likely reports something like 8192-16384, well above the
WebGPU spec floor of 2048). This needs verifying, not assuming.

## Goal

Confirm — or find counter-evidence for — the claim that the viewer is NOT artificially
capping itself below what an RTX 5000 (32GB VRAM) can deliver. Two failure modes to rule out
separately; don't conflate them:

- **Capacity-bound**: atlas too small → LRU eviction (`utils/brickScheduler.ts`) churns
  resident bricks even though data has already arrived over the wire.
- **Bandwidth-bound**: fetch/admission too conservative or genuinely server/network limited →
  bricks haven't arrived yet at `displayT`, regardless of atlas size (this was #893's finding
  on one bench; confirm it generalizes, or doesn't).

## Tasks

### A. Texture dimension / atlas sizing
1. In `utils/webgpuProbe.ts`, trace what happens if `adapter.requestDevice({ requiredLimits })`
   is rejected (some browsers/drivers silently clamp to the spec floor instead of throwing).
   Confirm the actual negotiated `device.limits.maxTextureDimension3D` is what gets passed
   into `chooseAtlasLayout`, not `adapter.limits` pre-negotiation or a stale/default value.
2. On an RTX 5000, what does Chrome/the Dawn WebGPU backend actually report for
   `maxTextureDimension3D`? (Check via `about://gpu` or a throwaway `navigator.gpu` probe —
   don't assume; different backends — Vulkan vs D3D12 vs Metal — report different ceilings.)
3. Given that reported limit, current brick size, and channel count, compute the atlas
   capacity `chooseAtlasLayout` actually lands on (`utils/brickAtlas.ts:147-190`). Is it
   using the full negotiated limit, or hitting some other earlier-return / clamp first?
4. Is there anywhere else in the codebase — a constant, a config default, a `requiredLimits`
   value — that clamps texture dimension, atlas slot counts, or VRAM budget below what the
   negotiated device limit allows? Grep for `2048`, `4096`, `8192` as suspicious hardcoded
   values outside of test fixtures.
5. Multi-atlas: confirm whether the codebase supports more than one atlas texture per image
   (for cases where one atlas' capacity, even at the max negotiated dimension, isn't enough
   for the working set). If single-atlas-only, is that a real ceiling for any current or
   planned dataset, given 32GB VRAM headroom?

### B. Fetch / admission throughput
1. Confirm `MAX_INFLIGHT` / `MAX_INFLIGHT_BG` (`lib/webgpu/brickVolumeRenderer.ts:88-101`)
   are still hardcoded constants (not runtime-tunable — #893 reverted the `?playInflight=N`
   knob). Are they scaled to anything about the client (VRAM, core count, connection), or
   fixed regardless of hardware tier?
2. Re-examine #893's conclusion ("bottleneck is server-side/network, not concurrency") —
   that was one bench (`2h06xA`, single-level store, 260 MB/t). Is there a dataset/scenario
   where raising `MAX_INFLIGHT` *would* help (e.g. a pyramided store where per-tick fetch
   size is already small, so concurrency — not per-request payload — becomes the limiter)?
   If so, the current hardcoded 16/8 could be leaving real throughput unused on a fast
   connection + strong GPU, even though it wasn't the limiter in that specific bench.
3. Where does the actual fetch happen (`utils/brickLoader.ts`?) — is it using HTTP/2 or
   HTTP/1.1 to the API? Connection-level concurrency limits (browser's per-origin limit,
   protocol) can silently cap throughput independent of `MAX_INFLIGHT`.
4. Server side (`api/src/`, wherever `/api/viewer/slab` is served from): is brick serving
   single-threaded / single-connection per client? Would it scale with more concurrent
   requests from a client, or is server-side concurrency itself the real ceiling regardless
   of client-side knobs?

## Deliverable

For each of A and B: a verdict (fine as-is / under-using the GPU / under-using the network)
with the specific file:line evidence, and — only if genuinely under-using — a concrete,
minimal proposed change. No speculative rewrites; this is a verification pass, not a
refactor. If everything checks out, say so plainly rather than manufacturing findings.
