**Status:** decided — not the right question (2026-09-02). Prompt's premise did not hold up against
measurement; no code change lands from this plan. Kept as a decision record.

## Outcome (2026-09-02)

Measured on branch `docs/cache-chip-ceiling-prompt`, Brave 1.x on RTX 2000 Ada Laptop with PRIME
render offload to NVIDIA (`vendor: nvidia`, `architecture: lovelace`): `maxBufferSize = 4294967292`
bytes = 2³² − 4 = 4 GiB − 4 bytes. That reading is the **WebGPU spec / Chromium-Dawn buffer
ceiling** (buffer offsets fit in a `u32`), not a Vulkan interop artifact and not the card's VRAM.
A workstation with a 24 GB card on any Chromium-family browser (Chrome, Brave, Edge, Opera, …)
will report the same 4 GiB.

Consequence: `cacheHardCapMB = floor(4294967292 * 0.7 / 1024²) ≈ 2867 MB`. The 4 GB chip already
disables itself here. Adding 8 GB / 16 GB chips would render disabled on every Chromium-family
browser regardless of GPU, making them UI noise. **Reverted P1 in this branch — chip list stays
`auto / 512 / 1 GB / 2 GB / 4 GB`.**

Firefox reading not obtained: this machine has snap Firefox on hybrid Intel/NVIDIA PRIME, and
Firefox freezes when opening a Cecelia image on it (known blocker), so a `maxBufferSize` reading
from Firefox+NVIDIA on this box was not attainable. Chromium reading stands as the load-bearing
evidence.

The prompt's "Chromium + Vulkan capped around 2048 MB" observation is **not reproduced** in Brave
on this hardware. Either the underlying issue was addressed in Chromium/Dawn since, or the
original reading was misinterpreted (e.g. `maxUniformBufferBindingSize`, `maxStorageBufferBindingSize`,
or a different limit, misread as `maxBufferSize`). Retired as a hypothesis without a live target.

**What the real question is, and why this plan doesn't answer it:** the cache atlas is
`maxBufferSize`-bound today (comment at `ViewerWindow.vue:991`), and the browser caps
`maxBufferSize` at 4 GiB, so 2.87 GiB is the honest ceiling for the current architecture. Lifting
the ceiling means uncoupling the atlas from a single buffer allocation (chunked staging uploads,
or a texture-first layout that trades `maxBufferSize` for `maxTextureDimension3D * bytesPerVoxel`,
which on this laptop = 2048³ × 2 B = 17 GB). That is a **renderer architecture question**, not a
UI-list question, and belongs to whoever picks up the brick renderer next
(`BRICK_INTEGRATION_PLAN.md`, `BRICK_OCTREE_TRANSPLANTS_PLAN.md`).

## Measurement — what the bench blob actually said (2026-09-02)

Bench blob taken today at `~/Downloads/TMP/bench8/` on Dml3RG (zolIMa MERTK timecourse — 181 t × 4 c
× 37 z × 1039 × 1060 uint16, `nLevels: 1`), flat renderer, `cacheMB=2048`, ~11 s of aggressive
interaction. Compared against the archived Aug 29 brick-mode blob on the same image
(`~/Downloads/TMP/bench/bench-Dml3RG-brick-2026-08-29_02-18-55-783Z.json`).

| Metric | Flat (today) | Brick (Aug 29) |
|---|---|---|
| fps | 6.8 | 17.4 (**2.5×**) |
| draw p95 | 0.2 ms (CPU idle waiting on fetch) | 12 ms (GPU doing real work) |
| VRAM footprint | ~1.87 GB (6 tp × 311 MB) | ~620 MB (256 bricks × ~2.4 MB) |
| Timepoints resident | 4/181 | 5/181 (effective coverage similar, delivered from a much smaller cache) |
| Bytes fetched | 1.24 GB in 11 s (109 MB/s) | 7.9 GB in 101 s (78 MB/s) |
| % of atlas cap used | 65 % of 2.87 GB | **22 %** of 2.87 GB — headroom, not saturation |

**The ceiling is not the felt problem on this image; the flat renderer is.** Bricking already
outperforms flat by 2.5× at 1/3 the VRAM. Lifting the atlas cap wouldn't touch that gap — flat is
fetch-bound (181 × 311 MB = 56 GB fits no browser). Bricking sidesteps the arithmetic by holding
only visible bricks per timepoint. In brick mode on this image at the current 2.87 GB cap, the
atlas sits at 22 % — a bigger cap has nothing to allocate to yet.

## What actually helps — the pointer

**`docs/todo/BRICK_INTEGRATION_PLAN.md` → B3 "wire it"**: make bricks default via
`shouldUseBricks(meta)`. On Dml3RG (~1.1 Mpx per plane) that predicate flips to brick automatically
once wired. Branch: `feat/brick-followup`. That's the fix for the felt problem this investigation
was reaching around.

**Ceiling lift stays a Step 2 question, only relevant *after* brick-auto is default.** In brick
mode a bigger atlas would buy more resident bricks (less pop-in on scrub); the multi-3D-texture
per-channel refactor (N × `maxBufferSize` instead of 1 × `maxBufferSize`) becomes worth measuring
*then*, on brick-mode numbers, not flat. The bench that would test it: brick mode at 1024 / 2048 /
4096 MB budgets on the hardest image and see whether atlas footprint scales with cap or plateaus.
Filed as a follow-up phase under `BRICK_INTEGRATION_PLAN.md`, not a new plan.

## Original goal (retained for context)

_Superseded by the outcome above. Left here as a decision record._

Raise the Viewer Advanced popover's cache-size ceiling so a workstation with real headroom
(e.g. discrete 16 GB card → `cacheHardCapMB` ≈ 11 GB) isn't capped at 4 GB by the fixed chip list.
Do it without silently trusting the adapter-reported `maxBufferSize`, which is known to be an
**artifact** on Chromium + Vulkan on discrete NVIDIA (reports ~2 GB against a card that has far
more VRAM).

Origin: `docs/archive/cache-chip-ceiling-prompt.md`. Two changes and one non-change:

- **P1** — add larger fixed chips (8 GB, 16 GB) to `BASE_CACHE_OPTIONS`; existing disable-with-tooltip
  logic handles chips above `cacheHardCapMB`.
- **P2** — detect the browser/backend artifact and surface a compact inline hint under the chips, so
  a low reported cap on a discrete GPU doesn't read as "your hardware".
- **Not this plan** — Auto's target does not scale with actual VRAM. Prompt explicitly parks it as
  "intentionally conservative" for now.

## Context

Code today (`frontend/src/modules/ViewerWindow.vue`):

- `AUTO_CACHE_MB` (line ~994): target = `looksDiscrete ? 2048 : 512`; capped at
  `maxBufferSize * 0.7`.
- `BASE_CACHE_OPTIONS` (line ~1008): fixed list — `auto, 512, 1024, 2048, 4096` MB.
- `cacheHardCapMB` (line ~1015): `maxBufferSize * 0.7`, computed from `activeAdapter`.
- `CACHE_MB_OPTIONS` (line ~1019): each chip gets `disabled: mb > cacheHardCapMB` + tooltip.
- Template: `:3758` (ChipSelect using `CACHE_MB_OPTIONS`), `:3774` (existing `!looksDiscrete`
  amber warn — the copy pattern to follow for P2).

Two renderer OOM guards already exist (`volumeRenderer.ts:729`, `brickAtlasTexture.ts:87`), so
disabling oversized chips is UX (don't offer a value that WILL trip a guard), not correctness.

## Decisions

**D1 (2026-09-02) — fixed steps, not a dynamic "Max (N MB)" chip.** Reasons:

- `activeAdapter` genuinely flips between `renderer.adapter` and `tileRenderer.adapter`
  (line 917), so a live-relabeled chip would shift under the user when the tile pipeline turns on.
- `settings.viewerCacheMB` is a number (also mirrored in `?cacheMB=` URL query and the
  "Using: X MB" caption); a dynamic "Max" chip would need a sentinel value that leaks into all
  three surfaces.
- Fixed steps preserve the ChipSelect-value-is-a-number invariant; the existing `overCap` disable
  logic already communicates "this GPU can't take that value".

**D2 (2026-09-02) — `AUTO_CACHE_SAFETY = 0.7` stays for every chip.** The comment already flags it
as "tune when we've measured" and there's no OOM data. A per-chip margin variance would need an
argument that's not in the prompt.

**D3 (2026-09-02) — do NOT override the reported `maxBufferSize` when it looks suspicious.** The
renderer's OOM guards run against the actual cap; a UI override would just push the crash. The
backend-artifact hint (P2) tells the user what's happening; the fix is theirs (switch backend,
different browser), not the app's.

**D4 (2026-09-02) — backend-artifact heuristic.** `looksDiscrete && maxBufferSize ≤ 2 GB` is the
provisional signal. A real discrete card exposes ≥ 4 GB; ≤ 2 GB on a discrete adapter is the
Chromium + Vulkan interop cap the prompt describes. This is a **hypothesis until measured on at
least two setups** (Chromium-default vs Chromium + Vulkan on the same discrete NVIDIA); ship the
detection with a `// TODO(measure)` note against the threshold, and record the two readings in the
PR before merging P2.

**D5 (2026-09-02) — hint copy is a short inline caption, not a modal or a repair button.** Per
`docs/ui/COPY.md`: one phrase, ≤ 10 words. Mirrors the shape of the existing `!looksDiscrete` warn
at `:3774`. No auto-relaunch, no clipboard-paste command. The caption names the suspected cause
and the class of workaround; the user decides.

## Phases

### P1 — raise the fixed chip ceiling

Small, self-contained; no new code paths.

- `BASE_CACHE_OPTIONS` (line ~1008) gains two entries:
  - `{ value: '8192', label: '8 GB', mb: 8192, tip: 'Discrete workstation GPU with real headroom' }`
  - `{ value: '16384', label: '16 GB', mb: 16384, tip: 'Discrete workstation GPU with >20 GB VRAM' }`
- No change to `CACHE_MB_OPTIONS`: chips above `cacheHardCapMB` are already disabled with the
  existing tooltip.
- No change to `AUTO_CACHE_MB` (Auto's target stays 2048 for discrete — deliberately conservative,
  see D2).
- `cacheSeverity` (line ~1039) needs no change: the "amber above 50 % of cap" band still
  distinguishes a comfortable pick from an aggressive one.

**Checkpoint:** on a discrete 16 GB card, 8 GB is selectable and shows the amber "Using: 8192 MB"
caption; 16 GB shows disabled with the "Beyond this GPU's buffer cap" tooltip when the cap is
below 16 GB. On integrated, both stay disabled. No new template surface.

### P2 — backend-artifact hint

New surface, gated on the P1 code landing so the two effects are separable.

- New `computed` `backendArtifactSuspected`:
  ```
  const a = activeAdapter.value
  return !!a && a.looksDiscrete && a.maxBufferSize <= 2 * 1024 * 1024 * 1024
  ```
- New template line under the ChipSelect (near `:3774`), same `cc-muted-warn cc-fs-2xs` class:
  copy ≤ 10 words, e.g. *"Discrete GPU capped low — try `--use-angle=gl`"*. Exact wording via
  `docs/ui/COPY.md` review before commit.
- Do NOT hide or override the disabled 4 GB / 8 GB chips — the OOM guards are downstream of this
  hint's advice and the user might not act on it.
- Record the two measurement readings in the PR body (Chromium-default vs Chromium + Vulkan on the
  same discrete NVIDIA), confirming the ≤ 2 GB threshold before merging. If the split isn't clean,
  revise D4 in this file before shipping P2.

**Checkpoint:** on a Chromium + Vulkan discrete NVIDIA reading ~2 GB, the hint appears and the 4 GB
chip is disabled with the existing tooltip. On Chromium-default with the same GPU, the hint does
not appear.

## Not in scope

- **Auto scaling with actual VRAM.** Prompt parks it: `AUTO_CACHE_MB`'s discrete target stays 2048
  MB. If we lift it later, that's a separate plan grounded in OOM measurements.
- **Loosening `AUTO_CACHE_SAFETY = 0.7`.** Same reason.
- **App-side override of `maxBufferSize`.** See D3. The renderer's OOM guards run against the real
  cap; a UI lie would just relocate the crash.
- **Auto backend switch / relaunch button.** D5: hint, not repair. Feasibility of a repair action
  is out of scope of this UX popover.
- **A dynamic "Max (N MB)" chip.** See D1. Fixed steps only.

## References

- `frontend/src/modules/ViewerWindow.vue:989-1044` — `AUTO_CACHE_MB`, `BASE_CACHE_OPTIONS`,
  `cacheHardCapMB`, `CACHE_MB_OPTIONS`, `cacheSeverity`.
- `frontend/src/modules/ViewerWindow.vue:3758` — ChipSelect binding.
- `frontend/src/modules/ViewerWindow.vue:3774` — existing `!looksDiscrete` amber warn (copy shape
  for P2's hint).
- `frontend/src/rendering/volumeRenderer.ts:729`, `frontend/src/rendering/brickAtlasTexture.ts:87`
  — the two OOM guards D3 protects.
- `docs/archive/cache-chip-ceiling-prompt.md` — the ask this plan answers.
