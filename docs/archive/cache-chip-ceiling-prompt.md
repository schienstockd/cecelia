> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/todo/CACHE_CHIP_CEILING_PLAN.md`.
>
> **Outcome (2026-09-02): premise did not hold; no code shipped.** Brave on RTX 2000 Ada laptop
> reads `maxBufferSize = 2³² − 4 = 4 GiB` — the Chromium/Dawn spec ceiling, not a Vulkan artifact
> and not the card's VRAM. A 24 GB workstation on any Chromium-family browser reports the same
> 4 GiB, so 8 GB / 16 GB chips would disable on every GPU. Lifting the ceiling is an atlas-vs-buffer
> renderer architecture question, out of scope for a chip-list edit. See
> `docs/todo/CACHE_CHIP_CEILING_PLAN.md` for the measurement + reasoning.

# Viewer Advanced popover — Cache chip ceiling

## Context

Commit `e957608` (`frontend/src/modules/ViewerWindow.vue`) made the Cache
size chips GPU-aware:

- `AUTO_CACHE_MB`: Auto target is `looksDiscrete ? 2048 : 512` MB, capped at
  `maxBufferSize * 0.7`.
- `BASE_CACHE_OPTIONS`: fixed chip list — `auto, 512, 1024, 2048, 4096` MB.
- `cacheHardCapMB` = `maxBufferSize * 0.7`, computed from the active WebGPU
  adapter.
- Chips whose `mb` exceeds `cacheHardCapMB` render disabled with a tooltip;
  chips within the cap stay selectable regardless of how much further below
  the cap they are.

## Problem

On a discrete GPU with real headroom (e.g. 16 GB card → `cacheHardCapMB`
≈ 11 GB), every chip up to 4096 MB stays enabled, but there is no chip above
4 GB. The static list under-serves workstation GPUs even though the safety
math (`cacheHardCapMB`) already supports a much larger value.

Separately, Auto's target is hardcoded to 2048 MB for *any* discrete GPU —
does not scale with actual VRAM. Treated as intentionally conservative and
out of scope here.

## Additional wrinkle: `maxBufferSize` can itself be wrong

Observed case: a Chromium + Vulkan setup on an NVIDIA discrete GPU reports
`maxBufferSize` capped around 2048 MB — well below the card's actual VRAM.
This is a browser/backend (Vulkan interop) limitation, not the GPU's real
ceiling. Consequence: `cacheHardCapMB` (70% of that) lands around 1433 MB,
which disables the 2048 and 4096 chips even though the hardware could
handle far more.

So the fix isn't only "raise the chip ceiling for GPUs with real headroom"
— it's also "the adapter-reported cap may itself be an artifact of the
browser's graphics backend, not the hardware." Worth having Opus reason
about whether/how to detect this case (e.g. known discrete NVIDIA/AMD GPU
but suspiciously low `maxBufferSize`) and surface it to the user — e.g. a
hint like "Your browser's Vulkan backend may be limiting available VRAM;
try Chromium's `--use-angle=gl` or an alternate backend" — rather than
silently treating the reported cap as ground truth.

## Proposed change (to reason about)

1. Add an 8192 MB (8 GB) chip to `BASE_CACHE_OPTIONS`.
2. Replace the fixed ceiling with a dynamic top chip — "Max (`cacheHardCapMB`
   MB)" — reflecting the actual adapter's cap, in addition to the fixed
   512/1024/2048/4096/8192 steps.
3. Leave `AUTO_CACHE_SAFETY = 0.7` unchanged — comment already flags it as
   "tune when we've measured"; no OOM measurement data yet to justify
   loosening it.

## Ask

Reason about:
- Whether a dynamic "Max" chip is the right UX vs. just adding fixed 8/16 GB
  steps.
- Any risk in exposing `cacheHardCapMB` directly as a selectable value
  (edge cases: adapter reports an unrealistic `maxBufferSize`, chip label
  needing to update live if `activeAdapter` changes).
- Whether 0.7 safety margin should differ for a "Max" chip specifically vs.
  the fixed steps.
- How to distinguish a real hardware `maxBufferSize` ceiling from a
  browser/backend artifact (e.g. Chromium's Vulkan interop capping a
  discrete NVIDIA GPU well below its actual VRAM), and whether/how to
  surface that distinction to the user instead of trusting the adapter
  value unconditionally.
- Implementation shape within the existing `computed(() => ...)` pattern for
  `CACHE_MB_OPTIONS`.

Relevant code: `frontend/src/modules/ViewerWindow.vue`, lines ~874-947 and
~3390-3437 (template), ~4150-4155 (CSS).
