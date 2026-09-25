# Design + build: WebGPU capability diagnostic for the Ubuntu workstation

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

## Context

Repo: `schienstockd/cecelia`, `frontend/src/`. Brick-based volumetric viewer over WebGPU.

All development to date has been on a laptop with a hybrid/integrated GPU. Actual test
hardware, now run once: **Ubuntu 24.04, NVIDIA RTX 2000 Ada (8GB VRAM), driver 580, CUDA
13.0**. That first pass already showed the bottleneck is **getting data onto the GPU** —
not texture allocation limits. That reframes what matters here:

**Prior investigation exists — read it first: PR #649** (`docs: napari vs WebGPU renderer
audit — measured, with a recommendation`, branch `audit/napari-webgpu`), specifically the
third commit (`a319f43`, "web viewer plan + real-data render, slider prototype, upload
probe"). It measured the upload-cold-path on this exact same RTX 2000 Ada and reported:
cost tracked rows (2.96×) not bytes (7.41×) when scaling payload size; a padded-vs-unpadded
256-byte-pitch test came back near-identical (214ms vs 209ms) as one candidate explanation;
and a `~100ms onSubmittedWorkDone` reading that came back flat across 35/88/92/163 MB
payloads, which the PR treats as a measurement-floor artifact rather than real GPU time.
Its unresolved next candidate was the JS-heap→GPU-visible staging copy
(`MAP_WRITE` + `getMappedRange`), left untested.

**Treat every one of those as a hypothesis to re-verify, not a settled conclusion** — don't
inherit "pitch doesn't matter" or "it's the staging copy" as given. Re-measure independently
on this run; if you land somewhere different from #649, say so and say why. The value of
that PR here is prior art and a starting point (what was tried, what tooling/harness they
used, what the readings looked like), not an answer to build on top of unquestioned.

1. **Upload path (primary target)** — `writeBrick` in `lib/webgpu/brickAtlasTexture.ts`
   does one `device.queue.writeTexture` call per brick (collapsed from N-per-channel in an
   earlier PR chain, #703, specifically because per-call driver-staging overhead was the
   measured problem then: mean 5.5ms, p99 44ms, max 412ms on a 4-channel ~4.85MB brick,
   before the collapse). Note: `writeBrick` itself does no repacking or copy — it wraps the
   incoming `data` in a zero-copy `Uint8Array` view and passes it straight to
   `writeTexture`; the payload already arrives in the layout the GPU wants. So any JS-side
   cost that matters is upstream of this file — in whatever decodes the
   `/api/viewer/slab` response into that typed array (`utils/brickLoader.ts` or wherever
   that lives). Don't assume where the cost sits — audit the actual call path end to end
   (fetch → decode/decompress → `writeBrick` → `writeTexture` → GPU) yourself and decide
   what stages are worth isolating; the breakdown above is a starting map, not a
   prescription.
2. **Atlas budget at 8GB** — `utils/brickAtlas.ts::pickAtlasLayout`'s VRAM budget
   (`budgetSlots = floor(vramBudget / oneBrickBytes)`) is now the likely-binding constraint,
   not `maxTextureDimension3D` — 8GB is comfortable but not huge, worth knowing the real
   number rather than assuming headroom.
3. **Texture dimension ceiling** — lower priority now (was the original concern before
   real hardware was tested; likely a non-issue at 8GB VRAM since the budget cap binds
   first, but still worth confirming `utils/webgpuProbe.ts` negotiates the adapter's real
   `maxTextureDimension3D` rather than falling back to the spec floor).

## Goal

Design and build a standalone diagnostic that runs on this workstation and answers: where is
the ceiling on getting brick data resident and visible — is it upload bandwidth (PCIe/driver
staging), atlas VRAM budget, or something else — and is the current code (batching, admission
caps, atlas sizing) already close to that ceiling or leaving throughput unused.

## Requirements

- Runs standalone in Chrome/Chromium on Ubuntu — no build step, no dev server, nothing beyond
  what's needed to open a file and click a button.
- **Upload path benchmark, front and center, broken into stages you determine by auditing
  the real call path** — don't collapse this into one number, and don't take the stage
  list below as given; trace `fetch → decode/decompress → writeBrick → writeTexture → GPU`
  yourself first and decide where the real seams are. As a baseline to react to, not a
  spec: JS-side decode/decompress, the staging copy into GPU-visible memory (`MAP_WRITE` +
  `getMappedRange`, or whatever path `writeTexture` actually takes under the hood — confirm
  which), the device-side transfer, and `onSubmittedWorkDone` as its own measurement
  (flagged as a possible floor/artifact per #649 — re-verify, don't assume).
- Re-run #649's specific tests independently: the row-vs-bytes payload scaling, and the
  256-byte-pitch aligned-vs-unaligned comparison. Confirm or contradict on this run.
- Measure sustained `writeTexture` MB/s into a real atlas-shaped `texture_3d` target, at
  brick-realistic transfer sizes (match `writeBrick`'s actual shape — one call per brick,
  `[bx, by, bz*nc]` box, r8uint/r16uint). Compare: (a) current one-call-per-brick pattern
  at realistic brick byte sizes, (b) a sweep across transfer sizes to find where per-call
  overhead stops dominating (the #703 finding), (c) if feasible, a same-total-bytes
  comparison of many-small-calls vs fewer-larger-calls. Report MB/s and mean/p95/max
  latency, not just an average.
- Report real negotiated WebGPU device limits (`maxTextureDimension3D`, `maxBufferSize`,
  `maxStorageBufferBindingSize`) and confirm via actual `createTexture` allocation — lower
  priority than the upload benchmark, but cheap to include and closes out the earlier
  open question.
- Run `pickAtlasLayout` (`utils/brickAtlas.ts`) against real brick configs at this device's
  actual 8GB budget (not a hypothetical 32GB one) — report real resident-brick capacity.
- Flag if Chrome picked a fallback/software adapter instead of the RTX 2000 Ada
  (`adapter.isFallbackAdapter`) — would produce misleading numbers that look like a code or
  hardware problem but are actually a driver/Vulkan setup issue on this fresh box.
- Output should be easy to hand back — copy-as-text or otherwise captureable without a
  screenshot.

## Out of scope

Don't touch or propose changes to the actual renderer code yet — this is a read-only
diagnostic to gather real numbers first. `MAX_INFLIGHT`/network fetch throughput is a
separate concern from GPU upload (needs the live API server) — note whether it's worth
covering here or as a follow-up once the upload-path numbers are in, since the two could
compound (network delivers the brick, then upload stalls getting it resident) in ways that
are easy to misattribute to one or the other without isolating them.

## Deliverable

The diagnostic itself (single file, runnable as described above), plus a short note on what
you chose to measure and why, and anything in the existing atlas-sizing code that looked
questionable while you were mirroring its logic.
