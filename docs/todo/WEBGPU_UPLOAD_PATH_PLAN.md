## WebGPU viewer upload path — diagnose then close the gaps

**Status:** planning (2026-09-15) · branch `feat/webgpu-upload-path` (main), plus
`feat/api-tls-http2` for U4. Diagnostic built + laptop numbers landed (see *Numbers*).
T1.1 clamp + T2a Method B upload shipped in this branch. Workstation numbers pending.

**Owns:** the whole load path from `/api/viewer/slab` HTTP response to a resident brick in
the atlas texture, and every constant that gates it (`MAX_INFLIGHT`, `DEFAULT_ATLAS_BUDGET`,
brick shape, `pickAtlasLayout`, `writeBrick`). Does NOT own: renderer WGSL, LRU eviction
policy, chain-scheduler cross-cutting concerns.

**Motivation:** the WebGPU viewer is how immunologists look at their imaging data. If time
is being lost anywhere in the pipeline — network, decode, JS-heap alloc, staging copy, atlas
sizing — that's a paper cut on every scrub, every playback, every load. #649 and #703 each
chipped one candidate off; this plan is the systematic pass that says "we know where the
milliseconds go, and we know what would move them."

## What we know now (before running the diagnostic)

Read from the code as of 6192da94; every quantitative claim below is a code observation, not
a measurement — that's what the diagnostic exists to close.

1. **The shipped upload shape is already optimised.** `writeBrick`
   (`frontend/src/lib/webgpu/brickAtlasTexture.ts:114-148`) issues **one**
   `queue.writeTexture` per brick with `[bx, by, bz*nc]` box — the collapse #703 measured
   as bringing mean from 5.5 ms → 0.71 ms. The `new Uint8Array(data.buffer, ...)` at line
   140 is a zero-copy view.
2. **The scheduler doesn't await `onSubmittedWorkDone` per brick.** The ~100 ms floor #649
   measured is a benchmark artefact from awaiting completion; the real playback path fires
   writeTexture and returns. So the 100 ms isn't a per-brick cost in production.
3. **The single-atlas ceiling on Chromium is ≈ 4 GiB regardless of card**
   (memory: `project_webgpu_maxbuffersize_cap.md`). Dawn caps `maxBufferSize` at 2³² − 4.
   The RTX 5000 workstation's 32 GB VRAM is unreachable through one atlas texture.
4. **`DEFAULT_ATLAS_BUDGET = 512 MB`** (`brickVolumeRenderer.ts:80`). On a 32 GB card that's
   1.5 % utilisation. Any user who never visits Settings runs at 512 MB.
5. **The caller of `pickAtlasLayout` has no `min(budget, maxBufferSize)` clamp**
   (`brickVolumeRenderer.ts:527-532`): a large-budget setting on the workstation silently
   error-toasts because `validateAtlasLayout` rejects the resulting VRAM demand. There's no
   fallback path.
6. **Dawn on Linux Vulkan has historically reported `maxTextureDimension3D = 2048`** on
   NVIDIA discrete cards (`webgpuProbe.ts:6-8`). If the RTX 5000 comes back with 2048, deep
   bricks (Dml3RG-shape: bz=128, nc=4) hit `maxZperAxis = 2048/(128*4) = 4` slots along z.
   That's an atlas-geometry ceiling *before* the VRAM budget bites.
7. **The likely-JS-side cost is `Response.arrayBuffer()`, not `MAP_WRITE` staging.**
   `brickLoader.ts:273` allocates a fresh ArrayBuffer sized to the whole response and
   memcpys the body into it, THEN `writeBrick`'s zero-copy view wraps it. #649 pointed at
   MAP_WRITE as the untested candidate; arrayBuffer sits *upstream* of that.
8. **`padBrickPayload` is a per-Z-edge-brick JS memcpy** (`brickLoader.ts:221-245`). On
   stores where `nZ % brickZ ≠ 0` (SRPabw was the poster case: nZ=193, brickZ=128 →
   last-Z brick nz=65) every last-Z brick pays an alloc + row-loop copy. Nothing else in
   the codebase measures it.
9. **`MAX_INFLIGHT = 16` was validated on one bench**
   (`brickVolumeRenderer.ts:82-101`, PR #893): a 2h06xA store over a network link at
   ~25 MB/s. Raising to 32 did nothing there — but that says nothing about a fast local
   pipe on the workstation where the API + browser run on the same box.
10. **HTTP/2 vs HTTP/1.1 is not known frontend-side.** The browser caps at 6 concurrent
    connections per host on HTTP/1.1. If the Julia server serves HTTP/1.1, the effective
    inflight ceiling is 6, not 16, regardless of the constant.

## Diagnostic — the source of truth for phase gating

Two files, on the `docs/webgpu-diagnostic` branch:

- **`docs/todo/spike/webgpu/diagnostic.html`** — 8 sections, JSON report.
  Sections A–G run standalone (open the file, no server); §H needs the local server.
  Precedent for the shape: `docs/todo/spike/webgpu/chunk_server.py` (PR #649).
- **`docs/todo/spike/webgpu/diagnostic_server.py`** — throwaway HTTP server on
  127.0.0.1:7789 (off every cecelia app port). Serves synthetic bricks with the same
  `X-Slab-Shape` header format the real `/api/viewer/slab` route uses. Optional real-brick
  endpoint reads from a project zarr level.

Run: `pixi run python docs/todo/spike/webgpu/diagnostic_server.py --port 7789` then open
`http://127.0.0.1:7789/`. Click **Run all**, copy the JSON report back.

Sections and the question each answers:

| § | Question | Gate for phase |
|---|---|---|
| A | What does this device really negotiate? Is `maxTextureDimension3D` 2048 or higher? Is a fallback adapter in use? | U0, U5 |
| B | At each candidate atlas budget, what is the real slot capacity and does it exceed `maxBufferSize`? | U1, U5 |
| C | Where does one brick's cost sit — writeTexture vs writeBuffer+copy vs MAP_WRITE vs the resident-buffer floor? Includes `Response.arrayBuffer()` and `padBrickPayload` proxies | U2, U3, U4 |
| D | Does upload cost track rows (#649's finding) or bytes, on this hardware? | U2 |
| E | Does 256-byte pitch alignment matter, on this hardware? | U2 |
| F | Sustained MB/s at brick-realistic shape, with mean / p95 / max — does #703's tail collapse still hold? | U2, U3 |
| G | Same total bytes, many-small vs few-large — is one-call-per-brick already optimal? | U2 |
| H | At concurrency N = 1, 4, 8, 16, 32, 64, does uncompressedMbps rise past N=16? Does identity-vs-zstd swap the answer? Server-Ms vs wire vs arrayBuffer vs upload attribution per brick | U4, U6 |

## Locked decisions

**Decision 1 — this plan is diagnosis-first, no code changes until numbers come in.**
Every phase below is guarded by "the diagnostic reported ≥X% cost sits here." Skips are
recorded — the deliverable of a "cheap, didn't move the needle" phase is a numbered row in
the report, not silence.

**Decision 2 — the diagnostic is the source of truth, not #649 or #703.** Prior spike blobs
are reference; re-verify every claim on this run. #649's row-cost, pitch alignment and
100 ms `onSubmittedWorkDone` floor are re-measured in §D, §E, §C.

**Decision 3 — measure BOTH laptop and workstation.** Chromium/Dawn caps for
`maxBufferSize` are the same on either machine; §A reveals whether
`maxTextureDimension3D` matches too. Everything under §C/D/E/F/G/H scales with GPU + PCIe.
Two JSON reports, both attached to this plan.

**Decision 4 — write nothing until the diagnostic runs.** Applies to `frontend/`,
`app/src/`, and `api/src/`. The plan can be re-scoped after numbers; that's cheaper than
un-shipping a change that didn't move the needle.

**Decision 5 — `pickAtlasLayout` is a shared helper, keep it that way.** Any budget-clamp
or multi-atlas support lands where the current code already reads (utils, not the renderer).
No parallel geometry math anywhere else — the memory rule `feedback_use_existing_framework`
applies.

**Decision 6 — HTTP-side changes (concurrency, protocol) are shipped iff §H shows real
throughput past N=16 on the local pipe.** #893 already settled the network-limited case;
this plan only overrides that if the fast-pipe data warrants it.

## Phases

Each phase is independently shippable. Phase U0 must land first; the rest are ordered by
expected-impact but can reorder based on §H results.

### U0 — Land the diagnostic and gather two runs. GATES EVERYTHING.

- Commit `docs/todo/spike/webgpu/{diagnostic.html,diagnostic_server.py}` + this plan +
  the README row. One PR, docs-only.
- Run on the laptop, save `docs/todo/spike/webgpu/reports/u0_laptop.json`.
- Run on the RTX 5000 workstation once it's up. Save
  `docs/todo/spike/webgpu/reports/u0_workstation.json`.
- Attach both to this plan's *Numbers* section (added below when they land).

**Deliverable:** two JSON blobs + a two-paragraph read-out at the top of this plan naming
the actual bottleneck for each machine.

### U1 — Budget clamp + soft-fallback in the atlas caller.

**Trigger:** §B shows any oversized layout on either machine's negotiated `maxBufferSize`
at the shipped `DEFAULT_ATLAS_BUDGET` or a user-set budget the Settings UI can produce.

**Change (`brickVolumeRenderer.ts:527-532`):**
```
const budget = Math.min(budgetBytes > 0 ? budgetBytes : DEFAULT_ATLAS_BUDGET,
                        limits.maxBufferSize)
```
Clamp keeps a "give me 32 GB" setting from silently erroring. Emit an info-level log line
naming the clamp when it fires, so users can see they're not getting what they asked for.

**Deliverable:** the clamp, a unit test in `frontend/src/utils/brickAtlas.test.ts` for the
clamp behaviour, one release note.

### U2 — Raise `DEFAULT_ATLAS_BUDGET`, or make it hardware-adaptive.

**Trigger:** §F sustained MB/s + §B budget probe indicate the current 512 MB default is
leaving working-set on the table on any tested hardware.

Options in preference order:
1. **Bump to `min(2 GB, 0.5 * maxBufferSize)`** — a static bump that respects the Chromium
   4 GiB cap and gives every discrete card 2 GB of atlas by default.
2. **Adaptive default:** at atlas creation, request the largest layout that fits and cap
   at 4 GiB. Downside: harder to reproduce a bug report ("what atlas size did the user
   have?"); mitigated by logging the resolved bytes to the console.

**Deliverable:** one of the two, `docs/UI.md`'s Settings section updated,
`docs/todo/spike/webgpu/reports/u2_before.json` and `u2_after.json` on the same store
demonstrating the working-set improvement.

### U3 — Persistent staging buffer, kill `Response.arrayBuffer()` allocation per brick.

**Trigger:** §C shows `Response.arrayBuffer()` costs > 10 % of total brick cost on either
machine, AND §H shows `arrayBufferMs` scaling badly with concurrency.

**Shape:** replace `res.arrayBuffer()` in `brickLoader.ts::fetchBrick` with a
`res.body.getReader()` stream that writes directly into a ring of reusable ArrayBuffers
sized to the max brick payload (`brickPayloadBytes(layout)`). One ring, N =
`MAX_INFLIGHT`. Callers get an index into the ring and a length; `writeBrick`'s zero-copy
view already accepts a byteOffset.

**Risks:** stream-decoded body may not come in one chunk; the ring gets contended if
`MAX_INFLIGHT` grows. Sized: pre-U3 estimated 1 ms/MB in `res.arrayBuffer()`, so a 4 MB
brick saves ~4 ms JS-side; multiplied by working-set-refill on scrub, that's real. Confirm
in the §C read-out before shipping.

**Deliverable:** ring implementation in `utils/brickPayloadRing.ts` (new), `fetchBrick`
switched, `brickLoader.test.ts` updated, before/after numbers.

### U4 — HTTP/2 on `api/` via self-signed TLS. TRIGGER FIRED (2026-09-15).

**Confirmed HTTP/1.1** via `curl -sI --http2 http://127.0.0.1:8080/api/version` →
`protocol=1.1`. §H measured this bites: wire time climbs from 4.5 ms (N=1) to 140 ms
(N=32) as Chromium queues behind its 6-per-origin cap. HTTP/2 multiplexes on one socket
and kills the queue; projected wire at N=16 drops from ~60 ms to ~5 ms per brick,
saving ~0.9 s per 100-brick refill on a scrub.

**HTTP.jl v2.4.0 already supports HTTP/2 server-side** (`AeVvA/src/http_server.jl:1` —
"Shared HTTP server kernel for HTTP/1, TLS, and HTTP/2"). Not a library gap.

**Blocker: browsers refuse cleartext HTTP/2 (h2c).** Chromium's stated policy — no h2c,
ever. HTTP/2 requires TLS via ALPN.

**Locked decision — self-signed TLS, NOT mkcert / not a reverse proxy** (peer-confirmed
2026-09-15). Rationale: researchers install cecelia locally, one-time-per-install click-
through is a reasonable tax; mkcert would bundle a third-party binary + escalate to write
the system trust store on three OSes as part of install (real packaging surface). If the
click-through turns out to be a support/friction problem in practice, revisit.

**Shape:**
1. `api/src/server.jl:714` — swap `HTTP.listen` to `HTTP.listen!(..., sslconfig=…)` when a
   cert exists at a known dev-config path (e.g. `<CECELIA_DEV_DIR>/tls/{cert,key}.pem`).
2. Generate the self-signed cert on first `pixi run dev` via a small task (`pixi run cert`
   as a bootstrap, invoked implicitly by `dev` when missing). OpenSSL one-liner or the
   Julia `MbedTLS`/`OpenSSL_jll` equivalent.
3. Frontend: swap `http://` → `https://` and `ws://` → `wss://` at the URL construction
   sites. Config-driven so dev + install both work.
4. `docs/INSTALL.md`, `docs/SHIPPING.md`: add the accept-the-cert note.
5. Fallback: if no cert present, `HTTP.listen` stays HTTP/1.1 on the same port (or a
   different port) so existing installs don't hard-break.

**Composes with U4b — bulk-brick endpoint.** Ship whichever lands first; they eat the
same problem (fewer round trips) from different angles and don't conflict.

**Deliverable:** working HTTPS on `api/`, HTTP/2 negotiated on the wire (verified by
re-running `curl -sI --http2 https://…/api/version → protocol=2`), `?cert=…` config
docs.

### U5 — Multi-atlas support.

**Trigger:** §A returns `maxTextureDimension3D = 2048` on the workstation (Dawn/Linux
Vulkan case), OR §B says even at 4 GiB `maxBufferSize` the working-set-per-image exceeds
one atlas' capacity on any real store.

**Shape:** `pickAtlasLayout` gains a `maxAtlases` param and can return an array of layouts
covering N atlas textures. `PageTable` gains an atlas index per slot. `writeBrick` routes
to atlas[slot / perAtlasCapacity]. Shader binds N atlas textures at build time (bounded —
say max 4 — WGSL doesn't do runtime-sized bind groups).

**Cost:** real, multi-week. Only shipped if U5's trigger actually fires. The `oversized
layouts` list from §B is the input.

**Deliverable:** design sub-plan (`docs/todo/WEBGPU_MULTI_ATLAS_PLAN.md`) if this fires;
implementation phased separately.

### U6 — `padBrickPayload` allocation elision.

**Trigger:** §C's padBrickPayload row shows > 2 % of total per-brick cost AND user hits
a store where `nZ % brickZ ≠ 0`.

**Shape:** reuse the same payload ring from U3 (if U3 shipped) — pad in place instead of
allocating. If U3 didn't ship, skip: the alloc is edge-brick-only and unlikely to matter.

**Deliverable:** patch to `brickLoader.padBrickPayload`, or a note explaining why the
measurement said skip.

## Retiring this plan

- After each U-phase ships, add a row to the *Phases* table with the PR link and the
  measured impact.
- When all triggered phases have shipped OR been explicitly declined based on numbers,
  promote the durable parts (the atlas-budget clamp rule, the staging-ring rule, the
  multi-atlas contract if built) into `docs/ARCHITECTURE.md`'s viewer section, then move
  this plan to the "Closed" section of `docs/todo/README.md` with a link to the
  architecture-doc section that superseded it.

## Numbers

### Laptop, RTX 2000 Ada 8 GB, Chromium 151 / Dawn / Linux Vulkan (2026-09-15)

Full report: `~/Downloads/prompts/cecelia-webgpu-diagnostic.json`. Highlights:

**§A device probe.** `maxTextureDimension3D = 2048` (Dawn/Linux Vulkan spec floor on a
discrete NVIDIA — the case `webgpuProbe.ts:6-8` warned about; **CONFIRMED**).
`maxBufferSize = 4 GiB − 4` (Chromium/Dawn cap). Not fallback adapter.

**§B atlas geometry at real limits.**
- At 512 MB budget: fXgbTl-shape 128 slots, Dml3RG-shape 32 slots.
- At 2 GB budget (Auto for discrete): fXgbTl 512, Dml3RG 128.
- At 4 GiB budget (Chromium cap): fXgbTl 1014, Dml3RG 252.
- At 8 GB and 32 GB budgets: **`validateAtlasLayout` rejects every preset** — single-atlas
  cannot exceed 4 GiB `maxBufferSize`. So 5+ GB of the laptop's VRAM is architecturally
  unreachable; more on the workstation.

**§C upload path, 4 MB brick, r16uint** (median submitted latency):
- A (`writeTexture`, shipped): 7.1 ms
- B (`writeBuffer + copyBufferToTexture`): **3.2 ms**
- C (`mappedAtCreation + copyBufferToTexture`, "MAP_WRITE candidate" #649 flagged):
  6.4 ms — **refutes #649**, MAP path is worse than plain writeBuffer at this shape.
- D (resident-buffer copy floor): 2.5 ms

**Method B beats the shipped Method A by ~4 ms per brick.** New lead. See U2 below.

Decode + edge-brick memcpy (JS-side, per brick):
- `Response.arrayBuffer()` (Blob proxy): **6.7 ms median, 626 MB/s** — real cost per brick.
- `padBrickPayload` shape: **1.9 ms median** on Z-edge bricks.

**§D row-vs-bytes**: cost tracks BOTH rows and bytes, not one cleanly. µs/row rises 1.12 →
2.42 across widths 441 → 2048 (unaligned to aligned). #649's clean row-cost story is
weaker on this hardware.

**§E 256B pitch alignment**: irrelevant. Aligned 1152 (61 ms) came in slower than
unaligned 1104 (56 ms). Confirms #649: alignment is not the story.

**§F sustained**: 128 × 4 MB bricks, per-call mean 0.3 ms, max 0.5 ms — **no tail. #703's
one-call collapse is holding.** Aggregate 1715 MB/s completion.

**§G granularity**: sweet spot 1–4 MB per call. 64 KB calls carry 43 µs/call overhead ×
1024 = 44 ms wasted; 16 MB calls slower per byte. Current 1 MB brick is right.

**§H live pipeline, MAX_INFLIGHT sweep** against `diagnostic_server.py` (localhost,
synthetic bricks, K=64):

| N | uncompressedMbps | wireMs (med) | arrayBufMs (med) | uploadMs (med) |
|---|---|---|---|---|
| 1  | 256 | 4.5 | 3.4 | 0.3 |
| 4  | 227 | 5.6 | 11.5 | 0.4 |
| 8  | 188 | 24 | 12.6 | 0.4 |
| 16 | 194 | 60 | 8.1 | 0.4 |
| 32 | 276 | 140 | 3.8 | 0.4 |

Throughput plateaus 200–280 MB/s. **`MAX_INFLIGHT = 16` is not obviously mis-tuned**;
raising to 32 gains 42% on this pipe but noisy — not the "clear throughput on the table"
case that would justify a change on its own. wireMs climb from 4.5 → 140 ms is Chromium's
HTTP/1.1 6-per-origin socket cap against Python's http.server (see U4).

**Read-out**: **the bottleneck is not the GPU side.** `writeTexture` is 0.3 ms, no tail.
The pipeline pain is JS + HTTP: 6.7 ms per brick in `Response.arrayBuffer()` alloc,
socket-cap queueing when N > 6. The single-atlas 4 GiB Chromium cap is the biggest
architectural ceiling — 5+ GB of the laptop's card unreachable today.

### Workstation — pending

RTX 5000 / 32 GB / 40 cores / 256 GB. Expected: `maxTextureDimension3D` almost
certainly 2048 (same Dawn/Linux Vulkan case); §C/D/E/F absolute numbers scale with PCIe;
§H MAX_INFLIGHT curve depends on server-side (see U4).

## Phase re-ordering after laptop numbers

- **U1 shrinks**: the budget clamp is still valid (people can pick 4 GB in Settings on a
  device where 0.7 × 4 GiB = 2.87 GiB is the real max), but the "bump default"
  half of U1 is DELETED. Auto already ships 2 GB on discrete via `AUTO_CACHE_MB`
  (`ViewerWindow.vue:1192-1198`). My earlier claim of "512 MB default is 1.5% utilisation"
  was wrong — that constant only fires when the caller passes `budgetBytes = 0`.
- **U2 → U2a + U2b** (both worthwhile):
  - **U2a**: switch `writeBrick` to `writeBuffer + copyBufferToTexture` into a persistent
    staging buffer. Measured 4 ms per-brick win in §C. New lead not in original plan.
  - **U2b**: the "raise default budget" phase is deleted; only the clamp remains.
- **U3 promoted**: staging ring for `Response.arrayBuffer()` — 6.7 ms per brick, the
  biggest single JS-side win in the trace.
- **U4 sharpened**: HTTP/2 verification on the real `api/`. `curl -I --http2` check first;
  fix `api/` only if HTTP/1.1 is being negotiated. Concurrency-cap raise (from 16 → 32)
  is NOT triggered by these numbers on their own.
- **U5 upgraded from conditional to committed**: laptop already leaves ~5 GB of VRAM
  stranded by the 4 GiB Chromium buffer cap; workstation will strand more. Multi-atlas
  is the ceiling raise. Sub-plan `WEBGPU_MULTI_ATLAS_PLAN.md` follows.
- **U6 unchanged**: fold into U3's ring once that ships.
