# Consistency and coherence audit — Run 1 (mechanical baseline)

**Status:** **draft** (2026-11-25, `origin/main` 96a2a2fc). Trimmed Run 1 of the archived brief
[`../archive/CONSISTENCY_COHERENCE_AUDIT_PROMPT.md`](../archive/CONSISTENCY_COHERENCE_AUDIT_PROMPT.md):
steps 3–4 only (mechanical sweeps + baseline). Steps 1–2 (guard map, invariants) were already done
elsewhere — see the brief's banner. Run 2 (judgement, lineage) is parked. Nothing built, no repo
dependencies added; every tool ran from a scratch dir with `npx -y` / `uvx`.

## 1. Summary

- **Duplication is low:** jscpd finds 176 clones, 0.81 % of 222 k lines. Julia carries most of them
  (104 clones, 934 lines), mostly task preambles.
- **The one pattern worth a ratchet:** tasks rebuild image paths by hand. 29 lines in 28 files spell
  `dirname(dirname(img._dir))` and 23 lines in 23 files join `"0", img.uid`, though `img_project_dir` / `img_zero_dir` /
  `img_filepath` (`app/src/model/image.jl:90-113`) exist (`img_zero_dir` since the first commit). No ratchet
  bans it.
- **Import cycles are mostly a tool artefact:** madge reports 286 frontend cycles, 99 without type
  imports, and only **2 with every edge static**. 91 of the 99 pass through one dynamic
  `import('../main')` in `stores/ws.ts:432`.
- **Cross-layer route shapes agree:** every `/api/...` literal in the frontend and in MCP/Python has a
  Julia route. Five routes have no caller outside tests.
- **Doc ↔ code identifier drift is ~1 %:** 20 of 1,977 identifiers cited in `docs/inventory/*.md`
  aren't in the source. Some are real (the retired `napari_utils` is still described).
- **Not covered by any tool here:** Julia dead code, Julia cycles (one module, `include` order) and
  Python import cycles. No good off-the-shelf tool exists for Julia (§4.6).

## 2. Existing guards — see the brief's banner

Not repeated. Short version: recital runs 3 reviewers and 4 mechanical checks per diff. ~20
whole-repo ratchet tests run in CI with capped baselines. The weekly judge promotes rules broken in
3+ sessions to `tighten` / `ratchet` proposals (`docs/ai-assist/WEEKLY_JUDGE.md` step 5). The gap
this run fills is **whole-repo counts from off-the-shelf tools, tracked over time**. Nothing in the
current stack produces those.

## 3. Invariants

Stub. The documented invariants are `CLAUDE.md`, `app/CLAUDE.md`, `frontend/CLAUDE.md`,
`docs/MAINTAINABILITY.md` and `docs/ARCHITECTURE.md` → *hidden invariants*, plus the tests that
enforce them ([`../archive/ratchet_inventory_audit.md`](../archive/ratchet_inventory_audit.md)). One
**implied, unenforced** invariant surfaced this run: *an image's on-disk paths come from its own
accessors* (§4.1).

## 4. Mechanical findings

### Baseline (2026-11-25, 96a2a2fc)

| # | Check | Tool / command | Runtime | Count |
|---|---|---|---|---|
| 1 | Duplicate blocks (≥ 70 tokens) | `npx jscpd@4.0.5 app/src api/src python/cecelia frontend/src --min-tokens 70` (tests excluded) | 29 s | 176 clones · 1,795 lines · 0.81 % |
| 1a | — per format | | | julia 104 / 934 · markup (Vue) 32 / 401 · ts 30 / 318 · python 10 / 142 |
| 2 | Frontend import cycles, all imports | `npx madge@8.0.0 --circular --extensions ts,vue` | 17 s | 286 |
| 2a | — type imports skipped | madge API, `skipTypeImports` | 4 s | 99 |
| 2b | — every edge a static import | 2a, filtered by a scratch script | <1 s | **2** |
| 3 | Unused exports / types (frontend) | `npx knip@6.45.1` (5.x crashes on TypeScript 6) | ~10 s | 162 exports · 161 types · 1 dep · 1 devDep · 2 unlisted · 0 files |
| 4 | Python dead code (≥ 80 % confidence) | `uvx vulture@2.14 python/cecelia --min-confidence 80` | 1 s | 4 |
| 5 | `/api` literal with no Julia route | scratch script over string literals | <1 s | frontend 0 / 257 · MCP+Python 0 / 94 |
| 5a | Julia route with no caller literal | same | | 13 (6 scanner misses → **5 real**, 1 unverified, §4.4) |
| 6 | Inventory identifier not in source | scratch script, backticked `name(` / `snake_name` in `docs/inventory/*.md` | <1 s | 20 / 1,977 |
| 7 | Hand-built image paths | `grep 'dirname(dirname(img._dir))'` · `grep 'joinpath(<proj>, "0", img.uid'` | <1 s | 29 lines in 28 files · 23 lines in 23 files |

Total wall time ≈ 1 min, no LLM tokens.

### False-positive spot check (12 findings)

| Finding | Evidence | Verdict |
|---|---|---|
| Cycle `stores/ws.ts → main.ts → …` (91 of 99 cycles) | `stores/ws.ts:432` is `void import('../main')`, a dynamic import inside a handler | **FP** — madge counts dynamic imports |
| Cycle `plots/plot.ts ↔ plots/categoryColours.ts` | `plot.ts:29` imports `categoryColours`; `categoryColours.ts:7` imports `POPULATION_PALETTE` | TP, low — a constant in the wrong file |
| Cycle `stores/projectMeta.ts ↔ stores/animation.ts` | `projectMeta.ts:8`, `animation.ts:8` | TP, low — Pinia stores used inside functions, safe at load |
| Clone `editImages/bin.jl:73` ≈ `resampleZ.jl:57` (30 lines) | Same preamble: read ccid, hand-join `proj_dir/"0"/uid`, find set | **TP** — leads to §4.1 |
| Vulture: `drift_corrected_path`, `chunk_size` unused (`correction_utils.py:1629-1630`, `:1824`) | No caller passes them (`tests/test_correction_utils.py:59,69,205`) | TP, low — dead params |
| Vulture: `_trimesh` unused (`measure_utils.py:39`) | `try: import trimesh` availability probe | **FP** |
| Knip: `regl-scatterplot` unused dependency (`package.json:24`) | Only named in a comment (`main.ts:171`) and a test string | **TP** |
| Knip: `playwright-core` unused devDependency (`package.json:35`) | Used by `scripts/lib/chromium.mjs`, outside `frontend/` | **FP** — knip sees only `frontend/` |
| Knip: 4 sampled unused exports (`ridgesVis.ts:171`, `shaderSource.ts:35`, `afVis.ts:212`, `density.ts:37`) | Each used only in its own file | TP, no value — a stray `export` keyword |
| Route `/api/kiwi/profiles/*` uncalled (6) | `utils/profileApi.ts:121,135` build `${apiBase}/api/...` | **FP** — scanner missed the template prefix |
| Routes `/api/gating/density`, `/api/system/weights(+/fetch)`, `/api/viewer/record-test`, `/api/kiwi/refs/cells` uncalled | Only `server.jl`, their handler, `api/test/` and docs name them | TP (inference: dead or external-only) |
| Inventory cites `napari_utils` `add_image` / `add_tracks` / … (`docs/inventory/PYTHON.md:32`) | File gone since `92f58092d` (2026-08-31, napari retired) | **TP** — stale doc row `test_doc_pointer_convention` can't see (no path) |
| Inventory cites `createLogger` (`FRONTEND.md:278`) | Vite's own API, not ours | **FP** |

**FP rate:** 5 of 12 headline findings (42 %). The FPs come in fixable groups: dynamic imports,
template-prefixed URLs, tools that see only one package, third-party names. Every one is a filter, so a
tuned sweep would be far quieter. Raw tool output is not fit to route anywhere as-is.

### 4.1 Hand-built image paths (the one finding worth acting on)

`img_project_dir(img)`, `img_zero_dir(img)` and `img_filepath(img, name; version)` are
`app/src/model/image.jl:90-113`, there since `e92bdf3b4` (first commit). Hand-built instead in 23
files: 9 of 10 `editImages/*` tasks (not `register`), 6 `cleanupImages/*` tasks, `segment/{cellpose,coastal,correct,measure_labels}.jl`,
`exportImages/ome_tiff.jl`, `task/composite.jl`, `storage.jl:413`, `model/set.jl`. jscpd's top Julia
clones (`bin.jl`/`resampleZ.jl`, `cropImage.jl`/`zProject.jl`, `dtype.jl`/`flip.jl`) are this preamble.
A ratchet in `app/test/suite/ratchets.jl` banning `img._dir` outside `model/` (with a capped
baseline of today's 23 files) is the shape every other accessor rule already has.

### 4.2–4.6 Lower-value findings

- **4.2 Clones.** The rest are small and mostly parallel siblings: `ClusterHmmStatesPanel.vue` /
  `ClusterHmmTransitionsPanel.vue` (32 + 24 lines), `LightPathFigureAiryscan.vue` / `…Confocal.vue`,
  `hmm_state_cards_api.jl` / `motif_cards_api.jl` (11 blocks; not diffed), `cellpose.jl:221` /
  `coastal.jl:183`. None checked against a canonical helper.
- **4.3 Unused frontend exports.** 323 items, nearly all "exported but used in-file". Cosmetic;
  not worth a gate.
- **4.4 Uncalled routes.** 5 routes, tested but with no in-repo caller. Inference: dead or meant
  for external scripts. `/api/auth` not checked (probably middleware).
- **4.5 Stale inventory rows.** 20 identifiers; `napari_utils` is the clear one. Others not checked.
- **4.6 No tool.** Julia: no maintained dead-code or duplication tool (jscpd's Julia tokenizer
  works for clones; JET/Aqua catch type and ambiguity problems, not drift). Python import cycles
  not run (a pydeps / ruff-based pass would work).

## 5. Fragmentation map and lineage

Run 2. Not started — parked (decision 3).

## 6. Proposed recurring check

| Component | Cadence | Routing | Ratchet path |
|---|---|---|---|
| Sweeps 1–7 above, with the FP filters (dynamic imports skipped, `${…}/api` templates parsed, repo-wide knip workspace, third-party names allow-listed) | Weekly, as one step of `judge-weekly` | Counts only, in a new *Coherence counts* section of the judge record, diffed against the last pass. **No decide items** | A count that rises two passes in a row → the judge files one `ratchet` proposal through the existing review screen |
| Specific ratchets (start with §4.1) | CI, every push | Existing test suites | Capped `_BASELINE` per the existing meta-ratchet |
| LLM rotating-area judgement | — | — | Not proposed. `DRIFT_PREVENTION_ASSESSMENT.md` declined it; no revisit trigger checked as fired |

**Cost:** zero tokens, ~1 min of a weekly run. Nothing to drop if over budget.
**Maturity tiers:** counts are whole-repo and tier-blind. A ratchet proposal on an Experimental
surface can be parked at review.

## 7. Decisions for you

1. **Ratchet §4.1 (image paths through the accessors)?** Recommend yes: one capped-baseline testset,
   then convert the 23 files over time. Cheapest alternative: an `app/CLAUDE.md` line, which the
   drift assessment says loses to the cost gradient.
2. **Add the weekly counts step to `judge-weekly`?** Recommend yes, counts only, with the FP filters.
   Cheapest alternative: rerun this doc by hand each quarter.
3. **Run 2 (judgement + lineage)?** Recommend park. The 0.81 % duplication and 2 static cycles don't
   justify it. Re-open on a `DRIFT_PREVENTION_ASSESSMENT.md` trigger.
4. **Small cleanups (drop `regl-scatterplot`, delete the stale `napari_utils` inventory row, the 3
   vulture dead params)?** Recommend one tidy PR. Alternative: leave for the first weekly count.

## 8. What I could not verify

- Whether the 5 uncalled routes have external callers (lab scripts, HPC).
- `hmm_state_cards_api.jl` vs `motif_cards_api.jl`: counted as clones, not diffed.
- 18 of the 20 stale inventory identifiers.
- Whether any `DRIFT_PREVENTION_ASSESSMENT.md` revisit trigger has fired (PR-trail drift rate not
  measured).
- Python import cycles, Julia dead code (no tool run).
