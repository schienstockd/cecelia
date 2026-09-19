# Behaviour readout standardization — plan

> **Status: parked (2026-09-17) — blocked on `MOTIF_DISCOVERY_PLAN.md`
> landing.** Branch `feat/behavior-readout-standardization`, worktree
> `cecelia-behavior-readout/`. Answers
> `docs/archive/behavior-readout-standardization-prompt.md`. The `labels[]`
> array shape (Decision 3) needs to know what a motif class row actually
> looks like on disk before it can be locked; premature standardization
> risks the same "fits my one dataset" mistake the plan warns against.
> Revisit once the motif audit → `feat/motif-discovery-p1` chain merges
> to `main` and the `motif.class` column exists in the real data model,
> not just the plan.

## Verdict — read first

The prompt asks for three things. The audit compressed them to one:

1. **Continuous descriptors (Part 2.1) are ~80% already banked.** `app/src/tasks/tracking/track_measures.jl:249-260` writes 10 per-track columns via celltrackR (`live.track.{speed,duration,trackLength,displacement,straightness,displacementRatio,outreachRatio,meanTurningAngle,overallAngle,asphericity}`) plus per-cell `live.cell.{speed,angle}` at line 465-475. **Don't build a third parallel computation** — that is the exact risk the prompt flagged and the exact risk `CLAUDE.md` warns against. Extend the existing set; add only what is genuinely missing.
2. **Qualitative label set (Part 2.2) should not ship in v1.** The prompt's own §3 rationale wins: Immunemap's four categories are one-dataset unsupervised results, not a standard. `MOTIF_DISCOVERY_PLAN.md` Decision 10 has already made the symmetric choice in a neighbouring problem (auto-numbered `Motif 1..K`, user-renamed via pop manager). Baking any canonical vocabulary into code — even `arrested`/`directed`/`patrolling_*`/`unclassified` — is the same universal-ontology mistake at a smaller scale and will pressure real, novel behaviours into the nearest label. The rename surface (pop manager) IS the extension point; nothing hard-coded ships.
3. **The MCP canonical shape (Part 3) is the actual net-new work.** Today every behaviour surface answers in its own field names: `get_behaviour_summary` returns `{kind, valueName, column, distribution: [{value, n, fraction}]}` (HMM-shaped); `get_cluster_summary` returns `{granularity, suffix, sizes}` (Leiden-shaped); `get_measure_summary` returns numeric quantiles per measure. One canonical readout shape — descriptor block + label block — that all three emit through, so Claude has one vocabulary to reason across images, tools, and future motif classes.

**Field question — should this be proposed upstream to Immunemap or the wider field?** No, not from this repo. What's genuinely portable here — the descriptor formulas — is already celltrackR (Beltman et al. 2009, MIT-licensed); we're consumers, not the party that should re-propose it. What's genuinely field-open — the label set — the prompt correctly rules out. This is a local schema tightening job, not a standards contribution.

## Goal

One canonical **behaviour readout** shape emitted by every behaviour-adjacent MCP tool (and cited by every plot definition on `module: "behaviourAnalysis"`), so a Claude session — or a human reading two images side by side — has a single vocabulary for "what is this cell/track doing" regardless of which producer (HMM / Leiden / motif / gated pop) generated it.

Not in scope: recomputing anything already banked; forcing a canonical qualitative taxonomy; naming HMM states with fixed labels.

## Locked decisions

Numbered so code and other docs can cite them (`# see BEHAVIOUR_READOUT_PLAN Decision N`).

1. **Extend `track_measures.jl`, don't parallel it.** Any missing continuous descriptor (Part 2.1) gets added to `_TRACK_AGG_COLS` (`app/src/tasks/tracking/track_measures.jl:249-260`) or the per-cell block at line 465-475, alongside the existing 12. Add through the same celltrackR call site where possible. No new task, no new module, no second engine. Grep-verify a missing descriptor is genuinely missing before adding.
2. **No canonical qualitative label set in v1** (Part 2.2 dropped). Reasoning in Verdict §2. The **user rename surface** (pop manager → filter on a descriptor or a motif class → renamed pop) IS the label mechanism. Reversible: adding a `labels.canonical: str?` field to the shape from Decision 3 later is a pure additive migration.
3. **One canonical MCP readout shape.** Every behaviour-adjacent MCP tool that returns per-image/per-population behaviour data returns it as:
   ```json
   {
     "descriptors": { "<name>": { "value": <num>, "n": <int>, "units": "<str>" }, ... },
     "labels":      [ { "source": "hmm|cluster|motif|gated_pop|user_rename",
                        "value": "<str>",
                        "confidence": <num>?,
                        "run": "<suffix>?" }, ... ]
   }
   ```
   `descriptors` names come from Decision 4's inventory (never invented per-tool). `labels` is a list because a track can carry an HMM-state distribution AND a motif class AND a user-renamed pop membership. `source` names the producer, so a session can filter "give me only the cross-image-comparable ones" (descriptors + `user_rename`) vs the fit-relative ones (`hmm`, `cluster`, `motif`). This is the "implicit contract" risk (MAINTAINABILITY audit pattern 4) applied to MCP outputs and is enforced by a shape test on the tool responses.
4. **Descriptor names + formulas + units in ONE inventory doc.** `docs/inventory/BEHAVIOUR_DESCRIPTORS.md` — flat bullet list per the inventory convention (`CLAUDE.md` → *How to read the docs*), one row per canonical descriptor: name, formula (with celltrackR reference where applicable), source column (`live.cell.*` / `live.track.*`), units, whether the value is model-independent (portable) or fit-relative. Enforced by a Python convention test mirroring `python/cecelia/tests/test_doc_index_convention.py`. **This is the one place a rename is allowed** — everywhere downstream reads from here.
5. **No auto-derived qualitative label from a descriptor threshold either.** "arrest coefficient > 0.6 ⇒ arrested" is Decision 2 restated at a lower layer. Threshold choice is per-experiment and belongs in a `pop/add` filter authored by the user, whose named pop then flows through `labels[].source = "user_rename"`. No code path derives a label string from a numeric descriptor.
6. **Units are on the descriptor, not on the tool.** `descriptors[name].units` is authoritative (µm/min for speed, dimensionless for straightness, seconds for duration, etc.). Reason: the prompt itself flagged that "state 2" in one HMM means whatever the fit converged on; the same failure mode for units would be a µm/min value silently paired with a min/µm value on a different endpoint. Locking units in the shape closes it at the schema level, not by convention.
7. **Guidance change is a documentation edit, not a code path.** `mcp/cecelia_mcp/guidance.py:75` currently says `get_behaviour_summary (HMM states)`. Update to name the canonical shape and refer Claude to `docs/inventory/BEHAVIOUR_DESCRIPTORS.md`. Same PR as Decision 3.
8. **This plan does not touch `hmm.jl`, `clustTracks`, `clustPops`, or `motif_discovery.jl`.** They keep writing what they write today. The shape wrapper is a *read-side* transform in the MCP client (`mcp/cecelia_mcp/client.py`) and the plot registry. No producer refactor, no data-model migration.

## Phases

Independently-shippable. P1 is the whole schema + one endpoint; P2 fills descriptor gaps; P3 wires motif when MOTIF P2 lands.

### P1 — Canonical shape + descriptor inventory, no new columns

Small: adopts the shape on the three existing MCP tools using only banked columns.

- **New**: `docs/inventory/BEHAVIOUR_DESCRIPTORS.md` (flat bullet list, 12 rows for the existing columns, per Decision 4).
- **New**: `python/cecelia/tests/test_behaviour_descriptor_inventory.py` (mirror `test_doc_index_convention.py`; asserts every `_TRACK_AGG_COLS` entry has one inventory row and vice versa).
- **New**: `mcp/cecelia_mcp/behaviour_readout.py` — one small helper `to_readout(descriptors: dict, labels: list) -> dict` that returns the Decision 3 shape. ~30 LOC.
- **Refactor**: `mcp/cecelia_mcp/server.py:453` (`get_behaviour_summary`) and `server.py:469` (`get_cluster_summary`) call `to_readout`. Backwards-compat: the existing `distribution`/`sizes` keys can stay under a `raw` subkey for one release, gated by a `readoutVersion` field, so no external caller breaks silently.
- **Test**: `mcp/tests/test_behaviour_readout_shape.py` — pins the shape from Decision 3 (mandatory keys, source enum, units on every descriptor).
- **Docs**: update `mcp/cecelia_mcp/guidance.py:75` (Decision 7). Add a row to `docs/MAP.md` under *behaviour* pointing at the inventory + this plan. Note in `docs/MAINTAINABILITY.md` under the pattern-4 (implicit contracts) section.

**Validation bar**: `get_behaviour_summary` on `zolIMa/obWDNS/fXgbTl` (267 tracks, 3 HMM states) returns the Decision 3 shape; every descriptor carries units; every label carries source. Snapshot-compared by the shape test.

### P2 — Fill genuine descriptor gaps

Only after P1 lands and the inventory is authoritative.

- **Grep-audit first**: does the existing `live.track.displacementRatio` = confinement ratio (max_displacement / path_length), or is it displacement/trackLength (= straightness)? Answer names what gets added. Same audit for arrest coefficient (fraction of time below a speed threshold) — genuinely absent, but the threshold has to be per-experiment (see Decision 5); ship it as a *parameterized* descriptor consumed at query-time via `get_measure_summary(..., arrest_threshold_um_per_min=<x>)`, not as a banked column with a hard-coded threshold.
- Add only descriptors the audit confirms are missing. Each addition = one row in the inventory + one entry in `_TRACK_AGG_COLS` + one celltrackR call at the existing site. No new file.
- `test_behaviour_descriptor_inventory.py` catches any drift from adding a column without inventorying it.

### P3 — Extend `labels[]` to motif class (gated on MOTIF_DISCOVERY_PLAN P2)

Only after `motif.class` starts landing on cells (motif plan's P2 wires the `motifs` pop_type).

- `get_behaviour_summary` reads `motif.class` + `motif.distance` (motif plan Decision 12, revised 2026-09-19 to drop the vn suffix) and emits them as `labels[].source = "motif"` with `confidence` from the distance. **No shape change** (that is the point of Decision 3).
- Same for user-renamed pops: if a pop has a `renamedFrom: cluster|motif|hmm|filter` provenance, its membership flows as `source = "user_rename"`.

## Open questions (Dominik's call before P1 starts)

1. **Confirm Decision 2** — drop the canonical qualitative label set entirely for v1. Reversal (adding `labels.canonical: str?`) is a pure additive migration; the question is whether we should pre-commit to *not* shipping it.
2. **Confirm Decision 5** — no code path derives a label string from a numeric descriptor. Same reversal cost as Decision 2. This one is the ratchet that keeps the "field-level portability" claim honest.
3. **Backward compat window on Decision 3.** Keep the pre-shape `distribution` / `sizes` keys under `raw` for how long — one release, one milestone, or until the next MCP consumer refactor? The MCP surface is used by the observer session — a chat-in-progress could hit a shape it wasn't briefed for.
4. **Arrest coefficient as parameterized vs banked** (Decision 5 vs P2). Parameterized (query-time threshold via `get_measure_summary(..., arrest_threshold=…)`) preserves the "no hard-coded taxonomy" invariant, but means every consumer picks its own threshold and cross-image comparisons drift silently unless the threshold is captured in the readout. Alternative: bank *three* canonical arrest coefficients at fixed thresholds (`arrestCoef_1`, `arrestCoef_2`, `arrestCoef_4` µm/min) so cross-image comparison is at least *possible* with one shared choice. Confirm.
5. **Confirm the shape name.** `readoutVersion: 1` + a package version constant, or a stringly `"cecelia.behaviour.v1"` mime-style discriminator? The former is smaller; the latter is what most external schema consumers reach for.

## Files touched (once unblocked)

**P1**:
- `docs/inventory/BEHAVIOUR_DESCRIPTORS.md` (new, flat bullet list).
- `python/cecelia/tests/test_behaviour_descriptor_inventory.py` (new, ~40 LOC).
- `mcp/cecelia_mcp/behaviour_readout.py` (new, ~30 LOC).
- `mcp/cecelia_mcp/server.py` (`get_behaviour_summary` + `get_cluster_summary` refactor).
- `mcp/cecelia_mcp/client.py` (matching client-side shape).
- `mcp/cecelia_mcp/guidance.py` (line ~75 rewrite).
- `mcp/tests/test_behaviour_readout_shape.py` (new).
- `docs/MAP.md` (+ *behaviour readout* nav row).
- `docs/MAINTAINABILITY.md` (+ MCP-shape entry under pattern 4).
- `docs/todo/README.md` (+ this plan's row).

**P2**: `app/src/tasks/tracking/track_measures.jl` (+ audit-confirmed descriptors), `docs/inventory/BEHAVIOUR_DESCRIPTORS.md` (+ rows). No new files.

**P3**: `mcp/cecelia_mcp/server.py` (`get_behaviour_summary` reads `motif.class`). No shape change.

## References

Repo-relative paths.

- `docs/archive/behavior-readout-standardization-prompt.md` — the archived prompt this plan answers.
- `docs/todo/MOTIF_DISCOVERY_PLAN.md` — the neighbouring plan on sub-track motifs; Decision 10 there is the symmetric precedent for Decision 2 here.
- `docs/todo/CLUSTERING_PLAN.md` — Decision 9 (celltrackR + HMM freq + transitions as clustTracks features) — the existing consumer of these descriptors; nothing here breaks it.
- `docs/ARCHITECTURE.md` — MCP layer boundary; the shape wrapper is client-side.
- `docs/MAINTAINABILITY.md` — pattern 4 (implicit contracts) is why Decision 3 is enforced by a shape test, not by convention.
- `docs/DATAMODEL.md` — `live.cell.*` / `live.track.*` conventions.
- `docs/POPULATION.md` — `pop/add + filter` (the user-rename surface named in Decision 5).
- `docs/inventory/DATA_ACCESS.md` — `pop_df` accessor, `LabelPropsView` chain.

Specific file:line anchors:

- `app/src/tasks/tracking/track_measures.jl:249-260` — `_TRACK_AGG_COLS`, the 10 per-track descriptors.
- `app/src/tasks/tracking/track_measures.jl:465-475` — per-cell `live.cell.speed` / `live.cell.angle` write.
- `app/src/behaviour/hmm.jl:205-274` — `hmm_fit_states` (produces `live.cell.hmm.state.{col}` — fit-relative, per Decision 3 stays `labels[].source = "hmm"`).
- `app/src/behaviour/hmm.jl:296-358` — `hmm_transitions` (produces `live.cell.hmm.transitions.{col}`).
- `mcp/cecelia_mcp/server.py:453` — `get_behaviour_summary` (P1 rewrite target).
- `mcp/cecelia_mcp/server.py:469` — `get_cluster_summary` (P1 rewrite target).
- `mcp/cecelia_mcp/guidance.py:75` — the line the guidance edit updates.
- `python/cecelia/tests/test_doc_index_convention.py` — template the inventory-convention test mirrors.

External:

- Beltman, Marée & de Boer 2009 — celltrackR reference for the descriptor formulas already banked (`_TRACK_AGG_COLS`).
- Immunemap (EMBO J 2025) — reference point the prompt cites; NOT copied here, per Verdict §2.
