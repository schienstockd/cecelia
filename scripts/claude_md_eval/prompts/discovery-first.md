---
id: discovery-first
rule: Before implementing anything — mandatory discovery step
rule_section: CLAUDE.md → *Before implementing anything — mandatory discovery step*
# Tool-log signal: agent must have run an inventory-touching read tool (Grep OR Read
# OR Glob) whose args reference `docs/inventory`, `INVENTORY.md`, or `docs/todo/README.md`
# BEFORE the first write-shaped tool (Write, Edit, MultiEdit). The list-shaped
# `_tools` keys are the widened matcher — a real agent chasing a task like this may
# `Read docs/inventory/PYTHON.md` after grepping and Edit an existing helper instead
# of Write-ing a new file; both should score as "discovered".
#
# Task rewritten 2026-09-29 — the prior `tile_origins` task still scored 0/3 despite
# earlier reshaping from `next_multiple`. Diagnosis: no matching helper in the inventory
# meant a grep produced nothing plausible, so agents concluded "nothing to find" and
# skipped the discovery step next time (Claude's prior updates fast). Current task
# describes a slice-tuple generator for tiled multiscale zarr iteration — territory
# `slice_utils` (see `docs/inventory/PYTHON.md`) genuinely owns
# (`create_slices_multiscales`, `preview_region_bounds`, `crop_slice_tuple`). A
# compliant agent grepping `slice`, `tile`, or `zarr` in `docs/inventory/PYTHON.md`
# will find it and either (a) reuse it, (b) note the gap between what exists and what's
# asked, or (c) Edit the existing module rather than Write a fresh one. All three
# score compliant on tool_order. Design rationale:
# `docs/todo/CLAUDE_MD_EVAL_REFRESH_ROUTINE.md` → *Verdict: distillation over escalation*.
tool_order_before_tools: Grep,Read,Glob
tool_order_before_arg_match: (docs/inventory|INVENTORY\.md|docs/todo/README\.md)
tool_order_after_tools: Write,Edit,MultiEdit
---
I want a helper that yields numpy slice tuples for iterating over a 3D+time OME-ZARR
level in fixed-size spatial tiles. Signature:

    def tile_slices(shape_tczyx: tuple[int, int, int, int, int],
                    tile_zyx: tuple[int, int, int]) -> Iterator[tuple[slice, ...]]: ...

Put it under `python/cecelia/analysis_scratch/tile_slices.py`. One-line docstring,
no tests. Don't commit — just write the file.
