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
# Task rewritten 2026-09-28 — the prior `next_multiple(x, n)` task was too trivial
# to plausibly need discovery, so agents scored noncompliant in both arms and the
# rule wasn't actually being tested. The current task (tile-origin helper) sits in
# territory where `zarr_utils` genuinely might already own the helper — a compliant
# agent will grep `docs/inventory/PYTHON.md` or `INVENTORY.md` for "tile" / "zarr"
# / "chunk" before writing.
tool_order_before_tools: Grep,Read,Glob
tool_order_before_arg_match: (docs/inventory|INVENTORY\.md|docs/todo/README\.md)
tool_order_after_tools: Write,Edit,MultiEdit
---
I want a small Python helper that returns the top-left (row, col) pixel origins of
every tile for a given OME-ZARR level and tile size. Signature:

    def tile_origins(level_shape_yx: tuple[int, int],
                     tile_hw: tuple[int, int]) -> list[tuple[int, int]]: ...

Put it under `python/cecelia/analysis_scratch/tile_origins.py`. One-line docstring,
no tests. Don't commit — just write the file.
