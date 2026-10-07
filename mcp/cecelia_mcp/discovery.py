"""Task discovery — what each step is for, and the switch that hides it (docs/todo/TASK_DISCOVERY_PLAN.md).

Two things live here, both shared by every MCP server in this package:

- `discovery_enabled()` — THE reader of `CECELIA_MCP_DISCOVERY` (`on`, the default, or `off`). The
  guide-run harness sets it to `off` for the arm that runs without the discovery information
  (`guide-run --discovery off`, plan Decision 9). Every surface the toggle governs asks this one
  function — the spec fields and `get_task_catalogue` here, the recommender's `evidence=metadata`
  and the `seg.*` findings elsewhere — so the arms can never disagree about which one they are in.
- `task_catalogue()` — every visible task's `purpose` / `useWhen` / `notWhen`, grouped by pipeline
  stage, built from the task definitions route (the task JSON is the one source, Decision 1).
"""
from __future__ import annotations

import os

DISCOVERY_ENV = "CECELIA_MCP_DISCOVERY"

# The spec fields the toggle governs. `get_module_params` carries them when discovery is on and
# strips them when it is off.
DISCOVERY_FIELDS = ("purpose", "useWhen", "notWhen")


def discovery_enabled() -> bool:
    """True unless `CECELIA_MCP_DISCOVERY=off`. Unset or `on` is the default arm.

    Any other value raises: a typo in an experiment arm must fail loudly at server start, not run
    silently as the wrong arm and get pooled with it."""
    v = os.environ.get(DISCOVERY_ENV, "").strip().lower()
    if v in ("", "on"):
        return True
    if v == "off":
        return False
    raise ValueError(f"{DISCOVERY_ENV}={v!r}: expected 'on' or 'off'")


# Task category (the part of a fun_name before the dot) → pipeline stage, in pipeline order. The
# stage names extend the run-log lineage's (`_stage_of`, app/src/ai/lineage.jl) to every category
# that ships a task. The order is TASK_DISCOVERY_PLAN Decision 7's (behaviour before cluster: Cluster
# tracks reads HMM states). A category missing here — a plugin's — still appears, as its own stage
# after these, so a new module can never drop out of the catalogue. Pinned by test_discovery.py:
# every built-in category has a stage, and each agrees with the Julia twin's.
_STAGES = (
    ("import",    ("importImages",)),
    ("cleanup",   ("cleanupImages",)),
    ("edit",      ("editImages",)),
    ("train",     ("opticalFlow",)),
    ("segment",   ("segment",)),
    ("track",     ("tracking",)),
    ("behaviour", ("behaviour",)),
    ("cluster",   ("clustPops", "clustTracks")),
    ("spatial",   ("spatialAnalysis", "clustRegions")),
    ("export",    ("exportImages",)),
)
_STAGE_OF = {cat: stage for stage, cats in _STAGES for cat in cats}
STAGE_ORDER = tuple(stage for stage, _ in _STAGES)


def stage_of(category: str) -> str:
    return _STAGE_OF.get(category, category)


def discovery_lines(lines) -> list[str]:
    """A spec's `useWhen` / `notWhen` as plain text. A line may be `{text, check}` — `check` names an
    advisory GUI check over the selected images (frontend/src/utils/taskDiscovery.ts) and means nothing
    to a reader without that selection, so only the text leaves the MCP."""
    return [l.get("text", "") if isinstance(l, dict) else l for l in (lines or [])]


def discovery_fields(spec: dict) -> dict:
    """The spec's discovery fields present and non-empty, lines as plain text."""
    out = {}
    for k in DISCOVERY_FIELDS:
        v = spec.get(k)
        if v:
            out[k] = v if k == "purpose" else discovery_lines(v)
    return out


def task_catalogue(definitions: dict, stage: str = "") -> dict:
    """`{stages: [{stage, tasks: [{fun_name, label, purpose, useWhen, notWhen}]}]}` in pipeline order.

    `definitions` is the `/api/tasks/definitions` payload (`{category: [spec, …]}`). Hidden tasks are
    left out — they are not on any module page. `stage` narrows to one stage; an unknown one raises
    `ValueError` naming the known ones."""
    grouped: dict[str, list] = {}
    for category, specs in (definitions or {}).items():
        for spec in specs or []:
            if not isinstance(spec, dict) or spec.get("hidden") or not spec.get("fun_name"):
                continue
            grouped.setdefault(stage_of(category), []).append({
                "fun_name": spec["fun_name"],
                "label": spec.get("label", ""),
                "purpose": spec.get("purpose", ""),
                "useWhen": discovery_lines(spec.get("useWhen")),
                "notWhen": discovery_lines(spec.get("notWhen")),
            })
    order = [s for s in STAGE_ORDER if s in grouped] + sorted(s for s in grouped if s not in STAGE_ORDER)
    if stage:
        if stage not in grouped:
            raise ValueError(f"no stage {stage!r}; known: {', '.join(order)}")
        order = [stage]
    return {"stages": [{"stage": s, "tasks": sorted(grouped[s], key=lambda t: t["fun_name"])}
                       for s in order]}
