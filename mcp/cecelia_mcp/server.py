"""Cecelia MCP observer server (Phase 1 — read-only).

Exposes the running Cecelia project to Claude over stdio: project state, images, task logs, QC, and
the lab log — plus a single append-only write to the lab log. No other mutation is possible; the
enforced allow-list lives in ``cecelia_mcp.client.ALLOWED_ROUTES``.

Run:   pixi run mcp          (or:  PYTHONPATH=mcp python -m cecelia_mcp.server)
Talks to the Julia API at $CECELIA_API_URL (default http://127.0.0.1:8080), so `pixi run dev` must
be running. See mcp/README.md for wiring this into Claude Code.

This is Phase 1 of the arc in docs/ai-assist/OBSERVER.md. Phase 2 (write tools: submit_task,
adjust_params, acknowledge_flag) is deliberately NOT wired here.
"""
from __future__ import annotations

import os

from mcp.server.fastmcp import FastMCP

from cecelia_mcp.client import CeceliaClient
from cecelia_mcp.monitor import SessionMonitor
from cecelia_mcp.wsclient import api_url_to_ws, start_listener

# Lab-log author tag for observer-written entries. Matches the frontend's authorKind() 'claude'.
CLAUDE_AUTHOR = "Claude"

_API_URL = os.environ.get("CECELIA_API_URL", "http://127.0.0.1:8080")
_client = CeceliaClient(base_url=_API_URL)
_monitor = SessionMonitor()
mcp = FastMCP("cecelia-observer")


@mcp.tool()
def get_project_info(project_uid: str) -> dict:
    """Project summary: name, kind, image count, its sets, a per-status breakdown, and `excludedCount`
    — how many images are EXCLUDED (included:false). An excluded image is a silent member: it still
    sits in the set as "done", so anything counting "images in the set" (cohort denominators, figures)
    is over by that many. If excludedCount > 0, check list_images/get_image_notes for which and why."""
    data = _client.list_images(project_uid)
    statuses: dict[str, int] = {}
    excluded = 0
    for img in data.get("images", []):
        s = img.get("status", "?")
        statuses[s] = statuses.get(s, 0) + 1
        if img.get("included") is False:
            excluded += 1
    return {
        "projectUid": project_uid,
        "name": data.get("name"),
        "kind": data.get("kind"),
        "imageCount": data.get("count"),
        "sets": data.get("sets"),
        "statusBreakdown": statuses,
        "excludedCount": excluded,
    }


@mcp.tool()
def list_images(project_uid: str) -> list:
    """Every image in the project: uid, name, processing status, which set it belongs to, and
    `included` — false means EXCLUDED from analysis (a silent member; downstream/cohort counts should
    drop it). An excluded image that is still "done" is intentional but easy to miss — see its note."""
    return _client.list_images(project_uid).get("images", [])


@mcp.tool()
def get_image_info(project_uid: str, image_uid: str) -> dict:
    """One image's full metadata: channels, dimensions, physical sizes, label props, QC, run log, note."""
    return _client.get_image_meta(project_uid, image_uid).get("image", {})


@mcp.tool()
def get_image_notes(project_uid: str, image_uid: str) -> str:
    """The user-written note for an image ('' if none) — the user's own words, first-class context."""
    img = _client.get_image_meta(project_uid, image_uid).get("image", {})
    return img.get("note", "") or ""


@mcp.tool()
def get_qc_metrics(project_uid: str, image_uid: str) -> dict:
    """Per-image QC flags/metrics computed after tasks run ({} if none yet). For "is THIS image an
    outlier vs the rest of the set?", use get_cohort_qc instead — a single image's number means little
    without the cohort."""
    img = _client.get_image_meta(project_uid, image_uid).get("image", {})
    return img.get("qc", {}) or {}


@mcp.tool()
def get_cohort_qc(project_uid: str, set_uid: str, fun_name: str, value_name: str | None = None) -> dict:
    """Cohort QC for one task across a set's images — the way to spot an outlier run ("image 7 has 8×
    fewer cells than the cohort"). Aggregates the objective metric each task banks, over the set's
    INCLUDED images, into mean/SD + z-scored outliers.

    `set_uid` comes from get_project_info's `sets` / list_images' per-image set. `fun_name` must be a
    metric producer (else the call errors AND lists the current valid funs). Check the fun of WHATEVER
    task actually ran (from get_task_history) — e.g. if you just clustered, check clustPops/clustTracks,
    not segmentation. The metric producers:
      - "segment.cellpose"           → nCells
      - "segment.measureLabels"      → nCells
      - "tracking.bayesian_tracking" → nTracks, meanTrackLength, nTrackedCells
      - "tracking.track_measures"    → nTracks, meanSpeed, meanDisplacement
      - "behaviour.hmm_states"       → nDecoded, nStates, dominantStateFrac
      - "behaviour.hmm_transitions"  → nTransitions, nDistinctTransitions
      - "clustPops.cluster"          → nCells, nClusters, largestClusterFrac
      - "clustTracks.cluster"        → nTracks, nClusters, largestClusterFrac

    **LEAVE value_name UNSET** unless you have a specific one. A task banks its QC under a value_name,
    and different tasks use different ones: segment/tracking bank under "default", but CLUSTERING banks
    PER LABEL SET (e.g. "T" and "B" — T-cells and B-cells). With no value_name, this returns every one
    the fun actually banked, so you don't have to know the suffix:
       {funName, valueNames: [...], byValueName: {"T": <doc>, "B": <doc>}}
    (that is why a bare clustering query used to come back empty — it defaulted to "default", where
    clustering banks nothing). Pass an explicit value_name only to get that single label set's <doc>.

    Each <doc> is {funName, valueName, nIncluded, metrics: {<key>: {n, median, mad, mean, sd, threshold,
    outliers: {imageUid: {value, z|relDev}}}}}. Outliers use a robust modified z-score (median/MAD) —
    the entry carries `z` (that score); when the cohort has no spread (MAD 0, ≥half identical) it
    carries `relDev` (relative departure) instead. Either way a clear outlier flags even at n=3. An
    `outliers` map with entries is the flag worth a note — name the image, the LABEL SET, its value, and
    the cohort median (numbers in the detail). `n` < 3 ⇒ too few images to judge. Advisory; reads current data."""
    return _client.get_cohort_qc(project_uid, set_uid, fun_name, value_name)


@mcp.tool()
def get_task_log(project_uid: str, image_uid: str, fun: str) -> str:
    """Raw log text for one task function (e.g. "segment.cellpose") on one image; '' if never run."""
    r = _client.get_task_log(project_uid, image_uid, fun)
    return r.get("content", "") if r.get("exists") else ""


@mcp.tool()
def get_task_history(project_uid: str, limit: int = 100) -> list:
    """Recent task runs across all images, newest first. Each row: `imageUid`, `imageName`, `fun`,
    `valueName`, `at` (timestamp), `status` (the image's current status), and **`runStatus`** — that
    run's outcome, `"done"` or `"failed"`.

    Watch `runStatus`: the same `fun` showing `"failed"` repeatedly on one image is a stuck point worth
    flagging (e.g. "hmm failed 5x on image KDIeEm — want to look at the params?"). **This is the place
    to catch repeated failures** — a failed task leaves little other trace, and the live-pattern
    detector (`poll_observations`) starts empty each run, so it won't have older failures. Cross-check
    `get_task_log` / `get_recent_logs` for the actual error before surfacing."""
    return _client.get_task_history(project_uid, limit).get("history", [])


@mcp.tool()
def get_analysis_lineage(project_uid: str, image_uid: str = "", set_uid: str = "") -> dict:
    """The synthesized ANALYSIS LINEAGE — how each image's data was produced, so you don't have to ask
    the user to re-explain the workflow. Scope with `image_uid` (one image) or `set_uid` (one set);
    omit both for the whole project.

    Returns:
      - `images`: per image `{uid, name, included, steps, segmentations, tracked, clusterRuns, gatedPops}`.
        - `steps`: the ordered pipeline from the run log — each `{stage, fun, valueName, status, at}`.
          `stage` ∈ import/cleanup/edit/segment/track/behaviour/cluster/other; `status` "done"/"failed".
          This IS the "denoised → segmented → tracked → clustered" story, in order, with what each wrote.
        - `segmentations`: the label-set value_names; `tracked`: those with a per-track table.
        - `clusterRuns`: `[{suffix, valueNames}]` — each clustering run and the label sets it clustered.
        - `gatedPops`: `[{valueName, popType, n, pops}]` — gate-defined populations (names/counts only).
      - `chains`: wired whiteboard templates `[{name, tasks}]` — which steps were pipelined vs ad-hoc.
      - `boards`: analysis-board tab names (best-effort; the board's plot detail is not exposed here).
      - `rollup`: `{pipeline, divergences}` — the common stage sequence across images, and which images
        diverge (missing a stage the others ran, or excluded). Start here to spot the odd image out.

    Summary-level only (names/counts/order — no raw cell/track rows). Reads current on-disk state."""
    return _client.get_analysis_lineage(project_uid, image_uid or None, set_uid or None)


@mcp.tool()
def get_populations(project_uid: str, image_uid: str = "", set_uid: str = "") -> dict:
    """Population DEFINITIONS per image — the detail behind lineage's `gatedPops`. Use this to know what
    a population actually MEANS: its gate geometry or filter rule, and where it sits in the tree. Scope
    with `image_uid` / `set_uid`, or omit both for the whole project.

    Per image `populations` is a flat list; each: `{path, name, parent, popType, valueName, colour,
    isTrack, gate, filter}`.
      - `path`/`parent` give the tree (a pop's cells are its gate/filter ∩ its parent).
      - `popType`: flow/track = gate-drawn; clust/trackclust = cluster pops.
      - `gate` (flow/track): `{kind: rectangle|polygon, x_channel, y_channel, x_transform, y_transform,
        …geometry}` — the drawn gate on two channels. null for filter pops.
      - `filter` (clust/live): `{measure, fun, values}` — e.g. a cluster pop is
        `{measure: "clusters.movement", fun: "in", values: [3]}`, which also ties it to that clustering run.
    `truncated: true` means the list was capped (many pops); the definitions are cheap sidecar reads.

    Definitions only — membership COUNTS (n cells/tracks per pop) are not here (they need computing gates
    over the full table); that's the measure summary. Reads current on-disk state."""
    return _client.get_populations(project_uid, image_uid or None, set_uid or None)


@mcp.tool()
def get_measure_summary(project_uid: str, image_uid: str = "", set_uid: str = "") -> dict:
    """Phenotype + motility SUMMARIES per population — what the cells/tracks actually look like. Use this
    for "how bright is CD8 in the T/_qc cells", "how fast do the tracked B cells move", cross-image
    comparisons of a measure. Scope with `image_uid` / `set_uid` (prefer one — this touches cell data,
    so it's heavier than lineage/populations); omit both for the whole project.

    Summarised over the MEANINGFUL populations, not the raw segmentation (most labels are usually gated
    out): the user's gated pops when present (e.g. `T/_qc`), else the base `_tracked` population (all
    tracked cells), else all cells. Per image, `summaries` is a list; each:
      `{population, valueName, kind: phenotype|motility, n, measures: [{name, n, median, q25, q75, mean}]}`.
      - `kind` "phenotype" = per-cell channel intensities (named by channel) + morphology (area, …);
        "motility" = per-track `live.track.*` (speed, displacement, trackLength, straightness, …).
      - A gated cell pop yields BOTH a phenotype row (its cells) and, when tracked, a motility row (its
        tracks). `n` is the cell/track count the stats are over.
    `truncated: true` means the population×measure list was capped. Summary-level only — medians and
    quantiles, never raw cell/track rows. Reads current on-disk state."""
    return _client.get_measure_summary(project_uid, image_uid or None, set_uid or None)


@mcp.tool()
def get_behaviour_summary(project_uid: str, image_uid: str = "", set_uid: str = "") -> dict:
    """HMM BEHAVIOUR distribution per image — how the tracked cells split across behaviour states, and
    their transitions. Scope with `image_uid` / `set_uid`; omit both for the whole project.

    Per image, `behaviour` is a list; each entry is one HMM column of one segmentation:
      - `kind` "state": `{valueName, column, n, nStates, distribution: [{value, n, fraction}]}` — the
        fraction of cells in each state (e.g. 0.42 Directed / 0.35 Scanning / 0.23 Meandering). `n` is
        the number of DECODED cells (untracked cells have no state and are excluded).
      - `kind` "transitions": `{valueName, column, n, nDistinct, distribution: [top transitions]}` —
        e.g. "1_2" is a 1→2 transition; distribution is the top-N by frequency, `nDistinct` the total.
    An image collapsed into one state, or a very different dominant-state fraction from its peers, is
    worth flagging. Summary-level (distributions, not raw rows). Reads current on-disk state."""
    return _client.get_behaviour_summary(project_uid, image_uid or None, set_uid or None)


@mcp.tool()
def get_cluster_summary(project_uid: str, image_uid: str = "", set_uid: str = "") -> dict:
    """CLUSTERING summary per image — for each clustering run, how the cells/tracks landed. Scope with
    `image_uid` / `set_uid`; omit both for the whole project.

    Per image, `clusters` is a list, one entry per (segmentation × run):
      `{valueName, suffix, granularity: cell|track, nClusters, n, largestFrac, sizes: [{value, n,
      fraction}], features}`.
      - `suffix` is the run id (e.g. "movement"/"test"); `granularity` "cell" = clustPops, "track" =
        clustTracks. `features` is the measure list the run clustered on.
      - `largestFrac` near 1.0 (one cluster swallowing most points) or a very low `nClusters` vs peers
        means a near-uninformative / collapsed clustering for that image — worth flagging.
    Summary-level (sizes, not raw cluster assignments). Reads current on-disk state."""
    return _client.get_cluster_summary(project_uid, image_uid or None, set_uid or None)


@mcp.tool()
def read_lab_log(project_uid: str) -> str:
    """The full lab-log markdown for the project — the accumulated cross-session memory."""
    return _client.read_lab_log(project_uid).get("content", "")


@mcp.tool()
def append_lab_log(project_uid: str, lines: list[str]) -> dict:
    """Append a dated [Claude] entry to the lab log. Append-only — never edits existing content.

    `lines` is one or more markdown lines. This is the ONLY write the observer can make.
    """
    return _client.append_lab_log(project_uid, CLAUDE_AUTHOR, lines)


@mcp.tool()
def get_recent_logs(level: str = "", limit: int = 100) -> list:
    """Recent lines from the backend's own console — server `@info`/`@warn`/`@error`, newest last.

    This is where a **Julia-side task crash lands** (e.g. a task that dies before its Python
    subprocess starts) — it does NOT appear in `get_task_log`, which only captures the Python
    process's stdout. When `poll_observations` shows a `repeat_attempts` / a task keeps failing but
    the task log looks empty, call this to find the actual error.

    `level` optionally filters to one of "info" / "warn" / "error" (default: all). `limit` caps how
    many of the most-recent lines are returned. It's a process-wide ring buffer (~500 lines, not
    persisted, not per-project), so it's for *live/recent* diagnosis, not historical forensics.
    """
    logs = _client.get_recent_logs().get("logs", [])
    if level:
        logs = [l for l in logs if str(l.get("level", "")).lower() == level.lower()]
    return logs[-limit:] if limit and limit > 0 else logs


@mcp.tool()
def poll_observations(project_uid: str) -> dict:
    """Drain the observer's pending observations since the last poll — the "sit next to me" signal.

    Call this periodically while watching a project. Returns `{observations, stats}`:

    `observations` is a list (often empty — most of the time nothing is worth surfacing) of:
    - `repeat_attempts`: the same function has run >3 times on one image this session
      (`imageUid`, `fn`, `attempts`, `completed`/`failed` tallies, `lastOutcome`). This is the core
      signal — surface it: "you've run cellpose on this image N times; want to talk through the goal?"
    - `image_note_added`: the user added a note to an image (`imageUid`, `note`) — ask *why* if the
      decision looks unusual; the answer belongs in the lab log.
    - `lab_log_entry_added`: a user (non-[Claude]) lab-log entry appeared (`summary`).

    `stats` reports the session throttle/cost state (`surfacedCount`, `surfaceCap`, `throttled`,
    `estimatedTokens`, `enabled`). Once `surfaceCap` observations have been surfaced, the observer
    goes quiet: `observations` stays empty and further patterns are appended to the lab log silently
    (so nothing is lost) — see `stats.throttled`. When `enabled` is false (see `set_observer_active`)
    `observations` is always empty.

    Empty `observations` ⇒ stay silent.
    """
    observations = _monitor.poll(project_uid)
    # Throttle-suppressed observations are flushed to the lab log silently, so a busy session still
    # records its patterns without spending chat tokens narrating them (OBSERVER.md §6).
    suppressed = _monitor.drain_for_log()
    if suppressed:
        _flush_to_lab_log(project_uid, suppressed)
    return {"observations": observations, "stats": _monitor.stats()}


@mcp.tool()
def set_observer_active(active: bool) -> dict:
    """Turn the live observer on or off (the off switch, per OBSERVER.md §6).

    When off, `poll_observations` surfaces nothing — but attempt counting keeps running in the
    background, so turning it back on resumes with full history. Use this if the observer becomes
    noisy or the user wants to work undisturbed. Returns the current session stats.
    """
    _monitor.set_enabled(active)
    return _monitor.stats()


@mcp.tool()
def get_observer_stats() -> dict:
    """The observer's running per-session state without draining anything: whether it's `enabled`,
    how many observations were `surfacedCount` (vs the `surfaceCap`), whether it's `throttled`, and a
    rough `estimatedTokens` cost. The token figure is an ESTIMATE (surfaced x ~2.5k) — the server
    can't see Claude's real usage — meant as a running gauge, not a bill."""
    return _monitor.stats()


def _flush_to_lab_log(project_uid: str, suppressed: list) -> None:
    """Append throttle-suppressed observations to the lab log as one compact [Claude] block. Best-
    effort — never let a lab-log write failure break a poll."""
    lines = ["_(observer throttled — logged silently, not surfaced)_"]
    for obs in suppressed:
        if obs.get("type") == "repeat_attempts":
            lines.append(f"- repeat: `{obs.get('fn')}` on image {obs.get('imageUid')} "
                         f"x{obs.get('attempts')} ({obs.get('completed')} ok / {obs.get('failed')} failed)")
        elif obs.get("type") == "image_note_added":
            lines.append(f"- note on image {obs.get('imageUid')}: {obs.get('note')}")
        elif obs.get("type") == "lab_log_entry_added":
            lines.append(f"- user log entry: {obs.get('summary')}")
    try:
        _client.append_lab_log(project_uid, CLAUDE_AUTHOR, lines)
    except Exception:  # noqa: BLE001 — best-effort; a poll must not fail on a lab-log write error
        pass


def main():
    # Best-effort: subscribe to the API's WS event stream so the monitor can detect patterns. If the
    # backend isn't up yet the listener reconnects on its own; the read tools work regardless.
    start_listener(_monitor, api_url_to_ws(_API_URL))
    mcp.run()  # stdio transport


if __name__ == "__main__":
    main()
