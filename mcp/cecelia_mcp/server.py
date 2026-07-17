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
    """Project summary: name, kind, image count, its sets, and a per-status breakdown of images."""
    data = _client.list_images(project_uid)
    statuses: dict[str, int] = {}
    for img in data.get("images", []):
        s = img.get("status", "?")
        statuses[s] = statuses.get(s, 0) + 1
    return {
        "projectUid": project_uid,
        "name": data.get("name"),
        "kind": data.get("kind"),
        "imageCount": data.get("count"),
        "sets": data.get("sets"),
        "statusBreakdown": statuses,
    }


@mcp.tool()
def list_images(project_uid: str) -> list:
    """Every image in the project: uid, name, processing status, and which set it belongs to."""
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
def get_cohort_qc(project_uid: str, set_uid: str, fun_name: str, value_name: str = "default") -> dict:
    """Cohort QC for one task across a set's images — the way to spot an outlier run ("image 7 has 8×
    fewer cells than the cohort"). Aggregates the objective metric each task banks, over the set's
    INCLUDED images, into mean/SD + z-scored outliers.

    `set_uid` comes from get_project_info's `sets` / list_images' per-image set. `fun_name` must be a
    metric producer (else the call errors):
      - "segment.cellpose"           → nCells
      - "segment.measureLabels"      → nCells
      - "tracking.bayesian_tracking" → nTracks, meanTrackLength, nTrackedCells
    `value_name` is the output variant (default "default").

    Returns {funName, valueName, nIncluded, metrics: {<key>: {n, mean, sd, sdThreshold,
    outliers: {imageUid: {value, z}}}}}. An `outliers` map with entries is the flag worth a note
    (name the image, its value, and the cohort mean — numbers in the detail). `n` < 3 ⇒ too few
    images to judge (never call an outlier then). Advisory only; reads current data (nothing cached)."""
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
