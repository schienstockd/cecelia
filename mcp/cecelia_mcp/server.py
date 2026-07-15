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

# Lab-log author tag for observer-written entries. Matches the frontend's authorKind() 'claude'.
CLAUDE_AUTHOR = "Claude"

_client = CeceliaClient(base_url=os.environ.get("CECELIA_API_URL", "http://127.0.0.1:8080"))
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
    """Per-image QC flags/metrics computed after tasks run ({} if none yet)."""
    img = _client.get_image_meta(project_uid, image_uid).get("image", {})
    return img.get("qc", {}) or {}


@mcp.tool()
def get_task_log(project_uid: str, image_uid: str, fun: str) -> str:
    """Raw log text for one task function (e.g. "segment.cellpose") on one image; '' if never run."""
    r = _client.get_task_log(project_uid, image_uid, fun)
    return r.get("content", "") if r.get("exists") else ""


@mcp.tool()
def get_task_history(project_uid: str, limit: int = 100) -> list:
    """Recent task runs across all images, newest first: image, function, value name, timestamp, status."""
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


def main():
    mcp.run()  # stdio transport


if __name__ == "__main__":
    main()
