"""cecelia-autonomous: the MCP server that STARTS work beside the read-only observer — start work, wait
for it, draw gates. Three callers: the agent-eval runner (`scripts/agent_eval/run_app.py`) starts it with
``CECELIA_MCP_PROJECT`` set to a disposable project; the chat launcher (`chat.py`, ``CECELIA_MCP_NO_GUI=1``)
for a user with no GUI at all; and the Cecelia plugin (``CECELIA_MCP_GUI=1``, `gui_mode.py`) in a GUI
user's own session, where it offers only the run and wait tools, each call approved by the user.
The guard and its allow-list live in ``cecelia_mcp.autonomous``.

Run:   CECELIA_MCP_PROJECT=<uid> [CECELIA_MCP_PREFIX=agent] PYTHONPATH=mcp:python python -m cecelia_mcp.autonomous_server
"""
from __future__ import annotations

import functools

from mcp.server.mcpserver.exceptions import ToolError

from cecelia_mcp.autonomous import (AutonomousClient, LockViolation, allowed_projects, locked_project,
                                    required_prefix)
from cecelia_mcp.caller import CallerMCPServer
from cecelia_mcp.issue_report import failure_text, note_failed_runs
from cecelia_mcp.gating_views import register_gating_tools
from cecelia_mcp.figure_tools import register_figure_tools
from cecelia_mcp.gui_mode import gui_mode_enabled
from cecelia_mcp.no_gui import no_gui_enabled
from cecelia_mcp.tool_kinds import annotations_for

INSTRUCTIONS = (
    "Runs analysis in ONE Cecelia project (the locked one) without a human at the keyboard. Use it "
    "beside cecelia-observer: the observer READS (lineage, measures, task logs, behaviour) and "
    "DESIGNS (create_chain); this server STARTS work (run_task, run_chain), WAITS for it "
    "(wait_for_tasks, wait_for_chain) and GATES (gate_histogram → add_gate dry_run + gate_examples → "
    "add_gate → gate_cells_view; add_boolean_pop for 'X but not Y' / 'X or Y'; add_filter_pop for rules on measures; add_cluster_pops to group a run's clusters, set_cluster_pops to regroup yours). "
    "Run pipelines as chains (create_chain → run_chain) so the Chains page holds what "
    "ran; run_task is for a single task outside a chain. Everything you do is recorded."
)
# No-GUI mode (no_gui.py): the user is at the keyboard and there is no GUI, so this server is also
# how data gets in. Only then are the three creation tools registered at all.
NO_GUI_INSTRUCTIONS = (
    " There is NO GUI in this session: the user works only through you. New data comes in with "
    "create_project → create_set → register_images (absolute file paths), then run_task "
    "`importImages.omezarr` over the new image uids to convert them — nothing is analysable before "
    "that import is done. Projects you create join this session's lock."
)
# GUI mode (gui_mode.py): the user's own session, the app open, each call approved at the keyboard.
GUI_INSTRUCTIONS = (
    "Starts work in the project the user has OPEN in Cecelia, with them approving each call: run_task / "
    "run_chain what you designed together (the observer's create_chain), then wait_for_tasks / "
    "wait_for_chain. The run shows in their Task Manager, where they can cancel it. Refused here, and "
    "for the user to run instead: anything that writes an image version or a label set (segmentation, "
    "measuring, tracking, image processing), and any run that would replace an existing output — the "
    "refusal names a free name to use instead. Before a long or GPU run, say what it is and roughly how "
    "long it takes."
)
if no_gui_enabled():
    INSTRUCTIONS += NO_GUI_INSTRUCTIONS
if gui_mode_enabled():
    INSTRUCTIONS = GUI_INSTRUCTIONS

_client = AutonomousClient()   # this user's backend: loopback.default_base_url()
mcp = CallerMCPServer("cecelia-autonomous", instructions=INSTRUCTIONS)


def _tool(fn):
    @functools.wraps(fn)
    def wrapped(*args, **kwargs):
        try:
            return fn(*args, **kwargs)
        except ToolError:
            raise
        except LockViolation as e:
            raise ToolError(str(e)) from e
        except Exception as e:   # a server error or crash also points at report_issue
            raise ToolError(failure_text(e)) from e
    return mcp.tool(annotations=annotations_for(fn.__name__))(wrapped)   # its kind, from tools.json


@_tool
def get_lock() -> dict:
    """Which project this session may run work in, and the output-name prefix it must use (empty =
    none). Call first; every other tool refuses a different project. `projectUids` lists every
    allowed project — in a session with no GUI, the projects it created join it; with the GUI
    (`gui: true`) it is the project open in Cecelia, and follows the user when they switch."""
    if gui_mode_enabled():
        open_uid = _client.gui_project()
        return {"projectUid": open_uid, "projectUids": [open_uid] if open_uid else [],
                "prefix": "", "noGui": False, "gui": True}
    return {"projectUid": locked_project(), "projectUids": allowed_projects(),
            "prefix": required_prefix(), "noGui": no_gui_enabled(), "gui": False}


def _no_gui_tool(fn):
    """A tool that exists only in no-GUI mode — the eval sandbox never sees it."""
    return _tool(fn) if no_gui_enabled() else fn


@_no_gui_tool
def create_project(name: str) -> dict:
    """Create a new, empty project (named `name`, unique) and add it to this session's lock. Returns
    `{projectUid}`. Then create_set, register_images, and run_task `importImages.omezarr`."""
    return _client.create_project(name)


@_no_gui_tool
def create_set(project_uid: str, name: str) -> dict:
    """Create a set (a group of images, e.g. one experiment or one condition) in the project. Returns
    `{setUid}`. A set name must be unique in the project."""
    return _client.create_set(project_uid, name)


@_no_gui_tool
def register_images(project_uid: str, set_uid: str, paths: list[str]) -> dict:
    """Add image files to a set — absolute paths to microscopy files (.czi, .lif, .nd2, .ims, .tif,
    .ome.tiff …) on this machine. Returns `{images: [{imageUid, name}], skipped}`; `skipped` lists
    paths that are not existing files. The images are PENDING until converted: run_task
    `importImages.omezarr` with their uids (get_module_params lists its params; the defaults suit
    most files), then wait_for_tasks."""
    return _client.register_images(project_uid, set_uid, paths)


@_tool
def run_task(project_uid: str, fun_name: str, params: dict, image_uids: list[str],
             set_uid: str = "") -> dict:
    """Start ONE task, exactly as the module page's Run button does. `fun_name` is `category.task`
    (get_module_params on the observer lists them with every param's key/default/range — pass the
    params you mean; omitted ones take the spec default). Image-scope tasks start once per image;
    set-scope tasks (e.g. `behaviour.hmm`) start once with all `image_uids` + `set_uid`.

    Returns `{tasks: [{taskId, imageUid, status, log}]}` — `status` is "submitted" or "failed" (an
    immediate refusal; `log` says why). Then call wait_for_tasks with the ids. A task that NAMES an
    output (a segmentation, an HMM column) is refused unless the name carries the session prefix.
    A segmentation under a name the image already has is refused with a free one to use instead — use
    it. `overwrite: true` is refused unless every output carries the prefix (only this session's own
    outputs). In a GUI session, a run that writes an image version or a label set is refused, and so is
    any run that would replace an existing output — that refusal also names a free one."""
    out = _client.run_task(project_uid, fun_name, params, image_uids, set_uid)
    return note_failed_runs(out, any(t.get("status") == "failed" for t in out.get("tasks") or []))


@_tool
def wait_for_tasks(task_ids: list[str], timeout_s: int = 900) -> dict:
    """Block until every task in `task_ids` reaches a terminal state (done / failed / cancelled /
    interrupted) or `timeout_s` passes (max 1800). Returns `{finished, states: {taskId: status}}`.
    A task's log is the observer's get_task_log(project, image, fun)."""
    out = _client.wait_tasks(task_ids, min(max(timeout_s, 10), 1800))
    return note_failed_runs(out, "failed" in (out.get("states") or {}).values())


@_tool
def run_chain(project_uid: str, chain_name: str, image_uids: list[str]) -> dict:
    """Run a chain (authored with the observer's create_chain) over `image_uids` — the
    same `chain:run` the Chains page's Run button sends. Every node's params are checked against the
    lock first. Returns `{runId}`; then wait_for_chain. To change a chain, create a new one under a
    new name (create_chain never overwrites) and run that — both stay on the Chains page as the record."""
    return _client.run_chain(project_uid, chain_name, image_uids)


@_tool
def wait_for_chain(project_uid: str, run_id: str, timeout_s: int = 1800) -> dict:
    """Block until chain run `run_id` stops (no node queued or running) or `timeout_s` passes (max
    3600). Returns per-image node statuses — `failed` / `skipped` nodes are where to look; the
    observer's get_task_log has the error. `waiting: true` = parked at a manual step for a person:
    not finished, and nothing here can continue it."""
    out = _client.wait_chain(project_uid, run_id, min(max(timeout_s, 10), 3600))
    return note_failed_runs(out, any(s == "failed" for nodes in (out.get("imageStates") or {}).values()
                                     for s in (nodes or {}).values()))


# The gating toolset — the observer's, defined ONCE in gating_views.py. This server's client adds the
# project lock, the output-name prefix and the measured-segmentation check (`_gating_guard`).
# The figure toolset (figure_tools.py) — the observer's too; the project lock applies through `_request`.
# Not in GUI mode: the observer beside it already offers both, and a second copy of every gating tool
# would only split which one Claude picks.
if not gui_mode_enabled():
    globals().update(register_gating_tools(_tool, _client))
    globals().update(register_figure_tools(_tool, _client))


def main():
    mcp.run()


if __name__ == "__main__":
    main()
