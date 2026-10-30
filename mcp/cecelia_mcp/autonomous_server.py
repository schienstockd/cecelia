"""cecelia-autonomous: the MCP server an UNATTENDED agent gets beside the read-only observer — start
work, wait for it, draw gates. Never wired into a GUI user's session. Two callers: the agent-eval runner
(`scripts/agent_eval/run_app.py`) starts it with ``CECELIA_MCP_PROJECT`` set to a disposable project,
and the chat launcher (`chat.py`, ``CECELIA_MCP_NO_GUI=1``) for a user with no GUI at all.
The guard and its allow-list live in ``cecelia_mcp.autonomous``.

Run:   CECELIA_MCP_PROJECT=<uid> [CECELIA_MCP_PREFIX=agent] PYTHONPATH=mcp:python python -m cecelia_mcp.autonomous_server
"""
from __future__ import annotations

import functools

from mcp.server.mcpserver import Image
from mcp.server.mcpserver.exceptions import ToolError

from cecelia_mcp.autonomous import (AutonomousClient, LockViolation, allowed_projects, locked_project,
                                    required_prefix)
from cecelia_mcp.caller import CallerMCPServer
from cecelia_mcp.client import ApiError, DisallowedRoute
from cecelia_mcp.gating_views import GATE_CELLS_VIEW_DOC, GATE_PLOT_DOC, GATE_SHAPE_DOC, with_doc
from cecelia_mcp.no_gui import no_gui_enabled

INSTRUCTIONS = (
    "Runs analysis in ONE Cecelia project (the locked one) without a human at the keyboard. Use it "
    "beside cecelia-observer: the observer READS (lineage, measures, task logs, behaviour) and "
    "DESIGNS (create_chain); this server STARTS work (run_task, run_chain), WAITS for it "
    "(wait_for_tasks, wait_for_chain) and GATES (gate_histogram → add_gate → gate_stats). "
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
if no_gui_enabled():
    INSTRUCTIONS += NO_GUI_INSTRUCTIONS

_client = AutonomousClient()   # this user's backend: loopback.default_base_url()
mcp = CallerMCPServer("cecelia-autonomous", instructions=INSTRUCTIONS)


def _tool(fn):
    @functools.wraps(fn)
    def wrapped(*args, **kwargs):
        try:
            return fn(*args, **kwargs)
        except (ApiError, DisallowedRoute, LockViolation) as e:
            raise ToolError(str(e)) from e
    return mcp.tool()(wrapped)


@_tool
def get_lock() -> dict:
    """Which project this session may run work in, and the output-name prefix it must use (empty =
    none). Call first; every other tool refuses a different project. `projectUids` lists every
    allowed project — in a session with no GUI, the projects it created join it."""
    return {"projectUid": locked_project(), "projectUids": allowed_projects(),
            "prefix": required_prefix(), "noGui": no_gui_enabled()}


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
    A name the image already has is refused with a free one to use instead — use it. `overwrite: true`
    is refused unless every output carries the prefix (only this session's own outputs)."""
    return _client.run_task(project_uid, fun_name, params, image_uids, set_uid)


@_tool
def wait_for_tasks(task_ids: list[str], timeout_s: int = 900) -> dict:
    """Block until every task in `task_ids` reaches a terminal state (done / failed / cancelled /
    interrupted) or `timeout_s` passes (max 1800). Returns `{finished, states: {taskId: status}}`.
    A task's log is the observer's get_task_log(project, image, fun)."""
    return _client.wait_tasks(task_ids, min(max(timeout_s, 10), 1800))


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
    observer's get_task_log has the error."""
    return _client.wait_chain(project_uid, run_id, min(max(timeout_s, 10), 3600))


@_tool
def gate_histogram(project_uid: str, image_uid: str, value_name: str, x: str, y: str = "",
                   transform: dict | None = None, pop: str = "root", bins: int = 30) -> dict:
    """The distribution to choose a gate from: per axis, quantiles + an even-width count table of
    segmentation `value_name`'s cells (inside population `pop`), AFTER `transform` — the same space
    gate coordinates are in. `x`/`y` are gateable columns (e.g. `mean_intensity_2` = channel index 2);
    `y` defaults to `x`. With a `y`, also `grid`: the joint x × y counts on a 20 × 20 grid
    (`counts[j][i]` = y-bin j low → high, x-bin i; edges in gate coordinates) — the shape of the
    cloud, to trace a polygon round a population or cut debris off a diagonal. Which columns exist depends on the measure task (mesh measures such as
    `volume_mesh` only come from the mesh-measuring tasks) — the observer's get_measure_summary lists
    them; get_image_info lists channel names in index order.
    `transform`: {"kind": "linear"} (default) | {"kind": "asinh", "cof": 150} |
    {"kind": "log", "floor": 1} | {"kind": "logicle", "T": 4096, "W": 0.5, "M": 4.5, "A": 0}."""
    return _client.gate_histogram(project_uid, image_uid, value_name, x, y or x, transform, pop,
                                  max(5, min(bins, 80)))


@_tool
@with_doc(GATE_PLOT_DOC)
def gate_plot(project_uid: str, image_uid: str, value_name: str, x: str, y: str,
              transform: dict | None = None, pop: str = "root") -> list:
    png, meta = _client.gate_plot(project_uid, image_uid, value_name, x, y, transform, pop)
    return [Image(data=png, format="png"), meta]


@_tool
@with_doc(GATE_CELLS_VIEW_DOC)
def gate_cells_view(project_uid: str, image_uid: str, value_name: str, pop: str, t: int = -1,
                    channels: list[int] | None = None, image_version: str = "") -> list:
    png, meta = _client.gate_cells_view(project_uid, image_uid, value_name, pop, t, channels, image_version)
    return [Image(data=png, format="png"), meta]


AUTO_ADD_GATE_DOC = "Add a gated population to segmentation `value_name`. " + GATE_SHAPE_DOC + """
Trace a polygon on gate_histogram's `grid`. Then CHECK it where it matters: gate_cells_view on the
new population shows which cells in the image it took. The population's path is `/<name>` (or
`<parent>/<name>`); pass it to tasks as e.g. `popsToTrack`."""


@_tool
@with_doc(AUTO_ADD_GATE_DOC)
def add_gate(project_uid: str, image_uid: str, value_name: str, name: str, gate: dict,
             parent: str = "root", colour: str = "#22c55e") -> dict:
    return _client.gating_post("/api/gating/pop/add", project_uid, image_uid, value_name,
                               name=name, parent=parent, gate=gate, colour=colour)


@_tool
def set_gate(project_uid: str, image_uid: str, value_name: str, path: str, gate: dict) -> dict:
    """Move an existing population's gate (same `gate` shape as add_gate)."""
    return _client.gating_post("/api/gating/pop/set-gate", project_uid, image_uid, value_name,
                               path=path, gate=gate)


@_tool
def delete_gate(project_uid: str, image_uid: str, value_name: str, path: str) -> dict:
    """Remove a population you drew (and its children)."""
    return _client.gating_post("/api/gating/pop/delete", project_uid, image_uid, value_name, path=path)


@_tool
def gate_stats(project_uid: str, image_uid: str, value_name: str, pop: str) -> dict:
    """Cells in population `pop` and its % of the parent. `pop` may be a gate (`/qc`) or a tracked set
    (`/qc/_tracked`, `/_tracked`) — the same paths a task takes as input."""
    return _client.pop_stats(project_uid, image_uid, value_name, pop)


def main():
    mcp.run()


if __name__ == "__main__":
    main()
