"""Cecelia MCP observer server.

Exposes the running Cecelia project to Claude over stdio: project state, images, task logs, QC, and
the lab log — plus a small set of additive writes (lab-log append, notebooks, and a chain TEMPLATE).
No destructive mutation is possible; the enforced allow-list lives in
``cecelia_mcp.client.ALLOWED_ROUTES``.

Note the split that shapes this server: Claude can DESIGN work but never START it. ``create_chain``
authors a pipeline the user then runs from the whiteboard; there is no run/submit tool, because
launching is a WebSocket message and this server speaks only HTTP.

The server also carries its own briefing — ``cecelia_mcp.guidance``. ``SERVER_INSTRUCTIONS`` goes out
with the ``initialize`` response (so a session knows to resolve the project and pull the briefing
first) and ``BRIEFING_GUIDANCE`` rides back with ``get_session_briefing``. That is what makes "check
my current project in cecelia" a sufficient prompt; **a tool added here must be named there**, or the
assistant never offers it (``mcp/tests/test_server.py`` fails if it isn't).

Run:   pixi run mcp          (or:  PYTHONPATH=mcp python -m cecelia_mcp.server)
Talks to the Julia API at $CECELIA_API_URL (default http://127.0.0.1:8080), so `pixi run dev` must
be running. See mcp/README.md for wiring this into Claude Code.

This is Phase 1 of the arc in docs/ai-assist/OBSERVER.md. Phase 2 (write tools: submit_task,
adjust_params, acknowledge_flag) is deliberately NOT wired here.
"""
from __future__ import annotations

import base64
import os

from mcp.server.fastmcp import FastMCP, Image

from cecelia_mcp.client import CeceliaClient
from cecelia_mcp.guidance import BRIEFING_GUIDANCE, SERVER_INSTRUCTIONS
from cecelia_mcp.landscape_slim import filter_landscape_tiles, slim_landscape_for_mcp
from cecelia_mcp.monitor import SessionMonitor
from cecelia_mcp.wsclient import api_url_to_ws, start_listener

# Lab-log author tag for observer-written entries. Matches the frontend's authorKind() 'claude'.
CLAUDE_AUTHOR = "Claude"

# The CLOSED set of author tags the observer may write under. A lab-log tag is a provenance claim —
# "who says so" — so it stays an enum here rather than becoming a free-text author field: [Claude] is
# the assistant's own reasoning, [LabArchives] is content sourced from the ELN. The tag itself is
# still injected server-side (append_lab_log!), so neither is forgeable from the prompt.
LAB_LOG_SOURCES = {"claude": CLAUDE_AUTHOR, "labarchives": "LabArchives"}

_API_URL = os.environ.get("CECELIA_API_URL", "http://127.0.0.1:8080")
_client = CeceliaClient(base_url=_API_URL)
_monitor = SessionMonitor()
# `instructions` reaches the client in the `initialize` response and lands in its system prompt, which
# is what makes "check my project in cecelia" enough on its own — the assistant knows to resolve the
# project and pull the briefing without the user pasting a prompt. Kept short on purpose; the long
# form is delivered by get_session_briefing. See cecelia_mcp/guidance.py for the split and its budget.
mcp = FastMCP("cecelia-observer", instructions=SERVER_INSTRUCTIONS)


@mcp.tool()
def list_projects() -> list:
    """Every Cecelia project on this machine, MOST-RECENTLY-OPENED FIRST — so for "my project" / "my
    current project" with no id given, the first entry is the one the user is working in. Name it back
    to them instead of asking for a uid (they mostly do not know it; it is in the app's title bar).

    `lastOpenedAt` is stamped when a project is OPENED in Cecelia, so the order tracks what the user
    has actually been looking at — not when the data was created. Two caveats worth stating out loud
    rather than guessing past: a project the user opened in a different install/projects dir is not
    here, and if they switch projects in the app mid-session this order goes stale. If the top entry
    is not the obvious match for what they asked about, say what you found and let them pick."""
    return [
        {"uid": p.get("uid"), "name": p.get("name"),
         "lastOpenedAt": p.get("lastOpenedAt"), "createdAt": p.get("createdAt")}
        for p in _client.get_projects().get("projects", [])
    ]


@mcp.tool()
def find_object(query: str, limit: int = 50) -> dict:
    """WHICH PROJECT a uid belongs to — call this the moment the user quotes an id you have no project
    for ("what happened to image p6t4mC?", a uid in a note, a filename, a lab-log line). Every other
    tool here needs a `project_uid`; a uid on its own does not carry one, so without this the only way
    to find it was list_images over every project until one matched. One call instead.

    `query` is a uid OR a name fragment:
      - a UID matches EXACTLY (case-sensitive) — image, set or project alike;
      - if nothing matches, it falls back to a case-insensitive substring search over image, set and
        project NAMES, so "shows me the mertk one" is answerable too.

    Returns {query, matchedBy: "uid"|"name", count, truncated, matches: [...]}, each match {kind:
    "image"|"set"|"project", uid, name, projectUid, projectName, + setUid/setName/status/included for
    an image, imageCount for a set}. Take `projectUid` from the match and carry on with the normal
    tools (get_image_info, get_session_briefing, …).

    `count: 0` means it is in NO project on this machine — say that rather than guessing at a
    near-match; a uid the user pasted from elsewhere may belong to another install or projects dir.
    Names are not unique, so a name search can return several matches across projects — offer them,
    don't pick. `truncated: true` means more matched than `limit`: narrow the query rather than
    presenting the list as complete."""
    return _client.find_object(query, limit)


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
        "imageCount": data.get("count"),
        "sets": data.get("sets"),
        "statusBreakdown": statuses,
        "excludedCount": excluded,
    }


@mcp.tool()
def list_images(project_uid: str) -> list:
    """Every image in the project: uid, name, processing status, which set it belongs to, `attr` (its
    attribute ASSIGNMENT, e.g. `{"Mouse": "3", "Location": "b"}`), and `included` — false means
    EXCLUDED from analysis (a silent member; downstream/cohort counts should drop it). An excluded
    image that is still "done" is intentional but easy to miss — see its note.

    Use `attr` to size the groups before choosing a cross-image plot: get_image_attributes says what you
    MAY group by, this says how many images land in each group once the excluded ones are dropped. A
    group of one is not a comparison."""
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
      - "segment.branching"          → nBranches, meanBranchLength, anisotropy
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

    Most metrics are COUNTS, but a few are ratios in 0–1 (`anisotropy`, `dominantStateFrac`,
    `largestClusterFrac`, `fracAggregated`, `fracInContact`) — quote those as fractions, not totals.
    `anisotropy` is structural directionality: 0 = uniform, 1 = non-uniform; real fibrous tissue
    sits around 0.1–0.4, so a low value is not a defect.

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
    `valueName`, `at` (timestamp), `status` (the image's current status), **`runStatus`** — that run's
    outcome — and **`params`**, the params that run used (the tuning trail; `{}` on older runs). Pair
    `params` with get_module_params + get_cohort_qc to suggest a param adjustment on an outlier: what
    was tried, the valid range, the direction to try. It is "what was tried", not a params→outcome
    relationship — suggest, don't predict.

    `runStatus` is one of `done` · `failed` · `cancelled` · `interrupted` · `running`. The last three
    mean **no output was produced**, so never read a result as belonging to one:
      * `cancelled` — someone stopped it.
      * `interrupted` — its process died mid-run (a backend/runner Ctrl-C or crash). Nobody chose this
        and nobody was told; a user staring at a half-finished batch asking "what happened to the other
        three?" is answered HERE and almost nowhere else.
      * `running` — live right now, outcome not yet known.
    A long-running `fun` that keeps coming back `interrupted` is worth raising as a workflow problem
    (work is being lost to restarts), not as a param problem.

    Watch `runStatus`: the same `fun` showing `"failed"` repeatedly on one image is a stuck point worth
    flagging (e.g. "hmm failed 5x on image KDIeEm — want to look at the params?"). **This is the place
    to catch repeated failures** — a failed task leaves little other trace, and the live-pattern
    detector (`poll_observations`) starts empty each run, so it won't have older failures. Cross-check
    `get_task_log` / `get_recent_logs` for the actual error before surfacing."""
    return _client.get_task_history(project_uid, limit).get("history", [])


@mcp.tool()
def get_module_params(category: str = "") -> dict:
    """Task PARAMETER SPECS — the valid range / default / type of every task's params. Read this before
    suggesting a parameter change, so the suggestion is IN RANGE and names the real param `key`.

    Returns `{category: [{fun_name, label, params: [{key, label, type, default, tip}]}]}` — trimmed to
    the suggestion-relevant fields (UI-widget plumbing is stripped). Numeric knobs (`type` int/float)
    also carry `min`/`max`/`step`. Pass `category` (the part before the dot in a fun_name — e.g.
    "tracking" for "tracking.bayesian_tracking") to get just that module; omit it for all modules.

    **A `group` or `section` param NESTS its real knobs under its own `params`** — cellpose's diameter and
    channel assignment live inside its `models` group, not at the top level. When you set one, send it
    nested the same way (`{"models": {"cellDiameter": 30}}`); that is how the whiteboard stores it and how
    the task reads it. Read the LABEL as well as the key: a unit usually lives there (`cellDiameter` is
    labelled "Cell diameter (µm)", so its default of 10 is 10 µm, not 10 px).

    A `select` param also carries `options` — its full list of legal values. Use one of them verbatim
    rather than echoing the default; anything else is not a value the task can take.

    **Selection params name live project state, which is NOT in the spec** — their candidates are absent
    here by design, so resolve them per project before you set one (this is where an under-informed guess
    usually happens). `type` (plus `field` / `popScope`) tells you which tool answers it:

      | param `type`             | what it wants                  | get the candidates from |
      |--------------------------|--------------------------------|-------------------------|
      | `channelSelection`       | a channel of the image         | get_image_info → `channels` |
      | `valueNameSelection`     | a versioned field's value_name | get_image_info (`field`, e.g. filepaths/labels) + get_analysis_lineage → `segmentations` |
      | `popSelection`           | a population path              | get_populations (`popScope` cells vs tracks) |
      | `labelPropsColsSelection`| measure columns                | get_measure_summary → the `measures` names |
      | `motionDimsSelection`    | motion dims                    | leave at `auto` unless the user says otherwise |

    Two honest limits. A value_name a LATER node will create does not exist yet (segment writes the label
    set that tracking then reads) — so read the chain's own wiring for those, not the project. And a
    population produced by a node in the same chain cannot be resolved at author time at all; leave it and
    say so. Project-independent; static package specs (plus any user drop-in modules). Suggest, cite the
    current value + range + QC; the user runs it — you don't."""
    return _client.get_module_params(category or None)


@mcp.tool()
def get_available_plots(module: str = "") -> list:
    """The plot types the analysis board can render — use this to SUGGEST a visualization ("plot the HMM
    state frequencies as a bar chart") or to pick the chart for a notebook.

    Returns `[{id, label, module, family, chartTypes, dataSource:{popType, granularity, measure,
    measureOptions}, scopeModes, …}]`. `chartTypes` = the applicable charts (boxplot/violin/bar/
    histogram/…); `dataSource` = what data it needs (population type, granularity, measure);
    `scopeModes` = per_image / summarised (pooled across a set). Pass `module` to narrow to one module
    page's plots; omit for all. Project-independent; read-only. These are the board's OWN plots — for a
    notebook you reproduce the equivalent with AlgebraOfGraphics (see get_repl_api)."""
    return _client.get_available_plots(module or None)


@mcp.tool()
def add_analysis_board(project_uid: str, name: str, plots: list, template: str = "",
                       compare_by: str = "") -> dict:
    """ADD one Analysis board to the project — a figure the user opens on the /analysis page.

    Additive and one board per call: this cannot modify, rename, reorder or delete any board. It lands
    BESIDE the user's own boards and they delete it in one click, so a board you got wrong costs them a
    click, not their work. 409 if the name is taken (pick another; never try to replace theirs).

    `plots` is a list, in reading order, of:
      {plot, measure?, chart?, pops?, groupBy?, statUnit?, imageAgg?}
      - `plot`     the plot-spec id from get_available_plots (e.g. "track_measures"). REQUIRED.
      - `chart`    one the spec offers ("boxplot", "violin", …); defaults to its first.
      - `measure`  one the spec carries (e.g. "live.track.speed"); defaults to the spec's own.
      - `pops`     populations as "valueName/pop" — EXACTLY as get_populations and get_analysis_boards
                   report them (e.g. "B/qc/_tracked"). A population that does not exist is rejected.
      - `groupBy`  a categorical column to split by (e.g. "live.cell.hmm.state.movement"). This splits
                   the plot by a measured VALUE; it is not the experimental grouping (see below).
      - **`popType` is NOT a field here.** It is DERIVED from the populations you name. get_analysis_boards
        reports one because that is what got stored — do not copy it back: a `popType` that disagrees
        with a population's own type produced a board where every panel said "Select one or more
        populations", and the request is now rejected rather than written.
      - `statUnit` "individual" (every cell/track a point) or "image" (each image collapsed to one
                   `imageAgg`, "mean"/"median"). PREFER "image" when per-image n is small — pooling
                   every track across images treats one image's 400 tracks as 400 replicates.
    `template` is "<cols>x<rows>" (e.g. "2x2"); omitted picks a grid that fits. The comic plates are
    GUI-only. Grid areas, styling and captions are the user's — you choose which plots, in what order.

    RESOLVE WHAT IS RESOLVABLE FIRST, like create_chain. get_analysis_boards for what they already
    built (match their measures and populations rather than inventing your own, and don't rebuild a
    board that exists — two boards differing only in `statUnit` are NOT duplicates); get_populations
    for the exact pop strings; get_available_plots for the spec ids and the charts each offers;
    get_measure_summary for whether a measure has the n to be worth plotting; get_image_attributes +
    list_images' `attr` before anything cross-image. Pick the canonical clustering run rather than
    guessing among leftovers, and drop excluded images.

    `compare_by` is what the board compares ACROSS IMAGES — board-level, so it governs every plot on it:
      - omitted        the app's default: one image at a time. A board with no `compare_by` is NOT a
                       cross-image figure, whatever its plots are.
      - "per_image"    one series per image
      - "summarised"   the whole set pooled into one series
      - an ATTRIBUTE NAME (e.g. "Mouse") groups images sharing that value into one series labelled by
        it — the experimental comparison. Two may be combined: "Treatment,Mouse".
    **This is the difference between a board and a figure.** If the user asks "does X differ between
    mice/treatments", the answer is `compare_by="Mouse"`, not a per-image board with a caveat. Take the
    name from get_image_attributes (the server rejects one the project does not have) and size the
    groups with list_images' `attr` FIRST: grouping by an axis where each group holds one image is not a
    comparison, and you should say so instead of drawing it.

    A spec the project cannot plot comes back 422 with a message naming what WAS available — read it
    and resubmit rather than reporting failure. What no validation can check is INTENT: a well-formed
    board built on the wrong clustering run is still wrong, and it is yours to get right. So say in
    chat which values you read from the data and which you defaulted, and tell the user the board was
    added beside their own. Also give it a PLAIN name — write "Behaviour & tracking", never "&amp;";
    you cannot rename it afterwards."""
    return _client.add_analysis_board(project_uid, name, plots, template, compare_by)


@mcp.tool()
def get_analysis_boards(project_uid: str) -> dict:
    """The Analysis boards this project already has, and WHAT EACH ONE SHOWS — read this before
    proposing a figure, so you extend the user's thinking instead of rebuilding it.

    Returns `boards: [{name, cols, rows, plots: [{slot, kind, ref, measure?, chart?, popType?,
    groupBy?, statUnit?, imageAgg?, pops?, highlight?, features?, title?}]}]`. `ref` is the plot-spec
    id (summary) or interactive view key; `pops` are the plotted populations as `valueName/pop`. Empty
    slots are omitted, so `plots: []` means a board exists but is blank. A SUMMARY, not the stored
    layout — grid geometry and styling are the user's and are not exposed.

    `statUnit` is the SUMMARY LEVEL, and two boards that differ only there are NOT duplicates:
    "individual" plots every cell/track as its own point, "image" collapses each image to one
    `imageAgg` (e.g. mean) value first — the same measures asked at two different levels, which is a
    normal and deliberate pair to keep side by side. Read it before saying a board repeats another.
    It is stored explicitly and removed where a plot has no summary level, so an ABSENT `statUnit`
    means exactly that — not "left at the default". `imageAgg` ("mean"/"median") comes with it and
    says how each image is collapsed.

    Use it to (a) not duplicate a board that already answers the question, (b) match the measures and
    populations the user already chose rather than inventing your own, and (c) name a new board so it
    reads beside theirs. `get_analysis_lineage` also lists board names; this is the plot detail."""
    return _client.get_analysis_boards(project_uid)


@mcp.tool()
def get_image_attributes(project_uid: str, set_uid: str, image_uids: str = "") -> dict:
    """The per-image ATTRIBUTES on a set — `{attrs: [{name, values}]}`, e.g.
    `[{name: "Mouse", values: ["1","2","3","4"]}, {name: "Location", values: ["a","b","c","d"]}]`.

    These are the axes a plot can GROUP BY. Without them you can only plot per-image or pooled, which
    throws away the comparison the experiment was designed around — four images from one mouse are not
    four replicates. Check this before proposing any cross-image plot, and say which attribute you
    grouped by and why.

    `set_uid` comes from get_project_info's `sets`; attributes are a SET-level concept, so a single
    image has none and an empty `attrs` means the set was never annotated (offer per-image or pooled,
    and say the grouping is unavailable rather than inventing one from filenames). Optional
    `image_uids` (comma-separated) narrows to a subset. Values are the DISTINCT values present, not
    the per-image assignment — for which image has which value, use list_images. Read-only."""
    return _client.get_image_attributes(project_uid, set_uid, image_uids or None)


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
      - `boards`: analysis-board tab NAMES (best-effort). For what each board actually plots, use
        get_analysis_boards — this is the cheap name-only view.
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
      fraction}]}`, and a top-level `featuresByRun: {suffix => features}`.
      - `suffix` is the run id (e.g. "movement"/"test"); `granularity` "cell" = clustPops, "track" =
        clustTracks. The measure list a run clustered on is in `featuresByRun[suffix]` (same for every
        image, so it's given once — not repeated per entry).
      - ONE `suffix` on SEVERAL `valueName`s is ONE JOINT run over those segmentations, not a run each:
        clustering pools the selected populations, so cluster 3 means the same thing on each of them and
        any named cluster populations are shared across them all (get_populations lists those names
        under EVERY member segmentation). Read the sizes per valueName, but the run once.
      - `largestFrac` near 1.0 (one cluster swallowing most points) or a very low `nClusters` vs peers
        means a near-uninformative / collapsed clustering for that image — worth flagging.
    Summary-level (sizes, not raw cluster assignments). Reads current on-disk state."""
    return _client.get_cluster_summary(project_uid, image_uid or None, set_uid or None)


@mcp.tool()
def get_spatial_stats(project_uid: str, image_uid: str = "", set_uid: str = "") -> dict:
    """SPATIAL summary per image — spatial region clustering + pairwise cell-type contact statistics.
    Scope with `image_uid` / `set_uid`; omit both for the whole project.

    Per image:
      - `regionRuns`: list, one per (segmentation × region run) —
        `{valueName, suffix, nRegions, n, largestFrac, sizes: [{value, n, fraction}]}`. Spatial regions
        are neighbourhood-composition niches (what cell types surround each cell); a cell has BOTH a
        cluster label and a region label. `suffix` is the run id.
      - `contactStats`: list, one per neighbourStats run —
        `{suffix, graphSuffix, basis: [populations], nCells, nEdges, coverage, nPermutations,
        pairs: [{popA, popB, observed, expected, logOdds, zScore, pValue, significant,
        association: associated|avoided}]}`. `logOdds` is the CODEX observed-vs-expected contact
        log-odds ratio — the EFFECT SIZE: > 0 = the two cell types selectively ASSOCIATE
        (co-localise), < 0 = they AVOID each other. `zScore`/`pValue` are the SIGNIFICANCE, from
        `nPermutations` random relabellings of the same neighbour graph: they answer "is this more
        than a random arrangement of these cell types would give?". Both are null when the test was
        skipped (nPermutations = 0), in which case logOdds is descriptive only. `pValue` cannot go
        below 1/(nPermutations+1), so p at that floor means "no permutation matched it", not p=0.
        `coverage` is the fraction of the graph's cells that were in `basis` — a low value means the
        statistics cover a small slice of the graph. Use this to answer "which cell types co-localise
        or avoid each other, and is it real?".
    Summary-level, reads current on-disk state (region columns + spatialStats sidecars)."""
    return _client.get_spatial_stats(project_uid, image_uid or None, set_uid or None)


@mcp.tool()
def get_chains(project_uid: str) -> dict:
    """The project's whiteboard CHAINS — the wired analysis pipelines and their runs. Use this to see the
    INTENDED pipeline (which task feeds which) and which chains were actually executed — the run log is a
    recent capped window, so a pipeline that ran earlier may have no dated steps, but its chain is here.

    Returns:
      - `templates`: `[{name, nodes: [{id, fun, scope}], edges: [{from, to}], startTargets}]` — the wired
        DAG per chain (`fun` is the task, `scope` image/set/incremental; `edges` are node→node). This is
        the configured pipeline, distinct from what the run log shows actually ran.
      - `runs`: recent chain executions, newest first — `{id, chainName, at, imageCount, nodeStatus}`;
        `nodeStatus` counts node outcomes across images (done/failed/skipped/…). A run with `failed` nodes
        is a pipeline that broke partway — cross-check get_task_log.
    Project-level (no image/set scope). Reads current on-disk state."""
    return _client.get_chains(project_uid)


_BRIEFING_OPEN_ENTRIES_MAX = 8    # Decision 5 — briefing carries up to this many open blackboard entries
_BRIEFING_RECENT_CAPTURES = 5     # Decision 5 — briefing carries up to this many recent captures

# Decision 9 — the two profile sections that must be filled before the briefing treats the project
# as authored. Matches the heading text seeded by _BB_PROFILE_PLACEHOLDER_BODY in
# api/src/blackboard_api.jl; keep the two in step or the newProject check will always fire.
_PROFILE_REQUIRED_SECTIONS = ("Subject", "Goal")


def _profile_section_has_content(body: str, heading: str) -> bool:
    """True if the `## <heading>` section in `body` has at least one line that isn't blank and isn't
    the seeded placeholder marker (`_(…)_` italic parenthetical). Section runs from the heading to
    the next `## ` heading or EOF. Cheap string scan — the placeholder body is <1 KiB, real profiles
    cap at 100 KiB."""
    import re
    m = re.search(rf'^##\s+{re.escape(heading)}\s*$', body, re.MULTILINE)
    if not m:
        return False
    start = m.end()
    nxt = re.search(r'^##\s+', body[start:], re.MULTILINE)
    section = body[start:start + nxt.start()] if nxt else body[start:]
    for line in section.split("\n"):
        s = line.strip()
        if not s:
            continue
        # Seeded placeholder line: `_(…)_`. Stripped, it counts as unfilled.
        if s.startswith("_(") and s.endswith(")_"):
            continue
        return True
    return False


def _profile_is_authored(profile: dict | None) -> bool:
    """Decision 9 gate: profile counts as authored iff Subject AND Goal have real content beyond the
    seeded placeholder. Missing profile ⇒ not authored (fresh project state)."""
    if not profile:
        return False
    body = profile.get("content", "") or ""
    return all(_profile_section_has_content(body, h) for h in _PROFILE_REQUIRED_SECTIONS)


def _memory_briefing_slice(project_uid: str) -> dict:
    """Compose the PROJECT_MEMORY_PLAN Decision 5 briefing slice: profile body + open Blackboard
    entries + last N captures. Split out so `get_session_briefing` stays legible, and so each
    upstream call's failure degrades independently rather than blanking the whole memory slice.
    Every field's default is empty/None — a fresh project with no memory yet gets an honest shape,
    not a placeholder.
    """
    profile = None
    open_entries: list[dict] = []
    try:
        entries = _client.list_blackboard_entries(project_uid).get("entries", [])
    except Exception:
        entries = []
    for entry in entries:
        entry_id = entry.get("entryId")
        if entry_id == "profile":
            # Fetch the full body — the profile is the "what is this project" record; a truncated
            # snippet would defeat the point. It's capped at 100 KiB per Blackboard invariants.
            try:
                full = _client.read_blackboard_entry(project_uid, "profile")
                body = full.get("entry", {})
                profile = {
                    "content":   body.get("content", ""),
                    "updatedAt": body.get("updatedAt", ""),
                    "status":    body.get("status", "open"),
                }
            except Exception:
                profile = None
            continue
        if entry.get("status", "open") != "open":
            continue
        open_entries.append({
            "entryId":          entry_id,
            "title":            entry.get("title", ""),
            "updatedAt":        entry.get("updatedAt", ""),
            "attachmentsCount": entry.get("attachmentsCount", 0),
        })
        if len(open_entries) >= _BRIEFING_OPEN_ENTRIES_MAX:
            break

    try:
        captures = _client.get_recent_captures(
            project_uid, _BRIEFING_RECENT_CAPTURES).get("items", [])[:_BRIEFING_RECENT_CAPTURES]
    except Exception:
        captures = []
    recent_captures = [{
        "captureId": c.get("captureId"),
        "createdAt": c.get("createdAt"),
        "surface":   c.get("surface"),
    } for c in captures]

    return {
        # PROJECT_MEMORY_PLAN Decision 9 — newProject is TRUE when the profile hasn't been filled
        # in past its seeded placeholder (Subject + Goal are what the briefing enforces). Guidance
        # tells Claude to greet + ask before proposing anything in that state.
        "newProject":            not _profile_is_authored(profile),
        "profile":               profile,
        "openBlackboardEntries": open_entries,
        "recentCaptures":        recent_captures,
    }


@mcp.tool()
def get_session_briefing(project_uid: str) -> dict:
    """Startup context for THIS session — call this FIRST when a chat begins, so you're oriented without
    the user re-explaining. Returns:
      - `projectName`, `imageCount`, `excludedCount` (how many of them are EXCLUDED from analysis —
        subtract before quoting a cohort size)
      - `flagged`: images with a warn/fail QC finding (same source as the app's image table) —
        `[{uid, name, worst: warn|fail, included, findings: [{level, short, fun}]}]`.
        **`included: false` means the user already dropped that image** — do not lead with its
        anomalies; they are usually WHY it was dropped. Lead with the flagged images that still count,
        and mention the excluded ones as already handled.
        `fun` is the task whose QC banked the finding: check it before believing a number. A probe or
        example module banking a hardcoded threshold looks exactly like a real pipeline finding
        otherwise ("4 images measured 0 cells" once came from a test probe, not segmentation).
      - `profile`: the project's Blackboard profile entry (Decision 2) —
        `{content, updatedAt, status}` — full markdown body, capped at 100 KiB. `null` if the
        project has no blackboard yet (it will after the first list_blackboard_entries call, so this
        is usually a fresh-project state). READ IT — this is what the profile exists for; don't
        skip past it.
      - `newProject` (bool): TRUE when the profile hasn't been filled in past its seeded
        placeholder (Decision 9 — Subject and Goal are the two required sections). When TRUE, don't
        propose anything until you've greeted the user and asked them to describe subject + goal;
        a briefing with no signal to lean on is worse than one that admits it needs signal.
      - `openBlackboardEntries`: up to 8 entries with status="open", newest-first —
        `[{entryId, title, updatedAt, attachmentsCount}]`. What's currently on the table across
        sessions. Reach for `read_blackboard_entry` on any that look relevant to what the user is
        about to ask.
      - `recentCaptures`: up to 5 shared frames, newest-first — `[{captureId, createdAt, surface}]`.
        Same shape as `get_recent_captures` minus the address (call `get_capture(captureId)` for
        the address + pixels when a specific one matters).

      - `guidance`: HOW TO WORK WITH THIS PROJECT — the disciplines that span tools (what to check
        before proposing any figure or cross-image comparison, and the rules for the few things you can
        write). Read it before you propose anything; it is written to be followed, not summarised.

    Deliberately DROPPED from the default response (PROJECT_MEMORY_PLAN Decision 5): the 7-day
    lab-log slice. The lab-log is a chronological record whose value is post-hoc; the durable
    "what's on the table" signal now comes from open Blackboard entries. `read_lab_log(project_uid)`
    is still available for the chronological view when a specific question needs it.

    Use this to open with what matters ("3 of 12 images flagged; 2 have too few tracks; profile
    says CD169 macrophages under MERTK KO — is that still the focus today?") and to pick up where
    the last session left off (open Blackboard entries + profile). Then ask the user which
    direction to take. Read-only."""
    # The guidance rides along with the briefing rather than sitting in the server instructions: it is
    # ~600 words that only matter once a session actually opens a project, and the observer is
    # registered user-scope, so in the instructions it would be in context for every unrelated `claude`
    # session on the machine. Server-side, not pasted by the user — that is the whole point (see
    # guidance.py). Merged into the response so one call orients AND briefs.
    base = _client.get_session_briefing(project_uid)
    # PROJECT_MEMORY_PLAN Decision 5 — recentLabLog drops out of the default; the durable memory
    # comes from the profile + open Blackboard entries. read_lab_log still serves the chronological
    # view on demand.
    base.pop("recentLabLog", None)
    return {**base, **_memory_briefing_slice(project_uid), "guidance": BRIEFING_GUIDANCE}


@mcp.tool()
def get_labarchives_context(project_uid: str) -> dict:
    """The project's LabArchives (ELN) context IN FULL — what the experiment is, as recorded in the
    lab notebook. The session briefing carries only the section headings + gaps; call this when you
    need the text.

    Returns `{source: {notebookName, url, …}, syncedAt, sections: [{heading, lines, sourceDate, url}],
    cohort: [{attr, value, n}], gaps: [{attr, value, declared, present}]}`.

    `gaps` is the important part and is DERIVED live, never stored: arms the ELN says exist that the
    project has no images for. Image attribute levels come from the images PRESENT, so a deleted arm
    leaves no trace inside cecelia — the ELN is the only record that the comparison was ever planned.
    Treat a gap as a real absence, but NOT as an error: it can mean not-yet-imaged, failed QC, or
    deliberately dropped, and those are indistinguishable from here. Ask, don't assume — and when the
    user explains it, append that reason with append_lab_log(source="labarchives").

    Empty (`present: false`) simply means nobody has linked a notebook to this project yet. Read-only."""
    return _client.get_labarchives_context(project_uid)


@mcp.tool()
def get_repl_api() -> dict:
    """The Cecelia REPL / notebook data-access surface — read THIS before writing any `using Cecelia`
    code (a Pluto notebook, a REPL snippet). It is the ground truth for the interface; do not guess
    function names or signatures.

    Returns:
      - `api`: `[{name, exported, documented, doc}]` — the notebook-safe read accessors
        (load_project, images, image, pop_df, label_props + the fluent view, track_props,
        plot_summary_data, …) with their LIVE docstrings (signatures + kwargs). Generated by
        introspecting the running package, so it can never be stale.
      - `doc`: the docs/REPL.md cookbook — the load→navigate→read idiom, the `|>` label-props chain,
        and the NOTEBOOK WRITE RULES (figures/CSV only; never .h5ad / QC store / lab log / ccid.json).

    Population paths/types are project-specific — get those from get_populations, not here. This tool is
    project-independent (the interface is the same for every project). Read-only."""
    return _client.get_repl_api()


@mcp.tool()
def read_lab_log(project_uid: str) -> str:
    """The full lab-log markdown for the project — the accumulated cross-session memory."""
    return _client.read_lab_log(project_uid).get("content", "")


@mcp.tool()
def list_notebooks(project_uid: str) -> dict:
    """List a project's notebooks (name, file, description, current version) plus the shipped examples.
    Use it to find the `file` for get_notebook / set_notebook_description when the user refers to a
    notebook by name."""
    return _client.list_notebooks(project_uid)


@mcp.tool()
def get_notebook(project_uid: str, file: str) -> dict:
    """Read a notebook's CURRENT Pluto source — including the user's own edits — so you can help when
    they're stuck ("can you have a look?"). Returns {file, scope, content}. `file` is the notebook
    filename (from list_notebooks / create_notebook, e.g. "speed.jl").

    TEACHING FLOW — the user is likely new to Julia. Read the source, explain what's wrong in plain
    terms, and walk them through the fix so they learn to do it themselves; suggest corrected cells for
    them to paste. Do NOT overwrite their notebook. If they ask you to make the changes for them,
    default to creating a NEW notebook version with create_notebook (a new name, e.g. "<name>-v2") and
    tell them first: "I'll make a new notebook version." — the original and their edits stay intact."""
    return _client.get_notebook(project_uid, file)


@mcp.tool()
def append_lab_log(project_uid: str, lines: list[str], source: str = "claude") -> dict:
    """Append a dated, tagged entry to the lab log. Append-only — never edits existing content.

    `lines` is one or more markdown lines. `source` picks the author tag, and is a CLOSED set:
      - `claude` (default) → `[Claude]` — your own reasoning, observations, methodology notes.
      - `labarchives` → `[LabArchives]` — a CHANGE sourced from the ELN. Use this only for a delta
        (a new page, an edited protocol, a cohort that moved) or for a human's explanation of a gap.
        The tag is a provenance claim, so the server REJECTS it (409) on a project with no notebook
        linked — call set_labarchives_context first, or append as `claude`.
        The current state belongs in set_labarchives_context, NOT in a log entry: the log is the
        dated record of what changed, the sidecar is what is true now.

    One of the writes the observer can make (with set_labarchives_context, create_notebook and
    set_notebook_description); all are non-destructive to project data.
    """
    author = LAB_LOG_SOURCES.get(str(source).strip().lower())
    if author is None:
        return {"error": f"unknown source {source!r}; expected one of {sorted(LAB_LOG_SOURCES)}"}
    return _client.append_lab_log(project_uid, author, lines)


@mcp.tool()
def set_labarchives_context(project_uid: str, source: dict, sections: list,
                            cohort: list | None = None) -> dict:
    """REPLACE the project's LabArchives context sidecar — the experimental background a future
    session (and the app's lab-log panel) reads to get oriented.

    Cecelia CANNOT read LabArchives itself: the connector is authenticated in the user's own Claude
    session, and the backend deliberately holds no credentials. So you are the sync — read the ELN
    with the LabArchives tools, then call this. A session with no LabArchives access still gets the
    context, because it reads what you stored here.

    - `source`: `{notebookId, notebookName, url, pageIds: [...]}` — the notebook/page(s) this came
      from. Set it EXPLICITLY from what the user pointed you at; never guess a notebook from the
      project name (searching one project name across a notebook returned 175 hits spanning 8 years).
    - `sections`: `[{heading, lines: [...], sourceDate, url}]` — the orientation, in a few short
      lines per section (Setup / Question / Protocol / Raw data). Max 12 sections, 12 lines each.
      Summarise; do not paste notebook pages.
    - `cohort`: `[{attr, value, n}]` — the experimental design AS THE ELN DECLARES IT, keyed to the
      project's image attributes (e.g. `{"attr": "Treatment", "value": "WT", "n": 6}`). This is what
      makes the gap check work, so include it whenever the notebook states a cohort — cecelia diffs
      it against the images and derives what's missing.

    This REPLACES the sidecar (it mirrors the ELN as of now; a merge would let a deleted section
    linger). It never touches the lab log — record a CHANGE there separately with
    append_lab_log(source="labarchives"). Confirm with the user before the first sync of a project."""
    return _client.set_labarchives_context(project_uid, source, sections, cohort or [])


@mcp.tool()
def list_blackboard_entries(project_uid: str) -> dict:
    """List this project's BLACKBOARD entries — shared thinking the user and you have iterated on
    across sessions, one entry per topic (Markdown + attached captureIds). Returns newest-first
    `{entries: [{entryId, title, current, updatedAt, attachmentsCount, status}]}`. Use before
    writing a new entry so you extend the topic the user already opened instead of creating a
    parallel one.

    `status` is one of `open` (still on the table), `resolved` (topic settled, entry kept as record),
    `parked` (deliberately set aside). The reserved entry `entryId="profile"` is auto-created and
    sorts to the top — it's the project's durable "what is this project" record (subject, cohort,
    goal, key channels). Read it FIRST when you open a project so you don't rediscover context
    the profile already carries. Update it via `revise_blackboard_entry` as your understanding
    deepens; retire a done thread via `set_blackboard_status(..., "resolved")`.

    Distinct from CHAINS (executable pipelines) and NOTEBOOKS (analysis code) — a blackboard entry
    is prose + diagrams; nothing here starts work."""
    return _client.list_blackboard_entries(project_uid)


@mcp.tool()
def read_blackboard_entry(project_uid: str, entry_id: str, version: int | None = None) -> dict:
    """Read a BLACKBOARD entry's current Markdown (or a snapshotted `version`). Returns
    `{entry: {entryId, title, content, current, updatedAt, versions, attachments, status}}`.
    `content` is the Markdown; `versions` is the list of snapshot ids you can pass to `version=`
    to read an older revision. `status` (open|resolved|parked) describes the LIVE entry — it's
    entry-level metadata, not versioned per snapshot, so a `version=` read returns the CURRENT
    status either way. Reach for this before `revise_blackboard_entry` so you propose the change
    on top of what actually exists, not a memory of it. `entry_id="profile"` reads the project's
    profile entry (auto-created, one per project)."""
    return _client.read_blackboard_entry(project_uid, entry_id, version)


@mcp.tool()
def create_blackboard_entry(project_uid: str, title: str, content_md: str,
                            attach_capture_ids: list[str] | None = None) -> dict:
    """Create a new BLACKBOARD entry — a shared idea worth keeping across sessions. Distinct from a
    CHAIN (executable, needs the user to Run) and a NOTEBOOK (analysis code the user opens and
    edits). This is prose + Mermaid diagrams (triple-backtick `mermaid` fences render on the
    frontend); nothing here starts work.

    `title` — ONE short label (capped at 200 chars); shown in the entries list and Kiwi.
    `content_md` — Markdown body, ≤ 100 KiB. Attach captured frames by id when the visual is
    load-bearing: `attach_capture_ids=[capX, capY]` — an unknown id is silently dropped (validated
    against the captures on disk at write time), so a stale reference doesn't fail the write.

    Create when the user asks to "record" / "keep" / "add to the board" a concept the two of you
    have been developing. Don't create speculatively — an unused entry sits in the list forever.
    Follow-up edits go through `revise_blackboard_entry`, which snapshots the pre-edit content so
    nothing is lost. Say "it's on the Blackboard" when you're done, no more."""
    return _client.create_blackboard_entry(project_uid, title, content_md, attach_capture_ids)


@mcp.tool()
def revise_blackboard_entry(project_uid: str, entry_id: str, content_md: str,
                            attach_capture_ids: list[str] | None = None,
                            note: str = "") -> dict:
    """Rewrite a BLACKBOARD entry's Markdown body. The server SNAPSHOTS the current content first
    (as a restorable version, visible in Kiwi / the /blackboard page's history), then overwrites —
    real versioning, nothing lost. Do NOT create a "<title>-v2" copy; that bypasses versioning
    and clutters the list.

    Flow: read the current entry with `read_blackboard_entry` first, propose the change to the
    user, THEN call this with the FULL new `content_md` (not a diff). `attach_capture_ids` is
    optional — OMIT to keep the entry's existing attachment set; pass an explicit list (possibly
    empty) to REPLACE it. Attachments are versioned per-snapshot: a later read at `version=N`
    returns the attachment set that was live when v<N> was captured. `note` is a short changelog
    line for a future history view; safe to include but not user-visible today.

    No-op skip: if both `content_md` AND the resolved attachment list are byte-for-byte the same
    as the current live state, the server returns `unchanged:true` and does NOT create a
    snapshot — resending the same payload doesn't clutter the history.

    404 if the entry doesn't exist. Use `create_blackboard_entry` for a brand-new entry."""
    return _client.revise_blackboard_entry(project_uid, entry_id, content_md,
                                            attach_capture_ids, note)


@mcp.tool()
def set_blackboard_status(project_uid: str, entry_id: str, status: str) -> dict:
    """Flip a BLACKBOARD entry's status without touching its content. `status` is one of `open`
    (still on the table), `resolved` (topic settled — the entry stays as a record but drops out of
    the "what's open" briefing), `parked` (deliberately set aside — same drop-out, different
    semantics). Returns `{ok:true, status}` — or `{ok:true, status, unchanged:true}` if the entry
    was already in that state.

    Does NOT create a snapshot — status is entry-level metadata, not a content revision. Use
    `revise_blackboard_entry` for content changes; use this to close a done thread, park an idea,
    or reopen one you had retired. Reach for it when: the topic of an entry has been resolved in
    this session (a decision was locked, a bug was fixed, a finding was acted on) → `"resolved"`;
    when the topic is deliberately set aside for later without deleting the entry → `"parked"`; to
    revive a parked/resolved entry that has come back up → `"open"`. Don't blanket-close entries
    as noise — a real transition, not housekeeping.

    404 if the entry doesn't exist. Never deletes; delete stays user-driven."""
    return _client.set_blackboard_status(project_uid, entry_id, status)


@mcp.tool()
def search_blackboard(project_uid: str, query: str,
                      status: str | None = None, limit: int | None = None) -> dict:
    """Search this project's BLACKBOARD entries. Case-insensitive substring over titles AND bodies;
    title matches are returned before body matches, newest-first within each. Returns
    `{results: [{entryId, title, snippet, status, updatedAt, matchType}]}` capped at `limit`
    (default 10, max 50). `matchType` is `"title"` or `"body"`; `snippet` is ±40 chars around the
    first hit.

    `status` optional (`"open"|"resolved"|"parked"`) — filter to entries in that state; omit for
    all. Use this to check "has this come up before in this project" BEFORE: proposing a phenotype
    label that sounds familiar; suggesting a processing step for an unfamiliar image; writing a
    new blackboard entry that might restate an existing one. Not reflexively on every session —
    reach for it when there's a specific thing to check. If the search returns nothing, the topic
    is genuinely new; if it returns a hit, `read_blackboard_entry` for the full context before
    proposing on top of it.

    Substring, not semantic: a query typed slightly differently from what an entry says won't
    match. If nothing comes back, try a shorter or differently-worded query before concluding the
    topic is new."""
    return _client.search_blackboard(project_uid, query, status, limit)


@mcp.tool()
def create_notebook(project_uid: str, name: str, cells: list[str], description: str = "") -> dict:
    """Create a Pluto NOTEBOOK from Julia cell sources — to answer a "give me the data / plot this"
    request with a runnable, editable artifact the user then owns. Read get_repl_api FIRST so the code
    uses the real accessors and the notebook write rules (figures/CSV only; never .h5ad / QC / lab log).

    `cells` = a list of Julia cell sources (one string per cell), e.g. loading via `init_object` /
    `pop_df` / `track_props`, computing a DataFrame, an AlgebraOfGraphics+CairoMakie plot, and a
    `CSV.write` export. The env-activation cell is prepended automatically, so DON'T include it; your
    first cell is typically `using Cecelia, DataFrames, AlgebraOfGraphics, CairoMakie, CSV`.

    `description` = ONE short line (a title-ish phrase shown in the notebook table), NOT a paragraph —
    e.g. "T/B-cell speed over time". It's capped server-side; keep it tight.

    CREATE-ONLY: 409 if `name` already exists — never overwrites (pick a new name, or use
    revise_notebook to make a new version of an existing one). After creating, tell the user it's ready
    in the **Notebooks page** — an open page auto-refreshes; if theirs was already open and doesn't show
    it, they can hit refresh. They open it, edit/iterate in Pluto (you can guide them + suggest corrected
    cells to paste), and once happy they run it without you. Non-destructive. Suggest, then create on the
    user's ask — don't spam notebooks. To reword its description afterwards, use set_notebook_description.

    REVISIONS: when the user asks you to change an EXISTING notebook, read it with get_notebook, then
    call revise_notebook — do NOT create a "<name>-v2" copy. revise_notebook snapshots the current
    notebook (a restorable version on the Notebooks page) then updates it in place, so it uses the real
    versioning and nothing is lost. Say so first: "I'll make a new version." Prefer teaching them the
    edit over doing it for them."""
    return _client.create_notebook(project_uid, name, cells, description)


@mcp.tool()
def set_notebook_description(project_uid: str, file: str, description: str) -> dict:
    """Update a notebook's description — ONE short line (title-ish, not a paragraph; capped server-side).
    Shown in the Notebooks page. Use this to reword the blurb after create_notebook — e.g. the user asks
    to make it briefer — instead of recreating the notebook. Edits ONLY the description string in the
    registry sidecar; the notebook's cells are untouched. `file` is the notebook filename create_notebook
    returned (e.g. "speed.jl"); a bare name works too. 404 if it doesn't exist. Non-destructive."""
    return _client.set_notebook_description(project_uid, file, description)


@mcp.tool()
def revise_notebook(project_uid: str, file: str, cells: list[str], description: str = "") -> dict:
    """Make a NEW VERSION of an EXISTING notebook — the correct way to change one the user already has.
    The server SNAPSHOTS the current notebook first (a restorable version, visible under History on the
    Notebooks page) then overwrites its cells, so it uses the real versioning and nothing is lost. Do
    NOT create a "<name>-v2" copy — that bypasses versioning and clutters the list.

    Flow: read the current notebook with get_notebook, tell the user "I'll make a new version", then call
    this with the full new `cells` (same rules as create_notebook — env-activation cell is prepended;
    figures/CSV only). `file` is the existing notebook's filename (bare name works; .jl appended). 409 if
    it doesn't exist — use create_notebook for a brand-new one. `description` optional (one short line,
    capped) — only changes it if you pass a non-empty value. Non-destructive: the pre-revision state is
    always snapshotted, so the user can Restore it."""
    return _client.revise_notebook(project_uid, file, cells, description)


@mcp.tool()
def create_chain(project_uid: str, name: str, nodes: list, edges: list,
                 start_targets: list | None = None) -> dict:
    """DESIGN a whiteboard chain — the wired pipeline for a project. You author it; **you cannot run
    it**. There is no run tool: starting a chain is the user's act, in the Chains whiteboard. Say so
    when you're done ("it's in the Chains whiteboard — have a look and press Run when it looks right"),
    and never imply it has started.

    `nodes` = `[{id, fn, params?, scope?, barrier_policy?, resource_pool?}]`:
      - `id` — any short unique string ("seg", "track"); `edges` reference these.
      - `fn` — a registered fun_name from get_module_params (e.g. "segment.cellpose"). A typo is
        rejected, not silently accepted.
      - `params` — **SPARSE: set only what you mean to change.** Every param you omit is filled from
        the task's spec default when the user opens the chain, so restating defaults is noise. Read
        get_module_params first so the keys are real and numbers are in range.
      - `scope` — omit it. It defaults from the task's own spec, so a set-scope (picnic) task like
        behaviour.hmm or clustTracks.cluster becomes a picnic node on its own.
      - `resource_pool` — omit unless you mean it (cpu / gpu / io / network); the task spec knows.
    `edges` = `[{from, to}]` — node id → node id, i.e. "to runs after from". Leave `start_targets`
    unset: the server fills it with the chain's roots, which is what makes the whiteboard's start dot
    appear. Pass it only to start a run PART-WAY in (then only that node and its descendants run).

    BEFORE you call this, resolve what is resolvable — a chain built without these is a guess, and the
    guesses land on the user:
      1. get_chains — the pipelines they already wired. Match their conventions and their task choices.
      2. get_analysis_lineage — what actually ran on these images, in order, and the `value_name`s it
         wrote. This is how you get the pipeline they really use (e.g. denoise BEFORE drift correction)
         and which stages are already done, instead of assuming a textbook order.
      3. get_module_params — param keys, ranges, and a `select`'s legal `options`.
      4. **get_image_info on one of the target images — for the CHANNELS.** A `channelSelection` param
         (a drift-correction reference channel, cellpose's cell/nuc channels) is unusable without them,
         and leaving it empty ships a node that cannot work. Its docstring has the full
         param-type → source table; use it rather than leaving a selection param blank.

    Then say in chat which values you took from where. A param you set from real project state and a
    param you left at its default are very different things to the person pressing Run.

    CREATE-ONLY: 409 if `name` exists — it can never overwrite a chain the user wired. To offer an
    alternative to an existing chain, create a NEW one named for what it does (not "-v2"), tell them
    it sits **beside** the original, and let them compare the two on the canvas and delete the loser.
    You cannot rename or delete a chain; both are the user's, in the GUI.

    The server validates the shape (unknown fn, dangling edge, cycle, out-of-range param → 400 naming
    the offender — fix and retry). It CANNOT validate intent: nothing here checks that you wired
    tracking after a segmentation that exists, and selection params (`valueName`, population pickers)
    name project state the spec doesn't list. So the user reading the graph before Run is doing real
    work — write the chain to be read, and flag in chat anything you had to guess."""
    return _client.create_chain(project_uid, name, nodes, edges, start_targets)


@mcp.tool()
def mark_tracks(project_uid: str, image_uid: str, value_name: str, track_ids: list[int],
                focus_id: int | None = None, label: str = "", ttl_s: int = 300) -> dict:
    """Highlight a set of TRACKS on the user's viewer — your "look at THESE tracks" pointer.

    Reuses the same setter the TrackSchemeView "Show" button drives (`setTrackHighlight`): the
    listed track ids are outlined by the WebGPU shader, and `focus_id` (optional) is drawn
    distinct so the user's eye lands on the one that matters. `label` (optional) rides along in
    the mark envelope so the correction cockpit + gating plots can label the pointer.

    EPHEMERAL: `ttl_s` defaults to 5 min (Decision 18 of `docs/todo/BIDIR_CONTEXT_PLAN.md`).
    Nothing about a mark is persisted; a process restart on the backend clears them all. This is
    for "point at what I just said", not for saving a state — use `add_analysis_board` /
    `create_notebook` if the finding is worth keeping.

    Scope is per (image_uid, value_name) because track ids are per-vn — a mark on `flowTom`'s
    track 42 must not narrow `default`'s track 42, which is a different cell. Pick the
    value_name that carried the tracks (see `get_analysis_lineage` if you're not sure).

    Returns `{ok: true, markerId}`. The id is short-lived — no follow-up read is needed; the
    frontend paints on receipt.
    """
    return _client.mark_tracks(project_uid, image_uid, value_name, track_ids, focus_id, label, ttl_s)


@mcp.tool()
def mark_cells(project_uid: str, image_uid: str, value_name: str, label_ids: list[int],
               focus_id: int | None = None, label: str = "", ttl_s: int = 300) -> dict:
    """Outline a set of CELLS on the user's viewer — your "look at THESE cells" pointer.

    `label_ids` are cell/label ids from the segmentation `value_name`. Reuses the correction
    cockpit's `setPickHighlight` setter; `focus_id` (optional) is the one distinct cell within
    the outlined set. Same EPHEMERAL contract as `mark_tracks` — 5-min default TTL, in-memory
    only.

    When to pick this vs `mark_tracks`: if you're pointing at OBJECTS AT A TIMEPOINT (segmented
    cells the user is looking at in the viewer's mask), use this. If you're pointing at whole
    TRAJECTORIES over time, use `mark_tracks`. A tracked cell has both; usually you want tracks.

    Scope is per (image_uid, value_name). Returns `{ok: true, markerId}`.
    """
    return _client.mark_cells(project_uid, image_uid, value_name, label_ids, focus_id, label, ttl_s)


@mcp.tool()
def point_at_ui(project_uid: str, anchor: str, label: str = "", ttl_s: int = 300) -> dict:
    """Point at a UI CONTROL on the user's screen — your "click here" pointer.

    `anchor` is a `data-guide` id (`"viewer.movieSection"`, `"nav.settings"`) or a route in
    `nav:/<path>` form (`"nav:/segment"`). The frontend resolves it via
    `utils/guideAnchor.ts::resolveAnchor` and paints a small pointer beside the element — same
    resolution scheme the onboarding guides already use. If the anchor doesn't resolve (element
    off-screen, panel collapsed) the pointer waits until it does.

    EPHEMERAL: 5-min default TTL (Decision 18 of `docs/todo/BIDIR_CONTEXT_PLAN.md`).

    When to use this vs `mark_tracks` / `mark_cells`: those point at DATA (specific cells or
    trajectories); this points at CONTROLS ("click this button", "look at that section"). Prefer
    naming a section (`viewer.movieSection`) over a single button — the anchor may shift as the
    app changes, but sections are stable.

    Returns `{ok: true, markerId}`. No follow-up needed — the frontend paints on receipt.
    """
    return _client.mark_ui(project_uid, anchor, label, ttl_s)


@mcp.tool()
def mark_freeform(project_uid: str, capture_id: str, overlay: list,
                  label: str = "", ttl_s: int = 300) -> dict:
    """Draw a FREEFORM overlay on a stored CAPTURE — your "look right HERE" pointer.

    `capture_id` is a `cap-…` id from `get_recent_captures`. The frontend renders the marks ON
    the frozen shared frame in the pop-out viewer (which stays visible after Save), so the user
    sees exactly the same frame you're pointing at.

    `overlay` is a list of `{kind: "rect"|"poly"|"stroke"|"circle"|"arrow", geom, label?}`.
    Coordinates are 0..1 in the FRAME's own space — the same coord system `get_capture` returns
    the user's overlay in, so a "point-at-what-you-shared" round-trip is exact. `geom` per kind:
    rect = `{x, y, w, h}`, poly / stroke = `{pts: [[x, y], …]}`, circle = `{cx, cy, r}`,
    arrow = `{x1, y1, x2, y2}`. Keep values in [0, 1].

    EPHEMERAL: 5-min default TTL. When to use this vs `mark_tracks` / `mark_cells`: those point at
    identified objects. Use `mark_freeform` when there's no id — you saw something in a capture
    that the segmentation didn't pick up, or you want to circle a REGION rather than a specific
    cell. If the user hasn't shared a frame yet, ask them to (Share button in the viewer panel)
    rather than making up a captureId.

    Returns `{ok: true, markerId}`.
    """
    return _client.mark_freeform(project_uid, capture_id, overlay, label, ttl_s)


@mcp.tool()
def mark_tile(project_uid: str, image_uid: str, cell_id: str,
              label: str = "", ttl_s: int = 300) -> dict:
    """Highlight ONE landscape/grid TILE on the viewer — your "look at THIS region" pointer when
    there is no segmented object to name.

    `cell_id` is a spreadsheet-style grid label ("B3", "H8") — the same ids `get_landscape`
    returns and the same ids the SoM grid on the viewer paints. Use this after `get_landscape`
    tells you a tile is worth attention (e.g. its category is `bright-textured` in a region where
    the user hasn't noticed yet), or when you want to point at a REGION rather than an identified
    cell / track. Prefer `mark_cells` / `mark_tracks` when a segmentation id exists — a tile
    highlight is a coarser pointer, and using it when a specific cell is meant is misleading.

    EPHEMERAL: 5-min default TTL. Returns `{ok: true, markerId}`.
    """
    return _client.mark_tile(project_uid, image_uid, cell_id, label, ttl_s)


@mcp.tool()
def get_landscape(project_uid: str, image_uid: str, value_name: str,
                  t: int = -1, z: int = -1) -> dict:
    """The user's current LANDSCAPE HEATMAP — a cheap categorical map over the viewer's grid tiles.

    Returned as `{grid: {cols, rows}, tiles: [{id, row, col, category, stats}], legend: [...]}`.
    `category` is one of `dark` / `bright-uniform` / `bright-textured` / `edge` / `mixed`.
    Use this BEFORE reading raw pixels from a capture so you have a rough semantic prior — "row 2
    is dominated by `bright-textured` tiles" beats "I looked at the RGB and guessed". Then
    `mark_tile(cell_id="B3")` when you want to point back at a region the map surfaces.

    This is NOT a segmentation — it's tile-level (typically 8×8 = 64 tiles), computed on the
    current shown frame's pixels in the browser. Coarse on purpose. When a segmentation exists,
    reach for `mark_cells` / `mark_tracks` instead; this tool is the "no segmentation yet" case.

    The landscape only exists if the user has toggled the "Landscape" overlay on in the viewer
    panel. Returns `{landscape: null}` if not — say so and ask the user to toggle it if you need
    the map; do not invent tiles.
    """
    return _client.get_landscape(project_uid, image_uid, value_name, t, z)


@mcp.tool()
def get_recent_captures(project_uid: str, limit: int = 10) -> list:
    """What the user has SHARED with you from their viewer — the "look at this" surface (BIDIR).

    A capture is a frozen viewer frame + a drawing overlay the user marked on it, addressed by
    project / image / t / z / extent so you never have to ask "which image". Returned NEWEST-FIRST
    as `[{captureId, createdAt, surface, address}, …]` — metadata only, no pixels; call
    get_capture(captureId) for the frame + overlay themselves.

    Call this when the user says "look at this" / "I just shared something" / when your last
    message asked them to point at something on the viewer, or when you notice they've gone quiet
    after being asked to. Empty list ⇒ nothing new; don't invent a capture.

    `address` is what disambiguates the frame: `{projectUid, imageUid?, valueName?, t?, z?,
    extentUm?}`. Use it exactly as authored — don't re-ask for an image uid a capture already
    carries. `surface` is `"viewer_frame"` for now; slabs (video), UI and plot captures will
    appear as `"viewer_slab"` / `"ui"` / `"plot"` in later PRs of the same feature.
    """
    return _client.get_recent_captures(project_uid, limit).get("items", [])


@mcp.tool()
def get_capture(project_uid: str, capture_id: str) -> list:
    """The full envelope of ONE capture — the pixels the user shared PLUS what they drew on top.

    JSON envelope with the address, overlay marks, any view-state snapshot, an optional
    `landscape` field (tile-level semantic map when the overlay was on at share time), and a
    `notes` field (free-text context the user typed alongside the frame).

    READ ALL FOUR. `notes` is the user's own words about the capture — "look at the T-cell
    channel here, segmentation looks under-called". It is the PRIORITY signal — if the user
    took the trouble to type it, address what they said before commenting on anything you
    noticed independently. Empty string when nothing was typed.

    The image tells you what they're looking at, the overlay tells you WHERE they're pointing,
    the address tells you which image / t / z it is, and the landscape gives you a rough semantic
    prior over the tiles before you squint at raw RGB. `landscape` is absent (or null) when the
    overlay was off.

    Landscape schema is versioned:
      - `schemaVersion: 1` — category-only. `tiles[i] = {id, row, col, category, stats}`.
      - `schemaVersion: 2` — augmented with backend-only per-tile fields, ALL sparse by
        visibility (each appears only if the matching layer was on at share time):
          • `tiles[i].channels = {<channelName>: {mean, snr}}` — per-channel mean + SNR from
            the raw multi-channel plane, untangling the colour-blend. A yellow tile might read
            "high mean on Tcells, low mean on SHG" even when its category is `bright-textured`.
          • `tiles[i].segCount` — how many segmented objects have a centroid inside this tile
            at the shown t. The "5 vs 12 cells" answer a downsampled composite can't give.
          • `tiles[i].pops = [{path, name, count}]` — which populations occupy this tile at
            the shown t, one entry per pop with count > 0. `path` is the pop manager's
            canonical id ("/live/tnaive"); use it when calling `mark_cells` back on member
            cells. Populations with 0 count in a tile are ABSENT — infer nothing from a
            missing entry beyond "not visible here or not toggled on".
          • `tiles[i].tracks = {count, meanDuration?, meanSpeed?}` — per-tile tracks summary.
            `count` = distinct tracks with a cell in this tile at t; `meanDuration` = mean
            full-lifetime frame count across those tracks; `meanSpeed` = mean instantaneous
            per-cell speed here at t (absent when the segmentation has no `live.cell.speed`).
            The "is anything moving in this region" answer the static composite can't give.

    Prefer these fields over guessing from the composite when v2 is available: `channels` for
    "which channel dominates this bright tile", `segCount` for "how densely populated is this
    region", `pops` for "which cell type sits here", `tracks` for "is there motion here".
    A missing field on a v2 tile means the corresponding layer was off — fall back to your
    visual read, don't infer zero.

    A v2 landscape may also carry `sourceRun` at the landscape (not tile) level — a per-field
    bag naming the run/vn/version that produced each augmented field:
      • `sourceRun.segCount = {valueName, labelsVersion}` — the label_props vn + resolved vN
      • `sourceRun.pops     = {valueName, popType, gatingMtime}` — gating file's on-disk mtime
      • `sourceRun.tracks   = {valueName, labelsVersion}`
      • `sourceRun.channels = {valueName, imageVersion, level}` — pyramid level actually read
    Use these to answer "which run produced this number", or to compare two capture envelopes
    and see whether the underlying gating map / re-tracked h5ad moved between shares.

    A v2 landscape also carries `viewport` describing how the compute reduced Z, so a tile's
    counts and channel stats read consistently with the viewer that produced them:
      • `viewport.renderMode = 'plane'` — viewer was showing a single Z slice; centroids were
         filtered to `zLo ≤ round(centroid_z) ≤ zHi` (typically ±1 around the shown plane) and
         channels are the single-plane means. So `segCount: 8` means "8 objects in this tile,
         on or immediately adjacent to the visible slice", NOT "8 through the whole stack".
      • `viewport.renderMode = 'volume'` — viewer was showing a MIP; centroids were filtered to
         the slab bounds the viewer's Z slider had set (`zLo..zHi`), and channels are the
         per-pixel MIP across that slab. `segCount: 8` means "8 objects in this XY tile,
         summed across the MIP'd Z range".
    For a 2D image, renderMode is always 'plane' and the filter is a no-op. When comparing
    tile counts across two captures, check `viewport` first — same tile ID at same t under
    different renderMode / zLo / zHi legitimately produces different numbers.

    `capture_id` is what get_recent_captures returns as `captureId`. 404 if it doesn't exist (a
    hallucinated id, a project the user has since deleted, or a capture from a different install
    — the storage is per-project, not per-user).

    Overlay `kind` is one of `"rect" | "poly" | "stroke" | "circle" | "arrow"`; `geom` is
    payload-relative (0..1 in the frame's own coord space) — the caller (frontend) authored
    them, and their exact rendering is not this tool's concern.

    Multi-panel plot captures (surface `"plot"` from the module-page canvas Share) also carry a
    `panels: [{panelId, position:{x,y,w,h}, plotRef, dataSlice}]` field on the envelope. `position`
    is in the composite PNG's own CSS-px frame (top-left origin), so a mark in the overlay can be
    matched to the panel it sits over. `plotRef` names the plot spec + its ui state (measure,
    chart type…) at capture time; `dataSlice` names the images / segmentations / series involved.
    Use these to answer "which panel is which" rather than guessing from the image alone.

    LANDSCAPE COMPRESSION FOR MCP DELIVERY. A v2 landscape's tile bag is compressed before it
    leaves this tool, so the whole envelope fits under Claude Code's tool-result token cap. The
    stored envelope on disk keeps the fat form (Kiwi + frontend read that); the shape you see:
      • `landscape.channelNames: [str, …]` — union of channel names across tiles, first-seen order.
      • `landscape.popMap: {"<key>": {path, name}}` — union of visible-pop (path, name) tuples.
      • Per-tile `channels: [mean, snr, mean, snr, …]` positional to `channelNames`
        (channel `i`'s stats live at indices `2i, 2i+1`).
      • Per-tile `pops: [[popKey, count], …]` — `popKey` is the string index into `popMap`.
      • Dropped from every tile: `row`, `col` (derivable from `id` via the cellLabel A1/B2 grid),
        `stats` (frontend-computed leftovers superseded by `channels` + `category` for augmented
        tiles).
      • Everything else (`id`, `category`, `segCount`, `tracks`) is verbatim.
    If a landscape is still too big to fit — a 64×64 grid with dense pops on a heavy image —
    use `get_capture_landscape_tiles(project_uid, capture_id, tile_ids=[...])` or `bbox=[x1,y1,x2,y2]`
    to fetch just the tiles under a marked region, still in the same slim shape.

    ESCAPE HATCH — `capturePath`. When Cecelia is loopback-bound (the local dev default), the
    envelope block also carries `capturePath`: the absolute filesystem path to the FAT
    `meta.json` on disk. Reach for `Read(capturePath)` when you genuinely need what the slim
    transform dropped (the raw per-tile `stats`, or the fat dict-of-dict channels shape) —
    for example when reasoning about the frontend's category clustering itself.

    The field is ABSENT (not just unreachable) when Cecelia is network-exposed
    (`CECELIA_HOST=0.0.0.0`, cloud-VM, remote workstation) — a remote caller can't reach the
    path anyway, so shipping one would be a foot-gun. When it's absent you already have
    everything you're going to get from `get_capture`; if the slim form isn't enough, use
    `get_capture_landscape_tiles` for on-demand tile subsets. Don't use `capturePath` as the
    DEFAULT path — the slim form is cheaper and sufficient for every reader task except the
    frontend-audit case just named.
    """
    envelope = _client.get_capture(project_uid, capture_id)
    frame_url = envelope.get("frame") or ""
    frame_b64 = frame_url.split(",", 1)[1] if frame_url.startswith("data:") else ""
    blocks: list = []
    if frame_b64:
        try:
            blocks.append(Image(data=base64.b64decode(frame_b64), format="png"))
        except Exception:  # noqa: BLE001 — a corrupt frame must not sink the envelope; keep going
            pass
    # Compress the capture envelope's landscape tile bag before returning — a 32×32
    # augmented v2 landscape serialises to ~640 KB, well past Claude Code's tool-result
    # token cap, and the peer session saw the tail sliced. The slim transform keeps every
    # field a reader actually needs (see landscape_slim.slim_landscape_for_mcp for the
    # shape). Fat form still lives on disk and in Kiwi.
    capture = envelope.get("capture", {})
    slim = slim_landscape_for_mcp(capture) if isinstance(capture, dict) else capture
    # Escape hatch: attach the absolute path to the FAT meta.json when Cecelia sent one, so a
    # LOCAL Claude session (Cecelia + Claude Code on the same machine) can `Read(capturePath)`
    # to recover what the slim transform dropped. Absent on cloud-VM deployments — a reader
    # who tries `Read` on a path from a remote host gets a not-found error and falls back to
    # the slim payload already returned. See the tool docstring's ESCAPE HATCH note.
    if isinstance(slim, dict):
        path = envelope.get("capturePath")
        if isinstance(path, str) and path:
            slim = dict(slim)
            slim["capturePath"] = path
    blocks.append(slim)
    return blocks


@mcp.tool()
def get_capture_landscape_tiles(project_uid: str, capture_id: str,
                                tile_ids: list[str] | None = None,
                                bbox: list[float] | None = None) -> dict:
    """Fetch a SUBSET of a capture's landscape tiles — drill-down when `get_capture` is truncated.

    A dense landscape (32×32 with many populations, or 64×64 anywhere) can still overflow
    Claude Code's tool-result token cap even after the compression `get_capture` applies. This
    tool returns just the tiles you name, in the same slim shape `get_capture` returns for the
    landscape section — see that tool's LANDSCAPE COMPRESSION FOR MCP DELIVERY note.

    Pick one:
      • `tile_ids=["A1", "B2", ...]` — spreadsheet-style ids from a previous
        `get_capture`. Use when you already know which tiles matter.
      • `bbox=[x1, y1, x2, y2]` — frame-relative coords in 0..1 (same coord system the overlay
        marks use). Use when you have a mark from `mark_freeform` or a `stroke` in the
        capture's overlay and want the tiles UNDER that mark. The tool clamps to the grid and
        includes any tile the rectangle intersects.
    Both absent ⇒ returns the entire landscape (equivalent to `get_capture`'s landscape section).
    Both present ⇒ `tile_ids` wins (explicit list beats bbox).

    Returns a dict `{captureId, landscape: {grid, legend, channelNames?, popMap?, tiles: [...]}}`
    — just the landscape section, filtered. The other envelope fields (address, viewStateSnapshot,
    overlay, sourceRun, viewport) are NOT re-shipped here — call `get_capture` once for those,
    then use this tool for per-region tile detail. `viewport` still governs what the counts mean;
    the reduction rules on `get_capture`'s note apply to these tiles too.

    404 when the capture doesn't exist. Empty `tiles: []` when the filter matches nothing.
    """
    envelope = _client.get_capture(project_uid, capture_id)
    capture = envelope.get("capture") if isinstance(envelope, dict) else None
    if not isinstance(capture, dict):
        return {"captureId": capture_id, "landscape": {"tiles": []}}
    slim = filter_landscape_tiles(capture, tile_ids=tile_ids, bbox=bbox)
    return {"captureId": capture_id, "landscape": slim.get("landscape") or {"tiles": []}}


@mcp.tool()
def register_push_target(project_uid: str, session_label: str = "") -> dict:
    """Pair THIS Claude Code session with a Cecelia project for cross-session push (BIDIR Part 5).

    You RARELY need to call this by hand — every other tool with a `project_uid` auto-pairs on
    its first call per session, so a fresh session pairs the moment you check the project.
    Call this explicitly when: (a) the user asks you to re-pair after a session restart on
    Cecelia's side, or (b) they've spun up a new Claude session and want push delivery for a
    project you haven't touched yet.

    Reads `CLAUDE_CODE_MESSAGING_SOCKET` + `CLAUDE_CODE_MESSAGING_TOKEN` (exported by Claude
    Code v2.1.224+) from this process's env — no arguments beyond `project_uid` are needed. If
    those env vars are missing, this session isn't reachable for push and the tool errors; the
    frontend's clipboard/toast fallback stays active as designed. `session_label` (optional)
    labels the pairing in the frontend's "paired" chip; defaults to the first 8 chars of the
    session id.

    Once paired, PR #2's Julia writer (when it ships) can push a plain-text capture-arrived
    notification directly to this session over its inbox socket, so you learn about a shared
    frame without the user leaving the viewer. This is a NOTIFICATION channel only — pairing
    doesn't grant any new code or execution access.

    Returns `{ok: true}` on success.
    """
    return _client.register_push_target(project_uid, session_label)


@mcp.tool()
def get_object_ids(project_uid: str, image_uid: str, value_name: str,
                   kind: str = "cells", limit: int = 200, sample: bool = False) -> dict:
    """Real cell / track ids for a segmentation — call this BEFORE mark_cells / mark_tracks.

    The point-out tools take literal ids (`labels=[…]` for cells, `trackIds=[…]` for tracks).
    Without this tool you'd be guessing — ids are per-vn and start at whatever the segmentation
    banked, not necessarily 1. A mark on an id that doesn't exist renders as nothing.

    `kind`: `"cells"` (per-cell label ids from `label_props`) or `"tracks"` (per-track ids from
    `track_props`, one row per track). A segmentation with no tracks returns `ids=[]`, `total=0`.

    Payload is capped at `limit` (default 200, max 5000) so a 50-000-cell segmentation doesn't
    ship half a MB per call. Response reports `total` + `truncated` so you know when you're
    seeing a slice. Set `sample=True` to get a stride-uniform sample across the WHOLE population
    (deterministic — same call, same sample) — useful when you want coverage rather than "the
    first 200 which are all in one corner".

    Returns `{kind, valueName, ids: [Int, …], total, truncated, sampled}`.
    """
    return _client.get_object_ids(project_uid, image_uid, value_name, kind, limit, sample)


@mcp.tool()
def get_recent_logs(level: str = "", source: str = "", limit: int = 100) -> list:
    """Recent lines from the app's console — everything the backend SIDE says, newest last.

    This is where a **Julia-side task crash lands** (e.g. a task that dies before its Python
    subprocess starts) — it does NOT appear in `get_task_log`, which only captures the Python
    process's stdout. When `poll_observations` shows a `repeat_attempts` / a task keeps failing but
    the task log looks empty, call this to find the actual error.

    Each record is `{seq, ts, level, source, message, detail?}`. Two fields are worth using:

    - **`detail` carries the formatted stacktrace** for anything logged with an exception. The
      `message` is the one-line summary; if you are diagnosing, read `detail`.
    - **`source` says which process spoke** — `backend` (the Julia server), `napari` (viewer bridge
      :7655), `preview` (task-preview worker :7656), `runner` (detached task runner :7657),
      `notebooks` (Pluto :7660). A Python traceback from any of the children arrives as ONE record
      with the frames in `detail`. Pass `source` to filter to one of them.

    `level` optionally filters to "info" / "warn" / "error" (default: all). `limit` caps how many of
    the most-recent lines are returned. It's a process-wide ring buffer (~500 records, not persisted,
    not per-project), so it's for *live/recent* diagnosis, not historical forensics.
    """
    logs = _client.get_recent_logs().get("logs", [])
    if level:
        logs = [l for l in logs if str(l.get("level", "")).lower() == level.lower()]
    if source:
        logs = [l for l in logs if str(l.get("source", "")).lower() == source.lower()]
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
