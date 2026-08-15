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

import os

from mcp.server.fastmcp import FastMCP

from cecelia_mcp.client import CeceliaClient
from cecelia_mcp.guidance import BRIEFING_GUIDANCE, SERVER_INSTRUCTIONS
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
    outcome, `"done"` or `"failed"` — and **`params`**, the params that run used (the tuning trail; `{}`
    on older runs). Pair `params` with get_module_params + get_cohort_qc to suggest a param adjustment
    on an outlier: what was tried, the valid range, the direction to try. It is "what was tried", not a
    params→outcome relationship — suggest, don't predict.

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
      - `recentLabLog`: entries from the last 7 days, newest-first — `[{date, author, summary}]`

      - `guidance`: HOW TO WORK WITH THIS PROJECT — the disciplines that span tools (what to check
        before proposing any figure or cross-image comparison, and the rules for the few things you can
        write). Read it before you propose anything; it is written to be followed, not summarised.

    Use it to open with what matters ("3 of 12 images flagged; 2 have too few tracks") and to pick up
    where the last session left off (the lab log). Then ask the user which direction to take. Read-only."""
    # The guidance rides along with the briefing rather than sitting in the server instructions: it is
    # ~600 words that only matter once a session actually opens a project, and the observer is
    # registered user-scope, so in the instructions it would be in context for every unrelated `claude`
    # session on the machine. Server-side, not pasted by the user — that is the whole point (see
    # guidance.py). Merged into the response so one call orients AND briefs.
    return {**_client.get_session_briefing(project_uid), "guidance": BRIEFING_GUIDANCE}


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
