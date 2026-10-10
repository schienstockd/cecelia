"""The results of each pipeline stage a run reached, as Analysis-board plots, for its record.

The record's sections show what the agent DID and the pictures it looked at; neither shows what came
out — a segmentation's QC, the tracks, the HMM states, the clusters. So after the run, for every stage
that RAN (read from the decisions — the chain and task runs — not from whether the agent chose to make
a board), this adds one board to the COPY project through the same route Claude uses
(`POST /api/boards/add`, `app/src/analysis_board_spec.jl`), renders it with the app's own frontend
(`board_render.py`) and hands each plot back as a picture for that stage's section.

Failure-tolerant by design: a stage whose board cannot be added or rendered gets a note saying what
failed, and the record is written regardless. Boards are named after the run and the section, so a
re-run of the record re-renders the same board (409 = it exists) rather than adding another.
"""
from __future__ import annotations

import json
import urllib.error
import urllib.parse

import app_project
import board_render
from board_render import auth_headers, open_url   # the token + http-or-https (cecelia_mcp.auth / .loopback)

# which stage a decision's function belongs to (None = no board)
_STAGE = {"segment": "segment", "tracking": "track", "behaviour.hmm": "hmm",
          "clustTracks": "trackclust", "clustPops": "clust"}


# cluster views that draw one thing per cluster POPULATION (the user ticks clusters into pops on the
# cluster page); with none defined they render "tick one or more track clusters", which is no picture
CLUSTER_POP_VIEWS = ("hmmStates", "hmmTransitions", "cellCards")


def output_value_name(params: dict) -> str:
    """The value_name a task WROTE: `outputValueName` when it has one (a segmentation reads the image
    version `valueName` and writes the labels under `outputValueName`), else `valueName` (a task that
    writes where it reads — tracking, drift correction)."""
    return (params or {}).get("outputValueName") or (params or {}).get("valueName") or ""


def stage_of(fn: str) -> str | None:
    if fn == "segment.measureLabels":
        return None
    for prefix, stage in _STAGE.items():
        if fn == prefix or fn.startswith(prefix + ".") or fn.startswith(prefix + "_"):
            return stage
    return None


def _pops_tracked(vn: str, pop: str) -> list[list[str]]:
    """Candidate populations for a tracked pop, most specific first (`/_tracked` only exists where it
    tells the picker something — the expander says which)."""
    base = f"{vn}/{pop.strip('/')}" if pop.strip("/") else f"{vn}/root"
    return [[f"{base}/_tracked"], [base]]


def _plots(stage: str, units: list[dict]) -> list[tuple[str, list[list[dict]]]]:
    """[(label, candidate plot lists)] — the expander picks the first candidate the project accepts."""
    p = (units[0].get("params") or {}) if units else {}
    if stage == "segment":
        segs = [output_value_name(u.get("params") or {}) or u.get("target") for u in units]
        pops = [f"{vn}/labels" for vn in dict.fromkeys(segs) if vn]
        return [("Segmentation QC", [[
            # a count chart's default is the raw count (`defaultNormalize`, plots/plot.ts) — cells per frame
            {"plot": "segmentation_qc", "chart": "count", "groupBy": "centroid_t", "pops": pops,
             "title": "Cells per frame"},
            {"plot": "segmentation_qc", "chart": "boxplot", "measure": "area", "pops": pops, "title": "Cell area"}]])]
    if stage == "track":
        vn, pop = p.get("valueName") or "default", p.get("popsToTrack") or "root"
        return [("Tracks", [[
            {"plot": "track_measures", "chart": "boxplot", "measure": "live.track.speed", "pops": c},
            {"plot": "track_measures", "chart": "boxplot", "measure": "live.track.straightness", "pops": c},
            {"plot": "trackPaths", "pops": c}] for c in _pops_tracked(vn, pop)])]
    if stage == "hmm":
        col = p.get("colName") or "default"
        raw = (p.get("pops") or ["default/root"])[0]
        vn, _, pop = raw.partition("/")
        return [("HMM", [[
            {"plot": "hmm_state_frequency", "chart": "frequency", "measure": f"live.cell.hmm.state.{col}", "pops": c},
            {"plot": "state_signature", "pops": c},
            {"plot": "transition_matrix", "measure": f"live.cell.hmm.transitions.{col}", "pops": c},
            {"plot": "hmmStateCards", "valueName": vn, "hmmCol": f"live.cell.hmm.state.{col}"}]
            for c in _pops_tracked(vn, pop)])]
    if stage in ("trackclust", "clust"):
        run = {"suffix": p.get("valueNameSuffix") or "default", "popType": stage}
        plots = [{"plot": "umap", **run}, {"plot": "heatmap", **run}]
        if stage == "trackclust":     # need cluster populations — dropped by `_needs_cluster_pops` when none
            plots += [{"plot": k, **run} for k in CLUSTER_POP_VIEWS]
        return [("Clusters", [plots])]
    return []


def board_specs(dec: dict, stamp: str, copy_images: list[dict]) -> list[dict]:
    """One board per section whose decision ran a stage with results to show, plus ONE gating board
    (a gating-strategy slot per gated image, each attached to that image's last gate section)."""
    out = []
    for s in dec["sections"]:
        if s["step"] == "gate" or not s["units"]:
            continue
        stage = stage_of(s["fn"])
        for label, candidates in _plots(stage, s["units"]) if stage else []:
            out.append({"name": f"Run {stamp} · {s['id']} · {label}", "label": label, "compareBy": "per_image",
                        "candidates": candidates, "attach": [s["id"]] * len(candidates[0])})
    copy_of = {im["sourceImageUid"]: im["imageUid"] for im in copy_images}
    last_gate = {}
    for s in dec["sections"]:
        if s["step"] == "gate" and s["images"] and s["units"][-1].get("target"):
            last_gate[s["images"][0]] = (s["id"], s["units"][-1]["target"])
    if last_gate:
        plots = [{"plot": "gatingStrategy", "image": copy_of.get(img, img), "valueName": vn, "popType": "live",
                  "hierarchy": True, "title": img} for img, (_, vn) in last_gate.items()]
        out.append({"name": f"Run {stamp} · gating", "label": "Gating strategy", "candidates": [plots],
                    "attach": [sid for sid, _ in last_gate.values()]})
    return out


def _cluster_pops(api: str, project_uid: str, image_uid: str, value_name: str, pop_type: str) -> int:
    q = urllib.parse.urlencode({"projectUid": project_uid, "imageUid": image_uid, "valueName": value_name,
                                "popType": pop_type})
    try:
        with open_url(api.rstrip("/"), f"/api/gating/popmap?{q}", headers=auth_headers(), timeout=60) as r:
            return len(((json.loads(r.read().decode("utf-8")) or {}).get("tree") or {}).get("populations") or [])
    except (urllib.error.URLError, OSError, ValueError):
        return -1                      # unknown: keep the views, the render will say what it shows


def _needs_cluster_pops(api: str, project_uid: str, specs: list[dict], dec: dict, copy_images: list[dict],
                        note) -> None:
    """Drop the per-cluster-population views from a clustering board whose run has no populations."""
    params = {s["id"]: (s["units"][0].get("params") or {}) for s in dec["sections"] if s["units"]}
    img = copy_images[0]["imageUid"] if copy_images else ""
    for spec in specs:
        plots = spec["candidates"][0]
        if not any(p["plot"] in CLUSTER_POP_VIEWS for p in plots):
            continue
        vn = ((params.get(spec["attach"][0], {}).get("popsToCluster") or ["default/"])[0]).split("/")[0]
        if _cluster_pops(api, project_uid, img, vn, plots[0]["popType"]) != 0:
            continue
        keep = [i for i, p in enumerate(plots) if p["plot"] not in CLUSTER_POP_VIEWS]
        spec["candidates"] = [[c[i] for i in keep] for c in spec["candidates"]]
        spec["attach"] = [spec["attach"][i] for i in keep]
        note(spec["attach"][:1], "no HMM-per-cluster or cell cards — the run defined no cluster populations")


def _add_board(api: str, project_uid: str, spec: dict) -> str | None:
    """Add the board (first candidate the expander accepts). None on success or when it already exists;
    else the reason."""
    err = "no plots"
    for plots in spec["candidates"]:
        body = {"projectUid": project_uid, "name": spec["name"], "plots": plots,
                "compareBy": spec.get("compareBy", "")}
        try:
            with open_url(api.rstrip("/"), "/api/boards/add", data=json.dumps(body).encode("utf-8"),
                          headers={"Content-Type": "application/json", **auth_headers()}, method="POST",
                          timeout=120):
                return None
        except urllib.error.HTTPError as e:
            msg = (json.loads(e.read().decode("utf-8") or "{}") or {}).get("error", f"HTTP {e.code}")
            if e.code == 409:
                return None                     # already added by an earlier record of this run
            err = msg
            if e.code != 422:
                break
        except (urllib.error.URLError, OSError) as e:
            return f"the app did not answer ({e})"
    return f"board not added: {err}"


def stage_pictures(api: str, run: dict, dec: dict, stamp: str) -> tuple[dict, dict]:
    """({sectionId: [(png base64, caption, address)]}, {sectionId: [note]}) for the run's stages."""
    pics: dict[str, list] = {}
    notes: dict[str, list[str]] = {}
    specs = board_specs(dec, stamp, app_project.copy_images(run))
    if not specs:
        return pics, notes
    note = lambda sids, text: [notes.setdefault(sid, []).append(text) for sid in dict.fromkeys(sids)]  # noqa: E731
    _needs_cluster_pops(api, run["projectUid"], specs, dec, app_project.copy_images(run), note)
    ready = []
    for spec in specs:
        why = _add_board(api, run["projectUid"], spec)
        if why:
            note(spec["attach"], f"{spec['label']} — {why}")
        else:
            ready.append(spec)
    if not ready:
        return pics, notes
    try:
        got = board_render.render_boards(api, run["projectUid"], [s["name"] for s in ready])
    except (board_render.RenderError, OSError) as e:
        for spec in ready:
            note(spec["attach"], f"{spec['label']} — not rendered: {e}")
        return pics, notes
    for spec in ready:
        b = got["boards"].get(spec["name"]) or {}
        if not b.get("ok"):
            note(spec["attach"], f"{spec['label']} — not rendered: {b.get('error', 'no result')}")
            continue
        for slot in b["slots"]:
            sid = spec["attach"][slot["index"]] if slot["index"] < len(spec["attach"]) else spec["attach"][-1]
            what = slot["name"] + (f" ({slot['title']})" if slot.get("title") else "")
            if not slot.get("png"):
                note([sid], f"{spec['label']} — {what}: the plot exported no image")
                continue
            pics.setdefault(sid, []).append((
                slot["png"], f"{what} — Analysis board “{spec['name']}” in the copy, rendered after the run",
                {"board": spec["name"], "slot": slot["index"]}))
    if got["blocked"]:      # the page tried to write — refused, but the frontend should not be trying
        notes.setdefault("", []).append("the render session refused writes: " + ", ".join(got["blocked"]))
    return pics, notes
