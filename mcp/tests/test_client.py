"""Unit tests for the read-only Cecelia API client — the observer's no-mutation guarantee.

Pure stdlib (unittest + mock); urllib is mocked so no server is needed. These pin the allow-list,
URL/param building, body shaping, and error mapping — the logic that keeps the observer read-only.
"""
import io
import json
import unittest
import urllib.error
from unittest import mock

from cecelia_mcp.client import ALLOWED_ROUTES, ApiError, CeceliaClient, DisallowedRoute


class _FakeResp:
    def __init__(self, payload):
        self._data = json.dumps(payload).encode()

    def read(self):
        return self._data

    def __enter__(self):
        return self

    def __exit__(self, *a):
        return False


def _patch_urlopen(payload=None):
    return mock.patch(
        "cecelia_mcp.client.urllib.request.urlopen",
        return_value=_FakeResp({} if payload is None else payload),
    )


class ClientTest(unittest.TestCase):
    def setUp(self):
        self.c = CeceliaClient(base_url="http://x:8080")

    def test_allowlist_blocks_unknown_routes(self):
        # a mutating route that exists in the API must NOT be reachable through the client
        with self.assertRaises(DisallowedRoute):
            self.c._request("POST", "/api/images/delete", body={"x": 1})
        with self.assertRaises(DisallowedRoute):
            self.c._request("GET", "/api/gating/save")

    def test_writes_are_only_the_recoverable_routes(self):
        # The no-mutation guarantee: the only non-GET routes are lab-log append (append-only),
        # notebook write (create-only), notebook describe (description text only), notebook revise
        # (snapshots first, so it's recoverable), chain create (create-only + validated — and a
        # template is inert until a human presses Run), the LabArchives context set (REPLACES a
        # sidecar that is a cache of an external, versioned system of record, so the rewrite loses
        # nothing), and the point-out mark writes (BIDIR PR #4 — EPHEMERAL, in-memory only, 5-min
        # default TTL). None can edit/delete cell data, images, gates, or QC.
        #
        # Changing this list is the GATE on widening what Claude can do to a project. If you are
        # here to add a route, the question to answer first is whether it can destroy something.
        writes = sorted((m, p) for (m, p) in ALLOWED_ROUTES if m != "GET")
        self.assertEqual(writes, [
            ("POST", "/api/blackboard/create"),
            ("POST", "/api/blackboard/outcome"),
            ("POST", "/api/blackboard/revise"),
            ("POST", "/api/blackboard/search"),
            ("POST", "/api/blackboard/status"),
            ("POST", "/api/boards/add"),
            ("POST", "/api/chains/create"),
            ("POST", "/api/lablog/append"),
            ("POST", "/api/notebooks/describe"),
            ("POST", "/api/notebooks/revise"),
            ("POST", "/api/notebooks/write"),
            ("POST", "/api/observer/labarchives/set"),
            ("POST", "/api/push/target"),
            ("POST", "/api/viewer/marks/cells"),
            ("POST", "/api/viewer/marks/freeform"),
            ("POST", "/api/viewer/marks/plot"),
            ("POST", "/api/viewer/marks/select"),
            ("POST", "/api/viewer/marks/tile"),
            ("POST", "/api/viewer/marks/tracks"),
            ("POST", "/api/viewer/marks/ui"),
            ("POST", "/api/viewer/navigate"),
            ("POST", "/api/viewer/seek"),
        ])

    def test_every_route_the_client_calls_is_on_the_allow_list(self):
        # The gap this closes, found the hard way: `get_labarchives_context` /
        # `set_labarchives_context` were written as client methods and wired into two MCP tools, but
        # their routes were never added to ALLOWED_ROUTES — so every call raised DisallowedRoute
        # before it reached the (perfectly healthy) server, and the assistant reported it as "a route
        # that isn't enabled on this server". Nothing caught it: the tools were tested against mocks,
        # and the allow-list was only ever asserted for routes someone remembered to name here.
        import re
        from pathlib import Path
        src = Path(__file__).resolve().parents[1].joinpath("cecelia_mcp", "client.py").read_text(encoding="utf-8")
        called = set(re.findall(r'_request\(\s*"(GET|POST)",\s*"([^"]+)"', src))
        self.assertGreaterEqual(len(called), 20, "regex found nothing — it stopped matching the source")
        self.assertEqual(sorted(called - set(ALLOWED_ROUTES)), [])

    def test_image_attributes_is_a_read_and_reuses_the_canonical_route(self):
        # Attribute discovery has ONE route (/api/plots/attrs) — the same one the summary canvas's
        # "compare by attribute" picker and the UMAP colour/facet picker use. The observer reads it
        # rather than growing a second attribute surface, and it is a GET, so the write set above is
        # untouched by exposing it. See docs/todo/MCP_BOARD_AUTHORING_PLAN.md, Phase 0.
        with _patch_urlopen({"attrs": [{"name": "Mouse", "values": ["1", "2"]}]}) as u:
            self.c.get_image_attributes("p", "s")
        req = u.call_args[0][0]
        self.assertEqual(req.method, "GET")
        self.assertIn("/api/plots/attrs", req.full_url)
        self.assertIn("setUid=s", req.full_url)
        # the subset filter is only sent when asked for
        self.assertNotIn("imageUids", req.full_url)
        with _patch_urlopen({"attrs": []}) as u:
            self.c.get_image_attributes("p", "s", "a,b")
        self.assertIn("imageUids=a%2Cb", u.call_args[0][0].full_url)

    def test_chain_launch_and_in_place_chain_edits_stay_unreachable(self):
        # The point of the whole design: Claude designs chains, the user runs them. Launching has no
        # HTTP route at all (it's the `chain:run` WS message), and the routes that would let Claude
        # overwrite, rename or delete a chain the user wired are deliberately off the list.
        for method, path in (("POST", "/api/chains/save"),      # unguarded overwrite (whiteboard's own)
                             ("POST", "/api/chains/rename"),
                             ("POST", "/api/chains/delete"),
                             ("POST", "/api/chains/run"),       # does not exist — must not be added
                             ("POST", "/api/chains/start")):
            with self.assertRaises(DisallowedRoute):
                self.c._request(method, path, body={"projectUid": "p"})

    def test_create_chain_posts_a_template(self):
        nodes = [{"id": "seg", "fn": "segment.cellpose", "params": {"cellDiameter": 30}},
                 {"id": "trk", "fn": "tracking.bayesian_tracking"}]
        edges = [{"from": "seg", "to": "trk"}]
        with _patch_urlopen({"ok": True, "name": "seg-track", "nodeCount": 2}) as u:
            self.c.create_chain("p", "seg-track", nodes, edges)
        req = u.call_args[0][0]
        self.assertEqual(req.method, "POST")
        self.assertTrue(req.full_url.endswith("/api/chains/create"))
        self.assertEqual(
            json.loads(req.data.decode()),
            {"projectUid": "p", "template": {"name": "seg-track", "nodes": nodes, "edges": edges}},
        )

    def test_create_chain_omits_empty_start_targets(self):
        # An empty startTargets must not be sent: the server treats a PRESENT-but-empty list the same
        # as absent today, but sending it invites a future reader to think the chain has a start dot.
        with _patch_urlopen({"ok": True}) as u:
            self.c.create_chain("p", "c", [{"id": "n1", "fn": "importImages.remove"}], [])
        body = json.loads(u.call_args[0][0].data.decode())
        self.assertNotIn("startTargets", body["template"])
        with _patch_urlopen({"ok": True}) as u:
            self.c.create_chain("p", "c", [{"id": "n1", "fn": "importImages.remove"}], [],
                                start_targets=["n1"])
        body = json.loads(u.call_args[0][0].data.decode())
        self.assertEqual(body["template"]["startTargets"], ["n1"])

    def test_revise_notebook_posts_cells(self):
        with _patch_urlopen({"ok": True, "file": "speed.jl", "snapshotVersion": 2}) as u:
            self.c.revise_notebook("p", "speed.jl", ["using Cecelia", "df = 2"], description="d2")
        req = u.call_args[0][0]
        self.assertEqual(req.method, "POST")
        self.assertTrue(req.full_url.endswith("/api/notebooks/revise"))
        self.assertEqual(
            json.loads(req.data.decode()),
            {"projectUid": "p", "file": "speed.jl", "cells": ["using Cecelia", "df = 2"], "description": "d2"},
        )

    def test_revise_notebook_omits_empty_description(self):
        # An empty description must NOT be sent — else the server (which updates the description only when
        # the key is present) would blank the notebook's existing description on a plain re-version.
        with _patch_urlopen({"ok": True, "file": "speed.jl", "snapshotVersion": 3}) as u:
            self.c.revise_notebook("p", "speed.jl", ["using Cecelia", "df = 3"])
        req = u.call_args[0][0]
        self.assertEqual(
            json.loads(req.data.decode()),
            {"projectUid": "p", "file": "speed.jl", "cells": ["using Cecelia", "df = 3"]},
        )

    def test_create_notebook_posts_cells(self):
        with _patch_urlopen({"ok": True, "file": "speed.jl"}) as u:
            self.c.create_notebook("p", "speed", ["using Cecelia", "df = 1"], description="d")
        req = u.call_args[0][0]
        self.assertEqual(req.method, "POST")
        self.assertTrue(req.full_url.endswith("/api/notebooks/write"))
        self.assertEqual(
            json.loads(req.data.decode()),
            {"projectUid": "p", "name": "speed", "cells": ["using Cecelia", "df = 1"], "description": "d"},
        )

    def test_set_notebook_description_posts(self):
        with _patch_urlopen({"ok": True}) as u:
            self.c.set_notebook_description("p", "speed.jl", "shorter blurb")
        req = u.call_args[0][0]
        self.assertEqual(req.method, "POST")
        self.assertTrue(req.full_url.endswith("/api/notebooks/describe"))
        self.assertEqual(
            json.loads(req.data.decode()),
            {"projectUid": "p", "file": "speed.jl", "description": "shorter blurb"},
        )

    def test_notebook_read_routes_allow_listed(self):
        self.assertIn(("GET", "/api/notebooks"), ALLOWED_ROUTES)
        self.assertIn(("GET", "/api/notebooks/content"), ALLOWED_ROUTES)

    # ── Blackboard (BIDIR_CONTEXT_PLAN Part 4) ──────────────────────────────
    def test_list_blackboard_entries_builds_url(self):
        with _patch_urlopen({"entries": []}) as u:
            self.c.list_blackboard_entries("p")
        req = u.call_args[0][0]
        self.assertEqual(req.method, "GET")
        self.assertIn("/api/blackboard?projectUid=p", req.full_url)

    def test_read_blackboard_entry_appends_version(self):
        with _patch_urlopen({"entry": {}}) as u:
            self.c.read_blackboard_entry("p", "bb-20260919T120000-abcdef", version=3)
        req = u.call_args[0][0]
        self.assertIn("entryId=bb-20260919T120000-abcdef", req.full_url)
        self.assertIn("version=3", req.full_url)

    def test_create_blackboard_entry_posts_content_and_attachments(self):
        with _patch_urlopen({"ok": True, "entryId": "bb-x"}) as u:
            self.c.create_blackboard_entry("p", "Chain design", "# hi\n",
                                            attach_capture_ids=["cap-1", "cap-2"])
        req = u.call_args[0][0]
        self.assertEqual(req.method, "POST")
        self.assertTrue(req.full_url.endswith("/api/blackboard/create"))
        self.assertEqual(
            json.loads(req.data.decode()),
            {"projectUid": "p", "title": "Chain design", "content": "# hi\n",
             "attachments": ["cap-1", "cap-2"]},
        )

    def test_create_blackboard_entry_omits_empty_attachments(self):
        with _patch_urlopen({"ok": True, "entryId": "bb-x"}) as u:
            self.c.create_blackboard_entry("p", "T", "b")
        self.assertNotIn("attachments", json.loads(u.call_args[0][0].data.decode()))

    def test_revise_blackboard_entry_omits_absent_attachments_and_note(self):
        # Attachments absent from a revise call MUST NOT be sent — else the server would REPLACE the
        # existing set with an empty list. The keep-existing-attachments contract is the whole reason
        # the parameter defaults to None here rather than [].
        with _patch_urlopen({"ok": True, "version": 4}) as u:
            self.c.revise_blackboard_entry("p", "bb-x", "# updated\n")
        body = json.loads(u.call_args[0][0].data.decode())
        self.assertEqual(body, {"projectUid": "p", "entryId": "bb-x", "content": "# updated\n"})
        # An empty explicit list DOES get sent — the caller means "drop all attachments".
        with _patch_urlopen({"ok": True, "version": 5}) as u:
            self.c.revise_blackboard_entry("p", "bb-x", "# updated\n", attach_capture_ids=[])
        self.assertEqual(
            json.loads(u.call_args[0][0].data.decode())["attachments"], [])

    def test_blackboard_destructive_routes_not_allow_listed(self):
        # Restore / prune / delete stay OFF the MCP surface — user-driven only, matching notebooks.
        for path in ("/api/blackboard/restore", "/api/blackboard/prune", "/api/blackboard/delete"):
            self.assertNotIn(("POST", path), ALLOWED_ROUTES)

    def test_mark_tracks_posts_the_right_body(self):
        # Point-out: track ids, per (image, vn), optional focus + label + TTL. Reaches the popup
        # viewer via the WS `viewer:mark` frame; no persistence.
        with _patch_urlopen({"ok": True, "markerId": "mark-abc"}) as u:
            self.c.mark_tracks("p", "img1", "flowTom", [3, 7, 42],
                               focus_id=7, label="two of interest", ttl_s=60)
        req = u.call_args[0][0]
        self.assertEqual(req.method, "POST")
        self.assertTrue(req.full_url.endswith("/api/viewer/marks/tracks"))
        self.assertEqual(json.loads(req.data.decode()), {
            "projectUid": "p", "imageUid": "img1", "valueName": "flowTom",
            "trackIds": [3, 7, 42], "focusId": 7, "label": "two of interest", "ttl_s": 60,
        })

    def test_mark_tracks_omits_optional_fields_when_default(self):
        # Sparse body — a mark without a label or explicit TTL should send only the required fields
        # so the server's own defaults (5-min TTL per Decision 18) apply.
        with _patch_urlopen({"ok": True, "markerId": "mark-abc"}) as u:
            self.c.mark_tracks("p", "img1", "flowTom", [3])
        req = u.call_args[0][0]
        body = json.loads(req.data.decode())
        self.assertNotIn("focusId", body)
        self.assertNotIn("label", body)
        self.assertNotIn("ttl_s", body)

    def test_mark_cells_uses_labelIds_key(self):
        # `label_ids` on the tool becomes `labelIds` in the body — the server key that the pick
        # membership uses. A `focusId` on the cell mark is the one distinct cell within the set.
        with _patch_urlopen({"ok": True, "markerId": "mark-def"}) as u:
            self.c.mark_cells("p", "img1", "flowTom", [101, 202], focus_id=101, label="lead")
        req = u.call_args[0][0]
        body = json.loads(req.data.decode())
        self.assertEqual(body["labelIds"], [101, 202])
        self.assertEqual(body["focusId"], 101)
        self.assertEqual(body["label"], "lead")

    def test_mark_ui_posts_the_anchor(self):
        with _patch_urlopen({"ok": True, "markerId": "mark-u1"}) as u:
            self.c.mark_ui("p", "nav:/segment", label="here", ttl_s=60)
        req = u.call_args[0][0]
        self.assertTrue(req.full_url.endswith("/api/viewer/marks/ui"))
        self.assertEqual(json.loads(req.data.decode()),
                         {"projectUid": "p", "anchor": "nav:/segment", "label": "here", "ttl_s": 60})

    def test_mark_freeform_posts_target_and_overlay(self):
        with _patch_urlopen({"ok": True, "markerId": "mark-f1"}) as u:
            self.c.mark_freeform("p", "cap-20260918T175413-a1b2c3",
                                 overlay=[{"kind": "circle", "geom": {"cx": 0.5, "cy": 0.5, "r": 0.1}}])
        req = u.call_args[0][0]
        self.assertTrue(req.full_url.endswith("/api/viewer/marks/freeform"))
        body = json.loads(req.data.decode())
        self.assertEqual(body["target"], "cap-20260918T175413-a1b2c3")
        self.assertEqual(len(body["overlay"]), 1)
        self.assertNotIn("imageUid", body)

    def test_mark_plot_posts_family_plot_id_and_coords(self):
        # BIDIR PR #4b — plot point-out. family + plotId + u/v; optional cell + label + ttl_s.
        with _patch_urlopen({"ok": True, "markerId": "mark-p1"}) as u:
            self.c.mark_plot("p", "gate-scatter", "gate:flow:IMG1:default:3", 0.72, 0.35,
                             label="outlier", ttl_s=90)
        req = u.call_args[0][0]
        self.assertTrue(req.full_url.endswith("/api/viewer/marks/plot"))
        body = json.loads(req.data.decode())
        self.assertEqual(body["family"], "gate-scatter")
        self.assertEqual(body["plotId"], "gate:flow:IMG1:default:3")
        self.assertEqual(body["u"], 0.72)
        self.assertEqual(body["v"], 0.35)
        self.assertNotIn("cell", body)                                  # omitted when empty
        self.assertEqual(body["label"], "outlier")
        self.assertEqual(body["ttl_s"], 90)

    def test_mark_plot_carries_cell_for_multi_cell_families(self):
        with _patch_urlopen({"ok": True, "markerId": "mark-p2"}) as u:
            self.c.mark_plot("p", "image-strip", "strip-abc", 0.5, 0.5, cell="cell=3")
        body = json.loads(u.call_args[0][0].data.decode())
        self.assertEqual(body["cell"], "cell=3")

    def test_select_on_plot_posts_kind_and_sources(self):
        # LINKED_BRUSHING follow-up — multi-source point-out. `sources` is the exact shape the
        # plot-brush emit produces on the frontend, so a captured brush can be replayed verbatim.
        sources = [
            {"imageUid": "imgA", "valueName": "flowTom", "pop": "root/B", "ids": [3, 7]},
            {"imageUid": "imgB", "valueName": "flowTom",                   "ids": [11]},
        ]
        with _patch_urlopen({"ok": True, "markerId": "mark-s1"}) as u:
            self.c.select_on_plot("p", "track", sources, focus_id=7, label="fastest few", ttl_s=120)
        req = u.call_args[0][0]
        self.assertEqual(req.method, "POST")
        self.assertTrue(req.full_url.endswith("/api/viewer/marks/select"))
        body = json.loads(req.data.decode())
        self.assertEqual(body["projectUid"], "p")
        self.assertEqual(body["kind"], "track")
        self.assertEqual(body["sources"], sources)
        self.assertEqual(body["focusId"], 7)
        self.assertEqual(body["label"], "fastest few")
        self.assertEqual(body["ttl_s"], 120)

    def test_select_on_plot_omits_optionals_when_default(self):
        with _patch_urlopen({"ok": True, "markerId": "mark-s2"}) as u:
            self.c.select_on_plot("p", "cell",
                                  [{"imageUid": "imgA", "valueName": "default", "ids": [42]}])
        body = json.loads(u.call_args[0][0].data.decode())
        self.assertNotIn("focusId", body)
        self.assertNotIn("label", body)
        self.assertNotIn("ttl_s", body)

    def test_bidir_capture_read_routes_allow_listed_but_write_is_not(self):
        # BIDIR share-in: Claude may READ the captures the user shares, but the write route (POST
        # /api/viewer/capture) is deliberately absent — captures are authored by the frontend
        # Share button, not by Claude, so a fabricated "the user shared this" cannot land in the
        # project's captures dir.
        self.assertIn(("GET", "/api/viewer/captures"), ALLOWED_ROUTES)
        self.assertIn(("GET", "/api/viewer/capture"), ALLOWED_ROUTES)
        self.assertNotIn(("POST", "/api/viewer/capture"), ALLOWED_ROUTES)

    def test_get_recent_captures_builds_url(self):
        with _patch_urlopen({"items": []}) as u:
            self.c.get_recent_captures("p", limit=5)
        req = u.call_args[0][0]
        self.assertEqual(req.method, "GET")
        self.assertIn("/api/viewer/captures?", req.full_url)
        self.assertIn("projectUid=p", req.full_url)
        self.assertIn("limit=5", req.full_url)

    def test_get_capture_builds_url(self):
        with _patch_urlopen({"capture": {}, "frame": ""}) as u:
            self.c.get_capture("p", "cap-20260918T140000-abcdef")
        req = u.call_args[0][0]
        self.assertEqual(req.method, "GET")
        self.assertIn("/api/viewer/capture?", req.full_url)
        self.assertIn("projectUid=p", req.full_url)
        self.assertIn("captureId=cap-20260918T140000-abcdef", req.full_url)

    def test_get_object_ids_builds_url_with_kind(self):
        # BIDIR follow-up: real cell/track id enumeration. Body carries kind + limit + optional
        # sample so the server can stride-sample a big population without churning the JSON.
        with _patch_urlopen({"ids": [1, 2, 3], "total": 3, "truncated": False, "sampled": False}) as u:
            self.c.get_object_ids("p", "img1", "flowTom", kind="cells", limit=200)
        req = u.call_args[0][0]
        self.assertEqual(req.method, "GET")
        self.assertIn("/api/labels/ids?", req.full_url)
        self.assertIn("projectUid=p", req.full_url)
        self.assertIn("imageUid=img1", req.full_url)
        self.assertIn("valueName=flowTom", req.full_url)
        self.assertIn("kind=cells", req.full_url)
        self.assertIn("limit=200", req.full_url)
        self.assertNotIn("sample=", req.full_url)   # default false ⇒ omitted

    def test_get_object_ids_passes_sample_when_true(self):
        with _patch_urlopen({"ids": [], "total": 0, "truncated": False, "sampled": False}) as u:
            self.c.get_object_ids("p", "img1", "flowTom", kind="tracks", limit=50, sample=True)
        req = u.call_args[0][0]
        self.assertIn("kind=tracks", req.full_url)
        self.assertIn("sample=true", req.full_url)

    def test_list_notebooks_builds_url(self):
        with _patch_urlopen({"notebooks": []}) as u:
            self.c.list_notebooks("p")
        req = u.call_args[0][0]
        self.assertEqual(req.method, "GET")
        self.assertIn("/api/notebooks?projectUid=p", req.full_url)

    def test_get_notebook_builds_url(self):
        with _patch_urlopen({"file": "speed.jl", "scope": "project", "content": "x=1"}) as u:
            self.c.get_notebook("p", "speed.jl")
        req = u.call_args[0][0]
        self.assertEqual(req.method, "GET")
        self.assertIn("/api/notebooks/content?", req.full_url)
        self.assertIn("file=speed.jl", req.full_url)

    def test_list_images_builds_url(self):
        with _patch_urlopen({"images": []}) as u:
            self.c.list_images("proj1")
        req = u.call_args[0][0]
        self.assertEqual(req.method, "GET")
        self.assertIn("/api/images?projectUid=proj1", req.full_url)

    def test_find_object_builds_url_and_is_a_read(self):
        # The one lookup that does NOT take a projectUid — it is how a bare uid gets one.
        self.assertIn(("GET", "/api/objects/find"), ALLOWED_ROUTES)
        with _patch_urlopen({"matches": []}) as u:
            self.c.find_object("p6t4mC")
        req = u.call_args[0][0]
        self.assertEqual(req.method, "GET")
        self.assertIn("/api/objects/find?q=p6t4mC", req.full_url)
        self.assertNotIn("projectUid", req.full_url)
        self.assertNotIn("limit", req.full_url)          # unset limit is the server's default
        with _patch_urlopen({"matches": []}) as u:
            self.c.find_object("mertk 3", limit=5)
        url = u.call_args[0][0].full_url
        self.assertIn("q=mertk+3", url)                  # a name fragment is url-encoded, not split
        self.assertIn("limit=5", url)

    def test_task_log_encodes_all_params(self):
        with _patch_urlopen({"exists": False, "content": ""}) as u:
            self.c.get_task_log("p", "img1", "segment.cellpose")
        url = u.call_args[0][0].full_url
        self.assertIn("imageUid=img1", url)
        self.assertIn("fun=segment.cellpose", url)

    def test_task_history_drops_none_limit(self):
        with _patch_urlopen({"history": []}) as u:
            self.c.get_task_history("p")
        self.assertNotIn("limit", u.call_args[0][0].full_url)
        with _patch_urlopen({"history": []}) as u:
            self.c.get_task_history("p", limit=5)
        self.assertIn("limit=5", u.call_args[0][0].full_url)

    def test_module_params_builds_url_and_trims_spec(self):
        self.assertIn(("GET", "/api/tasks/definitions"), ALLOWED_ROUTES)
        raw = {"tracking": [{
            "task": "bayesianTracking", "fun_name": "tracking.bayesian_tracking",
            "label": "Bayesian tracking", "category": "Tracking",
            "env": ["local"], "resource_pool": "default",
            "params": [
                {"key": "maxSearchRadius", "label": "Max search radius", "type": "int",
                 "min": 1, "max": 200, "step": 1, "default": 20, "tip": "px/frame"},
                {"key": "valueName", "type": "valueNameSelection", "field": "labels",
                 "default": "default", "options": ["a", "b", "c"]},
            ],
        }]}
        with _patch_urlopen(raw) as u:
            out = self.c.get_module_params("tracking")
        url = u.call_args[0][0].full_url
        self.assertIn("/api/tasks/definitions?", url)
        self.assertIn("category=tracking", url)
        spec = out["tracking"][0]
        self.assertEqual(spec["fun_name"], "tracking.bayesian_tracking")
        self.assertNotIn("env", spec)               # top-level UI plumbing stripped
        self.assertNotIn("resource_pool", spec)
        p_knob, p_sel = spec["params"]
        self.assertEqual((p_knob["min"], p_knob["max"], p_knob["default"]), (1, 200, 20))
        # `field` is KEPT — it says what a selection param wants (which versioned field), which is the
        # only hint available when authoring a chain node, since the option list is live project state
        # and not in the spec. It was stripped as "widget internals" while Claude could only suggest.
        self.assertEqual(p_sel["field"], "labels")
        self.assertNotIn("options", p_sel)          # big option lists stripped (the payload win)
        with _patch_urlopen({}) as u:               # no category → all modules
            self.c.get_module_params()
        self.assertNotIn("category", u.call_args[0][0].full_url)

    def test_select_options_are_kept_but_picker_options_are_not(self):
        # A `select`'s options are a SHORT static enum and the server validates against them
        # (_validate_leaf), so stripping them left the caller able only to echo the default — it could
        # not propose a considered value. The project-derived pickers are the opposite case: their
        # candidates aren't in the spec at all and have to be looked up per project, so there is nothing
        # useful to keep and the list can be long.
        raw = {"cleanupImages": [{
            "fun_name": "cleanupImages.driftCorrect", "label": "Drift",
            "params": [
                {"key": "driftNormalisation", "type": "select", "default": "phase",
                 "options": [{"label": "None", "value": "none"}, {"label": "Phase", "value": "phase"}]},
                {"key": "driftChannel", "type": "channelSelection", "default": [],
                 "options": ["ch1", "ch2"]},
            ],
        }]}
        with _patch_urlopen(raw):
            out = self.c.get_module_params("cleanupImages")
        p_select, p_channel = out["cleanupImages"][0]["params"]
        self.assertEqual(p_select["options"], ["none", "phase"])   # values, not {label,value} pairs
        self.assertNotIn("options", p_channel)

    def test_group_and_section_children_survive_the_trim(self):
        # A non-recursive trim reported {"key": "models", "type": "group"} and nothing inside — hiding
        # cellpose's whole knob set (diameter + its µm label + range, cell/nuc channels). An author then
        # cannot set them, and the server validates nested params anyway, so a 400 was reachable on a
        # param the caller had never been shown.
        raw = {"segment": [{"fun_name": "segment.cellpose", "label": "Cellpose", "params": [
            {"key": "models", "type": "group", "label": "Models", "params": [
                {"key": "cellDiameter", "type": "int", "label": "Cell diameter (µm)",
                 "min": 1, "max": 500, "default": 10, "tip": "Expected cell diameter in µm"},
                {"key": "cellChannels", "type": "channelSelection", "label": "Cell channels",
                 "default": [], "options": ["a", "b"]},
            ]},
            {"key": "imageTiling", "type": "section", "params": [
                {"key": "blockSize", "type": "int", "min": 128, "max": 4096, "default": 512},
            ]},
        ]}]}
        with _patch_urlopen(raw):
            out = self.c.get_module_params("segment")
        group, section = out["segment"][0]["params"]
        diameter, channels = group["params"]
        self.assertEqual(diameter["label"], "Cell diameter (µm)")   # the UNIT lives in the label
        self.assertEqual((diameter["min"], diameter["max"]), (1, 500))
        self.assertNotIn("options", channels)                      # picker options still stripped
        self.assertEqual(section["params"][0]["max"], 4096)         # sections recurse too

    def test_select_options_are_capped(self):
        raw = {"m": [{"fun_name": "m.t", "label": "T", "params": [
            {"key": "k", "type": "select", "default": "v0",
             "options": [{"value": f"v{i}"} for i in range(100)]},
        ]}]}
        with _patch_urlopen(raw):
            out = self.c.get_module_params("m")
        self.assertEqual(len(out["m"][0]["params"][0]["options"]), 24)

    def test_available_plots_builds_url_and_drops_unset_module(self):
        self.assertIn(("GET", "/api/plots/definitions"), ALLOWED_ROUTES)
        with _patch_urlopen([{"id": "cell_properties"}]) as u:
            out = self.c.get_available_plots("behaviourAnalysis")
        url = u.call_args[0][0].full_url
        self.assertIn("/api/plots/definitions?", url)
        self.assertIn("module=behaviourAnalysis", url)
        self.assertEqual(out[0]["id"], "cell_properties")
        with _patch_urlopen([]) as u:                       # no module → all plots
            self.c.get_available_plots()
        self.assertNotIn("module", u.call_args[0][0].full_url)

    def test_cohort_qc_builds_url_and_drops_unset(self):
        self.assertIn(("GET", "/api/qc/cohort"), ALLOWED_ROUTES)
        with _patch_urlopen({"metrics": {}}) as u:
            self.c.get_cohort_qc("p", "set1", "segment.measureLabels")
        url = u.call_args[0][0].full_url
        self.assertIn("/api/qc/cohort?", url)
        self.assertIn("setUid=set1", url)
        self.assertIn("funName=segment.measureLabels", url)
        self.assertNotIn("threshold", url)            # unset optional dropped
        # explicit optionals are passed through
        with _patch_urlopen({"metrics": {}}) as u:
            self.c.get_cohort_qc("p", "set1", "tracking.bayesian_tracking",
                                 value_name="A", threshold=3.0)
        url = u.call_args[0][0].full_url
        self.assertIn("valueName=A", url)
        self.assertIn("threshold=3.0", url)

    def test_analysis_lineage_builds_url_and_drops_unset(self):
        self.assertIn(("GET", "/api/analysis/lineage"), ALLOWED_ROUTES)
        with _patch_urlopen({"images": []}) as u:
            self.c.get_analysis_lineage("p")                       # whole project — no image/set scope
        url = u.call_args[0][0].full_url
        self.assertIn("/api/analysis/lineage?", url)
        self.assertIn("projectUid=p", url)
        self.assertNotIn("imageUid", url)                          # unset optionals dropped
        self.assertNotIn("setUid", url)
        with _patch_urlopen({"images": []}) as u:
            self.c.get_analysis_lineage("p", image_uid="i1")       # scoped to one image
        self.assertIn("imageUid=i1", u.call_args[0][0].full_url)

    def test_populations_builds_url_and_drops_unset(self):
        self.assertIn(("GET", "/api/analysis/populations"), ALLOWED_ROUTES)
        with _patch_urlopen({"images": []}) as u:
            self.c.get_populations("p", set_uid="s1")              # scoped to one set
        url = u.call_args[0][0].full_url
        self.assertIn("/api/analysis/populations?", url)
        self.assertIn("setUid=s1", url)
        self.assertNotIn("imageUid", url)                          # unset optional dropped

    def test_measure_summary_builds_url_and_drops_unset(self):
        self.assertIn(("GET", "/api/analysis/measures"), ALLOWED_ROUTES)
        with _patch_urlopen({"images": []}) as u:
            self.c.get_measure_summary("p", image_uid="i1")       # scoped to one image
        url = u.call_args[0][0].full_url
        self.assertIn("/api/analysis/measures?", url)
        self.assertIn("imageUid=i1", url)
        self.assertNotIn("setUid", url)                           # unset optional dropped

    def test_behaviour_and_cluster_summary_are_allowed_gets(self):
        self.assertIn(("GET", "/api/analysis/behaviour"), ALLOWED_ROUTES)
        self.assertIn(("GET", "/api/analysis/clusters"), ALLOWED_ROUTES)
        with _patch_urlopen({"images": []}) as u:
            self.c.get_behaviour_summary("p", set_uid="s1")
        self.assertIn("/api/analysis/behaviour?", u.call_args[0][0].full_url)
        self.assertIn("setUid=s1", u.call_args[0][0].full_url)
        with _patch_urlopen({"images": []}) as u:
            self.c.get_cluster_summary("p", image_uid="i1")
        self.assertIn("/api/analysis/clusters?", u.call_args[0][0].full_url)
        self.assertIn("imageUid=i1", u.call_args[0][0].full_url)

    def test_spatial_stats_is_an_allowed_get(self):
        self.assertIn(("GET", "/api/analysis/spatial"), ALLOWED_ROUTES)
        with _patch_urlopen({"images": []}) as u:
            self.c.get_spatial_stats("p", image_uid="i1")
        self.assertIn("/api/analysis/spatial?", u.call_args[0][0].full_url)
        self.assertIn("imageUid=i1", u.call_args[0][0].full_url)

    def test_chains_is_an_allowed_project_level_get(self):
        self.assertIn(("GET", "/api/analysis/chains"), ALLOWED_ROUTES)
        with _patch_urlopen({"templates": [], "runs": []}) as u:
            self.c.get_chains("p")
        url = u.call_args[0][0].full_url
        self.assertIn("/api/analysis/chains?", url)
        self.assertIn("projectUid=p", url)
        self.assertNotIn("imageUid", url)                          # project-level: no scope params
        self.assertNotIn("setUid", url)

    def test_repl_api_is_an_allowed_project_independent_get(self):
        self.assertIn(("GET", "/api/repl/api"), ALLOWED_ROUTES)
        with _patch_urlopen({"api": [{"name": "pop_df"}], "doc": "cookbook"}) as u:
            out = self.c.get_repl_api()
        req = u.call_args[0][0]
        self.assertEqual(req.method, "GET")
        self.assertTrue(req.full_url.endswith("/api/repl/api"))     # no query params
        self.assertEqual(out["api"][0]["name"], "pop_df")

    def test_session_briefing_builds_url(self):
        self.assertIn(("GET", "/api/observer/briefing"), ALLOWED_ROUTES)
        with _patch_urlopen({"projectUid": "p", "flagged": [], "recentLabLog": []}) as u:
            out = self.c.get_session_briefing("p")
        url = u.call_args[0][0].full_url
        self.assertIn("/api/observer/briefing?", url)
        self.assertIn("projectUid=p", url)
        self.assertEqual(out["projectUid"], "p")

    def test_recent_logs_is_an_allowed_get(self):
        self.assertIn(("GET", "/api/logs/recent"), ALLOWED_ROUTES)
        with _patch_urlopen({"logs": [{"level": "error", "message": "boom"}]}) as u:
            out = self.c.get_recent_logs()
        req = u.call_args[0][0]
        self.assertEqual(req.method, "GET")
        self.assertTrue(req.full_url.endswith("/api/logs/recent"))
        self.assertEqual(out["logs"][0]["message"], "boom")

    def test_append_posts_json_body(self):
        with _patch_urlopen({"ok": True}) as u:
            self.c.append_lab_log("p", "Claude", ["hello", "world"])
        req = u.call_args[0][0]
        self.assertEqual(req.method, "POST")
        self.assertEqual(req.get_header("Content-type"), "application/json")
        self.assertEqual(
            json.loads(req.data.decode()),
            {"projectUid": "p", "author": "Claude", "lines": ["hello", "world"]},
        )

    def test_http_error_becomes_apierror_with_detail(self):
        # clear=True on os.environ isolates the test from the bidir Part 5 auto-pair middleware
        # (which would fire an extra /api/push/target POST if CLAUDE_CODE_MESSAGING_* were set,
        # consuming the shared HTTPError body). Factory raises a fresh HTTPError per call so a
        # future test that keeps env set still gets a virgin body pointer on each urlopen.
        def make_err(*a, **kw):
            raise urllib.error.HTTPError(
                "http://x", 404, "nf", {},
                io.BytesIO(json.dumps({"error": "Project not found"}).encode()),
            )
        with mock.patch.dict("os.environ", {}, clear=True), \
             mock.patch("cecelia_mcp.client.urllib.request.urlopen", side_effect=make_err):
            with self.assertRaises(ApiError) as ctx:
                self.c.list_images("nope")
        self.assertEqual(ctx.exception.status, 404)
        self.assertIn("Project not found", ctx.exception.message)

    # ── Bidir Part 5 push pairing (BIDIR_PUSH_PLAN PR #1) ────────────────────────
    def test_auto_pair_noop_when_env_missing(self):
        # No CLAUDE_CODE_MESSAGING_SOCKET / TOKEN in the process (running under an old Claude Code,
        # standalone, or outside any Claude session) ⇒ auto-pair must be silent, and the real
        # tool call still goes through. This is the "runs anywhere" contract for the middleware.
        with mock.patch.dict("os.environ", {}, clear=True), _patch_urlopen({"images": []}) as up:
            self.c.list_images("proj-abc")
        # exactly one HTTP call: the tool's own, no auto-pair POST
        self.assertEqual(up.call_count, 1)
        req = up.call_args[0][0]
        self.assertTrue(req.full_url.endswith("/api/images?projectUid=proj-abc"))
        self.assertEqual({}, self.c._paired)

    def test_auto_pair_fires_once_per_project(self):
        # First tool call in a session with the env vars set: two HTTP calls — the pairing POST
        # and the real tool. Second call: cache hit, only the tool call.
        env = {
            "CLAUDE_CODE_MESSAGING_SOCKET": "/run/user/1000/cc-socks/12345.sock",
            "CLAUDE_CODE_MESSAGING_TOKEN":  "tok-abc",
            "CLAUDE_CODE_SESSION_ID":       "abcdef12-3456-7890",
            "CLAUDE_PID":                   "12345",
        }
        with mock.patch.dict("os.environ", env, clear=True), _patch_urlopen({"images": []}) as up:
            self.c.list_images("proj-abc")
            self.assertEqual(up.call_count, 2)  # pair POST + list_images GET
            # first call is the auto-pair POST — assert its body carries the socket+token
            pair_req = up.call_args_list[0][0][0]
            self.assertEqual(pair_req.method, "POST")
            self.assertTrue(pair_req.full_url.endswith("/api/push/target"))
            body = json.loads(pair_req.data.decode())
            self.assertEqual(body["projectUid"], "proj-abc")
            self.assertEqual(body["socketPath"], "/run/user/1000/cc-socks/12345.sock")
            self.assertEqual(body["token"], "tok-abc")
            self.assertEqual(body["pairedFromPid"], "12345")
            # second tool call on the same project ⇒ no extra pair POST
            up.reset_mock()
            self.c.list_images("proj-abc")
            self.assertEqual(up.call_count, 1)
            self.assertTrue(up.call_args[0][0].full_url.endswith("/api/images?projectUid=proj-abc"))

    def test_auto_pair_refires_on_different_project(self):
        # Two projects touched in one session ⇒ two pair POSTs (one per project), then cached.
        env = {"CLAUDE_CODE_MESSAGING_SOCKET": "/tmp/s.sock",
               "CLAUDE_CODE_MESSAGING_TOKEN":  "t"}
        with mock.patch.dict("os.environ", env, clear=True), _patch_urlopen({"images": []}) as up:
            self.c.list_images("proj-a")
            self.c.list_images("proj-b")
            self.c.list_images("proj-a")   # cached
            self.c.list_images("proj-b")   # cached
        # 4 tool calls + 2 pair POSTs = 6
        self.assertEqual(up.call_count, 6)
        methods = [call[0][0].method for call in up.call_args_list]
        # first two calls: pair POST then list_images GET for proj-a
        self.assertEqual(methods[0], "POST")
        self.assertEqual(methods[1], "GET")
        # third + fourth: pair POST then list_images GET for proj-b
        self.assertEqual(methods[2], "POST")
        self.assertEqual(methods[3], "GET")
        # last two: cached — no pair POST
        self.assertEqual(methods[4], "GET")
        self.assertEqual(methods[5], "GET")

    def test_auto_pair_never_recurses_on_the_pairing_route(self):
        # A POST to /api/push/target must not itself trigger auto-pair (infinite recursion). The
        # explicit register_push_target path exercises this: only ONE POST hits urlopen.
        env = {"CLAUDE_CODE_MESSAGING_SOCKET": "/tmp/s.sock",
               "CLAUDE_CODE_MESSAGING_TOKEN":  "t"}
        with mock.patch.dict("os.environ", env, clear=True), _patch_urlopen({"ok": True}) as up:
            self.c.register_push_target("proj-a")
        self.assertEqual(up.call_count, 1)
        self.assertEqual(up.call_args[0][0].method, "POST")

    def test_auto_pair_skipped_for_app_spawned_turns(self):
        # A headless `claude -p` spawned by Cecelia sets its OWN messaging env for its MCP children;
        # the headless MCP config adds CECELIA_OBSERVER_NO_PAIR so the turn can't re-pair the
        # project to a session that is about to exit (seen live, KIWI_ASSISTANT_PLAN Phase 0).
        env = {"CLAUDE_CODE_MESSAGING_SOCKET": "/tmp/s.sock",
               "CLAUDE_CODE_MESSAGING_TOKEN":  "t",
               "CECELIA_OBSERVER_NO_PAIR":     "1"}
        with mock.patch.dict("os.environ", env, clear=True), _patch_urlopen({"images": []}) as up:
            self.c.list_images("proj-abc")
        self.assertEqual(up.call_count, 1)               # the tool call only — no pair POST
        self.assertEqual(up.call_args[0][0].method, "GET")
        self.assertEqual({}, self.c._paired)
        # the explicit re-pair tool refuses too, rather than pairing the throwaway session
        with mock.patch.dict("os.environ", env, clear=True), _patch_urlopen({"ok": True}) as up:
            with self.assertRaises(ApiError):
                self.c.register_push_target("proj-abc")
            self.assertEqual(up.call_count, 0)
        # "0" means not set — pairing works as normal
        env["CECELIA_OBSERVER_NO_PAIR"] = "0"
        with mock.patch.dict("os.environ", env, clear=True), _patch_urlopen({"images": []}) as up:
            self.c.list_images("proj-abc")
        self.assertEqual(up.call_count, 2)

    def test_register_push_target_errors_without_env(self):
        # Explicit re-pair when env vars are absent ⇒ ApiError with an actionable message so the
        # user knows the fix is to upgrade Claude Code (not to keep retrying).
        with mock.patch.dict("os.environ", {}, clear=True):
            with self.assertRaises(ApiError) as ctx:
                self.c.register_push_target("proj-a")
        self.assertIn("CLAUDE_CODE_MESSAGING", ctx.exception.message)

    def test_unreachable_api_becomes_apierror(self):
        with mock.patch(
            "cecelia_mcp.client.urllib.request.urlopen",
            side_effect=urllib.error.URLError("refused"),
        ):
            with self.assertRaises(ApiError) as ctx:
                self.c.read_lab_log("p")
        self.assertEqual(ctx.exception.status, 0)
        self.assertIn("cannot reach Cecelia API", ctx.exception.message)


if __name__ == "__main__":
    unittest.main()
