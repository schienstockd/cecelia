"""Server-level checks: the FastMCP server imports cleanly and actually REGISTERS the expected tools.

Complements test_client.py (which covers the HTTP client in isolation). Importing `server` is
side-effect-free — the client is constructed lazily and `mcp.run()` only fires under `__main__` — so
this just asserts the wiring: e.g. `get_repl_api` is exposed as a tool, not merely defined. Needs
mcp/fastmcp in the env, so it runs under `pixi run test-mcp`.
"""
import asyncio
import unittest

from cecelia_mcp import guidance, server


class ServerToolRegistrationTest(unittest.TestCase):
    def setUp(self):
        # FastMCP.list_tools() is the async protocol accessor → the registered Tool objects.
        self.names = {t.name for t in asyncio.run(server.mcp.list_tools())}

    def test_get_repl_api_is_registered(self):
        self.assertIn("get_repl_api", self.names)

    def test_expected_read_and_write_tools_registered(self):
        for tool in (
            "get_project_info", "list_images", "find_object", "get_task_history",
            "get_module_params", "get_available_plots", "get_analysis_lineage", "get_populations",
            "get_measure_summary", "get_behaviour_summary", "get_cluster_summary",
            "get_chains", "get_cohort_qc", "get_repl_api", "get_session_briefing",
            "get_recent_logs", "read_lab_log", "append_lab_log", "create_notebook",
            "set_notebook_description", "revise_notebook", "list_notebooks", "get_notebook",
            "create_chain",
            "mark_tracks", "mark_cells",   # bidir point-out (BIDIR_CONTEXT_PLAN PR #4)
            "point_at_ui", "mark_freeform", # bidir point-out UI + freeform (PR #5)
            "mark_tile", "get_landscape",   # bidir landscape overlay (PR #6, Decision 14 reframe)
            "get_recent_captures", "get_capture",   # bidir share-in (BIDIR_CONTEXT_PLAN PR #3)
            "get_capture_landscape_tiles",          # landscape drill-down (LANDSCAPE Phase 6 follow-up)
            "get_object_ids",   # bidir follow-up: real cell/track ids for mark_cells / mark_tracks
            "register_push_target",   # bidir Part 5: explicit re-pair (auto-pair via middleware)
            "list_blackboard_entries", "read_blackboard_entry",   # bidir Part 4 (Blackboard) — reads
            "create_blackboard_entry", "revise_blackboard_entry", # bidir Part 4 (Blackboard) — writes
            "set_blackboard_status",                              # PROJECT_MEMORY_PLAN P1 — status flip (no snapshot)
            "search_blackboard",                                  # PROJECT_MEMORY_PLAN P2 — substring search over titles+bodies
        ):
            self.assertIn(tool, self.names)

    def test_get_capture_returns_image_and_envelope(self):
        # Wiring, not text: the tool must actually split the API's data-URL frame into an Image
        # content block AND the envelope, so Claude both SEES the frame and reads the address.
        original = server._client.get_capture
        # a 1x1 png (base64) — smallest legal payload
        png_b64 = ("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAD"
                   "UlEQVR4nGNgYGD4DwABBAEAfbLI3wAAAABJRU5ErkJggg==")
        server._client.get_capture = lambda uid, cid: {
            "capture": {"captureId": cid, "surface": "viewer_frame",
                        "address": {"projectUid": uid, "imageUid": "IMG1"}},
            "frame": f"data:image/png;base64,{png_b64}",
        }
        try:
            out = server.get_capture("NRUBxU", "cap-20260918T140000-abcdef")
        finally:
            server._client.get_capture = original
        self.assertEqual(2, len(out))
        # first block is the Image content, second is the envelope dict
        from mcp.server.fastmcp import Image
        self.assertIsInstance(out[0], Image)
        self.assertEqual({"captureId": "cap-20260918T140000-abcdef",
                          "surface": "viewer_frame",
                          "address": {"projectUid": "NRUBxU", "imageUid": "IMG1"}}, out[1])

    def test_get_capture_slims_landscape_tiles(self):
        # A capture with a fat v2 landscape must come back with the slim tile shape: prelude
        # (channelNames + popMap), per-tile positional channels + [popKey, count] pop pairs,
        # dropped row/col/stats. The stored envelope stays fat — this transform happens on the
        # tool response only. See mcp/cecelia_mcp/landscape_slim.py for the shape spec.
        original = server._client.get_capture
        fat_landscape = {
            "grid": {"cols": 2, "rows": 2}, "legend": [], "schemaVersion": 2,
            "tiles": [
                {"id": "A1", "row": 0, "col": 0, "category": "dark",
                 "stats": {"intensity": 0, "peak": 0, "variance": 0, "edges": 0},
                 "channels": {"GFP": {"mean": 0.5, "snr": 12.0},
                              "TOM": {"mean": 0.1, "snr": 3.0}},
                 "segCount": 3,
                 "pops": [{"path": "/live/tnaive", "name": "T naive", "count": 2}]},
                {"id": "B1", "row": 0, "col": 1, "category": "mixed",
                 "stats": {"intensity": 0.5, "peak": 0.9, "variance": 0.2, "edges": 0.1},
                 "channels": {"GFP": {"mean": 0.4, "snr": 8.0}}},
            ],
        }
        server._client.get_capture = lambda uid, cid: {
            "capture": {"captureId": cid, "landscape": fat_landscape},
            "frame": "",
        }
        try:
            out = server.get_capture("NRUBxU", "cap-20260921T092735-bd47d6")
        finally:
            server._client.get_capture = original
        env = out[-1]           # the envelope block sits after the (absent) Image block
        ls = env["landscape"]
        self.assertEqual(["GFP", "TOM"], ls["channelNames"])
        self.assertEqual({"0": {"path": "/live/tnaive", "name": "T naive"}}, ls["popMap"])
        t0 = ls["tiles"][0]
        self.assertEqual("A1", t0["id"])
        self.assertNotIn("row", t0)                     # dropped: derivable from id
        self.assertNotIn("col", t0)
        self.assertNotIn("stats", t0)                   # dropped: superseded by channels
        self.assertEqual([0.5, 12.0, 0.1, 3.0], t0["channels"])  # positional [m, s, m, s]
        self.assertEqual(3, t0["segCount"])
        self.assertEqual([["0", 2]], t0["pops"])
        # Tile 2 has only GFP — the prelude has both, but the tile's array only carries GFP's
        # two numbers, positional to the tile's own channel-name order.
        t1 = ls["tiles"][1]
        self.assertEqual([0.4, 8.0], t1["channels"])
        self.assertNotIn("pops", t1)                    # sparsity carries through

    def test_get_capture_attaches_capture_path_when_api_sent_one(self):
        # Local dev path — Cecelia + Claude Code on the same box. The API includes
        # `capturePath` alongside `capture`/`frame`, and the tool threads it into the envelope
        # block so a reader can `Read(capturePath)` for the fat form when needed.
        original = server._client.get_capture
        fake_path = "/home/dominik/cecelia-feijoa/projects/NRUBxU/captures/cap-x/meta.json"
        server._client.get_capture = lambda uid, cid: {
            "capture": {"captureId": cid, "surface": "viewer_frame"},
            "frame": "",
            "capturePath": fake_path,
        }
        try:
            out = server.get_capture("NRUBxU", "cap-x")
        finally:
            server._client.get_capture = original
        env = out[-1]
        self.assertEqual(fake_path, env["capturePath"])

    def test_get_capture_omits_capture_path_when_api_did_not_send_one(self):
        # Cloud-VM path — the API is on a remote host with no shared filesystem, so the
        # `capturePath` field is absent from the API response. The tool must NOT invent one;
        # the reader should fall through to the slim landscape it already got.
        original = server._client.get_capture
        server._client.get_capture = lambda uid, cid: {
            "capture": {"captureId": cid, "surface": "viewer_frame"},
            "frame": "",
        }
        try:
            out = server.get_capture("NRUBxU", "cap-x")
        finally:
            server._client.get_capture = original
        env = out[-1]
        self.assertNotIn("capturePath", env)

    def test_get_capture_landscape_tiles_filters_by_id(self):
        # The drill-down tool returns a filtered subset in the same slim shape.
        original = server._client.get_capture
        server._client.get_capture = lambda uid, cid: {
            "capture": {"captureId": cid, "landscape": {
                "grid": {"cols": 2, "rows": 2}, "legend": [], "schemaVersion": 2,
                "tiles": [
                    {"id": "A1", "row": 0, "col": 0, "category": "dark", "segCount": 3},
                    {"id": "B1", "row": 0, "col": 1, "category": "mixed", "segCount": 5},
                ],
            }}, "frame": "",
        }
        try:
            out = server.get_capture_landscape_tiles(
                "NRUBxU", "cap-20260921T092735-bd47d6", tile_ids=["B1"])
        finally:
            server._client.get_capture = original
        self.assertEqual(1, len(out["landscape"]["tiles"]))
        self.assertEqual("B1", out["landscape"]["tiles"][0]["id"])

    def test_get_capture_landscape_tiles_filters_by_bbox(self):
        # bbox is frame-relative 0..1 coords; the tool converts to grid col/row and includes any
        # intersecting tile. Left half of a 2×2 grid → col 0 only → A1 + A2.
        original = server._client.get_capture
        server._client.get_capture = lambda uid, cid: {
            "capture": {"captureId": cid, "landscape": {
                "grid": {"cols": 2, "rows": 2}, "legend": [], "schemaVersion": 2,
                "tiles": [
                    {"id": "A1", "row": 0, "col": 0, "category": "dark"},
                    {"id": "B1", "row": 0, "col": 1, "category": "mixed"},
                    {"id": "A2", "row": 1, "col": 0, "category": "dark"},
                    {"id": "B2", "row": 1, "col": 1, "category": "mixed"},
                ],
            }}, "frame": "",
        }
        try:
            out = server.get_capture_landscape_tiles(
                "NRUBxU", "cap-20260921T092735-bd47d6", bbox=[0.0, 0.0, 0.45, 1.0])
        finally:
            server._client.get_capture = original
        ids = sorted(t["id"] for t in out["landscape"]["tiles"])
        self.assertEqual(["A1", "A2"], ids)

    def test_no_tool_can_start_work(self):
        # Claude designs, the user runs. No tool may launch a chain or submit a task — enforced by the
        # transport (launching is a WS message; this server speaks HTTP) but asserted here so adding
        # one is a deliberate act with a failing test in front of it.
        for forbidden in ("run_chain", "start_chain", "start_chain_run", "submit_task", "run_task",
                          "rename_chain", "delete_chain", "save_chain"):
            self.assertNotIn(forbidden, self.names)

    def test_list_projects_is_registered(self):
        # What makes "check my current project" resolvable without the user pasting a uid.
        self.assertIn("list_projects", self.names)

    def test_the_uid_lookup_is_reachable_before_a_project_is_known(self):
        # The other half of resolving a project: the user quotes an image/set uid and nothing says
        # which project it is in. That has to be findable from the ALWAYS-in-context instructions —
        # if it is only named in the briefing, the session has to have opened a project first, which
        # is exactly the state a bare uid does not have. (Sweeping list_images over every project is
        # what this replaces.)
        self.assertIn("find_object", self.names)
        self.assertIn("find_object", guidance.SERVER_INSTRUCTIONS)


class GuidanceTest(unittest.TestCase):
    """The guidance is the server's own prompt — it must stay in step with the tools and stay small.

    This replaces a cross-file check that used to compare two hand-synced prompts (the Julia one and a
    ~900-word TypeScript one the user pasted). The TS copy is gone; the knowledge lives in guidance.py,
    so the staleness guard belongs here, where both the tool registry and the constants are importable
    rather than pattern-matched out of a file. The in-app Julia prompt keeps its own half of the guard
    in app/test/suite.jl.
    """

    # The tools the guidance deliberately does NOT name: the observer's own autonomous-loop
    # bookkeeping, which a user-facing session never drives.
    UNMENTIONED = {"poll_observations", "set_observer_active", "get_observer_stats"}

    def setUp(self):
        self.names = {t.name for t in asyncio.run(server.mcp.list_tools())}
        self.text = guidance.SERVER_INSTRUCTIONS + guidance.BRIEFING_GUIDANCE

    def test_every_tool_is_named_in_the_guidance(self):
        # An unmentioned tool is an unused one — the assistant never offers the capability. This has
        # gone stale twice in the prompt era (create_chain, then get_analysis_boards /
        # get_image_attributes), both times noticed only because Dominik read the prompt himself.
        missing = sorted(t for t in self.names if t not in self.text and t not in self.UNMENTIONED)
        self.assertEqual([], missing, f"tools missing from guidance.py: {missing}")
        # …and the exemption list cannot silently grow to cover a real tool: every name on it must
        # still exist, so a rename shows up here instead of quietly widening the hole.
        self.assertTrue(self.UNMENTIONED <= self.names)

    def test_instructions_get_the_assistant_to_the_briefing(self):
        # The one job of the always-in-context half: resolve the project, then pull the briefing. Both
        # calls are what make a bare "check my project in cecelia" work.
        self.assertIn("list_projects", guidance.SERVER_INSTRUCTIONS)
        self.assertIn("get_session_briefing", guidance.SERVER_INSTRUCTIONS)
        # …and the boundary that must hold BEFORE any tool call: it cannot start work, and it must not
        # go off configuring an MCP server it cannot reach.
        self.assertIn("START", guidance.SERVER_INSTRUCTIONS)
        self.assertIn("configure", guidance.SERVER_INSTRUCTIONS)

    def test_instructions_stay_short(self):
        # These sit in the system prompt of EVERY session with this server registered — and the
        # observer is registered user-scope, so most of those sessions are not about Cecelia at all.
        # Anything that can wait for the briefing waits for the briefing. A budget, not a style rule.
        self.assertLess(len(guidance.SERVER_INSTRUCTIONS.split()), 300)

    def test_the_working_rules_are_in_the_briefing_half(self):
        # The disciplines that cost real mistakes when missing. They live in the briefing (paid for
        # only once a session opens a project), not in the instructions.
        for rule in ("not four replicates",     # grouping: 4 images from 1 mouse are not 4 replicates
                     "statUnit",                # pooling every track fakes the n
                     "presses Run",             # a chain is authored inert; the user runs it
                     "beside",                  # boards/chains are add-only, never edits
                     "included: false",         # an image the user already dropped is not news
                     "excludedCount",           # …and it is not part of the cohort denominator
                     "`fun`",                   # which task's QC is talking (a probe ≠ segmentation)
                     "shared capture cap-"):    # BIDIR PR #3: how Claude recognises a pushed frame
            self.assertIn(rule, guidance.BRIEFING_GUIDANCE)

    def test_the_briefing_ships_the_guidance(self):
        # Wiring, not text: the tool must actually merge `guidance` into its response, or all of the
        # above is a string nobody reads. Patched client — no server needed. The memory-slice
        # (Decision 5) also patches list_blackboard_entries / read_blackboard_entry /
        # get_recent_captures so the shape assertions below are on real merged output, not the
        # try/except degradation path.
        c = server._client
        originals = (c.get_session_briefing, c.list_blackboard_entries,
                     c.read_blackboard_entry, c.get_recent_captures)
        c.get_session_briefing = lambda uid: {"projectUid": uid, "flagged": [],
                                              "recentLabLog": [{"date": "2026-09-19",
                                                                "author": "User",
                                                                "summary": "old"}]}
        c.list_blackboard_entries = lambda uid: {"entries": [
            {"entryId": "profile", "title": "Project profile", "current": 0,
             "updatedAt": "2026-09-20", "attachmentsCount": 0, "status": "open"},
            {"entryId": "bb-20260919T000000-abcdef", "title": "Thread A",
             "current": 1, "updatedAt": "2026-09-19", "attachmentsCount": 0,
             "status": "open"},
            {"entryId": "bb-20260918T000000-fedcba", "title": "Thread B (resolved)",
             "current": 1, "updatedAt": "2026-09-18", "attachmentsCount": 0,
             "status": "resolved"},
        ]}
        # Profile shape matches what _ensure_profile_entry! seeds — D9 gate looks for a filled
        # Subject AND Goal section (heading + non-placeholder body). This one is filled.
        c.read_blackboard_entry = lambda uid, eid, version=None: {"entry": {
            "entryId": eid, "title": "Project profile",
            "content": ("# Project profile\n\n"
                        "## Subject\nMERTK KO/WT cohort, live intravital LN\n\n"
                        "## Goal\ntrack live CD169+ macrophages\n"),
            "current": 0, "updatedAt": "2026-09-20",
            "versions": [], "attachments": [], "status": "open",
        }}
        c.get_recent_captures = lambda uid, limit=None: {"items": [
            {"captureId": "cap-20260920T100000-aaaaaa", "createdAt": "2026-09-20T10:00:00",
             "surface": "viewer_frame", "address": {"projectUid": uid}},
            {"captureId": "cap-20260920T090000-bbbbbb", "createdAt": "2026-09-20T09:00:00",
             "surface": "plot", "address": {"projectUid": uid}},
        ]}
        try:
            out = server.get_session_briefing("NRUBxU")
        finally:
            (c.get_session_briefing, c.list_blackboard_entries,
             c.read_blackboard_entry, c.get_recent_captures) = originals
        self.assertEqual("NRUBxU", out["projectUid"])
        self.assertEqual(guidance.BRIEFING_GUIDANCE, out["guidance"])
        # Decision 5 — recentLabLog dropped from the default shape.
        self.assertNotIn("recentLabLog", out)
        # Profile body carried in full (short here; real cap is 100 KiB).
        self.assertIsInstance(out["profile"], dict)
        self.assertIn("MERTK", out["profile"]["content"])
        # Decision 9 — Subject AND Goal filled ⇒ newProject:false.
        self.assertFalse(out["newProject"])
        # Open entries: only status=open surface, and the profile itself is stripped out (it has
        # its own top-level field).
        titles = [e["title"] for e in out["openBlackboardEntries"]]
        self.assertIn("Thread A", titles)
        self.assertNotIn("Thread B (resolved)", titles)
        self.assertNotIn("Project profile", titles)
        # Recent captures relayed with the slim shape.
        self.assertEqual(2, len(out["recentCaptures"]))
        for c_row in out["recentCaptures"]:
            self.assertIn("captureId", c_row)
            self.assertIn("surface", c_row)


class ProfileAuthoredGateTest(unittest.TestCase):
    """PROJECT_MEMORY_PLAN Decision 9 — the section-content heuristic that flips `newProject`.

    Kept as a unit test on `_profile_is_authored` (not just on the briefing shape) because the
    placeholder markers live in TWO places: the Julia seed body (`_BB_PROFILE_PLACEHOLDER_BODY`
    in api/src/blackboard_api.jl) and this Python check. A drift between them makes newProject
    always fire, which turns the greet-first flow into a nag.
    """
    # The exact placeholder body the Julia writer seeds — copied here so a rename on the Julia
    # side breaks this test, not a real session.
    PLACEHOLDER = (
        "# Project profile\n\n"
        "## Subject\n"
        "_(a short description of what this data is — whose project, what tissue, what preparation)_\n\n"
        "## Goal\n"
        "_(what you're trying to answer with this project — the science question)_\n\n"
        "## Modality\n"
        "_(e.g. resonant intravital, spinning-disk fixed, light-sheet organoid)_\n"
    )

    def test_placeholder_body_is_not_authored(self):
        self.assertFalse(server._profile_is_authored({"content": self.PLACEHOLDER}))

    def test_missing_profile_is_not_authored(self):
        self.assertFalse(server._profile_is_authored(None))
        self.assertFalse(server._profile_is_authored({"content": ""}))

    def test_only_subject_filled_is_not_authored(self):
        body = ("## Subject\nMERTK cohort\n\n"
                "## Goal\n_(what you're trying to answer with this project)_\n")
        self.assertFalse(server._profile_is_authored({"content": body}))

    def test_both_filled_is_authored(self):
        body = ("## Subject\nMERTK cohort\n\n"
                "## Goal\nTrack CD169+ macrophages\n")
        self.assertTrue(server._profile_is_authored({"content": body}))

    def test_optional_section_filled_alone_is_not_authored(self):
        # Modality filled but Subject/Goal still placeholders ⇒ still newProject.
        body = ("## Subject\n_(a short description)_\n\n"
                "## Goal\n_(what you're trying to answer)_\n\n"
                "## Modality\nresonant intravital\n")
        self.assertFalse(server._profile_is_authored({"content": body}))


if __name__ == "__main__":
    unittest.main()
