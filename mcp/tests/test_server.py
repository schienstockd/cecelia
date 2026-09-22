"""Server-level checks: the FastMCP server imports cleanly and actually REGISTERS the expected tools.

Complements test_client.py (which covers the HTTP client in isolation). Importing `server` is
side-effect-free — the client is constructed lazily and `mcp.run()` only fires under `__main__` — so
this just asserts the wiring: e.g. `get_repl_api` is exposed as a tool, not merely defined. Needs
mcp/fastmcp in the env, so it runs under `pixi run test-mcp`.
"""
import asyncio
import unittest
import unittest.mock

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
            "get_region_clusters", "get_contact_stats",   # split of former get_spatial_stats
            "get_chains", "get_cohort_qc", "get_repl_api", "get_session_briefing",
            "get_recent_logs", "read_lab_log", "append_lab_log", "create_notebook",
            "set_notebook_description", "revise_notebook", "list_notebooks", "get_notebook",
            "create_chain",
            "mark_tracks", "mark_cells",   # bidir point-out (BIDIR_CONTEXT_PLAN PR #4)
            "point_at_ui", "mark_freeform", # bidir point-out UI + freeform (PR #5)
            "mark_tile", "get_landscape",   # bidir landscape overlay (PR #6, Decision 14 reframe)
            "mark_plot",                    # bidir plot point-out (PR #4b)
            "list_plots",                   # bidir PR #8 — live plot registry discovery
            "seek_viewer",                  # RUBBER_DUCK_FIT_PLAN P2 — imperative jump to (t, z) without a mark/capture
            "get_recent_captures", "get_capture",   # bidir share-in (BIDIR_CONTEXT_PLAN PR #3)
            "get_capture_landscape_tiles",          # landscape drill-down (LANDSCAPE Phase 6 follow-up)
            "get_object_ids",   # bidir follow-up: real cell/track ids for mark_cells / mark_tracks
            "register_push_target",   # bidir Part 5: explicit re-pair (auto-pair via middleware)
            "list_blackboard_entries", "read_blackboard_entry",   # bidir Part 4 (Blackboard) — reads
            "create_blackboard_entry", "revise_blackboard_entry", # bidir Part 4 (Blackboard) — writes
            "set_blackboard_status",                              # PROJECT_MEMORY_PLAN P1 — status flip (no snapshot)
            "set_blackboard_outcome",                             # PROJECT_MEMORY_PLAN P4 — outcome tag good|bad + required note
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

    def test_get_capture_never_exposes_capture_path(self):
        # The file-based escape hatch was dropped: slim + `get_capture_landscape_tiles` cover
        # every reader task, and a fat form re-introduces the truncation risk slim exists to
        # avoid. If the API ever ships `capturePath` again, the tool must not forward it.
        original = server._client.get_capture
        server._client.get_capture = lambda uid, cid: {
            "capture": {"captureId": cid, "surface": "viewer_frame"},
            "frame": "",
            "capturePath": "/should/not/leak.json",
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

    def test_spatial_split_returns_only_its_own_slice(self):
        # get_region_clusters and get_contact_stats share the /api/analysis/spatial route; each must
        # strip the OTHER key from every per-image entry, so an LLM asking about niches doesn't get
        # the pair matrix and vice versa. Envelope + headers pass through.
        original = server._client.get_spatial_stats
        server._client.get_spatial_stats = lambda p, i, s: {
            "projectUid": p,
            "images": [
                {"imageUid": "IMG1", "regionRuns": [{"valueName": "base", "suffix": "r1"}],
                 "contactStats": [{"suffix": "n1", "pairs": []}]},
                {"imageUid": "IMG2", "regionRuns": [], "contactStats": []},
            ],
        }
        try:
            regions = server.get_region_clusters("NRUBxU")
            contacts = server.get_contact_stats("NRUBxU")
        finally:
            server._client.get_spatial_stats = original
        self.assertEqual("NRUBxU", regions["projectUid"])
        for im in regions["images"]:
            self.assertIn("regionRuns", im)
            self.assertNotIn("contactStats", im)
        for im in contacts["images"]:
            self.assertIn("contactStats", im)
            self.assertNotIn("regionRuns", im)
        # header field (imageUid) preserved
        self.assertEqual("IMG1", regions["images"][0]["imageUid"])
        self.assertEqual("IMG1", contacts["images"][0]["imageUid"])

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
        # Three open entries + one resolved. Two of the open ones are untagged and one is
        # `bad`-tagged; Decision 12 requires the `bad` one to lead the briefing slice even though
        # it's the OLDEST by id — otherwise a future session proposes on top of a known-wrong
        # thread. `Thread C (untagged, oldest)` comes last of the open ones per DESC id.
        c.list_blackboard_entries = lambda uid: {"entries": [
            {"entryId": "profile", "title": "Project profile", "current": 0,
             "updatedAt": "2026-09-20", "attachmentsCount": 0, "status": "open"},
            {"entryId": "bb-20260919T000000-abcdef", "title": "Thread A (newest, untagged)",
             "current": 1, "updatedAt": "2026-09-19", "attachmentsCount": 0,
             "status": "open"},
            {"entryId": "bb-20260918T120000-1a2b3c", "title": "Thread B (bad-tagged, middle age)",
             "current": 1, "updatedAt": "2026-09-18", "attachmentsCount": 0,
             "status": "open",
             "outcome": {"verdict": "bad",
                         "note": "wrong scan mode — cellpose diameter picked for galvo",
                         "taggedAt": "2026-09-18T12:00:00"}},
            {"entryId": "bb-20260918T000000-4d5e6f", "title": "Thread C (untagged, oldest open)",
             "current": 1, "updatedAt": "2026-09-18", "attachmentsCount": 0,
             "status": "open"},
            {"entryId": "bb-20260917T000000-fedcba", "title": "Thread R (resolved)",
             "current": 1, "updatedAt": "2026-09-17", "attachmentsCount": 0,
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
        self.assertNotIn("Thread R (resolved)", titles)
        self.assertNotIn("Project profile", titles)
        # Decision 12 tiebreak — the `bad`-tagged entry must lead, even though newer untagged
        # entries exist. Untagged entries fall to the back in stable order.
        self.assertEqual(titles[0], "Thread B (bad-tagged, middle age)")
        self.assertIn("Thread A (newest, untagged)", titles)
        self.assertIn("Thread C (untagged, oldest open)", titles)
        # The `bad` row still carries its outcome dict downstream (Claude reads the note).
        bad_row = out["openBlackboardEntries"][0]
        self.assertEqual(bad_row["outcome"]["verdict"], "bad")
        self.assertIn("galvo", bad_row["outcome"]["note"])
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


class OutcomeRankTest(unittest.TestCase):
    """PROJECT_MEMORY_PLAN Decision 12 — the rank function that keys the tiebreak in the briefing
    slice and mirrors `_bb_outcome_rank` in `api/src/blackboard_api.jl`. Kept as its own testset
    so a drift between the Python and Julia rank keys shows up here instead of as a mysterious
    ordering flip in a real briefing."""
    def test_bad_wins(self):
        self.assertEqual(server._outcome_rank({"verdict": "bad", "note": "…"}), 0)
    def test_good_middle(self):
        self.assertEqual(server._outcome_rank({"verdict": "good", "note": "…"}), 1)
    def test_untagged_last(self):
        self.assertEqual(server._outcome_rank(None), 2)
        self.assertEqual(server._outcome_rank({}), 2)
    def test_string_verdict_accepted(self):
        # Some paths (registry mirror, list-endpoint absent-when-untagged) may hand the rank
        # a bare "good"/"bad" string rather than the full dict; both must resolve.
        self.assertEqual(server._outcome_rank("bad"), 0)
        self.assertEqual(server._outcome_rank("good"), 1)
        self.assertEqual(server._outcome_rank(""), 2)
    def test_stable_sort_between_ranks(self):
        rows = [
            {"id": "a", "outcome": None},
            {"id": "b", "outcome": {"verdict": "bad"}},
            {"id": "c", "outcome": None},
            {"id": "d", "outcome": {"verdict": "good"}},
            {"id": "e", "outcome": {"verdict": "bad"}},
        ]
        rows.sort(key=lambda r: server._outcome_rank(r.get("outcome")))
        # bad rows first (in original order: b, e), then good (d), then untagged (a, c).
        self.assertEqual([r["id"] for r in rows], ["b", "e", "d", "a", "c"])


class StainClassifierTest(unittest.TestCase):
    """PROJECT_MEMORY_PLAN Phase 5.1 — channel-name → stain-class classifier used by the entry
    fingerprint. Pinned so a class rename or a new pattern surfaces here rather than as a silent
    change in what gets banked into `meta.fingerprint.stain_classes`. The full contract lives in
    `docs/inventory/fingerprint_extractors.md`; update both in the same change."""

    def test_membrane_prefix(self):
        self.assertEqual(server._classify_stain("mem-TOM"), "membrane")
        self.assertEqual(server._classify_stain("mem_gfp"), "membrane")

    def test_nucleus_prefix_and_dyes(self):
        self.assertEqual(server._classify_stain("nuc-GFP"), "nucleus")
        self.assertEqual(server._classify_stain("DAPI"), "nucleus")
        self.assertEqual(server._classify_stain("Hoechst-33342"), "nucleus")

    def test_cd_marker(self):
        # Ailsa's CD169-Kat lands as macrophage_or_marker (coarse immune-marker bucket).
        self.assertEqual(server._classify_stain("CD169-Kat"), "macrophage_or_marker")
        self.assertEqual(server._classify_stain("CD8"), "macrophage_or_marker")

    def test_reporter_bare(self):
        # Bare reporter, no organelle prefix, lands as "reporter" — not membrane / nucleus.
        self.assertEqual(server._classify_stain("GFP"), "reporter")
        self.assertEqual(server._classify_stain("RFP"), "reporter")

    def test_structural_and_autofl(self):
        self.assertEqual(server._classify_stain("SHG"), "structural")
        self.assertEqual(server._classify_stain("autofluorescence"), "autofluorescence")

    def test_unknown_is_signal_not_bug(self):
        # A channel name the map hasn't seen resolves to "unknown". That's a valid fingerprint
        # value — the retrieval side reads it as "no signal on this dimension", not as an error.
        self.assertEqual(server._classify_stain("random-probe-42"), "unknown")
        self.assertEqual(server._classify_stain(""), "unknown")
        self.assertEqual(server._classify_stain(None), "unknown")


class ModalityClassifierTest(unittest.TestCase):
    """PROJECT_MEMORY_PLAN Phase 5.2 — filename → modality classifier used by v2 fingerprints.
    Pinned so a new lab's convention (or a regex regression) surfaces here rather than as a
    silent shift in what gets banked. The human contract is
    `docs/inventory/fingerprint_extractors.md`."""

    def test_resonant_convention_hits_2p(self):
        # Ailsa's `-res_` convention — the specific marker P5.0 audit chose.
        self.assertEqual(server._classify_modality(
            {"oriPath": "/data/M2b-MERTK_KAT-SWHL-GFP-Tom-res_0001.tif"}), "2p")

    def test_direct_two_photon_callouts(self):
        for path in ("cell_2p.ome.tif", "sample_two-photon.tif", "MULTIPHOTON_stack.tif"):
            self.assertEqual(server._classify_modality({"oriPath": path}), "2p",
                             msg=f"expected 2p for {path}")

    def test_confocal(self):
        self.assertEqual(server._classify_modality({"oriPath": "confocal_stack.tif"}), "confocal")
        self.assertEqual(server._classify_modality({"oriPath": "cell_conf_01.tif"}), "confocal")

    def test_spinning_disk_and_lightsheet(self):
        self.assertEqual(server._classify_modality({"oriPath": "s-CSU-001.tif"}), "spinning_disk")
        self.assertEqual(server._classify_modality({"oriPath": "spinning-disk_007.tif"}),
                         "spinning_disk")
        self.assertEqual(server._classify_modality({"oriPath": "lightsheet_e11.tif"}), "lightsheet")
        self.assertEqual(server._classify_modality({"oriPath": "spim-sample.tif"}), "lightsheet")

    def test_widefield(self):
        self.assertEqual(server._classify_modality({"oriPath": "sample_widefield.tif"}), "widefield")

    def test_unknown_when_nothing_matches(self):
        # A filename with no modality tokens returns "unknown" — retrieval treats this as
        # "no signal on this dimension" and the fingerprint OMITS the modality field.
        self.assertEqual(server._classify_modality({"oriPath": "sample_001.tif"}), "unknown")

    def test_fallback_to_name(self):
        # oriPath missing → fall back to the display name. This is what happens on a legacy
        # image without an oriPath recorded (never blocks the fingerprint).
        self.assertEqual(server._classify_modality({"oriPath": None, "name": "M2b-res_0001"}),
                         "2p")


class TissueContextTest(unittest.TestCase):
    """PROJECT_MEMORY_PLAN Phase 5.2 — profile-prose → tissue-context parse. Reads the profile's
    Subject section only; the seeded placeholder counts as no content (matches
    `_profile_section_has_content` discipline)."""

    def _profile(self, subject: str) -> str:
        return f"# Project profile\n\n## Subject\n{subject}\n\n## Goal\nsomething\n"

    def test_germinal_centre_both_spellings(self):
        self.assertEqual(server._infer_tissue_context(
            self._profile("intravital 2P of germinal centres in MERTK mice")), "germinal_centre")
        self.assertEqual(server._infer_tissue_context(
            self._profile("germinal center reaction, GC macrophages")), "germinal_centre")

    def test_common_tissues(self):
        for subject, expected in [
            ("Ailsa's spleen flowcyto sort", "spleen"),
            ("bone marrow explants",         "bone_marrow"),
            ("kidney intravital",            "kidney"),
            ("lung alveolar imaging",        "lung"),
            ("brain cortex slice",           "brain"),
            ("gut intestinal Peyer's patch", "gut"),
            ("skin dermis whole mount",      "skin"),
            ("lymph node draining",          "lymph_node"),
        ]:
            self.assertEqual(server._infer_tissue_context(self._profile(subject)), expected,
                             msg=f"subject={subject!r}")

    def test_unauthored_profile_is_none(self):
        # Seeded placeholder text counts as no content — return None so the fingerprint field is
        # left absent (a fresh project isn't wrongly stamped as "unknown tissue").
        placeholder = ("# Project profile\n\n## Subject\n"
                       "_(a short description of what this data is)_\n\n## Goal\n"
                       "_(what you're trying to answer)_\n")
        self.assertIsNone(server._infer_tissue_context(placeholder))
        self.assertIsNone(server._infer_tissue_context(""))
        self.assertIsNone(server._infer_tissue_context(None))

    def test_authored_but_off_vocab_is_unknown(self):
        # A subject that IS filled in but doesn't mention a vocabulary tissue lands as "unknown"
        # — signal, not error. Distinguished from an unauthored profile which returns None.
        self.assertEqual(server._infer_tissue_context(
            self._profile("Ailsa's very-specific niche experiment")), "unknown")


class InferFingerprintTest(unittest.TestCase):
    """PROJECT_MEMORY_PLAN Phase 5.1/5.2 — `_infer_fingerprint` snapshots the active image's
    context into the schema banked with a new Blackboard entry. Guards checked here: unresolvable
    image ⇒ None (best-effort, entry still creates), stain classes de-duped + sorted, modality +
    tissue absent when they resolve to "unknown", version stamped."""

    def _patch(self, image_meta, profile_content: str | None = ""):
        # `_infer_fingerprint` reads through `_client.get_image_meta` AND (v2)
        # `_client.read_blackboard_entry` for the profile parse. Both are stubbed here so the
        # tests stay pure-logic.
        image_patch = unittest.mock.patch.object(
            server._client, "get_image_meta",
            return_value={"image": image_meta})
        if profile_content is None:
            profile_patch = unittest.mock.patch.object(
                server._client, "read_blackboard_entry",
                side_effect=RuntimeError("no profile"))
        else:
            profile_patch = unittest.mock.patch.object(
                server._client, "read_blackboard_entry",
                return_value={"entry": {"content": profile_content}})
        return image_patch, profile_patch

    def test_no_image_uid_is_none(self):
        self.assertIsNone(server._infer_fingerprint("proj", None))
        self.assertIsNone(server._infer_fingerprint("proj", ""))

    def test_full_snapshot_v2(self):
        image_patch, profile_patch = self._patch({
            "sizeC":           4,
            "activeValueName": "denoised",
            "channelNames":    ["mem-TOM", "nuc-GFP", "CD169-Kat", "unknown-probe"],
            "oriPath":         "/data/M2b-MERTK_KAT-SWHL-GFP-Tom-res_0001.tif",
        }, profile_content=(
            "# Project profile\n\n## Subject\n"
            "intravital 2P of germinal centres in MERTK mice\n\n## Goal\nx\n"))
        with image_patch, profile_patch:
            fp = server._infer_fingerprint("proj", "img1")
        self.assertEqual(fp["v"], server._BB_FINGERPRINT_VERSION)
        self.assertEqual(fp["channel_count"], 4)
        self.assertEqual(fp["pipeline_stage"], "denoised")
        self.assertEqual(fp["stain_classes"],
                         ["macrophage_or_marker", "membrane", "nucleus", "unknown"])
        # v2 fields — inferred from oriPath + profile Subject
        self.assertEqual(fp["modality"], "2p")
        self.assertEqual(fp["tissue_context"], "germinal_centre")

    def test_modality_and_tissue_absent_when_unknown(self):
        # A filename with no modality tokens AND a profile with no vocab tissue → both v2 fields
        # OMITTED from the fingerprint (retrieval reads absence as "no signal on this dimension",
        # NOT as a bucket key "unknown|unknown|…").
        image_patch, profile_patch = self._patch({
            "sizeC": 2, "activeValueName": "default",
            "channelNames": ["c1", "c2"],
            "oriPath": "/data/sample_001.tif",
        }, profile_content="# Project profile\n\n## Subject\n_(a placeholder)_\n")
        with image_patch, profile_patch:
            fp = server._infer_fingerprint("proj", "img1")
        self.assertNotIn("modality", fp)
        self.assertNotIn("tissue_context", fp)

    def test_missing_image_fields_pass_through(self):
        image_patch, profile_patch = self._patch({
            "sizeC": None, "activeValueName": "", "channelNames": [], "oriPath": "",
        }, profile_content="")
        with image_patch, profile_patch:
            fp = server._infer_fingerprint("proj", "img1")
        self.assertEqual(fp, {"v": server._BB_FINGERPRINT_VERSION})

    def test_profile_read_failure_does_not_break_fingerprint(self):
        # Profile fetch throws → tissue absent, other fields still populated. Best-effort: the
        # entry still creates with the image-derived context.
        image_patch, profile_patch = self._patch({
            "sizeC": 4, "activeValueName": "denoised",
            "channelNames": ["mem-TOM"],
            "oriPath": "res_stack.tif",
        }, profile_content=None)
        with image_patch, profile_patch:
            fp = server._infer_fingerprint("proj", "img1")
        self.assertEqual(fp["modality"], "2p")
        self.assertNotIn("tissue_context", fp)

    def test_backend_failure_is_none(self):
        with unittest.mock.patch.object(server._client, "get_image_meta",
                                         side_effect=RuntimeError("backend down")):
            self.assertIsNone(server._infer_fingerprint("proj", "img1"))


class FingerprintBucketKeyTest(unittest.TestCase):
    """PROJECT_MEMORY_PLAN Phase 5.2 — canonical bucket key. Two fingerprints share a bucket iff
    every dimension we key on matches. Missing fields on either side become "*", so a v1 entry
    (no modality) buckets with a v2 entry whose modality resolved to "unknown"."""

    def test_same_fingerprint_same_key(self):
        fp = {"v": 2, "channel_count": 4, "stain_classes": ["membrane", "nucleus"],
              "pipeline_stage": "denoised", "modality": "2p", "tissue_context": "germinal_centre"}
        self.assertEqual(server._fingerprint_bucket_key(fp),
                         server._fingerprint_bucket_key(dict(fp)))

    def test_stain_class_order_matters_for_the_key(self):
        # The stain_classes field is stored SORTED by _infer_fingerprint, so a bucket-key mismatch
        # from a reversed list means a bug on the writer side. Pinned so a future ordering change
        # surfaces here.
        fp_sorted   = {"v": 2, "stain_classes": ["membrane", "nucleus"]}
        fp_reversed = {"v": 2, "stain_classes": ["nucleus", "membrane"]}
        self.assertNotEqual(server._fingerprint_bucket_key(fp_sorted),
                            server._fingerprint_bucket_key(fp_reversed))

    def test_v1_and_v2_unknown_fall_into_same_bucket(self):
        # A v1 entry has no `modality` field; a v2 entry that resolved to "unknown" ALSO has no
        # modality field (dropped by _infer_fingerprint). Both should bucket together — the
        # retrieval-side benefit of "unknown = absent".
        v1 = {"v": 1, "channel_count": 4, "stain_classes": ["membrane"],
              "pipeline_stage": "denoised"}
        v2 = {"v": 2, "channel_count": 4, "stain_classes": ["membrane"],
              "pipeline_stage": "denoised"}
        self.assertEqual(server._fingerprint_bucket_key(v1),
                         server._fingerprint_bucket_key(v2))

    def test_different_pipeline_stage_different_bucket(self):
        # The core invariant: same context but different pipeline stage = different failure mode.
        fp_a = {"v": 2, "pipeline_stage": "denoised"}
        fp_b = {"v": 2, "pipeline_stage": "flowTom"}
        self.assertNotEqual(server._fingerprint_bucket_key(fp_a),
                            server._fingerprint_bucket_key(fp_b))


class MineGuardrailsTest(unittest.TestCase):
    """PROJECT_MEMORY_PLAN Phase 5.2 — group bad-tagged entries by fingerprint bucket; only
    clusters at ≥ N=3 members surface. Below-threshold buckets stay silent so the briefing
    doesn't cry wolf on a small corpus."""

    def _entry(self, entry_id: str, verdict: str | None, fp: dict | None,
               note: str = "why", tagged_at: str = "2026-09-21T10:00:00") -> dict:
        e = {"entryId": entry_id, "title": entry_id}
        if verdict is not None:
            e["outcome"] = {"verdict": verdict, "note": note, "taggedAt": tagged_at}
        if fp is not None:
            e["fingerprint"] = fp
        return e

    def test_empty_corpus_no_guardrails(self):
        self.assertEqual(server._mine_guardrails([]), [])

    def test_below_threshold_is_silent(self):
        fp = {"v": 2, "channel_count": 4, "pipeline_stage": "flowTom"}
        entries = [
            self._entry("bb-1", "bad", fp),
            self._entry("bb-2", "bad", fp),
        ]
        # 2 < 3 → silent. The plan's D5 says "one is coincidence, two is a hint, three is a
        # pattern"; below-threshold clusters stay off the briefing.
        self.assertEqual(server._mine_guardrails(entries), [])

    def test_at_threshold_surfaces(self):
        fp = {"v": 2, "channel_count": 4, "pipeline_stage": "flowTom",
              "stain_classes": ["membrane", "nucleus"], "modality": "2p"}
        entries = [
            self._entry("bb-1", "bad", fp, note="cellpose diameter too big"),
            self._entry("bb-2", "bad", fp, note="cellpose diameter too small"),
            self._entry("bb-3", "bad", fp, note="split failed on merged objects"),
        ]
        out = server._mine_guardrails(entries)
        self.assertEqual(len(out), 1)
        cluster = out[0]
        self.assertEqual(cluster["count"], 3)
        self.assertEqual(cluster["fingerprint"], fp)
        self.assertEqual({e["entryId"] for e in cluster["entries"]}, {"bb-1", "bb-2", "bb-3"})

    def test_good_and_untagged_dont_count(self):
        # Guardrails are the failure surface. `good`-tagged and untagged entries share the same
        # bucket key but must NOT contribute to the threshold — otherwise a well-behaved topic
        # would fire a spurious guardrail.
        fp = {"v": 2, "pipeline_stage": "flowTom"}
        entries = [
            self._entry("bb-1", "bad",  fp),
            self._entry("bb-2", "good", fp),
            self._entry("bb-3", None,   fp),
        ]
        self.assertEqual(server._mine_guardrails(entries), [])

    def test_entries_without_fingerprint_ignored(self):
        # Legacy pre-P5.1 (or entries created without image_uid) have no fingerprint. Skip them —
        # nothing to bucket against; a note that isn't keyable can't be a guardrail.
        entries = [
            self._entry("bb-1", "bad", None),
            self._entry("bb-2", "bad", None),
            self._entry("bb-3", "bad", None),
        ]
        self.assertEqual(server._mine_guardrails(entries), [])

    def test_different_buckets_reported_separately(self):
        fp_a = {"v": 2, "pipeline_stage": "flowTom", "channel_count": 4}
        fp_b = {"v": 2, "pipeline_stage": "denoised", "channel_count": 4}
        entries = [
            self._entry(f"bb-a{i}", "bad", fp_a) for i in range(3)
        ] + [
            self._entry(f"bb-b{i}", "bad", fp_b) for i in range(3)
        ]
        out = server._mine_guardrails(entries)
        self.assertEqual(len(out), 2)
        # Both clusters at threshold; each carries its own fingerprint representative.
        stages = {c["fingerprint"]["pipeline_stage"] for c in out}
        self.assertEqual(stages, {"flowTom", "denoised"})

    def test_newest_first_within_cluster(self):
        fp = {"v": 2, "pipeline_stage": "flowTom"}
        entries = [
            self._entry("bb-old", "bad", fp, tagged_at="2026-09-01T10:00:00"),
            self._entry("bb-mid", "bad", fp, tagged_at="2026-09-10T10:00:00"),
            self._entry("bb-new", "bad", fp, tagged_at="2026-09-20T10:00:00"),
        ]
        out = server._mine_guardrails(entries)
        self.assertEqual([e["entryId"] for e in out[0]["entries"]],
                         ["bb-new", "bb-mid", "bb-old"])


class BriefingSliceGuardrailsTest(unittest.TestCase):
    """The `_memory_briefing_slice` surfaces `guardrails` in addition to the P4 fields. Wired here
    so a future refactor of the slice doesn't drop the field silently."""

    def test_briefing_carries_guardrails_key(self):
        fp = {"v": 2, "pipeline_stage": "flowTom", "channel_count": 4}
        list_return = {"entries": [
            {"entryId": f"bb-{i}", "title": f"t{i}", "current": 0, "updatedAt": "x",
             "attachmentsCount": 0, "status": "open",
             "outcome":     {"verdict": "bad", "note": "n", "taggedAt": f"2026-09-2{i}"},
             "fingerprint": fp} for i in range(3)
        ]}
        with unittest.mock.patch.object(server._client, "list_blackboard_entries",
                                         return_value=list_return), \
             unittest.mock.patch.object(server._client, "read_blackboard_entry",
                                         return_value={"entry": {"content": ""}}), \
             unittest.mock.patch.object(server._client, "get_recent_captures",
                                         return_value={"items": []}):
            slc = server._memory_briefing_slice("proj")
        self.assertIn("guardrails", slc)
        self.assertEqual(len(slc["guardrails"]), 1)
        self.assertEqual(slc["guardrails"][0]["count"], 3)


if __name__ == "__main__":
    unittest.main()
