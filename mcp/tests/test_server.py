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
            "get_project_info", "list_images", "get_task_history",
            "get_module_params", "get_available_plots", "get_analysis_lineage", "get_populations",
            "get_measure_summary", "get_behaviour_summary", "get_cluster_summary",
            "get_chains", "get_cohort_qc", "get_repl_api", "get_session_briefing",
            "get_recent_logs", "read_lab_log", "append_lab_log", "create_notebook",
            "set_notebook_description", "revise_notebook", "list_notebooks", "get_notebook",
            "create_chain",
        ):
            self.assertIn(tool, self.names)

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
                     "`fun`"):                  # which task's QC is talking (a probe ≠ segmentation)
            self.assertIn(rule, guidance.BRIEFING_GUIDANCE)

    def test_the_briefing_ships_the_guidance(self):
        # Wiring, not text: the tool must actually merge `guidance` into its response, or all of the
        # above is a string nobody reads. Patched client — no server needed.
        original = server._client.get_session_briefing
        server._client.get_session_briefing = lambda uid: {"projectUid": uid, "flagged": []}
        try:
            out = server.get_session_briefing("NRUBxU")
        finally:
            server._client.get_session_briefing = original
        self.assertEqual("NRUBxU", out["projectUid"])
        self.assertEqual(guidance.BRIEFING_GUIDANCE, out["guidance"])


if __name__ == "__main__":
    unittest.main()
