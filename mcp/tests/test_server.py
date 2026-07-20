"""Server-level checks: the FastMCP server imports cleanly and actually REGISTERS the expected tools.

Complements test_client.py (which covers the HTTP client in isolation). Importing `server` is
side-effect-free — the client is constructed lazily and `mcp.run()` only fires under `__main__` — so
this just asserts the wiring: e.g. `get_repl_api` is exposed as a tool, not merely defined. Needs
mcp/fastmcp in the env, so it runs under `pixi run test-mcp`.
"""
import asyncio
import unittest

from cecelia_mcp import server


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
            "set_notebook_description", "list_notebooks", "get_notebook",
        ):
            self.assertIn(tool, self.names)


if __name__ == "__main__":
    unittest.main()
