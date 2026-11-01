"""The figure toolset's client (figure_tools.py, docs/todo/FIGURES_PLAN.md P3): what it sends and what it
reads back. The server's own-only rule is tested in the API suite."""
import base64
import unittest

from cecelia_mcp import figure_tools as ft


class FakeClient(ft.FigureClient):
    def __init__(self, replies):
        self.replies, self.calls = replies, []

    def _request(self, method, path, params=None, body=None):
        self.calls.append((method, path, params, body))
        return self.replies[(method, path, (params or {}).get("name"))]


CLAUDE = {"via": "claude"}
USER = {"via": "app"}


class FigureClientTest(unittest.TestCase):
    def test_render_sends_the_spec_and_splits_the_picture(self):
        png = b"\x89PNG..."
        c = FakeClient({("POST", ft.RENDER_ROUTE, None): {"png": base64.b64encode(png).decode(), "name": "a.png"}})
        got, meta = c.render_figure("P", "I", {"sizePx": 90}, "a", replace=False, dry_run=True)
        self.assertEqual((got, meta), (png, {"name": "a.png"}))
        body = c.calls[0][3]
        self.assertEqual((body["spec"], body["dryRun"], body["replace"]), ({"sizePx": 90}, True, False))

    def test_list_keeps_figures_only_and_says_which_are_claudes(self):
        c = FakeClient({
            ("GET", ft.LIST_ROUTE, None): {"movies": [
                {"name": "m.mp4", "kind": "movie"},
                {"name": "a.png", "kind": "figure", "imageUid": "I", "claudeMade": True},
                {"name": "b.png", "kind": "figure", "imageUid": "I", "claudeMade": False, "figureStale": True,
                 "staleReason": "image version denoised was re-processed since"}]},
            ("GET", ft.META_ROUTE, "a.png"): {"entry": {"createdBy": CLAUDE, "config": {"spec": {"z": 9}}}},
            ("GET", ft.META_ROUTE, "b.png"): {"entry": {"createdBy": CLAUDE, "updatedBy": USER}},
        })
        rows = c.list_figures("P")
        self.assertEqual([(r["name"], r["mine"], r["spec"]) for r in rows],
                         [("a.png", True, {"z": 9}), ("b.png", False, None)])   # the server's claudeMade, as is
        self.assertEqual([r["stale"] for r in rows], [None, "image version denoised was re-processed since"])

    def test_routes_are_the_allow_listed_ones(self):
        self.assertIn(("POST", ft.RENDER_ROUTE), ft.FIGURE_ROUTES)
        self.assertIn(("POST", ft.DELETE_ROUTE), ft.FIGURE_ROUTES)
        self.assertFalse(any(p == "/api/movies/delete" for _, p in ft.FIGURE_ROUTES))   # the GUI's delete


if __name__ == "__main__":
    unittest.main()
