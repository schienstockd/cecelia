"""Unit tests for the mechanical inventory check — `python/cecelia/effectiveness/inventory_coverage.py`."""
import json
import os
import pathlib
import tempfile
import unittest
from unittest import mock

from cecelia.effectiveness.inventory_coverage import (
    area_doc_for,
    find_undocumented_routes,
    find_uninventoried,
    format_section,
    is_named,
    is_route_documented,
    new_files_from_diff,
    new_routes_from_diff,
    read_inventory_text,
    run_inventory_check,
)


def _new_file_diff(*paths: str) -> str:
    """A staged diff that adds each path — the shape `git diff --staged` prints."""
    return "".join(
        f"diff --git a/{p} b/{p}\nnew file mode 100644\nindex 0000000..e69de29\n"
        f"--- /dev/null\n+++ b/{p}\n@@ -0,0 +1 @@\n+x\n"
        for p in paths
    )


_MODIFIED = (
    "diff --git a/frontend/src/utils/old.ts b/frontend/src/utils/old.ts\n"
    "index 1111111..2222222 100644\n--- a/frontend/src/utils/old.ts\n"
    "+++ b/frontend/src/utils/old.ts\n@@ -1 +1 @@\n-a\n+b\n"
)


class NewFilesFromDiffTest(unittest.TestCase):
    def test_only_added_files(self):
        diff = _MODIFIED + _new_file_diff("frontend/src/utils/fresh.ts")
        self.assertEqual(new_files_from_diff(diff), ["frontend/src/utils/fresh.ts"])

    def test_modified_file_is_not_new(self):
        # The lazy scan must stop at the next `diff --git` header, or a modified file followed
        # by an added one would be reported as new.
        self.assertEqual(new_files_from_diff(_MODIFIED), [])


class AreaDocForTest(unittest.TestCase):
    def test_shared_roots_map_to_their_area_file(self):
        self.assertEqual(area_doc_for("frontend/src/utils/x.ts"), "docs/inventory/FRONTEND.md")
        self.assertEqual(area_doc_for("frontend/src/components/X.vue"), "docs/inventory/FRONTEND.md")
        self.assertEqual(area_doc_for("app/src/gating/x.jl"), "docs/inventory/JULIA_APP.md")
        self.assertEqual(area_doc_for("api/src/x_api.jl"), "docs/inventory/JULIA_API.md")
        self.assertEqual(area_doc_for("python/cecelia/utils/x.py"), "docs/inventory/PYTHON.md")
        self.assertEqual(area_doc_for("mcp/cecelia_mcp/x.py"), "docs/inventory/MCP.md")

    def test_not_shared(self):
        for path in (
            "frontend/src/utils/x.test.ts",        # test
            "python/cecelia/tests/test_x.py",      # test
            "python/cecelia/utils/__init__.py",    # package marker
            "frontend/src/types/x.d.ts",           # type stub (and not a shared root)
            "app/src/tasks/segment/x.jl",          # leaf task file
            "frontend/src/modules/X/X.vue",        # module page — leaf
            "frontend/src/utils/x.json",           # not code
            "README.md",
        ):
            self.assertIsNone(area_doc_for(path), path)


class IsNamedTest(unittest.TestCase):
    def test_stem_with_or_without_extension_or_path(self):
        for text in ("`panelResize.ts` — …", "`panelResize` — …", "`utils/panelResize.ts`"):
            self.assertTrue(is_named("frontend/src/utils/panelResize.ts", text), text)

    def test_whole_word_only(self):
        # `resize` inside `panelResize` / `resize-observer` is not a mention of `resize.ts`.
        self.assertFalse(is_named("frontend/src/utils/resize.ts", "`panelResize` and resize-observer"))


class FindUninventoriedTest(unittest.TestCase):
    def test_flags_only_unnamed_shared_files(self):
        diff = _new_file_diff(
            "frontend/src/utils/named.ts",
            "frontend/src/utils/unnamed.ts",
            "frontend/src/utils/unnamed.test.ts",
        )
        shared, missing = find_uninventoried(diff, "- **Thing**: `frontend/src/utils/named.ts`")
        self.assertEqual(shared, ["frontend/src/utils/named.ts", "frontend/src/utils/unnamed.ts"])
        self.assertEqual(missing, [("frontend/src/utils/unnamed.ts", "docs/inventory/FRONTEND.md")])


_ROUTE_DIFF = (
    "diff --git a/api/src/server.jl b/api/src/server.jl\n@@ -224,2 +224,4 @@\n"
    '     "/api/health" => (req, body_bytes) -> (200, ""),\n'
    '+    "/api/x/new" => (req, body_bytes) -> (api_x_new(req)),\n'
    '+    "/api/x/documented" => (req, body_bytes) -> (api_x_doc(req)),\n'
    '-    "/api/x/gone" => (req, body_bytes) -> (api_x_gone(req)),\n'
)


class RoutesTest(unittest.TestCase):
    def test_only_added_routes(self):
        # Context (` `) and removed (`-`) lines are not new routes.
        self.assertEqual(new_routes_from_diff(_ROUTE_DIFF), ["/api/x/new", "/api/x/documented"])

    def test_documented_means_backticked_exact_path(self):
        doc = "| GET | `/api/x/documented?projectUid` | … |\n| GET | `/api/y` | … |"
        self.assertTrue(is_route_documented("/api/x/documented", doc))
        self.assertTrue(is_route_documented("/api/y", doc))
        # A parent path is not its child, and the child is not its parent.
        self.assertFalse(is_route_documented("/api/x", doc))
        self.assertFalse(is_route_documented("/api/y/sub", doc))

    def test_brace_shorthand_counts(self):
        doc = "| POST | `/api/images/{register,labels/delete}` · `/api/sets/{create, rename}?projectUid` |"
        for route in ("/api/images/register", "/api/images/labels/delete", "/api/sets/rename"):
            self.assertTrue(is_route_documented(route, doc), route)
        self.assertFalse(is_route_documented("/api/images/move", doc))

    def test_flags_undocumented(self):
        routes, missing = find_undocumented_routes(_ROUTE_DIFF, "`/api/x/documented`")
        self.assertEqual(routes, ["/api/x/new", "/api/x/documented"])
        self.assertEqual(missing, ["/api/x/new"])


class ExemptMarkerTest(unittest.TestCase):
    def test_marker_with_reason_silences_the_file(self):
        exempt_diff = (
            "diff --git a/frontend/src/utils/oneOff.ts b/frontend/src/utils/oneOff.ts\n"
            "new file mode 100644\n--- /dev/null\n+++ b/frontend/src/utils/oneOff.ts\n@@ -0,0 +1,2 @@\n"
            "+// INVENTORY-EXEMPT: only SelectionTable.vue uses it\n+export const x = 1\n"
        )
        diff = exempt_diff + _new_file_diff("frontend/src/utils/other.ts")
        shared, missing = find_uninventoried(diff, "")
        self.assertEqual(shared, ["frontend/src/utils/other.ts"])
        self.assertEqual([f for f, _ in missing], ["frontend/src/utils/other.ts"])

    def test_marker_without_reason_does_not_count(self):
        diff = (
            "diff --git a/frontend/src/utils/oneOff.ts b/frontend/src/utils/oneOff.ts\n"
            "new file mode 100644\n--- /dev/null\n+++ b/frontend/src/utils/oneOff.ts\n@@ -0,0 +1 @@\n"
            "+// INVENTORY-EXEMPT:\n"
        )
        self.assertEqual(len(find_uninventoried(diff, "")[1]), 1)


class FormatSectionTest(unittest.TestCase):
    def test_skipped_when_nothing_new(self):
        self.assertEqual(format_section([], shared_count=0),
                         "_Inventory check: skipped — no new shared files or routes_")

    def test_clean_run(self):
        self.assertEqual(format_section([], shared_count=2, route_count=1),
                         "_Inventory check: run — every new shared file and route is documented_")

    def test_route_warning_points_at_route_index(self):
        out = format_section([], shared_count=0, missing_routes=["/api/x/new"], route_count=1)
        self.assertIn("`/api/x/new`", out)
        self.assertIn("`docs/API.md` → *Route index*", out)
        self.assertTrue(out.endswith("_Inventory check: run_"))

    def test_warning_names_file_and_area_doc(self):
        out = format_section([("api/src/x_api.jl", "docs/inventory/JULIA_API.md")], shared_count=1)
        self.assertIn("_Inventory check (evidence):_", out)
        self.assertIn("`api/src/x_api.jl`", out)
        self.assertIn("`docs/inventory/JULIA_API.md`", out)
        self.assertTrue(out.endswith("_Inventory check: run_"))


class ReadInventoryTextTest(unittest.TestCase):
    def test_reads_area_files_index_and_primitives(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = pathlib.Path(tmp)
            for rel, text in (("docs/inventory/FRONTEND.md", "alpha"), ("INVENTORY.md", "beta"),
                              ("docs/ui/PRIMITIVES.md", "gamma"), ("docs/UI.md", "delta")):
                (root / rel).parent.mkdir(parents=True, exist_ok=True)
                (root / rel).write_text(text, encoding="utf-8")
            text = read_inventory_text(root)
        for word in ("alpha", "beta", "gamma"):
            self.assertIn(word, text)
        self.assertNotIn("delta", text)  # a non-inventory doc doesn't count as inventoried


class RunInventoryCheckTest(unittest.TestCase):
    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.log_path = pathlib.Path(self._tmp.name) / "events.jsonl"
        patch = mock.patch.dict(os.environ, {"CECELIA_EFFECTIVENESS_LOG": str(self.log_path)})
        patch.start()
        self.addCleanup(patch.stop)

    def _events(self):
        return [json.loads(line) for line in self.log_path.read_text(encoding="utf-8").splitlines()]

    def test_emits_one_event_with_the_warned_files(self):
        section = run_inventory_check(
            _new_file_diff("frontend/src/utils/a.ts", "frontend/src/utils/b.ts") + _ROUTE_DIFF,
            inventory_text="`a.ts`", api_doc_text="`/api/x/documented`", branch="feat/x",
        )
        self.assertIn("`frontend/src/utils/b.ts`", section)
        [event] = self._events()
        self.assertEqual(event["event"], "inventory_coverage_run")
        self.assertEqual(event["branch"], "feat/x")
        payload = event["payload"]
        self.assertEqual(payload["new_shared_files"], 2)
        self.assertEqual(payload["new_routes"], 2)
        self.assertEqual(payload["warnings_emitted"], 2)
        self.assertEqual(payload["files"], ["frontend/src/utils/b.ts"])
        self.assertEqual(payload["routes"], ["/api/x/new"])


if __name__ == "__main__":
    unittest.main()
