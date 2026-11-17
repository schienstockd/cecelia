"""report_issue: a session's exit when Cecelia is the problem — the draft, the public link, the hints.

Nothing here reaches GitHub or the user's config dir: the draft directory is a temp dir and the
client calls are faked.
"""
import asyncio
import os
import tempfile
import unittest
import unittest.mock
from urllib.parse import parse_qs, urlparse

from cecelia_mcp import issue_report, server
from cecelia_mcp.client import ApiError
from mcp.server.mcpserver.exceptions import ToolError

ARGS = {"kind": "bug", "title": "cellpose crashes on 2D image", "tried": "segment.cellpose on one image",
        "observed": "KeyError: 'z'", "expected": "labels", "workaround": "stopped"}


def _form(url: str) -> dict:
    return {k: v[0] for k, v in parse_qs(urlparse(url).query).items()}


class RedactTest(unittest.TestCase):
    def test_paths_and_names_leave_the_text(self):
        text = ("failed reading /data/lab/mouse3_LN/ccid.json and C:\\Users\\x\\proj\\a.zarr "
                "for image Mouse3-LN (uid p6t4mC)")
        out = issue_report.redact(text, ["mouse3-ln", "p6t4mC"])
        for leaked in ("/data/lab", "mouse3", "Users", "p6t4mC", "Mouse3-LN"):
            self.assertNotIn(leaked, out)
        self.assertIn("<path>", out)
        self.assertIn("<name>", out)

    def test_home_dir_is_removed(self):
        home = os.path.expanduser("~")
        self.assertNotIn(home, issue_report.redact(f"see {home}/x.log"))


class IssueUrlTest(unittest.TestCase):
    def _report(self, **kw):
        r = {"id": "abc", "at": "now", "kind": "bug", "title": "t", "tried": "x", "observed": "y",
             "expected": "z", "workaround": "stopped", "version": "v0.1.3", "platform": "linux-64",
             "params": {"model": "cyto3"}, "log": "", "patch": ""}
        r.update(kw)
        return r

    def test_fills_the_bug_form_fields(self):
        form = _form(issue_report.issue_url(self._report(log="Traceback: boom"), []))
        self.assertEqual("bug_report.yml", form["template"])
        self.assertEqual("v0.1.3", form["version"])
        self.assertEqual("Linux", form["os"])
        self.assertIn("Traceback: boom", form["logs"])
        self.assertIn("cyto3", form["what-happened"])

    def test_a_huge_log_and_patch_still_fit_and_keep_the_description(self):
        r = self._report(log="E" * 50_000, patch="+" * 20_000, params={"k": "v" * 10_000},
                         observed="the real complaint")
        url = issue_report.issue_url(r, [])
        self.assertLessEqual(len(url), issue_report.URL_MAX)
        self.assertIn("the real complaint", _form(url)["what-happened"])

    def test_a_short_report_keeps_its_patch(self):
        url = issue_report.issue_url(self._report(patch="-a\n+b"), [])
        self.assertIn("+b", _form(url)["what-happened"])


class FailureTextTest(unittest.TestCase):
    def test_hint_only_where_the_fault_is_likely_cecelias(self):
        hint = issue_report.REPORT_HINT
        self.assertIn(hint, issue_report.failure_text(ApiError(500, "internal")))
        self.assertIn(hint, issue_report.failure_text(KeyError("z")))
        for own_fault in (ApiError(409, "name taken"), ApiError(404, "no such pop"),
                          ApiError(0, "cannot reach Cecelia API"), ValueError("bad transform")):
            self.assertNotIn(hint, issue_report.failure_text(own_fault))

    def test_a_crash_reaches_the_model_with_the_hint(self):
        def boom():
            raise KeyError("projects")
        with unittest.mock.patch.object(server._client, "get_projects", boom):
            with self.assertRaises(ToolError) as cm:
                asyncio.run(server.mcp.call_tool("list_projects", {}))
        self.assertIn("KeyError", str(cm.exception))
        self.assertIn("report_issue", str(cm.exception))

    def test_failed_runs_in_history_carry_the_hint(self):
        reply = {"count": 1, "total": 1, "truncated": False,
                 "history": [{"imageUid": "I", "fun": "segment.cellpose", "runStatus": "failed"}]}
        with unittest.mock.patch.object(server._client, "get_task_history", lambda *a: reply):
            self.assertIn("report_issue", server.get_task_history("P")["onFailure"])
        reply["history"][0]["runStatus"] = "done"
        with unittest.mock.patch.object(server._client, "get_task_history", lambda *a: reply):
            self.assertNotIn("onFailure", server.get_task_history("P"))


    def test_autonomous_runs_that_failed_carry_the_hint(self):
        from cecelia_mcp import autonomous_server as auto
        with unittest.mock.patch.object(auto._client, "wait_tasks",
                                        lambda *a: {"finished": True, "states": {"t1": "failed"}}):
            self.assertIn("report_issue", auto.wait_for_tasks(["t1"])["onFailure"])
        with unittest.mock.patch.object(auto._client, "wait_tasks",
                                        lambda *a: {"finished": True, "states": {"t1": "done"}}):
            self.assertNotIn("onFailure", auto.wait_for_tasks(["t1"]))
        chain = {"active": False, "waiting": False, "imageStates": {"I": {"n1": "done", "n2": "failed"}}}
        with unittest.mock.patch.object(auto._client, "wait_chain", lambda *a: chain):
            self.assertIn("onFailure", auto.wait_for_chain("P", "r1"))


class ReportIssueToolTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        server._REPORTED.clear()
        patches = [
            unittest.mock.patch.object(server, "_reports_dir", lambda: self.tmp.name),
            unittest.mock.patch.object(server._client, "get_version", lambda: {"version": "v0.1.3"}),
            unittest.mock.patch.object(server._client, "get_projects",
                                       lambda: {"projects": [{"uid": "P", "name": "zolima-mertk"}]}),
            unittest.mock.patch.object(server._client, "get_task_history", lambda *a: {"history": [
                {"imageUid": "I", "imageName": "LN_mouse3", "fun": "segment.cellpose",
                 "params": {"model": "cyto3", "path": "/home/someone/LN_mouse3.czi"}}]}),
            unittest.mock.patch.object(server._client, "get_task_log", lambda *a: {
                "exists": True, "content": "reading /home/someone/zolima-mertk/LN_mouse3\nKeyError: 'z'"}),
        ]
        for p in patches:
            p.start()
            self.addCleanup(p.stop)
        self.addCleanup(self.tmp.cleanup)
        server._REPORTED.clear()

    def test_saves_a_full_draft_and_a_redacted_link(self):
        out = server.report_issue(**ARGS, project_uid="P", image_uid="I", fun="segment.cellpose")
        draft = open(out["draft"], encoding="utf-8").read()
        self.assertTrue(out["draft"].startswith(self.tmp.name))
        self.assertIn("LN_mouse3", draft)          # the local draft is complete
        self.assertIn("KeyError: 'z'", draft)
        self.assertIn("v0.1.3", draft)
        form = _form(out["issueUrl"])
        public = " ".join(form.values())
        for leaked in ("LN_mouse3", "zolima-mertk", "/home/someone"):
            self.assertNotIn(leaked, public)
        self.assertIn("KeyError: 'z'", form["logs"])
        self.assertIn("stop", out["next"])

    def test_the_same_report_twice_is_not_saved_again(self):
        first = server.report_issue(**ARGS)
        again = server.report_issue(**{**ARGS, "observed": "  keyerror: 'Z' "})
        self.assertTrue(again["duplicate"])
        self.assertEqual(first["draft"], again["draft"])
        self.assertEqual(1, len(os.listdir(self.tmp.name)))

    def test_a_workaround_the_user_asked_for_is_not_told_to_stop(self):
        out = server.report_issue(**{**ARGS, "workaround": "proceeding-user-asked"})
        self.assertIn("carry on", out["next"])

    def test_refuses_an_unknown_kind_or_workaround(self):
        with self.assertRaises(ToolError):
            server.report_issue(**{**ARGS, "kind": "feature"})
        with self.assertRaises(ToolError):
            server.report_issue(**{**ARGS, "workaround": "maybe"})

    def test_a_report_about_an_unreachable_app_is_still_written(self):
        def down(*a, **kw):
            raise ApiError(0, "cannot reach Cecelia API")
        with unittest.mock.patch.object(server._client, "get_version", down), \
                unittest.mock.patch.object(server._client, "get_projects", down):
            out = server.report_issue(**ARGS, project_uid="P")
        self.assertTrue(os.path.exists(out["draft"]))


if __name__ == "__main__":
    unittest.main()
