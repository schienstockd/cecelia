"""Tests for `scripts/judge/issues.py` — the judge's GitHub issue mirror.

Design: docs/todo/JUDGE_WORKFLOW_PLAN.md (D7–D11, D14). No test runs `gh`: `Gh` gets a fake runner
that keeps a small GitHub in memory, so a test can forge, edit or close an issue the way a person
would, and then check what the next pass does.

Run with `pixi run test-py`.
"""
from __future__ import annotations

import importlib.util
import json
import pathlib
import unittest

from cecelia.tests.test_judge_record import _bug, _Fixture

_REPO = pathlib.Path(__file__).resolve().parents[3]
REPO = "owner/repo"


def _load_issues():
    spec = importlib.util.spec_from_file_location("issues", _REPO / "scripts" / "judge" / "issues.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class FakeGitHub:
    """Issues by number, and every call made. `crash_on` raises on the first call whose args start so."""

    def __init__(self, login="owner"):
        self.login, self.issues, self.calls, self.crash_on = login, {}, [], None

    def add(self, title, *, user="owner", state="open", labels=("judge-bug",)):
        n = len(self.issues) + 1
        self.issues[n] = {"title": title, "user": user, "state": state, "labels": set(labels), "body": "",
                          "locked": False, "comments": [], "reason": None}
        return n

    def __call__(self, args, input):
        self.calls.append((list(args), input))
        if self.crash_on and args[:len(self.crash_on)] == self.crash_on:
            self.crash_on = None
            raise RuntimeError("pass died")
        if args[0] == "api":
            if args[1] == "user":
                return json.dumps({"login": self.login})
            q = dict(kv.split("=") for kv in args[1].split("?")[1].split("&"))
            rows = [{"number": n, "state": i["state"], "title": i["title"], "user": {"login": i["user"]}}
                    for n, i in sorted(self.issues.items())
                    if q["labels"] in i["labels"] and i["user"] == q["creator"]]
            page, per = int(q["page"]), int(q["per_page"])
            return json.dumps(rows[(page - 1) * per:page * per])
        opts = _opts(args)
        if args[:2] == ["label", "create"]:
            return ""
        if args[:2] == ["issue", "create"]:
            n = self.add(opts["--title"][0], user=self.login, labels=opts.get("--label", []))
            self.issues[n]["body"] = input
            return f"https://github.com/{REPO}/issues/{n}\n"
        i = self.issues[int(args[2])]
        if args[1] == "lock":
            i["locked"] = True
        elif args[1] == "edit":
            i["labels"] |= set(opts.get("--add-label", []))
            i["labels"] -= set(opts.get("--remove-label", []))
            i["body"] = input if "--body-file" in args else i["body"]
        elif args[1] == "close":
            i.update(state="closed", reason=opts["--reason"][0])
            i["comments"] += opts.get("--comment", [])
        elif args[1] == "reopen":
            i["state"] = "open"
            i["comments"] += opts.get("--comment", [])
        elif args[1] == "comment":
            i["comments"].append(input)
        return ""

    def writes(self):
        return [a for a, _ in self.calls if a[0] != "api"]


def _opts(args):
    out: dict = {}
    for k, v in zip(args, args[1:]):
        if k.startswith("--"):
            out.setdefault(k, []).append(v)
    return out


def _fix(id_, **over):
    return _bug(id_, verify={"verdict": "fix", "date": "2026-10-05", "effect": "it breaks", "evidence": "a.py:3"}, **over)


class _IssuesFixture(_Fixture):
    def setUp(self):
        super().setUp()
        self.i = _load_issues()
        self.github = FakeGitHub()
        self.slept = []

    def gh(self, dry=False):
        return self.i.Gh(REPO, self.github, dry=dry)

    def mirror(self, record, previous=None, dry=False, **kw):
        self.saved = []
        return self.i.mirror(record, gh=self.gh(dry), previous=previous, sleep=self.slept.append,
                             persist=lambda r: self.saved.append(json.loads(json.dumps(r))), **kw)


class MirrorTest(_IssuesFixture):
    def test_files_fix_and_decide_bugs_only_locked_with_the_body_on_stdin(self):
        record = self.build(bugs=[_fix("B1"), _bug("B2", verify={"verdict": "decide", "date": "x", "question": "q?"}),
                                  _bug("B3", status="unjudged"), _bug("B4", status="parked", verify={"verdict": "guard"}),
                                  _bug("B5", verify={"verdict": "guard"}), _bug("B6")])
        report = self.mirror(record)
        self.assertEqual(report["filed"], ["fanout-b1", "fanout-b2"])
        self.assertEqual({n: (i["title"], sorted(i["labels"]), i["locked"]) for n, i in self.github.issues.items()},
                         {1: ("[fanout-b1] fix: a.py:3", ["fix", "judge-bug"], True),
                          2: ("[fanout-b2] decide: a.py:3", ["decide", "judge-bug"], True)})
        creates = [(a, text) for a, text in self.github.calls if a[:2] == ["issue", "create"]]
        self.assertTrue(all("--body-file" in a and a[a.index("--body-file") + 1] == "-" and text for a, text in creates))
        self.assertFalse(any("desc fanout" in x or "it breaks" in x for a, _ in creates for x in a))   # not in argv
        self.assertEqual(record["bugs"][0]["issue"]["number"], 1)
        self.assertEqual(self.saved[0]["bugs"][0]["issue"], {"status": "pending"})   # intent before the create

    def test_a_rerun_files_nothing_twice(self):
        record = self.build(bugs=[_fix("B1")])
        self.mirror(record)
        before = len(self.github.writes())
        report = self.mirror(record)
        self.assertEqual((report["filed"], len(self.github.writes())), ([], before))

    def test_a_forged_issue_with_the_key_is_ignored_and_ours_is_filed(self):
        self.github.add("[fanout-b1] fix: a.py:3", user="stranger")
        record = self.build(bugs=[_fix("B1")])
        self.assertEqual(self.mirror(record)["filed"], ["fanout-b1"])
        self.assertEqual(record["bugs"][0]["issue"]["number"], 2)

    def test_an_owner_edit_is_never_read_and_is_overwritten_when_the_bug_changes(self):
        record = self.build(bugs=[_fix("B1")])
        self.mirror(record)
        self.github.issues[1]["body"] = "edited by hand"
        self.mirror(record)
        self.assertEqual(self.github.issues[1]["body"], "edited by hand")   # unchanged bug: no rewrite
        record["bugs"][0]["verify"]["effect"] = "it breaks worse"
        self.assertEqual(self.mirror(record)["updated"], ["fanout-b1"])
        self.assertIn("it breaks worse", self.github.issues[1]["body"])

    def test_a_crash_between_create_and_record_is_adopted_not_duplicated(self):
        record = self.build(bugs=[_fix("B1")])
        self.github.crash_on = ["issue", "lock"]
        with self.assertRaises(RuntimeError):
            self.mirror(record)
        stored = self.saved[-1]   # what the store holds: the intent, no number
        self.assertEqual(stored["bugs"][0]["issue"], {"status": "pending"})
        report = self.mirror(stored)
        self.assertEqual((report["filed"], len(self.github.issues)), ([], 1))
        self.assertEqual(stored["bugs"][0]["issue"]["number"], 1)

    def test_a_merge_keyword_close_is_reopened_while_the_bug_is_open(self):
        record = self.build(bugs=[_fix("B1")])
        self.mirror(record)
        self.github.issues[1]["state"] = "closed"   # `fixes #1` in a merged PR
        self.assertEqual(self.mirror(record)["reopened"], ["fanout-b1"])
        self.assertEqual(self.github.issues[1]["state"], "open")

    def test_gone_closes_completed_parked_and_dismissed_close_not_planned(self):
        record = self.build(bugs=[_fix("B1"), _fix("B2"), _fix("B3")])
        self.mirror(record)
        later = self.build("2026-10-12", bugs=[
            {**record["bugs"][0], "status": "gone", "why": "the function no longer exists"},
            {**record["bugs"][1], "status": "parked", "verify": {"verdict": "guard", "trigger": "a caller"}},
            {**record["bugs"][2], "status": "dismissed", "verify": {"verdict": "dismiss", "effect": "nothing"}}])
        self.assertEqual(self.mirror(later, previous=record)["closed"], ["fanout-b1", "fanout-b2", "fanout-b3"])
        self.assertEqual([self.github.issues[n]["reason"] for n in (1, 2, 3)], ["completed", "not planned", "not planned"])
        self.assertIn("parked", self.github.issues[2]["labels"])

    def test_a_parked_issue_reopens_without_its_label_once_verified_live_again(self):
        record = self.build(bugs=[_fix("B1")])
        self.mirror(record)
        parked = self.build("2026-10-12", bugs=[{**record["bugs"][0], "status": "parked", "verify": {"verdict": "guard"}}])
        self.mirror(parked, previous=record)
        live = self.build("2026-10-19", bugs=[{**parked["bugs"][0], "status": "open",
                                               "verify": {"verdict": "fix", "effect": "live now"}}])
        self.assertEqual(self.mirror(live, previous=parked)["reopened"], ["fanout-b1"])
        self.assertEqual(sorted(self.github.issues[1]["labels"]), ["fix", "judge-bug"])

    def test_a_wont_fix_bug_no_longer_carried_closes_its_issue(self):
        record = self.build(bugs=[_fix("B1")])
        self.mirror(record)
        record["bugs"][0]["status"] = "wont_fix"   # the owner's answer, applied to that record
        self.assertEqual(self.mirror(self.build("2026-10-12"), previous=record)["closed"], ["fanout-b1"])
        self.assertEqual(self.github.issues[1]["reason"], "not planned")

    def test_a_landed_fix_labels_and_comments_once(self):
        record = self.build(bugs=[_fix("B1")])
        self.mirror(record)
        record["bugs"][0]["fix_landed"] = [{"commit": "c" * 40, "subject": "fix", "pr": 7}]
        self.mirror(record)
        self.mirror(record)
        self.assertIn("fix-landed", self.github.issues[1]["labels"])
        self.assertEqual(len(self.github.issues[1]["comments"]), 1)
        self.assertIn(f"https://github.com/{REPO}/pull/7", self.github.issues[1]["comments"][0])

    def test_a_fix_session_labels_in_progress_and_an_answer_comments_once(self):
        record = self.build(bugs=[_bug("B1", verify={"verdict": "decide", "date": "x", "question": "q?"})])
        self.mirror(record)
        record["bugs"][0].update(owner_decision="open", owner_answer="guard it at the caller, cc @someone",
                                 fix_session="2026-10-06T01:00:00Z")
        self.mirror(record)
        self.mirror(record)
        self.assertEqual(sorted(self.github.issues[1]["labels"]), ["decide", "in-progress", "judge-bug"])
        self.assertEqual(self.github.issues[1]["comments"], ["Your answer: guard it at the caller, cc `@someone`\n"])

    def test_an_answer_that_leaks_is_not_quoted(self):
        record = self.build(bugs=[_bug("B1", verify={"verdict": "decide", "date": "x"})])
        self.mirror(record)
        record["bugs"][0].update(owner_decision="open", owner_answer="it's in jFWePN")
        self.mirror(record, uids=())
        self.i.defuse = lambda text, uids=(): text   # a defuse that missed one: the backstop still holds
        record["bugs"][0]["issue"].pop("answered")
        self.mirror(record, uids={"jFWePN"})
        self.assertEqual(self.github.issues[1]["comments"][1],
                         "Answered in `pixi run judge-review` (the answer isn't shown here).\n")

    def test_an_issue_not_in_the_list_is_missing_and_left_alone(self):
        record = self.build(bugs=[_fix("B1", issue={"number": 9, "state": "open"})])
        report = self.mirror(record)
        self.assertEqual((report["missing"], self.github.writes()), (["fanout-b1"], []))

    def test_over_the_cap_waits_for_the_next_pass(self):
        record = self.build(bugs=[_fix(f"B{i}") for i in range(1, 6)])
        report = self.mirror(record, max_creates=3)
        self.assertEqual((len(report["filed"]), len(report["over_cap"]), len(self.slept)), (3, 2, 2))

    def test_a_dry_run_reads_but_writes_nothing(self):
        record = self.build(bugs=[_fix("B1")])
        gh = self.gh(dry=True)
        report = self.i.mirror(record, gh=gh, persist=lambda r: self.fail("dry run persisted"))
        self.assertEqual((report["filed"], self.github.writes(), self.github.issues), (["fanout-b1"], [], {}))
        self.assertIn(["issue", "create"], [c[:2] for c, _ in gh.planned])
        self.assertNotIn("issue", record["bugs"][0])


class BodyTest(_IssuesFixture):
    def test_mentions_and_references_are_defused_paths_and_uids_stripped(self):
        text = self.i.defuse("bump @primeuix/themes, see #2 and other/repo#3; `@kept #4` in code; "
                             "/home/dominik/x and C:\\Users\\dominik\\y; image jFWePN", uids={"jFWePN"})
        self.assertEqual(text, "bump `@primeuix/themes`, see `#2` and `other/repo#3`; `@kept #4` in code; "
                               "~/x and ~\\y; image <uid>")
        self.assertEqual(self.i.leaks(text, {"jFWePN"}), [])

    def test_the_backstop_holds_a_body_that_still_leaks(self):
        self.assertEqual(self.i.leaks("hi @someone about #12 from /home/me", ()),
                         ["mention @someone", "reference #12", "home path /home/me"])
        record = self.build(bugs=[_fix("B1")])
        self.i.body = lambda b, **kw: "leaks @someone\n"
        report = self.mirror(record)
        self.assertEqual((report["held"], self.github.issues), ([{"key": "fanout-b1", "why": "mention @someone"}], {}))

    def test_a_body_is_built_from_fields_and_quotes_the_finding(self):
        b = _fix("B1", desc="see @primeuix and #2")
        text = self.i.body(b, repo=REPO, sha="a" * 40)
        self.assertIn(f"[`a.py:3`](https://github.com/{REPO}/blob/{'a' * 40}/a.py#L3)", text)
        self.assertIn("> see `@primeuix` and `#2`", text)
        self.assertEqual(self.i.leaks(text), [])

    def test_an_agent_error_shows_its_form_not_its_message(self):
        b = _bug("B1", kind="agent_run", file=None, line=None, tool="get_cohort_qc", repeat=True, runs=4,
                 error="HTTP 400: No cohort metrics for fun 'x' in project abcDEF", template="No cohort metrics for fun",
                 desc="raw: HTTP 400 ... abcDEF")
        text = self.i.body(b, repo=REPO, sha="a" * 40)
        self.assertIn("`get_cohort_qc` failed with HTTP 400: No cohort metrics for fun, in 4 separate runs", text)
        self.assertNotIn("abcDEF", text)
        self.assertEqual(self.i.title(b), "[fanout-b1] open: repeated agent error in get_cohort_qc")


class AllowlistTest(_IssuesFixture):
    def test_only_the_mirror_calls_reach_gh(self):
        gh = self.gh()
        for bad in (["repo", "delete", REPO], ["pr", "merge", "1"], ["issue", "delete", "1"],
                    ["api", "-X", "DELETE", f"repos/{REPO}/issues/1"], ["api", "repos/other/repo/issues"],
                    ["api", f"repos/{REPO}/issues", "-f", "title=x"]):
            with self.subTest(bad=bad), self.assertRaises(self.i.GhRefused):
                gh(*bad)
        self.assertEqual(self.github.calls, [])
        gh("api", "user")
        gh("issue", "lock", "1") if self.github.add("t") else None
        self.assertEqual([c[:2] for c, _ in self.github.calls], [["api", "user"], ["issue", "lock"]])


if __name__ == "__main__":
    unittest.main()
