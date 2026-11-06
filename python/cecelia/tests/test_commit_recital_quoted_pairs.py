"""The commit hook reads an outcome tag only outside backticks: a reviewer quoting the tag syntax in
the recital body (`[fanout-…: false_positive: <reason>]`) is prose, not a second outcome. Before,
the quote and the author's real tag counted as a duplicate slug and blocked the commit."""
import importlib.util
import pathlib
import unittest
from unittest import mock

_HOOK_PATH = pathlib.Path(__file__).resolve().parents[3] / ".claude" / "hooks" / "check_commit_recital.py"


def _load_hook():
    spec = importlib.util.spec_from_file_location("check_commit_recital", _HOOK_PATH)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class QuotedPairTest(unittest.TestCase):
    def setUp(self):
        self.hook = _load_hook()
        p = mock.patch.object(self.hook, "_current_head_sha", return_value=None)
        p.start()
        self.addCleanup(p.stop)

    def test_a_pair_quoted_in_backticks_is_not_a_tag(self):
        self.assertEqual(self.hook._parse_pairs("see `[fanout-abcd1234: false_positive: <reason>]`"), [])

    def test_a_quoted_pair_beside_the_real_tag_is_no_duplicate(self):
        msg = ("git commit -m '- x [**confirmed**] [fanout-abcd1234: fixed_pre_commit]\n"
               "- note: needs `[fanout-abcd1234: false_positive: <reason>]` or the bypass'")
        self.assertIsNone(self.hook.check(msg))

    def test_a_quoted_bare_tag_does_not_cover_a_finding(self):
        msg = "git commit -m '- x [**confirmed**] — the reviewer suggests `fixed_pre_commit` once fixed'"
        self.assertIsNotNone(self.hook.check(msg))

    def test_two_real_tags_still_duplicate(self):
        msg = ("git commit -m '- x [**confirmed**] [fanout-abcd1234: fixed_pre_commit]\n"
               "[fanout-abcd1234: false_positive: wrong]'")
        self.assertIn("Duplicate slug", self.hook.check(msg) or "")


if __name__ == "__main__":
    unittest.main()
