"""The escaped-newline repair (`cecelia_mcp/agent_text.py`) and every MCP write tool that applies it.

The positive case is the shape a model actually sent: a whole Markdown profile as one line with
literal backslash-n between its headings. The negatives are the content that must survive: text that
already has real newlines, inline code and regexes with a literal backslash-n, Windows paths, and a
single stray backslash-n in prose.
"""
import unittest
from unittest import mock

from cecelia_mcp import server
from cecelia_mcp.agent_text import repair_escaped_newlines, repair_lines

ESCAPED_PROFILE = ("# Project profile\\n\\n## Subject\\nNaive CD8 T cells, imaged intravitally.\\n\\n"
                   "## Key channels\\nc0 = P14-CTDR, c3 = gBT-CTV. CTV bleeds into the GFP channel.\\n")
REPAIRED_PROFILE = ("# Project profile\n\n## Subject\nNaive CD8 T cells, imaged intravitally.\n\n"
                    "## Key channels\nc0 = P14-CTDR, c3 = gBT-CTV. CTV bleeds into the GFP channel.\n")


class RepairEscapedNewlinesTest(unittest.TestCase):
    def test_double_escaped_markdown_is_repaired(self):
        self.assertEqual((REPAIRED_PROFILE, True), repair_escaped_newlines(ESCAPED_PROFILE))

    def test_crlf_escapes_become_newlines(self):
        self.assertEqual(("a\nb\nc", True), repair_escaped_newlines("a\\r\\nb\\r\\nc"))

    def test_already_multiline_text_is_unchanged(self):
        # real newlines → it was escaped correctly; a backslash-n left in it is content
        text = "## Regex\nSplit with `\\n` or with \\n\\n in a raw string."
        self.assertEqual((text, False), repair_escaped_newlines(text))

    def test_plain_one_line_text_is_unchanged(self):
        for text in ("", "One short line.", "uses µm and — dashes"):
            self.assertEqual((text, False), repair_escaped_newlines(text))

    def test_backslash_n_inside_code_spans_is_kept(self):
        # only code spans carry the sequences → nothing outside to repair
        text = "split on `\\n`, then join with ``\\n\\n``"
        self.assertEqual((text, False), repair_escaped_newlines(text))
        # and when the prose IS repaired, the code span is still left byte-for-byte
        fixed, did = repair_escaped_newlines("# T\\n\\nsplit on `\\n` here\\n")
        self.assertTrue(did)
        self.assertEqual("# T\n\nsplit on `\\n` here\n", fixed)

    def test_regex_and_windows_path_are_kept(self):
        for text in ("match \\d+\\n\\n\\s* between records",
                     "raw data in D:\\data\\new\\nuclei",
                     "tabs \\t and \\w+\\n\\n"):
            self.assertEqual((text, False), repair_escaped_newlines(text))

    def test_single_stray_backslash_n_is_kept(self):
        text = "the parser splits records on \\n before counting"
        self.assertEqual((text, False), repair_escaped_newlines(text))

    def test_escaped_backslash_then_n_is_kept(self):
        text = "x\\\\ny\\\\nz"   # literal \\n twice: an escaped backslash, then n
        self.assertEqual((text, False), repair_escaped_newlines(text))

    def test_non_string_passes_through(self):
        self.assertEqual((None, False), repair_escaped_newlines(None))

    def test_repair_lines_splits_a_repaired_line(self):
        lines, did = repair_lines(["first", "a\\n\\nb\\nc", "keep `\\n`"])
        self.assertTrue(did)
        self.assertEqual(["first", "a", "b", "c", "keep `\\n`"], lines)

    def test_repair_lines_untouched_list(self):
        self.assertEqual((["a", "b"], False), repair_lines(["a", "b"]))
        self.assertEqual(("not a list", False), repair_lines("not a list"))


class WriteToolsRepairTest(unittest.TestCase):
    """Each text-writing tool stores the repaired text and says so on the reply; correct text goes
    through untouched with no extra key."""

    def _call(self, client_method, tool, *args, **kwargs):
        with mock.patch.object(server._client, client_method, return_value={"ok": True}) as m:
            out = tool(*args, **kwargs)
        return m, out

    def test_create_blackboard_entry(self):
        with mock.patch.object(server, "_infer_fingerprint", return_value=None):
            m, out = self._call("create_blackboard_entry", server.create_blackboard_entry,
                                "P1", "Profile", ESCAPED_PROFILE)
        self.assertEqual(REPAIRED_PROFILE, m.call_args.args[2])
        self.assertEqual(["content_md"], out["repairedEscapedNewlines"])

    def test_revise_blackboard_entry(self):
        m, out = self._call("revise_blackboard_entry", server.revise_blackboard_entry,
                            "P1", "profile", ESCAPED_PROFILE, note="filled in\\nfrom metadata\\n")
        self.assertEqual(REPAIRED_PROFILE, m.call_args.args[2])
        self.assertEqual("filled in\nfrom metadata\n", m.call_args.args[4])
        self.assertEqual(["content_md", "note"], out["repairedEscapedNewlines"])

    def test_revise_blackboard_entry_correct_text_untouched(self):
        m, out = self._call("revise_blackboard_entry", server.revise_blackboard_entry,
                            "P1", "profile", REPAIRED_PROFILE)
        self.assertEqual(REPAIRED_PROFILE, m.call_args.args[2])
        self.assertEqual({"ok": True}, out)

    def test_set_blackboard_outcome_note(self):
        m, out = self._call("set_blackboard_outcome", server.set_blackboard_outcome,
                            "P1", "bb-1", "bad", "wrong params\\n\\nused galvo defaults")
        self.assertEqual("wrong params\n\nused galvo defaults", m.call_args.args[3])
        self.assertEqual(["note"], out["repairedEscapedNewlines"])

    def test_set_blackboard_section_outcome_note(self):
        m, out = self._call("set_blackboard_section_outcome", server.set_blackboard_section_outcome,
                            "P1", "bb-1", "s01", "bad", "debris\\n\\nsee cluster 4")
        self.assertEqual("debris\n\nsee cluster 4", m.call_args.args[4])
        self.assertEqual(["note"], out["repairedEscapedNewlines"])

    def test_append_lab_log(self):
        m, out = self._call("append_lab_log", server.append_lab_log,
                            "P1", ["Segmented 3 images\\nTracked all 3\\n"])
        self.assertEqual(["Segmented 3 images", "Tracked all 3"], m.call_args.args[2])
        self.assertEqual(["lines"], out["repairedEscapedNewlines"])

    def test_append_lab_log_correct_lines_untouched(self):
        m, out = self._call("append_lab_log", server.append_lab_log, "P1", ["one", "two"])
        self.assertEqual(["one", "two"], m.call_args.args[2])
        self.assertEqual({"ok": True}, out)

    def test_set_labarchives_context_section_lines(self):
        sections = [{"heading": "Setup", "lines": ["WT vs KO\\nn = 4 each\\n"], "sourceDate": "x"}]
        m, out = self._call("set_labarchives_context", server.set_labarchives_context,
                            "P1", {"notebookId": "nb"}, sections)
        sent = m.call_args.args[2]
        self.assertEqual(["WT vs KO", "n = 4 each"], sent[0]["lines"])
        self.assertEqual("Setup", sent[0]["heading"])
        self.assertEqual(["sections.lines"], out["repairedEscapedNewlines"])

    def test_notebook_cells_are_not_repaired(self):
        # Julia source: "a\nb" inside a string literal is legitimate code
        cell = 'println("a\\nb\\n\\nc")'
        m, _ = self._call("create_notebook", server.create_notebook, "P1", "nb", [cell])
        self.assertEqual([cell], m.call_args.args[2])


if __name__ == "__main__":
    unittest.main()
