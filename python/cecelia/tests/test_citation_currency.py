"""Unit tests for the mechanical citation-currency check.

Design in docs/archive/governance_layer_audit.md §Item 4 and the module docstring at
python/cecelia/effectiveness/citation_currency.py."""
import json
import os
import pathlib
import tempfile
import unittest

from cecelia.effectiveness import EVENT_TYPES  # noqa: F401 (asserts import surface intact)
from cecelia.effectiveness.citation_currency import (
    Citation,
    build_citation_index,
    find_stale_citations,
    format_section,
    run_citation_check,
    touched_files_from_diff,
)
from cecelia.effectiveness.log import default_log_path


class BuildCitationIndexTest(unittest.TestCase):
    """Extraction + resolution paths."""

    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.root = pathlib.Path(self._tmp.name)

    def _write(self, rel: str, text: str) -> None:
        p = self.root / rel
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(text, encoding="utf-8")

    def test_extracts_backticked_path_after_enforced_by(self):
        self._write("docs/A.md", "Some rule. Enforced by `path/to/thing.py`.\n")
        self._write("path/to/thing.py", "x = 1\n")
        idx = build_citation_index(
            self.root, repo_files=["docs/A.md", "path/to/thing.py"]
        )
        self.assertEqual(idx, {"path/to/thing.py": [Citation("docs/A.md", 1)]})

    def test_skips_docs_archive_and_docs_todo(self):
        self._write("docs/archive/frozen.md", "Enforced by `path/to/thing.py`.\n")
        self._write("docs/todo/PLAN.md", "Enforced by `path/to/thing.py`.\n")
        self._write("path/to/thing.py", "x = 1\n")
        idx = build_citation_index(
            self.root,
            repo_files=[
                "docs/archive/frozen.md",
                "docs/todo/PLAN.md",
                "path/to/thing.py",
            ],
        )
        self.assertEqual(idx, {})

    def test_extension_whitelist_skips_non_code_tokens(self):
        self._write(
            "docs/A.md",
            "Enforced by `some-config.yaml` and `notes.md` — real: `x.py`.\n",
        )
        self._write("x.py", "")
        idx = build_citation_index(
            self.root, repo_files=["docs/A.md", "x.py"]
        )
        # Only the whitelisted extension resolves.
        self.assertEqual(list(idx.keys()), ["x.py"])

    def test_resolves_bare_basename_when_unique(self):
        self._write("docs/A.md", "Enforced by `unique.py`.\n")
        self._write("dir1/unique.py", "")
        idx = build_citation_index(
            self.root, repo_files=["docs/A.md", "dir1/unique.py"]
        )
        self.assertEqual(idx, {"dir1/unique.py": [Citation("docs/A.md", 1)]})

    def test_ambiguous_basename_is_dropped(self):
        self._write("docs/A.md", "Enforced by `dupe.py`.\n")
        self._write("a/dupe.py", "")
        self._write("b/dupe.py", "")
        idx = build_citation_index(
            self.root, repo_files=["docs/A.md", "a/dupe.py", "b/dupe.py"]
        )
        self.assertEqual(idx, {})

    def test_line_without_enforcement_phrase_is_ignored(self):
        self._write(
            "docs/A.md",
            "See `dir/thing.py` for details.\n"
            "Enforced by `dir/thing.py`.\n",
        )
        self._write("dir/thing.py", "")
        idx = build_citation_index(
            self.root, repo_files=["docs/A.md", "dir/thing.py"]
        )
        # Only the second line, which has "Enforced by", counts.
        self.assertEqual(idx, {"dir/thing.py": [Citation("docs/A.md", 2)]})

    def test_claude_md_and_inventory_md_are_scanned(self):
        # These live at the repo root, not under docs/ — they must be included explicitly.
        self._write("CLAUDE.md", "Enforced by `x.py`.\n")
        self._write("INVENTORY.md", "Enforced by `y.py`.\n")
        self._write("x.py", "")
        self._write("y.py", "")
        idx = build_citation_index(
            self.root,
            repo_files=["CLAUDE.md", "INVENTORY.md", "x.py", "y.py"],
        )
        self.assertEqual(
            idx,
            {
                "x.py": [Citation("CLAUDE.md", 1)],
                "y.py": [Citation("INVENTORY.md", 1)],
            },
        )

    def test_pre_commit_hook_phrase_qualifies(self):
        # The three enforcement patterns: "enforced by", "ratchets X to zero", "pre-commit hook".
        self._write(
            "docs/A.md",
            "A pre-commit hook `.claude/hooks/check.py` runs on every commit.\n",
        )
        self._write(".claude/hooks/check.py", "")
        idx = build_citation_index(
            self.root, repo_files=["docs/A.md", ".claude/hooks/check.py"]
        )
        self.assertEqual(
            idx, {".claude/hooks/check.py": [Citation("docs/A.md", 1)]}
        )

    def test_bold_enforced_by_is_matched(self):
        # `**Enforced** by` (markdown bold) is the form MAINTAINABILITY.md uses; the plain-text
        # "enforced by" substring is broken by the closing `**`, so the regex needs the escape.
        self._write("docs/A.md", "The rule. **Enforced** by `src/x.py`.\n")
        self._write("src/x.py", "")
        idx = build_citation_index(
            self.root, repo_files=["docs/A.md", "src/x.py"]
        )
        self.assertEqual(idx, {"src/x.py": [Citation("docs/A.md", 1)]})


class TouchedFilesFromDiffTest(unittest.TestCase):
    def test_parses_diff_git_header(self):
        diff = (
            "diff --git a/foo/bar.py b/foo/bar.py\n"
            "index abc..def 100644\n"
            "--- a/foo/bar.py\n"
            "+++ b/foo/bar.py\n"
            "@@ -1,1 +1,1 @@\n"
        )
        self.assertEqual(touched_files_from_diff(diff), {"foo/bar.py"})

    def test_handles_multiple_files(self):
        diff = (
            "diff --git a/one.py b/one.py\n"
            "diff --git a/two.py b/two.py\n"
        )
        self.assertEqual(touched_files_from_diff(diff), {"one.py", "two.py"})

    def test_empty_diff_returns_empty_set(self):
        self.assertEqual(touched_files_from_diff(""), set())


class FindStaleCitationsTest(unittest.TestCase):
    def test_touched_cited_file_with_untouched_citing_doc_is_stale(self):
        idx = {"src/x.py": [Citation("docs/A.md", 5)]}
        touched = {"src/x.py"}
        self.assertEqual(
            find_stale_citations(idx, touched),
            [("src/x.py", [Citation("docs/A.md", 5)])],
        )

    def test_touched_cited_file_with_touched_citing_doc_is_not_stale(self):
        idx = {"src/x.py": [Citation("docs/A.md", 5)]}
        touched = {"src/x.py", "docs/A.md"}
        self.assertEqual(find_stale_citations(idx, touched), [])

    def test_any_touched_citing_doc_clears_the_warning(self):
        # Multiple citing docs — as long as ONE was touched, the warning is silenced.
        idx = {
            "src/x.py": [
                Citation("docs/A.md", 5),
                Citation("docs/B.md", 12),
            ]
        }
        self.assertEqual(
            find_stale_citations(idx, {"src/x.py", "docs/B.md"}), []
        )

    def test_untouched_file_never_warns_even_if_cited(self):
        idx = {"src/x.py": [Citation("docs/A.md", 5)]}
        self.assertEqual(find_stale_citations(idx, {"src/other.py"}), [])


class FormatSectionTest(unittest.TestCase):
    def test_no_citations_indexed_prints_skipped_tail(self):
        out = format_section([], touched_count=3, indexed_count=0)
        self.assertEqual(out, "_Citation-currency check: skipped — no citations indexed_")

    def test_no_touched_files_prints_skipped_tail(self):
        out = format_section([], touched_count=0, indexed_count=42)
        self.assertEqual(out, "_Citation-currency check: skipped — no code changes_")

    def test_run_no_stale_prints_clean_tail(self):
        out = format_section([], touched_count=3, indexed_count=42)
        self.assertEqual(out, "_Citation-currency check: run — no stale citations_")

    def test_run_with_stale_prints_evidence_and_tail(self):
        stale = [
            ("src/x.py", [Citation("docs/A.md", 5), Citation("docs/B.md", 12)])
        ]
        out = format_section(stale, touched_count=1, indexed_count=1)
        self.assertIn("_Citation-currency check (evidence):_", out)
        self.assertIn("`src/x.py`", out)
        self.assertIn("`docs/A.md:5`", out)
        self.assertIn("`docs/B.md:12`", out)
        self.assertTrue(out.rstrip().endswith("_Citation-currency check: run_"))


class RunCitationCheckTest(unittest.TestCase):
    """End-to-end: index + diff extraction + event emission + formatted output."""

    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.root = pathlib.Path(self._tmp.name)
        # Isolate the event log so this test doesn't pollute the real one.
        self._log = self.root / "events.jsonl"
        os.environ["CECELIA_EFFECTIVENESS_LOG"] = str(self._log)
        self.addCleanup(os.environ.pop, "CECELIA_EFFECTIVENESS_LOG", None)
        default_log_path.cache_clear() if hasattr(default_log_path, "cache_clear") else None

    def _write(self, rel: str, text: str) -> None:
        p = self.root / rel
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(text, encoding="utf-8")

    def _events(self):
        if not self._log.exists():
            return []
        return [json.loads(ln) for ln in self._log.read_text(encoding="utf-8").splitlines() if ln.strip()]

    def test_end_to_end_stale_warning(self):
        self._write("docs/A.md", "Enforced by `src/x.py`.\n")
        self._write("src/x.py", "x = 1\n")
        diff = "diff --git a/src/x.py b/src/x.py\n"
        section = run_citation_check(
            diff,
            repo_root=self.root,
            repo_files=["docs/A.md", "src/x.py"],
        )
        # Section body contains the warning and standard tail.
        self.assertIn("`src/x.py`", section)
        self.assertIn("`docs/A.md:1`", section)
        self.assertTrue(section.rstrip().endswith("_Citation-currency check: run_"))
        # One event with the expected payload keys.
        events = self._events()
        self.assertEqual(len(events), 1)
        self.assertEqual(events[0]["event"], "citation_currency_run")
        self.assertEqual(events[0]["payload"]["citations_indexed"], 1)
        self.assertEqual(events[0]["payload"]["staged_files_checked"], 1)
        self.assertEqual(events[0]["payload"]["warnings_emitted"], 1)
        self.assertIn("duration_s", events[0]["payload"])

    def test_end_to_end_clean_no_warnings(self):
        self._write("docs/A.md", "Enforced by `src/x.py`.\n")
        self._write("src/x.py", "")
        # Touch BOTH — the citing doc AND the cited file — so no warning fires.
        diff = (
            "diff --git a/src/x.py b/src/x.py\n"
            "diff --git a/docs/A.md b/docs/A.md\n"
        )
        section = run_citation_check(
            diff,
            repo_root=self.root,
            repo_files=["docs/A.md", "src/x.py"],
        )
        self.assertEqual(section, "_Citation-currency check: run — no stale citations_")
        events = self._events()
        self.assertEqual(events[0]["payload"]["warnings_emitted"], 0)


if __name__ == "__main__":
    unittest.main()
