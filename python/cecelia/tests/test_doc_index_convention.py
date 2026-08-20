"""Convention test: `docs/todo/README.md` must stay a true index of `docs/todo/`.

`docs/todo/` is excluded from default agent search (`CLAUDE.md` → *How to read the docs*), because
1.2 MB of design docs supplied ~40-50% of doc grep hits while 349 code citations already reach the
plans by name. That exclusion is only safe while the index is complete: a plan with no row is
invisible, so its locked design gets re-derived from scratch.

The index had already failed exactly that way before this test existed — it listed 20 of 57 plans, and
five of those rows contradicted their own plan's header (`MCP_BOARD_AUTHORING_PLAN` and
`TASK_LIST_UNIFICATION_PLAN` were filed as "planning" long after their headers said BUILT). Nothing
noticed, because nothing checked.

What this asserts, and what it deliberately does not:

* **Completeness both ways** — every plan file has exactly one row, every row resolves to a real file.
  Exact; this is the check that would have caught the 37 missing rows.
* **Every plan states a status** — so a reader who opens one knows whether it describes intent or
  reality. Trackers/audits are exempt: "status" is meaningless for a living checklist.
* **No outright contradiction** between a plan's own status line and the section its row sits in.
  This is a ONE-SIDED conflict detector, not a classifier. Plan headers are free prose and routinely
  mixed ("P1 + P4a built (PR #590); P2, P3 and the rest open"), so classifying them would misfire and
  a test that cries wolf gets muted. Only an unambiguous signal in the wrong section fails: a plan
  shouting BUILT/SHIPPED/COMPLETE while filed under Open, or saying "not built"/"not started" while
  filed under Built. Mixed or hedged headers are left alone by design.

It lives in the Python suite because that is the one auto-discovered, stdlib-only suite already
wired into CI (`pixi run test-py`) and it needs no toolchain — a dedicated task and CI job for a
single docs check would be a second way to do the same thing.

Run with `pixi run test-py`.
"""
import os
import re
import unittest

_REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))
_TODO = os.path.join(_REPO, 'docs', 'todo')
_INDEX = os.path.join(_TODO, 'README.md')

#: A row in the index: `| [`NAME.md`](NAME.md) | status | summary |`
_ROW = re.compile(r'^\|\s*\[`([^`]+\.md)`\]\(([^)]+)\)\s*\|', re.M)
#: The section headings the rows are grouped under. Matched on the leading words only, so the
#: human-facing rest of the heading can be reworded without breaking the test.
_SECTION = re.compile(r'^###\s+(Open|Built|Trackers)\b', re.M)
#: A plan's own status line, in any of the spellings in use: `Status:`, `**Status:**`,
#: `> Status: `, `Status (updated 2026-07-24):`, `**Status snapshot**`.
_STATUS = re.compile(r'^\s*>?\s*\**Status\**\s*(?:\([^)]*\))?\s*(?:[:：]|snapshot)', re.M | re.I)

#: Unambiguous "the work landed" markers. Upper-case only: prose uses "built" lower-case in mixed
#: statements ("P1-P4 built, P5 open"), where no conclusion may be drawn.
_DONE = re.compile(r'\b(BUILT|SHIPPED|COMPLETE|DONE)\b')
#: Unambiguous "the work has not started" markers.
_NOT_STARTED = re.compile(r'\b(not built|not started|no branch yet)\b', re.I)


def _read(path):
    with open(path, encoding='utf-8') as fh:
        return fh.read()


def _plan_files():
    return {
        f for f in os.listdir(_TODO)
        if f.endswith('.md') and f != 'README.md'
    }


def _rows_by_section(index_text):
    """-> {section: [(filename, link_target), ...]} in document order."""
    bounds = [(m.start(), m.group(1)) for m in _SECTION.finditer(index_text)]
    out = {name: [] for _, name in bounds}
    for m in _ROW.finditer(index_text):
        section = None
        for start, name in bounds:
            if m.start() > start:
                section = name
        if section is not None:
            out[section].append((m.group(1), m.group(2)))
    return out


class TodoIndexIsComplete(unittest.TestCase):
    """Every plan has a row and every row has a plan."""

    #: the whole point of the failure is the list of offenders; never elide it
    maxDiff = None

    def setUp(self):
        self.index = _read(_INDEX)
        self.by_section = _rows_by_section(self.index)
        self.rows = [r for rows in self.by_section.values() for r in rows]

    def test_the_index_has_the_expected_sections(self):
        self.assertEqual(
            {'Open', 'Built', 'Trackers'}, set(self.by_section),
            'the index sections were renamed; update _SECTION and the grouping this test relies on',
        )

    def test_every_plan_has_a_row(self):
        missing = sorted(_plan_files() - {name for name, _ in self.rows})
        self.assertEqual(
            [], missing,
            'plans with no row in docs/todo/README.md. docs/todo/ is excluded from default search, so '
            'a plan absent from the index is invisible and its design will be re-derived. Add a row '
            'in the same change that adds the plan:\n  ' + '\n  '.join(missing),
        )

    def test_every_row_points_at_a_real_plan(self):
        present = _plan_files()
        dangling = sorted(
            f'{name} (link -> {target})'
            for name, target in self.rows
            if name not in present or not os.path.isfile(os.path.join(_TODO, target))
        )
        self.assertEqual([], dangling, 'index rows pointing at a file that does not exist:\n  '
                                       + '\n  '.join(dangling))

    def test_no_plan_is_listed_twice(self):
        seen, dupes = set(), []
        for name, _ in self.rows:
            if name in seen:
                dupes.append(name)
            seen.add(name)
        self.assertEqual([], sorted(dupes), 'plans listed in more than one row/section')


class PlansStateTheirStatus(unittest.TestCase):
    """A plan filed as open or built must say which it is, in its own header."""

    maxDiff = None

    def test_open_and_built_plans_carry_a_status_line(self):
        index = _read(_INDEX)
        by_section = _rows_by_section(index)
        missing = []
        for section in ('Open', 'Built'):
            for name, _ in by_section.get(section, []):
                head = '\n'.join(_read(os.path.join(_TODO, name)).splitlines()[:14])
                if not _STATUS.search(head):
                    missing.append(name)
        self.assertEqual(
            [], sorted(missing),
            'plans with no Status line in their first 14 lines. Whoever opens one cannot tell whether '
            'it describes intent or reality, which is how a shipped design gets read as a spec:\n  '
            + '\n  '.join(sorted(missing)),
        )


class IndexDoesNotContradictThePlans(unittest.TestCase):
    """One-sided: only an unambiguous status in the wrong section fails."""

    maxDiff = None

    def setUp(self):
        self.by_section = _rows_by_section(_read(_INDEX))

    def _status_block(self, name):
        """Everything the author said about status: the status line to the first `##` heading.

        Neither a line count nor a paragraph is the right unit, and both were tried. `TASK_RUNNER_PLAN`
        opens "Phases 1 and 2 BUILT" and names what is left ("Phase 3's target badge, the spool
        (deferred), Phase 4's remote target") in a *separate paragraph* four lines down; a paragraph
        window reads it as finished, a 3-line window likewise. The header block — everything before the
        content starts — is the unit that matches how these are actually written, and erring long is
        safe here: extra prose can only make this one-sided detector stay quiet, never cry wolf.
        """
        text = _read(os.path.join(_TODO, name))
        m = _STATUS.search(text)
        if not m:
            return ''
        out = []
        for line in text[m.start():].splitlines():
            if out and line.lstrip().startswith('##'):
                break
            out.append(line.strip().lstrip('>').strip())
        return ' '.join(out)

    def test_no_built_plan_is_filed_as_open(self):
        offenders = []
        for name, _ in self.by_section.get('Open', []):
            status = self._status_block(name)
            if _DONE.search(status) and not _NOT_STARTED.search(status):
                # a mixed status ("P1-P3 BUILT; P4 open") is legitimately Open
                if not re.search(r'\bopen\b|\bremaining\b|in[- ]progress|\bnext\b|\bcut\b|deferred',
                                 status, re.I):
                    offenders.append(f'{name}: {status[:120]}')
        self.assertEqual(
            [], sorted(offenders),
            'filed under Open while their own status says the work landed. Move the row to Built (the '
            'plan then reads as a record of why, not a spec of what is) or correct the plan header:\n  '
            + '\n  '.join(sorted(offenders)),
        )

    def test_no_unstarted_plan_is_filed_as_built(self):
        offenders = [
            f'{name}: {self._status_block(name)[:120]}'
            for name, _ in self.by_section.get('Built', [])
            if _NOT_STARTED.search(self._status_block(name))
        ]
        self.assertEqual(
            [], sorted(offenders),
            'filed under Built while their own status says the work has not started:\n  '
            + '\n  '.join(sorted(offenders)),
        )


if __name__ == '__main__':
    unittest.main()
