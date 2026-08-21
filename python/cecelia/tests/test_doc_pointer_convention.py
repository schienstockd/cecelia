"""Convention test: every doc pointer in the repo must still land somewhere real.

`CLAUDE.md` is deliberately thin (#597): the reference detail moved out into the docs that own each
topic, and the area rules moved into nested `app/CLAUDE.md` / `frontend/CLAUDE.md`. That only works
while the *pointers* survive the move. They did not — #597 re-targeted file-level links but left
**28 section-level pointers aimed at `CLAUDE.md` sections it had just moved elsewhere**
(`*Calibration - three copies, one stamp*` -> `docs/OBJECTMODEL.md`, `*OME-ZARR dual-format*` ->
`docs/ARCHITECTURE.md`, `*Task system*` -> `app/CLAUDE.md`), plus 7 markdown links that resolved to
nothing and 6 doc paths cited from code that did not exist. Nothing noticed, because nothing checked; the PR claimed "all
internal links resolve" on the strength of a manual pass.

A dangling pointer is worse than no pointer: it reads as "the rule is written down over there", so
the reader stops looking, and the rule is re-derived.

What this asserts:

* **Every relative markdown link resolves** - across every tracked `.md`.
* **Every `docs/...`-shaped path cited from code resolves.** Only `docs/`-rooted paths, because that
  is the citation convention (`CLAUDE.md` -> *Where a note goes*: cite a `docs/<AREA>.md` section or a
  `docs/todo/X_PLAN.md` path). Bare `PLAN.md`-style names are reached by name and are checked by
  `test_doc_index_convention.py` instead.
* **Every `CLAUDE.md -> *Section*` pointer names a section that is actually in that `CLAUDE.md`.**
  Scoped to the three `CLAUDE.md` files ON PURPOSE. Those are the files whose content is actively
  being pushed down and out, so they are where pointers rot; and their headings are short and stable,
  so the check does not need to guess. Pointers into the big `docs/*.md` are NOT checked - 28 of them
  name a concept rather than a literal heading (`docs/UI.md` -> *Tooltips*), and failing those would
  make this test noise, which is how a guard gets muted.
* **Any doc over the 40 KB threshold `CLAUDE.md` itself sets carries a "slice it" hint** at the point
  of reference, in the doc index. The always-loaded file is where a session decides whether to read a
  102 KB doc whole; a rule stated once at the top is easy to walk past on row 14 of a table.
  A stated KB figure is checked loosely (25%) - a doc growing 2 KB must not fail CI, a doc that has
  doubled should.
* **Every nested `CLAUDE.md` is named in the root one.** Nested loading is harness behaviour, not
  repo behaviour; if it ever stops, the only thing keeping those rules reachable is that the root
  file points at them. A new nested file nobody linked would go quiet silently.

Run with `pixi run test-py`.
"""
import os
import re
import subprocess
import unittest

_REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))

#: Size above which `CLAUDE.md` -> *How to read the docs* says to read a slice, not the file.
_SLICE_KB = 40
#: How far a stated KB figure may drift before it is worth a failure.
_KB_TOLERANCE = 0.25

_CODE_GLOBS = ('*.jl', '*.ts', '*.vue', '*.py')
#: `[text](target)` with a relative target.
_LINK = re.compile(r'\[[^\]]*\]\(([^)\s]+)\)')
#: A `docs/...md` path cited in prose or a comment.
_DOC_PATH = re.compile(r'(?<![\w./-])(docs/[A-Za-z0-9_./-]*\.md)')
#: `CLAUDE.md` -> *Section* / `app/CLAUDE.md` => **Section**, in either arrow spelling. Matched over
#: the whole file, not per line: prose wraps, and one of these pointers hid across a line break.
_SECTION_PTR = re.compile(
    r'`?((?:app|frontend)/)?CLAUDE\.md`?[^\n]{0,12}?(?:→|->)\s*\*{1,2}([^*\n]+?)\*{1,2}', re.S)
#: A row of the doc index: `| [`docs/X.md`](docs/X.md) | ... |`
_INDEX_ROW = re.compile(r'^\|\s*\[`([^`]+)`\]\(([^)]+)\)\s*\|(.*?)\|\s*$', re.M)
_STATED_KB = re.compile(r'(\d+)\s*KB')

#: `docs/archive/` is explicitly not authoritative (`CLAUDE.md` -> *Where a note goes*), so a stale
#: pointer inside an archived brief is a record of what was asked, not a defect to fix.
_SKIP_DIRS = ('docs/archive/',)
#: This file itself: the docstring above has to spell out the pointer shapes being checked
#: (`docs/todo/X_PLAN.md`, `CLAUDE.md` -> *Section*), and every one of them is a placeholder.
#: It flagged itself the moment it was staged, which is at least evidence the matcher works.
_SKIP_FILES = (os.path.relpath(os.path.abspath(__file__), _REPO).replace(os.sep, '/'),)


def _git_ls(*globs):
    out = subprocess.check_output(['git', 'ls-files', *globs], cwd=_REPO).decode()
    return [p for p in out.split('\n')
            if p and not p.startswith(_SKIP_DIRS) and p not in _SKIP_FILES]


def _read(rel):
    with open(os.path.join(_REPO, rel), encoding='utf-8', errors='replace') as fh:
        return fh.read()


def _norm(text):
    return re.sub(r'[`*_.:]', '', text).strip().lower()


def _anchors(rel):
    """Headings AND bold lead-ins - the docs use `**Thing.**` as a subsection, and pointers cite both."""
    text = _read(rel)
    out = {_norm(h) for h in re.findall(r'^#{1,6}\s+(.*)$', text, re.M)}
    out |= {_norm(h) for h in re.findall(r'\*\*([^*\n]{3,90})\*\*', text)}
    return {a for a in out if a}


class DocPointerConventionTest(unittest.TestCase):
    def test_markdown_links_resolve(self):
        bad = []
        for rel in _git_ls('*.md'):
            for line_no, line in enumerate(_read(rel).split('\n'), 1):
                for target in _LINK.findall(line):
                    if target.startswith(('http://', 'https://', 'mailto:', '#')):
                        continue
                    path = target.split('#')[0]
                    if not path:
                        continue
                    resolved = os.path.normpath(os.path.join(os.path.dirname(rel), path))
                    if not os.path.exists(os.path.join(_REPO, resolved)):
                        bad.append(f'{rel}:{line_no} -> {target}')
        self.assertEqual([], bad, 'dangling markdown links:\n  ' + '\n  '.join(bad))

    def test_doc_paths_cited_from_code_resolve(self):
        bad = []
        for rel in _git_ls(*_CODE_GLOBS):
            for line_no, line in enumerate(_read(rel).split('\n'), 1):
                for path in _DOC_PATH.findall(line):
                    if os.path.exists(os.path.join(_REPO, path)):
                        continue
                    bad.append(f'{rel}:{line_no} -> {path}')
        self.assertEqual([], bad,
                         'code cites doc paths that do not exist (a sibling repo\'s doc needs its '
                         'prefix, e.g. `../coastal/docs/...`):\n  ' + '\n  '.join(bad))

    def test_claude_md_section_pointers_resolve(self):
        anchors = {rel: _anchors(rel) for rel in
                   ('CLAUDE.md', 'app/CLAUDE.md', 'frontend/CLAUDE.md')}
        bad = []
        for rel in _git_ls('*.md', *_CODE_GLOBS):
            text = _read(rel)
            for match in _SECTION_PTR.finditer(text):
                target = (match.group(1) or '') + 'CLAUDE.md'
                if target not in anchors:
                    continue
                section = ' '.join(match.group(2).split())
                key = _norm(section)
                if any(key == a or key in a for a in anchors[target]):
                    continue
                line_no = text.count('\n', 0, match.start()) + 1
                bad.append(f'{rel}:{line_no} -> {target} -> *{section}*')
        self.assertEqual([], bad,
                         'pointers into a CLAUDE.md section that is not there any more - find where '
                         'the section moved and re-target them:\n  ' + '\n  '.join(bad))

    def test_large_docs_are_flagged_where_they_are_referenced(self):
        missing, stale = [], []
        for _label, target, body in _INDEX_ROW.findall(_read('CLAUDE.md')):
            path = target.split('#')[0]
            full = os.path.join(_REPO, path)
            if not os.path.exists(full):
                continue
            kb = os.path.getsize(full) / 1024
            if kb <= _SLICE_KB:
                continue
            if 'slice' not in body.lower():
                missing.append(f'{path} ({kb:.0f} KB)')
                continue
            for stated in _STATED_KB.findall(body):
                if abs(int(stated) - kb) / kb > _KB_TOLERANCE:
                    stale.append(f'{path}: row says {stated} KB, file is {kb:.0f} KB')
        self.assertEqual([], missing,
                         f'doc-index rows for files over {_SLICE_KB} KB with no "slice it" hint - a '
                         'session reads the row, not the protocol at the top:\n  '
                         + '\n  '.join(missing))
        self.assertEqual([], stale, 'doc-index size figures have drifted:\n  ' + '\n  '.join(stale))

    def test_nested_claude_md_files_are_named_in_the_root(self):
        root = _read('CLAUDE.md')
        nested = [p for p in _git_ls('*CLAUDE.md') if p != 'CLAUDE.md']
        self.assertTrue(nested, 'no nested CLAUDE.md found - has the layout changed?')
        missing = [p for p in nested if p not in root]
        self.assertEqual([], missing,
                         'nested CLAUDE.md not referenced from the root file. Nested loading is '
                         'harness behaviour; the root pointer is what keeps these reachable if it '
                         'stops:\n  ' + '\n  '.join(missing))


if __name__ == '__main__':
    unittest.main()
