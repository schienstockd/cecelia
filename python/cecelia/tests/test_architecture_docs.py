"""Convention test: the hand-kept parts of `docs/architecture/` still point at real code.

`docs/architecture/` is mostly generated (`pixi run arch-diagrams`). Two parts are kept by hand, and
hand-kept parts rot silently:

* **Pins** — `system.md` and every `flows/*.md` sequence diagram name the file and symbol behind each
  arrow or step in a *Pins* table. A pin whose file is gone, or whose file no longer contains the
  symbol, fails here. This catches a dead arrow; it cannot catch a new arrow nobody drew.
* **`scripts/architecture/julia_areas.toml`** — the grouping of Julia files into areas. A new
  `app/src` or `api/src` file no pattern matches would vanish from the area view, so it fails here and
  forces a decision in the toml.

Run with `pixi run test-py`.
"""
import os
import re
import tomllib
import unittest

_REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))
_ARCH = os.path.join(_REPO, 'docs', 'architecture')
_SYSTEM = os.path.join(_ARCH, 'system.md')
_FLOWS = os.path.join(_ARCH, 'flows')
_AREAS = os.path.join(_REPO, 'scripts', 'architecture', 'julia_areas.toml')


def _pinned_docs():
    docs = [_SYSTEM]
    if os.path.isdir(_FLOWS):
        docs += sorted(os.path.join(_FLOWS, f) for f in os.listdir(_FLOWS) if f.endswith('.md'))
    return docs


def _pins(doc=_SYSTEM):
    with open(doc, encoding='utf-8') as f:
        text = f.read()
    section = text.split('## Pins', 1)[1]
    # A row is a numbered step/arrow, or a `note` (a side path the diagram does not draw) — both pinned.
    rows = re.findall(r'^\|\s*(\d+|[a-z]+)\s*\|\s*`([^`]+)`\s*\|\s*`(.+)`\s*\|\s*$', section, flags=re.M)
    return [(int(n) if n.isdigit() else n, path, sym.replace('\\|', '|')) for n, path, sym in rows]


class TestSystemPins(unittest.TestCase):
    def test_pins_parse(self):
        for doc in _pinned_docs():
            self.assertGreater(len(_pins(doc)), 5, f'{doc}: the Pins table did not parse — check its row format')

    def test_every_flow_step_is_pinned(self):
        for doc in _pinned_docs()[1:]:
            with open(doc, encoding='utf-8') as f:
                text = f.read()
            diagram = text.split('```mermaid', 1)[1].split('```', 1)[0]
            # autonumber: every message line (`A->>B`, `A-->>B`, `A-)B`) is one numbered step
            steps = len(re.findall(r'^\s*\w[\w ]*?(?:-{1,2}>>|-{1,2}\)|-{1,2}x)', diagram, flags=re.M))
            pinned = {n for n, _, _ in _pins(doc)}
            self.assertEqual(set(range(1, steps + 1)) - pinned, set(), f'{os.path.basename(doc)}: steps with no pin')

    def test_every_arrow_is_pinned(self):
        with open(_SYSTEM, encoding='utf-8') as f:
            arrows = {int(n) for n in re.findall(r'^\|\s*(\d+)\s*\|[^|]*→', f.read(), flags=re.M)}
        pinned = {n for n, _, _ in _pins()}
        self.assertEqual(arrows - pinned, set(), 'arrows in the table with no pin')

    def test_pinned_symbols_exist(self):
        missing = []
        for doc, n, path, sym in [(d, *p) for d in _pinned_docs() for p in _pins(d)]:
            full = os.path.join(_REPO, path)
            if not os.path.isfile(full):
                missing.append(f'{os.path.basename(doc)} #{n}: {path} (file gone)')
                continue
            with open(full, encoding='utf-8') as f:
                if sym not in f.read():
                    missing.append(f'{os.path.basename(doc)} #{n}: {path} has no `{sym}`')
        self.assertEqual(missing, [], 'pins that no longer match the code — '
                                      'redraw the arrow or move the pin')


class TestJuliaAreas(unittest.TestCase):
    def test_every_julia_file_has_an_area(self):
        with open(_AREAS, 'rb') as f:
            patterns = [p for ps in tomllib.load(f)['areas'].values() for p in ps]

        def mapped(rel):
            return any(rel.startswith(p) if (p.endswith('/') or not p.endswith('.jl')) else rel == p
                       for p in patterns)
        unmapped = []
        for root in ('app/src', 'api/src'):
            for d, _, fs in os.walk(os.path.join(_REPO, root)):
                for f in fs:
                    if f.endswith('.jl'):
                        rel = os.path.relpath(os.path.join(d, f), _REPO).replace(os.sep, '/')
                        if not mapped(rel):
                            unmapped.append(rel)
        self.assertEqual(unmapped, [], 'add these to scripts/architecture/julia_areas.toml')


if __name__ == '__main__':
    unittest.main()
