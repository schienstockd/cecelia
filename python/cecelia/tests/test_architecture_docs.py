"""Convention test: the hand-kept parts of `docs/architecture/` still point at real code.

`docs/architecture/` is mostly generated (`pixi run arch-diagrams`). Two parts are kept by hand, and
hand-kept parts rot silently:

* **`system.md` → Pins** — every arrow of the system view names the file and symbol at each end.
  A pin whose file is gone, or whose file no longer contains the symbol, fails here. This catches a
  dead arrow; it cannot catch a new arrow nobody drew.
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
_SYSTEM = os.path.join(_REPO, 'docs', 'architecture', 'system.md')
_AREAS = os.path.join(_REPO, 'scripts', 'architecture', 'julia_areas.toml')


def _pins():
    with open(_SYSTEM, encoding='utf-8') as f:
        text = f.read()
    section = text.split('## Pins', 1)[1]
    rows = re.findall(r'^\|\s*(\d+)\s*\|\s*`([^`]+)`\s*\|\s*`(.+)`\s*\|\s*$', section, flags=re.M)
    return [(int(n), path, sym) for n, path, sym in rows]


class TestSystemPins(unittest.TestCase):
    def test_pins_parse(self):
        self.assertGreater(len(_pins()), 20, 'the Pins table did not parse — check its row format')

    def test_every_arrow_is_pinned(self):
        with open(_SYSTEM, encoding='utf-8') as f:
            arrows = {int(n) for n in re.findall(r'^\|\s*(\d+)\s*\|[^|]*→', f.read(), flags=re.M)}
        pinned = {n for n, _, _ in _pins()}
        self.assertEqual(arrows - pinned, set(), 'arrows in the table with no pin')

    def test_pinned_symbols_exist(self):
        missing = []
        for n, path, sym in _pins():
            full = os.path.join(_REPO, path)
            if not os.path.isfile(full):
                missing.append(f'arrow {n}: {path} (file gone)')
                continue
            with open(full, encoding='utf-8') as f:
                if sym not in f.read():
                    missing.append(f'arrow {n}: {path} has no `{sym}`')
        self.assertEqual(missing, [], 'system.md pins that no longer match the code — '
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
