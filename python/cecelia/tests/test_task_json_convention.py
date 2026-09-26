"""Convention test: a task JSON with many top-level params collapses the less-critical ones behind
a `type: "section"` block.

Rule shape: if a task has more than `_MAX_TOPLEVEL` top-level entries in `params`, at least one of
them must be a `type: "section"` (collapsible advanced block). Without this, every knob is on-screen
by default and the beginner picker looks the same as an expert dashboard — the exact regression
`segment.ridges` shipped with (three tuning params — `threshold`, `darkRidges`, `minSizePx` — flat,
retrofitted into an `advanced` section post-hoc). Canonical shape: `segment.cellpose`,
`segment.coastal`, and the fixed `segment.ridges` — each puts advanced/tunable params inside a
collapsed section.

Threshold is deliberately conservative (7). Tasks up to 6 top-level params (the canonical
`segment.cellpose` shape) pass without a section; above that the assumption is that a picker
already needs advanced/expert separation.

Baseline: `segment.branching` — 15 top-level params, no section, shipped this way. Fold in when
the branching task gets re-visited; do not add new offenders.

Exemption: `# JSON-FLAT-OK: <reason>` on a single-line comment adjacent to the file has no meaning
(JSON has no comments). To exempt a file, add it to `_BASELINE` with a code-comment reason.

Run with `pixi run test-py`.
"""
import json
import os
import unittest

_REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))
_TASKS_ROOT = os.path.join(_REPO, 'app', 'src', 'tasks')

# Above this many top-level params, at least one `type: "section"` is required.
_MAX_TOPLEVEL = 6

# _BASELINE_MAX is a meta-ratchet: adding a file requires bumping the cap in the same PR, so a
# reviewer sees "weaken the check" attempts. See docs/todo/DRIFT_PREVENTION_ASSESSMENT.md.
_BASELINE_MAX = 5
_BASELINE = {
    # Every entry: shipped with a flat param list; can be split into a `type: "section"` "advanced"
    # block when the task is next touched. Not a regression — the ratchet exists to stop NEW tasks
    # from being added flat.
    os.path.join('app', 'src', 'tasks', 'segment', 'branching.json'),
    os.path.join('app', 'src', 'tasks', 'cleanupImages', 'drift_correct.json'),
    os.path.join('app', 'src', 'tasks', 'cleanupImages', 'flow_register.json'),
    os.path.join('app', 'src', 'tasks', 'cleanupImages', 'smooth.json'),
    os.path.join('app', 'src', 'tasks', 'clustRegions', 'cluster.json'),
}


def _task_json_files():
    for dirpath, dirnames, filenames in os.walk(_TASKS_ROOT):
        # `fragments/` holds composable param fragments (list-shaped JSON), not task specs.
        dirnames[:] = [d for d in dirnames if d != 'fragments']
        for fn in filenames:
            if not fn.endswith('.json'):
                continue
            yield os.path.relpath(os.path.join(dirpath, fn), _REPO)


def _load_task(rel):
    with open(os.path.join(_REPO, rel), encoding='utf-8') as f:
        data = json.load(f)
    return data if isinstance(data, dict) else None


class TaskJsonCollapseConventionTest(unittest.TestCase):
    def test_many_toplevel_params_require_a_section(self):
        offenders = []
        for rel in sorted(_task_json_files()):
            if rel in _BASELINE:
                continue
            try:
                spec = _load_task(rel)
            except json.JSONDecodeError:
                continue
            if spec is None:
                continue
            params = spec.get('params', [])
            if len(params) <= _MAX_TOPLEVEL:
                continue
            if any(p.get('type') == 'section' for p in params):
                continue
            offenders.append(f'{rel}: {len(params)} top-level params, no `type: "section"` — '
                             f'collapse the advanced/tuning ones behind a section, like '
                             f'`segment.cellpose` / `segment.coastal` / `segment.ridges` do')
        self.assertEqual(
            offenders, [],
            "a new task shipped with too many top-level params without an advanced section. Match "
            "the cellpose/coastal/ridges pattern (an `advanced` section with `collapsed: true`) or "
            "add the file to `_BASELINE` here with a reason:\n  "
            + '\n  '.join(offenders))

    def test_baseline_has_not_grown(self):
        """Meta-ratchet: growing _BASELINE means an agent added a file to skip a violation
        instead of fixing it. Bumping _BASELINE_MAX in the same PR makes that visible."""
        self.assertLessEqual(
            len(_BASELINE), _BASELINE_MAX,
            f'_BASELINE grew to {len(_BASELINE)} (cap {_BASELINE_MAX}). Either fix the new '
            f'violation, or bump _BASELINE_MAX and justify in the PR body. See '
            f'docs/todo/DRIFT_PREVENTION_ASSESSMENT.md.')

    def test_baseline_still_needed(self):
        stale = []
        for rel in sorted(_BASELINE):
            full = os.path.join(_REPO, rel)
            if not os.path.exists(full):
                stale.append(f'{rel}: file no longer exists — remove from `_BASELINE`')
                continue
            spec = _load_task(rel)
            if spec is None:
                stale.append(f'{rel}: not a task-spec dict — remove from `_BASELINE`')
                continue
            params = spec.get('params', [])
            if len(params) <= _MAX_TOPLEVEL:
                stale.append(f'{rel}: down to {len(params)} params — remove from `_BASELINE`')
                continue
            if any(p.get('type') == 'section' for p in params):
                stale.append(f'{rel}: now has a `type: "section"` — remove from `_BASELINE`')
        self.assertEqual(stale, [], '\n  ' + '\n  '.join(stale))


class TaskJsonScanCoverageTest(unittest.TestCase):
    """Guard against the scan silently covering nothing (a wrong root, no matches)."""
    def test_scan_reaches_known_task_json_files(self):
        known = {'cellpose.json', 'coastal.json', 'ridges.json', 'branching.json'}
        seen = {os.path.basename(rel) for rel in _task_json_files()}
        missing = known - seen
        self.assertEqual(missing, set(),
                         f'expected the scan to find {sorted(known)}, missing {sorted(missing)}')


if __name__ == '__main__':
    unittest.main()
