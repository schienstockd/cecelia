"""Every composite whose step can be previewed must itself have a preview backend.

`preview_worker._BACKENDS` dispatches on the `fun_name` the API sends, and the API sends the task the
USER ran. Module pages run the composite (`segment.cellposeMeasure`, `segment.coastalMeasure`), never
the bare segmenter — so a composite absent from `_BACKENDS` means the preview button is dead on the
page people actually use, and it fails with a `ValueError` naming the known backends rather than
anything a user can act on.

This has shipped broken twice. `app/src/tasks/task.jl` already warns about the class in its
CompositeTask header ("Forgetting one is SILENT and looks like 'the feature doesn't work for the
composite' — which is exactly how the live preview first shipped broken"), and the warning did not
prevent the second occurrence, because nothing checked. This does.

The keys are read from the SOURCE rather than by importing the worker: `preview_worker` pulls in
torch and cellpose at module scope, which would make a one-assertion convention test one of the
slowest in the suite.

Skipped when `app/` or `preview/` is absent — an external `pip install cecelia` consumer has neither.
"""
import ast
import json
import unittest
from pathlib import Path

_ROOT = Path(__file__).resolve().parents[3]
_WORKER = _ROOT / 'preview' / 'preview_worker.py'
_TASKS = _ROOT / 'app' / 'src' / 'tasks'


def _backend_keys():
    """The literal keys of the `_BACKENDS` dict, parsed from the source."""
    tree = ast.parse(_WORKER.read_text(encoding='utf-8'))
    for node in ast.walk(tree):
        if not isinstance(node, ast.Assign):
            continue
        if not any(isinstance(t, ast.Name) and t.id == '_BACKENDS' for t in node.targets):
            continue
        return {k.value for k in node.value.keys if isinstance(k, ast.Constant)}
    raise AssertionError('_BACKENDS not found in preview_worker.py')


def _canvas_plot_backends(candidates):
    """Which of `candidates` the optical-flow API names — canvas plots, not tasks.

    Membership rather than extraction: the route picks its `fun_name` with a ternary, so a
    `fun_name\s*=\s*"..."` pattern sees neither branch. Asking "does this file mention this backend
    by name" is both simpler and exactly the invariant — a backend is legitimate if something calls
    it.
    """
    src = (_ROOT / 'api' / 'src' / 'optical_flow_api.jl')
    if not src.is_file():
        return set()
    text = src.read_text(encoding='utf-8')
    return {k for k in candidates if f'"{k}"' in text}


def _composites():
    """`fun_name -> [step fun_names]` for every composite task spec."""
    out = {}
    for spec_path in _TASKS.glob('*/*.json'):
        try:
            spec = json.loads(spec_path.read_text(encoding='utf-8'))
        except json.JSONDecodeError:
            continue
        if not isinstance(spec, dict):     # not every JSON in a task dir is a task spec
            continue
        steps = spec.get('composite')
        fun = spec.get('fun_name')
        if steps and fun:
            out[fun] = list(steps)
    return out


@unittest.skipUnless(_WORKER.is_file() and _TASKS.is_dir(), 'app/ or preview/ not present')
class PreviewBackendCoverageTest(unittest.TestCase):
    def test_it_found_something_to_police(self):
        self.assertGreater(len(_backend_keys()), 3)
        self.assertGreater(len(_composites()), 1)

    def test_every_previewable_composite_has_a_backend(self):
        backends = _backend_keys()
        missing = [
            f'{fun} (step {step!r} is previewable)'
            for fun, steps in sorted(_composites().items())
            for step in steps
            if step in backends and fun not in backends
        ]
        self.assertEqual(missing, [], 'composites whose preview would raise "no preview backend"')

    def test_no_backend_names_a_task_that_does_not_exist(self):
        """The other direction: a renamed task leaves a dead entry, and the preview then fails with
        the same unactionable error while the registry looks complete."""
        funs = set()
        for spec_path in _TASKS.glob('*/*.json'):
            try:
                spec = json.loads(spec_path.read_text(encoding='utf-8'))
            except json.JSONDecodeError:
                continue
            if isinstance(spec, dict) and spec.get('fun_name'):
                funs.add(spec['fun_name'])
        # Some backends are CANVAS PLOTS, reached through /api/optical-flow/* rather than through a
        # task, so they legitimately have no spec. Read from the route that dispatches them instead
        # of allow-listed by name: an allow-list is a second place to remember, and the thing this
        # file exists to prevent is exactly a registry nobody remembered to update.
        funs |= _canvas_plot_backends(_backend_keys())
        unknown = sorted(k for k in _backend_keys() if k not in funs)
        self.assertEqual(unknown, [], 'preview backends naming no known task')


if __name__ == '__main__':
    unittest.main()
