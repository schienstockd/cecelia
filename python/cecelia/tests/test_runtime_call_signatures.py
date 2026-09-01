"""Every `cecelia.utils.*` call in a runtime process must match the library's real signature.

The runtime process (`preview/preview_worker.py`) is the only Python in the repo that is NOT
imported by any test — the worker needs a socket. So a parameter removed from a `cecelia.utils`
helper leaves its call sites red-free: nothing binds them until a user actually clicks.

The check is static: parse each runtime source, resolve its `cecelia.utils` import aliases, and
bind every call against the real `inspect.signature`. Calls that splat (`*args`/`**kwargs`) are
skipped — a static bind can say nothing about them.

Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import ast
import importlib
import inspect
import os
import unittest

_REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))

# The runtime processes: they import the library but no test imports them.
_SOURCES = (
    os.path.join(_REPO, 'preview', 'preview_worker.py'),
)


def _import_aliases(tree):
    """Map the source's local names → the `cecelia.utils` object they refer to.

    Two shapes reach a module alias (`import cecelia.utils.zarr_utils as zarr_utils`,
    `from cecelia.utils import napari_utils`) and one reaches a function directly
    (`from cecelia.utils.block_transfer import encode_block`). Anything not under `cecelia.utils` is
    ignored — this test is about the library boundary, not every call in the file.
    """
    aliases = {}
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            for a in node.names:
                if a.name.startswith('cecelia.utils') and a.asname:
                    aliases[a.asname] = importlib.import_module(a.name)
        elif isinstance(node, ast.ImportFrom):
            if not (node.module or '').startswith('cecelia.utils') or node.level:
                continue
            mod = importlib.import_module(node.module)
            for a in node.names:
                obj = getattr(mod, a.name, None)
                if obj is None:
                    # A SUBMODULE is not an attribute of its package until something imports it, so
                    # `from cecelia.utils import napari_utils` resolves to None on a bare getattr —
                    # which silently dropped every bridge call this test exists to check.
                    try:
                        obj = importlib.import_module(f'{node.module}.{a.name}')
                    except ImportError:
                        continue
                aliases[a.asname or a.name] = obj
    return aliases


def _target(call, aliases):
    """The library callable this Call node resolves to, or None if it isn't one.

    `napari_utils.record_timelapse(...)` → attribute on a module alias; `encode_block(...)` → a
    directly imported function.
    """
    f = call.func
    if isinstance(f, ast.Attribute) and isinstance(f.value, ast.Name):
        mod = aliases.get(f.value.id)
        if inspect.ismodule(mod):
            return getattr(mod, f.attr, None)
    elif isinstance(f, ast.Name):
        obj = aliases.get(f.id)
        if inspect.isfunction(obj) or inspect.isclass(obj):
            return obj
    return None


class TestRuntimeCallSignatures(unittest.TestCase):
    def test_library_calls_bind(self):
        checked = 0
        failures = []
        for path in _SOURCES:
            self.assertTrue(os.path.exists(path), f'runtime source moved: {path}')
            with open(path, encoding='utf-8') as fh:      # cp1252 default would crash on Windows
                tree = ast.parse(fh.read(), filename=path)
            aliases = _import_aliases(tree)
            self.assertTrue(aliases, f'no cecelia.utils imports resolved in {path} — did they move?')

            for call in (n for n in ast.walk(tree) if isinstance(n, ast.Call)):
                fn = _target(call, aliases)
                if fn is None or not callable(fn):
                    continue
                # A splat hides the real arguments — a static bind would be meaningless.
                if any(isinstance(a, ast.Starred) for a in call.args) or \
                   any(k.arg is None for k in call.keywords):
                    continue
                try:
                    sig = inspect.signature(fn)
                except (TypeError, ValueError):           # builtins / C-implemented
                    continue
                checked += 1
                try:
                    sig.bind(*[None] * len(call.args), **{k.arg: None for k in call.keywords})
                except TypeError as e:
                    failures.append(
                        f'{os.path.relpath(path, _REPO)}:{call.lineno} '
                        f'{getattr(fn, "__name__", fn)}{sig} — {e}')

        self.assertEqual(failures, [], 'runtime call sites disagree with the library:\n  ' +
                                      '\n  '.join(failures))
        # A resolver that quietly stops matching would make this suite vacuous.
        self.assertGreater(checked, 20, f'only {checked} library calls resolved — resolver broke')


if __name__ == '__main__':
    unittest.main()
