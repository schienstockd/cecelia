"""Convention test: every zarr array we create must pass `zarr_utils.store_compressor`.

The bug this guards is silent and permanent-ish. `create_array`/`open_array` without a `compressor`
takes whatever the zarr version defaults to, which is how three different codecs ended up on disk with
no decision behind any of them: `blosc/lz4-5` from bioformats2raw and from anything written under
zarr-python 2, plain `zstd` from everything written since the zarr 3 migration. Measured on real
16-bit acquisition data, the default is 33% larger than the canonical choice for no read-speed
benefit — and nothing ever fails, so it is invisible until someone measures a projects dir.

Matching is over the AST, not raw text, so a comment or docstring naming a writer is not an offender.

Run with `pixi run test-py`.
"""
import ast
import os
import unittest

_REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))

# Array-creating calls. `create_dataset` is zarr 2's spelling — banned outright elsewhere, listed here
# so a copy-pasted old snippet is caught by this test too rather than only at runtime.
_CREATORS = {'create_array', 'create_dataset'}
_CHOOSER = 'store_compressor'

_SEARCH_DIRS = (
    os.path.join('python', 'cecelia'),
    os.path.join('app', 'src'),
    'napari',
    'preview',
)


def _iter_sources():
    for rel in _SEARCH_DIRS:
        root = os.path.join(_REPO, rel)
        if not os.path.isdir(root):
            continue
        for dirpath, dirnames, filenames in os.walk(root):
            dirnames[:] = [d for d in dirnames if d != 'tests' and not d.startswith('.')]
            for f in filenames:
                if f.endswith('.py'):
                    path = os.path.join(dirpath, f)
                    yield os.path.relpath(path, _REPO), path


def _offenders(tree):
    """Calls to a creator that pass no `compressor=` (and aren't an `open_array(mode='r')` read)."""
    bad = []
    for node in ast.walk(tree):
        if not isinstance(node, ast.Call):
            continue
        name = node.func.attr if isinstance(node.func, ast.Attribute) else \
            (node.func.id if isinstance(node.func, ast.Name) else None)
        if name == 'open_array':
            # reads vastly outnumber writes; only a write-mode open creates an array
            modes = [kw.value.value for kw in node.keywords
                     if kw.arg == 'mode' and isinstance(kw.value, ast.Constant)]
            if not modes or all(m == 'r' for m in modes):
                continue
        elif name not in _CREATORS:
            continue
        if not any(kw.arg == 'compressor' for kw in node.keywords):
            bad.append((name, node.lineno))
    return bad


class StoreCompressorConventionTest(unittest.TestCase):
    def test_every_array_creation_sets_the_compressor(self):
        found = []
        for rel, path in _iter_sources():
            with open(path, encoding='utf-8') as fh:
                try:
                    tree = ast.parse(fh.read(), filename=path)
                except SyntaxError:                       # not ours to police
                    continue
            for name, lineno in _offenders(tree):
                found.append(f'{rel}:{lineno}: {name}(…) without compressor=')
        self.assertEqual(found, [], 'pass compressor=zarr_utils.store_compressor(kind):\n' +
                                    '\n'.join(found))

    def test_the_chooser_is_the_only_source_of_a_codec(self):
        """No writer may construct a Blosc/Zstd codec itself — the point of one chooser is that the
        codec is decided in one place, so a second construction site is the divergence starting."""
        found = []
        owner = os.path.join('python', 'cecelia', 'utils', 'zarr_utils.py')
        for rel, path in _iter_sources():
            if rel == owner:
                continue
            with open(path, encoding='utf-8') as fh:
                try:
                    tree = ast.parse(fh.read(), filename=path)
                except SyntaxError:
                    continue
            for node in ast.walk(tree):
                if isinstance(node, ast.Call):
                    name = node.func.attr if isinstance(node.func, ast.Attribute) else \
                        (node.func.id if isinstance(node.func, ast.Name) else None)
                    if name in ('Blosc', 'Zstd', 'LZ4', 'GZip'):
                        found.append(f'{rel}:{node.lineno}: {name}(…)')
        self.assertEqual(found, [], f'build the codec via {_CHOOSER} in zarr_utils:\n' +
                                    '\n'.join(found))

    def test_the_chooser_rejects_an_unknown_kind(self):
        from cecelia.utils import zarr_utils
        with self.assertRaises(ValueError):
            zarr_utils.store_compressor('intensity')

    def test_images_get_the_shuffle_filter_and_labels_do_not(self):
        """The measured split — see the constants in zarr_utils. Byte shuffle is what compresses
        16-bit intensity data (near-constant high byte); on >99%-zero label planes blosc's blocking
        and the shuffle both break up the long zero runs plain zstd exploits."""
        from cecelia.utils import zarr_utils
        img = zarr_utils.store_compressor('image').get_config()
        lbl = zarr_utils.store_compressor('labels').get_config()
        self.assertEqual(img['id'], 'blosc')
        self.assertEqual(img['cname'], 'zstd')
        self.assertEqual(img['shuffle'], 1)          # numcodecs Blosc.SHUFFLE
        self.assertEqual(lbl['id'], 'zstd')          # plain zstd, NOT blosc-wrapped
        self.assertNotIn('shuffle', lbl)


if __name__ == '__main__':
    unittest.main()
