"""A quote of a `pixi.toml` comment, kept in a docstring or comment elsewhere, must still be in `pixi.toml`.

`pixi.toml` comments carry real claims (the coastal floor, what checks it, what does not), and a
docstring that quotes one word for word is a second copy of that claim. When the comment is rewritten,
the copy rots silently: `test_coastal_contract` kept quoting the coastal floor's admission that no
check covered its calls after `pixi.toml` dropped that sentence and named `test_coastal_contract`
itself as the check.

Scope is Python prose only: docstrings and `#` comments in tracked `.py` files, where a quote of
another file is prose rather than a string the code uses. A quote counts when its sentence attributes
it to `pixi.toml` (names the file, or says "described there" / "its comment" inside a paragraph
that names it) and it runs to four words or more. Whitespace and the comment markers are normalised
on both sides, so rewrapping either file does not trip it.
"""
import ast
import io
import pathlib
import re
import subprocess
import tokenize
import unittest

REPO = pathlib.Path(__file__).resolve().parents[3]

_ATTRIBUTION = re.compile(r'pixi\.toml|described there|says there|its (?:own )?comment', re.I)


def _norm(text):
    return re.sub(r'\s+', ' ', text).strip()


def _pixi_comment_text():
    lines = (REPO / 'pixi.toml').read_text(encoding='utf-8').splitlines()
    return _norm(' '.join(re.sub(r'^\s*#\s?', '', line) for line in lines))


def _prose_blocks(source):
    """Docstrings, plus each run of consecutive `#` comment lines, as normalised text."""
    blocks = []
    for node in ast.walk(ast.parse(source)):
        if isinstance(node, (ast.Module, ast.ClassDef, ast.FunctionDef, ast.AsyncFunctionDef)):
            doc = ast.get_docstring(node, clean=True)
            if doc:
                blocks.extend(re.split(r'\n\s*\n', doc))
    run, last_row = [], None
    for tok in tokenize.generate_tokens(io.StringIO(source).readline):
        if tok.type != tokenize.COMMENT:
            continue
        if last_row is not None and tok.start[0] != last_row + 1:
            blocks.append(' '.join(run))
            run = []
        run.append(tok.string.lstrip('#'))
        last_row = tok.start[0]
    if run:
        blocks.append(' '.join(run))
    return [_norm(b) for b in blocks]


def _attributed_quotes(block):
    if 'pixi.toml' not in block:
        return []
    quotes = []
    for sentence in re.split(r'(?<=\.)\s+(?=[A-Z`*])', block):
        if _ATTRIBUTION.search(sentence):
            quotes += [q for q in re.findall(r'"([^"]+)"', sentence) if len(q.split()) >= 4]
    return quotes


def _tracked_python():
    # Untracked too, so a new file is checked before its first commit, not only in CI.
    out = subprocess.run(['git', 'ls-files', '--cached', '--others', '--exclude-standard', '*.py'],
                         cwd=REPO, capture_output=True, text=True)
    if out.returncode != 0:
        raise unittest.SkipTest('not a git checkout — nothing to list')
    return [REPO / p for p in out.stdout.split()]


class TestPixiCommentQuotes(unittest.TestCase):

    def test_detector_sees_the_shape(self):
        """Guard the guard: the stale-docstring shape above must be flagged, and a live quote must not."""
        stale = ('"""Its own comment in `pixi.toml` names the hole: *"Nothing catches a rollback past the '
                 '`segment.coastal` / `opticalFlow.train` calls above."*"""\n')
        self.assertEqual([q.rstrip('.') for q in _attributed_quotes(_prose_blocks(stale)[0])],
                         ['Nothing catches a rollback past the `segment.coastal` / `opticalFlow.train` calls above'])
        comment = '# the pin in `pixi.toml` is described there as "a HARD floor, not a\n#   snapshot".\nx = 1\n'
        self.assertEqual(_attributed_quotes(_prose_blocks(comment)[0]), ['a HARD floor, not a snapshot'])

    def test_every_quote_of_pixi_toml_is_still_there(self):
        pixi = _pixi_comment_text()
        stale = []
        for path in _tracked_python():
            try:
                source = path.read_text(encoding='utf-8')
                blocks = _prose_blocks(source)
            except (OSError, SyntaxError, UnicodeDecodeError, tokenize.TokenError):
                continue
            for block in blocks:
                for quote in _attributed_quotes(block):
                    if _norm(quote).rstrip('.') not in pixi:
                        stale.append(f'{path.relative_to(REPO)}: "{quote}"')
        self.assertEqual(stale, [], 'quoted pixi.toml text that pixi.toml no longer says — '
                                    'quote the current comment, or paraphrase what it used to say')


if __name__ == '__main__':
    unittest.main()
