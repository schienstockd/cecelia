"""Test value check — the mechanical recital step that asks whether the change's tests can fail when
its code is wrong (`docs/todo/TEST_VALUE_PLAN.md` → P4). Two findings, both outcome-tagged:

- **passes on base** (P4(a)): the change edits source code and adds tests, and none of the added tests
  fails on the code before the change (`HEAD`). CLAUDE.md: "A new test is evidence only once you have
  watched it fail." Python: the added test methods run in an export of `HEAD`'s tree with the staged
  test files on top. Julia: the staged suite files with added tests run in one process after the
  changed `app/src` files are put back to their `HEAD` text (`scripts/julia_on_base.jl`).
- **untested** (P4(b)): the change touches a result path and mutants on its changed lines survive the
  tests that name the changed function (`scripts/mutate_changed.py --staged`, at most `SAMPLE` mutants a
  file, `BUDGET_S` in all). One finding per function, listing up to three survivors.

Both are claims to check, not verdicts: a refactor's tests pass on base by design, and a mutant can be
equivalent (it changes nothing a caller can see). The tag says which; the reason is the record the
two-week review reads (TEST_VALUE_PLAN.md → P4). The weekly judge sweeps every `untested` one, tagged
`fixed_pre_commit` or not: it mutates the function again at main and judges the survivors
(`scripts/judge/bugs.py`). `passes on base` isn't swept: after the merge there is no base.

`run_test_value_check` is what recital calls; it runs in a thread beside the reviewers. The
subprocess seams (`on_base`, `mutate`) are injectable, so tests never start Julia or mutmut.
"""
from __future__ import annotations

import ast
import hashlib
import io
import json
import os
import pathlib
import re
import subprocess
import sys
import tarfile
import tempfile
import time
import typing as _t

from .git_context import FileDiff, git_dir, git_output, parse_diff, repo_root
from .log import append_event
from cecelia.utils.atomic_io import write_json_atomic

MECHANISM = "test_value"
TITLE = "Test value"
PASSES_ON_BASE = "passes on base"
UNTESTED = "untested"
#: Mutants per changed result file, drawn with a fixed seed (`mutate_changed.py --sample`).
SAMPLE = 20
#: Seconds after which no further file or Julia mutant starts (`--budget`). It runs beside the
#: reviewers, so most of it is hidden; P4(b)'s five PRs took 27 s – 6 min unsampled.
BUDGET_S = 480.0
#: Survivors quoted per finding.
QUOTED = 3

_PY_TEST = re.compile(r"^python/cecelia/tests/test_\w+\.py$")
_JL_TEST = re.compile(r"^app/test/suite/[\w-]+\.jl$")
_JL_ADDED_TEST = re.compile(r"^\s*@test(set|_throws|_broken|_skip)?\b")


def is_test_path(path: str) -> bool:
    return "/test/" in path or "/tests/" in path or path.endswith(".test.ts")


def source_changes(files: _t.Sequence[FileDiff]) -> list[str]:
    """Changed Julia/Python source the suites run (`app/src`, `python/cecelia`), tests excluded."""
    return [f.path for f in files if not f.is_deleted and not is_test_path(f.path)
            and (f.path.startswith(("app/src/", "python/cecelia/"))) and f.path.endswith((".jl", ".py"))]


def added_python_tests(f: FileDiff, staged_text: str) -> list[str]:
    """`module.Class.test_x` ids of the test methods whose `def` line the diff adds."""
    added = {ln.lineno for ln in f.added}
    module = f.path[len("python/"):-3].replace("/", ".")
    ids = []
    for node in ast.parse(staged_text).body:
        if isinstance(node, ast.ClassDef):
            ids += [f"{module}.{node.name}.{fn.name}" for fn in node.body
                    if isinstance(fn, (ast.FunctionDef, ast.AsyncFunctionDef))
                    and fn.name.startswith("test") and fn.lineno in added]
    return ids


def adds_julia_tests(f: FileDiff) -> bool:
    return any(_JL_ADDED_TEST.match(ln.text) for ln in f.added)


class Added(_t.NamedTuple):
    python: dict[str, list[str]]   # test file → added test ids
    julia: list[str]               # suite files that add a @test / @testset
    first: tuple[str, int]         # where the first added test sits, for the finding's location


def added_tests(files: _t.Sequence[FileDiff], staged: _t.Callable[[str], str]) -> Added | None:
    py, jl, first = {}, [], None
    for f in files:
        if f.is_deleted:
            continue
        if _PY_TEST.match(f.path):
            ids = added_python_tests(f, staged(f.path))
            if ids:
                py[f.path] = ids
                first = first or (f.path, f.added[0].lineno)
        elif _JL_TEST.match(f.path) and adds_julia_tests(f):
            jl.append(f.path)
            first = first or (f.path, next(ln.lineno for ln in f.added if _JL_ADDED_TEST.match(ln.text)))
    return Added(py, jl, first) if first else None


# ---------------------------------------------------------------- running on base

def _staged(root: pathlib.Path) -> _t.Callable[[str], str]:
    return lambda path: git_output("show", f":{path}", cwd=str(root)) or ""


# ---------------------------------------------------------------- the round cache

#: Per worktree, `<git dir>/test-value-cache/`: on-base verdicts and per-file mutation rows, keyed by
#: the content of everything they read, so a recital round over unchanged code reuses the last result
#: in seconds instead of minutes (a Julia file is a package load and its mutants). Approximate by
#: design: a per-file key holds that file, its changed lines and the tests mapped to it, not the
#: rest of the package, so a change elsewhere that alters what those tests see isn't a miss.
CACHE_DIR = "test-value-cache"
#: Entries kept; the oldest go first.
CACHE_KEEP = 200
#: The tools whose code is part of every key: a change to them is a miss.
_TOOLS = ("scripts/mutate_changed.py", "scripts/julia_mutate.jl", "scripts/julia_on_base.jl",
          "python/cecelia/effectiveness/test_value.py")


def cache_dir(root: pathlib.Path) -> pathlib.Path | None:
    g = git_dir(str(root))
    return g / CACHE_DIR if g else None


def content_key(root: pathlib.Path, *parts: str) -> str:
    """A key over `parts` and the tools' own text."""
    h = hashlib.sha256()
    for p in (*parts, *(_read(root / t) for t in _TOOLS)):
        h.update(p.encode("utf-8", "replace") + b"\0")
    return h.hexdigest()[:32]


def _read(path: pathlib.Path) -> str:
    try:
        return path.read_text(encoding="utf-8", errors="replace")
    except OSError:
        return ""


def cache_get(d: pathlib.Path | None, key: str) -> dict | None:
    try:
        return json.loads((d / f"{key}.json").read_text(encoding="utf-8")) if d else None
    except (OSError, ValueError):
        return None


def cache_put(d: pathlib.Path | None, key: str, value: dict) -> None:
    """Best effort: a cache that can't be written is a slower next round, never a failed one."""
    if d is None:
        return
    try:
        d.mkdir(parents=True, exist_ok=True)
        write_json_atomic(d / f"{key}.json", value)
        old = sorted(d.glob("*.json"), key=lambda f: f.stat().st_mtime)[:-CACHE_KEEP]
        for f in old:
            f.unlink(missing_ok=True)
    except OSError:
        pass


def _head_text(root: pathlib.Path, path: str) -> str:
    """`path` at HEAD, or "" for a file the change adds."""
    return git_output("show", f"HEAD:{path}", cwd=str(root)) or ""


#: The trees a Python on-base run takes from HEAD; the rest of the checkout is linked in.
_BASE_TREES = ("python", "app", "scripts")


def python_on_base(root: pathlib.Path, tests: dict[str, list[str]], staged: _t.Callable[[str], str]) -> bool:
    """True when at least one of `tests` fails (or cannot import) on HEAD's code. HEAD's `python/`,
    `app/` and `scripts/` are exported (tests load `scripts/` modules by path, so a linked one would be
    the change's); the staged test files go on top; the rest of the checkout is linked in, since tests
    find fixtures three levels above their file."""
    with tempfile.TemporaryDirectory(prefix="test_value_base_") as tmp:
        dest = pathlib.Path(tmp)
        tar = subprocess.run(["git", "archive", "--format=tar", "HEAD", *_BASE_TREES], cwd=root,
                             capture_output=True, check=True).stdout
        with tarfile.open(fileobj=io.BytesIO(tar)) as t:
            t.extractall(dest, filter="data")
        for d in root.iterdir():
            if d.name not in (*_BASE_TREES, ".git", ".pixi") and not (dest / d.name).exists():
                (dest / d.name).symlink_to(d)
        for path in tests:
            (dest / path).write_text(staged(path), encoding="utf-8")
        env = os.environ | {"PYTHONPATH": str(dest / "python")}
        ids = [i for v in tests.values() for i in v]
        p = subprocess.run([sys.executable, "-m", "unittest", *ids], cwd=dest / "python", env=env,
                           capture_output=True, text=True, encoding="utf-8", errors="replace")
        return p.returncode != 0


#: The package's root file: `include`s and `export`s, no methods of its own. Its old text can't be
#: loaded into the running package (it includes every file again), so the swap leaves it as it is.
_MODULE_ROOT = "app/src/Cecelia.jl"


class CannotJudge(Exception):
    """The added tests couldn't be run on the code before the change: no verdict either way."""


def julia_on_base(root: pathlib.Path, sources: _t.Sequence[str], suites: _t.Sequence[str]) -> bool:
    """True when the staged `suites` fail anywhere with the changed `app/src` files at their HEAD text.
    Raises `CannotJudge` when only the module root changed, or a HEAD text wouldn't load: the
    deleted methods then stay missing, so a failure would say nothing about the change."""
    swapped = [p for p in sources if p.startswith("app/src/") and p.endswith(".jl") and p != _MODULE_ROOT]
    if not swapped:
        raise CannotJudge(f"only `{_MODULE_ROOT}` changed, and its old text can't be loaded")
    with tempfile.TemporaryDirectory(prefix="test_value_base_") as tmp:
        pairs = []
        for i, path in enumerate(swapped):
            base = pathlib.Path(tmp) / f"{i}.jl"
            base.write_text(_head_text(root, path), encoding="utf-8")
            pairs.append(f"{root / path}={base}")
        out = pathlib.Path(tmp) / "out.txt"
        subprocess.run(["julia", f"--project={root / 'app'}", str(root / "scripts/julia_on_base.jl"), str(out),
                        *pairs, "--", *(str(root / s) for s in suites)],
                       cwd=root, capture_output=True, text=True, encoding="utf-8", errors="replace")
        if not out.is_file():
            raise RuntimeError("julia_on_base.jl wrote nothing")
        lines = out.read_text(encoding="utf-8").splitlines()
    broke = [ln[len("base-error "):] for ln in lines if ln.startswith("base-error ")]
    if broke:
        raise CannotJudge("the code before it wouldn't load: " + broke[0].replace(str(root) + "/", "")[:200])
    return bool(lines)


def default_on_base(root: pathlib.Path, added: Added, sources: _t.Sequence[str],
                    staged: _t.Callable[[str], str]) -> dict:
    """Which languages' added tests fail on base: `{"py": bool, "jl": bool}` for the ones run, and
    `unjudged: {lang: why}` for one that couldn't be (`CannotJudge`). Cached on HEAD plus the staged
    text of every source and test file (`cache_dir`): a round over the same change reuses it."""
    d = cache_dir(root)
    paths = sorted({*sources, *added.python, *added.julia})
    key = content_key(root, "on_base", git_output("rev-parse", "HEAD", cwd=str(root)) or "",
                      *(f"{p}\n{staged(p)}" for p in paths))
    hit = cache_get(d, key)
    if hit is not None:
        return {**hit, "cached": True}
    out: dict = {}
    if added.python and any(s.endswith(".py") for s in sources):
        out["py"] = python_on_base(root, added.python, staged)
    if added.julia and any(s.startswith("app/src/") and s.endswith(".jl") for s in sources):
        try:
            out["jl"] = julia_on_base(root, sources, added.julia)
        except CannotJudge as e:
            out.setdefault("unjudged", {})["jl"] = str(e)
    cache_put(d, key, out)
    return out


def run_mutate_changed(root: pathlib.Path, mode: _t.Sequence[str], *, python: _t.Sequence[str] = (sys.executable,),
                       env: dict | None = None, timeout: float | None = None) -> dict:
    """`mutate_changed.py <mode>`'s rows (`{"total_secs", "files"}`), run in checkout `root` by `python`
    (default this interpreter; the weekly judge gives the pinned worktree's `pixi run … python`).
    `SAMPLE` mutants a file, `BUDGET_S` in all."""
    with tempfile.TemporaryDirectory(prefix="test_value_mut_") as tmp:
        js = pathlib.Path(tmp) / "rows.json"
        p = subprocess.run([*python, str(root / "scripts/mutate_changed.py"), *mode,
                            "--sample", str(SAMPLE), "--budget", str(BUDGET_S), "--json", str(js), "--out", tmp],
                           cwd=root, capture_output=True, text=True, encoding="utf-8", errors="replace",
                           env=env, timeout=timeout)
        if not js.is_file():
            raise RuntimeError((p.stderr or p.stdout).strip()[-300:] or "mutate_changed.py wrote nothing")
        return json.loads(js.read_text(encoding="utf-8"))


def default_mutate(root: pathlib.Path) -> dict:
    """`mutate_changed.py --staged`'s rows, its per-file results cached across rounds (`cache_dir`)."""
    d = cache_dir(root)
    return run_mutate_changed(root, ["--staged", *(["--cache", str(d)] if d else [])])


def quote_survivors(svs: _t.Sequence[dict], n: int = QUOTED) -> str:
    """The first `n` survivors as ``line N `orig` → `new` `` (`survivor_detail` rows), `(+k more)` after."""
    quoted = "; ".join(f"line {d['line']} `{d['orig']}` → `{d['new']}`" for d in svs[:n])
    return quoted + (f" (+{len(svs) - n} more)" if len(svs) > n else "")


# ---------------------------------------------------------------- the section

def _bullet(file: str, line: int, text: str, marker: str) -> str:
    return f"- **{file}:{line}** — {text} [**{marker}**]"


_LANG_NAMES = {"py": "Python", "jl": "Julia"}


def unjudged_notes(failed: dict) -> list[str]:
    """One line per language whose added tests couldn't run on base: no verdict, said, not silent."""
    return [f"_{_LANG_NAMES[k]} on base: not judged — {why}_" for k, why in (failed.get("unjudged") or {}).items()]


def base_bullet(added: Added, failed: dict) -> str | None:
    """The `passes on base` bullet when no language's added tests failed on base (`failed`'s `py` /
    `jl`; one that couldn't be judged isn't in it)."""
    failed = {k: v for k, v in (failed or {}).items() if k in _LANG_NAMES}
    if not failed or any(failed.values()):
        return None
    names = [i.rsplit(".", 1)[-1] for v in added.python.values() for i in v] if "py" in failed else []
    names += [pathlib.Path(s).name for s in added.julia] if "jl" in failed else []
    shown = ", ".join(f"`{n}`" for n in names[:4]) + (f" and {len(names) - 4} more" if len(names) > 4 else "")
    return _bullet(*added.first, f"the tests this change adds ({shown}) all pass on the code before it, "
                   "so none of them shows the change works. Make one fail there (watch it fail); a "
                   "refactor that keeps behaviour is `false_positive: refactor`.", PASSES_ON_BASE)


def mutation_bullets(rows: _t.Sequence[dict]) -> list[str]:
    """One `untested` bullet per changed function with surviving mutants."""
    out = []
    for r in rows:
        by_fn: dict[str, list[dict]] = {}
        for d in r.get("survivor_detail") or []:
            by_fn.setdefault(d["fn"], []).append(d)
        tried = len(r.get("results") or [])
        for fn, svs in by_fn.items():
            out.append(_bullet(r["file"], svs[0]["def"],
                               f"`{fn}`: {len(svs)} of {tried} mutants on this change's lines survive the tests "
                               f"that name it or a caller in its file: {quote_survivors(svs)}. Add a test whose expected value comes from a "
                               "definition (a spec, a reference, the vendor file), never from this code's own "
                               "output; or `false_positive: <why the mutant changes nothing a caller sees>`.",
                               UNTESTED))
    return out


def unseen_note(rows: _t.Sequence[dict]) -> str | None:
    """The changed result lines no mutant was tried on, so a clean section isn't read as "all caught"."""
    unseen = [r for r in rows if "results" not in r and r.get("note") != "no mutant on a changed line"]
    partly = [r for r in rows if r.get("partial")]
    if not unseen and not partly:
        return None
    return " ".join(x for x in (
        "_Not mutated: " + ", ".join(f"`{r['file']}` ({r.get('note') or 'not looked at'})" for r in unseen) + "._"
        if unseen else "",
        "_Partly mutated: " + ", ".join(f"`{r['file']}` ({r['partial']})" for r in partly) + "._" if partly else "") if x)


def run_test_value_check(
    diff: str,
    *,
    root: pathlib.Path | None = None,
    on_base: _t.Callable[..., dict] | None = None,
    mutate: _t.Callable[[pathlib.Path], dict] | None = None,
    staged: _t.Callable[[str], str] | None = None,
    result_globs: _t.Sequence[str] = (),
    pr: str | None = None,
    commit: str | None = None,
    branch: str | None = None,
) -> str:
    """Run both checks on the staged diff, emit `test_value_run` + one `test_value_finding` per bullet,
    return the markdown section."""
    from .recital import Finding, _parse_findings, _inject_slugs, is_result_path  # recital imports this module
    start = time.monotonic()
    root = root or repo_root()
    files = parse_diff(diff)
    staged = staged or _staged(root)
    sources = source_changes(files)
    payload: dict = {}
    bullets: list[str] = []
    notes: list[str] = []
    errors: list[str] = []

    added = added_tests(files, staged) if sources else None
    if added:
        try:
            failed = (on_base or default_on_base)(root, added, sources, staged)
            payload["on_base"] = failed
            b = base_bullet(added, failed)
            bullets += [b] if b else []
            notes += unjudged_notes(failed)
        except Exception as e:   # a broken check must not cost the commit its recital
            errors.append(f"on base: {type(e).__name__}: {e}")

    if any(is_result_path(p, result_globs) for p in sources):
        try:
            rows = (mutate or default_mutate)(root).get("files", [])
            payload["mutated"] = [{"file": r["file"], "results": len(r.get("results") or []),
                                   "survived": len(r.get("survivor_detail") or []), "note": r.get("note"),
                                   **({"cached": True} if r.get("cached") else {})}
                                  for r in rows]
            bullets += mutation_bullets(rows)
            note = unseen_note(rows)
            notes += [note] if note else []
        except Exception as e:
            errors.append(f"mutation: {type(e).__name__}: {e}")

    text = "\n".join(bullets)
    findings: list[Finding] = _parse_findings(text, MECHANISM)
    for f in findings:
        append_event("test_value_finding",
                     {"file": f.file, "line": f.line, "desc": f.desc, "slug": f.slug, "marker": f.marker},
                     pr=pr, commit=commit, branch=branch)
    payload.update(duration_s=round(time.monotonic() - start, 2), findings=len(findings),
                   **({"errors": errors} if errors else {}))
    append_event("test_value_run", payload, pr=pr, commit=commit, branch=branch)

    if not added and "mutated" not in payload and not errors:
        return f"_{TITLE}: skipped — no added tests with a source change, no result path_"
    body = "\n\n".join(x for x in (_inject_slugs(text, MECHANISM) if text else "", *notes,
                                   *(f"> **test value check error** — {e}" for e in errors)) if x)
    tail = "run" + ("" if bullets else " — nothing flagged")
    return (f"_{TITLE} (evidence):_\n\n{body}\n\n_{TITLE}: {tail}_" if body else f"_{TITLE}: {tail}_")
