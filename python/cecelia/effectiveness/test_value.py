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

from .git_context import FileDiff, git_output, parse_diff, repo_root
from .log import append_event

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


def julia_on_base(root: pathlib.Path, sources: _t.Sequence[str], suites: _t.Sequence[str]) -> bool:
    """True when the staged `suites` fail anywhere with the changed `app/src` files at their HEAD text."""
    with tempfile.TemporaryDirectory(prefix="test_value_base_") as tmp:
        pairs = []
        for i, path in enumerate(p for p in sources if p.startswith("app/src/") and p.endswith(".jl")):
            base = pathlib.Path(tmp) / f"{i}.jl"
            base.write_text(_head_text(root, path), encoding="utf-8")
            pairs.append(f"{root / path}={base}")
        out = pathlib.Path(tmp) / "out.txt"
        subprocess.run(["julia", f"--project={root / 'app'}", str(root / "scripts/julia_on_base.jl"), str(out),
                        *pairs, "--", *(str(root / s) for s in suites)],
                       cwd=root, capture_output=True, text=True, encoding="utf-8", errors="replace")
        if not out.is_file():
            raise RuntimeError("julia_on_base.jl wrote nothing")
        return bool(out.read_text(encoding="utf-8").strip())


def default_on_base(root: pathlib.Path, added: Added, sources: _t.Sequence[str],
                    staged: _t.Callable[[str], str]) -> dict:
    """Which languages' added tests fail on base: `{"py": bool, "jl": bool}` for the ones run."""
    out = {}
    if added.python and any(s.endswith(".py") for s in sources):
        out["py"] = python_on_base(root, added.python, staged)
    if added.julia and any(s.startswith("app/src/") and s.endswith(".jl") for s in sources):
        out["jl"] = julia_on_base(root, sources, added.julia)
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
    """`mutate_changed.py --staged`'s rows."""
    return run_mutate_changed(root, ["--staged"])


def quote_survivors(svs: _t.Sequence[dict], n: int = QUOTED) -> str:
    """The first `n` survivors as ``line N `orig` → `new` `` (`survivor_detail` rows), `(+k more)` after."""
    quoted = "; ".join(f"line {d['line']} `{d['orig']}` → `{d['new']}`" for d in svs[:n])
    return quoted + (f" (+{len(svs) - n} more)" if len(svs) > n else "")


# ---------------------------------------------------------------- the section

def _bullet(file: str, line: int, text: str, marker: str) -> str:
    return f"- **{file}:{line}** — {text} [**{marker}**]"


def base_bullet(added: Added, failed: dict) -> str | None:
    """The `passes on base` bullet when no language's added tests failed on base."""
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
    if not unseen:
        return None
    return ("_Not mutated: " + ", ".join(f"`{r['file']}` ({r.get('note') or 'not looked at'})" for r in unseen) + "._")


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
        except Exception as e:   # a broken check must not cost the commit its recital
            errors.append(f"on base: {type(e).__name__}: {e}")

    if any(is_result_path(p, result_globs) for p in sources):
        try:
            rows = (mutate or default_mutate)(root).get("files", [])
            payload["mutated"] = [{"file": r["file"], "results": len(r.get("results") or []),
                                   "survived": len(r.get("survivor_detail") or []), "note": r.get("note")}
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
