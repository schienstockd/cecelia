#!/usr/bin/env python3
"""Mutation-test only what a change touched: the changed lines of its Julia and Python result files,
against only the tests that name the changed functions. TEST_VALUE_PLAN.md → P4(b).

    python scripts/mutate_changed.py BASE [--out DIR] [--timeout 120] [--sample N] [--dry-run] [--keep]
    python scripts/mutate_changed.py --staged [--sample N] [--budget S] [--json FILE]   # recital's call
    python scripts/mutate_changed.py --at FILE:LO-HI [...]   # these lines as they are now (the weekly judge)

Run in a checkout of the change itself (its HEAD is what gets mutated); BASE is the commit it is
compared to (for a merged PR, `<merge>^1`). `--staged` takes the staged diff instead and mutates the
files on disk; a file with unstaged edits on top is skipped (what is on disk is not what is committed).
`--at` mutates the given lines of HEAD's files, no diff: the weekly judge's re-check of a function an
`untested` finding named (`scripts/judge/bugs.py`). `--budget` starts no Julia mutant and no file after that many seconds; `--json` writes the rows. Result files are the `result-paths` globs of
`docs/ai-assist/RESULT_REVIEW.md`, as recital reads them (`is_result_path`). Per file:

- Julia (`app/src/**.jl`, included into module Cecelia): `scripts/julia_mutate.jl --lines <changed>
  --known-failures` against the `app/test/suite/*.jl` files that name a changed function (or that the
  change itself touched). Every mutant on a changed line runs.
- Python (`python/cecelia/**.py`): `mutmut` on the mutants that edit a changed line, against the `python/cecelia/tests/test_*.py` files that import the module and name a
  changed function. Needs mutmut in the pixi env (TEST_VALUE_PLAN.md → Tooling); runs in a copy.

Prints one row per file (mutants, killed, survived, score, seconds) and the survivors. A survivor is
"not killed by the mapped tests", not yet a gap.
"""
from __future__ import annotations

import argparse
import ast
import contextlib
import difflib
import json
import os
import random
import re
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path

from cecelia.effectiveness.git_context import parse_diff, repo_root
from cecelia.effectiveness.recital import is_result_path, repo_result_paths

# the checkout being mutated is the one this runs in (a copy of the script works on any checkout)
ROOT = repo_root().resolve()
HERE = Path(__file__).resolve().parent


def changed_lines(base: str | None) -> dict[str, set[int]]:
    """Each changed file → the line numbers its diff adds or rewrites (deleted files dropped): BASE
    against HEAD, or (`base=None`) the staged diff."""
    rev = ["--staged"] if base is None else [base, "HEAD"]
    out = subprocess.check_output(["git", "diff", "-U0", "--no-color", "--diff-filter=AM", *rev],
                                  cwd=ROOT, text=True, encoding="utf-8", errors="replace")
    return {f.path: {ln.lineno for ln in f.added} for f in parse_diff(out)}


def at_lines(specs: list[str]) -> dict[str, set[int]]:
    """`--at FILE:LO-HI[,N…]` → file → those line numbers."""
    out: dict[str, set[int]] = {}
    for spec in specs:
        rel, _, rng = spec.rpartition(":")
        if not rel or not re.fullmatch(r"\d+(-\d+)?(,\d+(-\d+)?)*", rng):
            raise ValueError(f"--at wants FILE:LO-HI, got {spec!r}")
        for part in rng.split(","):
            lo, _, hi = part.partition("-")
            out.setdefault(rel, set()).update(range(int(lo), int(hi or lo) + 1))
    return out


def unstaged_files() -> set[str]:
    """Files whose working copy differs from the index."""
    out = subprocess.run(["git", "diff", "--name-only"], cwd=ROOT, capture_output=True, text=True,
                         encoding="utf-8", errors="replace").stdout
    return {ln.strip() for ln in out.splitlines() if ln.strip()}


def ranges(lines: set[int]) -> str:
    out, s = [], sorted(lines)
    i = 0
    while i < len(s):
        j = i
        while j + 1 < len(s) and s[j + 1] == s[j] + 1:
            j += 1
        out.append(f"{s[i]}-{s[j]}" if j > i else str(s[i]))
        i = j + 1
    return ",".join(out)


# ---------------------------------------------------------------- Julia

_JL_DEF = re.compile(r"^(?:function\s+|macro\s+)?(?:[A-Z]\w*\.)?([A-Za-z_]\w*!?)\s*(?:\{[^}]*\})?\s*\(|"
                     r"^(?:const|struct|mutable struct|abstract type)\s+([A-Za-z_]\w*)")


def jl_owners(path: Path) -> tuple[dict[int, str], dict[str, int]]:
    """Each line → the top-level definition (column 0) it sits in, and each definition → its first line."""
    owner, first = {}, {}
    cur = None
    for i, ln in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        m = _JL_DEF.match(ln)
        if m:
            cur = m.group(1) or m.group(2)
            first.setdefault(cur, i)
        owner[i] = cur
    return owner, first


def jl_names(path: Path, lines: set[int]) -> set[str]:
    """The top-level definitions (column 0) enclosing the changed lines."""
    owner, _ = jl_owners(path)
    return {owner[i] for i in lines if owner.get(i)}


def with_callers(names: set[str], refs: dict[str, set[str]]) -> set[str]:
    """`names` plus every definition in the same file that calls one of them, repeated until nothing
    is added: a private helper is tested through its public caller (`_band` → `parse_settings`)."""
    out, grew = set(names), True
    while grew:
        more = {d for d, used in refs.items() if d not in out and used & out}
        out |= more
        grew = bool(more)
    return out


def jl_refs(path: Path) -> dict[str, set[str]]:
    """Each top-level definition → the identifiers its lines use."""
    owner, _ = jl_owners(path)
    refs: dict[str, set[str]] = {}
    for i, ln in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        if owner.get(i):
            refs.setdefault(owner[i], set()).update(re.findall(r"[A-Za-z_]\w*!?", ln.split("#", 1)[0]))
    return refs


def jl_tests(names: set[str], touched: set[str]) -> tuple[list[Path], list[str]]:
    """The suite files that name a changed definition or that the change touched, and the changed
    definitions no suite file names (a test may still reach them through a caller: not looked at)."""
    suite = ROOT / "app/test/suite"
    hits, named = [], set()
    for f in sorted(suite.glob("*.jl")):
        rel = f.relative_to(ROOT).as_posix()   # git paths use / on every OS
        txt = f.read_text(encoding="utf-8", errors="replace")
        here = {n for n in names if re.search(rf"(?<![\w!]){re.escape(n)}(?![\w!])", txt)}
        named |= here
        if rel in touched or here:
            hits.append(f)
    return hits, sorted(names - named)


def run_julia(rel: str, lines: set[int], touched: set[str], outdir: Path, timeout: int, sample: int,
              dry: bool = False, deadline: float | None = None) -> dict:
    src = ROOT / rel
    names = jl_names(src, lines)
    tests, unnamed = jl_tests(with_callers(names, jl_refs(src)), touched)
    unnamed = sorted(set(unnamed) & names)
    row = {"file": rel, "lang": "jl", "lines": len(lines), "names": sorted(names), "unnamed": unnamed,
           "tests": [t.name for t in tests]}
    if not tests:
        return row | {"note": "no test names a changed function"}
    if dry:
        return row
    listed = subprocess.run(["julia", f"--project={ROOT / 'app'}", str(HERE / "julia_mutate.jl"), str(src), "-", "--list"],
                            cwd=ROOT, capture_output=True, text=True).stdout.splitlines()
    if not any(int(ln.split("\t")[1]) in lines for ln in listed if ln.count("\t") >= 4):
        return row | {"note": "no mutant on a changed line", "results": []}
    out = outdir / (rel.replace("/", "__") + ".tsv")
    out.unlink(missing_ok=True)
    cmd = ["julia", f"--project={ROOT / 'app'}", str(HERE / "julia_mutate.jl"), str(src), str(out),
           *map(str, tests), "--lines", ranges(lines), "--known-failures", "--timeout", str(timeout)]
    if sample:
        cmd += ["--sample", str(sample)]
    if deadline is not None:
        cmd += ["--deadline", str(deadline)]
    t0 = time.time()
    p = subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True)
    row["secs"] = round(time.time() - t0)
    known = re.findall(r"fails unmutated, ignored: (\S+)", p.stderr)
    row["known"] = [k.replace(str(ROOT / "app/test/suite") + os.sep, "") for k in known]
    if not out.is_file():
        return row | {"note": "harness failed: " + p.stderr.strip()[-400:]}
    rows = [ln.split("\t") for ln in out.read_text(encoding="utf-8").splitlines()]
    base = [r for r in rows if r[0] == "0"]
    if not base or base[0][5] != "survived":
        return row | {"note": f"unmutated run {base[0][5] if base else 'missing'}: " + p.stderr.strip()[-400:]}
    muts = [r for r in rows if r[0] != "0"]
    row["results"] = [r[5] for r in muts]
    row["survivors"] = [f"{r[1]}: {r[2]}  {r[4]}" for r in muts if r[5] == "survived"]
    owner, first = jl_owners(src)
    row["survivor_detail"] = [{"fn": owner.get(int(r[1])) or "?", "def": first.get(owner.get(int(r[1])), int(r[1])),
                               "line": int(r[1]), "orig": r[3], "new": r[4]} for r in muts if r[5] == "survived"]
    return row


# ---------------------------------------------------------------- Python

def py_funcs(path: Path, lines: set[int]) -> dict[str, int]:
    """mutmut keys (`x_f`, `xǁClsǁm`) of the functions and methods holding a changed line → the
    line of their `def`."""
    tree = ast.parse(path.read_text(encoding="utf-8"))
    keys = {}
    for node in tree.body:
        fns = [(node, None)] if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)) else []
        if isinstance(node, ast.ClassDef):
            fns = [(n, node.name) for n in node.body if isinstance(n, (ast.FunctionDef, ast.AsyncFunctionDef))]
        for fn, cls in fns:
            start = min([fn.lineno] + [d.lineno for d in fn.decorator_list])
            if any(start <= i <= fn.end_lineno for i in lines):
                keys[f"xǁ{cls}ǁ{fn.name}" if cls else f"x_{fn.name}"] = fn.lineno
    return keys


def py_changed_mutants(mutfile: Path, defline: dict[str, int], lines: set[int]) -> dict[str, dict]:
    """The mutants (`<key>__mutmut_<n>`) whose edit is on a changed line → `{line, orig, new}` (the
    first edited line). mutmut writes each mutant as a whole copy of its function and records where
    each copy sits (`<file>.spans`); the line that differs from the `__mutmut_orig` copy, counted from
    the `def`, is the source line it mutates."""
    spans = json.loads(Path(str(mutfile) + ".spans").read_text(encoding="utf-8"))["spans"]
    text = mutfile.read_text(encoding="utf-8").splitlines()
    body = lambda a, b: text[a - 1:b]
    # each copy from the line after its own `def` (a copy can start with blank lines the other lacks)
    after_def = lambda ls: ls[next(i for i, ln in enumerate(ls) if re.match(r"\s*(async\s+)?def ", ln)) + 1:]
    out = {}
    for key, def_ln in defline.items():
        orig = after_def(body(*spans[f"{key}__mutmut_orig"]))
        for name, (a, b) in spans.items():
            if not re.fullmatch(rf"{re.escape(key)}__mutmut_\d+", name):
                continue
            mut = after_def(body(a, b))
            ops = [o for o in difflib.SequenceMatcher(None, orig, mut, autojunk=False).get_opcodes() if o[0] != "equal"]
            hit = {def_ln + 1 + i for _, i1, i2, _, _ in ops for i in range(i1, max(i2, i1 + 1))}
            if hit & lines:
                _, i1, i2, j1, j2 = ops[0]
                out[name] = {"line": min(hit & lines), "orig": " ".join(x.strip() for x in orig[i1:i2]),
                             "new": " ".join(x.strip() for x in mut[j1:j2])}
    return out


def py_refs(path: Path) -> dict[str, set[str]]:
    """Each top-level function / method, by mutmut key, → the names its body uses."""
    tree = ast.parse(path.read_text(encoding="utf-8"))
    out = {}
    for node in tree.body:
        fns = [(node, None)] if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)) else []
        if isinstance(node, ast.ClassDef):
            fns = [(n, node.name) for n in node.body if isinstance(n, (ast.FunctionDef, ast.AsyncFunctionDef))]
        for fn, cls in fns:
            used = {n.id for n in ast.walk(fn) if isinstance(n, ast.Name)}
            used |= {n.attr for n in ast.walk(fn) if isinstance(n, ast.Attribute)}
            out[f"xǁ{cls}ǁ{fn.name}" if cls else f"x_{fn.name}"] = used
    return out


def _py_name(key: str) -> str:
    return key.split("ǁ")[2] if "ǁ" in key else key[2:]


def py_with_callers(path: Path, keys) -> set[str]:
    """The keys plus their same-file callers, transitively (`with_callers` over mutmut keys)."""
    refs = py_refs(path)
    out, grew = set(keys), True
    while grew:
        more = {c for c, used in refs.items() if c not in out and any(_py_name(k) in used for k in out)}
        out |= more
        grew = bool(more)
    return out


def py_tests(module: str, keys, touched: set[str]) -> tuple[list[Path], list[str]]:
    """The test files that import the module and name a changed function (or its class), or that
    the change touched; and the changed functions no test file names."""
    short = module.rsplit(".", 1)[-1]
    words = {k: {k.split("ǁ")[2], k.split("ǁ")[1]} if "ǁ" in k else {k[2:]} for k in keys}
    hits, named = [], set()
    for f in sorted((ROOT / "python/cecelia/tests").glob("test_*.py")):
        rel = f.relative_to(ROOT).as_posix()   # git paths use / on every OS
        txt = f.read_text(encoding="utf-8", errors="replace")
        here = {k for k, ws in words.items() if any(re.search(rf"\b{w}\b", txt) for w in ws)} \
            if re.search(rf"\b{short}\b", txt) else set()
        named |= here
        if rel in touched or here:
            hits.append(f)
    return hits, sorted(set(words) - named)


def run_python(rel: str, lines: set[int], touched: set[str], outdir: Path, dry: bool = False,
               keep: bool = False, sample: int = 0) -> dict:
    module = rel[len("python/"):-3].replace("/", ".")
    keys = py_funcs(ROOT / rel, lines)
    row = {"file": rel, "lang": "py", "lines": len(lines), "names": sorted(keys)}
    if not keys:
        return row | {"note": "no changed function (module-level lines only)"}
    tests, unnamed = py_tests(module, py_with_callers(ROOT / rel, keys), touched)
    row["unnamed"] = sorted(set(unnamed) & set(keys))
    row["tests"] = [t.name for t in tests]
    if not tests:
        return row | {"note": "no test names a changed function"}
    if dry:
        return row
    t0 = time.time()
    with (contextlib.nullcontext(tempfile.mkdtemp(prefix="mutate_changed_", dir=outdir)) if keep else
          tempfile.TemporaryDirectory(prefix="mutate_changed_", dir=outdir)) as tmp:
        copy = Path(tmp)
        if keep:
            row["copy"] = str(copy / "python")   # `mutmut show <name>` there
        shutil.copytree(ROOT / "python", copy / "python",
                        ignore=shutil.ignore_patterns("__pycache__", "mutants", "*.egg-info", ".pytest_cache"))
        # tests find the repo three levels above their file: the copy's root, and inside mutmut's
        # `mutants/` that is `python/`, so the rest of the checkout is linked into both
        for d in ROOT.iterdir():
            if d.name not in ("python", ".git", ".pixi"):
                (copy / d.name).symlink_to(d)
                (copy / "python" / d.name).symlink_to(d)
        env = os.environ | {"PYTHONPATH": str(copy / "python/mutants")}
        py = sys.executable   # the env mutmut is installed in
        row["known"] = []   # tests that fail with nothing mutated, set aside one by one
        row["phases"] = []
        while True:
            t1 = time.time()
            (copy / "python/setup.cfg").write_text(
                "[mutmut]\nsource_paths = " + rel[len("python/"):] + "\nalso_copy =\n    cecelia\n"
                # a mutant that hangs costs this × the test time (mutmut's default 15 made two hung
                # dim_utils mutants most of a 9-minute run); 3 still clears a slow first import
                "timeout_multiplier = 3\n"
                "pytest_add_cli_args_test_selection =\n" + "".join(f"    cecelia/tests/{t.name}\n" for t in tests)
                + ("pytest_add_cli_args =\n" + "".join(f"    --deselect\n    {k}\n" for k in row["known"])
                   if row["known"] else ""), encoding="utf-8")
            shutil.rmtree(copy / "python/mutants", ignore_errors=True)
            (copy / "python/mutants").mkdir()
            # first pass runs no mutant: it writes them and maps which tests reach which function
            p = subprocess.run([str(py), "-m", "mutmut", "run", f"{module}.__none__"], cwd=copy / "python",
                               env=env, capture_output=True, text=True)
            # a test failing with nothing mutated stops mutmut (it runs with -x): deselect it and go
            # again, as julia_mutate's --known-failures sets such a test aside
            bad = sorted(set(re.findall(r"^(?:FAILED|ERROR) (cecelia/tests/\S+)", p.stdout, re.M)) - set(row["known"]))
            row["phases"].append(("baseline", round(time.time() - t1)))
            if "failed to collect stats" not in p.stdout + p.stderr or not bad or len(row["known"]) >= 20:
                break
            row["known"] += bad
        mutfile = copy / "python/mutants" / rel[len("python/"):]
        picked = py_changed_mutants(mutfile, keys, lines) if mutfile.is_file() else {}
        if sample and len(picked) > sample:   # a fixed draw, so a rerun picks the same ones
            picked = {n: picked[n] for n in sorted(random.Random(1).sample(sorted(picked), sample))}
        pats = [f"{module}.{n}" for n in picked]
        if pats:
            t1 = time.time()
            p = subprocess.run([str(py), "-m", "mutmut", "run", *pats], cwd=copy / "python", env=env,
                               capture_output=True, text=True)
            row["phases"].append(("mutants", round(time.time() - t1)))
        r = subprocess.run([str(py), "-m", "mutmut", "results", "--all", "true"], cwd=copy / "python", env=env,
                           capture_output=True, text=True)
    row["secs"] = round(time.time() - t0)
    res = []
    for ln in r.stdout.splitlines():
        m = re.match(r"\s*(\S+__mutmut_\d+): (.+)$", ln)
        if m and m.group(1) in pats:
            res.append((m.group(1), m.group(2).strip()))
    if not res:
        why = "no mutant on a changed line" if mutfile.is_file() else "mutmut wrote no mutants"
        return row | {"note": why + ": " + (p.stdout + p.stderr).strip()[-600:]}
    row["results"] = [s for _, s in res]
    if not any(s in ("killed", "survived") for _, s in res):
        return row | {"note": "no mutant ran: " + (p.stdout + p.stderr).strip()[-600:]}
    row["survivors"] = [n.split(".")[-1] for n, s in res if s == "survived"]
    row["survivor_detail"] = []
    for n, s in res:
        if s == "survived":
            short = n.split(".")[-1]
            key = short.rsplit("__mutmut_", 1)[0]
            row["survivor_detail"].append({"fn": key.split("ǁ")[-1] if "ǁ" in key else key[2:], "def": keys[key],
                                           **picked[short]})
    return row


# ---------------------------------------------------------------- main

def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("base", nargs="?", help="the commit the change is compared to (or --staged)")
    ap.add_argument("--staged", action="store_true", help="mutate the staged change (recital)")
    ap.add_argument("--at", action="append", default=[], metavar="FILE:LO-HI",
                    help="mutate these lines of HEAD's file, no diff (repeatable)")
    ap.add_argument("--budget", type=float, default=None, help="seconds: no file or Julia mutant starts after")
    ap.add_argument("--json", default=None, help="write the rows here")
    ap.add_argument("--out", default=None, help="where the per-file TSVs go (default: a temp dir)")
    ap.add_argument("--timeout", type=int, default=120)
    ap.add_argument("--sample", type=int, default=0, help="at most N mutants per file (a fixed draw)")
    ap.add_argument("--keep", action="store_true", help="Python: keep the mutmut copy (path in the report)")
    ap.add_argument("--dry-run", action="store_true", help="map the changed functions to tests; mutate nothing")
    a = ap.parse_args()
    if sum((a.base is not None, a.staged, bool(a.at))) != 1:
        ap.error("give one of BASE, --staged or --at")
    if "PIXI_PROJECT_ROOT" not in os.environ:
        # tests that start Python (run_py, the task runners) fail outside the env, and a test that
        # fails unmutated is set aside: the mutants it would have killed then read as survivors
        sys.exit("mutate_changed: run it inside the pixi env (`pixi run python scripts/mutate_changed.py …`;"
                 " for another checkout, `pixi run --manifest-path <this>/pixi.toml -- bash -c 'cd <it> && …'`)")
    outdir = Path(a.out or tempfile.mkdtemp(prefix="mutate_changed_")).resolve()
    outdir.mkdir(parents=True, exist_ok=True)
    globs = repo_result_paths(ROOT)
    try:
        changed = at_lines(a.at) if a.at else changed_lines(None if a.staged else a.base)
    except ValueError as e:
        ap.error(str(e))
    touched = set() if a.at else set(changed)   # `--at` is no change: no test file "touched" it
    dirty = unstaged_files() if a.staged else set()
    rows = []
    t0 = time.time()
    deadline = t0 + a.budget if a.budget else None
    for rel, lines in sorted(changed.items()):
        if not lines or not is_result_path(rel, globs):
            continue
        is_jl = rel.startswith("app/src/") and rel.endswith(".jl")
        is_py = rel.startswith("python/cecelia/") and rel.endswith(".py")
        if (is_jl or is_py) and rel in dirty:
            rows.append({"file": rel, "lines": len(lines), "note": "unstaged edits on top of the staged change"})
        elif (is_jl or is_py) and deadline is not None and time.time() > deadline:
            rows.append({"file": rel, "lines": len(lines), "note": "time budget spent before this file"})
        elif is_py and sys.platform == "win32" and not a.dry_run:
            rows.append({"file": rel, "lines": len(lines), "note": "mutmut does not run on Windows"})
        elif is_jl:
            rows.append(run_julia(rel, lines, touched, outdir, a.timeout, a.sample, a.dry_run, deadline))
        elif is_py:
            rows.append(run_python(rel, lines, touched, outdir, a.dry_run, a.keep, a.sample))
        else:
            rows.append({"file": rel, "lines": len(lines), "note": "not mutated (not in module Cecelia or python/cecelia)"})
    total = round(time.time() - t0)
    for r in rows:
        res = r.get("results", [])
        k, s = res.count("killed"), res.count("survived")
        score = f"{100 * k / (k + s):.0f}%" if k + s else "-"
        other = ", ".join(f"{st} {res.count(st)}" for st in sorted(set(res) - {"killed", "survived"}))
        print(f"{r['file']}  mutants {len(res)}  killed {k}  survived {s}  score {score}  {r.get('secs', 0)} s"
              + (f"  ({other})" if other else ""))
        if r.get("names"):
            print(f"  changed: {', '.join(r['names'])}")
        if r.get("unnamed"):
            print(f"  named by no test, nor any caller in its file: {', '.join(r['unnamed'])}")
        if r.get("tests") is not None:
            print(f"  tests: {', '.join(r['tests']) or '-'}")
        if r.get("phases"):
            print("  phases: " + ", ".join(f"{k} {v} s" for k, v in r["phases"]))
        if r.get("known"):
            print(f"  fail unmutated (ignored): {', '.join(r['known'])}")
        if r.get("note"):
            print(f"  note: {r['note']}")
        if r.get("copy"):
            print(f"  kept: {r['copy']}")
        for sv in r.get("survivors", []):
            print(f"  survived  {sv}")
    # changed result lines nothing looked at: a clean report above says nothing about them
    unseen = [r for r in rows if "results" not in r and r.get("note") != "no mutant on a changed line"
              and not (a.dry_run and r.get("tests"))]
    if unseen:
        print(f"NOT LOOKED AT: {sum(r['lines'] for r in unseen)} changed lines in {len(unseen)} result files: "
              + ", ".join(f"{r['file']} ({r['lines']})" for r in unseen))
    names = [n for r in rows for n in r.get("names", [])]
    unnamed = [n for r in rows for n in r.get("unnamed", [])]
    if names:
        print(f"changed functions named by no test: {len(unnamed)} of {len(names)}")
    print(f"total {total} s")
    summary = json.dumps({"total_secs": total, "files": rows}, indent=1)
    (outdir / "summary.json").write_text(summary, encoding="utf-8")
    if a.json:
        Path(a.json).write_text(summary, encoding="utf-8")
    return 0


if __name__ == "__main__":
    sys.exit(main())
