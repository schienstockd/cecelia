#!/usr/bin/env python3
"""Mutation-test only what a change touched: the changed lines of its Julia and Python result files,
against only the tests that name the changed functions. TEST_VALUE_PLAN.md → P4(b).

    python scripts/mutate_changed.py BASE [--out DIR] [--timeout 120] [--sample N] [--dry-run] [--keep]

Run in a checkout of the change itself (its HEAD is what gets mutated); BASE is the commit it is
compared to (for a merged PR, `<merge>^1`). Result files are the `result-paths` globs of
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


def changed_lines(base: str) -> dict[str, set[int]]:
    """Each changed file → the HEAD line numbers its diff adds or rewrites (deleted files dropped)."""
    out = subprocess.check_output(["git", "diff", "-U0", "--no-color", "--diff-filter=AM", base, "HEAD"],
                                  cwd=ROOT, text=True, encoding="utf-8", errors="replace")
    return {f.path: {ln.lineno for ln in f.added} for f in parse_diff(out)}


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


def jl_names(path: Path, lines: set[int]) -> set[str]:
    """The top-level definitions (column 0) enclosing the changed lines."""
    src = path.read_text(encoding="utf-8").splitlines()
    names, owner = set(), {}
    cur = None
    for i, ln in enumerate(src, 1):
        m = _JL_DEF.match(ln)
        if m:
            cur = m.group(1) or m.group(2)
        owner[i] = cur
    for i in lines:
        if owner.get(i):
            names.add(owner[i])
    return names


def jl_tests(names: set[str], touched: set[str]) -> tuple[list[Path], list[str]]:
    """The suite files that name a changed definition or that the change touched, and the changed
    definitions no suite file names (a test may still reach them through a caller: not looked at)."""
    suite = ROOT / "app/test/suite"
    hits, named = [], set()
    for f in sorted(suite.glob("*.jl")):
        rel = str(f.relative_to(ROOT))
        txt = f.read_text(encoding="utf-8", errors="replace")
        here = {n for n in names if re.search(rf"(?<![\w!]){re.escape(n)}(?![\w!])", txt)}
        named |= here
        if rel in touched or here:
            hits.append(f)
    return hits, sorted(names - named)


def run_julia(rel: str, lines: set[int], touched: set[str], outdir: Path, timeout: int, sample: int,
              dry: bool = False) -> dict:
    src = ROOT / rel
    names = jl_names(src, lines)
    tests, unnamed = jl_tests(names, touched)
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


def py_changed_mutants(mutfile: Path, defline: dict[str, int], lines: set[int]) -> list[str]:
    """The mutants (`<key>__mutmut_<n>`) whose edit is on a changed line. mutmut writes each mutant
    as a whole copy of its function and records where each copy sits (`<file>.spans`); the line that
    differs from the `__mutmut_orig` copy, counted from the `def`, is the source line it mutates."""
    spans = json.loads(Path(str(mutfile) + ".spans").read_text(encoding="utf-8"))["spans"]
    text = mutfile.read_text(encoding="utf-8").splitlines()
    body = lambda a, b: text[a - 1:b]
    # each copy from the line after its own `def` (a copy can start with blank lines the other lacks)
    after_def = lambda ls: ls[next(i for i, ln in enumerate(ls) if re.match(r"\s*(async\s+)?def ", ln)) + 1:]
    out = []
    for key, def_ln in defline.items():
        orig = after_def(body(*spans[f"{key}__mutmut_orig"]))
        for name, (a, b) in spans.items():
            if not re.fullmatch(rf"{re.escape(key)}__mutmut_\d+", name):
                continue
            sm = difflib.SequenceMatcher(None, orig, after_def(body(a, b)), autojunk=False)
            hit = {def_ln + 1 + i for tag, i1, i2, _, _ in sm.get_opcodes() if tag != "equal"
                   for i in range(i1, max(i2, i1 + 1))}
            if hit & lines:
                out.append(name)
    return out


def py_tests(module: str, keys, touched: set[str]) -> tuple[list[Path], list[str]]:
    """The test files that import the module and name a changed function (or its class), or that
    the change touched; and the changed functions no test file names."""
    short = module.rsplit(".", 1)[-1]
    words = {k: {k.split("ǁ")[2], k.split("ǁ")[1]} if "ǁ" in k else {k[2:]} for k in keys}
    hits, named = [], set()
    for f in sorted((ROOT / "python/cecelia/tests").glob("test_*.py")):
        rel = str(f.relative_to(ROOT))
        txt = f.read_text(encoding="utf-8", errors="replace")
        here = {k for k, ws in words.items() if any(re.search(rf"\b{w}\b", txt) for w in ws)} \
            if re.search(rf"\b{short}\b", txt) else set()
        named |= here
        if rel in touched or here:
            hits.append(f)
    return hits, sorted(set(words) - named)


def run_python(rel: str, lines: set[int], touched: set[str], outdir: Path, dry: bool = False,
               keep: bool = False) -> dict:
    module = rel[len("python/"):-3].replace("/", ".")
    keys = py_funcs(ROOT / rel, lines)
    row = {"file": rel, "lang": "py", "lines": len(lines), "names": sorted(keys)}
    if not keys:
        return row | {"note": "no changed function (module-level lines only)"}
    tests, row["unnamed"] = py_tests(module, keys, touched)
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
        pats = [f"{module}.{n}" for n in py_changed_mutants(mutfile, keys, lines)] if mutfile.is_file() else []
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
    return row


# ---------------------------------------------------------------- main

def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("base")
    ap.add_argument("--out", default=None, help="where the per-file TSVs go (default: a temp dir)")
    ap.add_argument("--timeout", type=int, default=120)
    ap.add_argument("--sample", type=int, default=0, help="Julia: at most N mutants per file")
    ap.add_argument("--keep", action="store_true", help="Python: keep the mutmut copy (path in the report)")
    ap.add_argument("--dry-run", action="store_true", help="map the changed functions to tests; mutate nothing")
    a = ap.parse_args()
    if "PIXI_PROJECT_ROOT" not in os.environ:
        # tests that start Python (run_py, the task runners) fail outside the env, and a test that
        # fails unmutated is set aside: the mutants it would have killed then read as survivors
        sys.exit("mutate_changed: run it inside the pixi env (`pixi run python scripts/mutate_changed.py …`;"
                 " for another checkout, `pixi run --manifest-path <this>/pixi.toml -- bash -c 'cd <it> && …'`)")
    outdir = Path(a.out or tempfile.mkdtemp(prefix="mutate_changed_")).resolve()
    outdir.mkdir(parents=True, exist_ok=True)
    globs = repo_result_paths(ROOT)
    changed = changed_lines(a.base)
    touched = set(changed)
    rows = []
    t0 = time.time()
    for rel, lines in sorted(changed.items()):
        if not lines or not is_result_path(rel, globs):
            continue
        if rel.startswith("app/src/") and rel.endswith(".jl"):
            rows.append(run_julia(rel, lines, touched, outdir, a.timeout, a.sample, a.dry_run))
        elif rel.startswith("python/cecelia/") and rel.endswith(".py"):
            rows.append(run_python(rel, lines, touched, outdir, a.dry_run, a.keep))
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
            print(f"  named by no test: {', '.join(r['unnamed'])}")
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
    (outdir / "summary.json").write_text(json.dumps({"total_secs": total, "files": rows}, indent=1), encoding="utf-8")
    return 0


if __name__ == "__main__":
    sys.exit(main())
