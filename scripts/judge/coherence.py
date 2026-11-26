#!/usr/bin/env python3
"""Weekly judge — coherence counts: whole-repo numbers that drift one PR at a time.

Recital reviews a diff, and a ratchet bans one shape. Neither sees the repo drift as a whole: copied
blocks pile up, an import cycle closes across three files, a route loses its last caller, an inventory
row outlives the code it names. This step counts each with a deterministic tool, so the record shows
the trend. Design and baseline: `docs/todo/CONSISTENCY_COHERENCE_AUDIT.md`.

| Check | Counts | How |
|---|---|---|
| `clones` | copied blocks of 70+ tokens in `app/src`, `api/src`, `python/cecelia`, `frontend/src` (tests out) | `jscpd` |
| `cycles` | frontend import cycles, type-only and `import()` edges skipped | `madge` |
| `py_dead` | Python names never read (80 %+ confidence; unused imports out — `try: import x` is a probe) | `vulture` |
| `routes_uncalled` | Julia `/api` routes no frontend, MCP or Python string names | scan |
| `inventory_stale` | identifiers `docs/inventory/*.md` cites that no source file contains | scan |

**Counts, never a queue.** Nothing here adds an owner item. A count that rose on each of the last two
passes is listed as *rising*: a shape is spreading, and the fix is a ratchet test that bans it (the
image-path ratchet in `app/test/suite/ratchets.jl` is the worked example). A tool that is missing or
fails leaves its count `None` with the reason, and the rest still count.

Usage:
    python scripts/judge/coherence.py [ROOT]   # print the counts for a checkout (default: this one)
"""

from __future__ import annotations

import json
import pathlib
import re
import shutil
import subprocess
import sys
import tempfile
import typing as _t

_REPO = pathlib.Path(__file__).resolve().parents[2]

#: Pinned so a tool release can't move the counts on its own.
JSCPD = "jscpd@4.0.5"
MADGE = "madge@8.0.0"
VULTURE = "vulture==2.14"

CHECKS = ("clones", "cycles", "py_dead", "routes_uncalled", "inventory_stale")
CLONE_DIRS = ("app/src", "api/src", "python/cecelia", "frontend/src")
#: Names inventory rows cite that belong to a dependency, not to us.
THIRD_PARTY = frozenset({"createLogger", "body_path"})
TIMEOUT_S = 600

Run = _t.Callable[..., subprocess.CompletedProcess]


def _tool(cmd: list[str], *, cwd: pathlib.Path, run: Run) -> subprocess.CompletedProcess:
    exe = shutil.which(cmd[0])
    if not exe:
        raise RuntimeError(f"`{cmd[0]}` not on PATH")
    return run([exe, *cmd[1:]], cwd=str(cwd), capture_output=True, text=True, timeout=TIMEOUT_S)


def count_clones(root: pathlib.Path, run: Run = subprocess.run) -> int:
    with tempfile.TemporaryDirectory() as out:
        _tool(["npx", "-y", JSCPD, *CLONE_DIRS, "--min-tokens", "70",
               "--format", "julia,python,typescript,javascript,markup",
               "--ignore", "**/node_modules/**,**/__pycache__/**,**/*.test.ts,**/tests/**,**/test/**",
               "--reporters", "json", "--output", out, "--silent"], cwd=root, run=run)
        report = pathlib.Path(out) / "jscpd-report.json"
        if not report.is_file():
            raise RuntimeError("jscpd wrote no report")
        return int(json.loads(report.read_text(encoding="utf-8"))["statistics"]["total"]["clones"])


def count_cycles(root: pathlib.Path, run: Run = subprocess.run) -> int:
    skip = {"skipTypeImports": True, "skipAsyncImports": True}
    rc = {"fileExtensions": ["ts", "vue"], "tsConfig": str(root / "frontend" / "tsconfig.app.json"),
          "detectiveOptions": {k: skip for k in ("ts", "tsx", "es6", "vue")}}
    # madge reads `.madgerc` from its cwd; a scratch cwd keeps the checkout clean
    with tempfile.TemporaryDirectory() as cwd:
        (pathlib.Path(cwd) / ".madgerc").write_text(json.dumps(rc), encoding="utf-8")
        p = _tool(["npx", "-y", MADGE, "--circular", "--json", str(root / "frontend" / "src")],
                  cwd=pathlib.Path(cwd), run=run)
    try:
        return len(json.loads(p.stdout))
    except json.JSONDecodeError as e:
        raise RuntimeError(f"madge printed no JSON (exit {p.returncode})") from e


def count_py_dead(root: pathlib.Path, run: Run = subprocess.run) -> int:
    p = _tool(["uvx", VULTURE, "python/cecelia", "--min-confidence", "80", "--exclude", "*/tests/*"],
              cwd=root, run=run)
    if p.returncode not in (0, 3):   # 3 = found dead code
        raise RuntimeError(f"vulture exit {p.returncode}: {p.stderr.strip()[:200]}")
    return sum(1 for line in p.stdout.splitlines() if ": unused " in line and ": unused import " not in line)


def _source_files(root: pathlib.Path, dirs: _t.Iterable[str], suffixes: _t.Container[str],
                  tests: bool = False) -> _t.Iterator[pathlib.Path]:
    for d in dirs:
        for p in (root / d).rglob("*"):
            if not p.is_file() or p.suffix not in suffixes:
                continue
            parts = set(p.relative_to(root).parts)
            if parts & {"node_modules", "__pycache__", ".pixi"}:
                continue
            if not tests and (parts & {"test", "tests"} or p.name.endswith((".test.ts", ".spec.ts"))):
                continue
            yield p


_JL_ROUTE = re.compile(r'"(/api/[^"$]*)"')
# a caller names a route as a literal, or after a template base: `${apiBase}/api/x`, f"{base}/api/x"
_CALLER = re.compile(r"""(?:^|[`'"}])(/api/[^`'"$?{\s]*)""")


# Where route callers are looked for. Shared with scripts/architecture/boundaries_extract.py, which
# lists the same callers per route, so its uncalled routes and this count are one number.
CALLER_DIRS = ["frontend/src", "mcp", "python/cecelia", "app/src", "preview", "scripts"]
CALLER_SUFFIXES = {".ts", ".vue", ".py", ".mjs"}


def strip_comments(text: str, suffix: str) -> str:
    """Drop comments and docstrings, so a route named in prose ("Called by /api/x") is not a caller."""
    if suffix == ".py":
        text = re.sub(r'(?s)("""|\'\'\')(.*?)\1', "", text)
        return re.sub(r"(?m)#.*$", "", text)
    if suffix == ".jl":
        text = re.sub(r'(?s)""".*?"""', "", text)
        return re.sub(r"(?m)^\s*#.*$", "", text)
    text = re.sub(r"(?s)/\*.*?\*/", "", text)
    return re.sub(r"(?m)^\s*//.*$", "", text)


def route_callers(root: pathlib.Path) -> dict[str, set[pathlib.Path]]:
    """Every `/api/...` literal outside tests and comments → the files that name it."""
    out: dict[str, set[pathlib.Path]] = {}
    for p in _source_files(root, CALLER_DIRS, CALLER_SUFFIXES):
        for m in _CALLER.findall(strip_comments(p.read_text(encoding="utf-8", errors="ignore"), p.suffix)):
            c = m.rstrip("/")
            if c != "/api":
                out.setdefault(c, set()).add(p)
    return out


def route_called_by(route: str, callers: dict[str, set[pathlib.Path]]) -> set[pathlib.Path]:
    """Files calling `route`: as itself, as `/api/x/<id>`, or through a prefix that builds `/api/x/<op>`."""
    out: set[pathlib.Path] = set()
    for c, fs in callers.items():
        if c == route or c.startswith(route + "/") or (route.startswith(c + "/") and c.count("/") > 2):
            out |= fs
    return out


def count_routes_uncalled(root: pathlib.Path) -> int:
    routes = {m.rstrip("/") for p in _source_files(root, ["api/src"], {".jl"})
              for m in _JL_ROUTE.findall(p.read_text(encoding="utf-8", errors="ignore"))}
    callers = route_callers(root)
    return sum(1 for r in routes if r.count("/") > 1 and not route_called_by(r, callers))


_CITED = (re.compile(r"`([A-Za-z_][A-Za-z0-9_!]*)\("), re.compile(r"`([a-z_][a-z0-9_]*_[a-z0-9_]*!?)`"))


def count_inventory_stale(root: pathlib.Path) -> int:
    cited = {m for p in (root / "docs" / "inventory").glob("*.md")
             for line in p.read_text(encoding="utf-8").splitlines() for pat in _CITED for m in pat.findall(line)}
    tokens: set[str] = set()
    for p in _source_files(root, ["app", "api", "python", "frontend/src", "mcp", "scripts", ".claude", ".githooks"],
                           {".jl", ".py", ".ts", ".vue", ".mjs", ".json", ".toml", ".wgsl", ".sh", ""}, tests=True):
        tokens.update(re.findall(r"[A-Za-z_][A-Za-z0-9_]*!?", p.read_text(encoding="utf-8", errors="ignore")))
    tokens.update(re.findall(r"[A-Za-z_][A-Za-z0-9_-]*", (root / "pixi.toml").read_text(encoding="utf-8")))
    return sum(1 for n in cited - THIRD_PARTY if n not in tokens and n.rstrip("!") not in tokens)


def run(root: pathlib.Path = _REPO, *, tool_run: Run = subprocess.run) -> dict:
    """Every check's count at `root`; a check that can't run is `None` with its reason in `errors`."""
    fns: dict[str, _t.Callable[[], int]] = {
        "clones": lambda: count_clones(root, tool_run), "cycles": lambda: count_cycles(root, tool_run),
        "py_dead": lambda: count_py_dead(root, tool_run),
        "routes_uncalled": lambda: count_routes_uncalled(root),
        "inventory_stale": lambda: count_inventory_stale(root)}
    counts: dict[str, int | None] = {}
    errors: dict[str, str] = {}
    for name in CHECKS:
        try:
            counts[name] = fns[name]()
        except Exception as e:   # one broken tool must not cost the other counts or the pass
            counts[name], errors[name] = None, f"{type(e).__name__}: {e}"[:300]
    return {"counts": counts, **({"errors": errors} if errors else {})}


def trend(current: dict, earlier: _t.Sequence[dict]) -> dict:
    """`current` (from `run`) with the last pass's counts beside it, and the checks rising on each of the
    last two passes. `earlier`: the earlier pass records, oldest first."""
    past = [r["coherence"]["counts"] for r in earlier if (r.get("coherence") or {}).get("counts")]
    last = past[-1] if past else {}
    before = past[-2] if len(past) > 1 else {}

    def rose(new: dict, old: dict, k: str) -> bool:
        return new.get(k) is not None and old.get(k) is not None and new[k] > old[k]
    rising = [k for k in CHECKS if rose(current["counts"], last, k) and rose(last, before, k)]
    return {**current, **({"previous": last} if last else {}), **({"rising": rising} if rising else {})}


def main(argv: list[str] | None = None) -> int:
    argv = sys.argv[1:] if argv is None else argv
    root = pathlib.Path(argv[0]).resolve() if argv else _REPO
    print(json.dumps(run(root), indent=2))
    return 0


if __name__ == "__main__":
    sys.exit(main())
