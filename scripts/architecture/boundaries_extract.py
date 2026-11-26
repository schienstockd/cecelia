"""Generate docs/architecture/boundaries.md (the map) and boundaries_reference.md (every touchpoint).

    pixi run arch-diagrams          (runs this with the other extractors)

Each touchpoint is extracted from the code, with the file at each end:
  1. Julia → Python runners  — every `run_py("<script>", …)` call site, the script it launches, and
                               the params keys that script reads (`params["k"]`, `.get("k")`,
                               `get_param(…, "k")`).
  2. HTTP /api routes        — routes the Julia server declares, grouped by area, with the callers on
                               each side (Vue, MCP, Python library/runners, preview worker). Route and
                               caller regexes are the weekly judge's (`scripts/judge/coherence.py`).
  3. WebSocket messages      — browser → Julia types (the `WsMsgType` table in api/src/sockets.jl and
                               the frontend's `send({ type: … })`) and Julia → browser types (literal
                               `type` fields in api/src + app/src against the frontend's `on('…')`,
                               `type === '…'`, `case '…'`). Unmatched types are listed: an emitted type
                               nobody handles, or a handler for a type nothing sends.
  4. Shared data files       — non-code files (.json, .wgsl, .toml) named from source in two or more
                               languages: schemas, shader sources, parity fixtures.
  5. On-disk formats         — which languages name each project file kind.

Mismatched ends (an unhandled type, an uncalled route, a shared file no test names) are recorded as
gaps (`gaps.py` → docs/architecture/gaps.md).

Static text matching: a route or type built at runtime from pieces is not seen. Tests are excluded.
"""
from __future__ import annotations

import collections
import importlib.util
import sys
import re
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
GENERATED = ("> **Generated** by `scripts/architecture/boundaries_extract.py` — do not edit; "
             "run `pixi run arch-diagrams`.\n")

_spec = importlib.util.spec_from_file_location("coherence", REPO / "scripts" / "judge" / "coherence.py")
coherence = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(coherence)
sys.path.insert(0, str(Path(__file__).parent))
import gaps  # noqa: E402

OUT = gaps.ARCH / "boundaries.md"
REF = gaps.ARCH / "boundaries_reference.md"

source_files = lambda dirs, suffixes: coherence._source_files(REPO, dirs, suffixes)  # noqa: E731

GENERIC = {"plugin.json", "config.json", "settings.json", "index.json"}
LANG = {".jl": "Julia", ".py": "Python", ".ts": "TS/Vue", ".vue": "TS/Vue", ".mjs": "JS tooling"}


def rel(p: Path) -> str:
    return p.relative_to(REPO).as_posix()


def read(p: Path) -> str:
    return p.read_text(encoding="utf-8", errors="ignore")


strip_comments = coherence.strip_comments


def caller_side(r: str) -> str:
    if r.startswith("frontend/"):
        return "Vue"
    if r.startswith("mcp/"):
        return "MCP"
    if r.startswith("preview/"):
        return "preview"
    if r.startswith("scripts/"):
        return "scripts"
    return "Python"


# ── 1. run_py ─────────────────────────────────────────────────────────────────

RUN_PY = re.compile(r'\brun_py\(\s*"([^"]+)"')
PARAM_KEY = re.compile(r"""(?:\bparams\s*\[\s*|\bparams\.get\(\s*|get_param\(\s*\w+\s*,\s*)['"]([A-Za-z_][\w]*)['"]""")


def runpy_section():
    calls = collections.defaultdict(list)   # script -> [caller file:line]
    dynamic = []
    for p in source_files(["app/src", "api/src"], {".jl"}):
        for i, line in enumerate(read(p).splitlines(), 1):
            if line.lstrip().startswith("#") or "function run_py" in line:
                continue
            m = RUN_PY.search(line)
            if m:
                calls[m.group(1)].append(f"{rel(p)}:{i}")
            elif re.search(r"\brun_py\(", line):
                dynamic.append(f"{rel(p)}:{i}")
    rows = []
    for script, sites in sorted(calls.items()):
        base = REPO / ("app/src" if script.startswith("tasks/") else "python/cecelia") / script
        keys = sorted(set(PARAM_KEY.findall(read(base)))) if base.is_file() else None
        rows.append((script, sites, base, keys))
    runners = {rel(p) for p in (REPO / "app/src/tasks").rglob("*_run.py")}
    launched = {rel(b) for _, _, b, _ in rows if b.is_file()}
    return rows, dynamic, sorted(runners - launched)


# ── 2. HTTP routes ────────────────────────────────────────────────────────────

HANDLER = re.compile(r'"(/api/[^"$]*)"\s*=>.*?\b(api_[A-Za-z0-9_!]+)\(')


def routes_section():
    # The route table lives in api/src/server.jl (`"/api/x" => (req, b) -> api_x(req)`); the Julia
    # side of a route is the file that DEFINES its handler, not the table.
    jl_srcs = {rel(p): read(p) for p in source_files(["api/src"], {".jl"})}
    defined_in = {}
    for f, t in jl_srcs.items():
        for m in re.finditer(r"^function\s+(api_[A-Za-z0-9_!]+)\(|^(api_[A-Za-z0-9_!]+)\(.*\)\s*=", t, flags=re.M):
            defined_in.setdefault(m.group(1) or m.group(2), f)
    routes = collections.defaultdict(set)   # route -> julia files (handler definitions)
    exempt = {}                             # route -> why it has no caller, on purpose
    for f, t in jl_srcs.items():
        for line in t.splitlines():
            for m in coherence._JL_ROUTE.finditer(line):
                r = m.group(1).rstrip("/")
                if r.count("/") <= 1:
                    continue
                if coherence.ROUTE_EXEMPT in line:
                    exempt[r] = line.split(coherence.ROUTE_EXEMPT, 1)[1].strip()
                h = HANDLER.search(line)
                routes[r].add(defined_in.get(h.group(2), f) if h else f)
    callers = coherence.route_callers(REPO)
    called_by = lambda r: {rel(p) for p in coherence.route_called_by(r, callers)}  # noqa: E731
    area = lambda r: "/".join(r.split("/")[:3])  # noqa: E731
    by_area = collections.defaultdict(lambda: {"routes": 0, "jl": set(), "callers": collections.defaultdict(set),
                                               "uncalled": []})
    for r, jl in routes.items():
        a = by_area[area(r)]
        a["routes"] += 1
        a["jl"] |= jl
        fs = called_by(r)
        for f in fs:
            a["callers"][caller_side(f)].add(f)
        if not fs and r not in exempt:
            a["uncalled"].append(r)
    return routes, by_area, exempt


# ── 3. WebSocket ──────────────────────────────────────────────────────────────

# The `type` field set on a line — `type = "a"`, `"type" => "a"`, `f["type"] = "a"`, also `ok ? "a" : "b"`.
JL_TYPE_LINE = re.compile(r'''(?:\btype\s*=|"type"\s*=>|\["type"\]\s*=)(?!=)(.*)$''', re.M)
JL_CONSUME = re.compile(r'''\b(?:t|type|typ)\s*==\s*"([a-z][a-z0-9_]*:[a-z0-9_:-]+)"''')
JL_TYPE_LIT = re.compile(r'"([a-z][a-z0-9_]*(?::[a-z0-9_:-]+)?)"')
TS_CONSUME = re.compile(r"""(?:\.on\(\s*|type\s*===\s*|case\s+)['"]([a-z][a-z0-9_:-]*)['"]""")
TS_SEND = re.compile(r"""\.send\(\s*\{\s*type:\s*['"]([a-z][a-z0-9_:-]*)['"]""")


def ws_section():
    sockets = read(REPO / "api/src/sockets.jl")
    table = sockets[sockets.index("_WS_MSG_TYPE_STR"):]
    table = table[: table.index(")\n")]
    inbound = set(re.findall(r'=>\s*"([^"]+)"', table)) - {"unknown"}
    sent = collections.defaultdict(set)
    consumed = collections.defaultdict(set)
    for p in source_files(["frontend/src"], {".ts", ".vue"}):
        src = read(p)
        for t in TS_SEND.findall(src):
            sent[t].add(rel(p))
        for t in TS_CONSUME.findall(src):
            consumed[t].add(rel(p))
    emitted = collections.defaultdict(set)
    jl_consumed = collections.defaultdict(set)   # kept apart: a reader is not an emitter (below)
    for p in source_files(["api/src", "app/src"], {".jl"}):
        code = strip_comments(read(p), ".jl")
        # A frame Julia itself reads back (`t == "runner:log"`: the runner's events, relayed by the API
        # server) has a handler — on the Julia side.
        for t in JL_CONSUME.findall(code):
            jl_consumed[t].add(rel(p))
        for line in JL_TYPE_LINE.findall(code):
            for t in JL_TYPE_LIT.findall(line):
                emitted[t].add(rel(p))
        # A type handed to a helper that sets the field (`_ask_browser("viewer:request", …)`) is still a
        # literal: any quoted string equal to a type the browser handles counts as emitting it.
        for t in consumed:
            if f'"{t}"' in code:
                emitted[t].add(rel(p))
    # Keep only message-shaped types: namespaced `a:b`, `*_changed/_updated`, or handled by the socket store.
    msgish = lambda t: ":" in t or t.endswith(("_changed", "_updated"))  # noqa: E731
    # A plain word (`pong`, `error`) only when the socket dispatcher itself handles it — `case 'bar'` in a
    # plot switch is not a message.
    ws_handled = lambda t: "frontend/src/stores/ws.ts" in consumed.get(t, ())  # noqa: E731
    outbound = sorted(t for t in emitted if msgish(t) or ws_handled(t))
    outbound = [t for t in outbound if t not in inbound or t in consumed]
    orphans_out = [t for t in outbound if t not in consumed and t not in jl_consumed]
    for t, fs in jl_consumed.items():   # shown as handled in the reference table, Julia side
        if t in emitted and t not in consumed:
            consumed[t] = set(fs)
    orphans_in = sorted(t for t in consumed if msgish(t) and t not in emitted and t not in inbound
                        and t not in sent)
    return inbound, sent, outbound, emitted, consumed, orphans_out, orphans_in


# ── 4. shared data files ──────────────────────────────────────────────────────

def shared_files_section():
    data = [p for d in ("app/src", "api/src", "frontend/src", "python/cecelia", "mcp", "preview", "pluto")
            for p in (REPO / d).rglob("*")
            if p.is_file() and p.suffix in {".json", ".wgsl", ".toml"}
            and not ({"node_modules", "__fixtures__", "tests", "test"} & set(p.relative_to(REPO).parts))
            and p.name not in {"package.json", "package-lock.json", "tsconfig.json", "Project.toml",
                               "Manifest.toml", "pyproject.toml"}]
    # Task specs (tasks/<cat>/<name>.json) are read by Julia and served to the browser over HTTP
    # (`/api/tasks/definitions`), never named from TS — reported as one row, not per file.
    specs = [p for p in data if rel(p).startswith("app/src/tasks/") and p.suffix == ".json"]
    data = [p for p in data if p not in specs]
    by_name = collections.defaultdict(list)
    for p in data:
        by_name[p.name].append(p)
    refs = collections.defaultdict(lambda: collections.defaultdict(set))
    srcs = list(source_files(["app/src", "api/src", "frontend/src", "python/cecelia", "mcp", "preview",
                              "scripts", "pluto"], set(LANG)))
    texts = {s: read(s) for s in srcs}
    for name in by_name:
        pat = re.compile(r"(?<![\w.-])" + re.escape(name) + r"(?![\w])")
        for s, t in texts.items():
            if name in t and pat.search(t):   # substring first: the regex alone over every file is ~40 s
                refs[name][LANG[s.suffix]].add(rel(s))
    # A generic name needs its directory named too, or a different file of the same name counts.
    for name in GENERIC & set(refs):
        parent = by_name[name][0].parent.name
        for lang in list(refs[name]):
            refs[name][lang] = {f for f in refs[name][lang] if parent in texts[REPO / f]}
            if not refs[name][lang]:
                del refs[name][lang]
    shared = {n: r for n, r in refs.items() if len([lang for lang in r if lang != "JS tooling"]) >= 2}
    return specs, by_name, shared


# ── 5. on-disk formats ────────────────────────────────────────────────────────

DISK = [
    ("project.json", r"project\.json"),
    ("ccid.json (image / set record)", r"ccid\.json|STATE_FILENAME"),
    ("OME-ZARR (.zarr)", r"\.zarr\b"),
    (".h5ad (cell data)", r"\.h5ad\b"),
    ("gating JSON", r"gating[/_][^\s'\"]*\.json|gating_history_path|gating_path"),
    ("run log", r"run_log|runLog|run-log"),
]


def disk_section():
    out = []
    srcs = list(source_files(["app/src", "api/src", "frontend/src", "python/cecelia", "mcp", "preview"],
                             {".jl", ".py", ".ts", ".vue"}))
    texts = {s: read(s) for s in srcs}
    for label, pat in DISK:
        rx = re.compile(pat)
        by = collections.Counter(LANG[s.suffix] for s, t in texts.items() if rx.search(t))
        out.append((label, by))
    return out


def test_texts() -> dict[str, str]:
    """Every test source, for "does any test name this shared file"."""
    out = {}
    for d in ("app/test", "api/test", "python/cecelia/tests", "mcp", "preview"):
        for p in (REPO / d).rglob("*"):
            if p.suffix in {".jl", ".py", ".ts", ".mjs"} and (d.endswith("test") or d.endswith("tests")
                                                              or "test" in p.name):
                out[rel(p)] = read(p)
    for p in (REPO / "frontend").rglob("*.test.ts"):
        if "node_modules" not in p.parts:
            out[rel(p)] = read(p)
    return out


# ── render ────────────────────────────────────────────────────────────────────

def fmt_files(fs, n=3):
    fs = sorted(fs)
    s = ", ".join(f"`{f}`" for f in fs[:n])
    return s + (f" +{len(fs) - n}" if len(fs) > n else "")


def main() -> int:
    rp_rows, rp_dynamic, rp_unlaunched = runpy_section()
    routes, by_area, exempt = routes_section()
    inbound, sent, outbound, emitted, consumed, orphans_out, orphans_in = ws_section()
    specs, by_name, shared = shared_files_section()
    disk = disk_section()
    tests = test_texts()
    guarded = {n: sorted(f for f, t in tests.items() if n in t) for n in shared}

    side_files = collections.defaultdict(set)
    for a in by_area.values():
        for side, fs in a["callers"].items():
            side_files[side] |= fs
    pair = lambda a, b: sum(1 for r in shared.values() if a in r and b in r)  # noqa: E731
    n_tasks_runpy = sum(1 for s, *_ in rp_rows if s.startswith("tasks/"))
    uncalled = sorted(r for d in by_area.values() for r in d["uncalled"])
    ws_no_handler = sorted(t for t in sent if t not in inbound)

    # ── gaps ──
    G = [{"kind": "ws-unhandled", "subject": f"`{t}`", "where": sorted(emitted[t])} for t in orphans_out]
    G += [{"kind": "ws-no-emitter", "subject": f"`{t}`", "where": sorted(consumed[t])} for t in orphans_in]
    G += [{"kind": "ws-no-handler", "subject": f"`{t}`", "where": sorted(sent[t])} for t in ws_no_handler]
    G += [{"kind": "route-uncalled", "subject": f"`{r}`", "where": sorted(routes[r])} for r in uncalled]
    G += [{"kind": "runner-unlaunched", "subject": f"`{r}`", "where": [r]} for r in rp_unlaunched]
    G += [{"kind": "shared-file-unguarded", "subject": f"`{n}`",
           "detail": "read by " + ", ".join(sorted(lang for lang in shared[n] if lang != "JS tooling")),
           "where": [rel(p) for p in by_name[n]]} for n in sorted(shared) if not guarded[n]]
    gaps.record("boundaries", G)
    gap_count = collections.Counter(g["kind"] for g in G)
    gl = lambda k, what: f"[{gap_count[k]} {what}](gaps.md#{k})" if gap_count[k] else "none"  # noqa: E731

    # ── boundaries.md: the map ──
    L = ["# Cross-language boundaries\n", GENERATED, """
Where one language hands off to another: which file holds each end, how the contract is kept, and
what does not line up. The processes and ports are in [`system.md`](system.md); the step-by-step
crossings are in [`flows/`](flows/). Every row of every touchpoint:
[`boundaries_reference.md`](boundaries_reference.md).
""", "```mermaid", "flowchart LR",
        '  vue["Vue / TS<br/>frontend/src"]',
        '  jl["Julia<br/>api/src + app/src"]',
        '  py["Python runners + cecelia lib"]',
        '  mcp["MCP server<br/>mcp/cecelia_mcp"]',
        '  disk[("Project dir")]',
        f'  vue -->|"HTTP: {len(side_files["Vue"])} files call it"| jl',
        f'  vue -->|"WS: {len(inbound)} types in"| jl',
        f'  jl -->|"WS: {len(outbound)} types out"| vue',
        f'  jl -->|"run_py: {len(rp_rows)} scripts"| py',
        f'  mcp -->|"HTTP: {len(side_files["MCP"])} files"| jl',
        f'  py -->|"HTTP: {len(side_files["Python"])} file"| jl',
        f'  vue -.-|"{pair("TS/Vue", "Python")} shared files"| py',
        f'  jl -.-|"{pair("Julia", "Python")} shared files"| py',
        f'  vue -.-|"{pair("TS/Vue", "Julia")} shared files"| jl',
        '  jl --> disk',
        '  py --> disk',
        "```\n",
        "Solid: a call or a message. Dotted: a data file both sides read (a schema, a shader, a constants table).\n",
        "## Where each boundary lives\n",
        "| Boundary | Julia end | Other end | Contract kept by | Size | Gaps |",
        "|---|---|---|---|---|---|",
        f"| Julia → Python | `app/src/py_runner.jl` `run_py` | `script_params()` in "
        f"`python/cecelia/utils/script_utils.py` | params JSON keys; `[PROGRESS] n/total`; exit code | "
        f"{len(rp_rows)} scripts ({n_tasks_runpy} task runners) | {gl('runner-unlaunched', 'never launched')} |",
        f"| Vue → Julia, HTTP | route table in `api/src/server.jl`, handlers across `api/src` | "
        f"`frontend/src/utils/api.ts` (the client), routes named in {len(side_files['Vue'])} files | "
        f"`docs/API.md` → *Route index* | "
        f"{len(routes)} routes in {len(by_area)} areas | {gl('route-uncalled', 'uncalled routes')} |",
        f"| MCP → Julia, HTTP | same routes | {fmt_files(side_files['MCP'], 2)} | `mcp/cecelia_mcp` tool specs | "
        f"{len(side_files['MCP'])} files | — |",
        f"| Python → Julia, HTTP | same routes | {fmt_files(side_files['Python'], 2)} | — | "
        f"{len(side_files['Python'])} file | — |",
        f"| Vue ↔ Julia, WebSocket | `api/src/sockets.jl` `handle_message` (in), `api/src/server.jl` "
        f"`broadcast_ws` (out) | `frontend/src/stores/ws.ts` | `WsMsgType` table (in); type strings (out) | "
        f"{len(inbound)} in, {len(outbound)} out | {gl('ws-unhandled', 'unhandled')}, "
        f"{gl('ws-no-emitter', 'never sent')}, {gl('ws-no-handler', 'sent, not handled')} |",
        f"| Shared data files | — | — | a test that names the file | {len(shared)} files + {len(specs)} task "
        f"specs | {gl('shared-file-unguarded', 'with no test')} |",
        "",
        "## Shared data files\n",
        f"Data files named from two or more languages. The {len(specs)} task specs "
        "(`app/src/tasks/<category>/<name>.json`) are not listed: Julia reads them and serves them over "
        "`/api/tasks/definitions`, so the browser never names one. *Test* names a test file that names the "
        "file — the thing that fails if the two sides drift.\n",
        "| File | Read by | Test |", "|---|---|---|"]
    for name, langs in sorted(shared.items()):
        by = ", ".join(sorted(lang for lang in langs if lang != "JS tooling"))
        t = fmt_files(guarded[name], 1) if guarded[name] else "**none**"
        L.append(f"| `{name}` | {by} | {t} |")
    L.append("")
    OUT.write_text("\n".join(L), encoding="utf-8")

    # ── boundaries_reference.md: every row ──
    R = ["# Cross-language boundaries — reference\n", GENERATED,
         "\nEvery touchpoint, row by row. The map and what does not line up: [`boundaries.md`](boundaries.md). "
         "Look things up here; do not read it through.\n",
         "## 1. Julia → Python: `run_py`\n",
         "*Keys read* is what the script takes from its params (`params[\"k\"]`, `.get(\"k\")`, `get_param(…, \"k\")`) "
         "— the Python side of the interface.\n",
         "| Script | Launched from | Keys read |", "|---|---|---|"]
    for script, sites, base, keys in rp_rows:
        k = "**missing file**" if keys is None else (", ".join(f"`{x}`" for x in keys[:12]) + (f" +{len(keys)-12}" if len(keys) > 12 else "") or "—")
        R.append(f"| `{script}` | {fmt_files(sites, 2)} | {k} |")
    R.append("")
    if rp_dynamic:
        R.append("Call sites with a computed script name (not resolved): " + fmt_files(rp_dynamic, 8) + "\n")
    R += ["## 2. HTTP: `/api` routes by area\n",
          "Caller files per side; matching rules from `scripts/judge/coherence.py`, comments excluded.\n",
          "| Area | Routes | Julia side | Vue | MCP | Python | preview | No caller |", "|---|---|---|---|---|---|---|---|"]
    for a, d in sorted(by_area.items(), key=lambda kv: (-kv[1]["routes"], kv[0])):
        c = d["callers"]
        R.append(f"| `{a}` | {d['routes']} | {fmt_files(d['jl'], 2)} | {len(c.get('Vue', ())) or ''} | "
                 f"{len(c.get('MCP', ())) or ''} | {len(c.get('Python', ())) or ''} | {len(c.get('preview', ())) or ''} | "
                 f"{len(d['uncalled']) or ''} |")
    if exempt:
        R += ["", "Routes with no caller on purpose (`ARCH-EXEMPT:` on the route's line):\n"]
        R += [f"- `{r}` — {why}" for r, why in sorted(exempt.items())]
    R += ["", "## 3. WebSocket `/ws`\n", "### Browser → Julia\n", "| Type | Sent from | Handled in |", "|---|---|---|"]
    for t in sorted(inbound | set(sent)):
        R.append(f"| `{t}` | {fmt_files(sent.get(t, ()), 3) or '—'} | "
                 f"{'`api/src/sockets.jl`' if t in inbound else '**no handler**'} |")
    R += ["", "### Julia → browser\n", "| Type | Emitted in | Handled in |", "|---|---|---|"]
    for t in outbound:
        R.append(f"| `{t}` | {fmt_files(emitted[t], 2)} | {fmt_files(consumed.get(t, ()), 3) or '**nothing handles it**'} |")
    R += ["", "## 4. Shared data files — where each is named\n", "| File | Path | Named from |", "|---|---|---|"]
    for name, langs in sorted(shared.items()):
        where = ", ".join(f"`{rel(p)}`" for p in by_name[name])
        R.append(f"| `{name}` | {where} | " + "; ".join(f"{lang}: {fmt_files(fs, 2)}" for lang, fs in sorted(langs.items())) + " |")
    R += ["", "## 5. On-disk formats — who names them\n",
          "Files per language that name each kind of project file (a reference, not necessarily a read or a "
          "write). Readers and writers: `docs/OBJECTMODEL.md`, `docs/DATAMODEL.md`.\n",
          "| File kind | Julia | Python | TS/Vue |", "|---|---|---|---|"]
    for label, by in disk:
        R.append(f"| {label} | {by.get('Julia', '')} | {by.get('Python', '')} | {by.get('TS/Vue', '')} |")
    R.append("")
    REF.write_text("\n".join(R), encoding="utf-8")
    print(f"wrote {OUT}, {REF}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
