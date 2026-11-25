"""Generate docs/architecture/vue/*.md from frontend/src — import graph by regex over import statements.

    pixi run arch-diagrams          (runs this + the Julia and Python extractors)

Views:
  layers.md   — the frontend's directories as groups and the imports between them; where HTTP is
                spoken (files calling `fetch(` / `new WebSocket`).
  stores.md   — the Pinia stores: which stores lean on which, and who reads each one.
  viewer.md   — the viewer (lib/webgpu + utils/viewer + components/viewerWindow) and every import
                it makes into the rest of the app.

Groups are mechanical: the first directory under src/, split one level further for components/,
utils/ and lib/ subdirectories with at least MIN_GROUP files (smaller ones fold into "<dir> (other)").
`import type` statements are skipped (no runtime coupling); static, re-export and dynamic
`import('…')` are counted. Tests are excluded.
"""
from __future__ import annotations

import collections
import re
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
SRC = REPO / "frontend" / "src"
OUT = REPO / "docs" / "architecture" / "vue"
GENERATED = ("> **Generated** by `scripts/architecture/vue_extract.py` — do not edit; "
             "run `pixi run arch-diagrams`.\n")
MIN_GROUP = 5
MAX_EDGES = 30
BASE_STORE_MIN = 5
SPLIT = ("components", "utils", "lib")
VIEWER = ("lib/webgpu", "utils/viewer", "components/viewerWindow")

IMPORT_RE = re.compile(
    r"""(?:^|\n)\s*(?:import\s+(?!type\b)(?:[^'";]*?\sfrom\s+)?|export\s+[^'";]*?\sfrom\s+)['"]([^'"]+)['"]"""
    r"""|import\(\s*['"]([^'"]+)['"]\s*\)""")


def is_test(p: Path) -> bool:
    n = p.name
    return ".test." in n or ".spec." in n or "__fixtures__" in p.parts or p.parts[len(SRC.parts)] == "test"


def sources() -> list[Path]:
    return sorted(p for p in SRC.rglob("*") if p.suffix in (".ts", ".vue") and not is_test(p)
                  and not p.name.endswith(".d.ts"))


def rel(p: Path) -> str:
    return p.relative_to(SRC).as_posix()


def resolve(frm: Path, spec: str, known: set[str]) -> str | None:
    if not spec.startswith("."):
        return None
    base = (frm.parent / spec).resolve()
    for cand in (base, *(base.with_name(base.name + ext) for ext in (".ts", ".vue")),
                 base / "index.ts"):
        try:
            r = cand.relative_to(SRC).as_posix()
        except ValueError:
            return None
        if r in known:
            return r
    return None


def main() -> int:
    files = sources()
    known = {rel(p) for p in files}
    counts = collections.Counter()
    for r in known:
        parts = r.split("/")
        if parts[0] in SPLIT and len(parts) > 2:
            counts["/".join(parts[:2])] += 1

    def group(r: str) -> str:
        parts = r.split("/")
        if len(parts) == 1:
            return "app root"
        if parts[0] in SPLIT:
            if len(parts) > 2:
                sub = "/".join(parts[:2])
                return sub if counts[sub] >= MIN_GROUP else f"{parts[0]} (other)"
            return f"{parts[0]} (flat)"
        return parts[0]

    edges: dict[str, set[str]] = collections.defaultdict(set)
    loc = collections.Counter()
    nfiles = collections.Counter()
    http: dict[str, list[str]] = collections.defaultdict(list)
    for p in files:
        src = p.read_text(encoding="utf-8")
        r = rel(p)
        g = group(r)
        loc[g] += src.count("\n")
        nfiles[g] += 1
        code = re.sub(r"/\*.*?\*/", "", src, flags=re.S)
        code = re.sub(r"(?m)^\s*//.*$", "", code)
        for m in IMPORT_RE.finditer(code):
            t = resolve(p, m.group(1) or m.group(2), known)
            if t and t != r:
                edges[r].add(t)
        if re.search(r"\bfetch\(|new WebSocket\(|new EventSource\(", code):
            http[g].append(r)

    OUT.mkdir(parents=True, exist_ok=True)
    nid = lambda s: "".join(c if c.isalnum() else "_" for c in s)
    groups = sorted(nfiles)
    gedges: dict[tuple[str, str], int] = collections.Counter()
    for a, ts in edges.items():
        for t in ts:
            ga, gb = group(a), group(t)
            if ga != gb:
                gedges[(ga, gb)] += 1

    def top_edges(pairs):
        ranked = sorted(pairs, key=lambda kv: -kv[1])
        if not ranked:
            return []
        cut = ranked[min(MAX_EDGES, len(ranked)) - 1][1]
        return [kv for kv in ranked if kv[1] >= cut]

    # ── layers.md ──────────────────────────────────────────────────────────────
    L = ["# Vue — layers and who imports whom\n", GENERATED, f"""
Groups are the directories under `frontend/src/` (rule in the script header). `A → B (n)`: n import
statements from files in A to files in B. The strongest {MAX_EDGES} arrows are drawn; the full matrix is below.
""", "```mermaid", "flowchart LR"]
    L += [f'  {nid(g)}["{g}<br/><small>{nfiles[g]} files · {loc[g]/1000:.1f}k lines</small>"]' for g in groups]
    L += [f"  {nid(a)} -->|{n}| {nid(b)}" for (a, b), n in top_edges(gedges.items())]
    L.append("```\n")
    L.append("## Full matrix (rows import columns)\n")
    L.append("| | " + " | ".join(groups) + " |")
    L.append("|---|" + "---|" * len(groups))
    for a in groups:
        L.append(f"| **{a}** | " + " | ".join("·" if a == b else (str(gedges[(a, b)]) if gedges[(a, b)] else "")
                                            for b in groups) + " |")
    L.append("")
    nhttp = sum(len(v) for v in http.values())
    L.append("## Where the backend is called\n")
    L.append(f"Files that call `fetch(`, open a `WebSocket` or an `EventSource`: **{nhttp}** of {len(files)}. "
             "There is no single API client — each of these speaks HTTP itself.\n")
    L.append("| Group | Files |\n|---|---|")
    for g, fs in sorted(http.items(), key=lambda kv: -len(kv[1])):
        L.append(f"| {g} | {len(fs)} |")
    L.append("")
    (OUT / "layers.md").write_text("\n".join(L), encoding="utf-8")

    # ── stores.md ──────────────────────────────────────────────────────────────
    stores = sorted(r for r in known if r.startswith("stores/"))
    sname = lambda r: r[len("stores/"):].rsplit(".", 1)[0]
    readers: dict[str, collections.Counter] = {s: collections.Counter() for s in stores}
    for a, ts in edges.items():
        for t in ts:
            if t in readers and a != t:
                readers[t][group(a)] += 1
    store_in = collections.Counter(t for s in stores for t in edges.get(s, ()) if t in readers)
    base = sorted(t for t, n in store_in.items() if n >= BASE_STORE_MIN)
    L = ["# Vue — stores\n", GENERATED, f"""
The {len(stores)} Pinia stores in `frontend/src/stores/`. Arrow `a → b`: store a imports store b.

**Base stores** — imported by {BASE_STORE_MIN} or more other stores: {', '.join('`'+sname(b)+'`' for b in base)}.
Arrows into them are not drawn (they would hide everything else); each box says which base stores it
uses instead. Box also shows how many files import the store.
""", "```mermaid", "flowchart LR"]
    L.append('  subgraph BASE["base stores"]')
    for b in base:
        L.append(f'    {nid(sname(b))}["{sname(b)}<br/><small>{sum(readers[b].values())} importers</small>"]')
    L.append("  end")
    for s in stores:
        if s in base:
            continue
        uses = [sname(t) for t in sorted(edges.get(s, ())) if t in base]
        extra = f"<br/><small>uses {', '.join(uses)}</small>" if uses else ""
        L.append(f'  {nid(sname(s))}["{sname(s)}<br/><small>{sum(readers[s].values())} importers</small>{extra}"]')
    for s in stores:
        for t in sorted(edges.get(s, ())):
            if t in readers and (t not in base or s in base):
                L.append(f"  {nid(sname(s))} --> {nid(sname(t))}")
    L.append("```\n")
    L.append("## Who imports each store\n")
    L.append("| Store | Importers | By group |\n|---|---|---|")
    for s in sorted(stores, key=lambda s: -sum(readers[s].values())):
        by = ", ".join(f"{g} {n}" for g, n in readers[s].most_common())
        L.append(f"| `{sname(s)}` | {sum(readers[s].values())} | {by} |")
    L.append("")
    (OUT / "stores.md").write_text("\n".join(L), encoding="utf-8")

    # ── viewer.md ──────────────────────────────────────────────────────────────
    inside = lambda r: r.startswith(tuple(v + "/" for v in VIEWER))
    out_edges = collections.defaultdict(set)   # viewer group -> outside targets
    in_count = collections.Counter()
    for a, ts in edges.items():
        for t in ts:
            if inside(a) and not inside(t):
                out_edges[group(a)].add(t)
            elif inside(t) and not inside(a):
                in_count[group(a)] += 1
    vgroups = [g for g in groups if g in VIEWER]
    L = ["# Vue — the viewer boundary\n", GENERATED, f"""
The viewer is `{'`, `'.join(VIEWER)}`. Below: what it imports from the rest of the app
(the less, the more the viewer is a self-contained engine), and who imports it.
""", "```mermaid", "flowchart LR", '  subgraph V["viewer"]']
    L += [f'    {nid(g)}["{g}<br/><small>{nfiles[g]} files</small>"]' for g in vgroups]
    L.append("  end")
    out_groups = collections.Counter()
    for vg, ts in out_edges.items():
        for t in ts:
            out_groups[(vg, group(t))] += 1
    for (vg, og), n in sorted(out_groups.items(), key=lambda kv: -kv[1]):
        L.append(f'  {nid(vg)} -->|{n}| {nid(og)}["{og}"]')
    for g, n in in_count.most_common():
        L.append(f'  {nid(g)}["{g}"] -.->|{n}| V')
    L.append("```\n")
    L.append("Solid: the viewer imports from outside (n files). Dotted: outside imports the viewer (n imports).\n")
    L.append("## Viewer → app stores\n")
    L.append("Each store the viewer reaches into — the coupling a self-contained viewer would replace with inputs.\n")
    L.append("| Viewer group | Stores imported |\n|---|---|")
    for vg in vgroups:
        st = sorted(sname(t) for t in out_edges.get(vg, ()) if t.startswith("stores/"))
        L.append(f"| {vg} | {', '.join('`'+s+'`' for s in st) or '—'} |")
    L.append("")
    (OUT / "viewer.md").write_text("\n".join(L), encoding="utf-8")
    print(f"wrote {OUT.relative_to(REPO)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
