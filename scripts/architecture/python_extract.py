"""Generate docs/architecture/python/*.md from the Python source — imports and classes via `ast`.

    pixi run arch-diagrams          (runs this + the Julia and Vue extractors)

Python in this repo is mostly functions, not classes, so its "interfaces" are the helpers everybody
imports. Views:
  packages.md   — the Python parts (task runners by category, cecelia.utils, writers, MCP server,
                  preview worker, dev tooling) and the imports between them.
  utils.md      — inside cecelia.utils: the most-imported modules, the edges among them, import
                  cycles, the most-imported functions (the de-facto API) and the class hierarchies.

Static: `import x` / `from x import y` statements anywhere in a file, including inside functions
(lazy imports are the norm here — see the dask rule in CLAUDE.md). A dynamic `importlib` call is not
seen. Tests are excluded.
"""
from __future__ import annotations

import ast
import collections
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
OUT = REPO / "docs" / "architecture" / "python"
GENERATED = ("> **Generated** by `scripts/architecture/python_extract.py` — do not edit; "
             "run `pixi run arch-diagrams`.\n")
MAX_EDGES = 25
TOP_MODULES = 20

# Parts: (label, glob root, how a file maps to its part). Runners are grouped by task category.
def part_of(rel: str) -> str | None:
    p = rel.split("/")
    if "tests" in p or p[-1].startswith("test_"):
        return None
    if rel.startswith("app/src/tasks/"):
        return f"runners: {p[3]}"
    if rel.startswith("app/src/"):
        return "runners: other"
    if rel.startswith("python/cecelia/utils/"):
        return "cecelia.utils"
    if rel.startswith("python/cecelia/writers/"):
        return "cecelia.writers"
    if rel.startswith("python/cecelia/effectiveness/"):
        return "dev tooling (effectiveness)"
    if rel == "python/cecelia/cecelia_client.py":
        return "cecelia_client"
    if rel.startswith("mcp/cecelia_mcp/"):
        return "MCP server (cecelia_mcp)"
    if rel.startswith("preview/"):
        return "preview worker"
    return None


ROOTS = ["app/src", "python/cecelia", "mcp/cecelia_mcp", "preview"]


def files():
    for r in ROOTS:
        for f in sorted((REPO / r).rglob("*.py")):
            rel = f.relative_to(REPO).as_posix()
            if part_of(rel):
                yield rel, f


def module_name(rel: str) -> str | None:
    """Importable dotted name for a library file, None for a runner (run by path, never imported)."""
    if rel.startswith("python/"):
        m = rel[len("python/"):-3].replace("/", ".")
    elif rel.startswith("mcp/"):
        m = rel[len("mcp/"):-3].replace("/", ".")
    else:
        return None
    return m[: -len(".__init__")] if m.endswith(".__init__") else m


def imports(tree: ast.AST, this_mod: str | None):
    """Yield (module, [names]) for every import, resolving relative imports against this_mod."""
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            for a in node.names:
                yield a.name, []
        elif isinstance(node, ast.ImportFrom):
            mod = node.module or ""
            if node.level and this_mod:
                base = this_mod.split(".")[: -node.level]
                mod = ".".join(base + ([mod] if mod else []))
            yield mod, [a.name for a in node.names]


def main() -> int:
    mods: dict[str, str] = {}          # dotted module -> rel path
    trees: dict[str, ast.AST] = {}
    loc = collections.Counter()
    nfiles = collections.Counter()
    for rel, f in files():
        src = f.read_text(encoding="utf-8")
        trees[rel] = ast.parse(src, filename=rel)
        loc[part_of(rel)] += src.count("\n")
        nfiles[part_of(rel)] += 1
        m = module_name(rel)
        if m:
            mods[m] = rel

    def resolve(mod: str, names: list[str]) -> list[str]:
        """Repo files an import statement reaches (a `from pkg import submodule` reaches the submodule)."""
        out = []
        for n in names:
            if f"{mod}.{n}" in mods:
                out.append(mods[f"{mod}.{n}"])
        if not out:
            parts = mod.split(".")
            while parts:
                if ".".join(parts) in mods:
                    out.append(mods[".".join(parts)]); break
                parts.pop()
        return out

    part_edges: dict[tuple[str, str], set[str]] = collections.defaultdict(set)
    file_edges: dict[str, set[str]] = collections.defaultdict(set)
    symbol_use: dict[tuple[str, str], set[str]] = collections.defaultdict(set)  # (module, name) -> importers
    for rel, tree in trees.items():
        a = part_of(rel)
        for mod, names in imports(tree, module_name(rel)):
            for target in resolve(mod, names):
                if target == rel:
                    continue
                file_edges[rel].add(target)
                b = part_of(target)
                if b != a:
                    part_edges[(a, b)].add(rel)
            if mod in mods:
                for n in names:
                    if f"{mod}.{n}" not in mods and n != "*":
                        symbol_use[(mod, n)].add(rel)
        # Module-style use: `import cecelia.utils.zarr_utils as zu` / `from cecelia.utils import zarr_utils`
        # then `zu.open_as_zarr(...)` — count the attribute as a use of that function.
        alias: dict[str, str] = {}
        for node in ast.walk(tree):
            if isinstance(node, ast.Import):
                for a in node.names:
                    if a.name in mods and a.asname:
                        alias[a.asname] = a.name
            elif isinstance(node, ast.ImportFrom) and node.module:
                for a in node.names:
                    if f"{node.module}.{a.name}" in mods:
                        alias[a.asname or a.name] = f"{node.module}.{a.name}"
        for node in ast.walk(tree):
            if isinstance(node, ast.Attribute) and isinstance(node.value, ast.Name) and node.value.id in alias:
                symbol_use[(alias[node.value.id], node.attr)].add(rel)

    OUT.mkdir(parents=True, exist_ok=True)
    nid = lambda s: "".join(c if c.isalnum() else "_" for c in s)
    label = lambda p: f'{p}<br/><small>{nfiles[p]} files · {loc[p]/1000:.1f}k lines</small>'

    # ── packages.md ────────────────────────────────────────────────────────────
    L = ["# Python — the parts and who imports whom\n", GENERATED, """
Python has three jobs here: **task runners** (`app/src/tasks/<category>/*_run.py`, run by path through
`run_py`, never imported), the **`cecelia` IO library** they import (`python/cecelia/utils` +
`writers`), and side processes (the MCP server Claude talks to, the preview worker). Dev tooling
(recital, judge) lives in `cecelia.effectiveness` and is not part of the app.

`A → B (n)`: n files in A import something from B. Runners are grouped by task category.
""", "```mermaid", "flowchart LR"]
    parts = sorted({p for p in nfiles})
    runner_parts = [p for p in parts if p.startswith("runners:")]
    L.append('  subgraph RUN["task runners — app/src/tasks (run by run_py)"]')
    L += [f'    {nid(p)}["{label(p)}"]' for p in runner_parts]
    L.append("  end")
    L += [f'  {nid(p)}["{label(p)}"]' for p in parts if p not in runner_parts]
    for (a, b), fs in sorted(part_edges.items(), key=lambda kv: -len(kv[1])):
        L.append(f"  {nid(a)} -->|{len(fs)}| {nid(b)}")
    L.append("```\n")
    L.append("Runners and side processes never import each other; everything shared goes through "
             "`cecelia.utils`. An arrow between two runner groups or into a runner would be a violation.\n")
    bad = [(a, b) for (a, b) in part_edges if b.startswith("runners:")]
    L.append(f"Arrows into a runner group today: **{len(bad)}**"
             + ("" if not bad else " — " + ", ".join(f"{a} → {b}" for a, b in bad)) + "\n")
    (OUT / "packages.md").write_text("\n".join(L), encoding="utf-8")

    # ── utils.md ───────────────────────────────────────────────────────────────
    utils = {r for r in trees if r.startswith("python/cecelia/utils/")}
    fan_in = collections.Counter()
    for src, tgts in file_edges.items():
        for t in tgts:
            if t in utils:
                fan_in[t] += 1
    short = lambda r: r.split("/")[-1][:-3]
    top = [r for r, _ in fan_in.most_common(TOP_MODULES)]
    L = ["# Python — inside `cecelia.utils`\n", GENERATED, f"""
`cecelia.utils` is {len(utils)} modules, {sum(Path(REPO / u).read_text(encoding='utf-8').count(chr(10)) for u in utils)/1000:.1f}k lines. Which ones carry the weight, and how they lean on each other.

## The {TOP_MODULES} most-imported modules

Box: module and how many repo files import it. Arrow `a → b`: a imports b. Only edges among these {TOP_MODULES}.
""", "```mermaid", "flowchart TB"]
    for r in top:
        L.append(f'  {nid(short(r))}["{short(r)}<br/><small>imported by {fan_in[r]}</small>"]')
    for r in top:
        for t in sorted(file_edges.get(r, ())):
            if t in top:
                L.append(f"  {nid(short(r))} --> {nid(short(t))}")
    L.append("```\n")

    # cycles (Tarjan SCC over utils)
    idx, low, stack, on, sccs, counter = {}, {}, [], set(), [], [0]
    sys.setrecursionlimit(10000)

    def strong(v):
        idx[v] = low[v] = counter[0]; counter[0] += 1; stack.append(v); on.add(v)
        for w in file_edges.get(v, ()):
            if w not in utils:
                continue
            if w not in idx:
                strong(w); low[v] = min(low[v], low[w])
            elif w in on:
                low[v] = min(low[v], idx[w])
        if low[v] == idx[v]:
            comp = []
            while True:
                w = stack.pop(); on.discard(w); comp.append(w)
                if w == v:
                    break
            if len(comp) > 1:
                sccs.append(sorted(comp))
    for u in sorted(utils):
        if u not in idx:
            strong(u)
    L.append("## Import cycles\n")
    if sccs:
        L.append("Groups of modules that import each other (directly or round a loop). Lazy imports "
                 "inside functions make these legal at runtime, but each group is one unit to understand.\n")
        L += [f"- {', '.join('`'+short(m)+'`' for m in c)}" for c in sccs]
    else:
        L.append("None.")
    L.append("")

    L.append("## The de-facto API — most-imported functions and classes\n")
    L.append("`from cecelia.utils.x import y` counted per importing file, or `zarr_utils.open_as_zarr(…)` after a module import, counted per using file; dev tooling included. "
             "These are the interfaces a change ripples through.\n")
    L.append("| Name | Module | Imported by |\n|---|---|---|")
    ranked = sorted(symbol_use.items(), key=lambda kv: -len(kv[1]))
    ranked = [kv for kv in ranked if kv[0][0].startswith(("cecelia.utils", "cecelia.writers"))
              and not kv[0][1].startswith("_")]
    for (mod, n), users in ranked[:30]:
        L.append(f"| `{n}` | `{mod}` | {len(users)} |")
    L.append("")

    L.append("## Class hierarchies\n")
    L.append("Classes with a base class defined in the repo (exceptions and NamedTuples left out).\n")
    classes: dict[str, tuple[str, list[str]]] = {}
    for rel, tree in trees.items():
        for node in ast.walk(tree):
            if isinstance(node, ast.ClassDef):
                bases = [b.id if isinstance(b, ast.Name) else getattr(b, "attr", "") for b in node.bases]
                classes[node.name] = (rel, bases)
    rows = [(n, b, rel) for n, (rel, bases) in classes.items() for b in bases if b in classes]
    if rows:
        L += ["```mermaid", "classDiagram"]
        for n, b, rel in sorted(rows):
            L.append(f"  {b} <|-- {n}")
        L.append("```\n")
        L += [f"- `{n}` ← `{b}` — `{rel}`" for n, b, rel in sorted(rows)]
    L.append("")
    (OUT / "utils.md").write_text("\n".join(L), encoding="utf-8")
    print(f"wrote {OUT.relative_to(REPO)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
