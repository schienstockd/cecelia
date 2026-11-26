"""Coherence gaps found by the architecture extractors, one list across the languages.

Each extractor calls `record(source, items)`, which writes `docs/architecture/gaps/<source>.json`.
Run as a script (last step of `pixi run arch-diagrams`), this composes them into
`docs/architecture/gaps.md`.

A gap is a place where the code does not line up with itself: two ends of a boundary that do not
match, a loop that makes parts inseparable, a contract only half the implementers follow. Each item
says what it is, where to look, and what closing it would mean. The extractors only see static text,
so an item can be a false alarm (a name built at runtime); closing one means fixing the code or
confirming it in a comment the extractor can see — never editing the JSON.
"""
from __future__ import annotations

import json
import os
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
# Where every extractor writes. CECELIA_ARCH_OUT redirects all of it — the ratchet test regenerates
# into a temp dir and never touches the committed files.
ARCH = Path(os.environ.get("CECELIA_ARCH_OUT") or REPO / "docs" / "architecture")
DIR = ARCH / "gaps"
OUT = ARCH / "gaps.md"
BASELINE = Path(__file__).with_name("gaps_baseline.json")

# kind -> (section, one-line meaning, what closing it means). Order = order in gaps.md.
KINDS = {
    "ws-unhandled": ("Broken ends", "Julia broadcasts a WebSocket type nothing in the browser handles",
                     "handle it in the frontend, or stop sending it"),
    "ws-no-emitter": ("Broken ends", "The browser handles a WebSocket type no Julia literal emits",
                      "delete the handler, or make the emitter's type a literal"),
    "ws-no-handler": ("Broken ends", "The browser sends a WebSocket type `sockets.jl` does not handle",
                      "add it to the `WsMsgType` table, or stop sending it"),
    "route-uncalled": ("Broken ends", "An `/api` route no Vue, MCP or Python code calls",
                       "delete the route, or name the caller (a route built from pieces is invisible here)"),
    "runner-unlaunched": ("Broken ends", "A `*_run.py` runner no literal `run_py` launches",
                          "delete it, or launch it by a literal name"),
    "shared-file-unguarded": ("Loose contracts", "A data file two languages read, named by no test",
                              "add a parity or schema test that names the file"),
    "julia-upward": ("Layers", "A Julia function called from an area below its own in the declared order",
                     "move it down to the lowest tier that calls it (or a shared lower area), or — if the "
                     "order is wrong — change `[layers]` in `scripts/architecture/julia_areas.toml`. Most "
                     "callers first: one move closes every listed call"),
    "viewer-store": ("Layers", "The viewer engine (`lib/webgpu`, `utils/viewer`) imports an app store",
                     "pass what it needs in as an input instead (`lib/webgpu` already holds this rule). "
                     "The viewer's own `viewer` store and `log` are allowed"),
    "py-cycle": ("Tangles", "Python modules that import each other", "break the loop"),
    "store-cycle": ("Tangles", "Pinia stores that import each other", "break the loop"),
}


def cycles(nodes, succ) -> list[list]:
    """Strongly connected groups of more than one node (Tarjan). succ(n) -> iterable of nodes."""
    sys.setrecursionlimit(max(sys.getrecursionlimit(), 10 * len(nodes) + 100))
    idx, low, stack, on, out, n = {}, {}, [], set(), [], [0]

    def visit(v):
        idx[v] = low[v] = n[0]; n[0] += 1; stack.append(v); on.add(v)
        for w in succ(v):
            if w not in nodes:
                continue
            if w not in idx:
                visit(w); low[v] = min(low[v], low[w])
            elif w in on:
                low[v] = min(low[v], idx[w])
        if low[v] == idx[v]:
            comp = []
            while True:
                w = stack.pop(); on.discard(w); comp.append(w)
                if w == v:
                    break
            if len(comp) > 1:
                out.append(sorted(comp))
    for v in sorted(nodes):
        if v not in idx:
            visit(v)
    return out


def record(source: str, items: list[dict]) -> None:
    """items: {"kind": KINDS key, "subject": str, "where": [paths], "detail": str, "rank": int (both optional;
    lower rank = listed first — the easiest to close)}."""
    for it in items:
        assert it["kind"] in KINDS, it["kind"]
    DIR.mkdir(parents=True, exist_ok=True)
    items = sorted(items, key=lambda it: (list(KINDS).index(it["kind"]), it["subject"]))
    (DIR / f"{source}.json").write_text(json.dumps(items, indent=1, ensure_ascii=False) + "\n", encoding="utf-8")


def compose() -> int:
    items = load()
    by_kind = {k: [it for it in items if it["kind"] == k] for k in KINDS}
    sections = list(dict.fromkeys(s for s, _, _ in KINDS.values()))
    L = ["# Coherence gaps\n",
         "> **Generated** by `scripts/architecture/gaps.py` from what the other extractors found — do not "
         "edit; run `pixi run arch-diagrams`. Machine-readable: `gaps/*.json`.\n",
         "Where the code breaks the rules in [`README.md`](README.md#rules-and-how-they-are-enforced). Each "
         "count may only go down: `test_architecture_gaps.py` fails a change that adds one. Static text "
         "matching, so an item can be a false alarm (a name built at runtime) — close it by making the name "
         "a literal. After closing gaps: `pixi run arch-diagrams && pixi run arch-gaps-baseline`.\n",
         "| Section | Kind | Count |", "|---|---|---|"]
    for k, (sec, meaning, _) in KINDS.items():
        if by_kind[k]:
            L.append(f"| {sec} | [{meaning}](#{k}) | {len(by_kind[k])} |")
    L.append("")
    for sec in sections:
        kinds = [k for k, v in KINDS.items() if v[0] == sec and by_kind[k]]
        if not kinds:
            continue
        L.append(f"## {sec}\n")
        for k in kinds:
            _, meaning, close = KINDS[k]
            L.append(f'<a id="{k}"></a>\n### {meaning}\n')
            L.append(f"`{k}` · to close: {close}.\n")
            for it in by_kind[k]:
                where = ", ".join(f"`{w}`" for w in it.get("where", [])[:4])
                more = f" +{len(it['where']) - 4}" if len(it.get("where", [])) > 4 else ""
                detail = f" — {it['detail']}" if it.get("detail") else ""
                L.append(f"- **{it['subject']}**{detail}" + (f" · {where}{more}" if where else ""))
            L.append("")
    OUT.write_text("\n".join(L), encoding="utf-8")
    print(f"wrote {OUT} ({len(items)} gaps)")
    return 0


def counts(items) -> dict[str, int]:
    out = {k: 0 for k in KINDS}
    for it in items:
        out[it["kind"]] += 1
    return out


def load(directory: Path = DIR) -> list[dict]:
    return [it for p in sorted(directory.glob("*.json")) for it in json.loads(p.read_text(encoding="utf-8"))]


def write_baseline() -> int:
    """`pixi run arch-gaps-baseline`: record today's count per kind. Only ever to LOWER it after closing gaps."""
    BASELINE.write_text(json.dumps(counts(load()), indent=1) + "\n", encoding="utf-8")
    print(f"wrote {BASELINE.relative_to(REPO)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(write_baseline() if "--baseline" in sys.argv else compose())
