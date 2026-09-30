"""Inventory-coverage check — the mechanical third recital step.

`INVENTORY.md` → *Keeping this current*: a new shared util / canonical helper / reusable component /
API handler file gets a line in the matching `docs/inventory/*.md` in the same change. Nothing
checked that, and it's the drift the discovery step pays for: the next session greps inventory,
finds nothing, and builds a second copy. This check flags every file the staged diff ADDS under a
shared root whose name appears in no inventory doc — and every route the diff adds to the router
table (`api/src/server.jl`) that `docs/API.md` → *Route index* doesn't list.

**Mechanical, not a reviewer subagent** — advisory only; never blocks. "Significant" is a judgement
the check can't make, so a flagged file may rightly stay out: a comment `INVENTORY-EXEMPT: <reason>`
anywhere in the new file silences it, and the reason is on record in the file itself.
Replayed over the 150 merged PRs before it landed: fired on 19, flagging 30 files — 5 were added
to inventory by hand later (real misses, caught late), 24 still weren't, incl. 5 new API handlers.
Routes, same replay: 14 PRs added routes, 12 would have fired, 24 routes — 2 reached API.md
before the 2026-09-30 backfill of the route index.

Replaces the earlier citation-currency check (warned when an `Enforced by X` file changed but
the citing doc didn't): 0 real hits in 150 PRs, and the dangling-path half of its job is covered in
CI by `python/cecelia/tests/test_doc_pointer_convention.py`.
"""
from __future__ import annotations

import re
import time
import typing as _t
from pathlib import Path

from .git_context import repo_root as _repo_root
from .log import append_event

#: Shared roots → the inventory area file a new entry belongs in (the hint in the warning). First
#: matching prefix wins, so leaf dirs listed in `_LEAF_PREFIXES` are carved out before this runs.
_SHARED_ROOTS: tuple[tuple[str, str], ...] = (
    ("frontend/src/utils/", "docs/inventory/FRONTEND.md"),
    ("frontend/src/lib/", "docs/inventory/FRONTEND.md"),
    ("frontend/src/composables/", "docs/inventory/FRONTEND.md"),
    ("frontend/src/stores/", "docs/inventory/FRONTEND.md"),
    ("frontend/src/components/", "docs/inventory/FRONTEND.md"),
    ("app/src/", "docs/inventory/JULIA_APP.md"),
    ("api/src/", "docs/inventory/JULIA_API.md"),
    ("python/cecelia/", "docs/inventory/PYTHON.md"),
    ("mcp/", "docs/inventory/MCP.md"),
)

#: Leaf files `INVENTORY.md` says are omitted on purpose ("leaf task files and one-off components").
_LEAF_PREFIXES = ("app/src/tasks/", "python/cecelia/tasks/")

_CODE_EXT = (".py", ".jl", ".ts", ".vue", ".js")

#: Tests, type stubs, package markers — never inventory material.
_NOT_SHARED = re.compile(r"(\.test\.|\.spec\.|\.d\.ts$|/tests?/|/test_[^/]*$|/__init__\.py$)")

#: Where a mention counts. `docs/ui/PRIMITIVES.md` is the inventory for UI primitives.
_INVENTORY_DOCS_GLOB = "docs/inventory/*.md"
_EXTRA_INVENTORY_DOCS = ("INVENTORY.md", "docs/ui/PRIMITIVES.md")

#: A file the diff adds: `diff --git a/X b/X` followed (before the next header) by `new file mode`.
_NEW_FILE = re.compile(r"^diff --git a/\S+ b/(\S+)\n(?:(?!diff --git ).*\n)*?new file mode", re.MULTILINE)

#: A route the diff ADDS to the router table: `+    "/api/x/y" => (req, body_bytes) -> …`.
_NEW_ROUTE = re.compile(r'^\+\s*"(/api/[^"\s]+)"\s*=>', re.MULTILINE)
_ROUTE_DOC = "docs/API.md"

#: Opt-out for a genuine one-off, same shape as `# COHORT-EXEMPT:` / `# DASK-OK:` elsewhere.
_EXEMPT_MARKER = re.compile(r"INVENTORY-EXEMPT:\s*\S")

_TITLE = "Inventory check"


def new_files_from_diff(diff: str) -> list[str]:
    """Repo-relative paths the diff creates, in diff order."""
    return _NEW_FILE.findall(diff)


def exempt_files_from_diff(diff: str) -> set[str]:
    """New files whose added lines carry an `INVENTORY-EXEMPT: <reason>` marker."""
    exempt: set[str] = set()
    for block in re.split(r"^(?=diff --git )", diff, flags=re.MULTILINE):
        m = _NEW_FILE.match(block)
        if m and any(_EXEMPT_MARKER.search(ln) for ln in block.splitlines() if ln.startswith("+")):
            exempt.add(m.group(1))
    return exempt


def area_doc_for(path: str) -> str | None:
    """The inventory area file a new `path` belongs in, or None if it isn't a shared file."""
    if not path.endswith(_CODE_EXT) or _NOT_SHARED.search(path):
        return None
    if path.startswith(_LEAF_PREFIXES):
        return None
    for prefix, doc in _SHARED_ROOTS:
        if path.startswith(prefix):
            return doc
    return None


def read_inventory_text(repo_root: Path) -> str:
    """Concatenated text of every inventory doc, read from the working tree."""
    paths = sorted(repo_root.glob(_INVENTORY_DOCS_GLOB))
    paths += [repo_root / p for p in _EXTRA_INVENTORY_DOCS]
    parts = []
    for p in paths:
        try:
            parts.append(p.read_text(encoding="utf-8"))
        except (OSError, UnicodeDecodeError):
            continue
    return "\n".join(parts)


def is_named(path: str, inventory_text: str) -> bool:
    """True if the file's stem appears as a whole word in the inventory — `panelResize.ts`,
    `panelResize`, and `utils/panelResize.ts` all count. Stem, not basename, because Vue
    components are written as `ImageTable` and Julia files as `label_props` about as often as
    with the extension."""
    stem = path.rsplit("/", 1)[-1].rsplit(".", 1)[0]
    return re.search(rf"(?<![\w-]){re.escape(stem)}(?![\w-])", inventory_text) is not None


def new_routes_from_diff(diff: str) -> list[str]:
    """Routes the diff adds to the router table, deduped, in diff order. A route moved within the
    table shows as `-` + `+` and still counts — harmless, because a documented one isn't flagged."""
    return list(dict.fromkeys(_NEW_ROUTE.findall(diff)))


#: A backticked path using brace shorthand for siblings: `` `/api/sets/{create,rename,delete}` ``.
_BRACE_PATH = re.compile(r"`(/api/[^`{}]*)\{([^`{}]*,[^`{}]*)\}([^`{}]*)`")


def _expand_braces(text: str) -> str:
    """Append each brace-shorthand path's expansions as plain backticked paths, so one matcher
    handles both forms. One brace group per path — the only shape the docs use."""
    extra = [f"`{head}{alt.strip()}{tail}`"
             for head, alts, tail in _BRACE_PATH.findall(text) for alt in alts.split(",")]
    return text + "\n" + "\n".join(extra)


def is_route_documented(route: str, api_doc_text: str) -> bool:
    """True if `docs/API.md` names the route as a backticked path — `` `/api/x` ``, with a query
    `` `/api/x?projectUid` ``, or inside brace shorthand `` `/api/{x,y}` ``. `/api/x` does NOT count
    as documenting `/api/x/y`, or the reverse."""
    return re.search(rf"`{re.escape(route)}[`?]", _expand_braces(api_doc_text)) is not None


def find_undocumented_routes(diff: str, api_doc_text: str) -> tuple[list[str], list[str]]:
    """Return `(new_routes, [route, ...] missing from the API.md route index)`."""
    routes = new_routes_from_diff(diff)
    return routes, [r for r in routes if not is_route_documented(r, api_doc_text)]


def find_uninventoried(diff: str, inventory_text: str) -> tuple[list[str], list[tuple[str, str]]]:
    """Return `(new_shared_files, [(file, area_doc), ...] not named in the inventory)`."""
    exempt = exempt_files_from_diff(diff)
    shared = [(f, doc) for f in new_files_from_diff(diff)
              if f not in exempt and (doc := area_doc_for(f))]
    missing = [(f, doc) for f, doc in shared if not is_named(f, inventory_text)]
    return [f for f, _ in shared], missing


def format_section(
    missing: _t.Sequence[tuple[str, str]],
    *,
    shared_count: int,
    missing_routes: _t.Sequence[str] = (),
    route_count: int = 0,
) -> str:
    """Standard reservations section — evidence bullets + `_<Title>: <verdict>_` tail."""
    if shared_count == 0 and route_count == 0:
        return f"_{_TITLE}: skipped — no new shared files or routes_"
    if not missing and not missing_routes:
        return f"_{_TITLE}: run — every new shared file and route is documented_"
    lines = [f"_{_TITLE} (evidence):_", ""]
    for f, doc in missing:
        lines.append(
            f"- `{f}` — new shared file, not named in any inventory doc. If other code should "
            f"reuse it, add a line to `{doc}` in this change; if it's a one-off, say so with an "
            f"`INVENTORY-EXEMPT: <reason>` comment in the file."
        )
    for r in missing_routes:
        lines.append(
            f"- `{r}` — new route, not in the `{_ROUTE_DOC}` → *Route index*. Add a row in this change."
        )
    lines += ["", f"_{_TITLE}: run_"]
    return "\n".join(lines)


def run_inventory_check(
    diff: str,
    *,
    repo_root: Path | None = None,
    inventory_text: str | None = None,
    api_doc_text: str | None = None,
    pr: str | None = None,
    commit: str | None = None,
    branch: str | None = None,
) -> str:
    """Find uninventoried new shared files, emit one `inventory_coverage_run` event, return the
    markdown section. `inventory_text` / `api_doc_text` are test seams; default to the working-tree docs."""
    start = time.monotonic()
    root = repo_root or _repo_root()
    if inventory_text is None:
        inventory_text = read_inventory_text(root)
    if api_doc_text is None:
        try:
            api_doc_text = (root / _ROUTE_DOC).read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError):
            api_doc_text = ""
    shared, missing = find_uninventoried(diff, inventory_text)
    routes, missing_routes = find_undocumented_routes(diff, api_doc_text)
    append_event(
        "inventory_coverage_run",
        {
            "new_shared_files": len(shared),
            "new_routes": len(routes),
            "warnings_emitted": len(missing) + len(missing_routes),
            # Which files / routes, not just how many — so the log can say later whether the
            # warnings were acted on (did the file land in inventory in a follow-up?).
            "files": [f for f, _ in missing],
            "routes": missing_routes,
            "duration_s": round(time.monotonic() - start, 3),
        },
        pr=pr,
        commit=commit,
        branch=branch,
    )
    return format_section(missing, shared_count=len(shared),
                          missing_routes=missing_routes, route_count=len(routes))
