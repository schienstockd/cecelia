#!/usr/bin/env python3
"""CLAUDE.md eval — the function around a line, the bug sweep's excerpt unit.

Design: docs/todo/CLAUDE_MD_EVAL_SUPERVISOR_PLAN.md → Decision 19. A judge shown ±20 lines often
couldn't see the code a finding was about; the whole function it sits in usually can. This is a
pragmatic, per-language heuristic, not a parser:

- Python: `def` / `class` by indentation; the body ends before the next non-blank line indented no
  deeper than the header.
- Julia: `function` / `macro` ends at the first `end` at the header's indentation; a one-line
  `f(x) = …` is its own range.
- Brace languages (TS, JS, Vue, WGSL): a `function` / arrow `const` / method / `fn` header, then
  `{` `}` counted (strings and `//` comments stripped first).

Anything else (markdown, TOML, shell) has no function: the caller falls back to a line window.
A header found but never closed is ignored rather than guessed at.
"""
from __future__ import annotations

import re
import typing as _t

#: How far up from the line a header is looked for.
SCAN_UP = 400

_PY = re.compile(r"^(\s*)(?:async\s+)?(?:def|class)\s+(\w+)")
_JL = re.compile(r"^(\s*)(?:function|macro)\s+([\w.!]+)")
_JL_SHORT = re.compile(r"^(\s*)([\w.!]+)\([^()]*\)\s*(?:where\b[^=]*)?=(?![=>])")
_KEYWORDS = {"if", "for", "while", "switch", "catch", "return", "else", "do", "with", "function"}
_BRACE = (
    re.compile(r"^(\s*)(?:export\s+)?(?:default\s+)?(?:async\s+)?function\s*\*?\s*(\w+)"),
    re.compile(r"^(\s*)(?:export\s+)?(?:const|let|var)\s+(\w+)\s*(?::[^=]+)?=\s*(?:async\s+)?"
               r"(?:\([^)]*\)|\w+)\s*(?::[^=]+)?=>"),
    re.compile(r"^(\s*)(?:(?:public|private|protected|static|async|get|set)\s+)*(\w+)\s*\([^)]*\)\s*"
               r"(?::[^{]+)?\{\s*$"),
    re.compile(r"^(\s*)fn\s+(\w+)"),   # WGSL
)
_STRINGS = re.compile(r"'(?:\\.|[^'\\])*'|\"(?:\\.|[^\"\\])*\"|`(?:\\.|[^`\\])*`")


def _lang(path: str) -> str | None:
    ext = path.rsplit(".", 1)[-1].lower() if "." in path else ""
    if ext == "py":
        return "py"
    if ext == "jl":
        return "jl"
    if ext in ("ts", "tsx", "js", "mjs", "cjs", "vue", "wgsl"):
        return "brace"
    return None


def _indent(s: str) -> int:
    return len(s) - len(s.lstrip())


def _header(lang: str, line: str) -> str | None:
    """The name a function header on this line declares, or None."""
    if lang == "py":
        m = _PY.match(line)
    elif lang == "jl":
        m = _JL.match(line) or _JL_SHORT.match(line)
    else:
        m = next((m for rx in _BRACE if (m := rx.match(line))), None)
        if m and m.group(2) in _KEYWORDS:
            return None
    return m.group(2) if m else None


def _end(lang: str, lines: list[str], start: int) -> int | None:
    """1-based last line of the function whose header is at `start` (1-based), or None if unclosed."""
    head = lines[start - 1]
    if lang == "py":
        ind, last = _indent(head), start
        for n in range(start + 1, len(lines) + 1):
            s = lines[n - 1]
            if not s.strip():
                continue
            if _indent(s) <= ind:
                break
            last = n
        return last
    if lang == "jl":
        if _JL_SHORT.match(head) and not _JL.match(head):
            return start
        ind = _indent(head)
        for n in range(start + 1, len(lines) + 1):
            s = lines[n - 1]
            if _indent(s) == ind and re.match(r"\s*end\b", s):
                return n
        return None
    depth, opened = 0, False
    for n in range(start, len(lines) + 1):
        s = _STRINGS.sub("", lines[n - 1]).split("//", 1)[0]
        for ch in s:
            if ch == "{":
                depth, opened = depth + 1, True
            elif ch == "}":
                depth -= 1
                if opened and depth == 0:
                    return n
        if not opened and n - start > 3:   # a header whose body never opened: not a function
            return None
    return None


def enclosing(text: str, line: int, path: str) -> tuple[str, int, int] | None:
    """(name, start, end), 1-based inclusive, of the innermost function holding `line`; else None."""
    lang = _lang(path)
    if not lang or not line:
        return None
    lines = text.splitlines()
    if line > len(lines):
        return None
    for start in range(line, max(0, line - SCAN_UP), -1):
        name = _header(lang, lines[start - 1])
        if not name:
            continue
        end = _end(lang, lines, start)
        if end is not None and start <= line <= end:
            return name, start, end
    return None


def find(text: str, name: str, path: str) -> tuple[int, int] | None:
    """(start, end) of the first function named `name` in `text`; None when it isn't defined there."""
    lang = _lang(path)
    if not lang:
        return None
    lines = text.splitlines()
    for start, s in enumerate(lines, 1):
        if _header(lang, s) == name:
            end = _end(lang, lines, start)
            if end is not None:
                return start, end
    return None


def mentions(text: str, name: str) -> bool:
    """Whether `name` still appears as a whole word anywhere in `text` (a call, an import, a def)."""
    return re.search(rf"(?<![\w.]){re.escape(name)}(?![\w!])", text) is not None


def window(lines: _t.Sequence[str], lo: int, hi: int) -> str:
    """Numbered lines `lo`..`hi` (1-based, clamped)."""
    lo, hi = max(1, lo), min(hi, len(lines))
    return "\n".join(f"{n:>5}  {lines[n - 1]}" for n in range(lo, hi + 1))
