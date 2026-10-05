"""Repair model-authored text before an MCP write tool stores it.

A model filling a JSON tool argument sometimes escapes twice: it means a newline and sends the two
characters backslash + ``n``. The value arrives as one line full of literal ``\\n`` and renders as one
line in the app (a project profile was stored that way). The GUI never produces this — a textarea
sends real newlines — so the repair lives here, where model-authored text enters, and not in the API
routes, where a person who typed a literal backslash-n meant it.

The rule is narrow so that real backslash-n content survives:

- text that already has a real line break is returned untouched (it was escaped correctly; any
  ``\\n`` left in it is content — a regex, a code example);
- the literal ``\\n`` (and ``\\r\\n``) sequences are counted OUTSIDE backtick code spans, and only
  ones not preceded by another backslash (``\\\\n`` is an escaped backslash, then ``n``);
- at least two are needed — one stray ``\\n`` in prose is as likely a mention as a mistake;
- any other backslash-letter/digit sequence outside code (``\\d``, ``\\s``, ``D:\\data``) means the
  text is not a double-escaped string but carries real backslashes (a regex, a Windows path), so it
  is left alone.

Only newlines are decoded. Code spans are never touched. Notebook cells (Julia source, where
``"a\\nb"`` is legitimate) are deliberately not passed through this.
"""
from __future__ import annotations

import re

# inline code spans: ``…`` or `…` (no real newline can be present when we get this far, so no fences)
_CODE_SPAN = re.compile(r"(``.*?``|`[^`]*`)")
# a literal \n or \r\n not preceded by a backslash
_ESC_NL = re.compile(r"(?<!\\)(?:\\r)?\\n")
# a backslash + letter/digit that is not a JSON escape we would expect in a double-escaped string
_FOREIGN_ESC = re.compile(r"(?<!\\)\\(?![nrtu\\])[A-Za-z0-9]")


def repair_escaped_newlines(text):
    """Return ``(text, repaired)`` — ``text`` with literal ``\\n`` sequences turned into newlines when
    the whole value was evidently escaped twice (see the module docstring), else unchanged."""
    if not isinstance(text, str) or "\n" in text or "\r" in text or "\\" not in text:
        return text, False
    parts = _CODE_SPAN.split(text)      # odd indices are code spans
    prose = parts[0::2]
    if sum(len(_ESC_NL.findall(p)) for p in prose) < 2:
        return text, False
    if any(_FOREIGN_ESC.search(p) for p in prose):
        return text, False
    parts[0::2] = [_ESC_NL.sub("\n", p) for p in prose]
    return "".join(parts), True


def repair_lines(lines):
    """For list-of-lines writes (lab log, LabArchives sections): repair each line, then split one that
    became multi-line into its own lines (the server would otherwise fold it back into one with
    spaces). Returns ``(lines, repaired)``. Non-list input passes through."""
    if not isinstance(lines, list):
        return lines, False
    out, any_fixed = [], False
    for line in lines:
        fixed, did = repair_escaped_newlines(line)
        if did:
            any_fixed = True
            out.extend(ln for ln in fixed.split("\n") if ln.strip())
        else:
            out.append(line)
    return out, any_fixed
