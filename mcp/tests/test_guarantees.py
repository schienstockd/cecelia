"""A promise the server makes to the assistant must point at a test that backs it.

WHY THIS EXISTS. `guidance.py` and the tool docstrings are prose in Python; the behaviour they
describe is Julia. Nothing tied the two, so a promise could be false for months — and one was. Both
the `add_analysis_board` docstring and BRIEFING_GUIDANCE said the server "rejects a plot or population
that doesn't exist rather than writing a board that renders blank", while `_expand_plot` cheerfully
accepted a `popType` that made every panel render empty. It took a user opening the board to find out.

The rule this file enforces: **if the prose states a guarantee, name the test that proves it.** That is
weaker than verifying the behaviour from here (which would mean booting the API and driving the tools —
a lot of machinery, and it would break the headless split the four suites depend on). It is not
nothing: it makes "ship a promise nobody tested" a failing test, which is the step that was missing.

Two rules keep the list honest, and they are as important as the list:

1. **State only guarantees the assistant ACTS on.** "Add-only, cannot rename or delete" changes what
   it tells the user (it must not offer to fix a board it cannot touch). "The server refuses to write a
   board that would render blank" changed nothing — it submits, and either gets a 422 or does not — so
   that sentence was deleted rather than tested. A guarantee that only reassures is a liability with no
   upside: it can be wrong, and being right buys nothing.
2. **The anchor must be an ASSERTION, not a mention.** Point at the string a test actually checks.

Cross-language by necessity, in the same spirit as `app/test/suite.jl` reading `server.py` — this
reads back the other way. Stdlib only; no server needed.
"""
from __future__ import annotations

import pathlib
import unittest

_REPO = pathlib.Path(__file__).resolve().parents[2]


def _read(rel: str) -> str:
    return (_REPO / rel).read_text(encoding="utf-8")


def _flat(s: str) -> str:
    """One line, single spaces — so a phrase can straddle a hard wrap.

    The prose lives in `\\`-continued Python string literals, so a sentence is broken by a backslash
    AND a newline; dropping the continuation first is what makes "tagged [Claude] server-side"
    findable when the source has it as "tagged \\<newline>[Claude] server-side".
    """
    return " ".join(s.replace("\\\n", "").split())


# claim → (prose file that states it, phrase IN that prose, test file, phrase the TEST asserts)
#
# Adding a guarantee to the prose means adding a row here, which means having a test to point at. If
# you cannot name one, the honest options are to write it or to not make the promise.
GUARANTEES: dict[str, tuple[str, str, str, str]] = {
    # It must not offer to fix, rename or tidy a board — it can only add one beside the user's.
    "boards are add-only": (
        "mcp/cecelia_mcp/server.py", "cannot modify, rename, reorder or delete any board",
        "app/test/suite.jl", "append_board: ADD-ONLY",
    ),
    # It must not claim a name is taken-and-replaced, nor retry the same name.
    "a duplicate board name is refused": (
        "mcp/cecelia_mcp/server.py", "409 if the name is taken",
        "app/test/suite.jl", "duplicate name",
    ),
    # It must not set popType. This is the one that was false: the promise existed, the check did not.
    "popType must reach the named populations": (
        "mcp/cecelia_mcp/server.py", "popType` is NOT a field here",
        "app/test/suite.jl", "cannot reach",
    ),
    # It must write a plain name — the repair is real, so "&" is safe to type.
    "a board name is stored as it renders": (
        "mcp/cecelia_mcp/server.py", "never \"&amp;\"",
        "app/test/suite.jl", "board_display_name",
    ),
    # It must hand a chain over to be READ, never say it started one. Guaranteed by the ALLOWED_ROUTES
    # allow-list (there is no run route to call), which test_client pins against what the tools call.
    "a chain is inert until the user runs it": (
        "mcp/cecelia_mcp/guidance.py", "sits inert",
        "mcp/tests/test_client.py", "ALLOWED_ROUTES",
    ),
    # It must fix and retry rather than reporting a chain as impossible.
    "a malformed chain is refused before it is written": (
        "mcp/cecelia_mcp/server.py", "unknown fn, dangling edge, cycle",
        "api/test/runtests.jl", "api_chains_create",
    ),
    # It must take the attribute name from get_image_attributes rather than guessing one.
    "an unknown compare_by attribute is refused": (
        "mcp/cecelia_mcp/server.py", "the server rejects one the project does not have",
        "app/test/suite.jl", "compare_by = \"Genotype\"",
    ),
    # It must call set_labarchives_context first, or append under `claude` — not forge ELN provenance.
    "an ELN-tagged lab-log line needs a linked notebook": (
        "mcp/cecelia_mcp/server.py", "REJECTS it (409) on a project with no notebook linked",
        "api/test/runtests.jl", "\"author\"=>\"LabArchives\"",
    ),
    # It must make a NEW version rather than a "-v2" copy — licensed by the snapshot.
    "revise snapshots before overwriting": (
        "mcp/cecelia_mcp/guidance.py", "snapshots first",
        "api/test/runtests.jl", "SNAPSHOTS the current notebook",
    ),
    # It must never write the tag itself (it would be forging provenance).
    "lab-log entries are tagged server-side": (
        "mcp/cecelia_mcp/guidance.py", "tagged [Claude] server-side",
        "api/test/runtests.jl", "[Claude]",
    ),
    # The whole designs-but-never-starts split, which it is told not to apologise for.
    "nothing here can start work": (
        "mcp/cecelia_mcp/guidance.py", "nothing here can START it",
        "mcp/tests/test_server.py", "test_no_tool_can_start_work",
    ),
}


class GuaranteeTest(unittest.TestCase):
    def test_every_guarantee_is_stated_in_the_prose(self):
        for claim, (prose_file, phrase, _, _) in GUARANTEES.items():
            with self.subTest(claim=claim):
                self.assertIn(_flat(phrase), _flat(_read(prose_file)),
                              f"{claim}: {prose_file} no longer says it — remove the row or restore "
                              f"the sentence, but do not leave the two disagreeing")

    def test_every_guarantee_names_a_test_that_backs_it(self):
        for claim, (_, _, test_file, anchor) in GUARANTEES.items():
            with self.subTest(claim=claim):
                self.assertIn(_flat(anchor), _flat(_read(test_file)),
                              f"{claim}: {test_file} no longer contains {anchor!r}. The promise is "
                              f"now unbacked — point at the test that replaced it, or drop the promise")

    def test_the_prose_makes_no_UNLISTED_server_guarantee(self):
        """The backstop: a new "the server rejects/refuses…" sentence must join the list above.

        Deliberately narrow. Grepping for every "never"/"cannot" drowns in prose that merely describes
        data ("quantiles, never raw rows"), so this looks only for the shape that burned us — a claim
        about what the SERVER does with a bad request, which is exactly the class nobody can verify by
        reading the Python.
        """
        import re
        # Match the whole SENTENCE, and ask whether some row's phrase lives in it — the phrase is drawn
        # from that sentence, so this is exact. (Comparing loose word fragments instead was wrong: it
        # failed a claim that WAS listed, because the row quoted a different clause of it.)
        pattern = re.compile(r"[^.\n]*\bserver\b[^.]{0,80}?\b(rejects?|refuses?|validates?|guarantees?)"
                             r"\b[^.]*\.", re.IGNORECASE)
        phrases = [p for _, p, _, _ in GUARANTEES.values()]
        for f in ("mcp/cecelia_mcp/server.py", "mcp/cecelia_mcp/guidance.py"):
            text = _flat(_read(f))                 # unwrap: a sentence straddles the hard wrap
            for m in pattern.finditer(text):
                claim = m.group(0).strip()
                with self.subTest(file=f, claim=claim[:60]):
                    self.assertTrue(
                        any(_flat(p) in claim for p in phrases),
                        f"{f} makes a server guarantee not in GUARANTEES: {claim!r}. Either add a row "
                        f"naming the test that proves it, or say what to DO instead of what the server "
                        f"promises (e.g. \"a 422 names what was available — read it and resubmit\")")


if __name__ == "__main__":
    unittest.main()
