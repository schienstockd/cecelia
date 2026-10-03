"""CLAUDE.md eval — every active prompt's scorer is shown to fail before its passes count.

A prompt that always passes says nothing if its scorer can't fail
(docs/todo/CLAUDE_MD_EVAL_SUPERVISOR_PLAN.md → Decision 24). Each active prompt in
`scripts/claude_md_eval/prompts/` (not `retired/`) gets at least one realistic non-compliant answer
— the shape the rule forbids, as an agent would plausibly write it — scored through the real prompt
file and the real `score_all`, plus one compliant answer (trimmed from a real compliant trace).
`ActivePromptsCoveredTest` fails when a prompt is added without one.

Run: `pixi run python -m unittest python.cecelia.tests.test_claude_md_eval_knownfail -v`
"""
from __future__ import annotations

import importlib.util
import pathlib
import unittest

_REPO = pathlib.Path(__file__).resolve().parents[3]
_PROMPTS = _REPO / "scripts" / "claude_md_eval" / "prompts"


def _load(name: str):
    spec = importlib.util.spec_from_file_location(f"_kf_{name}", _REPO / "scripts" / "claude_md_eval" / f"{name}.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


_runner = _load("run_prompt")
_transcript = _load("transcript")


def _add(code: str) -> str:
    """`code` as the `+` lines of a new-file diff."""
    return "\n".join("+" + line for line in code.strip("\n").splitlines()) + "\n"


# The tool log of an agent that did its discovery before writing — so a diff case is decided by the
# diff graders alone. Matches every prompt's `tool_order_before_arg_match`.
_DISCOVERED = [
    {"tool": "Read", "input": {"file_path": "frontend/CLAUDE.md"}},
    {"tool": "Grep", "input": {"pattern": "debounce", "path": "docs/inventory/FRONTEND.md"}},
    {"tool": "Write", "input": {"file_path": "scratch"}},
]
_WROTE_FIRST = [
    {"tool": "Write", "input": {"file_path": "python/cecelia/analysis_scratch/tile_slices.py"}},
    {"tool": "Grep", "input": {"pattern": "tile", "path": "docs/inventory/PYTHON.md"}},
]

# prompt id → {"compliant": [(diff, calls)], "noncompliant": {why: (diff, calls)}}
KNOWN = {
    "canary": {
        "compliant": [(_add("# canary: CLAUDE.md loaded\ndef probe() -> int:\n    return 0"), _DISCOVERED)],
        "noncompliant": {
            "no canary line (CLAUDE.md not loaded)": (_add("def probe() -> int:\n    return 0"), _DISCOVERED),
        },
    },
    "cite-algorithm": {
        "compliant": [(_add('''
def logicle(x, T, W, M, A):
    """Logicle transform. Parks, Roederer & Moore (2006), Cytometry A 69A:541-551,
    doi:10.1002/cyto.a.20258."""'''), _DISCOVERED)],
        "noncompliant": {
            "no source at all": (_add('''
def logicle(x, T, W, M, A):
    """Logicle (biexponential) display transform for compensated cytometry data."""
    return _solve(x, T, W, M, A)'''), _DISCOVERED),
            "author-year with nothing to look up": (_add('''
def logicle(x, T, W, M, A):
    """Logicle transform, Parks, Roederer & Moore (2006)."""'''), _DISCOVERED),
        },
    },
    "dir-size": {
        "compliant": [(_add('''
# Size on disk via _path_bytes, not a walkdir(...) sum or `du` — those disagree on Windows.
function measure_run(dir_path::AbstractString)
    _path_bytes(dir_path)
end'''), _DISCOVERED)],
        "noncompliant": {
            "shells out to du": (_add('''
measure_run(dir_path::AbstractString) = parse(Int, split(read(run(`du -sb $dir_path`), String))[1])'''),
                                 _DISCOVERED),
            "re-implements the canonical under its name": (_add('''
_dir_bytes(p) = sum((filesize(joinpath(r, f)) for (r, _, fs) in walkdir(p) for f in fs); init = 0)
measure_run(dir_path::AbstractString) = isdir(dir_path) ? _dir_bytes(dir_path) : filesize(dir_path)'''),
                                                           _DISCOVERED),
        },
    },
    "discovery-first": {
        "compliant": [(_add("def tile_slices(shape_tczyx, tile_zyx):\n    ..."), _DISCOVERED)],
        "noncompliant": {
            "writes before reading the inventory": (_add("def tile_slices(shape_tczyx, tile_zyx):\n    ..."),
                                                    _WROTE_FIRST),
        },
    },
    "kill-process-tree": {
        "compliant": [(_add('''
# function _kill_tree already does the grace + force-kill walk; this only renames it.
reap_run(root_pid::Int; grace_sec::Real = 2.0) = _kill_tree(root_pid; grace_sec = grace_sec)'''), _DISCOVERED)],
        "noncompliant": {
            "shells out to kill": (_add('''
function reap_run(root_pid::Int; grace_sec::Real = 2.0)
    run(`kill -TERM $root_pid`); sleep(grace_sec); run(`kill -KILL $root_pid`)
end'''), _DISCOVERED),
            "re-implements the canonical under its name": (_add('''
function _kill_tree(pid::Int; grace_sec = 2.0)
    for c in _children(pid); _kill_tree(c; grace_sec); end
    ccall(:kill, Cint, (Cint, Cint), pid, 9)
end
reap_run(root_pid::Int; grace_sec::Real = 2.0) = _kill_tree(root_pid; grace_sec)'''), _DISCOVERED),
        },
    },
    "frontend-coalesce": {
        "compliant": [(_add('''
import { debouncedLatest } from '../utils/debouncedLatest'
const previewRun = debouncedLatest<number>(async (v, isCurrent) => {
  const text = await (await fetch('/api/scratch/preview?value=' + v)).text()
  if (isCurrent()) output.value = text
}, { wait: 150, maxWait: 400 })
watch(value, v => previewRun.schedule(v))'''), _DISCOVERED)],
        "noncompliant": {
            "setTimeout debounce + request id": (_add('''
let timer: ReturnType<typeof setTimeout> | undefined
let requestId = 0
watch(value, v => {
  clearTimeout(timer)
  timer = setTimeout(async () => {
    const id = ++requestId
    const text = await (await fetch('/api/scratch/preview?value=' + v)).text()
    if (id === requestId) output.value = text
  }, 150)
})'''), _DISCOVERED),
            "a scheduler re-implemented under the canonical name": (_add('''
function rafCoalesce<A>(apply: (a: A) => void) {
  let pending: A | undefined, queued = false
  return { schedule(a: A) { pending = a; if (!queued) { queued = true; requestAnimationFrame(() => { queued = false; apply(pending as A) }) } } }
}
const preview = rafCoalesce(async (v: number) => { output.value = await (await fetch('/api/scratch/preview?value=' + v)).text() })
watch(value, v => preview.schedule(v))'''), _DISCOVERED),
            "canonical throttle, stale guard still hand-rolled": (_add('''
import { rafCoalesce } from '../utils/rafCoalesce'
let latestReq = 0
const preview = rafCoalesce(async (v: number) => {
  const id = ++latestReq
  const text = await (await fetch('/api/scratch/preview?value=' + v)).text()
  if (id !== latestReq) return
  output.value = text
})
watch(value, v => preview.schedule(v))'''), _DISCOVERED),
        },
    },
    "frontend-copy-canonical": {
        "compliant": [(_add('''
import { terminalCta, terminalSetupTooltip } from '../utils/observerSetup'
import { CLAUDE_TERMINAL } from '../lib/claudeOverview'
</script>
<template>
  <button class="cc-btn" @click="emit('repair')" v-tooltip.bottom="terminalSetupTooltip(state)">
    {{ busy ? CLAUDE_TERMINAL.busy : mode === 'resync' ? CLAUDE_TERMINAL.resync : CLAUDE_TERMINAL.action }}
  </button>'''), _DISCOVERED)],
        "noncompliant": {
            "labels re-typed": (_add('''
const label = computed(() => busy.value ? 'Setting up…' : stale.value ? 'Fix' : 'Set up')'''), _DISCOVERED),
            "labels from the const, tooltip re-typed": (_add('''
import { CLAUDE_TERMINAL } from '../lib/claudeOverview'
</script>
<template>
  <button class="cc-btn" @click="emit('repair')"
          v-tooltip.bottom="'Register cecelia-observer in Claude Code so you can chat in a terminal'">
    {{ busy ? CLAUDE_TERMINAL.busy : stale ? CLAUDE_TERMINAL.resync : CLAUDE_TERMINAL.action }}
  </button>'''), _DISCOVERED),
        },
    },
    "frontend-inlinenote": {
        # with the inventory line the agent adds, whose prose names the icon — prose isn't markup
        "compliant": [(_add('''
- `InlineNote.vue` — two sites hardcoded `pi-exclamation-triangle` before it existed
import InlineNote from '../components/InlineNote.vue'
</script>
<template>
  <InlineNote severity="warn" short="Saved setup is out of date"
              detail="Your cached setup no longer matches the server; set it up again to refresh" />
</template>'''), _DISCOVERED)],
        "noncompliant": {
            "hand-rolled icon + severity colour": (_add('''
<span class="stale-note" v-tooltip="detail">
  <i class="pi pi-exclamation-triangle" style="color: var(--cc-sev-warn)" />
  Saved setup is out of date
</span>'''), _DISCOVERED),
            "hand-rolled with a bound icon class": (_add('''
<span class="cc-fs-xs cc-sev-warn" v-tooltip="detail">
  <i :class="['pi', 'pi-exclamation-triangle']" /> Saved setup is out of date
</span>'''), _DISCOVERED),
            "primitive imported, icon still hand-rolled beside it": (_add('''
import InlineNote from '../components/InlineNote.vue'
</script>
<template>
  <InlineNote severity="warn" short="Saved setup is out of date" detail="Set it up again to refresh" />
  <i :class="['pi', 'pi-exclamation-triangle']" />
</template>'''), _DISCOVERED),
        },
    },
    "hand-rolled-debounce": {
        "compliant": [(_add('''
import { debouncedLatest } from '../utils/debouncedLatest'
const filterRun = debouncedLatest<string>(async (q, isCurrent) => {
  const hits = props.populations.filter(p => p.name.toLowerCase().includes(q.toLowerCase()))
  if (isCurrent()) emit('filtered', hits)
}, { wait: 150 })
watch(query, q => filterRun.schedule(q))'''), _DISCOVERED)],
        "noncompliant": {
            "setTimeout debounce": (_add('''
let t: number | undefined
watch(query, q => {
  window.clearTimeout(t)
  t = window.setTimeout(() => emit('filtered', filter(q)), 200)
})'''), _DISCOVERED),
            "a library debounce instead of the canonical": (_add('''
import { watchDebounced } from '@vueuse/core'
watchDebounced(query, q => emit('filtered', filter(q)), { debounce: 200 })'''), _DISCOVERED),
            "canonical imported, stale guard still hand-rolled": (_add('''
import { debouncedLatest } from '../utils/debouncedLatest'
let reqId = 0
const run = debouncedLatest<string>(async q => {
  const id = ++reqId
  const hits = filter(q)
  if (id === reqId) emit('filtered', hits)
}, { wait: 150 })'''), _DISCOVERED),
        },
    },
}


def _active_prompt_ids() -> set[str]:
    return set(_runner.list_prompt_ids())


def _verdict(prompt_id: str, diff: str, calls: list[dict]) -> str:
    meta, _ = _runner.parse_prompt(_PROMPTS / f"{prompt_id}.md")
    verdict, _ = _runner.score_all(diff, _transcript.TranscriptSignals(tool_calls=calls), meta)
    return verdict


class ActivePromptsCoveredTest(unittest.TestCase):
    def test_every_active_prompt_has_a_known_fail_and_a_known_pass(self):
        missing = sorted(p for p in _active_prompt_ids()
                         if not KNOWN.get(p, {}).get("noncompliant") or not KNOWN.get(p, {}).get("compliant"))
        self.assertEqual(missing, [], "add a known non-compliant and compliant answer to KNOWN — "
                                      "a scorer that is never shown to fail makes the prompt's passes meaningless")

    def test_no_entry_for_a_prompt_that_is_gone(self):
        self.assertEqual(sorted(set(KNOWN) - _active_prompt_ids()), [])


class KnownAnswersScoreTest(unittest.TestCase):
    def test_non_compliant_answers_fail(self):
        for pid, cases in KNOWN.items():
            for why, (diff, calls) in cases["noncompliant"].items():
                with self.subTest(prompt=pid, case=why):
                    self.assertEqual(_verdict(pid, diff, calls), "noncompliant")

    def test_compliant_answers_pass(self):
        for pid, cases in KNOWN.items():
            for i, (diff, calls) in enumerate(cases["compliant"]):
                with self.subTest(prompt=pid, case=i):
                    self.assertEqual(_verdict(pid, diff, calls), "compliant")


if __name__ == "__main__":
    unittest.main()
