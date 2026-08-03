"""Params helpers shared by every task runner — `cecelia.utils.script_utils`.

`channel_indices` / `channel_index` exist because of a real failure: a run died with
``ValueError: invalid literal for int() with base 10: 'CH3'`` from deep inside a streaming loop, after
channel NAMES reached Python instead of 0-based indices. The cause was not a bad parameter — the
worktree's files carried a param rename while its running backend still had the previous Julia
translator compiled (`app/src` is Revise-tracked and a merge under a live server does not always reload
it). Nothing in the message said so.

Every task that takes channels goes through the same name→index translation on the Julia side, so the
coercion + diagnosis is one helper here rather than a bare `int(c)` at each site. The detector at the
bottom is what stops a new site reintroducing one.
"""
import re
import unittest
from pathlib import Path

import numpy as np

import cecelia.utils.script_utils as script_utils


class ChannelIndicesTest(unittest.TestCase):
    def test_indices_pass_through(self):
        self.assertEqual(script_utils.channel_indices([0, 2, 3]), [0, 2, 3])
        self.assertEqual(script_utils.channel_indices([np.int64(1)]), [1])

    def test_none_and_empty_are_empty(self):
        self.assertEqual(script_utils.channel_indices(None), [])
        self.assertEqual(script_utils.channel_indices([]), [])

    def test_a_digit_string_still_converts(self):
        # no stricter than the bare `int(c)` this replaced — a REPL/chain caller may hand back strings
        self.assertEqual(script_utils.channel_indices(['0', '2']), [0, 2])

    def test_a_channel_name_names_itself_the_translator_and_the_fix(self):
        with self.assertRaises(ValueError) as ctx:
            script_utils.channel_indices(
                ['CH2', 'CH4'], 'competingChannels for channel 2',
                'af_combinations_for_python (af_correct.jl)')
        msg = str(ctx.exception)
        self.assertIn("'CH2'", msg)                        # the offending value
        self.assertIn('channel 2', msg)                    # which combination
        self.assertIn('af_combinations_for_python', msg)   # what should have run
        self.assertIn('restart', msg)                      # what to do about it

    def test_the_translator_is_named_per_caller(self):
        for translator in ('cellpose_models_for_python (cellpose.jl)', 'drift_correct.jl',
                           'branching.jl'):
            with self.assertRaises(ValueError) as ctx:
                script_utils.channel_indices(['CH1'], 'x', translator)
            self.assertIn(translator, str(ctx.exception))

    def test_without_a_translator_it_still_says_something_useful(self):
        with self.assertRaises(ValueError) as ctx:
            script_utils.channel_indices(['CH1'])
        self.assertIn('Julia param translator', str(ctx.exception))

    def test_a_bool_is_refused_rather_than_indexing_channel_0_or_1(self):
        # bool is an int subclass, so `int(True)` would silently mean channel 1
        for v in (True, False):
            with self.assertRaises(ValueError) as ctx:
                script_utils.channel_indices([v])
            self.assertIn('bool', str(ctx.exception))


class ChannelIndexTest(unittest.TestCase):
    """The singular form, for a `channelSelection` with `multiple=false` — which still arrives as a
    one-element list, because that is how the widget stores it."""

    def test_a_one_element_list_and_a_bare_scalar_agree(self):
        self.assertEqual(script_utils.channel_index([2]), 2)
        self.assertEqual(script_utils.channel_index(2), 2)

    def test_absent_or_empty_takes_the_default(self):
        self.assertEqual(script_utils.channel_index(None), 0)
        self.assertEqual(script_utils.channel_index([]), 0)
        self.assertEqual(script_utils.channel_index(None, default=3), 3)

    def test_a_name_raises_the_same_diagnosis(self):
        with self.assertRaises(ValueError) as ctx:
            script_utils.channel_index(['CH3'], 'driftChannel', 'drift_correct.jl')
        self.assertIn('drift_correct.jl', str(ctx.exception))


class NoBareChannelCoercionTest(unittest.TestCase):
    """Detector: a channel param must not be coerced with a bare `int()` again.

    The bug this file documents was invisible precisely because each site did its own `int(c)`. Four
    modules had one. A fifth would fail the same way, with the same unhelpful message, and nothing
    would notice — so the convention is enforced rather than described.
    """

    #: `int(...)` at the PARAMS BOUNDARY — where a value that came from Julia is first turned into an
    #: index. That is the only place a channel NAME can appear.
    #:
    #: Deliberately NOT every `int(ch)`: re-coercing a value already validated (a dict key, a log
    #: format, a cache key) is harmless noise, and flagging it would train people to silence the
    #: detector. The first draft of this pattern was that broad — it produced 5 false positives and 1
    #: real one, which is the wrong ratio for a rule meant to be obeyed.
    #: The camelCase `...Channels` is the tell: params KEYS are camelCase (`cellChannels`,
    #: `modelChannels`, `competingChannels`) while an already-validated local is plain `channels`. That
    #: one distinction separates the boundary from the interior without needing to parse the expression.
    _BARE = re.compile(
        r"int\(\s*[\w.]*params(?:\[|\.get\()\s*['\"]\w*[Cc]hannel"      # int(params['driftChannel'])
        r"|int\(\s*\w+\s*\)\s+for\s+\w+\s+in\s+[^\n]*[a-z]Channels?\b", # [int(c) for c in …someChannels]
        re.X)

    #: Files allowed to convert: the helper itself, and this test.
    _ALLOWED = {'script_utils.py', 'test_script_utils.py'}

    def _sources(self):
        root = Path(__file__).resolve().parents[3]
        for base in ('python/cecelia/utils', 'python/cecelia/writers', 'preview',
                     'app/src/tasks'):
            d = root / base
            if d.is_dir():
                yield from d.rglob('*.py')

    def test_no_module_coerces_a_channel_param_with_a_bare_int(self):
        offenders = []
        for f in self._sources():
            if f.name in self._ALLOWED:
                continue
            for i, line in enumerate(f.read_text(encoding='utf-8').splitlines(), 1):
                if line.lstrip().startswith('#'):
                    continue
                if self._BARE.search(line):
                    offenders.append(f'{f.name}:{i}: {line.strip()}')
        self.assertEqual(offenders, [], 'use script_utils.channel_indices/channel_index instead:\n'
                                        + '\n'.join(offenders))

    def test_the_detector_matches_the_five_real_shapes(self):
        # a detector that matches nothing passes forever — pin it against the forms actually found
        for bad in ("    drift_channel = int(params['driftChannel'])",
                    "    a = [int(c) for c in model_params.get('cellChannels', [])]",
                    "    b = [int(c) for c in (list(model_params.get('cellChannels', [])))]",
                    "    c = [int(ch) for ch in x['modelChannels']]",
                    "    d = [int(d) for d in (combos[ch].get('competingChannels') or [])]"):
            self.assertTrue(self._BARE.search(bad), f'detector missed: {bad}')

    def test_the_detector_leaves_already_validated_values_alone(self):
        # re-coercing a validated value is noise, not risk — flagging it would train people to silence
        # the detector, which is worse than not having one
        for ok in ("    n = int(total)",
                   "    z = int(round(scale))",
                   "    idx = script_utils.channel_indices(v, 'cellChannels')",
                   "    return {int(ch): _af_slab(data, du, int(ch), t) for ch in channels}",
                   "    channels = [int(ch) for ch in slabs]",
                   "    key = (im_path, int(channel_idx), method)",
                   "    log(f'ch{channel_idx}: {stats.backgrounds.get(int(channel_idx)):.0f}')"):
            self.assertIsNone(self._BARE.search(ok), f'detector false-positived: {ok}')


if __name__ == '__main__':
    unittest.main()


class ContractVersionTest(unittest.TestCase):
    """The params-contract guard: a runner must refuse a params file from an older backend.

    A runner is spawned FRESH from disk every run while the Julia process building its params can be
    stale (`app/src` is Revise-tracked and does not always reload after a branch switch or a merge under
    a live server). That asymmetry is invisible without a handshake — it surfaced once as
    `invalid literal for int() with base 10: 'CH3'`, which named neither the cause nor the fix.
    """

    def test_a_matching_version_passes(self):
        v = script_utils.CONTRACT_VERSION
        self.assertEqual(script_utils.check_contract_version({'CECELIA_PY_CONTRACT': str(v)}), v)

    def test_an_absent_variable_is_allowed_through(self):
        # not launched by run_py: a developer replaying a saved params file by hand, or an external
        # caller. Deliberately NOT a failure — the guard must not make manual replay impossible.
        self.assertIsNone(script_utils.check_contract_version({}))
        self.assertIsNone(script_utils.check_contract_version({'CECELIA_PY_CONTRACT': ''}))

    def test_an_unparseable_value_never_fails_a_run(self):
        # the guard is a safety net, not a gate: it must not become the thing that breaks a working run
        self.assertIsNone(script_utils.check_contract_version({'CECELIA_PY_CONTRACT': 'v2'}))

    def test_a_mismatch_exits_and_names_the_fix(self):
        older = str(script_utils.CONTRACT_VERSION - 1)
        with self.assertRaises(SystemExit) as ctx:
            script_utils.check_contract_version({'CECELIA_PY_CONTRACT': older})
        msg = str(ctx.exception)
        self.assertIn(older, msg)                                # what the backend sent
        self.assertIn(str(script_utils.CONTRACT_VERSION), msg)   # what this runner speaks
        self.assertIn('Revise', msg)                             # why the two disagree
        self.assertIn('Restart the backend', msg)                # what to do

    def test_a_newer_backend_is_also_refused(self):
        # symmetric: Python behind Julia is just as broken as Julia behind Python
        with self.assertRaises(SystemExit):
            script_utils.check_contract_version(
                {'CECELIA_PY_CONTRACT': str(script_utils.CONTRACT_VERSION + 1)})

    def test_script_params_runs_the_check(self):
        # the guard sits at the one function every runner already calls, so a NEW runner is covered by
        # writing nothing at all — that is the whole point of putting it here
        import inspect
        self.assertIn('check_contract_version', inspect.getsource(script_utils.script_params))
