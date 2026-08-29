"""Does a RUN's tile seam cross the previewed region?

The preview hands the whole visible region to `predict_slice` as ONE tile. A run tiles at `blockSize`
and re-stitches labels split across each seam, so where a seam crosses the region the run's mask is two
inferences plus an IoU re-join and the preview's is one — counts and boundaries near it differ. The
preview says so; these pin WHEN it says so.

The subtlety worth testing: the answer is POSITIONAL. "Is the region bigger than blockSize" is the
wrong question, because the run's tile grid is anchored at the image ORIGIN
(`_create_xy_tiles`: y = 0, block_size, 2*block_size, …) and only the *write* bounds land on it —
reads are padded by `overlap`.

Run with `pixi run test-py`.
"""
import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..', 'preview'))
from preview_worker import (_run_tile_seams, _base_groups, _merge_pass,   # noqa: E402
                            _post_process_merged)

FULL = {'Y': 2048, 'X': 2048}


def seams(y=(0, 100), x=(0, 100), block=512, full=FULL):
    return _run_tile_seams({'Y': list(y), 'X': list(x)}, full, block)


class RunTileSeamsTest(unittest.TestCase):
    def test_a_region_inside_one_tile_has_no_seam(self):
        self.assertEqual(seams(y=(0, 300), x=(0, 300)), {})
        self.assertEqual(seams(y=(520, 1000), x=(520, 1000)), {})   # wholly inside tile (1,1)

    def test_a_big_region_that_still_fits_one_tile_has_no_seam(self):
        # THE case a size-only test gets wrong: 600 px > blockSize, but with block 1024 it is one tile
        self.assertEqual(seams(y=(0, 600), x=(0, 600), block=1024), {})

    def test_a_small_region_straddling_a_boundary_has_a_seam(self):
        # ...and the mirror image: 300 px < blockSize, but it crosses y=512
        self.assertEqual(seams(y=(400, 700), x=(0, 100)), {'Y': 1})

    def test_counts_a_seam_per_axis(self):
        self.assertEqual(seams(y=(400, 700), x=(400, 700)), {'Y': 1, 'X': 1})

    def test_counts_several_seams_on_one_axis(self):
        # 0..1600 crosses 512, 1024 and 1536
        self.assertEqual(seams(y=(0, 1600), x=(0, 10))['Y'], 3)

    def test_a_boundary_exactly_at_the_region_edge_is_not_a_seam(self):
        # the region ENDS where the tile ends: the run writes it as one whole tile too, so no re-join
        # inside what the user is looking at. Strict inequality, deliberately.
        self.assertEqual(seams(y=(0, 512), x=(0, 512)), {})
        self.assertEqual(seams(y=(512, 1024), x=(512, 1024)), {})

    def test_the_live_case_that_prompted_this(self):
        # r0hufV, 541x576 at the image origin, default blockSize 512 — the run tiles 2x2 here, so the
        # 12 cells the preview reported are close to but not exactly what a run produces
        self.assertEqual(_run_tile_seams({'Y': [0, 541], 'X': [0, 576]},
                                         {'Y': 541, 'X': 576}, 512), {'Y': 1, 'X': 1})

    def test_an_image_smaller_than_one_tile_never_seams(self):
        self.assertEqual(_run_tile_seams({'Y': [0, 300], 'X': [0, 300]},
                                         {'Y': 300, 'X': 300}, 512), {})

    def test_a_missing_or_absurd_block_size_reports_nothing(self):
        # never invent a warning from a bad param — an absent blockSize must not read as "tiled"
        self.assertEqual(seams(y=(0, 2000), x=(0, 2000), block=0), {})
        self.assertEqual(seams(y=(0, 2000), x=(0, 2000), block=-1), {})

    def test_axes_absent_from_the_region_are_skipped(self):
        self.assertEqual(_run_tile_seams({'Y': [400, 700]}, FULL, 512), {'Y': 1})
        self.assertEqual(_run_tile_seams({}, FULL, 512), {})


if __name__ == '__main__':
    unittest.main()


class PreviewCallsPredictSliceCorrectlyTest(unittest.TestCase):
    """The preview's call to `predict_slice` matches the method's own signature.

    This is a bug that reached Dominik: `predict_slice` used to take `context=` and `context_index=`,
    grew to six such kwargs, and had them collected into ONE `TemporalWindow`. The preview worker was
    not updated, so every coastal preview raised

        TypeError: CoastalUtils.predict_slice() got an unexpected keyword argument 'context'

    Nothing caught it because the preview path needs torch, coastal and a real movie to execute, so no
    test called it — while the thing that broke was not the computation at all, it was the argument
    list. That IS checkable without any of the above: bind the call the preview makes against the
    signature the method declares.
    """

    def test_the_parameter_list_is_the_one_the_preview_was_written_against(self):
        import inspect
        from cecelia.utils.coastal_utils import CoastalUtils
        params = list(inspect.signature(CoastalUtils.predict_slice).parameters)
        # NAMES, not just arity. The preview passes the window POSITIONALLY, so a rename back to
        # `context=` would still bind — and then fail deep inside on an object of the wrong shape.
        # Asserting the names is what makes the drift visible at the seam instead of at runtime.
        self.assertEqual(['self', 'tile', 'model_params', 'norm_params', 'window'], params)

    def test_the_preview_call_binds(self):
        import inspect
        from cecelia.utils.coastal_utils import CoastalUtils
        sig = inspect.signature(CoastalUtils.predict_slice)
        # exactly what `_preview_coastal` passes: three positionals + the window, no keywords
        sig.bind(object(), object(), {'model': 'm.pt'}, None, object())

    def test_the_kwargs_that_broke_it_are_rejected(self):
        # fails loudly if `context=` is ever reintroduced as an alias, which would let the two call
        # shapes drift apart again
        import inspect
        from cecelia.utils.coastal_utils import CoastalUtils
        sig = inspect.signature(CoastalUtils.predict_slice)
        with self.assertRaises(TypeError):
            sig.bind(object(), object(), {}, None, context=object(), context_index=0)

    def test_the_window_the_preview_builds_has_every_field_the_run_sets(self):
        """A window missing `start`/`tile`/`id` reads as valid and breaks the per-window caches."""
        from cecelia.utils.segmentation_utils import TemporalWindow
        import numpy as np
        w = TemporalWindow(frames=np.zeros((3, 1, 4, 4), np.float32), index=1, start=7,
                           tile=(0, 4, 0, 4), channels=None, id=1)
        self.assertEqual(1, w.index)
        self.assertEqual(7, w.start)                 # the movie index of frames[0], not t_now
        self.assertEqual((0, 4, 0, 4), w.tile)
        self.assertIsNone(w.channels)                # the preview reads every channel
        self.assertGreater(w.id, 0)                  # 0 is the default and means "no window to key on"

    def test_the_preview_never_reuses_a_window_id(self):
        from preview_worker import _next_window_id
        ids = {_next_window_id() for _ in range(50)}
        self.assertEqual(50, len(ids))


class PreviewMultiPassMergeTest(unittest.TestCase):
    """A multi-group preview must show what the RUN would write, not one group of it.

    The bug: both preview backends looped the base model groups and REASSIGNED their output block
    each time, with no id offset and no fill-only merge. So a two-pass coastal config previewed as
    the last group alone — full-frame and unclipped, because nothing had claimed pixels ahead of it.
    On Dominik's `flowTom` config that is the small-seed pass on its own, which looks like a
    fragmented mess and is not what the run produces.

    Pinned against `SegmentationUtils`'s own primitives rather than a hand-written expectation, so
    the preview cannot drift from the run: if the run's merge rule changes, this fails.
    """

    class _Seg:
        """Enough of SegmentationUtils for the merge helpers — they are static."""
        from cecelia.utils.segmentation_utils import SegmentationUtils as _SU
        LABEL_DTYPE = _SU.LABEL_DTYPE
        model_order = staticmethod(_SU.model_order)
        offset_pass = staticmethod(_SU.offset_pass)
        fill_unlabelled = staticmethod(_SU.fill_unlabelled)

    @staticmethod
    def _count(masks):
        import numpy as np
        return int(np.unique(masks[masks > 0]).size)

    def test_the_second_pass_fills_only_what_the_first_left(self):
        import numpy as np
        seg = self._Seg()
        # pass 1 claims the top row; pass 2 would claim the whole array
        p1 = np.array([[1, 1], [0, 0]], dtype=np.uint32)
        p2 = np.array([[1, 1], [1, 1]], dtype=np.uint32)
        merged, passes = _merge_pass(seg, None, p1, '0', [], self._count)
        merged, passes = _merge_pass(seg, merged, p2, '1', passes, self._count)
        np.testing.assert_array_equal(merged, np.array([[1, 1], [2, 2]], dtype=np.uint32))
        self.assertEqual([p['group'] for p in passes], ['0', '1'])
        self.assertEqual([p['objects'] for p in passes], [1, 1])

    def test_a_pass_entirely_covered_reports_zero_objects(self):
        """The number being judged on a two-pass config: what pass 2 added. A merged total cannot
        say it is zero, and zero is the answer a config where both passes are near-identical gives."""
        import numpy as np
        seg = self._Seg()
        p1 = np.ones((2, 2), dtype=np.uint32)
        p2 = np.ones((2, 2), dtype=np.uint32)
        merged, passes = _merge_pass(seg, None, p1, '0', [], self._count)
        merged, passes = _merge_pass(seg, merged, p2, '1', passes, self._count)
        self.assertEqual(passes[0]['objects'], 1)
        self.assertEqual(passes[1]['objects'], 0, 'pass 2 contributed nothing and must say so')
        self.assertEqual(int(merged.max()), 1, 'pass 1 must keep every pixel it claimed')

    def test_ids_do_not_collide_between_passes(self):
        """Both passes label "1" locally. Without the offset the merge would fuse two unrelated
        objects into one label, and the pass ranges would be meaningless.

        `passes` has to be THREADED, not re-seeded per call: it carries the running id counter (see
        `PreviewPassCounterTest`). Handing a fresh list to the second call resets the counter and
        both passes come out as id 1 — which is what this asserted against before the counter moved
        off `merged.max()`.
        """
        import numpy as np
        seg = self._Seg()
        p1 = np.array([[1, 0]], dtype=np.uint32)
        p2 = np.array([[0, 1]], dtype=np.uint32)
        merged, passes = _merge_pass(seg, None, p1, '0', [], self._count)
        merged, passes = _merge_pass(seg, merged, p2, '1', passes, self._count)
        self.assertEqual(sorted(int(v) for v in np.unique(merged) if v), [1, 2])

    def test_base_groups_follow_the_run_order_and_drop_nuc(self):
        seg = self._Seg()
        models = {'10': {'matchAs': 'base'}, '2': {'matchAs': 'base'},
                  '1': {'matchAs': 'nuc'}, '0': {'matchAs': 'base'}}
        # numeric order (model_order), base only — '1' is a nuc pass the run matches INTO the base
        self.assertEqual(_base_groups(seg, models), ['0', '2', '10'])

    def test_a_preview_backend_never_orders_groups_with_plain_sorted(self):
        """The regression guard. `sorted()` over group keys is the lexicographic bug this replaced,
        and it reads as correct for the two-group case that is being tested above."""
        here = os.path.dirname(__file__)
        src = open(os.path.join(here, '..', '..', '..', 'preview', 'preview_worker.py'),
                   encoding='utf-8').read()
        self.assertNotIn('for key in sorted(models.keys())', src,
                         'a preview backend is ordering model groups lexicographically again')


class PreviewPassCounterTest(unittest.TestCase):
    """The id counter must not be read back off the merged array.

    A pass whose output is entirely covered by an earlier one leaves nothing in the array, so
    `merged.max()` falls back to the previous pass's top — and the pass after it then reuses ids the
    covered pass already owns. The ranges overlap, `label_pass_lookup`-style attribution becomes
    ambiguous, and every later count is wrong. Needs THREE passes to show up, which is why the
    two-pass tests above all passed with the bug present.
    """

    _Seg = PreviewMultiPassMergeTest._Seg
    _count = staticmethod(PreviewMultiPassMergeTest._count)

    def test_three_passes_with_a_fully_covered_middle_keep_disjoint_ranges(self):
        import numpy as np
        seg = self._Seg()
        merged, passes = None, []
        # pass 1 claims the left half; pass 2 is entirely inside it; pass 3 takes the right half
        for masks, key in ((np.array([[1, 1, 0, 0]], dtype=np.uint32), '0'),
                           (np.array([[1, 0, 0, 0]], dtype=np.uint32), '1'),
                           (np.array([[0, 0, 1, 1]], dtype=np.uint32), '2')):
            merged, passes = _merge_pass(seg, merged, masks, key, passes, self._count)

        spans = [(p['from'], p['to']) for p in passes]
        self.assertEqual(len(spans), 3, 'a covered pass still owns its id block')
        for (_, a_hi), (b_lo, _) in zip(spans, spans[1:]):
            self.assertLess(a_hi, b_lo, f'ranges must stay disjoint: {spans}')
        self.assertEqual([p['objects'] for p in passes], [1, 0, 1])
        # pass 3's label must not collide with pass 1's
        self.assertEqual(sorted(int(v) for v in np.unique(merged) if v), [1, 3])


class PreviewPostProcessRunsAfterTheMergeTest(unittest.TestCase):
    """The label modifications must be applied to the MERGED image, as the run applies them.

    The bug: both preview backends called `post_process` INSIDE the model-group loop, so a two-pass
    config was smoothed and size-filtered per pass, at each pass's FULL extent, and only then did
    `fill_unlabelled` clip the later pass. The run does the opposite — `predict_from_zarr` merges
    every group into the frame and calls `post_process` once afterwards — so the preview filtered
    objects the run never sees and kept slivers the run removes.

    Measured on zolIMa/fXgbTl with Dominik's own `flowTom` config: 52 objects in run order against
    53 in the old preview order, with non-identical foreground. It diverges at `minCellSize` 0 too,
    because `labelSmoothing` defaults to 0.5 and is just as order-sensitive — which is why this
    pins the ORDER rather than any one parameter.

    Pinned against the run's own composition rather than a hand-written expectation, so the preview
    cannot drift from it again.
    """

    class _Ctx:
        """`_post_process_merged` reads only these two off the context."""
        bounds = {'Y': (0, 8), 'X': (0, 8)}
        axis_len = {'Y': 8, 'X': 8}

    @staticmethod
    def _seg(**params):
        from cecelia.utils.segmentation_utils import SegmentationUtils
        seg = SegmentationUtils.__new__(SegmentationUtils)      # no zarr/taskDir needed
        for k, v in dict(label_erosion=0, label_expansion=0, label_smoothing=0.0,
                         min_cell_size=0, cell_size_max=0,
                         clear_depth=False, clear_touching_border=False).items():
            setattr(seg, k, params.get(k, v))
        return seg

    @staticmethod
    def _count(masks):
        import numpy as np
        return int(np.unique(masks[masks > 0]).size)

    def _passes(self):
        """Pass 0 claims a 6x4 block (24 px); pass 1 would claim 6x6 (36 px) overlapping it.

        After the merge pass 1 keeps a 6x2 sliver — 12 px. A `minCellSize` of 20 therefore separates
        the two orders exactly: the run judges the sliver (12 < 20, dropped) while the old per-pass
        order judged the 36 px block it came from (kept). Pass 0 is 24 px so it survives either way,
        which keeps this about the SLIVER rather than about pass 0 vanishing.
        """
        import numpy as np
        p0 = np.zeros((8, 8), dtype=np.uint32); p0[1:7, 1:5] = 1
        p1 = np.zeros((8, 8), dtype=np.uint32); p1[1:7, 1:7] = 1
        return p0, p1

    def _old_preview_order(self, seg, p0, p1):
        """What the preview did BEFORE this fix: post_process each pass, then merge."""
        merged, passes = None, []
        for key, masks in (('0', p0), ('1', p1)):
            masks = seg.post_process(masks, ['Y', 'X'], None, 1, False, real_border=None)
            merged, passes = _merge_pass(seg, merged, masks, key, passes, self._count)
        return merged, passes

    def test_the_old_per_pass_order_really_did_differ(self):
        """The guard on the guard. The three tests below are written against `_post_process_merged`,
        which did not exist before the fix, so none of them can fail against the old code directly.
        This is what pins that the old order was actually WRONG — if the fixture ever stops
        separating the two, they are all guarding nothing."""
        import numpy as np
        p0, p1 = self._passes()
        seg = self._seg(min_cell_size=20)
        merged, passes = _merge_pass(seg, None, p0, '0', [], self._count)
        merged, passes = _merge_pass(seg, merged, p1, '1', passes, self._count)
        run_order, _ = _post_process_merged(seg, merged, passes, self._Ctx())
        old_order, _ = self._old_preview_order(seg, *self._passes())
        self.assertFalse(np.array_equal(run_order > 0, old_order > 0),
                         'fixture no longer separates the two orders')
        self.assertEqual(self._count(run_order), 1, 'the run drops the 12 px sliver')
        self.assertEqual(self._count(old_order), 2, 'the old preview kept it')

    def test_matches_the_run_composition(self):
        import numpy as np
        p0, p1 = self._passes()
        seg = self._seg(min_cell_size=20)

        merged, passes = _merge_pass(seg, None, p0, '0', [], self._count)
        merged, passes = _merge_pass(seg, merged, p1, '1', passes, self._count)
        got, passes = _post_process_merged(seg, merged, passes, self._Ctx())

        # what the RUN does: the same merge, then post_process once over the result
        want = np.zeros((8, 8), dtype=np.uint32); want[1:7, 1:5] = 1
        want[1:7, 5:7] = 2
        want = seg.post_process(want, ['Y', 'X'], None, 1, False)
        np.testing.assert_array_equal(got, want)

    def test_the_sliver_is_judged_at_its_clipped_size(self):
        """The sliver is 12 px; the block it was clipped from is 36 px. A 10 px floor keeps it and
        a 20 px floor drops it, while the old per-pass order judged 36 and kept it at both."""
        for floor, survives in ((10, True), (20, False)):
            p0, p1 = self._passes()
            seg = self._seg(min_cell_size=floor)
            merged, passes = _merge_pass(seg, None, p0, '0', [], self._count)
            merged, passes = _merge_pass(seg, merged, p1, '1', passes, self._count)
            got, passes = _post_process_merged(seg, merged, passes, self._Ctx())
            self.assertEqual(passes[1]['objects'], 1 if survives else 0,
                             f'pass 1 sliver at minCellSize={floor}')
            self.assertEqual(2 in got, survives)

    def test_per_pass_counts_are_recounted_after_filtering(self):
        """`objects` must describe what is ON SCREEN. Without the recount it would still report the
        pre-filter count, so a pass the size filter emptied would claim objects nobody can see."""
        p0, p1 = self._passes()
        seg = self._seg(min_cell_size=20)
        merged, passes = _merge_pass(seg, None, p0, '0', [], self._count)
        merged, passes = _merge_pass(seg, merged, p1, '1', passes, self._count)
        self.assertEqual(passes[1]['objects'], 1, 'before post_process the sliver is still there')
        _, passes = _post_process_merged(seg, merged, passes, self._Ctx())
        self.assertEqual(passes[1]['objects'], 0)
        # the id RANGES are untouched: post_process zeroes and reshapes, it never renumbers
        self.assertEqual([(p['from'], p['to']) for p in passes], [(1, 1), (2, 2)])
