"""The preview region decision: visible XY, ONE z-plane, one timepoint.

Pure/headless — no napari, no zarr. What matters here is that the region can never quietly become
bigger than asked: a preview's whole reason to exist is latency (~2 µs per canonical voxel, and cost
scales with CELLS not pixels — see docs/todo/TASK_PREVIEW_PLAN.md Decision 8), so a z-range leaking in
turns a 2 s preview into a 90 s one.

Pairs with `crop_slice_tuple`, which turns these bounds into slices (tested in test_crop_image.py).
"""
import unittest

from cecelia.utils.slice_utils import preview_region_bounds, crop_slice_tuple


class PreviewRegionBoundsTest(unittest.TestCase):
    LEN = {'X': 2048, 'Y': 1024, 'Z': 40, 'T': 50}

    def test_visible_box_one_plane_one_timepoint(self):
        bounds, fb = preview_region_bounds(
            {'X': (100, 600), 'Y': (50, 300)}, z_index=12, t_index=7, axis_len=self.LEN)
        self.assertEqual(bounds, {'X': (100, 600), 'Y': (50, 300), 'Z': (12, 13), 'T': (7, 8)})
        self.assertFalse(fb)

    def test_z_is_always_a_single_plane(self):
        # the guarantee the latency budget rests on — one plane, never a range
        for z in (0, 5, 39):
            bounds, _ = preview_region_bounds(
                {'X': (0, 10), 'Y': (0, 10)}, z_index=z, t_index=0, axis_len=self.LEN)
            self.assertEqual(bounds['Z'][1] - bounds['Z'][0], 1)

    def test_view_beyond_the_image_edge_is_clamped(self):
        # a zoomed-out view reports corners past the edge; an unclamped slice silently returns less
        bounds, _ = preview_region_bounds(
            {'X': (-200, 99999), 'Y': (-5, 4000)}, z_index=0, t_index=0, axis_len=self.LEN)
        self.assertEqual(bounds['X'], (0, 2048))
        self.assertEqual(bounds['Y'], (0, 1024))

    def test_out_of_range_plane_indices_are_clamped(self):
        bounds, _ = preview_region_bounds(
            {'X': (0, 10), 'Y': (0, 10)}, z_index=999, t_index=-3, axis_len=self.LEN)
        self.assertEqual(bounds['Z'], (39, 40))
        self.assertEqual(bounds['T'], (0, 1))

    def test_3d_display_mode_still_previews_one_plane_but_flags_it(self):
        bounds, fb = preview_region_bounds(
            {'X': (0, 512), 'Y': (0, 512)}, z_index=20, t_index=0,
            axis_len=self.LEN, ndisplay=3)
        self.assertEqual(bounds['Z'], (20, 21), 'a 3D viewer must not widen the preview to the stack')
        self.assertTrue(fb, 'the 2D fallback has to be reported so the user can be told')

    def test_no_fallback_flag_for_a_2d_image_in_3d_mode(self):
        # nothing was given up if there is only one plane to begin with
        _, fb = preview_region_bounds(
            {'X': (0, 8), 'Y': (0, 8)}, z_index=0, t_index=0,
            axis_len={'X': 8, 'Y': 8, 'Z': 1, 'T': 1}, ndisplay=3)
        self.assertFalse(fb)

    def test_axes_the_image_lacks_are_omitted(self):
        bounds, fb = preview_region_bounds(
            {'X': (0, 8), 'Y': (0, 8)}, z_index=3, t_index=4, axis_len={'X': 8, 'Y': 8})
        self.assertEqual(bounds, {'X': (0, 8), 'Y': (0, 8)})
        self.assertFalse(fb)

    def test_degenerate_box_is_dropped_not_inverted(self):
        bounds, _ = preview_region_bounds(
            {'X': (600, 600), 'Y': (300, 100)}, z_index=0, t_index=0, axis_len=self.LEN)
        self.assertNotIn('X', bounds)
        self.assertNotIn('Y', bounds)

    def test_feeds_crop_slice_tuple_directly(self):
        # the two halves meet: decision -> slices, no third representation in between
        bounds, _ = preview_region_bounds(
            {'X': (100, 600), 'Y': (50, 300)}, z_index=12, t_index=7, axis_len=self.LEN)
        axis_idx = {'T': 0, 'C': 1, 'Z': 2, 'Y': 3, 'X': 4}
        s = crop_slice_tuple(5, axis_idx, bounds)
        self.assertEqual(s, (slice(7, 8), slice(None), slice(12, 13),
                            slice(50, 300), slice(100, 600)))
        self.assertEqual(s[1], slice(None), 'channels must stay full')


if __name__ == '__main__':
    unittest.main()
