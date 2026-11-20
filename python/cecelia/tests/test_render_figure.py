"""Figure spec validation + labels (docs/todo/FIGURES_PLAN.md). The look's overlays are rendered end to
end on a tiny store (a GPU adapter or the software one, as the movie runner's tests); the house look
itself is checked by eye."""
import os
import shutil
import tempfile
import unittest

import numpy as np

from cecelia.writers import render_figure as rf


def _spec(**kw):
    s = dict(zarrPath='x.zarr', outDir='out', center=[10, 10], sizePx=20, z=0,
             frames=[0, 1], rows=[{'channels': [{'c': 0, 'colour': '#ff00ff'}]}])
    s.update(kw)
    return s


class TestFigureSpec(unittest.TestCase):
    def test_defaults(self):
        s = rf.validate(_spec())
        self.assertEqual((s['kind'], s['interpolation'], s['zHalf']), ('sequence', 'pixels', 0))
        # auto tile width: a still or a pair 80 mm, nine frames 20 mm (a page width), never past the clamps
        self.assertEqual([rf.tile_mm(rf.STYLE, n) for n in (1, 2, 3, 9, 20)], [80, 80, 60, 20, 20])
        self.assertEqual(rf.tile_mm(dict(rf.STYLE, tileMm=35), 9), 35)          # a chosen width wins
        self.assertEqual(rf.tile_px(s, rf.STYLE, 9), 240)   # 20 mm at 300 dpi = 236 px, snapped to 12 x 20 px
        self.assertEqual(rf.tile_px(s, rf.STYLE, 1), 940)   # 80 mm = 945 px → 47 x 20

    def test_errors_name_the_field(self):
        cases = [(dict(kind='montage'), 'kind'), (dict(frames=[]), 'frames'),
                 (dict(tilePx=99999), 'tilePx'), (dict(interpolation='cubic'), 'interpolation'),
                 (dict(rows=[{'channels': [{'c': 0, 'colour': 'pink'}]}]), 'colour'),
                 (dict(rows=[{'channels': []}]), 'rows[].channels'),
                 (dict(frames=list(range(65))), 'exceeds'),
                 (dict(kind='zstack'), 't and planes'), (dict(layout='matrix'), 'layout')]
        for kw, needle in cases:
            with self.subTest(kw=kw):
                with self.assertRaises(rf.FigureSpecError) as e:
                    rf.validate(_spec(**kw))
                self.assertIn(needle, str(e.exception))

    def test_elapsed_label(self):
        # the viewer's clock by default; minutes (hours from two hours) as a style option
        self.assertEqual(rf.elapsed_label(50, 15), '0:12:30')
        self.assertEqual(rf.STYLE['timeFormat'], 'clock')
        self.assertEqual(rf.elapsed_label(50, 15, 'min'), '12.5 min')
        self.assertEqual(rf.elapsed_label(64, 15, 'min'), '16 min')
        self.assertEqual(rf.elapsed_label(110, 900, 'min'), '27.5 h')
        self.assertEqual(rf.elapsed_label(7, None), 't7')
        with self.assertRaises(rf.FigureSpecError):     # an unknown format is named, not drawn as minutes
            rf.elapsed_label(50, 15, 'hours')

    def test_still_has_no_time_label(self):
        # a still's only tile is not "t0": no title, and no empty band above the tiles for it
        s = rf.validate(_spec(frames=[0]))
        self.assertEqual([c[3] for c in rf._columns(s, still=True)], [''])
        self.assertEqual([c[3] for c in rf._columns(s)], ['t0'])          # a movie's first frame keeps it

    def test_circle_lands_where_the_crop_puts_it(self):
        # the tile shows the 200 px square around center [y=500, x=300] at 100 px: x 300 → 50, r 40 → 20
        self.assertEqual(rf.circle_box({'x': 300, 'y': 500, 'r': 40}, [500, 300], 200, 100), (30.0, 30.0, 70.0, 70.0))
        self.assertEqual(rf.circle_box({'x': 250, 'y': 450, 'r': 10}, [500, 300], 200, 100), (20.0, 20.0, 30.0, 30.0))
        with self.assertRaises(rf.FigureSpecError):
            rf.validate(_spec(circles=[{'x': 1, 'y': 2}]))
        rf.validate(_spec(circles=[{'x': 1, 'y': 2, 'r': 3, 'colour': '#ffffff'}]))

    def test_zstack_columns_label_offset(self):
        s = rf.validate(_spec(kind='zstack', t=3, planes=[4, 5, 6], z=5, zStepUm=2))
        self.assertEqual([c[3] for c in rf._columns(s)], ['z4 (-2 µm)', 'z5 (+0 µm)', 'z6 (+2 µm)'])

    def test_scale_bar_placement(self):
        on = lambda w: [(r, c) for r in range(2) for c in range(3) if rf._shows_bar(w, r, c, 2, 3)]
        self.assertEqual(on('first'), [(0, 0)])
        self.assertEqual(on('last'), [(1, 2)])
        self.assertEqual(len(on('all')), 6)
        with self.assertRaises(rf.FigureSpecError):
            rf._shows_bar('middle', 0, 0, 1, 1)

    def test_grid_columns(self):
        # strip = one line; grid = near-square, the last line as full as it can be
        self.assertEqual(rf.grid_columns(8, 1, 'strip'), 8)
        self.assertEqual(rf.validate(_spec())['layout'], 'strip')
        self.assertEqual([rf.grid_columns(n, 1, 'grid') for n in (1, 2, 4, 8, 9, 10, 16, 64)],
                         [1, 2, 2, 3, 3, 4, 4, 8])
        # a split figure's frame is several tiles tall → more frames per line
        self.assertEqual(rf.grid_columns(8, 4, 'grid'), 4)
        self.assertEqual(rf.grid_columns(9, 3, 'grid'), 5)

    def test_compose_grid_wraps_frames(self):
        from PIL import Image
        tile, n = 30, 8
        style = dict(rf.STYLE, dpi=100)
        tiles = [Image.new('RGB', (tile, tile), (10 * i + 5, 0, 0)) for i in range(n)]  # none black
        labels = [f't{i}' for i in range(n)]
        rows = [(tiles, [('A', (255, 0, 255))])]
        strip = rf._compose(rows, labels, tile, style)
        grid = rf._compose(rows, labels, tile, style, rf.grid_columns(n, 1, 'grid'))
        gap = rf._px(style['gapMm'], style['dpi'])
        self.assertEqual(strip.width, n * tile + (n - 1) * gap)
        self.assertEqual(grid.width, 3 * tile + 2 * gap)
        self.assertGreater(grid.height, 2.5 * strip.height)          # three lines of frames
        # frame 3 opens the second line, at x = 0; frame 7 ends the third, in the middle column
        blocks_y = [y for y in range(grid.height) if grid.getpixel((tile // 2, y)) == (35, 0, 0)]
        self.assertTrue(blocks_y)
        self.assertTrue(any(grid.getpixel((tile + gap + tile // 2, y)) == (75, 0, 0)
                            for y in range(grid.height)))
        # one row → one legend band (black) under the last line only: none between the lines
        bands = [y for y in range(grid.height) if grid.getpixel((tile // 2, y)) == (0, 0, 0)]
        self.assertTrue(all(y > blocks_y[-1] for y in bands) and bands)


class TestFigureLookOverlays(unittest.TestCase):
    """A spec's ``lookOverlays`` (Julia's ``figure_look_overlays``) reach the movie renderer: the mask and
    the points are drawn on the tiles, each frame's points on its own tile, and figure.json keeps only
    that they were drawn."""
    N, Z = 40, 4

    @classmethod
    def setUpClass(cls):
        from cecelia.tests.test_render_animation_run import _store
        from cecelia.tests.test_wgsl_utils import _shared_host
        _shared_host()                      # skip, or fail under CECELIA_REQUIRE_WGPU, like the rest
        cls.d = tempfile.mkdtemp()
        img = np.full((2, 1, cls.Z, cls.N, cls.N), 300, np.uint16)     # a dim channel everywhere
        cls.zarr = _store(os.path.join(cls.d, 'img.zarr'), img, 'tczyx')
        lab = np.zeros((2, cls.Z, cls.N, cls.N), np.uint32)
        lab[:, :, 4:16, 4:16] = 3                                       # one cell, top-left, every t
        cls.labels = _store(os.path.join(cls.d, 'lab.zarr'), lab, 'tzyx')

    @classmethod
    def tearDownClass(cls):
        shutil.rmtree(cls.d, ignore_errors=True)

    def _render(self, name, **extra):
        from PIL import Image
        out = os.path.join(self.d, name)
        spec = dict(zarrPath=self.zarr, outDir=out, center=[20, 20], sizePx=self.N, z=1, frames=[0, 1],
                    tilePx=2 * self.N, rows=[{'channels': [{'c': 0, 'colour': '#ff00ff'}]}],
                    display={'ranges': {'0': [0, 1000]}})
        spec.update(extra)
        rf.render_figure(spec)
        import json
        with open(os.path.join(out, 'figure.json')) as f:
            resolved = json.load(f)
        return np.asarray(Image.open(os.path.join(out, 'figure.png')).convert('RGB')).astype(int), resolved

    def test_the_looks_mask_and_points_are_drawn(self):
        look = {'params': {'labelsPath': self.labels, 'labelOpacity': 1.0, 'labelContourPx': 0,
                           'labelColouring': 'table', 'labelColours': {'ids': [3], 'colours': [[0.0, 0.0, 1.0]]},
                           'pointSizePx': 3, 'overlaySizeUnit': 'image', 'notAFigureKey': 1},
                # a point on t = 1 only, bottom-right; on plane 1, inside the column's window
                'frames': {'1': {'points': {'x': [30.0], 'y': [30.0], 'z': [1.0], 'colour': [[0.0, 1.0, 0.0]]}}},
                'planeFilters': {'1:1': {'points': [0, 2], 'tracks': [0, 2]}}}
        plain, plain_spec = self._render('plain')
        drawn, drawn_spec = self._render('drawn', lookOverlays=look)
        blue = lambda a: (a[..., 2] > 150) & (a[..., 0] < 80) & (a[..., 1] < 80)
        green = lambda a: (a[..., 1] > 150) & (a[..., 0] < 80) & (a[..., 2] < 80)
        self.assertEqual(int(blue(plain).sum()), 0)
        self.assertEqual(int(green(plain).sum()), 0)
        # the mask on both tiles (12 px cell at x2 = 24 x 24 output px each), the point on the second only
        ys, xs = np.nonzero(blue(drawn))
        tile = 2 * self.N
        self.assertGreater(int((xs < tile).sum()), 300)
        self.assertGreater(int((xs > tile).sum()), 300)
        gy, gx = np.nonzero(green(drawn))
        self.assertGreater(len(gx), 4)
        self.assertTrue((gx > tile).all())
        self.assertNotIn('lookOverlays', drawn_spec)
        self.assertTrue(drawn_spec.get('lookOverlaysDrawn'))
        self.assertNotIn('lookOverlaysDrawn', plain_spec)

    def test_points_off_the_columns_planes_are_hidden(self):
        look = {'params': {'pointSizePx': 3, 'overlaySizeUnit': 'image'},
                'frames': {'1': {'points': {'x': [30.0], 'y': [30.0], 'z': [3.0], 'colour': [[0.0, 1.0, 0.0]]}}},
                'planeFilters': {'1:1': {'points': [0, 2], 'tracks': [0, 2]}}}
        drawn, _ = self._render('offplane', lookOverlays=look)
        self.assertEqual(int(((drawn[..., 1] > 150) & (drawn[..., 0] < 80)).sum()), 0)


    def test_split_rows_show_the_look_on_the_merge_only(self):
        look = {'params': {'labelsPath': self.labels, 'labelOpacity': 1.0, 'labelContourPx': 0,
                           'labelColouring': 'table', 'labelColours': {'ids': [3], 'colours': [[0.0, 0.0, 1.0]]}}}
        rows = [{'channels': [{'c': 0, 'colour': '#ff00ff'}]}, {'channels': [{'c': 0, 'colour': '#ffffff'}]}]
        drawn, _ = self._render('split', lookOverlays=look, rows=rows)
        blue = (drawn[..., 2] > 150) & (drawn[..., 0] < 80) & (drawn[..., 1] < 80)
        ys, _ = np.nonzero(blue)
        self.assertGreater(len(ys), 300)
        self.assertLess(int(ys.max()), drawn.shape[0] // 2)        # the merge row is the top half


class TestLookWidths(unittest.TestCase):
    """The viewer draws the mask outline in image px (it grows with zoom) and the point border in screen
    px — a figure draws each as it looked."""

    def test_scales(self):
        self.assertAlmostEqual(rf.screen_px_scale(300, 100, 100), 3.125)
        self.assertAlmostEqual(rf.screen_px_scale(96, 50, 200), 0.25)     # smooth: drawn small, enlarged
        self.assertAlmostEqual(rf.image_px_scale(828, 414), 2.0)          # a crop enlarged 2x

    def test_outline_is_image_px_border_is_screen_px(self):
        s = {'lookOverlays': {'params': {'labelContourPx': 1, 'pointBorderPx': 2, 'pointSizePx': 3}}}
        self.assertEqual(rf._look_params(s, screen=3.125, image=2.0),
                         {'labelContourPx': 2, 'pointBorderPx': 6, 'pointSizePx': 3})
        self.assertEqual(rf._look_params({'lookOverlays': {'params': {'labelContourPx': 0}}}, image=3.0),
                         {'labelContourPx': 0})
        self.assertEqual(rf._look_params({'lookOverlays': {'params': {'labelContourPx': 1}}}, image=0.2),
                         {'labelContourPx': 1})


if __name__ == '__main__':
    unittest.main()
