"""Figure spec validation + labels (docs/todo/FIGURES_PLAN.md). Rendering needs a GPU adapter and
real data, so it is checked by eye, not here."""
import unittest

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


if __name__ == '__main__':
    unittest.main()
