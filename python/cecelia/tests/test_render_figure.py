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
                 (dict(kind='zstack'), 't and planes')]
        for kw, needle in cases:
            with self.subTest(kw=kw):
                with self.assertRaises(rf.FigureSpecError) as e:
                    rf.validate(_spec(**kw))
                self.assertIn(needle, str(e.exception))

    def test_clock_label_matches_movie_format(self):
        self.assertEqual(rf.clock_label(50, 15), '0:12:30')
        self.assertEqual(rf.clock_label(110, 900), '27:30:00')
        self.assertEqual(rf.clock_label(7, None), 't7')

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


if __name__ == '__main__':
    unittest.main()
