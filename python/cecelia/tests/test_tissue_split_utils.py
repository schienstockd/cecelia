"""Tissue detection for `editImages.splitTissues` (`cecelia.utils.tissue_split_utils`).

Synthetic slides at 10 µm/px, each built so the expected tissue count follows from the rule under
test, not from running the code: interlaced sections, a thin neck vs a dent, crumbs of a torn section,
dust, reading order, and the masks never overlapping."""
import unittest

import numpy as np

from cecelia.utils import tissue_split_utils as T

PX = 10.0   # µm per pixel


def _params(**kw):
    p = T.SplitParams()
    for k, v in kw.items():
        setattr(p, k, v)
    return p


def _split(img, **kw):
    return T.split_tissue_labels(img.astype(np.float32), PX, _params(**kw))


class TestSplitTissueLabels(unittest.TestCase):

    def test_two_separate_sections(self):
        img = np.zeros((300, 400))
        img[20:120, 20:150] = 1
        img[150:280, 220:380] = 1
        lab, areas = _split(img)
        self.assertEqual(len(areas), 2)

    def test_interlaced_sections_have_disjoint_masks(self):
        # two L-shapes whose bounding boxes overlap, 200 µm apart: the masks must not share a pixel,
        # and each must cover its own L
        img = np.zeros((300, 300))
        img[20:260, 20:80] = 1;  img[200:260, 20:200] = 1        # L
        img[20:80, 100:280] = 1; img[20:180, 220:280] = 1        # rotated L, 20 px from the first
        lab, areas = _split(img)
        self.assertEqual(len(areas), 2)
        a, b = lab[50, 50], lab[50, 250]
        self.assertNotEqual(a, b)
        self.assertNotEqual(a, 0)
        self.assertTrue((lab[200:260, 20:200] == a).all())
        self.assertTrue((lab[20:180, 220:280] == b).all())

    def test_thin_neck_splits_but_a_dent_does_not(self):
        neck = np.zeros((200, 400))
        neck[40:160, 20:180] = 1
        neck[40:160, 220:380] = 1
        neck[95:105, 180:220] = 1                                 # 10 px bridge, sides 120 px tall
        _, areas = _split(neck)
        self.assertEqual(len(areas), 2)

        dent = np.zeros((200, 400))
        dent[40:160, 20:380] = 1
        dent[40:80, 190:210] = 0                                  # notch a third of the way in
        _, areas = _split(dent)
        self.assertEqual(len(areas), 1)

    def test_split_neck_zero_never_splits(self):
        neck = np.zeros((200, 400))
        neck[40:160, 20:180] = 1
        neck[40:160, 220:380] = 1
        neck[95:105, 180:220] = 1
        _, areas = _split(neck, splitNeck=0)
        self.assertEqual(len(areas), 1)

    def test_crumbs_join_their_section_within_the_gap(self):
        img = np.zeros((300, 400))
        img[50:250, 50:250] = 1                                   # 4 mm² section
        img[100:110, 270:280] = 1                                 # crumbs 200 µm off its edge
        img[150:160, 270:280] = 1
        _, areas = _split(img, mergeGapUm=300, minAreaMm2=0.001)
        self.assertEqual(len(areas), 1)
        _, areas = _split(img, mergeGapUm=100, minAreaMm2=0.001)  # gap too small: crumbs stand alone
        self.assertEqual(len(areas), 3)

    def test_two_close_sections_of_similar_size_stay_apart(self):
        # 200 µm apart, inside the join gap — but neither is a crumb of the other
        img = np.zeros((200, 400))
        img[40:160, 20:180] = 1
        img[40:160, 200:380] = 1
        _, areas = _split(img, mergeGapUm=300)
        self.assertEqual(len(areas), 2)

    def test_dust_and_small_pieces_are_dropped(self):
        img = np.zeros((300, 300))
        img[20:180, 20:180] = 1
        img[10:12, 280:282] = 1                                   # 0.0004 mm²: dust
        img[260:280, 260:280] = 1                                 # 0.04 mm², 800 µm from the section
        _, areas = _split(img, minAreaMm2=0.1)
        self.assertEqual(len(areas), 1)
        _, areas = _split(img, minAreaMm2=0.01)                   # small piece kept, dust still not
        self.assertEqual(len(areas), 2)

    def test_reading_order(self):
        img = np.zeros((400, 400))
        img[20:150, 220:380] = 1                                  # top right
        img[30:160, 20:180] = 1                                   # top left, a little lower
        img[230:380, 120:280] = 1                                 # bottom
        lab, _ = _split(img)
        self.assertEqual([lab[80, 100], lab[80, 300], lab[300, 200]], [1, 2, 3])

    def test_blank_image_finds_nothing(self):
        lab, areas = _split(np.zeros((100, 100)))
        self.assertEqual(areas, {})
        self.assertFalse(lab.any())
        self.assertEqual(lab.dtype, np.uint16)                     # same type as a slide with tissue

    def test_margin_grows_the_mask_but_not_into_a_neighbour(self):
        img = np.zeros((200, 320))
        img[50:150, 20:140] = 1
        img[50:150, 160:300] = 1                                  # 200 µm apart
        lab, _ = _split(img, marginUm=100, mergeGapUm=0)
        self.assertNotEqual(lab[100, 15], 0)                       # 50 µm outside: grown into
        self.assertEqual(lab[100, 145], lab[100, 100])             # each grows into the gap
        self.assertEqual(lab[100, 155], lab[100, 200])
        self.assertNotEqual(lab[100, 100], lab[100, 200])


class TestGeometry(unittest.TestCase):

    def test_detection_level_is_the_finest_at_least_the_target(self):
        shapes = [(8000, 8000), (4000, 4000), (2000, 2000), (1000, 1000)]
        # level pixels 0.5, 1, 2, 4 µm — none reaches 10, so the coarsest
        self.assertEqual(T.detection_level(shapes, 0.5, target_um=10), 3)
        # level pixels 3, 6, 12, 24 µm — level 2 is the first at least 10
        self.assertEqual(T.detection_level(shapes, 3.0, target_um=10), 2)
        # the ratio is read from the shapes, even for a 1-row level: 20 rows → 1 row is 20 µm at 1 µm
        self.assertEqual(T.detection_level([(20, 20), (1, 1), (1, 1)], 1.0, target_um=15), 1)

    def test_labels_block_maps_full_resolution_to_detection_pixels(self):
        lab = np.array([[1, 2], [3, 4]], np.uint16)
        out = T.labels_block(lab, (10.0, 10.0), np.array([0, 9, 10, 19]), np.array([5, 15]))
        self.assertEqual(out.tolist(), [[1, 2], [1, 2], [3, 4], [3, 4]])
        # a full-resolution row past the detection grid's last row maps to that last row
        out = T.labels_block(lab, (10.0, 10.0), np.array([25]), np.array([0]))
        self.assertEqual(out.tolist(), [[3]])

    def test_largest_share_of_the_image(self):
        # a 100 × 200 detection grid at 10 µm/px = 2 mm²; a 0.5 mm² tissue is a quarter of it
        det = T.Detection(np.zeros((100, 200), np.uint16), 0, (2.0, 2.0), (200, 400), 10.0,
                          [T.Tissue(1, 0.5, (0, 1, 0, 1)), T.Tissue(2, 0.1, (0, 1, 0, 1))])
        self.assertAlmostEqual(det.largest_share(), 0.25)

    def test_boxes_in_full_resolution(self):
        lab = np.zeros((10, 10), np.uint16)
        lab[2:5, 3:7] = 1
        (t,) = T.tissues_from_labels(lab, {1: 0.5}, (4.0, 4.0), (40, 40))
        self.assertEqual(t.box, (8, 20, 12, 28))
        self.assertEqual(t.area_mm2, 0.5)                          # the area passed in for label 1
        self.assertEqual(t.centroid, (3 * 4.0, 4.5 * 4.0))         # rows 2–4, cols 3–6, × 4 px

    def test_tissue_signal_ignores_blank_channels(self):
        planes = np.ones((3, 40, 40))                              # glass at 1 in every channel
        planes[0, 10:20, 10:20] = 100
        planes[2, 25:30, 25:30] = 7
        sig = T.tissue_signal(planes)
        self.assertAlmostEqual(float(sig[15, 15]), 0.5)            # 1 of the 2 channels with contrast
        self.assertAlmostEqual(float(sig[27, 27]), 0.5)
        self.assertEqual(float(sig[0, 0]), 0.0)                    # glass


if __name__ == '__main__':
    unittest.main()
