"""`qptiff_meta` on TIFFs written with the per-page ImageDescription of real Akoya `.qptiff` files
(test-data/acquisition/qptiff_*: a Fusion 2.4.1 single scan, a Fusion 2.4.0 PhenoCycler run with its
`.xpd`, a Fusion 2.3.0 brightfield scan; names scrubbed).

The pixels are stand-ins (the real pages are 106560 x 63360); the descriptions and the DateTime tag are
the file's own. Expected values are read from the fixture's XML in the test where they can be.
"""
import json
import os
import re
import shutil
import tempfile
import unittest

import numpy as np
import tifffile   # tests write the TIFF the reader opens; the reader itself goes through ome_xml_utils

from cecelia.utils import qptiff_meta

_FIX = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'test-data', 'acquisition'))
PAGES = os.path.join(_FIX, 'qptiff_fusion-2.4.1_pages.json')
CYCLIC = os.path.join(_FIX, 'qptiff_fusion-2.4.0_cyclic_pages.json')
XPD = os.path.join(_FIX, 'qptiff_fusion-2.4.0_cyclic.xpd')
BRIGHTFIELD = os.path.join(_FIX, 'qptiff_fusion-2.3.0_brightfield_pages.json')


def _pages(path=PAGES):
    with open(path, encoding='utf-8') as f:
        return json.load(f)['pages']


def _tag(desc, name):
    return re.search(rf'<{name}>([^<]*)</{name}>', desc).group(1)


class _QptiffCase(unittest.TestCase):
    def qptiff(self, pages, full_shape=(8, 8), xpd=None):
        """A TIFF with one page per fixture page, in its own folder (beside a copy of `xpd` when given):
        the full-resolution pages `full_shape`, the rest 4x4; an RGB fixture page stays RGB."""
        folder = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, folder)
        path = os.path.join(folder, 'scan.qptiff')
        if xpd:
            shutil.copy(xpd, os.path.join(folder, 'run.xpd'))
        with tifffile.TiffWriter(path) as w:
            for p in pages:
                full = '<ImageType>FullResolution</ImageType>' in p['description']
                rgb = len(p.get('shape', ())) == 3
                shape = tuple(full_shape if full else (4, 4)) + ((3,) if rgb else ())
                w.write(np.zeros(shape, np.uint8 if rgb else np.uint16), description=p['description'],
                        datetime=p['datetime'], metadata=None, photometric='rgb' if rgb else 'minisblack')
        return path


class TestFusionScan(_QptiffCase):
    def setUp(self):
        self.pages = _pages()
        self.full = [p['description'] for p in self.pages if '<ImageType>FullResolution' in p['description']]
        self.s = qptiff_meta.acquisition_settings(self.qptiff(self.pages), series=0)

    def test_one_channel_per_full_resolution_page(self):
        self.assertEqual([c['name'] for c in self.s['channels']], [_tag(d, 'Name') for d in self.full])
        self.assertEqual([c['name'] for c in self.s['channels']], ['DAPI', 'ATTO 550', 'FITC'])

    def test_system_technology_camera(self):
        s = self.s
        self.assertEqual((s['source'], s['technology'], s['system'], s['softwareVersion']),
                         ('qptiff', 'widefield', 'PhenoImager Fusion', 'Fusion 2.4.1'))
        self.assertEqual(s['readerVersion'], qptiff_meta.READER_VERSION)
        self.assertEqual(s['camera'], {'name': 'Sony IMX421-based camera', 'binning': 1, 'bitDepth': 14,
                                       'sensorPixelUm': None})
        self.assertEqual(s['objective']['magnification'], 20.0)
        self.assertEqual((s['lightSource'], s['processing'], s['saturationProtection'], s['pixelSizeUm']),
                         ('XCiteMultiBandLed', 'OffsetAndShading', 'EnhancedDynamicRange', 0.25))
        self.assertEqual(s['lasers'], [])

    def test_exposure_is_microseconds_in_the_file(self):
        for c, d in zip(self.s['channels'], self.full):
            self.assertAlmostEqual(c['exposureMs'], float(_tag(d, 'ExposureTime')) / 1000)
        self.assertEqual([c['exposureMs'] for c in self.s['channels']], [1.5, 393.21, 78.67])

    def test_bands_are_the_active_excitation_and_its_emission_partner(self):
        chans = self.s['channels']
        self.assertEqual([(c['excitationFilter'], c['emissionFilter']) for c in chans],
                         [('377-405 nm', '440-466 nm'), ('533-559 nm', '570-640 nm'), ('450-486 nm', '502-548 nm')])
        self.assertEqual([c['filterCube'] for c in chans], ['DAPI / ATTO550 / AF750'] * 2 + ['FITC / Cy5'])

    def test_no_marker_means_no_biomarker_and_not_cyclic(self):
        self.assertEqual({c['biomarker'] for c in self.s['channels']}, {None})
        self.assertNotIn('cyclic', self.s['modes'])
        self.assertEqual({c['cycle'] for c in self.s['channels']}, {None})
        self.assertNotIn('cycles', self.s)

    def test_acquired_at_is_the_tiff_datetime_without_zone(self):
        self.assertEqual(self.s['acquiredAt'], '2026-11-05T09:13:34')

    def test_one_camera_field_is_not_tiled_a_wider_image_is(self):
        self.assertEqual(self.s['modes'], [])
        wide = qptiff_meta.acquisition_settings(self.qptiff(self.pages, full_shape=(2, 1921)))
        self.assertEqual(wide['modes'], ['tiled'])

    def test_binning_shrinks_a_camera_field(self):
        pages = [dict(p) for p in self.pages]
        for p in pages:
            p['description'] = p['description'].replace('<Binning>1</Binning>', '<Binning>2</Binning>', 1)
        self.assertEqual(qptiff_meta.acquisition_settings(self.qptiff(pages, full_shape=(2, 1500)))['modes'], ['tiled'])

    def test_other_series_are_not_fluorescence(self):
        self.assertIn('reason', qptiff_meta.acquisition_settings(self.qptiff(self.pages), series=1))

    def test_biomarker_names_the_channel_and_the_fluor_moves_to_dye(self):
        pages = [dict(p) for p in self.pages]
        pages[0]['description'] = pages[0]['description'].replace('<Name>DAPI</Name>', '<Name>DAPI</Name>\n  <Biomarker>CD8</Biomarker>', 1)
        c = qptiff_meta.acquisition_settings(self.qptiff(pages))['channels'][0]
        self.assertEqual((c['name'], c['dyePreset'], c['biomarker']), ('CD8', 'DAPI', 'CD8'))


class TestPhenoCyclerRun(_QptiffCase):
    def setUp(self):
        self.pages = _pages(CYCLIC)
        self.s = qptiff_meta.acquisition_settings(self.qptiff(self.pages, xpd=XPD))
        with open(XPD, encoding='utf-8') as f:
            self.xpd = json.load(f)

    def test_markers_name_the_channels(self):
        self.assertEqual([c['name'] for c in self.s['channels']],
                         ['DAPI', 'Podoplanin', 'SMA', 'CD68', 'CD45', 'HLA-DR', 'HIF1A'])
        self.assertIn('cyclic', self.s['modes'])

    def test_cycle_is_the_rank_of_the_page_write_time(self):
        self.assertEqual([c['cycle'] for c in self.s['channels']], [1, 2, 2, 2, 3, 3, 3])
        # the .xpd wells in run order: blank, C1, C2, blank — the markers' cycles are the wells' positions
        wells = [[it['markerName'] for it in w['items']] for w in self.xpd['wells']]
        for c in self.s['channels'][1:]:
            self.assertIn(c['name'], wells[c['cycle'] - 1])
        self.assertEqual(self.s['cycles'], {'count': 3, 'planned': 4, 'blank': 2, 'experiment': 'EXPERIMENT'})

    def test_xpd_adds_reagents_and_planned_exposure(self):
        cd68 = next(c for c in self.s['channels'] if c['name'] == 'CD68')
        self.assertEqual((cd68['barcode'], cd68['reporter'], cd68['clone'], cd68['dyePreset']),
                         ('BX015', 'RX015', 'KP1', 'AF647'))
        for c in self.s['channels']:
            self.assertEqual(c['exposureMs'], c['plannedExposureMs'])   # ms in the .xpd, µs in the file

    def test_autofluorescence_already_subtracted(self):
        self.assertEqual({c['afSubtracted'] for c in self.s['channels']}, {True})

    def test_without_the_xpd_the_file_alone_still_gives_markers_and_cycles(self):
        s = qptiff_meta.acquisition_settings(self.qptiff(self.pages))
        self.assertEqual([c['cycle'] for c in s['channels']], [1, 2, 2, 2, 3, 3, 3])
        self.assertEqual(s['cycles'], {'count': 3})
        self.assertEqual({c['barcode'] for c in s['channels']}, {None})


class TestBrightfield(_QptiffCase):
    def test_brightfield_is_not_fluorescence(self):
        s = qptiff_meta.acquisition_settings(self.qptiff(_pages(BRIGHTFIELD)))
        self.assertEqual((s['technology'], s['lightSource']), ('brightfield', 'WhiteLED'))
        self.assertEqual([(c['name'], c['excitationFilter'], c['cycle']) for c in s['channels']], [('BF', None, None)])
        self.assertNotIn('cyclic', s['modes'])


class TestNotQptiff(_QptiffCase):
    def test_plain_tiff_is_a_reason(self):
        fd, path = tempfile.mkstemp(suffix='.qptiff')
        os.close(fd)
        self.addCleanup(os.remove, path)
        tifffile.imwrite(path, np.zeros((4, 4), np.uint8))
        self.assertIn('reason', qptiff_meta.acquisition_settings(path))


if __name__ == '__main__':
    unittest.main()
