"""RESULT-REVIEW: result-08c10576 — auto Z brightness comes from the acquisition block that ran (`ExperimentBlockIndex`)
and counts as off under a z-stack setup switched off; a flag in an unused block or setup must not read as on."""
import os
import unittest
import xml.etree.ElementTree as ET  # edits a real fixture into the case

from cecelia.utils import czi_meta

_FIX = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'test-data', 'acquisition'))


def _zen_blue():
    with open(os.path.join(_FIX, 'czi_lsm800_zenblue_metadata.xml'), 'rb') as f:
        return ET.fromstring(f.read())


class ActiveBlockTest(unittest.TestCase):
    def test_flag_under_a_switched_off_z_stack_reads_off(self):
        r = _zen_blue()
        next(r.iter('UseAutoBrightnessCorrection')).text = 'true'
        next(r.iter('ZStackSetup')).set('IsActivated', 'false')
        self.assertEqual(czi_meta.parse_settings(ET.tostring(r))['brightZ']['enabled'], False)

    def test_flag_in_an_unused_block_listed_first_is_ignored(self):
        r = _zen_blue()
        blocks = r.find('Metadata/Experiment/ExperimentBlocks')
        ran = blocks.find('AcquisitionBlock')
        unused = ET.fromstring(ET.tostring(ran))
        next(unused.iter('UseAutoBrightnessCorrection')).text = 'true'
        blocks.insert(0, unused)
        r.find('Metadata/Experiment/ExperimentBlockIndex').text = '1'
        self.assertEqual(czi_meta.parse_settings(ET.tostring(r))['brightZ']['enabled'], False)


if __name__ == '__main__':
    unittest.main()
