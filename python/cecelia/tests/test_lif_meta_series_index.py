"""RESULT-REVIEW: result-6a6bd8d3

`series` is the series picker's index (readlif's `image_list`), so `lif_meta` must find the SAME image
for it. An image element whose `Memory Size` is 0 is still listed by readlif; a reader that skipped it
read the NEXT image's settings for every later series — and when the neighbour has the same shape, the
store's channel/z/t check cannot catch it. The header is the real LAS X fixture with only that one
attribute changed (and, in the second case, two images moved into a sub-folder).
"""
import os
import re
import tempfile
import unittest

from cecelia.tests.test_lif_meta import HEADER, write_lif
from cecelia.utils import lif_meta


def _xml():
    with open(HEADER, encoding='utf-8') as f:
        return f.read()


class TestSeriesIsThePickersIndex(unittest.TestCase):
    def _settings(self, xml, series):
        fd, path = tempfile.mkstemp(suffix='.lif')
        os.close(fd)
        self.addCleanup(os.remove, path)
        return lif_meta.acquisition_settings(write_lif(xml, path), series=series)

    def test_zero_size_image_still_counts(self):
        # Image001's own <Memory Size="..."> (its last Memory element before Image002's Element) set to 0
        head, tail = _xml().split('<Element Name="Image002"', 1)
        head = re.sub(r'<Memory Size="\d+"([^>]*)>(?!.*<Memory )', r'<Memory Size="0"\1>', head, flags=re.S)
        xml = head + '<Element Name="Image002"' + tail
        self.assertIn('<Memory Size="0"', xml)
        names = [self._settings(xml, i).get('imageName') for i in range(3)]
        self.assertEqual(names, ['Image001', 'Image002', 'Image007'])

    def test_images_in_a_sub_folder(self):
        from readlif.reader import LifFile
        xml = _xml()
        i002 = xml.index('<Element Name="Image002"')
        end = xml.rindex('</Children></Element></LMSDataContainerHeader>')
        moved = xml[i002:end]
        folder = ('<Element Name="Folder" Visibility="1" CopyOption="1"><Data><Collection/></Data>'
                  '<Memory Size="0" MemoryBlockID=""/><Children>' + moved + '</Children></Element>')
        xml = xml[:i002] + folder + xml[end:]
        fd, path = tempfile.mkstemp(suffix='.lif')
        os.close(fd)
        self.addCleanup(os.remove, path)
        lf = LifFile(write_lif(xml, path))
        self.assertEqual([e['name'] for e in lf.image_list], ['Image001', 'Folder/Image002', 'Folder/Image007'])
        self.assertEqual([self._settings(xml, i).get('imageName') for i in range(3)], ['Image001', 'Image002', 'Image007'])


if __name__ == '__main__':
    unittest.main()
