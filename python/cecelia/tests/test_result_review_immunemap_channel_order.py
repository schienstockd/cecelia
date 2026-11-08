"""RESULT-REVIEW: result-4879a40d

`channel_names` labelled channels by an entry's POSITION in `cell_stainings[].channels[]` and ignored
its `channel_number`. A staining list not in channel order put B cells on the collagen channel; the
register route writes those names onto the image and name-keyed gates/params then run on the wrong
channel. Each entry's `channel_number` now decides; position is only the fallback.
"""
import unittest

from cecelia.utils.immunemap import channel_names


def _ch(numbers_selected):
    return [{'channel_number': n, 'selected': sel} for n, sel in numbers_selected]


class ChannelOrderTest(unittest.TestCase):
    def test_channel_number_wins_over_list_position(self):
        st = [{'class': 'B cells', 'fluorophore': 'GFP',
               'channels': _ch([(1, True), (0, False), (2, False)])},
              {'class': 'Collagen', 'fluorophore': 'SHG',
               'channels': _ch([(1, False), (0, True), (2, False)])}]
        self.assertEqual(channel_names(st, 3), ['Collagen (SHG)', 'B cells (GFP)', 'Channel 2'])

    def test_position_is_the_fallback_without_channel_number(self):
        st = [{'class': 'T cells', 'fluorophore': 'CFP',
               'channels': [{'selected': False}, {'selected': True}]}]
        self.assertEqual(channel_names(st, 2), ['Channel 0', 'T cells (CFP)'])


if __name__ == '__main__':
    unittest.main()
