"""RESULT-REVIEW: result-2d6d2cf9 — a legacy OME-XML axis with no unit attribute is in OME's default µm.

`read_ome_meta` states every PhysicalSize* in the one shared `PhysicalSizeUnit` (X's). A Z with no
`PhysicalSizeZUnit` beside X in nm means 2 µm by the OME schema (UnitsLength default "µm"), i.e. 2000 nm —
kept as 2.0 under "nm", the migrate's NGFF write states it 1000x too small.
"""
import tempfile
import unittest
from pathlib import Path

import cecelia.utils.legacy_migrate as lm


class TestUnitlessAxisIsOmeDefault(unittest.TestCase):
    def test_unitless_z_beside_nm_x_is_micrometres(self):
        with tempfile.TemporaryDirectory() as tmp:
            p = Path(tmp) / "METADATA.ome.xml"
            p.write_text('<OME><Image><Pixels PhysicalSizeX="500" PhysicalSizeXUnit="nm" '
                         'PhysicalSizeY="500" PhysicalSizeYUnit="nm" PhysicalSizeZ="2"/></Image></OME>',
                         encoding="utf-8")
            m = lm.read_ome_meta(p)
        self.assertEqual(m["PhysicalSizeUnit"], "nm")
        self.assertAlmostEqual(m["PhysicalSizeZ"], 2000.0)
        self.assertEqual(m["PhysicalSizeY"], 500.0)


if __name__ == "__main__":
    unittest.main()
