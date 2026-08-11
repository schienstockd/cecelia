"""Units written into OME-XML must be schema-valid SYMBOLS.

OME's `UnitsLength` / `UnitsTime` are enumerations. A value outside them makes the whole `<Pixels>`
element schema-invalid, and Bio-Formats then discards the ENTIRE OME block and falls back to counting
IFDs — a 31x4x32 movie opens as 3968 timepoints, one channel, no names and no voxel size. Confirmed
against real Bio-Formats (bioformats2raw): `µm` round-trips in full, `micrometer` yields nothing.

The trap is that `micrometer` is CORRECT where it comes from — NGFF `.zattrs` axes use UDUNITS-2
names, and `ccid.json` mirrors them because the importer reads the unit off the axes. Only the
OME-XML boundary needs the symbol, which is what `_OME_XML_UNIT` is for. The Julia side keeps its own
copy of the map (`omezarr.jl`); the cross-language golden test keeps the two equal, and this asserts
the vocabulary itself is legal.

Part of the Python (analysis-env) test suite — run with `pixi run test-py`.
"""
import re
import unittest
from pathlib import Path

import cecelia.utils.zarr_utils as zarr_utils

VALID_LENGTH = {'Ym', 'Zm', 'Em', 'Pm', 'Tm', 'Gm', 'Mm', 'km', 'hm', 'dam', 'm', 'dm', 'cm', 'mm',
                'µm', 'nm', 'pm', 'fm', 'am', 'zm', 'ym', 'Å', 'thou', 'li', 'in', 'ft', 'yd', 'mi',
                'ua', 'ly', 'pc', 'pt', 'pixel', 'reference frame'}
VALID_TIME = {'Ys', 'Zs', 'Es', 'Ps', 'Ts', 'Gs', 'Ms', 'ks', 'hs', 'das', 's', 'ds', 'cs', 'ms',
              'µs', 'ns', 'ps', 'fs', 'as', 'zs', 'ys', 'min', 'h', 'd'}
VALID = VALID_LENGTH | VALID_TIME

_ROOTS = [Path(__file__).resolve().parents[1],                      # python/cecelia
          Path(__file__).resolve().parents[3] / 'app' / 'src']      # app/src (task runners)


class OmeUnitSymbolTest(unittest.TestCase):

    def test_every_mapped_unit_is_a_legal_ome_symbol(self):
        for src, sym in zarr_utils._OME_XML_UNIT.items():
            with self.subTest(unit=src):
                self.assertIn(sym, VALID, f'{src!r} maps to {sym!r}, not an OME enumeration member')

    def test_the_map_is_idempotent_on_symbols(self):
        """A value already in symbol form must survive a second pass unchanged."""
        for sym in set(zarr_utils._OME_XML_UNIT.values()):
            self.assertEqual(zarr_utils._OME_XML_UNIT.get(sym, sym), sym)

    def test_the_vocabularies_ccid_actually_stores_all_convert(self):
        # What the importer writes into ccid.json, read off the NGFF axes.
        for ngff in ('micrometer', 'nanometer', 'millimeter', 'second', 'minute'):
            with self.subTest(unit=ngff):
                self.assertIn(zarr_utils._OME_XML_UNIT.get(ngff), VALID)

    def test_no_module_assigns_an_ome_unit_without_converting(self):
        """The bypass that shipped: the OME-TIFF export copied ccid.json's 'micrometer' straight
        into PhysicalSizeXUnit, while every other writer converted first."""
        assign = re.compile(r"""(physical_size_[xyz]_unit|time_increment_unit"""
                            r"""|['"](?:PhysicalSize[XYZ]Unit|TimeIncrementUnit)['"])\s*[:=]""")
        offenders = []
        for root in _ROOTS:
            if not root.is_dir():
                continue
            for path in root.rglob('*.py'):
                if 'tests' in path.parts:
                    continue
                src = path.read_text(encoding='utf-8')
                if assign.search(src) and '_OME_XML_UNIT' not in src:
                    offenders.append(str(path.relative_to(root)))
        self.assertFalse(offenders,
                         'these assign an OME-XML unit attribute without going through '
                         f'_OME_XML_UNIT: {offenders}. NGFF/ccid store UDUNITS names '
                         "('micrometer'); OME-XML needs the symbol ('µm'), and an invalid one "
                         'voids the entire metadata block.')
