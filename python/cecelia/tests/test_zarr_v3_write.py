"""Our OWN writers can emit zarr v3, and a derived store inherits its source's format.

Phase 3 of docs/todo/ZARR_V3_PLAN.md. Two things are pinned here:

1. **The codec decision survives the format change.** v2 takes `compressor=<numcodecs codec>`, v3 takes
   `codecs=[pipeline]`. Passing the wrong shape is either an error or — worse — silently ignored, which
   is precisely how three unintended codecs ended up on disk before `store_compressor` existed. Both
   shapes are derived from the SAME table, and that is asserted.

2. **Derived stores inherit the source's format** (D9). Every writer has to handle v3 — corrections,
   crop, copy, labels, branching, rechunk — but none may grow its own format param, or a v2 original
   could acquire a v3 drift-corrected variant. That is the same inconsistency
   `bf2raw_compression_flags` exists to prevent.

Round-tripped through the Phase 1 readers rather than checked against hand-written expectations: a
store we write and cannot read back is the failure that matters.
"""
import os
import shutil
import tempfile
import unittest

import numpy as np

from cecelia.utils import zarr_utils

_OME = """<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06"><Image ID="Image:0"><Pixels
    ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16" SizeT="{t}" SizeC="1" SizeZ="{z}"
    SizeY="{y}" SizeX="{x}" PhysicalSizeX="0.5" PhysicalSizeXUnit="µm"
    PhysicalSizeY="0.5" PhysicalSizeYUnit="µm" PhysicalSizeZ="2.0" PhysicalSizeZUnit="µm">
    <Channel ID="Channel:0:0" SamplesPerPixel="1"/><MetadataOnly/></Pixels></Image></OME>"""


def _fixture(shape=(2, 1, 1, 16, 16), seed=0):
    import ome_types
    from cecelia.utils.dim_utils import DimUtils
    t, c, z, y, x = shape
    du = DimUtils(ome_types.from_xml(_OME.format(t=t, z=z, y=y, x=x)), use_channel_axis=True)
    du.calc_image_dimensions(shape)
    # noise, not a gradient: a mis-placed or mis-decoded value shows up rather than blending in
    arr = np.random.default_rng(seed).integers(1, 60000, size=shape, dtype=np.uint16)
    # dask, because that is what the real writers hand `create_multiscales` (the corrections build a
    # dask graph); the ndarray branch needs a `reference_zarr` to take its chunking from.
    import dask.array as da
    return da.from_array(arr, chunks=(1, 1, 1) + shape[3:]), arr, du


class CodecShapePerFormatTest(unittest.TestCase):
    def test_v2_and_v3_carry_the_same_decision_in_different_shapes(self):
        self.assertEqual(['compressor'], list(zarr_utils._codec_kwargs('image', 2)))
        self.assertEqual(['compressors'], list(zarr_utils._codec_kwargs('image', 3)))
        # `compressors` PLURAL is the v3 kwarg; `codecs` is silently rejected by create_array
        self.assertEqual(['compressors', 'shards'],
                         list(zarr_utils._codec_kwargs('image', 3, shards=(1, 1, 1, 64, 64))))
        # v2 has no sharding — dropped rather than raising, so a caller need not branch on format
        self.assertEqual(['compressor'], list(zarr_utils._codec_kwargs('image', 2, shards=(1, 1, 1, 64, 64))))

    def test_the_v3_pipeline_matches_the_configured_choice(self):
        spec = zarr_utils.IMAGE_COMPRESSOR_CHOICES[zarr_utils.image_compressor_name()]
        codecs = zarr_utils.store_codecs('image')
        self.assertEqual(1, len(codecs), 'compression codecs only — bytes/serializer is not one')
        blosc = codecs[-1]
        self.assertEqual(spec['cname'], str(blosc.cname.value if hasattr(blosc.cname, 'value') else blosc.cname))
        self.assertEqual(spec['clevel'], blosc.clevel)

    def test_labels_still_get_the_label_codec_in_v3(self):
        # labels are plain zstd, NOT blosc — the opposite answer to images, for a measured reason.
        # Inheriting the FORMAT must not drag the image codec along with it.
        names = [type(c).__name__ for c in zarr_utils.store_codecs('labels')]
        self.assertIn('ZstdCodec', names)
        self.assertNotIn('BloscCodec', names)

    def test_an_unknown_kind_is_rejected_in_both_shapes(self):
        with self.assertRaises(ValueError):
            zarr_utils.store_codecs('nope')
        with self.assertRaises(ValueError):
            zarr_utils.store_compressor('nope')


class WriteBothFormatsTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.mkdtemp()
        self.dark, self.arr, self.du = _fixture()

    def tearDown(self):
        shutil.rmtree(self.tmp, ignore_errors=True)

    def _write(self, name, **kw):
        p = os.path.join(self.tmp, name)
        zarr_utils.create_multiscales(self.dark, p, dim_utils=self.du, nscales=1, **kw)
        return p

    def test_writes_and_reads_back_both_formats(self):
        for fmt in (2, 3):
            p = self._write(f'out{fmt}.ome.zarr', zarr_format=fmt)
            self.assertEqual(fmt, zarr_utils.store_format(p), f'v{fmt} store_format')
            self.assertEqual(['t', 'c', 'z', 'y', 'x'], zarr_utils.read_axes(p), f'v{fmt} axes')
            levels, _ = zarr_utils.open_as_zarr(p)
            np.testing.assert_array_equal(self.arr, np.asarray(levels[0][:]))

    def test_calibration_survives_in_both(self):
        # the read side returning None here is the silent failure the whole plan is about
        got = [zarr_utils.read_scale(self._write(f'c{f}.ome.zarr', zarr_format=f)) for f in (2, 3)]
        for scale in got:
            self.assertIsNotNone(scale)
        np.testing.assert_allclose(np.asarray(got[0], float), np.asarray(got[1], float))
        self.assertAlmostEqual(0.5, float(got[1][-1]))     # a real value, not the 1.0 fallback

    def test_default_is_v2_when_there_is_nothing_to_inherit_from(self):
        # every store was v2 before bioformats2raw 0.12; the default must not silently change
        self.assertEqual(2, zarr_utils.store_format(self._write('def.ome.zarr')))

    def test_the_v3_group_declares_ngff_0_5_under_ome(self):
        import zarr
        p = self._write('v3attrs.ome.zarr', zarr_format=3)
        attrs = dict(zarr.open_group(p, mode='r').attrs)
        self.assertIn('ome', attrs, 'NGFF 0.5 nests under `ome`')
        self.assertEqual('0.5', attrs['ome'].get('version'))
        self.assertIn('multiscales', attrs['ome'])
        # and the read-side unwrap agrees with what was written
        self.assertIn('multiscales', zarr_utils.ngff_attrs(attrs))

    def test_a_sharded_v3_store_round_trips_and_reports_its_shard(self):
        # Sharding is the whole reason for going to v3 (far fewer files). It must survive the write,
        # be readable, and be REPORTED — the metadata modal distinguishes chunk from shard, and the
        # two are easy to swap.
        p = self._write('sharded.ome.zarr', zarr_format=3, shards=(1, 1, 1, 16, 16))
        self.assertEqual(3, zarr_utils.store_format(p))
        np.testing.assert_array_equal(self.arr, np.asarray(zarr_utils.open_as_zarr(p)[0][0][:]))
        import json
        with open(os.path.join(p, '0', 'zarr.json'), encoding='utf-8') as fh:
            meta = json.load(fh)
        self.assertTrue([c for c in meta['codecs'] if c['name'] == 'sharding_indexed'],
                        'shards= did not produce a sharding codec')

    def test_sharding_is_ignored_for_v2_rather_than_failing(self):
        # a derived writer inherits its format and should not have to branch on it to pass shards
        p = self._write('v2shard.ome.zarr', zarr_format=2, shards=(1, 1, 1, 16, 16))
        self.assertEqual(2, zarr_utils.store_format(p))
        np.testing.assert_array_equal(self.arr, np.asarray(zarr_utils.open_as_zarr(p)[0][0][:]))


class InheritFormatTest(unittest.TestCase):
    """D9: a derived store takes its format from its source, and only the import chooses."""

    def setUp(self):
        self.tmp = tempfile.mkdtemp()
        self.dark, self.arr, self.du = _fixture()

    def tearDown(self):
        shutil.rmtree(self.tmp, ignore_errors=True)

    def test_store_encoding_of_reports_the_source_format(self):
        for fmt in (2, 3):
            p = os.path.join(self.tmp, f'src{fmt}.ome.zarr')
            zarr_utils.create_multiscales(self.dark, p, dim_utils=self.du, nscales=1, zarr_format=fmt)
            self.assertEqual({'zarr_format': fmt}, zarr_utils.store_encoding_of(p))

    def test_a_missing_or_unreadable_source_falls_back_to_v2_rather_than_raising(self):
        # a derived write must not fail because a source's metadata could not be parsed
        self.assertEqual({'zarr_format': 2}, zarr_utils.store_encoding_of(os.path.join(self.tmp, 'nope')))
        self.assertEqual({'zarr_format': 2}, zarr_utils.store_encoding_of(None))
        self.assertEqual({'zarr_format': 2}, zarr_utils.store_encoding_of(''))

    def test_the_streaming_writer_inherits_too(self):
        # open_multiscales_for_writing is the path the correction tasks use — the main way a derived
        # store could end up in a different format from the image it came from
        import zarr
        for fmt in (2, 3):
            src = os.path.join(self.tmp, f'in{fmt}.ome.zarr')
            zarr_utils.create_multiscales(self.dark, src, dim_utils=self.du, nscales=1, zarr_format=fmt)
            out = os.path.join(self.tmp, f'streamed{fmt}.ome.zarr')
            grp, level0, _ = zarr_utils.open_multiscales_for_writing(
                out, self.arr.shape, self.arr.dtype, self.du, nscales=1, reference_zarr=src)
            level0[:] = self.arr
            self.assertEqual(fmt, int(grp.metadata.zarr_format), f'inherited from v{fmt} source')
            self.assertEqual(fmt, zarr_utils.store_format(out))
            np.testing.assert_array_equal(self.arr, np.asarray(zarr_utils.open_as_zarr(out)[0][0][:]))

    def test_an_open_zarr_array_is_accepted_as_the_reference(self):
        # cropImage_run passes `reference_zarr=im_dat[0]` — an OPEN ARRAY, not a path. Handling only
        # paths made that caller fall back to v2 silently, writing a v2 crop of a v3 image.
        for fmt in (2, 3):
            src = os.path.join(self.tmp, f'open{fmt}.ome.zarr')
            zarr_utils.create_multiscales(self.dark, src, dim_utils=self.du, nscales=1, zarr_format=fmt)
            level0 = zarr_utils.open_as_zarr(src)[0][0]
            self.assertEqual({'zarr_format': fmt}, zarr_utils.store_encoding_of(level0),
                             f'open v{fmt} array as reference')

    def test_a_crop_style_derived_write_keeps_the_source_format(self):
        # the whole D9 chain through create_multiscales' own reference_zarr path
        for fmt in (2, 3):
            src = os.path.join(self.tmp, f'cropsrc{fmt}.ome.zarr')
            zarr_utils.create_multiscales(self.dark, src, dim_utils=self.du, nscales=1, zarr_format=fmt)
            level0 = zarr_utils.open_as_zarr(src)[0][0]
            out = os.path.join(self.tmp, f'cropped{fmt}.ome.zarr')
            zarr_utils.create_multiscales(self.dark, out, dim_utils=self.du, nscales=1,
                                          reference_zarr=level0)
            self.assertEqual(fmt, zarr_utils.store_format(out), f'crop of a v{fmt} source')


class CalibrationRestampTest(unittest.TestCase):
    """A calibration RE-stamp must land where the store actually keeps its NGFF attributes.

    This is the dangerous one. Writing `attrs['multiscales']` on a v3 store puts it at the top level,
    where no reader looks — the store keeps its OLD multiscales and the update is silently ignored. The
    numbers appear written and are not there (CLAUDE.md → *Calibration — three copies, one stamp*).
    """

    def setUp(self):
        self.tmp = tempfile.mkdtemp()
        self.dark, self.arr, self.du = _fixture()

    def tearDown(self):
        shutil.rmtree(self.tmp, ignore_errors=True)

    def test_restamp_is_readable_back_in_both_formats(self):
        for fmt in (2, 3):
            p = os.path.join(self.tmp, f'restamp{fmt}.ome.zarr')
            zarr_utils.create_multiscales(self.dark, p, dim_utils=self.du, nscales=1, zarr_format=fmt)
            ok = zarr_utils.set_ngff_axes(p, ['t', 'c', 'z', 'y', 'x'],
                                          scale={'t': 7.0, 'c': 1.0, 'z': 9.0, 'y': 0.25, 'x': 0.25},
                                          units={'t': 'second', 'z': 'micrometer',
                                                 'y': 'micrometer', 'x': 'micrometer'})
            self.assertTrue(ok, f'v{fmt} restamp reported failure')
            scale = zarr_utils.read_scale(p)
            self.assertIsNotNone(scale, f'v{fmt}: restamped scale is unreadable')
            self.assertAlmostEqual(0.25, float(scale[-1]), places=6,
                                   msg=f'v{fmt}: restamp did not land where readers look')
            self.assertEqual(7.0, zarr_utils.read_time_increment(p), f'v{fmt} t interval')

    def test_a_v3_restamp_leaves_no_stale_top_level_copy(self):
        # the specific silent failure: a top-level `multiscales` on a v3 store is invisible to readers,
        # so a writer that put it there would leave the OLD values in force under `ome`
        import zarr
        p = os.path.join(self.tmp, 'v3clean.ome.zarr')
        zarr_utils.create_multiscales(self.dark, p, dim_utils=self.du, nscales=1, zarr_format=3)
        zarr_utils.set_ngff_axes(p, ['t', 'c', 'z', 'y', 'x'],
                                 scale={'t': 1.0, 'c': 1.0, 'z': 1.0, 'y': 0.75, 'x': 0.75})
        attrs = dict(zarr.open_group(zarr_utils.series_base(p), mode='r').attrs)
        self.assertIn('ome', attrs)
        self.assertNotIn('multiscales', attrs, 'v3 must not carry a top-level multiscales copy')
        self.assertAlmostEqual(0.75, float(zarr_utils.read_scale(p)[-1]), places=6)


class ValidBoxIsFormatAgnosticTest(unittest.TestCase):
    """`validBox` needs NO v3 branch, and this pins why so nobody adds one.

    It lives under `CECELIA_ATTR` — a cecelia-PRIVATE namespace, not an NGFF attribute. The `ome`
    nesting that NGFF 0.5 introduced applies to the spec's own keys (`multiscales`, `omero`), not to
    arbitrary custom ones, and `zarr-python`'s `Group.attrs` already hides `.zattrs` vs
    `zarr.json`→`attributes`. So writer and reader agree on the same key in both formats.

    Worth a test rather than a comment because the obvious-looking "fix" — routing this through
    `write_ngff_attrs` like the multiscales writers — would bury a private key inside `ome`, where it
    is neither NGFF nor findable by the reader.
    """

    def setUp(self):
        self.tmp = tempfile.mkdtemp()
        self.dark, self.arr, self.du = _fixture()

    def tearDown(self):
        shutil.rmtree(self.tmp, ignore_errors=True)

    def _store(self, fmt):
        p = os.path.join(self.tmp, f'vb{fmt}.ome.zarr')
        zarr_utils.create_multiscales(self.dark, p, dim_utils=self.du, nscales=1, zarr_format=fmt)
        return p

    def test_a_static_box_round_trips_in_both_formats(self):
        for fmt in (2, 3):
            p = self._store(fmt)
            zarr_utils.write_valid_box(p, ['Y', 'X'], {'Y': (2, 10), 'X': (3, 11)})
            self.assertEqual({'Y': (2, 10), 'X': (3, 11)}, zarr_utils.read_valid_box(p, level=0),
                             f'v{fmt} static valid box')

    def test_a_per_timepoint_box_round_trips_in_both_formats(self):
        # the drift case: the valid region moves per frame, and a consumer skips what the writer left
        # empty. A box that silently failed to persist would make consumers trust padding as data.
        for fmt in (2, 3):
            p = self._store(fmt)
            zarr_utils.write_valid_box(p, ['Y', 'X'],
                                       {0: {'Y': (1, 9), 'X': (0, 8)}, 1: {'Y': (2, 10), 'X': (1, 9)}})
            self.assertEqual({'Y': (1, 9), 'X': (0, 8)},
                             zarr_utils.read_valid_box(p, level=0, timepoint=0), f'v{fmt} t=0')
            self.assertEqual({'Y': (2, 10), 'X': (1, 9)},
                             zarr_utils.read_valid_box(p, level=0, timepoint=1), f'v{fmt} t=1')

    def test_the_private_key_stays_out_of_the_ome_namespace(self):
        import zarr
        p = self._store(3)
        zarr_utils.write_valid_box(p, ['Y', 'X'], {'Y': (0, 4), 'X': (0, 4)})
        attrs = dict(zarr.open_group(zarr_utils.series_base(p), mode='r').attrs)
        self.assertIn(zarr_utils.CECELIA_ATTR, attrs, 'private attrs belong at the top level')
        self.assertNotIn(zarr_utils.CECELIA_ATTR, attrs.get('ome', {}))


if __name__ == '__main__':
    unittest.main()
