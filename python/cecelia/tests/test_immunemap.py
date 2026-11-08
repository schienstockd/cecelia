"""Tests for the Immunemap client (`cecelia.utils.immunemap`).

The fixtures in `test-data/immunemap/` are real `api.immunemap.org` responses (experiment 63 and
acquisition 309, fetched 2026-11-08), not shapes written to suit the parser. HTTP is replayed from
them; nothing here touches the network.

Run with `pixi run test-py`.
"""
import hashlib
import io
import json
import os
import shutil
import tempfile
import unittest
import urllib.error

import cecelia.utils.immunemap as im

_FIX = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'test-data',
                                     'immunemap'))


def _load(name):
    with open(os.path.join(_FIX, name), encoding='utf-8') as f:
        return json.load(f)


class ParseRefTest(unittest.TestCase):
    def test_forms(self):
        self.assertEqual(im.parse_experiment_ref(63), 63)
        self.assertEqual(im.parse_experiment_ref(' 63 '), 63)
        self.assertEqual(im.parse_experiment_ref(
            'https://app.immunemap.org/experiment-public-view?id=63'), 63)
        self.assertEqual(im.parse_experiment_ref('https://api.immunemap.org/experiment/63'), 63)

    def test_rejects_garbage(self):
        with self.assertRaises(ValueError):
            im.parse_experiment_ref('https://app.immunemap.org/')


class ChannelNamesTest(unittest.TestCase):
    def test_real_acquisition(self):
        # The fluorophore arrives as ' Rhodamine' (leading space) — stripped, not kept.
        acq = _load('acquisition_309.json')
        n = acq['video_details']['size']['number_of_channels']
        self.assertEqual(im.channel_names(acq['cell_stainings'], n),
                         ['Microspheres (Rhodamine)', 'B cells (GFP)', 'Collagen (Autofluorescent)'])

    def test_virtual_channels_beyond_the_real_count_are_ignored(self):
        st = [{'class': 'T cells', 'fluorophore': 'CFP',
               'channels': [{'selected': False}] * 4 + [{'selected': True}]}]
        self.assertEqual(im.channel_names(st, 3), ['Channel 0', 'Channel 1', 'Channel 2'])

    def test_shared_and_multi_selected_channels(self):
        st = [{'class': 'Neutrophils', 'fluorophore': 'GFP',
               'channels': [{'selected': True}, {'selected': True}]},
              {'class': 'Vessels', 'fluorophore': '',
               'channels': [{'selected': False}, {'selected': True}]}]
        self.assertEqual(im.channel_names(st, 3),
                         ['Neutrophils (GFP)', 'Neutrophils (GFP) + Vessels', 'Channel 2'])


class VideoRecordTest(unittest.TestCase):
    def test_real_acquisition(self):
        acq = _load('acquisition_309.json')
        g = {'id': 101, 'name': 'Control, before immunization'}
        r = im.video_record(acq, g)
        self.assertEqual(r['videoUid'], '2JBABTBNRF8MR978J016GM606A')
        self.assertEqual(r['md5'], 'da279b07eb1994d5b09dea2ff77da1c1')
        self.assertEqual(r['sizeBytes'], 575530182)
        self.assertEqual(r['condition'], 'Control, before immunization')
        self.assertEqual(r['name'], 'Obino-Movie1-SteadyState')
        self.assertAlmostEqual(r['intervalSec'], 30.0, places=3)
        self.assertEqual(r['intervalFromTimeStepsSec'], 30.0)
        self.assertEqual(r['spacingXYZ'], [1.799537109375, 1.799537109375, 4])
        self.assertEqual(r['nCuratedTracks'], 151)
        self.assertEqual(r['dois'], ['10.1016/j.celrep.2018.11.052'])
        self.assertTrue(r['importable'])

    def test_deleted_video_is_not_importable(self):
        acq = _load('acquisition_309.json')
        acq['video_details']['deleted'] = True
        r = im.video_record(acq, {'id': 1, 'name': 'g'})
        self.assertFalse(r['importable'])
        self.assertIn('deleted', r['reason'])


class ManifestTest(unittest.TestCase):
    def test_real_experiment_with_private_videos(self):
        exp = _load('experiment_63.json')
        acq = _load('acquisition_309.json')

        def fetch(path):
            if path == '/experiment/63':
                return exp
            if path == '/acquisition/309':
                return acq
            raise im.ImmunemapError(path, status=401)

        m = im.experiment_manifest('https://app.immunemap.org/experiment-public-view?id=63',
                                   fetch=fetch, pause=0)
        self.assertEqual(m['experiment']['id'], 63)
        self.assertEqual(m['experiment']['dois'], ['10.1016/j.celrep.2018.11.052'])
        self.assertEqual([g['id'] for g in m['groups']], [101, 102, 103, 104, 105])
        self.assertEqual(m['totals']['videos'], 15)
        self.assertEqual(m['totals']['importable'], 1)
        self.assertEqual(m['totals']['bytes'], 575530182)
        private = [v for v in m['videos'] if not v['importable']]
        self.assertEqual(len(private), 14)
        self.assertTrue(all(v['reason'] == 'not public' for v in private))
        # every row knows its condition, importable or not
        self.assertEqual({v['condition'] for v in m['videos']}, {g['name'] for g in m['groups']})


class IntervalTest(unittest.TestCase):
    def test_short_or_bad(self):
        self.assertIsNone(im.interval_from_time_steps([]))
        self.assertIsNone(im.interval_from_time_steps(['2013-01-01T00:00:00+0000']))
        self.assertIsNone(im.interval_from_time_steps(['nope', 'nope']))


class SpacingMismatchTest(unittest.TestCase):
    def test_tolerance(self):
        self.assertEqual(im.spacing_mismatch([1.7995, 1.7995, 4.0], [1.799537109375] * 2 + [4]), [])
        self.assertEqual(im.spacing_mismatch([0.45, 0.45, 4.0], [1.8, 1.8, 4.0]), ['x', 'y'])
        self.assertEqual(im.spacing_mismatch(None, [1.8, 1.8, 4.0]), [])


class _FakeResponse(io.BytesIO):
    def __init__(self, data, status):
        super().__init__(data)
        self.status = status

    def __enter__(self):
        return self

    def __exit__(self, *a):
        self.close()


def _server(payload, fail_after=None, honour_range=True):
    """An opener serving `payload`, optionally dropping the connection after `fail_after` bytes once."""
    calls = []

    def opener(url, headers=None, timeout=None):
        rng = (headers or {}).get('Range')
        start = int(rng.split('=')[1].rstrip('-')) if (rng and honour_range) else 0
        calls.append(start)
        body = payload[start:]
        if fail_after is not None and len(calls) == 1:
            class Broken(_FakeResponse):
                def read(self, n=-1):
                    if self.tell() >= fail_after:
                        raise ConnectionError('dropped')
                    return super().read(min(n, fail_after - self.tell()) if n > 0 else n)
            return Broken(body, 200)
        return _FakeResponse(body, 206 if start else 200)
    return opener, calls


class DownloadTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.mkdtemp()
        self.dest = os.path.join(self.tmp, 'v.ims')
        self.payload = os.urandom(50_000)
        self.md5 = hashlib.md5(self.payload).hexdigest()

    def tearDown(self):
        shutil.rmtree(self.tmp, ignore_errors=True)

    def test_resumes_after_a_dropped_connection(self):
        opener, calls = _server(self.payload, fail_after=20_000)
        got = im.download('u', self.dest, size=len(self.payload), md5=self.md5, chunk=4096,
                          backoff=0, opener=opener)
        self.assertEqual(got, self.md5)
        self.assertEqual(calls[0], 0)
        self.assertEqual(calls[1], 20_000)            # second request resumed, not restarted
        self.assertFalse(os.path.exists(self.dest + '.partial'))

    def test_server_ignoring_range_restarts_cleanly(self):
        with open(self.dest + '.partial', 'wb') as f:
            f.write(self.payload[:1000])
        opener, _ = _server(self.payload, honour_range=False)
        self.assertEqual(im.download('u', self.dest, size=len(self.payload), md5=self.md5,
                                     backoff=0, opener=opener), self.md5)
        with open(self.dest, 'rb') as f:
            self.assertEqual(f.read(), self.payload)

    def test_wrong_md5_fails_and_leaves_no_file(self):
        opener, _ = _server(self.payload)
        with self.assertRaises(im.ImmunemapError):
            im.download('u', self.dest, size=len(self.payload), md5='0' * 32, backoff=0,
                        opener=opener)
        self.assertFalse(os.path.exists(self.dest))
        self.assertFalse(os.path.exists(self.dest + '.partial'))

    def test_verified_file_is_reused_without_a_request(self):
        with open(self.dest, 'wb') as f:
            f.write(self.payload)

        def opener(*a, **k):
            raise AssertionError('should not request')
        self.assertEqual(im.download('u', self.dest, md5=self.md5, opener=opener), self.md5)

    def test_private_video_is_final(self):
        def opener(url, headers=None, timeout=None):
            raise urllib.error.HTTPError(url, 401, 'Unauthorized', {}, None)
        with self.assertRaises(im.ImmunemapError) as cm:
            im.download('u', self.dest, backoff=0, opener=opener)
        self.assertEqual(cm.exception.status, 401)


if __name__ == '__main__':
    unittest.main()
