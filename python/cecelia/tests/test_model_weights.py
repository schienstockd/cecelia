"""`cecelia.utils.model_weights.download` — the weights fetch the model-weights job runs.

No network and no cellpose import: `download` takes the opener, and the destination is a temp dir.
"""
import io
import os
import tempfile
import unittest

from cecelia.utils import model_weights


class _Resp(io.BytesIO):
    def __init__(self, data: bytes, length=None):
        super().__init__(data)
        self.headers = {} if length is None else {"Content-Length": str(length)}


class _Logger:
    def __init__(self):
        self.progress_calls, self.lines = [], []

    def progress(self, n, total):
        self.progress_calls.append((n, total))

    def log(self, msg):
        self.lines.append(msg)


class DownloadTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.dst = os.path.join(self.dir, "models", "cpsam_v2")

    def test_writes_the_file_and_reports_every_chunk_to_the_end(self):
        data = os.urandom(5 * model_weights._CHUNK + 17)
        logger = _Logger()
        model_weights.download("u", self.dst, logger=logger, opener=lambda url: _Resp(data, len(data)))
        with open(self.dst, "rb") as f:
            self.assertEqual(f.read(), data)
        self.assertEqual(len(logger.progress_calls), 6)
        self.assertEqual(logger.progress_calls[-1], (len(data), len(data)))   # the bar completes
        self.assertEqual(os.listdir(os.path.dirname(self.dst)), ["cpsam_v2"])   # no temp left

    def test_a_short_download_leaves_no_file(self):
        # The presence check treats a file on disk as complete weights, so a truncated stream must
        # not leave one behind.
        with self.assertRaises(OSError):
            model_weights.download("u", self.dst, logger=_Logger(),
                                   opener=lambda url: _Resp(b"x" * 10, 100))
        self.assertEqual(os.listdir(os.path.dirname(self.dst)), [])

    def test_no_length_header_still_downloads(self):
        model_weights.download("u", self.dst, logger=_Logger(), opener=lambda url: _Resp(b"abc"))
        with open(self.dst, "rb") as f:
            self.assertEqual(f.read(), b"abc")

    def test_main_without_models_is_a_usage_error(self):
        self.assertEqual(model_weights.main([]), 2)


if __name__ == "__main__":
    unittest.main()
