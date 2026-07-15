"""Unit tests for the read-only Cecelia API client — the observer's no-mutation guarantee.

Pure stdlib (unittest + mock); urllib is mocked so no server is needed. These pin the allow-list,
URL/param building, body shaping, and error mapping — the logic that keeps the observer read-only.
"""
import io
import json
import unittest
import urllib.error
from unittest import mock

from cecelia_mcp.client import ALLOWED_ROUTES, ApiError, CeceliaClient, DisallowedRoute


class _FakeResp:
    def __init__(self, payload):
        self._data = json.dumps(payload).encode()

    def read(self):
        return self._data

    def __enter__(self):
        return self

    def __exit__(self, *a):
        return False


def _patch_urlopen(payload=None):
    return mock.patch(
        "cecelia_mcp.client.urllib.request.urlopen",
        return_value=_FakeResp({} if payload is None else payload),
    )


class ClientTest(unittest.TestCase):
    def setUp(self):
        self.c = CeceliaClient(base_url="http://x:8080")

    def test_allowlist_blocks_unknown_routes(self):
        # a mutating route that exists in the API must NOT be reachable through the client
        with self.assertRaises(DisallowedRoute):
            self.c._request("POST", "/api/images/delete", body={"x": 1})
        with self.assertRaises(DisallowedRoute):
            self.c._request("GET", "/api/gating/save")

    def test_append_is_the_only_write(self):
        writes = sorted((m, p) for (m, p) in ALLOWED_ROUTES if m != "GET")
        self.assertEqual(writes, [("POST", "/api/lablog/append")])

    def test_list_images_builds_url(self):
        with _patch_urlopen({"images": []}) as u:
            self.c.list_images("proj1")
        req = u.call_args[0][0]
        self.assertEqual(req.method, "GET")
        self.assertIn("/api/images?projectUid=proj1", req.full_url)

    def test_task_log_encodes_all_params(self):
        with _patch_urlopen({"exists": False, "content": ""}) as u:
            self.c.get_task_log("p", "img1", "segment.cellpose")
        url = u.call_args[0][0].full_url
        self.assertIn("imageUid=img1", url)
        self.assertIn("fun=segment.cellpose", url)

    def test_task_history_drops_none_limit(self):
        with _patch_urlopen({"history": []}) as u:
            self.c.get_task_history("p")
        self.assertNotIn("limit", u.call_args[0][0].full_url)
        with _patch_urlopen({"history": []}) as u:
            self.c.get_task_history("p", limit=5)
        self.assertIn("limit=5", u.call_args[0][0].full_url)

    def test_append_posts_json_body(self):
        with _patch_urlopen({"ok": True}) as u:
            self.c.append_lab_log("p", "Claude", ["hello", "world"])
        req = u.call_args[0][0]
        self.assertEqual(req.method, "POST")
        self.assertEqual(req.get_header("Content-type"), "application/json")
        self.assertEqual(
            json.loads(req.data.decode()),
            {"projectUid": "p", "author": "Claude", "lines": ["hello", "world"]},
        )

    def test_http_error_becomes_apierror_with_detail(self):
        err = urllib.error.HTTPError(
            "http://x", 404, "nf", {}, io.BytesIO(json.dumps({"error": "Project not found"}).encode())
        )
        with mock.patch("cecelia_mcp.client.urllib.request.urlopen", side_effect=err):
            with self.assertRaises(ApiError) as ctx:
                self.c.list_images("nope")
        self.assertEqual(ctx.exception.status, 404)
        self.assertIn("Project not found", ctx.exception.message)

    def test_unreachable_api_becomes_apierror(self):
        with mock.patch(
            "cecelia_mcp.client.urllib.request.urlopen",
            side_effect=urllib.error.URLError("refused"),
        ):
            with self.assertRaises(ApiError) as ctx:
                self.c.read_lab_log("p")
        self.assertEqual(ctx.exception.status, 0)
        self.assertIn("cannot reach Cecelia API", ctx.exception.message)


if __name__ == "__main__":
    unittest.main()
