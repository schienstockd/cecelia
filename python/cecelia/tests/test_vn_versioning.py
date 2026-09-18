"""VN versioning resolver (``cecelia.utils.vn_versioning``).

Python mirror of the Julia composer in ``app/src/helpers.jl``. Covers:

* the OUTER axis (``_active`` / value_name variant) — resolve, get, keys
* the INNER axis (``_latest`` / version per value_name) — is_versioned_entry, latest, get, keys
* the composers ``versioned_get_field_at`` and ``unversion_value`` — legacy shape, new shape, mixed
* the passthrough contract on the 4 legacy path helpers widened in P1d — ``version`` kwarg is a
  no-op on today's legacy path assembly (P4b rewrites these to grouped-layout paths).
"""
import os
import tempfile
import unittest

import anndata as ad
import numpy as np

from cecelia.utils.label_props_utils import LabelPropsUtils, LabelPropsView
from cecelia.utils.vn_versioning import (
    LATEST_ACTIVE_KEY, LATEST_DEFAULT_VAL, VERSIONED_ACTIVE_KEY, VERSIONED_DEFAULT_VAL,
    is_versioned_entry, resolve_value_name, resolve_version,
    unversion_value, version_get, version_keys, version_latest,
    versioned_active, versioned_get, versioned_get_field, versioned_get_field_at,
    versioned_keys,
)


class OuterAxisTest(unittest.TestCase):
    """The outer axis — value_name variants keyed by ``_active``."""

    def test_versioned_active_default_fallback(self):
        self.assertEqual(versioned_active({}), VERSIONED_DEFAULT_VAL)
        self.assertEqual(versioned_active({"default": "x"}), VERSIONED_DEFAULT_VAL)

    def test_versioned_active_explicit(self):
        d = {"default": "a", "dtype": "b", VERSIONED_ACTIVE_KEY: "dtype"}
        self.assertEqual(versioned_active(d), "dtype")

    def test_versioned_get_defaults_to_active(self):
        d = {"default": "a", "dtype": "b", VERSIONED_ACTIVE_KEY: "dtype"}
        self.assertEqual(versioned_get(d), "b")

    def test_versioned_get_by_name(self):
        d = {"default": "a", "dtype": "b", VERSIONED_ACTIVE_KEY: "dtype"}
        self.assertEqual(versioned_get(d, "default"), "a")
        self.assertIsNone(versioned_get(d, "missing"))

    def test_versioned_get_field_missing_field(self):
        self.assertIsNone(versioned_get_field({}, "filepath"))

    def test_versioned_get_field_bare_scalar_passthrough(self):
        # A non-dict at the field (very legacy) passes through unchanged.
        self.assertEqual(versioned_get_field({"filepath": "old.zarr"}, "filepath"), "old.zarr")

    def test_versioned_get_field_active(self):
        raw = {"filepath": {"default": "a.zarr", "dtype": "b.zarr", VERSIONED_ACTIVE_KEY: "dtype"}}
        self.assertEqual(versioned_get_field(raw, "filepath"), "b.zarr")

    def test_versioned_keys_excludes_active(self):
        d = {"default": "a", "dtype": "b", VERSIONED_ACTIVE_KEY: "dtype"}
        self.assertEqual(sorted(versioned_keys(d)), ["default", "dtype"])

    def test_resolve_value_name_active_fallback(self):
        d = {"default": "a", "dtype": "b", VERSIONED_ACTIVE_KEY: "dtype"}
        self.assertEqual(resolve_value_name(d), "dtype")
        self.assertEqual(resolve_value_name(d, "default"), "default")


class InnerAxisTest(unittest.TestCase):
    """The inner axis — versions per value_name, keyed by ``_latest``."""

    def test_is_versioned_entry(self):
        self.assertTrue(is_versioned_entry({LATEST_ACTIVE_KEY: "v1", "v1": "x"}))
        self.assertFalse(is_versioned_entry({"v1": "x"}))
        self.assertFalse(is_versioned_entry("legacy.zarr"))
        self.assertFalse(is_versioned_entry(["legacy.zarr"]))
        self.assertFalse(is_versioned_entry(None))

    def test_version_latest_default(self):
        self.assertEqual(version_latest({}), LATEST_DEFAULT_VAL)
        self.assertEqual(version_latest({"v1": "x"}), LATEST_DEFAULT_VAL)

    def test_version_latest_explicit(self):
        self.assertEqual(version_latest({"v1": "a", "v2": "b", LATEST_ACTIVE_KEY: "v2"}), "v2")

    def test_version_get_defaults_to_latest(self):
        entry = {"v1": "a", "v2": "b", LATEST_ACTIVE_KEY: "v2"}
        self.assertEqual(version_get(entry), "b")

    def test_version_get_by_name(self):
        entry = {"v1": "a", "v2": "b", LATEST_ACTIVE_KEY: "v2"}
        self.assertEqual(version_get(entry, "v1"), "a")
        self.assertIsNone(version_get(entry, "v99"))

    def test_version_keys_excludes_latest(self):
        entry = {"v1": "a", "v2": "b", LATEST_ACTIVE_KEY: "v2"}
        self.assertEqual(sorted(version_keys(entry)), ["v1", "v2"])


class ComposerTest(unittest.TestCase):
    """``versioned_get_field_at`` and ``unversion_value`` — walk both axes."""

    def test_legacy_shape_bare_scalar_returns_unchanged(self):
        raw = {"filepath": {"default": "old.zarr"}}
        self.assertEqual(versioned_get_field_at(raw, "filepath"), "old.zarr")
        self.assertEqual(versioned_get_field_at(raw, "filepath", "default"), "old.zarr")

    def test_legacy_shape_bare_vector_returns_unchanged(self):
        raw = {"labels": {"default": ["cell.zarr"]}}
        self.assertEqual(versioned_get_field_at(raw, "labels", "default"), ["cell.zarr"])

    def test_new_shape_resolves_latest(self):
        raw = {"filepath": {
            "default": {"v1": "a.zarr", "v2": "b.zarr", LATEST_ACTIVE_KEY: "v2"},
        }}
        self.assertEqual(versioned_get_field_at(raw, "filepath", "default"), "b.zarr")

    def test_new_shape_explicit_version(self):
        raw = {"filepath": {
            "default": {"v1": "a.zarr", "v2": "b.zarr", LATEST_ACTIVE_KEY: "v2"},
        }}
        self.assertEqual(versioned_get_field_at(raw, "filepath", "default", version="v1"), "a.zarr")

    def test_new_shape_missing_version_returns_none(self):
        raw = {"filepath": {
            "default": {"v1": "a.zarr", LATEST_ACTIVE_KEY: "v1"},
        }}
        self.assertIsNone(versioned_get_field_at(raw, "filepath", "default", version="v99"))

    def test_mixed_shape_across_value_names(self):
        # `default` still legacy, `dtype` migrated to new shape — must resolve each correctly.
        raw = {"filepath": {
            "default": "legacy.zarr",
            "dtype": {"v1": "dtype_v1.zarr", "v2": "dtype_v2.zarr", LATEST_ACTIVE_KEY: "v2"},
            VERSIONED_ACTIVE_KEY: "dtype",
        }}
        self.assertEqual(versioned_get_field_at(raw, "filepath"), "dtype_v2.zarr")
        self.assertEqual(versioned_get_field_at(raw, "filepath", "default"), "legacy.zarr")

    def test_unversion_value_legacy_returns_unchanged(self):
        self.assertEqual(unversion_value("old.zarr"), "old.zarr")
        self.assertEqual(unversion_value(["a.zarr", "b.zarr"]), ["a.zarr", "b.zarr"])
        self.assertIsNone(unversion_value(None))

    def test_unversion_value_new_shape_latest(self):
        entry = {"v1": "a.zarr", "v2": "b.zarr", LATEST_ACTIVE_KEY: "v2"}
        self.assertEqual(unversion_value(entry), "b.zarr")

    def test_unversion_value_new_shape_explicit(self):
        entry = {"v1": "a.zarr", "v2": "b.zarr", LATEST_ACTIVE_KEY: "v2"}
        self.assertEqual(unversion_value(entry, "v1"), "a.zarr")

    def test_unversion_value_version_kwarg_is_a_noop_on_legacy(self):
        # Mirrors the Julia P1b test: passing ``version=…`` to a legacy scalar returns it unchanged.
        self.assertEqual(unversion_value("old.zarr", "v99"), "old.zarr")

    def test_resolve_version_new_shape(self):
        entry = {"v1": "a", "v2": "b", LATEST_ACTIVE_KEY: "v2"}
        self.assertEqual(resolve_version(entry), "v2")
        self.assertEqual(resolve_version(entry, "v1"), "v1")

    def test_resolve_version_legacy_always_v1(self):
        # Mirrors the Julia P1b `resolve_version — legacy always resolves to v1` testset.
        self.assertEqual(resolve_version("old.zarr"), LATEST_DEFAULT_VAL)
        self.assertEqual(resolve_version(["a.zarr", "b.zarr"]), LATEST_DEFAULT_VAL)
        self.assertEqual(resolve_version(None), LATEST_DEFAULT_VAL)


class LabelPropsPassthroughTest(unittest.TestCase):
    """The ``version`` kwarg on the widened path helpers is a no-op today (legacy shape)."""

    def setUp(self):
        self.td = tempfile.mkdtemp()

    def _write_view(self, path):
        os.makedirs(os.path.dirname(path), exist_ok=True)
        a = ad.AnnData(np.arange(6, dtype="float32").reshape(3, 2))
        a.obs["label"] = list(range(1, 4))
        a.write_h5ad(path)

    def test_label_props_utils_filepath_version_kwarg_is_a_noop(self):
        u = LabelPropsUtils(self.td, value_name="default")
        legacy = u.label_props_filepath("default")
        with_version = u.label_props_filepath("default", version="v99")
        self.assertEqual(legacy, with_version)
        self.assertTrue(legacy.endswith(os.path.join("labelProps", "default.h5ad")))

    def test_label_props_utils_carries_version(self):
        u = LabelPropsUtils(self.td, value_name="default", version="v3")
        self.assertEqual(u.version, "v3")

    def test_label_props_view_carries_version(self):
        p = os.path.join(self.td, "labelProps", "default.h5ad")
        self._write_view(p)
        v = LabelPropsView(p, version="v2")
        self.assertEqual(v.version, "v2")
        # legacy default construction — version stays None
        v2 = LabelPropsView(p)
        self.assertIsNone(v2.version)


if __name__ == "__main__":
    unittest.main()
