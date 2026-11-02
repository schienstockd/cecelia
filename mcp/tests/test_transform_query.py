"""The gating tools' `transform` argument → the plot routes' per-axis query keys.

A spec the routes cannot read was dropped without a word: a per-axis `{"x": …, "y": …}` plotted both
axes linear, and the stored gate's own `cofactor` key (what gate_plot replies with) was ignored, so the
axis fell back to the data-derived cofactor. Both now map, and anything else is refused."""
import unittest

from cecelia_mcp.gating_views import axis_transform_query


class AxisTransformQuery(unittest.TestCase):
    def test_one_spec_applies_to_both_axes(self):
        self.assertEqual(axis_transform_query({"kind": "asinh", "cof": 5}),
                         {"xt": "asinh", "xcof": 5, "yt": "asinh", "ycof": 5})

    def test_none_is_linear(self):
        self.assertEqual(axis_transform_query(None), {"xt": "linear", "yt": "linear"})

    def test_per_axis_specs(self):
        q = axis_transform_query({"x": {"kind": "log", "floor": 1}, "y": {"kind": "linear"}})
        self.assertEqual(q, {"xt": "log", "xfloor": 1, "yt": "linear"})

    def test_per_axis_missing_axis_is_linear(self):
        self.assertEqual(axis_transform_query({"y": {"kind": "log", "floor": 1}}),
                         {"xt": "linear", "yt": "log", "yfloor": 1})

    def test_stored_cofactor_key_is_accepted(self):
        # a gate's x_transform as gate_plot / get_populations report it, passed back as is
        self.assertEqual(axis_transform_query({"kind": "asinh", "cofactor": 0.3}),
                         {"xt": "asinh", "xcof": 0.3, "yt": "asinh", "ycof": 0.3})

    def test_unknown_parameter_is_refused(self):
        with self.assertRaisesRegex(ValueError, "floor"):
            axis_transform_query({"kind": "asinh", "floor": 1})

    def test_unknown_kind_is_refused(self):
        with self.assertRaisesRegex(ValueError, "sqrt"):
            axis_transform_query({"kind": "sqrt"})

    def test_mixed_shapes_are_refused(self):
        with self.assertRaises(ValueError):
            axis_transform_query({"kind": "log", "x": {"kind": "linear"}})


if __name__ == "__main__":
    unittest.main()
