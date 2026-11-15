"""RESULT-REVIEW: result-30e77572

A sweep's runs go out together unless the spec says `sweep.sequential`. Tasks that add their named columns
to the image's label-props table load the whole `.h5ad`, add a column and save it back
(`LabelPropsView.add_obs(...).save()`, no lock), so two runs at once keep only the last one's columns:
a `clusters.r0.5` column vanishes with no error when `clusters.r1.0` saves over it. Every sweepable task
that writes label props must therefore declare `sequential`. The Julia sweep ratchet only catches the
plain-text-output case (HMM), not a namespaced output that still writes into label props.
"""
import json
import unittest
from pathlib import Path

TASKS = Path(__file__).resolve().parents[3] / "app" / "src" / "tasks"

# Sweepable tasks whose runs write columns INTO label props (the writer each one ends in).
LABEL_PROPS_WRITERS = {
    "behaviour/hmm_states.json": "hmm_states.jl: label_props(props) |> add_obs |> save!",
    "spatialAnalysis/detectAggregates.json": "detectAggregates.jl: label_props |> add_obs |> save!",
    "clustPops/cluster.json": "clustering_utils.split_back_and_write: LabelPropsView.add_obs().save()",
    "clustTracks/cluster.json": "clustering_utils.split_back_and_write",
    "clustRegions/cluster.json": "clustering_utils.split_back_and_write",
}


class LabelPropsSweepsAreSequential(unittest.TestCase):
    def test_label_props_writers_sweep_one_after_another(self):
        for rel, writer in LABEL_PROPS_WRITERS.items():
            with self.subTest(spec=rel):
                spec = json.loads((TASKS / rel).read_text(encoding="utf-8"))
                sweep = spec.get("sweep")
                self.assertIsNotNone(sweep, f"{rel}: no longer sweepable — drop it from this list")
                self.assertIs(sweep.get("sequential"), True,
                              f"{rel} writes label props ({writer}): set sweep.sequential")


if __name__ == "__main__":
    unittest.main()
