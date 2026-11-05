"""get_acquisition_knowledge serves the committed acquisition_knowledge.json — and the file holds
together: every cited source exists with a DOI, every symptom says how to check it and what else it
could be, stages are the light path's, and no scope feature claims to be verified by default."""
import unittest

from cecelia_mcp import guidance, server

# The light path's stages, in order — `frontend/src/utils/acquisitionLightPath.ts` (`lightPathStages`)
# and the metadata dialog use the same keys, so a symptom lands at the stage the user is looking at.
LIGHT_PATH_STAGES = ["laser", "power", "scanner", "objective", "sample", "filters",
                     "detectors", "digitisation", "stack"]


class AcquisitionKnowledgeTest(unittest.TestCase):
    def setUp(self):
        self.k = server.get_acquisition_knowledge()

    def test_stages_are_the_light_path_stages_in_order(self):
        self.assertEqual(LIGHT_PATH_STAGES, [s["key"] for s in self.k["stages"]])

    def test_every_symptom_is_checkable_has_alternatives_and_cites_known_sources(self):
        ids = set()
        for st in self.k["stages"]:
            for sym in st["symptoms"]:
                with self.subTest(symptom=sym["id"]):
                    self.assertNotIn(sym["id"], ids)
                    ids.add(sym["id"])
                    for field in ("symptom", "explanation", "check", "alternatives"):
                        self.assertTrue(sym[field].strip(), field)
                    self.assertTrue(sym["sources"])
                    self.assertLessEqual(set(sym["sources"]), set(self.k["sources"]))
        self.assertIn("darker-with-depth", ids)        # the plan's worked example

    def test_offset_advice_only_for_a_clipped_background(self):
        # a high zero fraction alone is no reason to touch the offset: on a photon-counting detector
        # the zeros are voxels with no photon (intensity_utils.floor_stats tells the two apart)
        syms = {sym["id"]: sym for st in self.k["stages"] for sym in st["symptoms"]}
        self.assertIn("background clipped at 0", syms["background-at-zero"]["check"])
        no_photon = syms["zeros-are-no-photon"]
        self.assertIn("photon-counted", no_photon["check"])
        self.assertIn("Never suggest the offset", no_photon["check"])

    def test_every_source_has_a_doi_and_is_cited(self):
        cited = {s for st in self.k["stages"] for sym in st["symptoms"] for s in sym["sources"]}
        for sid, src in self.k["sources"].items():
            with self.subTest(source=sid):
                self.assertTrue(src["doi"].startswith("10."))
                self.assertIn(sid, cited)

    def test_scope_features_sit_at_real_stages_and_start_unverified(self):
        for scope in self.k["scopes"]:
            for f in scope["features"]:
                with self.subTest(system=scope["system"], feature=f["name"]):
                    self.assertIn(f["stage"], LIGHT_PATH_STAGES)
                    self.assertIs(f["verified"], False)

    def test_system_keeps_only_that_scope(self):
        out = server.get_acquisition_knowledge("fv4000")
        self.assertEqual(["FV4000"], [s["system"] for s in out["scopes"]])
        self.assertEqual([], server.get_acquisition_knowledge("LSM 980")["scopes"])

    def test_briefing_compares_settings_within_a_set_first(self):
        # a setting changed between movies is diffed before any optical or biological explanation
        self.assertIn("compare their `acquisition` first", guidance.BRIEFING_GUIDANCE)


if __name__ == "__main__":
    unittest.main()
