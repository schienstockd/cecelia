"""get_acquisition_knowledge serves the committed acquisition_knowledge/ directory — and the files hold
together: every technology's symptoms say how to check them and what else they could be and cite
sources that exist with a DOI; every microscope profile names a known technology and a stage of it,
and no feature claims to be verified by default. A new technology or microscope file is checked by
these tests without editing them (docs/todo/MICROSCOPE_PROFILES_PLAN.md)."""
import json
import os
import re
import unittest
from pathlib import Path

from cecelia_mcp import guidance, server

# Task QC finding codes: the keys of QC_TEXT, where every finding's text lives.
_QC_TEXT = Path(__file__).resolve().parents[2] / "app" / "src" / "qc" / "text.jl"
_DIR = Path(server._ACQUISITION_KNOWLEDGE_DIR)

# The two-photon light path's stages, in order — `frontend/src/utils/acquisitionLightPath.ts`
# (`lightPathStages`) and the metadata dialog use the same keys, so a symptom lands at the stage the
# user is looking at. Other technologies have no diagram yet; their stages are their own file's.
TWO_PHOTON_STAGES = ["laser", "power", "scanner", "objective", "sample", "filters",
                     "detectors", "digitisation", "stack"]


def _load(kind):
    return [json.loads(p.read_text(encoding="utf-8")) for p in sorted((_DIR / kind).glob("*.json"))]


TECHS = server.acquisition_technologies()   # resolved: `extends` applied
SCOPES = _load("scopes")
MODES = _load("modes")
SOURCES = json.loads((_DIR / "sources.json").read_text(encoding="utf-8"))


class AcquisitionKnowledgeFilesTest(unittest.TestCase):
    def test_two_photon_stages_are_the_light_path_stages_in_order(self):
        self.assertEqual(TWO_PHOTON_STAGES, [s["key"] for s in TECHS["two-photon"]["stages"]])

    def test_known_technologies(self):
        self.assertLessEqual({"two-photon", "confocal", "airyscan", "widefield"}, set(TECHS))

    def test_a_variant_states_only_what_differs(self):
        conf, ary = TECHS["confocal"], TECHS["airyscan"]
        keys = [st["key"] for st in ary["stages"]]
        self.assertIn("processing", keys)
        self.assertNotIn("processing", [st["key"] for st in conf["stages"]])
        shared = {st["key"]: st for st in conf["stages"]}
        self.assertIs(next(st for st in ary["stages"] if st["key"] == "filters"), shared["filters"])
        raw = json.loads((_DIR / "technologies" / "airyscan.json").read_text(encoding="utf-8"))
        self.assertNotIn("filters", [st["key"] for st in raw["stages"]])   # inherited, not copied

    def test_every_symptom_is_checkable_has_alternatives_and_cites_known_sources(self):
        for name, t in TECHS.items():
            ids = set()
            self.assertTrue(t["summary"].strip() and t["status"].strip(), name)
            for st in t["stages"]:
                for sym in st["symptoms"]:
                    with self.subTest(technology=name, symptom=sym["id"]):
                        self.assertNotIn(sym["id"], ids)
                        ids.add(sym["id"])
                        for field in ("symptom", "explanation", "check", "alternatives"):
                            self.assertTrue(sym[field].strip(), field)
                        self.assertTrue(sym["sources"])
                        self.assertLessEqual(set(sym["sources"]), set(SOURCES))
        self.assertIn("darker-with-depth", {s["id"] for st in TECHS["two-photon"]["stages"] for s in st["symptoms"]})

    def test_offset_advice_only_for_a_clipped_background(self):
        # a high zero fraction alone is no reason to touch the offset: on a photon-counting detector
        # the zeros are voxels with no photon (intensity_utils.floor_stats tells the two apart)
        syms = {sym["id"]: sym for st in TECHS["two-photon"]["stages"] for sym in st["symptoms"]}
        self.assertIn("background clipped at 0", syms["background-at-zero"]["check"])
        no_photon = syms["zeros-are-no-photon"]
        self.assertIn("photon-counted", no_photon["check"])
        self.assertIn("Never suggest the offset", no_photon["check"])

    def test_every_mode_symptom_is_checkable_cites_known_sources_and_real_qc_codes(self):
        known = set(re.findall(r'^\s*"([\w.]+)"\s*=>', _QC_TEXT.read_text(encoding="utf-8"), re.M))
        self.assertIn("cyclic", {m["mode"] for m in MODES})
        for m in MODES:
            self.assertTrue(m["summary"].strip() and m["status"].strip(), m["mode"])
            ids = [sym["id"] for sym in m["symptoms"]]
            self.assertEqual(len(ids), len(set(ids)), m["mode"])
            for sym in m["symptoms"]:
                with self.subTest(mode=m["mode"], symptom=sym["id"]):
                    for field in ("symptom", "explanation", "check", "alternatives"):
                        self.assertTrue(sym[field].strip(), field)
                    self.assertTrue(sym["sources"])
                    self.assertLessEqual(set(sym["sources"]), set(SOURCES))
                    self.assertLessEqual(set(sym["qcCodes"]), known)
        cyclic = next(m for m in MODES if m["mode"] == "cyclic")
        self.assertIn("register.misaligned", [c for s in cyclic["symptoms"] for c in s["qcCodes"]])

    def test_every_source_has_a_doi_and_is_cited(self):
        cited = {s for t in TECHS.values() for st in t["stages"] for sym in st["symptoms"] for s in sym["sources"]}
        cited |= {s for m in MODES for sym in m["symptoms"] for s in sym["sources"]}
        for sid, src in SOURCES.items():
            with self.subTest(source=sid):
                self.assertTrue(src["doi"].startswith("10."))
                self.assertIn(sid, cited)

    def test_scopes_name_a_technology_and_a_stage_of_it_and_start_unverified(self):
        systems = [s["system"].lower() for s in SCOPES]
        self.assertEqual(len(systems), len(set(systems)))
        for scope in SCOPES:
            self.assertTrue(scope["technologies"], scope["system"])
            for t in scope["technologies"]:
                self.assertIn(t, TECHS, scope["system"])
            stages = {st["key"] for t in scope["technologies"] for st in TECHS[t]["stages"]}
            for f in scope["features"]:
                with self.subTest(system=scope["system"], feature=f["name"]):
                    self.assertIn(f["stage"], stages)
                    self.assertIs(f["verified"], False)
                    self.assertTrue(f["seenInFiles"].strip())   # what the lab's own files record

    def test_symptom_qc_codes_are_real_task_qc_codes(self):
        # A pilot's task QC reaches a stage through these: a renamed code would silently drop off.
        known = set(re.findall(r'^\s*"([\w.]+)"\s*=>', _QC_TEXT.read_text(encoding="utf-8"), re.M))
        for name, t in TECHS.items():
            mapped = [c for st in t["stages"] for sym in st["symptoms"] for c in sym["qcCodes"]]
            self.assertEqual(len(mapped), len(set(mapped)), name)   # one symptom per code
            for code in mapped:
                with self.subTest(technology=name, code=code):
                    self.assertIn(code, known)
        self.assertIn("drift.unreliable",   # the pilot's own finding on a real crop
                      [c for st in TECHS["two-photon"]["stages"] for s in st["symptoms"] for c in s["qcCodes"]])


class GetAcquisitionKnowledgeTest(unittest.TestCase):
    def test_modes_add_their_symptoms_and_sources(self):
        out = server.get_acquisition_knowledge("", "widefield", modes=["tiled", "cyclic"])
        self.assertEqual(["cyclic"], [m["mode"] for m in out["modes"]])      # tiled has no file
        self.assertEqual(out["technology"], "widefield")
        for sid in {s for sym in out["modes"][0]["symptoms"] for s in sym["sources"]}:
            self.assertIn(sid, out["sources"])
        self.assertNotIn("modes", server.get_acquisition_knowledge("", "widefield"))
        self.assertIn({"mode": "cyclic", "summary": MODES[0]["summary"]},
                      server.get_acquisition_knowledge()["modes"])
        alone = server.get_acquisition_knowledge(modes=["cyclic"])          # no technology recorded
        self.assertEqual(["cyclic"], [m["mode"] for m in alone["modes"]])

    def test_no_argument_is_the_index_only(self):
        out = server.get_acquisition_knowledge()
        self.assertNotIn("stages", out)
        self.assertEqual(set(TECHS), {t["technology"] for t in out["technologies"]})
        self.assertIn({"system": "DM5500B", "technologies": ["widefield"]}, out["microscopes"])

    def test_system_resolves_its_technology_and_keeps_only_that_scope(self):
        out = server.get_acquisition_knowledge("fv4000")
        self.assertEqual(out["technology"], "two-photon")
        self.assertEqual(["FV4000"], [s["system"] for s in out["scopes"]])
        wf = server.get_acquisition_knowledge("DM5500B")
        self.assertEqual(wf["technology"], "widefield")
        self.assertIn("camera", [st["key"] for st in wf["stages"]])
        cited = {s for st in wf["stages"] for sym in st["symptoms"] for s in sym["sources"]}
        self.assertEqual(set(wf["sources"]), cited)   # only this technology's sources

    def test_unknown_system_borrows_nothing(self):
        out = server.get_acquisition_knowledge("Nikon A1R")
        self.assertEqual(out["unknownSystem"], "Nikon A1R")
        self.assertNotIn("stages", out)
        by_tech = server.get_acquisition_knowledge("Nikon A1R", technology="two-photon")
        self.assertEqual(by_tech["technology"], "two-photon")
        self.assertEqual(by_tech["scopes"], [])
        self.assertEqual(by_tech["unknownSystem"], "Nikon A1R")

    def test_the_images_technology_picks_among_a_microscopes(self):
        ary = server.get_acquisition_knowledge("LSM 980", technology="airyscan")
        conf = server.get_acquisition_knowledge("LSM 980", technology="confocal")
        self.assertEqual((ary["technology"], conf["technology"]), ("airyscan", "confocal"))
        self.assertIn("processing", [st["key"] for st in ary["stages"]])
        self.assertEqual(["LSM 980"], [s["system"] for s in conf["scopes"]])

    def test_technology_alone_lists_its_scopes(self):
        out = server.get_acquisition_knowledge(technology="two-photon")
        self.assertEqual({"FV4000", "FVMPE-RS"}, {s["system"] for s in out["scopes"]})

    def test_briefing_compares_settings_within_a_set_first(self):
        # a setting changed between movies is diffed before any optical or biological explanation
        self.assertIn("compare their `acquisition` first", guidance.BRIEFING_GUIDANCE)


if __name__ == "__main__":
    unittest.main()
