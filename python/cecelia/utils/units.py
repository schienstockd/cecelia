"""Physical-unit factors — the one Python table for converting a stored calibration unit to µm.

Calibration is stored in its own unit (OME `PhysicalSizeXUnit`, NGFF `axes[].unit`), so a pixel size in
nm read as µm makes every micron-denominated parameter 1000× off. Mirrors `_LENGTH_UNIT_UM` /
`length_unit_um` in app/src/model/image.jl. Time units are converted by
`ome_xml_utils.read_time_increment`.
"""

# µm per unit — NGFF (UDUNITS) names and OME abbreviations; `µ` and `u` both accepted
LENGTH_UNIT_UM = {"um": 1.0, "µm": 1.0, "micrometer": 1.0, "micrometre": 1.0, "micron": 1.0, "microns": 1.0,
                  "nm": 1e-3, "nanometer": 1e-3, "nanometre": 1e-3,
                  "mm": 1e3, "millimeter": 1e3, "millimetre": 1e3,
                  "cm": 1e4, "centimeter": 1e4, "centimetre": 1e4,
                  "m": 1e6, "meter": 1e6, "metre": 1e6,
                  "inch": 25400.0, "inches": 25400.0}


def length_unit_um(unit, default=1.0):
    """µm per `unit`. No unit → `default` (µm assumed); an unknown unit → `default` as well — callers
    that must tell the two apart check `unit in LENGTH_UNIT_UM` themselves."""
    if unit is None or str(unit).strip() == "":
        return default
    return LENGTH_UNIT_UM.get(str(unit).strip().lower(), default)
