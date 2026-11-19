# Sampling against the optics, for the acquisition check (acq_check.jl): is the pixel size and the
# z-step fine enough for what the objective resolves, and do a confocal's channels share one pinhole?
# Read from the settings alone (meta["acquisition"] + the pixel calibration), so they hold on any file a
# settings reader covers. Pure over meta.
#
# Source for every formula: Faklaris O et al. (2022) "Quality assessment in light microscopy for routine
# use through simple tools and robust metrics", J Cell Biol 221(11):e202107093, doi:10.1083/jcb.202107093.
#   - Nyquist, scanning confocal: "met whenever pixel size was equal to or lower than λex/8*NA" for a
#     closed pinhole; "For a 1 A.U. pinhole, the Nyquist criterion allows a 1.6× bigger pixel size".
#     Worked value there: NA 1.4, λex 488 nm → 43 nm.
#   - Resolution (FWHM of the PSF): wide-field lateral 0.51·λem/NA, axial 1.77·n·λem/NA²; scanning
#     confocal (pinhole ≥ 1 AU, NA > 0.5) lateral 0.51·λex/NA, axial 0.88·λex/(n − √(n² − NA²)).
#   - Sampling: "at least more than two times the resolution" in xy and z — the criterion used here for
#     the z-step (both technologies) and for a wide-field pixel (no λ/8NA form is given for wide field).
#   - The shortest wavelength is "the most stringent resolution value" — used across channels.
# Two-photon and Airyscan have no formula in that source, so they get no sampling finding (`basis` says why).

#: a pixel finer than this fraction of the Nyquist pixel is oversampled — more light per cell and slower
#: frames for no added detail. Our cut-off (the source gives the lower bound only).
const ACQ_OVERSAMPLE = 0.5
#: a confocal pinhole at or above this many Airy units counts as the 1 AU case (the readers report
#: 0.99 and 1.01 for a 1 AU setting)
const ACQ_PINHOLE_1AU = 0.9
#: channels whose pinholes (AU) differ by more than this ratio have different optical sections
const ACQ_PINHOLE_SPREAD = 1.25

# refractive index of an immersion medium, when the reader recorded none
const _ACQ_IMMERSION_N = Dict("air" => 1.0, "dry" => 1.0, "water" => 1.333, "oil" => 1.518, "glycerol" => 1.47,
                              "glycerin" => 1.47, "silicone" => 1.406)

# the centre of a detection band "555-624 nm" (first range of several), or nothing
function _acq_band_centre(b)
    b isa AbstractString || return nothing
    m = match(r"(\d+(?:\.\d+)?)\s*-\s*(\d+(?:\.\d+)?)", b)
    m === nothing && return nothing
    (parse(Float64, m[1]) + parse(Float64, m[2])) / 2
end

# a length in µm along `axis` from meta and its unit (`length_unit_um`); nothing for a unit we don't read
function _acq_um(meta, axis)
    x = _anum(_ag(meta, "PhysicalSize$(axis)"))
    # the unit through `_ag` like the value (meta read back through JSON3 may be Symbol-keyed); the
    # per-axis → shared fallback as `_meta_axis_length_unit`
    f = length_unit_um(something(_ag(meta, "PhysicalSize$(axis)Unit"), _ag(meta, "PhysicalSizeUnit"), Some(nothing)))
    (x === nothing || x <= 0 || f === nothing) ? nothing : x * f
end

"""
    acq_sampling(meta) -> Dict

The sampling of one image against its optics: `pixelUm` / `nyquistXYUm` (the pixel the optics need),
`zStepUm` / `nyquistZUm` (z-stacks only), `pinholes` (per channel, AU) and `basis` — the technology and
wavelength the numbers rest on, or why there are none (`"no formula for two-photon"`, `"no NA"`, …).
Absent values are `nothing`. See the file header for the formulas and their source.
"""
function acq_sampling(meta::AbstractDict)::Dict{String,Any}
    out = Dict{String,Any}("pixelUm" => nothing, "nyquistXYUm" => nothing, "zStepUm" => nothing,
                           "nyquistZUm" => nothing, "pinholes" => Float64[], "basis" => nothing)
    acq = _ag(meta, "acquisition")
    acq isa AbstractDict || (out["basis"] = "no settings read"; return out)
    tech = acquisition_technology(meta)
    obj = _ag(acq, "objective")
    na = _anum(_ag(obj, "na"))
    imm = _ag(obj, "immersion")
    n = something(_anum(_ag(obj, "refractiveIndex")), get(_ACQ_IMMERSION_N, lowercase(string(something(imm, ""))), nothing), Some(nothing))
    chans = [c for c in _ag(acq, "channels", Any[]) if c isa AbstractDict]
    tech == "confocal" && (out["pinholes"] = Float64[p for p in (_anum(_ag(c, "pinholeAiry")) for c in chans) if p !== nothing])
    out["pixelUm"] = _acq_um(meta, "X")
    size_z = something(_anum(_ag(meta, "SizeZ")), 1.0)
    size_z > 1 && (out["zStepUm"] = _acq_um(meta, "Z"))
    tech in ("confocal", "widefield") || (out["basis"] = "no formula for $(something(tech, "this technology"))"; return out)
    na === nothing && (out["basis"] = "no NA"; return out)
    if tech == "confocal"
        # per channel: its shortest line, and the 1.6× allowance when its pinhole is at 1 AU or wider
        # (unknown pinhole: the 1 AU case — the default every confocal ships with)
        cands = Tuple{Float64,Float64}[]
        for c in chans
            ls = [w for w in (_anum(_ag(l, "wavelengthNm")) for l in something(_ag(c, "laserLines"), Any[]) if l isa AbstractDict) if w !== nothing]
            isempty(ls) && continue
            ph = _anum(_ag(c, "pinholeAiry"))
            push!(cands, (minimum(ls), ph === nothing || ph >= ACQ_PINHOLE_1AU ? 1.6 : 1.0))
        end
        if isempty(cands)   # no per-channel lines: the image's lasers, pinhole as above
            ws = [w for w in (_anum(_ag(l, "wavelengthNm")) for l in _ag(acq, "lasers", Any[]) if l isa AbstractDict) if w !== nothing]
            ph = _anum(_ag(acq, "pinholeAiry"))
            isempty(ws) || push!(cands, (minimum(ws), ph === nothing || ph >= ACQ_PINHOLE_1AU ? 1.6 : 1.0))
        end
        isempty(cands) && (out["basis"] = "no excitation wavelength"; return out)
        out["nyquistXYUm"] = minimum(w / 1000 / (8 * na) * f for (w, f) in cands)
        lam = minimum(first, cands) / 1000
        out["basis"] = "confocal, $(round(Int, lam * 1000)) nm excitation, NA $(round(na; digits = 2))"
        if n !== nothing && n > na
            out["nyquistZUm"] = 0.88 * lam / (n - sqrt(n^2 - na^2)) / 2
        end
    else
        ems = [e for e in (_acq_band_centre(_ag(c, "emissionFilter")) for c in chans) if e !== nothing]
        isempty(ems) && (out["basis"] = "no emission band"; return out)
        lam = minimum(ems) / 1000
        out["nyquistXYUm"] = 0.51 * lam / na / 2
        out["basis"] = "widefield, $(round(Int, lam * 1000)) nm emission, NA $(round(na; digits = 2))"
        n === nothing || (out["nyquistZUm"] = 1.77 * n * lam / na^2 / 2)
    end
    out
end

"""
    acq_sampling_state(s) -> (xy, z)

`:under`, `:over`, `:fine` or `nothing` (not known) for the pixel and the z-step of `acq_sampling(meta)`:
coarser than the Nyquist value is under; finer than `ACQ_OVERSAMPLE` of it is over. Oversampling is
read in xy only — a fine z-step is often the point of a stack.
"""
function acq_sampling_state(s)
    st(v, q; over = true) = (v === nothing || q === nothing) ? nothing :
        v > q * (1 + ACQ_REL_TOLERANCE) ? :under : over && v < ACQ_OVERSAMPLE * q ? :over : :fine
    st(s["pixelUm"], s["nyquistXYUm"]), st(s["zStepUm"], s["nyquistZUm"]; over = false)
end
