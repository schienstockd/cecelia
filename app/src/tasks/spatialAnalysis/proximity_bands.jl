# ── Proximity profile — the pure band arithmetic (no image, no I/O) ──────────────────────────────
#
# Which populations sit near a structure? Every counted cell has a distance (µm) to the nearest cell of
# the structure; the distances fall into equal-width BANDS (0–w, w–2w, …) plus one band beyond the last
# edge. Per band, each population's share of the counted cells is compared with its share across the
# whole image: log2 of the ratio is the enrichment (> 0 = more of it near the structure than the image
# as a whole would give, < 0 = fewer). The image composition is the expectation under random placement
# of the same cells, so no permutation is needed for the effect size; comparisons between groups of
# images use the image as the unit (the per-image metric plots).
#
# Idea from SPACEc's patch proximity analysis (Tan et al., Nat Commun 16:10652, 2025,
# doi:10.1038/s41467-025-65658-3), which counts cells in radii around a structure's border cells. That
# reports raw composition; the image-composition baseline here is ours. No SPACEc code is used.
#
# The DENOMINATOR is a REFERENCE population — by default every cell of the counted populations'
# segmentations (`<vn>/root`) — so a share is "of the tissue's cells in that band", and a single picked
# population or a parent of the others still reads its own distribution. (A baseline built from the
# picked populations alone made a lone population, or a parent covering the rest, read 0 everywhere.)
# The task adds every counted cell to the reference, so a share never exceeds 1.

"""Pseudocount added to counts before the log, so an empty band reads as strongly depleted, not −Inf
(the same 0.5 the contact log-odds use)."""
const PROXIMITY_PSEUDOCOUNT = 0.5

# trailing ".0" off a whole number, so a band reads "0–15 µm" rather than "0.0–15.0 µm"
_band_num(x::Real) = isinteger(x) ? string(Int(x)) : string(round(x; digits = 1))

"""
    proximity_bands(width, n) -> Vector{NamedTuple}

`n` equal-width bands from the structure out to `n × width` µm, plus one open band beyond. Each is
`(lo, hi, label)`; the last has `hi = Inf`.
"""
function proximity_bands(width::Real, n::Integer)
    w = Float64(width)
    bands = [(; lo = (k - 1) * w, hi = k * w, label = "$(_band_num((k - 1) * w))–$(_band_num(k * w)) µm")
             for k in 1:n]
    push!(bands, (; lo = n * w, hi = Inf, label = ">$(_band_num(n * w)) µm"))
    bands
end

"""
    proximity_band_index(d, width, n) -> Int

Which band a distance falls in: 1 for `0 ≤ d ≤ width`, `k` for `(k-1)·width < d ≤ k·width`, `n + 1`
beyond the last edge, and 0 for a cell with no distance (NaN — no structure cell in its frame), which is
left out of the profile.
"""
function proximity_band_index(d::Real, width::Real, n::Integer)::Int
    isnan(d) && return 0
    d <= width && return 1
    min(ceil(Int, d / width), n + 1)
end

"""
    proximity_records(pops, members, reference, bands) -> Vector{Dict}

One record per (population, band), populations in the order given. `members[pop]` and `reference` map
each cell's key — `(value_name, label)` — to its band index (`proximity_band_index`); index 0 is skipped.
Record fields: `pop`, `band` (label), `lo`, `hi` (`nothing` for the open band), `n` (this population's
cells in the band), `nBand` (reference cells in the band), `fraction` (`n / nBand`, `nothing` for an
empty band), `imageFraction` (this population's cells over all reference cells) and `log2Enrichment`.
"""
function proximity_records(pops::AbstractVector{<:AbstractString}, members::AbstractDict,
                           reference::AbstractDict, bands::AbstractVector)
    nb = length(bands)
    n_band = zeros(Int, nb)
    for b in values(reference); 1 <= b <= nb && (n_band[b] += 1); end
    n_all = sum(n_band)
    pc = PROXIMITY_PSEUDOCOUNT
    records = Dict{String,Any}[]
    for pop in pops
        m = get(members, pop, Dict())
        counts = zeros(Int, nb)
        for b in values(m); 1 <= b <= nb && (counts[b] += 1); end
        n_pop = sum(counts)
        image_frac = n_all == 0 ? nothing : n_pop / n_all
        for (b, band) in enumerate(bands)
            enrich = (n_all == 0 || n_band[b] == 0) ? nothing :
                log2(((counts[b] + pc) / (n_band[b] + pc)) / ((n_pop + pc) / (n_all + pc)))
            push!(records, Dict{String,Any}(
                "pop" => String(pop), "band" => band.label, "lo" => band.lo,
                "hi" => isinf(band.hi) ? nothing : band.hi,
                "n" => counts[b], "nBand" => n_band[b],
                "fraction" => n_band[b] == 0 ? nothing : counts[b] / n_band[b],
                "imageFraction" => image_frac, "log2Enrichment" => enrich))
        end
    end
    records
end

"""Fewest counted cells in a band before its shares read as descriptive only."""
const PROXIMITY_MIN_BAND_CELLS = 20

"""
    proximity_findings(n_structure, records) -> Vector{Dict}

Advisory QC over one run: no structure cells, a picked population with no counted cells (it may BE the
structure — its cells are never counted around itself), and bands too thin to read.
"""
function proximity_findings(n_structure::Integer, records::AbstractVector)
    n_structure == 0 && return [qc_finding("warn", "proximity.no_structure", "No structure cells",
        "Check the structure population: it resolved to no cells.")]
    findings = Dict{String,Any}[]
    for pop in unique(String(r["pop"]) for r in records)
        sum(Int(r["n"]) for r in records if r["pop"] == pop) == 0 &&
            push!(findings, qc_finding("warn", "proximity.empty_pop", "No cells to count",
                "Pick a population that is not the structure itself."; detail = pop))
    end
    thin = unique(String(r["band"]) for r in records if Int(r["nBand"]) < PROXIMITY_MIN_BAND_CELLS)
    isempty(thin) || push!(findings, qc_finding("warn", "proximity.thin_band", "Few cells in a band",
        "Read those bands as descriptive, or widen the bands."; detail = join(thin, ", ")))
    findings
end
