# Version stamp for the notebook sysimage (pluto/deps.so).
#
# A sysimage is native code tied to the EXACT Julia version + the baked package versions. After a
# package or Julia update the on-disk image is STALE and must be rebuilt — a Julia-version mismatch
# would make Pluto's workers reject it outright, not merely run slow. So we write a sidecar
# `deps.so.stamp` at build time and check it before use (launch.jl) and to decide when to rebuild.
#
# Stamp format (hand-written JSON — no dep in the pluto env):
#   {"julia":"<VERSION>","manifest":"<hash>","variant":"deps"|"full"}
# where <hash> fingerprints pluto/Manifest.toml (the resolved dep versions).
#
# WHY `variant` EXISTS. Both builders write the SAME pluto/deps.so — that is deliberate (one path,
# two recipes; see docs/NOTEBOOKS.md), so launch.jl picks up whichever was built. But the deps-only
# build EXCLUDES Cecelia, so workers load it from source and pick up edits, while the -full build
# bakes Cecelia into the image at build time. Without recording which recipe produced the image the
# two are indistinguishable on disk: a `pixi run notebooks-sysimage-full` on a dev machine silently
# hands notebook workers a FROZEN Cecelia and edits to app/src stop reaching notebooks, with nothing
# to say so. (Verified against real 1.4 GB images: under `full`, a brand-new session returns the
# build-time definition while the source says otherwise — and `pathof(Cecelia)` still points at the
# source, so inspection does not reveal it.) The stamp records the recipe so launch.jl can report
# which image it is actually using instead of always claiming "deps".
#
# NB this is about the image being frozen, NOT about Revise: nothing in the notebook path loads
# Revise at all (it lives in the global default env and only api/dev.jl uses it, for the API server).
#
# THIS FILE IS THE SINGLE IMPLEMENTATION. api/src/notebooks_api.jl used to carry a parallel copy of
# all of it — its own path/fingerprint helpers plus its own JSON-parsing stamp readers — with a
# comment admitting they were "kept trivially in sync". They were not one edit away from diverging;
# they were one *forgotten* edit away. The API server now `include`s this file and calls these
# functions, which is why they stay dependency-free (no JSON3): an env that cannot take a dependency
# must still be able to load it by path.

_sysimage_file(dir)  = joinpath(dir, "deps.so")
_sysimage_stamp(dir) = joinpath(dir, "deps.so.stamp")
_manifest_fingerprint(dir) = (m = joinpath(dir, "Manifest.toml"); isfile(m) ? string(hash(read(m, String))) : "")

# Write the stamp next to a freshly-built deps.so. Call right after create_sysimage.
# `variant` is "deps" (Cecelia loaded from source, edits apply) or "full" (Cecelia baked in, frozen).
function write_sysimage_stamp(dir, variant::AbstractString = "deps")
    variant in ("deps", "full") || throw(ArgumentError("variant must be \"deps\" or \"full\", got $(repr(variant))"))
    open(_sysimage_stamp(dir), "w") do io
        print(io, "{\"julia\":\"", VERSION, "\",\"manifest\":\"", _manifest_fingerprint(dir),
                  "\",\"variant\":\"", variant, "\"}")
    end
end

# ── Pure predicates over the stamp CONTENTS ──────────────────────────────────
# THE single implementation, shared by both callers: the Pluto launcher (which has a directory) and
# the API server (which has already read the bytes). Deliberately string-matching rather than JSON
# parsing, so this file stays dependency-free and any env can `include` it by path — which is what
# lets the API server use it instead of keeping a second copy.

"""Does the stamp match this Julia + Manifest? `nothing` (no stamp) is never a match."""
function stamp_matches(stamp::Union{String,Nothing}, julia::AbstractString, manifest::AbstractString)::Bool
    stamp === nothing && return false
    occursin("\"julia\":\"$(julia)\"", stamp) && occursin("\"manifest\":\"$(manifest)\"", stamp)
end

"""
Which recipe built the image: "deps", "full", or "unknown" (no stamp, unreadable, or written before
`variant` existed). "unknown" is NOT an error — it must not affect freshness, only what we report —
so an older image keeps working and is described honestly rather than mislabelled "deps".
"""
function stamp_variant(stamp::Union{String,Nothing})::String
    stamp === nothing && return "unknown"
    occursin("\"variant\":\"full\"", stamp) ? "full" :
    occursin("\"variant\":\"deps\"", stamp) ? "deps" : "unknown"
end

# ── Thin IO wrappers over the predicates above ───────────────────────────────

"""Stamp contents for `dir`, or `nothing` if absent/unreadable."""
function read_sysimage_stamp(dir)::Union{String,Nothing}
    isfile(_sysimage_stamp(dir)) || return nothing
    try read(_sysimage_stamp(dir), String) catch; nothing end
end

sysimage_variant(dir)::String = stamp_variant(read_sysimage_stamp(dir))

# Fresh = the image exists AND its stamp matches this Julia + the current Manifest. A missing stamp
# (e.g. an image from before stamping existed) counts as NOT fresh, so it gets rebuilt once and stamped.
sysimage_fresh(dir)::Bool =
    isfile(_sysimage_file(dir)) &&
    stamp_matches(read_sysimage_stamp(dir), string(VERSION), _manifest_fingerprint(dir))
