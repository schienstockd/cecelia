# Version stamp for the notebook sysimage (pluto/deps.so).
#
# A sysimage is native code tied to the EXACT Julia version + the baked package versions. After a
# package or Julia update the on-disk image is STALE and must be rebuilt — a Julia-version mismatch
# would make Pluto's workers reject it outright, not merely run slow. So we write a sidecar
# `deps.so.stamp` at build time and check it before use (launch.jl) and to decide when to rebuild.
#
# Stamp format (hand-written JSON — no dep in the pluto env): {"julia":"<VERSION>","manifest":"<hash>"}
# where <hash> fingerprints pluto/Manifest.toml (the resolved dep versions).
#
# The API server mirrors this same two-field classification in api/src/notebooks_api.jl
# (_classify_sysimage) — kept trivially in sync; if you change the fields, change both.

_sysimage_file(dir)  = joinpath(dir, "deps.so")
_sysimage_stamp(dir) = joinpath(dir, "deps.so.stamp")
_manifest_fingerprint(dir) = (m = joinpath(dir, "Manifest.toml"); isfile(m) ? string(hash(read(m, String))) : "")

# Write the stamp next to a freshly-built deps.so. Call right after create_sysimage.
function write_sysimage_stamp(dir)
    open(_sysimage_stamp(dir), "w") do io
        print(io, "{\"julia\":\"", VERSION, "\",\"manifest\":\"", _manifest_fingerprint(dir), "\"}")
    end
end

# Fresh = the image exists AND its stamp matches this Julia + the current Manifest. A missing stamp
# (e.g. an image from before stamping existed) counts as NOT fresh, so it gets rebuilt once and stamped.
function sysimage_fresh(dir)::Bool
    (isfile(_sysimage_file(dir)) && isfile(_sysimage_stamp(dir))) || return false
    try
        s = read(_sysimage_stamp(dir), String)
        occursin("\"julia\":\"$(VERSION)\"", s) &&
            occursin("\"manifest\":\"$(_manifest_fingerprint(dir))\"", s)
    catch
        false
    end
end
