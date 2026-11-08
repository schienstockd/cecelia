# Step 1 of `importImages.immunemap`: download one Immunemap video into the image's folder, verified
# against Immunemap's checksums, and point `ori_path` at it so step 2 (`importImages.omezarr`, the
# ordinary converter) imports it like any local file. Returning the file as `filename` is what makes
# the composite delete it once the conversion has succeeded — and only then: a failed run keeps the
# download (or its `.partial`) and the next run resumes it.
#
# The image was registered by `/api/import/register-immunemap`, which stored everything this step
# needs under `meta["immunemap"]`. Design: `docs/todo/IMMUNEMAP_IMPORT_PLAN.md`.

# COHORT-EXEMPT: download size varies by video by design, and the one quality signal (file vs
# Immunemap voxel size) is a per-image yes/no already raised as a finding — nothing to outlier-check.
struct ImmunemapFetch <: CciaTask end

const IMMUNEMAP_SOURCE_FILE = "immunemap_source.ims"

# No params: everything the download needs was stored on the image at registration
# (`meta.immunemap`). The empty struct keeps the task on the typed-params convention, so a param
# added later starts typed.
struct ImmunemapFetchParams end
function parse_immunemap_fetch_params(::AbstractDict)::ImmunemapFetchParams
    ImmunemapFetchParams()
end

"""
    immunemap_fetch_qc_findings(mismatch_axes, file_xyz, immunemap_xyz) -> Vector

Advisory findings for an Immunemap download. Pure, so the rule is unit-tested.

The one finding is a voxel size the file and Immunemap disagree on. The conversion takes the file's,
which is the right default (it is what the microscope wrote), but every speed and distance downstream
scales with it, so a disagreement is worth a badge rather than a log line.
"""
function immunemap_fetch_qc_findings(mismatch_axes::AbstractVector, file_xyz, immunemap_xyz)
    isempty(mismatch_axes) && return Dict{String,Any}[]
    [qc_finding("warn", "import.immunemap_spacing"; file = file_xyz, published = immunemap_xyz)]
end

function _run_task(task::ImmunemapFetch, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    parse_immunemap_fetch_params(params)
    im = get(img.meta, "immunemap", nothing)
    if !(im isa AbstractDict) || isempty(string(get(im, "videoUid", "")))
        on_log("[ERROR] This image has no Immunemap source recorded. Add it through " *
               "Import from Immunemap, which registers the video before this step can download it.")
        return nothing
    end
    dest        = joinpath(img_zero_dir(img), IMMUNEMAP_SOURCE_FILE)
    run_dir     = task_run_dir(img._dir)
    result_file = joinpath(run_dir, "immunemap_fetch.$(string(rand(UInt32); base = 16)).result.json")
    spacing     = [Float64(x) for x in get(im, "spacingXYZ", Any[])]
    ok = run_py("tasks/importImages/fetch_immunemap_run.py",
        (; videoUid = string(im["videoUid"]), destPath = dest,
           sizeBytes = get(im, "sizeBytes", nothing), md5 = get(im, "md5", nothing),
           spacingXYZ = spacing, resultPath = result_file),
        run_dir; on_log = on_log, on_progress = on_progress, on_process = on_process)
    if !(ok && isfile(result_file))
        on_log("[ERROR] Download failed. Re-run to resume — the partial file is kept.")
        return nothing
    end
    r = JSON3.read(read(result_file, String))
    rm(result_file; force = true)

    retrieved = Dates.format(Dates.now(Dates.UTC), Dates.dateformat"yyyy-mm-ddTHH:MM:SSZ")
    img.meta["ori_path"] = dest
    img.meta["immunemap"] = merge(Dict{String,Any}(String(k) => v for (k, v) in pairs(im)),
                                  Dict{String,Any}("retrieved" => retrieved,
                                                   "md5Verified" => Bool(r.md5Verified)))
    # only these meta keys — a full `save!` would write back the whole loaded image
    commit_state!(img) do raw
        m = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict{String,Any}()))
        m["ori_path"]  = img.meta["ori_path"]
        m["immunemap"] = img.meta["immunemap"]
        raw["meta"] = m
    end

    try
        bad      = String[string(a) for a in r.spacingMismatch]
        file_xyz = isnothing(r.fileVoxelSizeXYZ) ? nothing : [Float64(x) for x in r.fileVoxelSizeXYZ]
        findings = immunemap_fetch_qc_findings(bad, file_xyz, spacing)
        write_qc(img, "importImages.immunemapFetch", VERSIONED_DEFAULT_VAL, findings;
                 metrics = Dict{String,Any}("downloadMB" => round(filesize(dest) / 1e6; digits = 1),
                                            "spacingMismatch" => length(bad)))
        isempty(findings) || on_log("[QC] $(length(findings)) finding(s) — see the image's QC badge.")
    catch e
        on_log("[QC] could not compute download QC: $e")
    end

    # `filename` → the composite deletes the download after the conversion step succeeds.
    Dict{String,Any}("uid" => img.uid, "filename" => IMMUNEMAP_SOURCE_FILE)
end
