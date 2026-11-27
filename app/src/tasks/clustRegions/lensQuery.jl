# ── clustRegions.lensQuery — "find regions like this one" from a pinned lens ─────────────────────────
#
# docs/todo/LENS_QUERY_PLAN.md. The lens's signature is the mean neighbourhood composition of the
# cells inside it, from a region run's composition columns (`region_lens_query.jl`). Every cell of
# every image in that run is scored by its similarity to it above the run's average cell, 0–1
# (`lens_similarity_scores`), and written as one numeric obs column,
# `spatial.lens.<name>` (`lens_score_col`). No population, no threshold: the viewer colours by the
# column, a filter population on it saves "places like this".
#
# Set-scope; the viewer sends the image the lens is on. The run's other images come from its
# clustfeatures entry (`_clustfeatures_part_of`) — the composition basis is shared across them, so the
# signature carries; pixel geometry never does.

struct LensQuery <: CciaTask end
register_builtin_task!("clustRegions.lensQuery", LensQuery(); spec = joinpath(@__DIR__, "lensQuery.json"))

Base.@kwdef struct LensQueryParams
    valueName::String    = "default"
    regionSuffix::String = "default"
    lensX::Float64       = NaN
    lensY::Float64       = NaN
    lensR::Float64       = NaN
    lensT::Int           = -1          # the frame whose cells form the lens; -1 = every frame / a still
    outputName::String   = "lens"
end

function parse_lens_query_params(d::AbstractDict)::LensQueryParams
    f(k) = (v = get(d, k, nothing); v === nothing ? NaN : Float64(v))
    LensQueryParams(;
        valueName    = string(get(d, "valueName", "default")),
        regionSuffix = string(get(d, "regionSuffix", "default")),
        lensX = f("lensX"), lensY = f("lensY"), lensR = f("lensR"),
        lensT        = Int(get(d, "lensT", -1)),
        outputName   = string(get(d, "outputName", "lens")))
end

# One image's cells as the query reads them: composition rows (+ usable flag), labels, x/y (+ t).
function _lens_query_table(img::CciaImage, vn::AbstractString, feats::Vector{String})
    lp = label_props(img; value_name = vn)
    spatial = centroid_columns(lp; order = [:x, :y])
    length(spatial) == 2 || return nothing
    tcols = temporal_columns(lp)
    df = lp |> view_centroid_cols |> v -> select_cols(v, feats) |> as_df
    X, ok = composition_rows(Matrix(df[!, feats]))
    (; X, ok, labels = Int.(df[!, :label]), x = Float64.(df[!, spatial[1]]), y = Float64.(df[!, spatial[2]]),
       t = isempty(tcols) ? nothing : round.(Int, df[!, first(tcols)]))
end

function _run_task(::LensQuery, imgs::Vector{CciaImage}, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    isempty(imgs) && (on_log("[ERROR] lensQuery: no images"); return nothing)
    p = parse_lens_query_params(params)
    (isfinite(p.lensX) && isfinite(p.lensY) && isfinite(p.lensR) && p.lensR >= 1) ||
        (on_log("[ERROR] lensQuery: the lens needs x, y and a radius of at least 1 px"); return nothing)
    src = imgs[1]
    col = lens_score_col(p.outputName)

    # the run's images: those sent, plus every image the run recorded (its composition basis is shared)
    entry = _clustfeatures_entry(img_label_props_path(src, p.valueName), p.regionSuffix; family = "regions")
    entry === nothing &&
        (on_log("[ERROR] lensQuery: no region run \"$(p.regionSuffix)\" on $(p.valueName) — run Cluster regions first"); return nothing)
    members = Dict{String,CciaImage}(img.uid => img for img in imgs)
    for u in lens_run_images(src, p.valueName, p.regionSuffix)
        haskey(members, u) && continue
        mi = try init_object(img_project_uid(src), u) catch; nothing end
        mi isa CciaImage && p.valueName in versioned_keys(mi.label_props) && (members[u] = mi)
    end
    feats = region_lens_features(img_label_props_path(src, p.valueName), p.regionSuffix)
    order = vcat([src], [m for (u, m) in members if u != src.uid])
    on_progress(1, length(order) + 2)

    tabs = Any[]; imgs_ok = CciaImage[]
    for m in order
        f = region_lens_features(img_label_props_path(m, p.valueName), p.regionSuffix)
        f == feats || (on_log("[WARN] lensQuery: $(m.uid) has a different region run \"$(p.regionSuffix)\" — skipped"); continue)
        tb = _lens_query_table(m, p.valueName, feats)
        tb === nothing && (on_log("[WARN] lensQuery: $(m.uid) has no x/y centroids — skipped"); continue)
        push!(tabs, tb); push!(imgs_ok, m)
    end
    (isempty(imgs_ok) || imgs_ok[1].uid != src.uid) &&
        (on_log("[ERROR] lensQuery: the lens's image could not be read"); return nothing)

    # the lens's signature, from the image it is on
    s = tabs[1]
    inside = s.ok .& lens_inside(s.x, s.y, (; x = p.lensX, y = p.lensY, r = p.lensR))
    (p.lensT >= 0 && s.t !== nothing) && (inside .&= s.t .== p.lensT)
    n_in = count(inside)
    n_in >= LENS_QUERY_MIN_CELLS ||
        (on_log("[ERROR] lensQuery: too few cells in the lens ($n_in) — make it bigger"); return nothing)
    sig = lens_signature(s.X, inside)
    any(>(0), sig) || (on_log("[ERROR] lensQuery: no neighbourhood composition in the lens — re-run Cluster regions"); return nothing)
    gmean = vec(sum(reduce(vcat, [tb.X[tb.ok, :] for tb in tabs]); dims = 1)) ./ sum(count(tb.ok) for tb in tabs)
    max_lift = maximum(abs.(sig .- gmean))

    mu = lens_background(tabs, sig)
    coherence = lens_coherence(s.X, inside, sig)
    on_progress(2, length(order) + 2)
    on_log("[INFO] lensQuery: $(n_in) cells in the lens (coherence $(round(coherence; digits = 2))); " *
           "average cell $(round(mu; digits = 2)); scoring $(length(imgs_ok)) image(s) → $col")

    findings = lens_query_qc_findings(max_lift, coherence)
    for (i, (m, tb)) in enumerate(zip(imgs_ok, tabs))
        z = lens_similarity_scores(tb.X, tb.ok, sig, mu)
        props = img_label_props_path(m, p.valueName)
        label_props(props) |> add_obs(DataFrame("label" => tb.labels, col => z)) |> save!
        write_qc(m, "clustRegions.lensQuery", p.valueName, findings; output = col,
                 metrics = Dict{String,Any}("nScored" => count(tb.ok), "nInside" => m.uid == src.uid ? n_in : 0,
                                            "maxLift" => round(max_lift; digits = 3),
                                            "coherence" => round(coherence; digits = 3),
                                            "nStrong" => count(v -> isfinite(v) && v >= LENS_STRONG, z)))
        on_progress(i + 2, length(order) + 2)
    end
    Dict{String,Any}("column" => col, "images" => [m.uid for m in imgs_ok], "nInside" => n_in)
end
