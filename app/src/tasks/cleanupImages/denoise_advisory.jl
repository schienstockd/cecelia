# ── Denoising (SUPPORT) — form-time advisories, served by /api/tasks/validate ─────────────────────
#
# The model picker says whether there is a model to pick and whether its temporal window fits the
# selected movies; the channel picker says which picks are saturated (Run drops them) or not
# photon-limited. Same rules the handler (denoise.jl) and `denoise_run.py` refuse on.

"""
    denoise_input_frames(resolved, picked=nothing) -> Int

The temporal window a resolved model (`denoise_model_resolve`) needs: a pooled model's
`arch.inputFrames`, or for a per-channel bundle the widest sub-model among the `picked` channel
names (all of them when `picked` is `nothing`); 61 when none is recorded. The handler passes this
number to `denoise_run.py`, which uses it rather than re-reading the manifests.
"""
function denoise_input_frames(resolved, picked = nothing)::Int
    frames(m) = begin
        m isa AbstractDict || return 0
        a = get(m, "arch", get(m, :arch, nothing))
        a isa AbstractDict || return 0
        v = _cal_num(get(a, "inputFrames", get(a, :inputFrames, nothing)))
        isnothing(v) ? 0 : round(Int, v)
    end
    w = resolved.kind === :pooled ? frames(resolved.manifest) : 0
    for (name, e) in resolved.perChannel
        (picked === nothing || name in picked) && (w = max(w, frames(e.manifest)))
    end
    w > 0 ? w : 61
end

"""
    denoise_short_movie(window, sizeT) -> Bool

The refusal rule: a movie with fewer timepoints than the model's window cannot be denoised. Shared
by the handler's pre-flight and the model advisory. An unknown `sizeT` (≤ 0) is not refused here.
"""
denoise_short_movie(window::Integer, sizeT::Integer) = 0 < sizeT < window

function _denoise_model_advisory(value, imgs::AbstractVector, siblings::AbstractDict;
                                 models = list_denoise_models, resolve = denoise_model_resolve)
    name = value isa AbstractString ? strip(value) : ""
    if isempty(name)
        isempty(models()) || return nothing
        return (severity = "warn", message = "No model yet: train one first",
                tip = "Denoising runs a trained model. Train one with Train denoise model (SUPPORT), then pick it here.")
    end
    r = resolve(name)
    r === nothing && return (severity = "fail", message = "Model not in the vault",
                             tip = "No denoise model named '$name'. Pick another, or train it again.")
    # Per image, the window of the channels Run will denoise: the picks minus the ones its
    # saturation gate drops (denoise.jl), as the handler computes it.
    picks = get(siblings, "channels", nothing)
    windows, ts = Int[], Int[]
    for img in imgs
        t = something(meta_int(img.meta, "SizeT"), 0)
        t > 0 || continue
        picked = nothing
        if picks isa AbstractVector && !isempty(picks)
            idx, names = _picked_channels(img, picks)
            sat = channel_signal_flags(img.meta, idx).saturated
            keep = [names[i + 1] for i in idx if !(i in sat) && i < length(names)]
            isempty(keep) || (picked = keep)
        end
        push!(windows, denoise_input_frames(r, picked)); push!(ts, t)
    end
    isempty(ts) && return nothing
    w = maximum(windows)
    n, short = length(ts), count(k -> denoise_short_movie(windows[k], ts[k]), eachindex(ts))
    if short == 0
        return (severity = "ok", message = "$(w)-frame window fits $(n == 1 ? "the movie" : "all $n movies")",
                tip = "Every selected movie has at least $w timepoints, the model's temporal window.")
    end
    (severity = short == n ? "fail" : "warn",
     message  = short == n ? "Movie shorter than the $(w)-frame window" :
                             "$short of $n movies shorter than the $(w)-frame window",
     tip      = "This model reads $w frames at a time, so Run refuses a movie with fewer timepoints " *
                "(shortest here: $(minimum(ts))). Pick longer movies or a model trained with a smaller window.")
end

register_param_validator!("cleanupImages.denoise", "model", _denoise_model_advisory)

