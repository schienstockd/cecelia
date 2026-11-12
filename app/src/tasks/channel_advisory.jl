# ── Channel-pick advisory — the import's per-channel facts, under a task's channel picker ─────────
#
# A cleanup step that helps only photon-limited channels (denoising, smoothing), or that cannot undo
# what a saturated channel clipped (denoising, AF correction), says so where the channel is picked.
# It reads the facts the import measured (`channel_signal_flags`, qc.jl) — the same flag the denoise
# gate refuses on and the same rule `import.photon_limited` fires on — so the form and the run
# cannot disagree. The tasks that use it, each with its own reasons, are registered in the table at
# the end of this file; docs/MODULES.md → *Param advisories*.

const _ChannelReason = Union{Nothing,Tuple{String,String}}   # (short reason, tip)

# A pick resolved as the Run handlers resolve it — against the `default` names, the order the import's
# per-channel facts are indexed in — so the form and Run agree on which channel a name means.
# `(indices, names)`; no indices when the pick names a channel this image lacks.
function _picked_channels(img::CciaImage, picks)
    names = something(channel_names(img; value_name = VERSIONED_DEFAULT_VAL), String[])
    (try channel_indices(picks, names) catch; Int[] end), names
end

_short_names(ns) = length(ns) <= 2 ? join(ns, ", ") : "$(join(ns[1:2], ", ")) +$(length(ns) - 2)"

"""
    channel_pick_advisory(value, imgs; saturated=nothing, not_photon_limited=nothing) -> NamedTuple | Nothing

`value` is the picked channel name(s). Each image resolves the pick as the Run handlers do
(`_picked_channels`) and reads `channel_signal_flags` from its import meta. A `(reason, tip)` pair
turns a check on: the line reads "<channels> saturated: <reason>". Saturation is reported first.
`ok` only when the photon check is on and every picked channel on every image measured
photon-limited and none is saturated (whether or not the task gives a saturation reason). `nothing` when there is no pick, no image resolves it, or the import has no numbers.
"""
function channel_pick_advisory(value, imgs::AbstractVector;
                               saturated::_ChannelReason = nothing,
                               not_photon_limited::_ChannelReason = nothing)
    picks = value isa AbstractVector ? collect(value) :
            (value === nothing || value == "") ? Any[] : Any[value]
    isempty(picks) && return nothing
    sat_names, npl_names = Set{String}(), Set{String}()
    n, n_sat, n_npl, all_pl = 0, 0, 0, true
    for img in imgs
        idx, names = _picked_channels(img, picks)
        isempty(idx) && continue
        f = channel_signal_flags(img.meta, idx)
        nm(i) = i < length(names) ? names[i + 1] : "C$(i + 1)"
        n += 1
        isempty(f.saturated)        || (n_sat += 1; union!(sat_names, nm.(f.saturated)))
        isempty(f.notPhotonLimited) || (n_npl += 1; union!(npl_names, nm.(f.notPhotonLimited)))
        (length(f.photonLimited) == length(idx) && isempty(f.saturated)) || (all_pl = false)
    end
    n == 0 && return nothing
    on(k) = n == 1 ? "" : " ($k of $n images)"
    if saturated !== nothing && n_sat > 0
        also = (not_photon_limited !== nothing && n_npl > 0) ?
            " Also not photon-limited: $(join(sort!(collect(npl_names)), ", "))." : ""
        return (severity = "warn",
                message  = "$(_short_names(sort!(collect(sat_names)))) saturated: $(saturated[1])$(on(n_sat))",
                tip      = saturated[2] * also)
    end
    if not_photon_limited !== nothing && n_npl > 0
        return (severity = "warn",
                message  = "$(_short_names(sort!(collect(npl_names)))) not photon-limited: $(not_photon_limited[1])$(on(n_npl))",
                tip      = not_photon_limited[2])
    end
    (not_photon_limited !== nothing && all_pl) || return nothing
    (severity = "ok", message = "Photon-limited$(n == 1 ? "" : " on all $n images")",
     tip = "The import found these channels sparse and low-count, the case this step is for.")
end

# ── Which pickers get it, and the reason each gives ───────────────────────────────────────────────
# One table rather than a line in each task file: the reasons read side by side, so the four steps
# describe the same import facts the same way.

const _SAT_TIP = "Clipped at the detector on import, so those values are lost."
const _NPL_TIP = "The import found these channels bright and dense. Shot noise matters only on sparse, low-count channels."

register_param_validator!("cleanupImages.denoise", "channels",
    (v, imgs, _) -> channel_pick_advisory(v, imgs;
        saturated = ("skipped at Run", _SAT_TIP * " Shot-noise denoisers do nothing on clipped data, so Run drops these channels, and refuses if every pick is saturated."),
        not_photon_limited = ("little noise to remove", _NPL_TIP)))

register_param_validator!("opticalFlow.trainSupportDenoise", "trainChannels",
    (v, imgs, _) -> channel_pick_advisory(v, imgs;
        saturated = ("no shot noise to learn", _SAT_TIP * " A denoise model trained on them learns nothing useful."),
        not_photon_limited = ("little noise to learn", _NPL_TIP)))

register_param_validator!("cleanupImages.smooth", "channels",
    (v, imgs, _) -> channel_pick_advisory(v, imgs;
        not_photon_limited = ("smoothing changes little", _NPL_TIP)))

register_param_validator!("cleanupImages.afCorrect", "targetChannel",
    (v, imgs, _) -> channel_pick_advisory(v, imgs;
        saturated = ("clipped values are lost", _SAT_TIP * " AF correction subtracts from what was recorded and cannot restore it.")))
