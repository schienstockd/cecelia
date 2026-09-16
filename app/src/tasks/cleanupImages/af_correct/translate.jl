# Inbound shape of ONE `afCombinations` entry as the frontend sends it: a numerator channel
# (`targetChannel`) plus the competitors that will contribute to its denominator. The frontend can
# send either scalars or single-element vectors; `channel_indices` normalises both, so this struct
# just captures what came in without pre-resolving.
struct AfCombinationSpec
    key::String                    # numerator's name-or-index as sent (used for output keys)
    targetChannel::Any             # scalar or Vector; resolved via channel_indices
    competingChannels::Any         # Vector of names/indices; resolved via channel_indices
end
"""
    af_combinations_for_python(params, raw) -> Dict

The `afCombinations` bag as the PYTHON side needs it: `competingChannels` resolved from channel NAMES
to 0-based indices, and `targetChannel` resolved to the combination's key (the channel being
corrected). Names come from the **default** version deliberately — a corrected variant inherits them
by versioned fallback and may carry no list of its own.

**Shared by the run and the task preview, and it has to be.** Exactly the shape of the cellpose bug
(`cellpose_models_for_python`): the frontend sends names, Python wants indices, and the preview sends
the frontend's params. Real saved params on a live project carry
`competingChannels = ["CH4", "CD169-Kat"]` where `"CH4"` is not in that image's `imChannelNames` at all
— a stale name that silently resolves to nothing. Keeping one translator means the preview drops it
exactly where the run does, rather than the two disagreeing about which channels were used.

**The target is dropped from its own competitor list.** It is already the numerator's channel, so
naming it again would square its term into the denominator twice and quietly halve the channel's own
output. Silently dropped rather than rejected because the two lists are separate widgets and picking
the same channel in both is an easy slip with one obvious intent.
"""
# Parse the raw params bag's `afCombinations` map into typed `AfCombinationSpec` values, keyed by
# the entry's original key (the frontend's numerator label). Missing or malformed entries are
# skipped rather than raising — the frontend's UI can produce partial entries the user is still
# editing.
function parse_af_combinations(params::AbstractDict)::Vector{AfCombinationSpec}
    raw = get(params, "afCombinations", nothing)
    (isnothing(raw) || !(raw isa AbstractDict)) && return AfCombinationSpec[]
    specs = AfCombinationSpec[]
    for (k, v) in raw
        v isa AbstractDict || continue
        entry = Dict{String,Any}(String(ck) => cv for (ck, cv) in v)
        push!(specs, AfCombinationSpec(
            String(k),
            get(entry, "targetChannel", []),
            get(entry, "competingChannels", [])))
    end
    specs
end

function af_combinations_for_python(params::AbstractDict, raw::AbstractDict)::Dict{String,Any}
    ch_names = ccid_channel_names(raw)

    af_combos = Dict{String,Any}()
    for spec in parse_af_combinations(params)
        # competingChannels / targetChannel → 0-based indices via the one resolver (model/image.jl).
        # Idempotent on integers, so a REPL or chain caller may hand back a converted dict; `unique`
        # is the resolver's default because a channel named twice would square its term into the
        # weight's denominator a second time.
        idx_channels = channel_indices(spec.competingChannels, ch_names;
                                       what = "competingChannels")

        target_sel = channel_indices(spec.targetChannel, ch_names; what = "targetChannel")
        combo_key  = isempty(target_sel) ? spec.key : string(first(target_sel))

        # the target competes with the OTHERS, never with itself — see the docstring
        target_idx = tryparse(Int, combo_key)
        af_combos[combo_key] = Dict{String,Any}(
            "competingChannels" => isnothing(target_idx) ? unique(idx_channels) :
                                   filter(!=(target_idx), unique(idx_channels)))
    end
    af_combos
end
