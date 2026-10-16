# Inbound shape of ONE `afCombinations` entry as the frontend sends it: a numerator channel
# (`targetChannel`) plus the competitors that will contribute to its denominator. The frontend can
# send either scalars or single-element vectors; `channel_indices` normalises both, so this struct
# just captures what came in without pre-resolving.
struct AfCombinationSpec
    key::String                    # numerator's name-or-index as sent (used for output keys)
    targetChannel::Any             # scalar or Vector; resolved via channel_indices
    competingChannels::Any         # Vector of names/indices; resolved via channel_indices
    afChannels::Any                # AF-only reference channels (a mask, not a competitor)
    exclusive::Bool                # "Different cell types" — picks the bleedthrough estimator
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

**Within one combination a channel is a competitor OR an AF reference, never both** — a reference
zeroes the target where it is bright, a competitor splits the voxel by brightness, and applying both to
one channel would do the second job on top of the first. The form makes it a choice per channel
(`channelRoles`); in the older two-list shape a channel named in both is kept as the reference, the
more specific statement. ACROSS combinations it may differ — AF for one target, a competitor of
another — and the run handles that (`af_weight_stats`).

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
        # `channelRoles` (what the form sends): one role per OTHER channel, so within one target a
        # channel cannot both compete and be AF. The two lists are the older shape, still read for
        # saved chains; both are merged so neither silently wins.
        roles = get(entry, "channelRoles", nothing)
        roles = roles isa AbstractDict ? Dict{String,String}(string(c) => string(r) for (c, r) in roles) :
                                         Dict{String,String}()
        with_role(r) = sort([c for (c, v) in roles if v == r])
        as_list(v) = isnothing(v) ? Any[] : (v isa AbstractVector ? collect(Any, v) : Any[v])
        push!(specs, AfCombinationSpec(
            String(k),
            get(entry, "targetChannel", []),
            vcat(as_list(get(entry, "competingChannels", nothing)), with_role("competing")),
            vcat(as_list(get(entry, "afChannels", nothing)), with_role("af")),
            # only an explicit `false` turns it off — absent means the spec's default (on)
            get(entry, "exclusive", true) !== false))
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

        idx_refs = channel_indices(spec.afChannels, ch_names; what = "afChannels")

        # the target competes with the OTHERS, never with itself — see the docstring
        target_idx = tryparse(Int, combo_key)
        not_target(xs) = isnothing(target_idx) ? unique(xs) : filter(!=(target_idx), unique(xs))
        refs = not_target(idx_refs)
        af_combos[combo_key] = Dict{String,Any}(
            "competingChannels" => filter(c -> !(c in refs), not_target(idx_channels)),
            "afChannels" => refs,
            # Python reads it per combination (`af_correct_image`, the preview worker) and defaults
            # it to true when absent — so dropping it here made the toggle a no-op in both.
            "exclusive" => spec.exclusive)
    end
    af_combos
end
