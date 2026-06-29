# ── Chain event bus ───────────────────────────────────────────────────────────
# Package-level publish/subscribe for chain execution events.
# REPL users can subscribe directly for live feedback; the API WebSocket layer
# is just another subscriber — no special-casing.
#
# Events are NamedTuples. Currently fired:
#   "node:done"   (run_id, image_uid, node_id, result)
#
# Thread safety: the handler list is protected by _CHAIN_EVENTS_LOCK.
# Handlers are copied before dispatch so subscribe/unsubscribe during dispatch
# is safe (affects the next dispatch, not the current one).

const _CHAIN_EVENTS      = Dict{String, Vector{Function}}()
const _CHAIN_EVENTS_LOCK = ReentrantLock()

"""
Subscribe `handler` to chain events of `event_type`.
The handler receives a single NamedTuple argument with event-specific fields.
"""
function subscribe_chain_events!(event_type::String, handler::Function)
    lock(_CHAIN_EVENTS_LOCK) do
        push!(get!(() -> Function[], _CHAIN_EVENTS, event_type), handler)
    end
end

"""
Remove a previously-registered handler. No-op if not found.
"""
function unsubscribe_chain_events!(event_type::String, handler::Function)
    lock(_CHAIN_EVENTS_LOCK) do
        haskey(_CHAIN_EVENTS, event_type) &&
            filter!(h -> h !== handler, _CHAIN_EVENTS[event_type])
    end
end

"""
Fire an event to all registered handlers. Handler errors are logged but do not
propagate — a misbehaving subscriber must not disrupt the chain execution.
"""
function _fire_chain_event!(event_type::String, payload::NamedTuple)
    handlers = lock(_CHAIN_EVENTS_LOCK) do
        copy(get(_CHAIN_EVENTS, event_type, Function[]))
    end
    for h in handlers
        try
            Base.invokelatest(h, payload)
        catch e
            @warn "Chain event handler error" event_type exception=e
        end
    end
end
