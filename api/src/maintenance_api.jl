# Data-patch catalogue for the Settings → Data patches section. Listing only — running a patch is a
# WS action (maintenance:run, streamed over the task rail; see sockets.jl). One project at a time.
function api_maintenance_patches(req::HTTP.Request)
    patches = [(; id = p.id, title = p.title, description = p.description) for p in maintenance_patches()]
    200, JSON3.write((; patches = patches))
end
