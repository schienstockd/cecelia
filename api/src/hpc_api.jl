# ── HPC settings + connection test (Settings → HPC) — docs/todo/HPC_PLAN.md P1 ─────────────────
#
#   GET  /api/hpc/settings?projectUid=   → payload below
#   POST /api/hpc/settings/set           body {projectUid?, site?, identity?, project?: {projectUid, account}}
#                                        → payload below, or 400 {error} naming every problem
#   POST /api/hpc/test                   → the connection test on the SAVED settings
#
# Thin: validation and storage live in `app/src/config/hpc.jl`, the ssh call in
# `app/src/hpc/transport.jl`. The scheduler list is served (HPC_SCHEDULERS), never restated in Vue.

import JSON3

function _hpc_payload(project_uid::AbstractString)
    profile = active_profile_name()
    id = hpc_identity(profile)
    project = nothing
    if !isempty(project_uid)
        root = try load_project(String(project_uid)).root catch; "" end
        isempty(root) || (project = merge(Dict{String,Any}("projectUid" => project_uid),
                                          read_hpc_project_settings(root)))
    end
    Dict{String,Any}(
        "available"  => hpc_available(),
        "schedulers" => [Dict("name" => s.name, "label" => s.label, "available" => s.available)
                         for s in HPC_SCHEDULERS],
        "site"       => Cecelia.hpc_site_dict(hpc_site()),
        "identity"   => Dict{String,Any}("profile" => profile, "user" => id.user, "keyFile" => id.key_file),
        "project"    => project)
end

function api_hpc_settings_get(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    200, JSON3.write(_hpc_payload(get(q, "projectUid", "")))
end

function api_hpc_settings_set(body_bytes::Vector{UInt8})
    data = _parse_body(body_bytes)
    data isa Tuple && return data
    project_uid = _wstr(data, :projectUid)   # only scopes the reply's `project` block
    try
        site = get(data, :site, nothing)
        site isa AbstractDict && set_hpc_site!(site)
        ident = get(data, :identity, nothing)
        if ident isa AbstractDict
            u = get(ident, :user, nothing); k = get(ident, :keyFile, nothing)
            set_hpc_identity!(; user = isnothing(u) ? nothing : string(u),
                              key_file = isnothing(k) ? nothing : string(k))
        end
        proj = get(data, :project, nothing)
        if proj isa AbstractDict
            project_uid = _wstr(proj, :projectUid)
            isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
            root = try load_project(project_uid).root catch
                return 404, JSON3.write((; error = "Project not found"))
            end
            set_hpc_project_account!(root, _wstr(proj, :account))
        end
    catch e
        e isa ArgumentError && return 400, JSON3.write((; error = e.msg))
        rethrow()
    end
    200, JSON3.write(_hpc_payload(project_uid))
end

# Blocking for up to HPC_TEST_TIMEOUT_S; the router serves each request on its own task.
api_hpc_test(_body_bytes) = 200, JSON3.write(Cecelia.hpc_test_dict(hpc_test_connection()))
