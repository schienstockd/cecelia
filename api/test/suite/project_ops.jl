# Project-ops testsets (lab log, chain, rename, delete, object find) — extracted from api/test/runtests.jl.
#
# Six testsets covering the mutation/discovery API surface a project owner reaches for:
#  - `API: lab log`
#  - `API: chain create (create-only + validated) and rename`
#  - `API: set rename`
#  - `API: project delete`
#  - `API: deleting a label set sweeps its tracks/branch/cluster companions`
#  - `API: object find (a uid with no project in hand)`
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

@testset "API: lab log" begin
    # Redirect projects_dir() → a temp dir so we never touch the real dev projects dir.
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir()
    dirs["projects"] = tmp
    try
        proj = create_project!(name="api-lablog")
        uid  = proj.uid
        read_ll() = JSON3.read(api_lablog_read(HTTP.Request("GET", "/api/lablog?projectUid=$uid"))[2])

        # empty to start
        r0 = read_ll()
        @test r0.content == "" && length(r0.entries) == 0

        # bad requests
        @test api_lablog_read(HTTP.Request("GET", "/api/lablog"))[1] == 400              # projectUid missing
        @test _post(api_lablog_append, Dict("projectUid"=>uid))[1] == 400                # author+lines missing
        @test _post(api_lablog_append, Dict("author"=>"User","lines"=>"x"))[1] == 400    # projectUid missing
        @test _post(api_lablog_append, Dict("projectUid"=>"nope","author"=>"User","lines"=>"x"))[1] == 404

        # append accepts a string OR an array; server injects the date + author tag
        @test _post(api_lablog_append, Dict("projectUid"=>uid,"author"=>"User","lines"=>"single line"))[1] == 200
        st, body = _post(api_lablog_append, Dict("projectUid"=>uid,"author"=>"Claude","lines"=>["a","b"]))
        @test st == 200
        j = JSON3.read(body)
        @test startswith(j.block, "## ") && occursin("[Claude]", j.block)
        @test length(j.entries) == 2 && j.entries[1].author == "Claude"   # newest-first

        # empty/whitespace-only content rejected by append_lab_log! → 400
        @test _post(api_lablog_append, Dict("projectUid"=>uid,"author"=>"User","lines"=>["   "]))[1] == 400

        # read reflects appends
        r = read_ll()
        @test occursin("[User]", r.content) && occursin("[Claude]", r.content)
        @test length(r.entries) == 2

        # [LabArchives] is a PROVENANCE claim and the caller picks it, so the server makes the one check
        # it honestly can: no linked notebook ⇒ no notebook provenance. 409 with an actionable message.
        st_la, body_la = _post(api_lablog_append,
            Dict("projectUid"=>uid,"author"=>"LabArchives","lines"=>["from the ELN"]))
        @test st_la == 409
        @test occursin("set_labarchives_context", JSON3.read(body_la).error)
        @test !occursin("[LabArchives]", read_ll().content)          # and nothing was written
        # …every other author is unaffected by the guard
        @test _post(api_lablog_append, Dict("projectUid"=>uid,"author"=>"Claude","lines"=>["ok"]))[1] == 200

        # once a notebook IS linked, the same append is accepted
        write_la_doc!(load_project(uid); source = Dict("notebookName" => "Ailsa"),
                      sections = [Dict("heading" => "Setup", "lines" => ["x"])])
        st_ok, body_ok = _post(api_lablog_append,
            Dict("projectUid"=>uid,"author"=>"LabArchives","lines"=>["from the ELN"]))
        @test st_ok == 200 && occursin("[LabArchives]", JSON3.read(body_ok).block)


        # ── capture (auto [Cecelia] activity digest) ──
        # no task activity yet → captured=false, nothing appended
        let cap = JSON3.read(_post(api_lablog_capture, Dict("projectUid"=>uid))[2])
            @test cap.ok == true && cap.captured == false
        end
        # add run-log activity, then capture → captured=true with a [Cecelia] entry
        let s = add_set!(proj; name="set-A"),
            img = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))
            append_run_log!(img, "segment.cellpose", "default")
            cap = JSON3.read(_post(api_lablog_capture, Dict("projectUid"=>uid))[2])
            @test cap.captured == true
            @test occursin("[Cecelia]", cap.block)
            @test any(e -> e.author == "Cecelia", cap.entries)
            @test occursin(img.uid, cap.block) && !occursin("img-1", cap.block)   # digest refs by uid
            # read exposes the uid→name map the "Show names" toggle resolves against
            let r = read_ll()
                @test Symbol(img.uid) in propertynames(r.imageNames) && getproperty(r.imageNames, Symbol(img.uid)) == "img-1"
            end
        end
        # bad requests
        @test _post(api_lablog_capture, Dict())[1] == 400              # projectUid missing
        @test _post(api_lablog_capture, Dict("projectUid"=>"nope"))[1] == 404

        # ── dismiss (hide an entry → config sidecar; the log file stays append-only) ──
        let d = JSON3.read(_post(api_lablog_dismiss, Dict("projectUid"=>uid, "id"=>"ff00aa", "dismissed"=>true))[2])
            @test d.ok == true && "ff00aa" in d.dismissed
        end
        @test "ff00aa" in JSON3.read(api_lablog_read(HTTP.Request("GET", "/api/lablog?projectUid=$uid"))[2]).dismissed  # surfaced on read
        let d = JSON3.read(_post(api_lablog_dismiss, Dict("projectUid"=>uid, "id"=>"ff00aa", "dismissed"=>false))[2])
            @test !("ff00aa" in d.dismissed)                                                             # un-hidden
        end
        @test _post(api_lablog_dismiss, Dict("projectUid"=>uid, "dismissed"=>true))[1] == 400            # id missing
        @test _post(api_lablog_dismiss, Dict("id"=>"x", "dismissed"=>true))[1] == 400                    # projectUid missing

        # An explicit JSON null on a required field is ordinary — `String(get(body,:x,""))` and
        # `Bool(get(body,:x,false))` USED to crash (MethodError: no method matching String(::Nothing)),
        # aborting the handler with no status frame. `_wstr` / `_wbool` (sockets.jl) absorb that at
        # the boundary; every lab_log handler now reads through them.
        @test _post(api_lablog_dismiss, Dict("projectUid"=>nothing, "id"=>"x", "dismissed"=>true))[1] == 400
        @test _post(api_lablog_dismiss, Dict("projectUid"=>uid,      "id"=>nothing, "dismissed"=>true))[1] == 400
        # a null Bool takes the default — dismissed = false — and reaches the handler cleanly
        @test _post(api_lablog_dismiss, Dict("projectUid"=>uid, "id"=>"aa11bb", "dismissed"=>nothing))[1] == 200
        @test _post(api_lablog_append,  Dict("projectUid"=>nothing, "author"=>"User", "lines"=>"x"))[1] == 400
        @test _post(api_lablog_append,  Dict("projectUid"=>uid,      "author"=>nothing, "lines"=>"x"))[1] == 400
        @test _post(api_lablog_capture, Dict("projectUid"=>nothing))[1] == 400
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive=true, force=true)
    end
end

# Chain authoring from outside the whiteboard (Claude via the MCP) + rename. The two properties that
# matter: create NEVER overwrites a chain the user wired, and an invalid template is rejected HERE
# rather than mid-run, after the user pressed Run on something they didn't write.
@testset "API: chain create (create-only + validated) and rename" begin
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name="api-chains")
        uid  = proj.uid
        rm_params = Dict("valueName"=>"default", "newDefault"=>"default")
        node(id) = Dict("id"=>id, "fn"=>"importImages.remove", "params"=>rm_params)
        tmpl(name, nodes, edges) = Dict("name"=>name, "nodes"=>nodes, "edges"=>edges)
        create(t) = _post(api_chains_create, Dict("projectUid"=>uid, "template"=>t))
        path(name) = joinpath(tmp, uid, "settings", "chains", "$(name).json")

        # guards
        @test create(nothing)[1] == 400
        @test _post(api_chains_create, Dict("projectUid"=>uid))[1] == 400            # no template
        @test _post(api_chains_create, Dict("template"=>tmpl("x", [node("n1")], [])))[1] == 400
        @test create(tmpl("", [node("n1")], []))[1] == 400                           # no name
        @test _post(api_chains_create,
                    Dict("projectUid"=>"nope",
                         "template"=>tmpl("x", [node("n1")], [])))[1] == 404

        # a name is a filename — path traversal must not resolve anywhere
        for bad in ("../../evil", "a/b", "..", ".hidden")
            @test create(tmpl(bad, [node("n1")], []))[1] == 400
        end

        # happy path
        st, body = create(tmpl("pipeline", [node("n1"), node("n2")],
                               [Dict("from"=>"n1", "to"=>"n2")]))
        @test st == 200
        @test JSON3.read(body).nodeCount == 2
        @test isfile(path("pipeline"))

        # CREATE-ONLY: the whole point — an outside author cannot replace the user's chain
        @test create(tmpl("pipeline", [node("n1")], []))[1] == 409

        # …while the whiteboard's own save still overwrites (it is the user's own edit, not an
        # outside author's) — now validated too, see the "chain save" testset below
        @test _post(api_chains_save,
                    Dict("projectUid"=>uid, "template"=>tmpl("pipeline", [node("n9")], [])))[1] == 200

        # VALIDATION, and the message names the offender so the author can fix it
        st, body = create(tmpl("bad-fn", [Dict("id"=>"oops", "fn"=>"importImages.nope")], []))
        @test st == 400
        err = String(JSON3.read(body).error)
        @test occursin("oops", err) && occursin("importImages.nope", err)
        @test create(tmpl("dangling", [node("n1")], [Dict("from"=>"n1","to"=>"ghost")]))[1] == 400
        @test create(tmpl("cyclic", [node("n1"), node("n2")],
                          [Dict("from"=>"n1","to"=>"n2"), Dict("from"=>"n2","to"=>"n1")]))[1] == 400
        @test !isfile(path("bad-fn"))          # a rejected template leaves nothing on disk

        # SPARSE params are accepted — an outside author sets only what it means to; the whiteboard
        # fills the rest from the spec defaults when it loads the template.
        @test create(tmpl("sparse", [Dict("id"=>"n1", "fn"=>"tracking.bayesian_tracking",
                                          "params"=>Dict("maxSearchRadius"=>35))], []))[1] == 200

        # startTargets is FILLED with the roots when the author omits it. Without it the whiteboard
        # draws no start dot (buildStartGraph returns null with no target and no saved position), so the
        # chain opens with nothing marking where a run begins — which is how the first authored chain
        # reached the user. Execution is unchanged either way; this is for the editor.
        @test create(tmpl("pipeline-is-rooted", [node("first"), node("second")],
                          [Dict("from"=>"first", "to"=>"second")]))[1] == 200
        @test JSON3.read(read(path("pipeline-is-rooted"), String)).startTargets == ["first"]
        # an explicit startTargets is respected (starting a run part-way in)
        rooted = tmpl("pipeline-mid", [node("a"), node("b")], [Dict("from"=>"a", "to"=>"b")])
        rooted["startTargets"] = ["b"]
        @test create(rooted)[1] == 200
        @test JSON3.read(read(path("pipeline-mid"), String)).startTargets == ["b"]

        # ── rename ──
        ren(from, to) = _post(api_chains_rename,
                              Dict("projectUid"=>uid, "name"=>from, "newName"=>to))
        @test ren("pipeline", "")[1] == 400                     # newName required
        @test ren("ghost", "whatever")[1] == 404                # source must exist
        @test ren("pipeline", "sparse")[1] == 409               # target must not
        @test ren("pipeline", "../evil")[1] == 400              # guarded on both names
        @test ren("pipeline", "pipeline")[1] == 200             # no-op, not an error

        st, body = ren("pipeline", "pipeline-v2")
        @test st == 200 && String(JSON3.read(body).name) == "pipeline-v2"
        @test isfile(path("pipeline-v2")) && !isfile(path("pipeline"))
        # the `name` FIELD moves too — else the whiteboard saves the renamed chain back under the old
        # name and the rename silently undoes itself on the next save
        @test String(JSON3.read(read(path("pipeline-v2"), String)).name) == "pipeline-v2"
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive=true, force=true)
    end
end

@testset "API: set rename" begin
    # Redirect projects_dir() → a temp dir so we never touch the real dev projects dir.
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name="api-set-rename")
        uid  = proj.uid
        # BOTH sets up front, before the first handler call. `add_set!` ends in `save!(proj)`, which
        # cascades `save!` over every set loaded in THIS (now stale) project object — so building a
        # fixture in the middle would rewrite a name a handler had just committed from its own fresh
        # load, and the test would be asserting against its own clobber. (Same trap a long-lived REPL
        # session has; the handlers are safe because each request loads the project fresh.)
        s     = add_set!(proj; name="before")
        other = add_set!(proj; name="sibling")
        img   = add_image!(s; name="im")

        ren(args...) = _post(api_sets_rename, Dict(args...))
        cre(args...) = _post(api_sets_create, Dict(args...))

        # CREATE carries the same two rules as rename now — it was the one set route that neither
        # trimmed nor checked the name, so " sibling " and "sibling" could become two sets whose picker
        # rows are indistinguishable.
        @test cre("projectUid"=>uid, "name"=>"sibling")[1] == 409                # taken
        @test cre("projectUid"=>uid, "name"=>"  sibling  ")[1] == 409            # trimmed, then checked
        @test cre("projectUid"=>uid, "name"=>"   ")[1] == 400                    # whitespace-only = empty
        st_c, body_c = cre("projectUid"=>uid, "name"=>"  fresh  ")
        @test st_c == 200 && String(JSON3.read(body_c).name) == "fresh"          # stored trimmed…
        @test init_object(uid, String(JSON3.read(body_c).uid)).name == "fresh"   # …on disk, not just echoed

        @test ren("setUid"=>s.uid, "name"=>"x")[1] == 400                      # projectUid required
        @test ren("projectUid"=>uid, "name"=>"x")[1] == 400                    # setUid required
        @test ren("projectUid"=>uid, "setUid"=>s.uid)[1] == 400                # name required
        # whitespace-only is empty: a blank row in the picker cannot be told from a bug
        @test ren("projectUid"=>uid, "setUid"=>s.uid, "name"=>"   ")[1] == 400
        @test ren("projectUid"=>"nope", "setUid"=>s.uid, "name"=>"x")[1] == 404
        @test ren("projectUid"=>uid, "setUid"=>"ghost", "name"=>"x")[1] == 404

        st, body = ren("projectUid"=>uid, "setUid"=>s.uid, "name"=>"  after  ")
        @test st == 200
        @test String(JSON3.read(body).name) == "after"                         # trimmed, echoed back
        @test String(JSON3.read(body).uid) == s.uid                            # identity unchanged

        # The name guard is the MODEL's (`set_name_taken`), so a REPL caller gets it too; the handler
        # maps it to the 409 `api_chains_rename` already uses for a taken target.
        @test ren("projectUid"=>uid, "setUid"=>other.uid, "name"=>"after")[1] == 409
        @test init_object(uid, other.uid).name == "sibling"                    # refused, nothing written
        # …and the trim happens BEFORE the check, else " after " would slip a duplicate past it
        @test ren("projectUid"=>uid, "setUid"=>other.uid, "name"=>" after ")[1] == 409
        # renaming a set to its OWN name is a 200 no-op, which keeps a re-run idempotent
        @test ren("projectUid"=>uid, "setUid"=>s.uid, "name"=>"after")[1] == 200
        # …and it is on DISK, with the membership intact — the handler commits one field, so an image
        # dropping out here would mean it wrote the whole object back from a stale read.
        reloaded = init_object(uid, s.uid)
        @test reloaded.name == "after" && reloaded.image_uids == [img.uid]
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive=true, force=true)
    end
end

@testset "API: project delete" begin
    # Redirect projects_dir() → a temp dir so we never touch the real dev projects dir.
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name="api-del")
        uid  = proj.uid
        @test isdir(proj.root)

        @test _post(api_projects_delete, Dict())[1] == 400                    # uid missing
        @test _post(api_projects_delete, Dict("uid"=>"nope"))[1] == 404       # not found
        st, body = _post(api_projects_delete, Dict("uid"=>uid))
        @test st == 200 && JSON3.read(body).ok == true
        @test !isdir(proj.root)                                               # gone from disk
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive=true, force=true)
    end
end

# Deleting a label set must take its COMPANIONS with it. Before this, the route removed `labels[vn]`
# and `label_props[vn]` and left `{vn}__tracks.h5ad`, `{vn}__branch.h5ad`, the branch zarr and the
# clustfeatures sidecars behind as files nothing could reach — invisible, and counted as analysis
# forever. The prefix rule is what makes it complete; the "B2 survives" case is what stops it being
# too greedy.
@testset "API: deleting a label set sweeps its tracks/branch/cluster companions" begin
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name="api-del-labels")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="a")

        labels_dir = joinpath(img._dir, "labels");       mkpath(labels_dir)
        branch_dir = joinpath(img._dir, "branchLabels"); mkpath(branch_dir)
        props_dir  = joinpath(img._dir, "labelProps");   mkpath(props_dir)
        mkpath(joinpath(labels_dir, "B.zarr")); write(joinpath(labels_dir, "B.zarr", "c"), "x")
        mkpath(joinpath(branch_dir, "B.zarr")); write(joinpath(branch_dir, "B.zarr", "c"), "x")
        for f in ("B.h5ad", "B__tracks.h5ad", "B__branch.h5ad",
                  "B.clustfeatures.json", "B__tracks.clustfeatures.json",
                  "B2.h5ad")                                  # B2 must NOT be swept by the "B" prefix
            write(joinpath(props_dir, f), "x")
        end
        img.labels        = Dict("B"=>["B.zarr"], "B2"=>["B2.zarr"])
        img.label_props   = Dict("B"=>"B.h5ad", "B2"=>"B2.h5ad")
        img.branch_labels = Dict("B"=>["B.zarr"])
        save!(img)

        st, body = _post(api_images_delete_labels,
                         Dict("projectUid"=>proj.uid, "imageUid"=>img.uid, "valueName"=>"B"))
        @test st == 200 && JSON3.read(body).ok == true

        @test !ispath(joinpath(labels_dir, "B.zarr"))                       # cell labels
        @test !ispath(joinpath(branch_dir, "B.zarr"))                       # branch labels — the gap
        for f in ("B.h5ad", "B__tracks.h5ad", "B__branch.h5ad",
                  "B.clustfeatures.json", "B__tracks.clustfeatures.json")
            @test !isfile(joinpath(props_dir, f))
        end
        @test isfile(joinpath(props_dir, "B2.h5ad"))                        # a sibling name survives

        ri = init_object(proj.uid, img.uid)
        @test !haskey(ri.labels, "B") && !haskey(ri.branch_labels, "B")     # registrations cleared
        @test !haskey(ri.label_props, "B")
        @test haskey(ri.labels, "B2")                                       # B2 still registered
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive=true, force=true)
    end
end

@testset "API: object find (a uid with no project in hand)" begin
    # Redirect projects_dir() → a temp dir so we never touch the real dev projects dir.
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        pa = create_project!(name="api-find-alpha")
        sa = add_set!(pa; name="set-A")
        ia = add_image!(sa; name="shared-name")
        pb = create_project!(name="api-find-beta")
        sb = add_set!(pb; name="set-B")
        ib = add_image!(sb; name="shared-name")
        ic = add_image!(sb; name="unique-image")

        find(q) = JSON3.read(api_objects_find(HTTP.Request("GET", "/api/objects/find?$q"))[2])

        @test api_objects_find(HTTP.Request("GET", "/api/objects/find"))[1] == 400          # q required
        @test api_objects_find(HTTP.Request("GET", "/api/objects/find?q=%20%20"))[1] == 400 # whitespace-only

        # THE point of the route: a bare image uid resolves to its project without one being supplied.
        let r = find("q=$(ic.uid)")
            @test r.matchedBy == "uid" && r.count == 1
            m = r.matches[1]
            @test m.kind == "image" && m.uid == ic.uid && m.name == "unique-image"
            @test m.projectUid == pb.uid && m.projectName == "api-find-beta"
            @test m.setUid == sb.uid && m.setName == "set-B"
            @test m.included == true
        end
        # …and it does not stop at images: a set uid and a project uid answer through the same call.
        let r = find("q=$(sb.uid)")
            @test r.matchedBy == "uid" && r.count == 1
            @test r.matches[1].kind == "set" && r.matches[1].imageCount == 2
        end
        let r = find("q=$(pa.uid)")
            @test r.matchedBy == "uid" && r.count == 1
            @test r.matches[1].kind == "project" && r.matches[1].projectUid == pa.uid
        end
        # A uid must not be matched case-insensitively or as a fragment — uids are exact, and a
        # near-miss falling through to the name pass would answer a different question silently.
        # THE PLATFORM TRAP: macOS/Windows filesystems are case-insensitive, so the handler's
        # `{proj}/1/{uid}` existence check answers yes for a wrong-case uid and `init_object` then
        # reads the real object through that path. This passed on Linux and failed on the other two
        # until the handler re-checked `obj.uid == q` — the match is the string comparison, the stat
        # is only a pre-filter. `gen_uid` mixes case, so flipping it gives a genuinely different key.
        let variant = any(islowercase, ic.uid) ? uppercase(ic.uid) : lowercase(ic.uid)
            # An all-digit uid has no case to flip (1 in ~55k of `gen_uid`); skip rather than assert
            # a vacuous inequality, so the suite can't flake on the RNG.
            variant == ic.uid ? (@test_skip find("q=$variant").count == 0) :
                                (@test      find("q=$variant").count == 0)
        end
        @test find("q=no-such-uid").count == 0

        # Name pass: case-insensitive substring, across EVERY project — the same name in two projects
        # is two answers, not a guess at which one was meant.
        let r = find("q=SHARED")
            @test r.matchedBy == "name" && r.count == 2
            @test Set(m.projectUid for m in r.matches) == Set([pa.uid, pb.uid])
            @test all(m -> m.kind == "image" && m.name == "shared-name", r.matches)
        end
        @test find("q=set-").count == 2                      # sets match by name too
        @test find("q=api-find-alpha").matches[1].kind == "project"

        # A capped list must SAY it was capped, or the caller reads a trimmed list as the whole answer.
        let r = find("q=shared-name&limit=1")
            @test r.count == 1 && r.truncated == true
        end
        @test find("q=shared-name").truncated == false

        # Read-only, like GET /api/images: no lastOpenedAt bump, nothing rewritten. This is what lets
        # the observer call it while keeping its no-mutation guarantee.
        let f = joinpath(tmp, pb.uid, "project.json"), before = read(f, String)
            find("q=$(ib.uid)"); find("q=shared")
            @test read(f, String) == before
        end
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive=true, force=true)
    end
end

