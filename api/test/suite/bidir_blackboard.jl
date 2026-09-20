# BIDIR Part 4 blackboard CRUD testset — extracted from api/test/runtests.jl.
#
# One large testset covering the Blackboard storage discipline end-to-end (docs/todo/BIDIR_CONTEXT_PLAN.md
# Part 4): Markdown entries with snapshot-per-revise history and attached captureIds. Frontend
# page + MCP tools are separate follow-ups.
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

@testset "API: blackboard CRUD + versioning + attachments (BIDIR Part 4)" begin
    # docs/todo/BIDIR_CONTEXT_PLAN.md Part 4. Backend of the Blackboard: Markdown entries with a
    # snapshot-per-revise history and attached captureIds. Frontend page + MCP tools are separate
    # follow-ups; this testset covers the storage discipline end-to-end.
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        uid = "TESTBB"; mkpath(joinpath(tmp, uid))
        # A captured frame lives on disk so an attachment reference resolves against something real.
        # `_clean_attachments` reads the captures dir directly; we just need the folder shape.
        cap_id  = "cap-20260101T000000-aaaaaa"
        cap_id2 = "cap-20260101T000001-bbbbbb"      # a second, for versioned-attachments tests
        mkpath(joinpath(tmp, uid, "captures", cap_id))
        mkpath(joinpath(tmp, uid, "captures", cap_id2))
        w(path, b) = _post(path, b)

        # ── Guards on create ─────────────────────────────────────────────────
        @test w(api_blackboard_create, Dict("title"=>"t"))[1] == 400
        @test w(api_blackboard_create, Dict("projectUid"=>"NOPE", "title"=>"t"))[1] == 404
        @test w(api_blackboard_create, Dict("projectUid"=>uid, "title"=>""))[1] == 400
        # Content size cap — anything past 100 KiB is rejected before write.
        oversized = repeat("x", 100 * 1024 + 1)
        @test w(api_blackboard_create, Dict("projectUid"=>uid, "title"=>"t", "content"=>oversized))[1] == 400

        # ── Create ───────────────────────────────────────────────────────────
        # Attachments mix: one valid captureId (kept), one made-up (dropped because the dir is
        # missing), one malformed (dropped by the id regex before path resolution). Duplicate is
        # collapsed. The stored list must reflect what actually exists.
        st_c, body_c = w(api_blackboard_create, Dict("projectUid"=>uid,
            "title"=>"Chain design for MERTK sample",
            "content"=>"# Working notes\n\nDiscuss segmentation w/ Claude.",
            "attachments"=>[cap_id, "cap-19700101T000000-000000", "../../../etc/passwd", cap_id]))
        @test st_c == 200
        eid = String(JSON3.read(body_c).entryId)
        @test occursin(r"^bb-[0-9]{8}T[0-9]{6}-[0-9a-f]{6}$", eid)
        entry_dir = joinpath(tmp, uid, "blackboard", eid)
        @test isdir(entry_dir) && isfile(joinpath(entry_dir, "entry.md"))
        @test isfile(joinpath(entry_dir, "meta.json"))
        # Registry now knows about it.
        reg_path = joinpath(tmp, uid, "settings", "blackboard.json")
        reg = JSON3.read(read(reg_path, String), Dict{String,Any})
        @test haskey(reg, eid) && String(reg[eid]["title"]) == "Chain design for MERTK sample"

        # ── List — newest-first, attachmentsCount surfaced ───────────────────
        st_l, body_l = api_blackboard_list(HTTP.Request("GET",
            "/api/blackboard?projectUid=$uid"))
        @test st_l == 200
        entries = JSON3.read(body_l).entries
        @test length(entries) == 1
        @test String(entries[1].entryId) == eid
        @test entries[1].current == 0                # never snapshotted yet
        @test entries[1].attachmentsCount == 1       # dedup + validation kept one of four

        # ── Read live ────────────────────────────────────────────────────────
        st_e, body_e = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test st_e == 200
        e = JSON3.read(body_e).entry
        @test occursin("Working notes", String(e.content))
        @test isempty(e.versions)                    # no snapshots yet
        @test String(e.attachments[1]) == cap_id

        # ── Revise → v1 snapshot of prior content, live now = new content ──
        # Second revise flips attachments only (from [cap_id] to [cap_id2]) — this verifies both
        # halves of the state (markdown AND attachments) are versioned, so a later read at v2 must
        # see the OLD attachments (cap_id), not the current (cap_id2).
        st_r1, body_r1 = w(api_blackboard_revise, Dict("projectUid"=>uid, "entryId"=>eid,
            "content"=>"# Working notes (v2)\n\nNow with a Mermaid diagram."))
        @test st_r1 == 200
        @test JSON3.read(body_r1).version == 1
        # Snapshot v1 is the OLD content; live is the new. Attachments unchanged so v1's recorded
        # atts equal the live atts (still [cap_id]).
        st_v1, body_v1 = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid&version=1"))
        @test st_v1 == 200
        @test occursin("Working notes\n", String(JSON3.read(body_v1).entry.content))
        @test String(JSON3.read(body_v1).entry.attachments[1]) == cap_id
        st_live, body_live = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test occursin("(v2)", String(JSON3.read(body_live).entry.content))
        @test JSON3.read(body_live).entry.current == 1

        # Second revise: same content is fine, DIFFERENT attachments. Must NOT be a no-op — the
        # attachment change is a real diff — and v2's recorded attachments must be the OLD set
        # ([cap_id]), not the new one.
        st_r2, body_r2 = w(api_blackboard_revise, Dict("projectUid"=>uid, "entryId"=>eid,
            "content"=>"# Working notes (v3)", "attachments"=>[cap_id2]))
        @test st_r2 == 200
        @test JSON3.read(body_r2).version == 2
        st_l2, body_l2 = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test JSON3.read(body_l2).entry.current == 2
        @test sort(collect(JSON3.read(body_l2).entry.versions)) == [1, 2]
        @test String(JSON3.read(body_l2).entry.attachments[1]) == cap_id2  # current = new
        st_v2, body_v2 = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid&version=2"))
        @test String(JSON3.read(body_v2).entry.attachments[1]) == cap_id   # v2 record = OLD

        # ── No-op revise: same content, same attachments ⇒ unchanged:true, no new snapshot ─
        st_no, body_no = w(api_blackboard_revise, Dict("projectUid"=>uid, "entryId"=>eid,
            "content"=>"# Working notes (v3)", "attachments"=>[cap_id2]))
        @test st_no == 200
        @test JSON3.read(body_no).unchanged == true
        @test JSON3.read(body_no).version == 2
        st_l_noop, body_l_noop = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        # Version list did NOT grow — a repeat-with-same-payload doesn't spend a snapshot.
        @test sort(collect(JSON3.read(body_l_noop).entry.versions)) == [1, 2]

        # ── Restore v1 → snapshots current first (as v3), then restores v1 ──
        # Restore brings back BOTH the markdown AND the attachment set recorded for v1 (cap_id).
        st_re, body_re = w(api_blackboard_restore, Dict("projectUid"=>uid, "entryId"=>eid,
            "version"=>"1"))
        @test st_re == 200
        @test JSON3.read(body_re).version == 1
        st_after, body_after = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        e_after = JSON3.read(body_after).entry
        @test occursin("Working notes\n", String(e_after.content))    # matches v1's content
        @test e_after.current == 1
        @test String(e_after.attachments[1]) == cap_id                 # matches v1's atts
        # The un-snapshotted-before-restore content is now v3 — critical: we can undo the restore.
        @test sort(collect(e_after.versions)) == [1, 2, 3]
        # And v3 records the STATE THAT WAS LIVE right before this restore fired — cap_id2.
        st_v3, body_v3 = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid&version=3"))
        @test String(JSON3.read(body_v3).entry.attachments[1]) == cap_id2

        # Restore unknown version ⇒ 404
        @test w(api_blackboard_restore, Dict("projectUid"=>uid, "entryId"=>eid,
            "version"=>"99"))[1] == 404

        # ── Prune to keep 2 most recent ─────────────────────────────────────
        st_p, body_p = w(api_blackboard_prune, Dict("projectUid"=>uid, "entryId"=>eid, "keep"=>"2"))
        @test st_p == 200 && JSON3.read(body_p).pruned == 1
        st_prune, body_prune = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test sort(collect(JSON3.read(body_prune).entry.versions)) == [2, 3]

        # ── Delete ───────────────────────────────────────────────────────────
        st_d, body_d = w(api_blackboard_delete, Dict("projectUid"=>uid, "entryId"=>eid))
        @test st_d == 200 && JSON3.read(body_d).deleted == true
        @test !isdir(entry_dir)
        st_d2, body_d2 = w(api_blackboard_delete, Dict("projectUid"=>uid, "entryId"=>eid))
        @test st_d2 == 200 && JSON3.read(body_d2).deleted == false  # idempotent

        # ── Read guards ─────────────────────────────────────────────────────
        @test api_blackboard_entry_get(HTTP.Request("GET", "/api/blackboard/entry"))[1] == 400
        @test api_blackboard_entry_get(HTTP.Request("GET", "/api/blackboard/entry?projectUid=$uid"))[1] == 400
        @test api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=nope"))[1] == 400
        @test api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=bb-20260101T000000-abcdef"))[1] == 404
        @test api_blackboard_list(HTTP.Request("GET", "/api/blackboard"))[1] == 400
        # Traversal via entryId is rejected by the regex before path composition.
        @test w(api_blackboard_delete, Dict("projectUid"=>uid, "entryId"=>"../../etc/passwd"))[1] == 400
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end
