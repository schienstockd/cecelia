# Blackboard testsets — BIDIR Part 4 storage discipline (docs/todo/BIDIR_CONTEXT_PLAN.md) plus the
# PROJECT_MEMORY_PLAN P1/P2 additions that ride on the same store:
#   • CRUD + versioning + attachments (BIDIR Part 4)
#   • status field + reserved `profile` entry           (PROJECT_MEMORY_PLAN P1 — Decisions 2, 3)
#   • substring search over titles + bodies             (PROJECT_MEMORY_PLAN P2 — Decision 4)
#
# All three sit here so a future blackboard change touches ONE suite file, not three. Extracted so
# runtests.jl contains only include lines + section-header comments — same shape as app/test/suite/*.jl.

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
        # The list endpoint auto-creates the reserved `profile` entry (PROJECT_MEMORY_PLAN P1); the
        # BIDIR-shape check filters it out so this testset only asserts on the entries this test
        # created. `profile` sorts before `bb-…` in DESC (p > b), hence position [1]. Profile
        # auto-creation itself has its own testset below (MEMORY P1).
        st_l, body_l = api_blackboard_list(HTTP.Request("GET",
            "/api/blackboard?projectUid=$uid"))
        @test st_l == 200
        all_entries = JSON3.read(body_l).entries
        entries = [e for e in all_entries if String(e.entryId) != "profile"]
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

# ── Status field + reserved profile entry (PROJECT_MEMORY_PLAN P1) ──────────────────────────────
# Decisions 2 (reserved `profile` id, auto-created on first list) and 3 (status: open/resolved/parked
# on every entry; missing backfills as `open`; status changes travel independently of content
# revisions, no snapshot). The status endpoint is the new /api/blackboard/status handler.
@testset "API: blackboard status field + reserved profile entry (MEMORY P1)" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        uid = "TESTBBP1"; mkpath(joinpath(tmp, uid))
        w(path, b) = _post(path, b)

        # ── Profile auto-created on first list; sorts to the top ────────────
        # First list on a fresh project: no blackboard dir yet — endpoint calls _ensure_profile_entry!
        # which creates the reserved entry.
        st, body = api_blackboard_list(HTTP.Request("GET", "/api/blackboard?projectUid=$uid"))
        @test st == 200
        entries = JSON3.read(body).entries
        @test length(entries) == 1
        @test String(entries[1].entryId) == "profile"
        @test String(entries[1].title)   == "Project profile"
        @test String(entries[1].status)  == "open"       # default status
        @test entries[1].current == 0
        @test isfile(joinpath(tmp, uid, "blackboard", "profile", "entry.md"))
        @test isfile(joinpath(tmp, uid, "blackboard", "profile", "meta.json"))
        reg = JSON3.read(read(joinpath(tmp, uid, "settings", "blackboard.json"), String),
                         Dict{String,Any})
        @test haskey(reg, "profile") && String(reg["profile"]["status"]) == "open"

        # Second list is idempotent: no duplicate created, no snapshot fired.
        st2, body2 = api_blackboard_list(HTTP.Request("GET", "/api/blackboard?projectUid=$uid"))
        @test st2 == 200
        @test length(JSON3.read(body2).entries) == 1

        # ── Read the profile via entry_get: reserved id passes the validator ─
        st_e, body_e = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=profile"))
        @test st_e == 200
        e = JSON3.read(body_e).entry
        @test String(e.entryId) == "profile"
        @test String(e.status)  == "open"
        # Decision 9 — seeded body carries the five suggested headings. The MCP briefing's
        # newProject check keys on Subject + Goal being non-placeholder; kept in step with the
        # Python side (see mcp/tests/test_server.py::ProfileAuthoredGateTest).
        content = String(e.content)
        @test occursin("## Subject", content)
        @test occursin("## Goal", content)
        @test occursin("## Modality", content)
        @test occursin("_(", content)                    # placeholder markers present

        # ── Create a normal entry: status defaults to "open" ─────────────────
        st_c, body_c = w(api_blackboard_create, Dict("projectUid"=>uid,
            "title"=>"A thread we're working on", "content"=>"# notes"))
        @test st_c == 200
        eid = String(JSON3.read(body_c).entryId)
        @test occursin(r"^bb-[0-9]{8}T[0-9]{6}-[0-9a-f]{6}$", eid)
        st_r, body_r = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test String(JSON3.read(body_r).entry.status) == "open"

        # Explicit status on create.
        st_c2, body_c2 = w(api_blackboard_create, Dict("projectUid"=>uid,
            "title"=>"An idea we're parking", "content"=>"# tbd", "status"=>"parked"))
        @test st_c2 == 200
        eid_parked = String(JSON3.read(body_c2).entryId)
        st_rp, body_rp = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid_parked"))
        @test String(JSON3.read(body_rp).entry.status) == "parked"

        # Invalid status on create ⇒ 400.
        @test w(api_blackboard_create, Dict("projectUid"=>uid, "title"=>"t",
            "content"=>"c", "status"=>"nope"))[1] == 400

        # List returns status per entry; profile still sorts first (DESC name: "profile" > "bb-…").
        st_l, body_l = api_blackboard_list(HTTP.Request("GET", "/api/blackboard?projectUid=$uid"))
        rows = JSON3.read(body_l).entries
        @test length(rows) == 3
        @test String(rows[1].entryId) == "profile"
        @test Set(String(r.status) for r in rows) == Set(["open", "open", "parked"])

        # ── POST /api/blackboard/status ──────────────────────────────────────
        # Happy path: open → resolved.
        st_s, body_s = w(api_blackboard_status, Dict("projectUid"=>uid, "entryId"=>eid,
            "status"=>"resolved"))
        @test st_s == 200
        @test String(JSON3.read(body_s).status) == "resolved"
        st_rs, body_rs = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test String(JSON3.read(body_rs).entry.status) == "resolved"
        # Registry mirrors the change.
        reg2 = JSON3.read(read(joinpath(tmp, uid, "settings", "blackboard.json"), String),
                          Dict{String,Any})
        @test String(reg2[eid]["status"]) == "resolved"

        # No-op: setting the same status again returns unchanged:true, no snapshot side-effect.
        versions_before = JSON3.read(body_rs).entry.versions
        st_no, body_no = w(api_blackboard_status, Dict("projectUid"=>uid, "entryId"=>eid,
            "status"=>"resolved"))
        @test st_no == 200
        @test JSON3.read(body_no).unchanged == true
        st_rs2, body_rs2 = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test collect(JSON3.read(body_rs2).entry.versions) == collect(versions_before)

        # A status change does NOT snapshot. Revise the entry once to give it v1, then flip status,
        # then confirm versions == [1] (not [1, 2]).
        st_rev, _ = w(api_blackboard_revise, Dict("projectUid"=>uid, "entryId"=>eid,
            "content"=>"# notes v2"))
        @test st_rev == 200
        st_ss, _ = w(api_blackboard_status, Dict("projectUid"=>uid, "entryId"=>eid,
            "status"=>"open"))
        @test st_ss == 200
        st_after, body_after = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test collect(JSON3.read(body_after).entry.versions) == [1]
        @test String(JSON3.read(body_after).entry.status) == "open"

        # Status persists across a subsequent revise (content diff, status untouched).
        st_ss2, _ = w(api_blackboard_status, Dict("projectUid"=>uid, "entryId"=>eid,
            "status"=>"parked"))
        @test st_ss2 == 200
        st_rev2, _ = w(api_blackboard_revise, Dict("projectUid"=>uid, "entryId"=>eid,
            "content"=>"# notes v3"))
        @test st_rev2 == 200
        st_a2, body_a2 = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test String(JSON3.read(body_a2).entry.status) == "parked"

        # …and across a restore.
        st_re, _ = w(api_blackboard_restore, Dict("projectUid"=>uid, "entryId"=>eid, "version"=>"1"))
        @test st_re == 200
        st_a3, body_a3 = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test String(JSON3.read(body_a3).entry.status) == "parked"   # restore doesn't touch status

        # ── Status endpoint guards ───────────────────────────────────────────
        @test w(api_blackboard_status, Dict("projectUid"=>uid))[1] == 400          # entryId missing
        @test w(api_blackboard_status, Dict("projectUid"=>uid, "entryId"=>eid))[1] == 400  # status missing
        @test w(api_blackboard_status, Dict("projectUid"=>uid, "entryId"=>eid,
            "status"=>"nope"))[1] == 400                                            # bad status
        @test w(api_blackboard_status, Dict("projectUid"=>uid, "entryId"=>"../../etc/passwd",
            "status"=>"open"))[1] == 400                                            # traversal
        @test w(api_blackboard_status, Dict("projectUid"=>uid,
            "entryId"=>"bb-20260101T000000-ffffff", "status"=>"open"))[1] == 404   # missing entry
        # Profile entry accepts status updates too — the reserved id passes the validator.
        st_pp, _ = w(api_blackboard_status, Dict("projectUid"=>uid, "entryId"=>"profile",
            "status"=>"parked"))
        @test st_pp == 200

        # ── Backfill: an entry whose meta.json predates the status field reads as "open" ────
        uid2 = "TESTBBP1LEG"; mkpath(joinpath(tmp, uid2, "blackboard"))
        legacy_id = "bb-20250101T000000-cafefe"
        legacy_dir = joinpath(tmp, uid2, "blackboard", legacy_id); mkpath(legacy_dir)
        write(joinpath(legacy_dir, "entry.md"), "legacy body")
        write(joinpath(legacy_dir, "meta.json"), JSON3.write(Dict{String,Any}(
            "entryId" => legacy_id, "title" => "legacy", "createdAt" => "2025-01-01",
            "updatedAt" => "2025-01-01", "current" => 0, "attachments" => Any[],
            "snapshots" => Any[])))   # NO status key on disk
        st_leg, body_leg = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid2&entryId=$legacy_id"))
        @test st_leg == 200
        @test String(JSON3.read(body_leg).entry.status) == "open"    # backfilled
        st_ll, body_ll = api_blackboard_list(HTTP.Request("GET", "/api/blackboard?projectUid=$uid2"))
        rows_ll = JSON3.read(body_ll).entries
        # Two entries: the auto-created profile + the legacy — both surface a status.
        @test length(rows_ll) == 2
        @test all(haskey(r, :status) for r in rows_ll)
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

# ── Blackboard search (PROJECT_MEMORY_PLAN P2) ─────────────────────────────────────────────────
# Decision 4: case-insensitive substring over title + body, title matches beat body matches, snippet
# ±40 chars, limit ≤ 50 (default 10), optional status filter. Not semantic — a v1 that's cheap at
# the sizes this store reaches.
@testset "API: blackboard substring search (MEMORY P2)" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        uid = "TESTBBP2"; mkpath(joinpath(tmp, uid))
        w(path, b) = _post(path, b)

        # Seed entries: profile (auto), plus three normal entries with mixed title/body content and
        # status. Ordering matters — bb-ids are timestamp-sortable; add them in known order so the
        # DESC sort is predictable.
        api_blackboard_list(HTTP.Request("GET", "/api/blackboard?projectUid=$uid"))   # auto-creates profile
        # Set profile status + title so it can show up in a targeted query.
        w(api_blackboard_revise, Dict("projectUid"=>uid, "entryId"=>"profile",
            "content"=>"Subject: MERTK zolIMa; goal: track live CD169+ macrophages"))

        _, b1 = w(api_blackboard_create, Dict("projectUid"=>uid,
            "title"=>"Segmentation for CD169 macrophages",
            "content"=>"Diameter 30 worked; smaller cells missed."))
        eid1 = String(JSON3.read(b1).entryId)
        sleep(0.01)
        _, b2 = w(api_blackboard_create, Dict("projectUid"=>uid,
            "title"=>"Notes on drift correction",
            "content"=>"CD169 channel is bright enough to anchor multiLag on this cohort."))
        eid2 = String(JSON3.read(b2).entryId)
        sleep(0.01)
        _, b3 = w(api_blackboard_create, Dict("projectUid"=>uid,
            "title"=>"Old parked idea",
            "content"=>"Try SUPPORT denoise on the noisy set. TBD.",
            "status"=>"parked"))
        eid3 = String(JSON3.read(b3).entryId)

        # ── Guards ──────────────────────────────────────────────────────────
        @test w(api_blackboard_search, Dict("projectUid"=>uid))[1] == 400          # query missing
        @test w(api_blackboard_search, Dict("query"=>"x"))[1] == 400                # projectUid missing
        @test w(api_blackboard_search, Dict("projectUid"=>"NOPE", "query"=>"x"))[1] == 404
        @test w(api_blackboard_search, Dict("projectUid"=>uid, "query"=>"x",
            "status"=>"bogus"))[1] == 400

        # ── Title vs body ordering ──────────────────────────────────────────
        # "CD169" appears in eid1's TITLE and in eid2's BODY. Title match must come first.
        st, body = w(api_blackboard_search, Dict("projectUid"=>uid, "query"=>"CD169"))
        @test st == 200
        results = JSON3.read(body).results
        @test length(results) >= 2
        @test String(results[1].entryId) == eid1
        @test String(results[1].matchType) == "title"
        # Second should be a body match — eid2's body has CD169; profile's body has it too.
        matches_after = String(results[2].matchType)
        @test matches_after == "body"
        # Every returned row carries a snippet + status.
        @test all(haskey(r, :snippet) && !isempty(String(r.snippet)) for r in results)
        @test all(haskey(r, :status) for r in results)

        # ── Case-insensitive ────────────────────────────────────────────────
        _, body_ci = w(api_blackboard_search, Dict("projectUid"=>uid, "query"=>"cd169"))
        @test !isempty(JSON3.read(body_ci).results)

        # ── Status filter ───────────────────────────────────────────────────
        # "SUPPORT" appears only in eid3 (parked). Filter parked → returns it; filter open → empty.
        _, b_parked = w(api_blackboard_search, Dict("projectUid"=>uid, "query"=>"SUPPORT",
            "status"=>"parked"))
        @test length(JSON3.read(b_parked).results) == 1
        _, b_open = w(api_blackboard_search, Dict("projectUid"=>uid, "query"=>"SUPPORT",
            "status"=>"open"))
        @test isempty(JSON3.read(b_open).results)

        # ── Snippet shape ───────────────────────────────────────────────────
        # A body hit's snippet contains the needle, with "…" markers when the body is truncated.
        _, b_snip = w(api_blackboard_search, Dict("projectUid"=>uid, "query"=>"multiLag"))
        snip = String(JSON3.read(b_snip).results[1].snippet)
        @test occursin("multiLag", snip)

        # ── Empty result ────────────────────────────────────────────────────
        _, b_none = w(api_blackboard_search, Dict("projectUid"=>uid, "query"=>"asdfqwerty"))
        @test isempty(JSON3.read(b_none).results)

        # ── Limit clamp ─────────────────────────────────────────────────────
        # A limit above the cap is silently clamped rather than 400 (a query with limit=1000 is a
        # caller sanity slip, not a security issue).
        _, b_lim = w(api_blackboard_search, Dict("projectUid"=>uid, "query"=>"CD169", "limit"=>1000))
        @test length(JSON3.read(b_lim).results) <= 50
        _, b_lim2 = w(api_blackboard_search, Dict("projectUid"=>uid, "query"=>"CD169", "limit"=>1))
        @test length(JSON3.read(b_lim2).results) == 1
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end
