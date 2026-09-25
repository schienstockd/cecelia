# Blackboard testsets — BIDIR Part 4 storage discipline (docs/todo/BIDIR_CONTEXT_PLAN.md) plus the
# PROJECT_MEMORY_PLAN P1/P2/P4/P5.1 additions that ride on the same store:
#   • CRUD + versioning + attachments (BIDIR Part 4)
#   • status field + reserved `profile` entry           (PROJECT_MEMORY_PLAN P1 — Decisions 2, 3)
#   • substring search over titles + bodies             (PROJECT_MEMORY_PLAN P2 — Decision 4)
#   • outcome tag (good/bad + required note)            (PROJECT_MEMORY_PLAN P4 — Decision 11)
#   • outcome tiebreak on search ordering               (PROJECT_MEMORY_PLAN P4 — Decision 12)
#   • entry fingerprint (set-once, preserved)           (PROJECT_MEMORY_PLAN P5.1)
#
# All five sit here so a future blackboard change touches ONE suite file, not five. Extracted so
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

# ── Outcome tagging (PROJECT_MEMORY_PLAN P4 — Decision 11) ─────────────────────────────────────
# Additive good/bad + required note on every entry, exposed via POST /api/blackboard/outcome. Same
# discipline as /status: no snapshot fired, preserves content + version history. Absent-when-untagged
# on the wire (Decision 11 D4 — untagged means "no signal", not "neutral").
@testset "API: blackboard outcome tagging (MEMORY P4)" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        uid = "TESTBBP4"; mkpath(joinpath(tmp, uid))
        w(path, b) = _post(path, b)

        # Seed one entry to tag.
        _, body_c = w(api_blackboard_create, Dict("projectUid"=>uid,
            "title"=>"Segmentation attempt on the bright cohort",
            "content"=>"tried diameter 30 with default flow"))
        eid = String(JSON3.read(body_c).entryId)

        # ── Guards ──────────────────────────────────────────────────────────
        @test w(api_blackboard_outcome, Dict("projectUid"=>uid))[1] == 400            # entryId missing
        @test w(api_blackboard_outcome, Dict("projectUid"=>uid,
            "entryId"=>eid))[1] == 400                                                # verdict missing
        @test w(api_blackboard_outcome, Dict("projectUid"=>uid, "entryId"=>eid,
            "verdict"=>"maybe", "note"=>"…"))[1] == 400                               # bad verdict
        @test w(api_blackboard_outcome, Dict("projectUid"=>uid, "entryId"=>eid,
            "verdict"=>"bad", "note"=>""))[1] == 400                                  # empty note
        @test w(api_blackboard_outcome, Dict("projectUid"=>uid, "entryId"=>eid,
            "verdict"=>"bad", "note"=>"   "))[1] == 400                               # whitespace-only note
        @test w(api_blackboard_outcome, Dict("projectUid"=>uid, "entryId"=>eid,
            "verdict"=>"bad", "note"=>repeat("x", 2 * 1024 + 1)))[1] == 400           # note too big
        @test w(api_blackboard_outcome, Dict("projectUid"=>uid, "entryId"=>"../../etc/passwd",
            "verdict"=>"bad", "note"=>"x"))[1] == 400                                 # traversal
        @test w(api_blackboard_outcome, Dict("projectUid"=>uid,
            "entryId"=>"bb-20260101T000000-ffffff", "verdict"=>"bad",
            "note"=>"x"))[1] == 404                                                   # missing entry

        # ── Happy path: tag bad ─────────────────────────────────────────────
        st_o, body_o = w(api_blackboard_outcome, Dict("projectUid"=>uid, "entryId"=>eid,
            "verdict"=>"bad",
            "note"=>"wrong scan mode — cellpose diameter picked for galvo on a resonant image"))
        @test st_o == 200
        rsp = JSON3.read(body_o)
        @test String(rsp.outcome.verdict) == "bad"
        @test occursin("resonant", String(rsp.outcome.note))
        @test !isempty(String(rsp.outcome.taggedAt))

        # entry_get surfaces the outcome; list surfaces it too (only on tagged rows).
        st_e, body_e = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test st_e == 200
        e = JSON3.read(body_e).entry
        @test String(e.outcome.verdict) == "bad"

        st_l, body_l = api_blackboard_list(HTTP.Request("GET", "/api/blackboard?projectUid=$uid"))
        rows_all = JSON3.read(body_l).entries
        tagged = only([r for r in rows_all if String(r.entryId) == eid])
        @test String(tagged.outcome.verdict) == "bad"
        # Untagged rows (profile) do NOT carry an outcome key — absent = no signal (D4).
        prof = only([r for r in rows_all if String(r.entryId) == "profile"])
        @test !haskey(prof, :outcome)

        # Registry mirrors the verdict (small, for cheap filter without loading meta).
        reg = JSON3.read(read(joinpath(tmp, uid, "settings", "blackboard.json"), String),
                         Dict{String,Any})
        @test String(reg[eid]["outcome"]) == "bad"

        # ── Idempotence: same verdict + same note ⇒ unchanged:true, no rewrite ───
        st_no, body_no = w(api_blackboard_outcome, Dict("projectUid"=>uid, "entryId"=>eid,
            "verdict"=>"bad",
            "note"=>"wrong scan mode — cellpose diameter picked for galvo on a resonant image"))
        @test st_no == 200
        @test JSON3.read(body_no).unchanged == true

        # ── Change of mind: re-tag as good with a new note ──────────────────
        st_g, body_g = w(api_blackboard_outcome, Dict("projectUid"=>uid, "entryId"=>eid,
            "verdict"=>"good",
            "note"=>"actually held up after re-checking with the imaging setup"))
        @test st_g == 200
        @test String(JSON3.read(body_g).outcome.verdict) == "good"

        # ── No snapshot fired: versions still empty after outcome flips ─────
        st_v, body_v = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test isempty(JSON3.read(body_v).entry.versions)

        # ── Outcome persists across a content revise ────────────────────────
        st_r, _ = w(api_blackboard_revise, Dict("projectUid"=>uid, "entryId"=>eid,
            "content"=>"updated: still valid on the bright cohort"))
        @test st_r == 200
        st_a, body_a = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test String(JSON3.read(body_a).entry.outcome.verdict) == "good"   # revise doesn't drop outcome

        # ── Outcome persists across a status flip too ───────────────────────
        st_s, _ = w(api_blackboard_status, Dict("projectUid"=>uid, "entryId"=>eid,
            "status"=>"resolved"))
        @test st_s == 200
        st_a2, body_a2 = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test String(JSON3.read(body_a2).entry.outcome.verdict) == "good"

        # ── Outcome surfaces in search results too (tagged only) ────────────
        _, body_srch = w(api_blackboard_search, Dict("projectUid"=>uid, "query"=>"Segmentation"))
        results = JSON3.read(body_srch).results
        hit = only([r for r in results if String(r.entryId) == eid])
        @test String(hit.outcome.verdict) == "good"

        # ── Backfill: an entry with no outcome key on disk reads as untagged ─
        uid2 = "TESTBBP4LEG"; mkpath(joinpath(tmp, uid2, "blackboard"))
        legacy_id = "bb-20250101T000000-cafefe"
        legacy_dir = joinpath(tmp, uid2, "blackboard", legacy_id); mkpath(legacy_dir)
        write(joinpath(legacy_dir, "entry.md"), "legacy body")
        write(joinpath(legacy_dir, "meta.json"), JSON3.write(Dict{String,Any}(
            "entryId" => legacy_id, "title" => "legacy", "createdAt" => "2025-01-01",
            "updatedAt" => "2025-01-01", "current" => 0, "attachments" => Any[],
            "snapshots" => Any[])))   # NO outcome key
        st_leg, body_leg = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid2&entryId=$legacy_id"))
        @test st_leg == 200
        e_leg = JSON3.read(body_leg).entry
        @test !haskey(e_leg, :outcome)
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

# ── Outcome-note friction telemetry (RUBBER_DUCK_FIT_PLAN P4) ─────────────────────────────────
# Pins the counter's shape and the three call sites: `attempted` on any well-formed request that
# passes schema + project-exists; `dropped_no_note` on the empty-note 400; `succeeded_with_note`
# on the 200 path (both fresh writes and the idempotent no-op). Written to
# `<proj>/settings/blackboard_outcome_telemetry.json`. Never sent, never surfaced — read by hand.
@testset "API: blackboard outcome telemetry counter (RUBBER_DUCK_FIT_PLAN P4)" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    try
        uid = "TESTBBP4TEL"; mkpath(joinpath(tmp, uid))
        w(path, b) = _post(path, b)
        _, body_c = w(api_blackboard_create, Dict("projectUid"=>uid,
            "title"=>"tel", "content"=>"c"))
        eid = String(JSON3.read(body_c).entryId)
        tel_path = joinpath(tmp, uid, "settings", "blackboard_outcome_telemetry.json")

        # No file yet — a fresh project reads as all zeros.
        @test !isfile(tel_path)

        # A structural 400 (missing verdict) fires BEFORE attempted — a broken client isn't
        # friction. File still absent.
        @test w(api_blackboard_outcome, Dict("projectUid"=>uid, "entryId"=>eid))[1] == 400
        @test !isfile(tel_path)

        # Empty note ⇒ attempted +1, dropped_no_note +1.
        @test w(api_blackboard_outcome, Dict("projectUid"=>uid, "entryId"=>eid,
            "verdict"=>"bad", "note"=>""))[1] == 400
        counts = JSON3.read(read(tel_path, String), Dict{String,Any})
        @test Int(counts["attempted"]) == 1
        @test Int(counts["dropped_no_note"]) == 1
        @test Int(counts["succeeded_with_note"]) == 0

        # Whitespace-only note stripped to empty ⇒ same increment pair.
        @test w(api_blackboard_outcome, Dict("projectUid"=>uid, "entryId"=>eid,
            "verdict"=>"good", "note"=>"   \t\n"))[1] == 400
        counts = JSON3.read(read(tel_path, String), Dict{String,Any})
        @test Int(counts["attempted"]) == 2 && Int(counts["dropped_no_note"]) == 2

        # A real tag ⇒ attempted +1, succeeded_with_note +1.
        @test w(api_blackboard_outcome, Dict("projectUid"=>uid, "entryId"=>eid,
            "verdict"=>"bad", "note"=>"real reason"))[1] == 200
        counts = JSON3.read(read(tel_path, String), Dict{String,Any})
        @test Int(counts["attempted"]) == 3
        @test Int(counts["dropped_no_note"]) == 2
        @test Int(counts["succeeded_with_note"]) == 1

        # Idempotent re-tag (same verdict + same note) STILL counts as a success — the user
        # committed to the tag; only the disk write was skipped.
        @test w(api_blackboard_outcome, Dict("projectUid"=>uid, "entryId"=>eid,
            "verdict"=>"bad", "note"=>"real reason"))[1] == 200
        counts = JSON3.read(read(tel_path, String), Dict{String,Any})
        @test Int(counts["succeeded_with_note"]) == 2

        # Bad projectUid ⇒ 404 before the counter, no file written under NOPE.
        @test w(api_blackboard_outcome, Dict("projectUid"=>"NOPE", "entryId"=>eid,
            "verdict"=>"bad", "note"=>"x"))[1] == 404
        @test !isfile(joinpath(tmp, "NOPE", "settings", "blackboard_outcome_telemetry.json"))
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

# ── Outcome tiebreak on search ordering (PROJECT_MEMORY_PLAN P4 — Decision 12) ─────────────────
# Within each search bucket (title/body), `bad` beats `good` beats untagged on equal match
# strength. Applied AFTER full-scan collection (not by early-stopping), so a `bad`-tagged hit
# later in the ID-DESC scan still surfaces above earlier untagged hits within the same bucket.
@testset "API: blackboard search outcome tiebreak (MEMORY P4 Decision 12)" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        uid = "TESTBBP4RB"; mkpath(joinpath(tmp, uid))
        w(path, b) = _post(path, b)

        # Three entries all matching the SAME title needle. Newest first by id (bb-…) is:
        #   eid_new (untagged) > eid_mid (bad) > eid_old (good).
        # Pre-tiebreak order (by id DESC): new, mid, old → all title-bucket.
        # Post-tiebreak: bad (mid) > good (old) > untagged (new).
        _, b_new = w(api_blackboard_create, Dict("projectUid"=>uid,
            "title"=>"Segmentation strategy for the bright cohort",
            "content"=>"newest, untagged"))
        eid_new = String(JSON3.read(b_new).entryId)
        sleep(0.01)
        _, b_mid = w(api_blackboard_create, Dict("projectUid"=>uid,
            "title"=>"Segmentation strategy — first attempt",
            "content"=>"middle, will be bad"))
        eid_mid = String(JSON3.read(b_mid).entryId)
        sleep(0.01)
        _, b_old = w(api_blackboard_create, Dict("projectUid"=>uid,
            "title"=>"Segmentation strategy — reference",
            "content"=>"oldest, will be good"))
        eid_old = String(JSON3.read(b_old).entryId)
        # Yes the ID ordering above is oldest→newest by TIMESTAMP. In DESC sort by id, eid_old is
        # actually the LAST (its timestamp is highest since it was created last). Correct: eid_old
        # > eid_mid > eid_new in DESC. So pre-tiebreak: old, mid, new. Post-tiebreak (Decision 12):
        # bad (mid) > good (old) > untagged (new).

        # Tag the two.
        w(api_blackboard_outcome, Dict("projectUid"=>uid, "entryId"=>eid_mid,
            "verdict"=>"bad",  "note"=>"tried galvo-tuned diameter on resonant data — wrong"))
        w(api_blackboard_outcome, Dict("projectUid"=>uid, "entryId"=>eid_old,
            "verdict"=>"good", "note"=>"held up on the bright cohort"))

        _, body = w(api_blackboard_search, Dict("projectUid"=>uid, "query"=>"Segmentation strategy"))
        results = JSON3.read(body).results
        ids_in_order = [String(r.entryId) for r in results]
        # bad (eid_mid) first, then good (eid_old), then untagged (eid_new) — all in title bucket.
        @test ids_in_order == [eid_mid, eid_old, eid_new]

        # And the tiebreak does NOT cross bucket boundaries: an untagged TITLE hit still beats a
        # bad-tagged BODY hit. "cohort" appears in eid_new's title (untagged) and in eid_mid's body
        # (bad); title bucket wins the primary key.
        _, b_bx = w(api_blackboard_search, Dict("projectUid"=>uid, "query"=>"cohort"))
        rows = JSON3.read(b_bx).results
        # eid_new is a title hit for "cohort"; both eid_mid and eid_old would only match body if at
        # all. First result must be a title match regardless of outcome.
        @test String(rows[1].matchType) == "title"
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

# ── Entry fingerprint (PROJECT_MEMORY_PLAN Phase 5.1) ─────────────────────────────────────────
# The fingerprint is a small structured key set once at create, preserved across every mutation
# (status flip, outcome tag, revise, restore, prune) and exposed on the list + entry read. Absent
# on entries created without one (the same shape a pre-P5.1 legacy entry has).
@testset "API: blackboard entry fingerprint (MEMORY P5.1)" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        uid = "TESTBBP51"; mkpath(joinpath(tmp, uid))
        w(path, b) = _post(path, b)

        fp = Dict{String,Any}(
            "v"              => 1,
            "channel_count"  => 4,
            "stain_classes"  => ["macrophage_or_marker", "membrane", "nucleus"],
            "pipeline_stage" => "denoised",
        )

        # ── Guards ──────────────────────────────────────────────────────────
        # Fingerprint present but missing v ⇒ 400.
        @test w(api_blackboard_create, Dict("projectUid"=>uid, "title"=>"t", "content"=>"c",
            "fingerprint"=>Dict("channel_count"=>4)))[1] == 400
        # Non-int v ⇒ 400.
        @test w(api_blackboard_create, Dict("projectUid"=>uid, "title"=>"t", "content"=>"c",
            "fingerprint"=>Dict("v"=>"one")))[1] == 400
        # Over the byte cap ⇒ 400 (fingerprint is a key, not a payload).
        big = Dict{String,Any}("v"=>1, "junk"=>repeat("x", 2 * 1024 + 1))
        @test w(api_blackboard_create, Dict("projectUid"=>uid, "title"=>"t", "content"=>"c",
            "fingerprint"=>big))[1] == 400

        # ── Happy path: create WITH fingerprint ─────────────────────────────
        st_c, body_c = w(api_blackboard_create, Dict("projectUid"=>uid,
            "title"=>"Segmentation attempt on the bright cohort",
            "content"=>"diameter 30 with default flow",
            "fingerprint"=>fp))
        @test st_c == 200
        eid = String(JSON3.read(body_c).entryId)

        # entry_get surfaces the fingerprint.
        st_e, body_e = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test st_e == 200
        e_fp = JSON3.read(body_e).entry.fingerprint
        @test Int(e_fp.v) == 1
        @test Int(e_fp.channel_count) == 4
        @test String(e_fp.pipeline_stage) == "denoised"
        @test Set(String(c) for c in e_fp.stain_classes) ==
              Set(["macrophage_or_marker", "membrane", "nucleus"])

        # list surfaces the fingerprint per row too (retrieval reads the whole project cheaply).
        st_l, body_l = api_blackboard_list(HTTP.Request("GET", "/api/blackboard?projectUid=$uid"))
        rows = JSON3.read(body_l).entries
        row = only([r for r in rows if String(r.entryId) == eid])
        @test Int(row.fingerprint.v) == 1
        # Profile row has no fingerprint (created without one).
        prof = only([r for r in rows if String(r.entryId) == "profile"])
        @test !haskey(prof, :fingerprint)

        # An entry created WITHOUT a fingerprint reads back without one (absent-on-missing).
        _, body_no = w(api_blackboard_create, Dict("projectUid"=>uid,
            "title"=>"unrelated idea", "content"=>"no image context"))
        eid_no = String(JSON3.read(body_no).entryId)
        st_en, body_en = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid_no"))
        @test !haskey(JSON3.read(body_en).entry, :fingerprint)

        # ── Preservation across every mutation ──────────────────────────────
        # revise (content diff)
        st_r, _ = w(api_blackboard_revise, Dict("projectUid"=>uid, "entryId"=>eid,
            "content"=>"revised body — same context"))
        @test st_r == 200
        st_ar, body_ar = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test Int(JSON3.read(body_ar).entry.fingerprint.channel_count) == 4

        # status flip
        st_s, _ = w(api_blackboard_status, Dict("projectUid"=>uid, "entryId"=>eid,
            "status"=>"resolved"))
        @test st_s == 200
        st_as, body_as = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test Int(JSON3.read(body_as).entry.fingerprint.channel_count) == 4

        # outcome tag
        st_o, _ = w(api_blackboard_outcome, Dict("projectUid"=>uid, "entryId"=>eid,
            "verdict"=>"bad", "note"=>"the diameter was picked from the wrong scan mode"))
        @test st_o == 200
        st_ao, body_ao = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        entry_ao = JSON3.read(body_ao).entry
        @test Int(entry_ao.fingerprint.channel_count) == 4
        @test String(entry_ao.outcome.verdict) == "bad"   # still there too

        # restore
        st_rev2, _ = w(api_blackboard_revise, Dict("projectUid"=>uid, "entryId"=>eid,
            "content"=>"a second edit so a v2 exists to restore"))
        @test st_rev2 == 200
        st_rest, _ = w(api_blackboard_restore, Dict("projectUid"=>uid, "entryId"=>eid,
            "version"=>"1"))
        @test st_rest == 200
        st_arest, body_arest = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$eid"))
        @test Int(JSON3.read(body_arest).entry.fingerprint.channel_count) == 4

        # ── Backfill: an entry with no fingerprint key on disk reads as untagged ─
        uid2 = "TESTBBP51LEG"; mkpath(joinpath(tmp, uid2, "blackboard"))
        legacy_id = "bb-20250101T000000-cafefe"
        legacy_dir = joinpath(tmp, uid2, "blackboard", legacy_id); mkpath(legacy_dir)
        write(joinpath(legacy_dir, "entry.md"), "legacy body")
        write(joinpath(legacy_dir, "meta.json"), JSON3.write(Dict{String,Any}(
            "entryId" => legacy_id, "title" => "legacy", "createdAt" => "2025-01-01",
            "updatedAt" => "2025-01-01", "current" => 0, "attachments" => Any[],
            "snapshots" => Any[])))   # NO fingerprint key
        st_leg, body_leg = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid2&entryId=$legacy_id"))
        @test st_leg == 200
        @test !haskey(JSON3.read(body_leg).entry, :fingerprint)
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: blackboard kiwiRefs sidecar (KIWI_CAPTURE_AND_BLACKBOARD_PLAN P2)" begin
    # Additive `kiwiRefs` field on meta.json — a saved Kiwi turn's per-ref sidecar. Round-trips
    # through create → read; preserved across revise + status flip + outcome tag; missing on a
    # normal (non-Kiwi) entry; a runaway sidecar rejected at write; malformed shape rejected.
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        uid = "TESTKR"; mkpath(joinpath(tmp, uid))
        w(path, b) = _post(path, b)
        get_entry(id) = api_blackboard_entry_get(HTTP.Request("GET",
            "/api/blackboard/entry?projectUid=$uid&entryId=$id"))

        # Reject a non-object sidecar; reject one that blows the 200 KiB cap.
        @test w(api_blackboard_create, Dict("projectUid"=>uid, "title"=>"t",
            "content"=>"body", "kiwiRefs"=>"not-an-object"))[1] == 400
        oversized = Dict("k$(i)" => Dict("ref"=>Dict("kind"=>"plot","plotId"=>"p"),
                                          "label"=>"x", "snapshot"=>Dict("kind"=>"plot",
                                                                          "plotSummary"=>repeat("y", 4_000),
                                                                          "label"=>"x"),
                                          "savedAt"=>"2026-09-25T12:00:00Z") for i in 1:100)
        @test w(api_blackboard_create, Dict("projectUid"=>uid, "title"=>"t",
            "content"=>"body", "kiwiRefs"=>oversized))[1] == 400

        # Normal entry (no kiwiRefs) → field absent on read.
        _, body_plain = w(api_blackboard_create, Dict("projectUid"=>uid, "title"=>"plain", "content"=>""))
        eid_plain = String(JSON3.read(body_plain).entryId)
        @test !haskey(JSON3.read(get_entry(eid_plain)[2]).entry, :kiwiRefs)

        # Saved Kiwi turn with two fragile refs (a plot + a population) round-trips through create,
        # revise (content edit preserves sidecar), status flip, outcome tag.
        refs = Dict(
            "k-plot" => Dict("ref"=>Dict("kind"=>"plot","plotId"=>"p1"), "label"=>"plot",
                              "snapshot"=>Dict("kind"=>"plot", "plotSummary"=>"chart: box\nN=7", "label"=>"plot"),
                              "savedAt"=>"2026-09-25T12:00:00.000Z"),
            "k-pop"  => Dict("ref"=>Dict("kind"=>"population","imageUid"=>"IMG1","valueName"=>"default","popPath"=>"/x"),
                              "label"=>"/x · default",
                              "snapshot"=>Dict("kind"=>"population","imageName"=>"img_005","label"=>"/x · default"),
                              "savedAt"=>"2026-09-25T12:00:00.000Z"),
        )
        _, body_k = w(api_blackboard_create, Dict("projectUid"=>uid, "title"=>"Kiwi turn — why the drift",
            "content"=>"# Kiwi turn", "kiwiRefs"=>refs))
        eid_k = String(JSON3.read(body_k).entryId)
        got1 = JSON3.read(get_entry(eid_k)[2]).entry
        @test String(got1.kiwiRefs["k-plot"].snapshot.plotSummary) == "chart: box\nN=7"
        @test String(got1.kiwiRefs["k-pop"].snapshot.imageName) == "img_005"

        # Revise (content edit) preserves sidecar unchanged.
        st_r, _ = w(api_blackboard_revise, Dict("projectUid"=>uid, "entryId"=>eid_k,
            "content"=>"# Kiwi turn\n\nrevised"))
        @test st_r == 200
        got2 = JSON3.read(get_entry(eid_k)[2]).entry
        @test String(got2.kiwiRefs["k-plot"].snapshot.plotSummary) == "chart: box\nN=7"

        # Status flip preserves sidecar too.
        st_s, _ = w(api_blackboard_status, Dict("projectUid"=>uid, "entryId"=>eid_k, "status"=>"resolved"))
        @test st_s == 200
        got3 = JSON3.read(get_entry(eid_k)[2]).entry
        @test haskey(got3.kiwiRefs, :"k-plot")
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end
