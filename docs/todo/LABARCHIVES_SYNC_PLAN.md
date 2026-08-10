# LabArchives context sync — parked plan

Pull the experimental context that lives in the lab's **LabArchives** ELN (cohort, protocol, the
question being asked) into a cecelia project, so a fresh Claude session is oriented by the *experiment*
and not just by the file tree.

Status: **all three phases BUILT** (2026-08-10), verified end-to-end against a live backend on project
`zolIMa`: a session wrote the sidecar, the WT gap derived correctly (declared 4, present 0), the
briefing carried it, and a dated `[LabArchives]` block landed in the log. Not yet merged. What remains
is promotion of the durable parts into `docs/ai-assist/LAB-LOG.md` (already started) and deleting this
file once nothing points at it. Related: `docs/ai-assist/LAB-LOG.md`,
`docs/ai-assist/OBSERVER.md`, `app/src/ai/briefing.jl`.

---

## The inversion that makes this cheap

**Cecelia's backend cannot talk to LabArchives, and should not learn how.** LabArchives access is an
authenticated MCP connector registered in the *user's Claude session*
(`la-mcp.gimr.garvan.org.au`, Okta-linked, per-user permissions). Teaching the Julia app to speak the
LabArchives API would mean a second OAuth integration, credential storage, and a token lifecycle — to
re-fetch data the session already has in hand.

So the direction inverts versus a normal integration:

```
LabArchives ──(read-only MCP, Claude's session)──> Claude
                                                     │  set_labarchives_context (new, additive write)
                                                     ▼
                                        settings/labarchives.json   ← the sidecar (a CACHE)
                                                     │
                              session_briefing ──────┴────> the NEXT Claude session, and the GUI card
```

**Claude pulls; cecelia stores and serves.** Consequences, all of them good:

- The sidecar is a **cache of an external system of record**, not primary data. Regenerating it loses
  nothing — LabArchives is itself versioned. This is what lets it be rewritable while the lab log
  stays append-only (see *Append-only*, below).
- A session with **no** LabArchives access still gets the context, because it reads the sidecar via
  the briefing. Only *refreshing* it needs the connector.
- No credentials, no scheduler, no network dependency in the backend. The sync happens when a human
  asks for it in a session.

---

## Locked decisions

1. **Sidecar at `{proj.root}/settings/labarchives.json`** — matches every other project sidecar
   (`analysisBoards.json`, `lab-log-context.json`, `observer-session.json`), written through
   `write_json_atomic` + `with_transaction`.
   **JSON, not the `.md` first sketched**: the card needs a sync timestamp, a gap count, and
   per-section source links as *fields* — a markdown blob would have to be re-parsed to render any of
   that. It still renders as markdown in the panel; `sections[]` is heading + bullet lines.
2. **The lab log stays append-only.** The sidecar carries current state; the log carries dated
   `[LabArchives]` **deltas**. No new rewrite path — `upsert_daily_context_block!` remains the one
   sanctioned in-place rewrite (the `[Cecelia]` daily digest), and this feature does not extend it.
3. **Deltas, not a cadence.** LabArchives content is *state* (cohort, protocol, question), not events.
   A daily/weekly aggregate would be empty most days and repeat itself the rest. First sync writes a
   full context block; later syncs append only what changed. Nothing auto-fires — a sync is a session
   action.
4. **`append_lab_log` gains a `source` param, a CLOSED enum** — `claude` (default) | `labarchives`.
   The tag is still injected server-side and remains unforgeable; the enum keeps `[LabArchives]` from
   becoming a free-text author field.
5. **Explicit project↔notebook link, never inferred.** The sidecar stores `notebookId` +
   `pageIds[]`. Inference from the project name is not acceptable: searching `MERTK` against one
   notebook returned **175 hits spanning 2018–2026**, most of them a different assay.
6. **Gaps are computed by cecelia, not asserted by Claude.** `gaps[]` compares the sidecar's declared
   `cohort[]` against the live attribute levels (`get_image_attributes`) and reports what LabArchives
   records that the project has no images for. Claude supplies the *cohort*; cecelia derives the
   *discrepancy*, so the flag can't drift from the data.
7. **A gap's reason is human-supplied and lives in the lab log.** The sync can detect an absence; it
   can never know why. See the worked case below.

---

## Why the gap flag is the load-bearing feature

Worked case, project `zolIMa` ("MERTK", 2026-08-10):

- LabArchives records the cohort as **4 WT / 4 MerTK** (Ailsa's notebook, the July-2026 round
  "Imaging repeat — Katushka"). NOTE the trap: a note on a DIFFERENT page said 6 WT / 5 MertkKO and
  belongs to an earlier round — which is exactly why the link is an explicit `pageIds`, not a search.
- The project holds **2 mice, all `Treatment=MERTK`**. The WT images were deleted on purpose — too
  noisy to use.
- `get_image_attributes` reports `Treatment: ["MERTK"]`. Attribute levels are derived from the images
  **present**, so deleting the WT arm deleted the *evidence that it ever existed*. Nothing inside
  cecelia can now tell you a WT arm was planned.

LabArchives is the only remaining record. Without the flag, the next person to open this project — or
the next Claude session — sees a single-arm experiment and has no reason to ask. With it, they see
"LA cohort: 4 WT · project has 0 WT" and one appended human line explaining why.

That is the whole argument for this feature. The orientation text is convenience; **the gap flag is
the part that prevents a wrong conclusion.**

---

## Sidecar shape

```jsonc
{
  "version": 1,
  "source": {
    "notebookId": "MjkyMjE3…",           // opaque LabArchives id
    "notebookName": "Ailsa",
    "pageIds": ["…"],                     // the linked page(s)/folder
    "url": "https://auapi.labarchives.com/share/…"
  },
  "syncedAt": "2026-08-10T04:31:00Z",
  "syncedBy": "claude",
  "sections": [                           // renders as markdown; order preserved
    { "heading": "Setup",    "lines": ["…"], "sourceDate": "2026-02-24", "url": "…" },
    { "heading": "Question", "lines": ["…"], "sourceDate": "2026-02-10", "url": "…" }
  ],
  "cohort": [                             // the DECLARED design — what LA says should exist
    { "attr": "Treatment", "value": "WT",    "n": 4 },
    { "attr": "Treatment", "value": "MERTK", "n": 4 }
  ]
}
```

`gaps[]` is **not stored** — it is derived on read, so it can never go stale against the images.

---

## Build sequence

### Phase 1 — backend (sidecar + briefing + MCP) ✅
- `app/src/ai/labarchives.jl` — `la_doc_path`, `read_la_doc`, `write_la_doc!`, `la_gaps(proj, doc)`,
  `la_briefing(proj)` (the compact form). One reader/writer, per the boards-doc lesson (two parsers
  disagreed and silently reported "no boards").
- `session_briefing` gains a `labarchives` field: `{notebookName, syncedAt, sections: [heading], gaps}`.
  Headings only — the briefing stays an orientation, not a report. Full text via its own tool.
- Routes: `GET`/`PUT /api/observer/labarchives` (`api/src/routes.jl`), reusing
  `_observer_summary_route` for the read.
- MCP: `get_labarchives_context` (read) + `set_labarchives_context` (the **third** additive write,
  alongside `append_lab_log` + `create_notebook`); `append_lab_log` gains `source`.
- Tests (`app/test/suite.jl`): round-trip, gap derivation incl. the deleted-arm case, the closed
  author enum, and that the log stays append-only across a sync.

### Phase 2 — GUI card ✅
- Pinned collapsible card at the **top of `LabLogPanel.vue`**, above the dated blocks: sync timestamp,
  gap count, link out. Distinct chrome (it is a mirror, not an entry). Collapsed by default — quiet
  when nothing changed, loud when a gap appears.
- Uses `CollapsibleSection` + `.cc-card`; no new primitive (`docs/UI.md`).

### Phase 3 — prompts + docs ✅
- **Four surfaces, all of them** (a tool added to fewer is the recurring bug): `mcp/cecelia_mcp/server.py`,
  `app/src/ai/observer_prompt.jl`, `frontend/src/lib/chatHandoff.ts`, and the opening menu in
  `frontend/src/lib/claudeOverview.ts`.
- `docs/ai-assist/LAB-LOG.md` gains the sidecar-vs-log split; `INVENTORY.md` gains the reader/writer line.

---

## Learned while building

- **The MCP client's `ALLOWED_ROUTES` is a third place a new tool must be registered.** Both tools were
  written, wired and tested against mocks, and every call still failed — the allow-list refused them
  before they reached a healthy server, and the assistant reported it as "a route that isn't enabled on
  this server", which sent us to the backend. A detector now asserts every `_request` literal in
  `client.py` is on the list.
- **LabArchives is chat-only, structurally.** The in-app "Ask Claude" agent is spawned with
  `--mcp-config` listing ONLY `cecelia-observer`, so it has no ELN connector and could never read one.
  Naming the tools in its prompt advertised a capability that build cannot have.
- **Don't open on the absence.** Finding the notebook took ~6 searches across two colleagues' books
  (the project name matched 175 pages spanning 2018–2026), so "no context linked" is a DIRECTION on the
  menu, not a line in the greeting.
- **A connector the user hides must vanish from the prompt and the capability dialog**, or Claude offers
  a tool they've switched off.

## Deliberately out of scope

- **Writing to LabArchives.** The connector is read-only; there is no push path and this plan assumes
  none ever appears.
- **Attachment/image sync.** Only text context. Raw intravital data stays where it is (for `zolIMa`:
  the shared drive, "Abigail Rhino").
- **Automatic/scheduled sync.** No backend network calls — see the inversion above.
- **Semantic search.** Opt-in per account and not enabled as of 2026-08-10; per-notebook full-text
  search is the only pull path, and it is scoped to one notebook per call.
