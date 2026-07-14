# Phase: MCP Server — Claude Integration Infrastructure

This is Phase 1 of a three-phase Claude integration arc. Build the infrastructure here. The behaviour that runs on top of it is defined in `qc-process-prompt.md` and `lab-log-prompt.md`. All three documents form one system — read them together.

---

## The arc

**Phase 1 — Observer** (this prompt)
Claude sits next to the user. Watches what's happening. Reads task logs, image notes, project state. Asks questions. Notices patterns. Stays silent unless something is worth saying. Builds the lab log. No mutations, no autonomy.

**Phase 2 — Designer** (after API and chain scheduler are stable)
Claude proposes analysis strategies, suggests parameter adjustments, flags before things go wrong rather than after. Write-capable MCP with human confirmation on every action.

**Phase 3 — Analyst** (after Phase 2 is proven)
The overnight analyst. "I have 20 images, two treatment groups, I need to know if there's a behaviour difference by tomorrow." Claude runs the pipeline, flags edge cases, writes the morning summary. User reviews results, not a blank screen.

---

## Data sources Claude reads

The value of the observer session depends entirely on what Claude can see. Claude must have access to:

- **Project state** — images, processing status per stage, label props available
- **Task logs** — every task writes a log file per image. Claude reads these. A task that failed 10 times leaves 10 log files. Claude notices.
- **Image notes** — users can add notes to individual images in Cecelia. These are first-class data for Claude — the user's own words about what they observed.
- **Lab log** — the accumulated cross-session memory (see `lab-log-prompt.md`)
- **QC metrics** — per-stage quality flags computed after each task (see `qc-process-prompt.md`)

## MCP tools to implement

**Read-only (Phase 1 — implement now):**
```
get_project_info          → name, version, image count, current stage
list_images               → UIDs, names, processing status per stage, any user notes
get_image_info            → one image: channels, dims, label props, task history
get_task_log              → log content for a specific task run on a specific image
get_task_history          → recent tasks across all images: function, status, attempt count, timestamp
get_image_notes           → user-written notes for a specific image
read_lab_log              → full lab log content
get_qc_metrics            → per-image QC flags for a given stage
```

**Write (Phase 1 — lab log only):**
```
append_lab_log            → append a dated [Claude] entry. Append-only, never edits existing content.
```

**Write (Phase 2 — deferred):**
```
submit_task               → propose and submit a task (requires user confirmation)
adjust_params             → propose parameter adjustment for a flagged image
acknowledge_flag          → user acknowledges a QC flag, pipeline proceeds
```

Only expose endpoints that already exist and are stable in the Julia API. Add missing routes as thin read-only endpoints before wiring into MCP.

## Transport

stdio — standard for local MCP servers. Claude Code connects over stdin/stdout. The MCP server starts alongside the Julia API or independently for sessions where the GUI isn't running.

## The observer session (sit next to me mode)

The observer session is a Claude chat window with MCP access to the running Cecelia project. The user opens it and says "just sit next to me" or "watch what I'm doing." Claude doesn't wait to be asked — it monitors and surfaces observations when something is worth saying.

**Event-driven, not continuous.** Claude does not stream logs in real time — that would be prohibitively expensive in tokens. Instead, the MCP server pushes event notifications to Claude when specific things happen:

```
task_completed            → image UID, function name, outcome, attempt count
task_failed               → image UID, function name, error summary, attempt count
image_note_added          → image UID, note content
qc_flag_fired             → image UID, stage, metric, value, cohort comparison
lab_log_entry_added       → new entry summary (user-written entries only)
```

On each event, Claude receives a compact context packet (the event + relevant recent history for that image) and decides: stay silent, or surface an observation. Most events → silence. Claude speaks when the pattern is worth naming.

**The 10-attempts pattern.** If `task_completed` or `task_failed` fires with attempt_count > 3 for the same function on the same image, Claude surfaces it: "You've run cellpose on image 7 ten times. The outcome has been consistent each time. Want to talk through what you're trying to achieve?" This is the core value — pattern recognition across attempts that the user is too close to notice.

**The why question.** When a user does something unexpected — a parameter choice that deviates from prior runs, an image note that suggests an unusual decision — Claude asks why. Not to judge, to record. The answer goes in the lab log. Lab heads want figures. This is how the methodology doesn't get lost.

## Token cost

The event-driven approach keeps costs manageable. Each event triggers one Claude call with a compact context packet — not a full project dump. Estimate: 10-20 events per analysis session, each ~2-3k tokens of context. Comparable to a normal chat session. The observer session does not stream continuously.

**Honest caveat**: this estimate is optimistic for heavy sessions. 20 images with multiple failed tasks, cohort stats, and QC flags could be 10x that. Measure actual token cost in practice and implement a throttle — a configurable max events per session before Claude stops surfacing observations and just logs silently. The user should be able to see running token cost and disable the observer session if it becomes prohibitive.

## Early calibration warning

The observer session becomes more valuable over time, not immediately. Early in a project, before the lab log is rich and before Claude has seen the cohort, its sense of what's "unexpected" is poorly calibrated. It may ask obvious questions or miss genuinely unusual decisions. Set user expectations accordingly — the first few sessions are about building context, not catching errors.

## Verify

- Connect Claude to MCP, open a project — Claude describes project state correctly without being told
- Run a task that fails 3 times — Claude surfaces the pattern without being asked
- Add an image note — Claude acknowledges it and incorporates it in context
- Append to lab log via MCP — entry appears with correct [Claude] tag and date
- Confirm no mutations possible beyond lab log append
- Confirm token usage is reported per session and throttle fires correctly
