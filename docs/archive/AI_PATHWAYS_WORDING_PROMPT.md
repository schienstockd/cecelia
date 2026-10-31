> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> AI wording lives in [`frontend/src/lib/claudeOverview.ts`](../../frontend/src/lib/claudeOverview.ts)
> and [`user-docs/README.md`](../../user-docs/README.md) → *Working with Claude*; the observer's tools in
> [`docs/ai-assist/OBSERVER.md`](../ai-assist/OBSERVER.md); Kiwi's design in
> [`docs/todo/KIWI_ASSISTANT_PLAN.md`](../todo/KIWI_ASSISTANT_PLAN.md).
>
> **Outcome (2026-10-31):** audited (history, capabilities from code, string inventory); not as a
> single proposal PR. Dominik's call: **two ways in, not a loop**. Kiwi (structured rubber duck) and
> Claude Code (the interactive route) sit side by side, either first, Blackboard as the bridge. Kiwi's
> `?` now shows the two modes with a schematic each, plus *What each can use* — `lib/claudeTools.ts`,
> pinned by its test to the real tool registrations and `KIWI_READ_TOOLS` (replaces the hand-written
> `CLAUDE_ACCESS` table). The Guides toggle is *AI help*. Decision in
> `docs/todo/WORKFLOW_RECIPES_PLAN.md` → *Two ways in*. Still open: one name per surface (the
> `components/kiwi/` naming ratchet still bans "Claude"), ✦ meaning only "Claude made this" (Cleanup,
> What's new and the drift guide still use it — needs a new glyph). Premise corrections, checked
> against `origin/main` on archiving:
> - c4045a2 ("one explanation of Claude") has **merged** via #1841, so `claudeOverview.ts` and its
>   test are on `main`, not only on `docs/user-docs`.
> - The #1841 renames have **landed** (e.g. *Whiteboard*); use them.
> - #1852 (`add_gate`) was open on archiving and merged the same day; its tool is a line in
>   `claudeTools.ts`.
> - `pixi run chat` exists (`pixi.toml` → `chat.py`), so the third surface is real.

# AI_PATHWAYS_WORDING_PROMPT

Audit how users are meant to use AI inside Cecelia, find the **simplest** change that removes the confusion, and propose it. Wording and layout are the starting point, not the limit. Audit first, propose, wait for go-ahead, then edit.

## Motivation (read this first)

Users can't tell how to use the AI in Cecelia. Seen in the Guides screen (Multiplex tissue recipe):

- Two AI paths exist (in-app **Kiwi** and a free-text **Claude Code terminal session**) and the UI presents them as one sequence (Point at it → Ask Kiwi → Keep it → **Go further** = terminal). That implies the terminal is step 4 after Kiwi. It isn't.
- The sparkle icon has no legend and appears in two places with different apparent meanings.
- What Kiwi does at each step is only in a hover tooltip (which covers adjacent text and fails on touch/keyboard).
- The how-to cards sit below the page grid, the least important block.
- The toggle says "Claude", the cards say "Kiwi". It is unclear what the toggle controls and what happens to the sparkles when it is off.
- Jargon assumed known: "Share a frame", "@ Add to Kiwi", "tag it good or bad on the Blackboard", "Go further".

The goal is **less confusion about how to use AI with Cecelia**. Better wording may not be the best fix. There may be a simpler structural solution (one entry point, one name, fewer concepts). The audit must look for it and not assume the current two-path structure is right.

## Already done (commit c4045a2 on `docs/user-docs`, read from the diff only)

Commit "one explanation of Claude — the loop, everywhere it's shown" already moved part of this forward. Build on it, don't redo it.

- `frontend/src/lib/claudeOverview.ts` is now the single source: `CLAUDE_LOOP` (Point at it → Ask Kiwi → Keep it → Go further), `CLAUDE_ACCESS` (one Kiwi-vs-terminal table) and `CLAUDE_PROVISIONAL`. It feeds Kiwi's `?` dialog, the "Assist with Kiwi" guide, the daily Claude tip and `user-docs/README.md`. The old capability grid (Sees/Suggests/Creates/Can't) and example chips are gone.
- `user-docs/README.md` gets a **third column for `pixi run chat`**: no GUI, experimental, runs steps, edits gates, brings in data, each call approved; no captures, no LabArchives under `--strict-mcp-config`. So there may be **three** surfaces, not two: Kiwi, a terminal session attached to the running app (the "observer"), and standalone `pixi run chat` (likely the "autonomous server"). Verify and name them consistently.
- Tests pin the table: Kiwi only reads (no row lets Kiwi do what the terminal can't); terminal-only rows include chain, board, notebook and "Marks cells"; one row says neither mode runs a task or chain or changes or deletes the user's work; one says it warns when a bad-tagged failure keeps coming back.
- Not clicked through in the app. `claudeOverview.ts` itself and the rendered README were not read (only the diff and commit message), so row wording is unverified.

What is still open after it:

1. **"Go further" is still step 4 of the loop**, so the terminal is still framed as what you do after Kiwi. Decide whether the loop stays a single sequence with the terminal as an optional exit, or becomes two modes with their own flows.
2. **The table is copy, not capability.** Its rows must match the real tool allow-lists. PR #1852 adds `add_gate` (terminal only, additive, stamped, undoable) and the table has no row for it. Derive the matrix from the allow-lists / MCP tool registrations, or add a test pinning the rows to them, so the table can't drift.
3. **Scope is not specific enough.** The two-column table says yes/no. Users need: what each mode can *read*, what it can *propose or add*, what it can *never* do, and what happens to the result (kept as a record, marked, undoable).
4. The Kiwi dialog header still uses `pi-sparkles`, and the Guides map and the population manager's "Proposed by Claude" marker use the same icon.

## Working model (hypothesis from discussion, to be verified, not decided)

**Kiwi: the rubber duck.** Points at evidence and asks questions back. Reads what you share (frame, selection, table), shows what supports or undermines a claim, names the knob worth checking. Does not co-drive the analysis or make decisions. You stay in charge.

**Terminal session: the analysis co-driver.** Free-text interactive Claude Code session on your project. You work through the analysis *with* it. It is more than "run a function for me"; it is a collaborator you drive alongside.

They have different purposes and neither is a step after the other. Either may be opened first.

## Discussion so far (positions to test, not conclusions)

1. **Keep the distinction.** The two modes make different promises. "Points at evidence, you decide" is a guardrail against users reading Kiwi output as "the AI says the cluster is real". Merging into one general assistant may blur that.
2. **Unify the surface anyway.** Two names, two entry points and two mental models is part of the confusion. One option: one assistant, one name, two stances (e.g. *Look with me* / *Work it through*), with Kiwi as a restricted read-only profile of the same engine (plugin/subagent) so the read-only promise is enforced by configuration, not just wording.
3. **Counter-argument against folding Kiwi into the terminal.** Kiwi emits **data links** that jump to the data when clicked, because it runs inside the app. A terminal session only prints text. If Kiwi moved into the terminal, that evidence-checking loop may break or weaken. Possible mitigation: terminals support clickable hyperlinks (OSC 8); if the links are routable URLs the app handles, the terminal session could emit the same links. That would be a worse experience (separate window, raw links, unclear whether the app view focuses or a new tab opens). Whether it works at all depends on how the links are implemented, which has not been checked.
4. **Other reasons for caution on a full merge:** many users may never open a terminal; a Claude Code plugin path may imply a Claude Code seat per user (cost/licensing); "read-only mode inside a general session" is only as strong as the permissions around it; a quick question shouldn't need a full session.
5. **Concrete example where the modes differ: PR #1852** (`add_gate`). Per the author, adding a gate is Claude Code territory and Kiwi cannot do it. The PR adds one additive `flow` gate as a new child population via the observer allow-list (`add-gate` only; set-gate/delete/rename/move stay off), stamps it with provenance (`createdBy` = profile, via, model, session), shows a ✦ "Proposed by Claude" marker in the population manager, and Undo removes it. So the split is already enforced by an allow-list, not just by wording, and the co-driver is bounded (proposes, marked, undoable). Two implications to check: (a) the one-sentence definition of the terminal mode should say what it can and cannot change; (b) the ✦ in the population manager reuses the same sparkle icon (`pi-sparkles`) as the Guides map, where it means "Kiwi can help at this step". Same icon, different meaning and possibly a different actor. Verify whether the stamp distinguishes Kiwi from the terminal session (`via`), and whether "Proposed by Claude" is consistent with the naming decision.
6. **Unresolved.** I do not know the answers to the link mechanism, access/cost, or the toggle's scope. The audit decides.

## Claude's take (opinion from the chat discussion; test it, don't adopt it)

Written without seeing the code. Treat as a prior to confirm or refute.

- **Likely Option A plus one structural cut.** The confusion looks like presentation, not architecture: the terminal shown as "step 4" after Kiwi, a sparkle with no legend, hints hidden in hover, two names for the AI. All fixable without touching the engines.
- **The structural cut:** take the terminal out of the recipe guides. Recipes are analysis steps, and Kiwi's sparkles fit there because it helps *at a step*. The terminal co-driver isn't tied to a step. Give it one clearly separate place with its own one-line description. That alone removes the false-sequence problem.
- **Don't merge Kiwi into the terminal.** Click-to-evidence is what makes Kiwi trustworthy: the user checks the claim instead of believing the sentence. Trading that for naming tidiness is a bad deal. Revisit a unified engine only if the link mechanism demonstrably works from a terminal, and don't let it drive this decision.
- **Decide the toggle first.** If nobody can say what "Claude" gates, no wording fixes that. Settle its scope before the wording.
- **Caveats on this take:** if the data links are plain routable URLs, the case against merging weakens. And if the audit comes back proposing a big restructure, the likely right answer is "do A and stop". The audit is worth it mainly for the link mechanism and toggle scope. Do not let it grow beyond that.

## Icon and toggle proposal (evaluate; don't assume)

Proposed in discussion, intended to fix the sparkle collision and make the toggle's meaning obvious:

- **🥝 (a kiwi icon) = Kiwi.** Used for step hints, the Kiwi panel/dialog header and anything Kiwi pointed at.
- **✦ (sparkle) = Claude, the terminal-session co-driver.** Used where a terminal session proposes or does something (e.g. "Proposed by Claude" on a gate from #1852) and wherever a recipe step suggests the terminal.
- One icon per actor, never both for one thing. If either icon starts being used for the other mode, the confusion returns.
- **The toggle becomes "AI"**, and the Guides screen shows **both markers per step** so users see at a glance which mode is relevant: 🥝 where Kiwi works, ✦ where the terminal session works, both where either does. This also stops recipes implying a fixed sequence.
- **Icon implementation:** a custom SVG in the app's icon set (the repo re-vendors `icons.css`), not the 🥝 emoji character, so it matches theme colours and renders the same everywhere. Keep a text label/`aria-label` so it is not icon-only.
- **Open for Opus:** what the "AI" toggle actually controls. The terminal exists outside the GUI, so the toggle can probably only show/hide markers and hints, not gate the terminal itself, unless it already gates something in code. State that plainly in its label or tooltip. Decide what is shown when it is off.
- **Naming:** Kiwi also runs Claude underneath, so "Claude" alone is ambiguous. Propose labels for the three surfaces (Kiwi / terminal session / `pixi run chat`) and use them in the dialog, guides, README, tooltips and the provenance stamp.

## Do first (read, then report before proposing)

1. Read `CLAUDE.md`, `docs/MAINTAINABILITY.md`, `user-docs/README.md`, `frontend/src/lib/claudeOverview.ts` (and its test), and the Kiwi and terminal-session code (e.g. `KiwiCockpit.vue` and the session/terminal bridge). Check whether `docs/user-docs` and #1852 have merged and which names they use. Find where each of these lives: recipe step hints, sparkle tooltips, the 4 cards, the toggle label, Share / Add-to-Kiwi buttons, Blackboard tagging UI, empty states.
2. State precisely from code (tool allow-lists, MCP server tool registrations, not UI copy) what each mode can read, propose/add, change and run, and where they share state (Blackboard, captures, notes). Distinguish checked-by-running from read-in-code. Check the working model above against this; correct it if wrong.
3. **Data links.** How are Kiwi's clickable data links implemented (in-app event, route, URL, custom scheme)? Can the terminal session emit them? Can a click from a terminal reach the running app and focus the right view? Test it if you can, otherwise say you couldn't.
4. **Could Kiwi be a restricted profile of the same engine as the terminal session** (tools, prompt, permissions)? What would break, and what in-app features does Kiwi use that the terminal path lacks (frame sharing, Blackboard tagging, in-app context)?
5. **Who can use what?** Which users have Claude Code access and what does each path cost them? What does the Claude toggle enable or gate? How does Kiwi authenticate today (the user's own logged-in Claude Code CLI session, or a login Cecelia runs itself via the SDK)? Report which, because it affects licensing: Anthropic's terms, as quoted secondhand, say third parties can't offer Claude.ai login or route requests through users' plan credentials, and the Agent SDK has its own credit rules. Verify against Anthropic's official legal-and-compliance page for Claude Code, not against this note. What does a user **without** Claude Code see, and do the guides and map state the prerequisite plainly (AI features optional, require a Claude Code plan)?
6. Inventory every user-visible string naming the AI ("Kiwi", "Claude", "Claude Code", "assistant", "terminal", "session", "agent") and report inconsistencies. List every entry point for each mode.
7. Find where wording is defined (guide data, components, `guides.json`, the generated map) so edits land in one place.

## Options to compare (include any simpler one you find)

- **A. Wording and layout only.** Keep two paths. Two-mode side-by-side presentation, a "which one?" cue, sparkle legend, inline per-step hints, one name per mode, jargon pass.
- **B. One entry point, two stances.** Single assistant name and entry; stance chosen at use. Kiwi stays in-app; terminal session reachable from the same place.
- **C. Unify the engine.** Kiwi as a restricted profile/plugin of the terminal engine. Evaluate against the data-link and access findings.
- **D. Simpler than all of these**, if the audit finds one (e.g. drop a path from recipes, remove the toggle, show the AI only at the steps where it helps).

For each: what confusion it removes, what it costs (PRs, risk), what it breaks (especially the click-to-evidence loop), and its verdict. Prefer the smallest change that makes the usage obvious. "A is enough" is an acceptable and welcome result.

## Constraints

- Do not oversell. No wording that implies Kiwi decides, validates or analyses on the user's behalf, and none that implies the terminal session is autonomous or verified. The pointer-to-evidence role of Kiwi and the user's ownership of the analysis must be visible in the words.
- Do not change behaviour in this PR. If the audit finds behaviour itself is unclear (e.g. toggle scope), report it as a separate finding.
- Follow `docs/MAINTAINABILITY.md` and the inventory check. Regenerate `guides.json` only if guide text changes. Preserve ids and routes.
- Keep it short. Cut words before adding UI.

## Out of scope

- The map, guide accordion and generated docs (`INAPP_MAP_PROMPT.md`). Coordinate placement, don't duplicate.
- Code-doc generation (`CODE_DOCS_AUDIT_PROMPT.md`).
- Building new AI features or the plugin/engine unification itself. Evaluate only.
- The renames from #1841 (Whiteboard, "Capture activity", Profile). If they land first, use the new names.

## Output

1. **Findings:** a precise **capability matrix** per surface (Kiwi / terminal session / `pixi run chat`) derived from code: reads, proposes/adds, never does, and what happens to the result; plus the gaps between it and `CLAUDE_ACCESS`. Also: capability statement per mode, the link mechanism, access/cost, toggle scope, string inventory, entry points. Mark each as checked-by-running or read-in-code.
2. **Options table:** A to D (plus any you add), with the comparison above and a recommendation with reasons.
3. **Proposal for the recommended option:** names, one-sentence definition of each mode, layout (sketched in text), sparkle and toggle semantics, every string old → new.
4. **Open questions** the code can't answer.
5. After my go-ahead: one PR with the wording/layout edits only. List reservations; state what was checked in the running app versus read in code.
