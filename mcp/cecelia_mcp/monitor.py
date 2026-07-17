"""Session monitor — the observer's live pattern detection, driven off the Julia API's WS stream.

The MCP server holds one ``SessionMonitor`` for the life of the process. A background task feeds it
normalized events (see ``normalize_frame``) as they arrive on the WebSocket; the ``poll_observations``
tool drains the observations it has accumulated. This is the "sit next to me" signal, but pull-based:
Claude asks what's worth surfacing rather than the server pushing unsolicited (MCP is client-pull;
server→client push for the continuous version is a later slice — see docs/ai-assist/OBSERVER.md §7).

Everything here is pure and thread-safe (a single lock): no network, no MCP SDK. That keeps the core
signal — the 10-attempts pattern — unit-testable without a live backend or Claude Code.

The 10-attempts pattern (OBSERVER.md §"The 10-attempts pattern"): counting is **session-scoped and
launch-path-agnostic**. A user iterating on cellpose params re-runs the same function on the same
image many times — sometimes as whiteboard chain nodes, sometimes as module-page single tasks, each
a fresh submission. We count terminal outcomes per (image_uid, fn) off the event stream, so every
launch path lands in the same tally, and "you've run this N times *this session*" is exactly what the
pattern means (a persisted counter would resurface stale weeks-old counts as noise).
"""
from __future__ import annotations

import threading

# Surface the repeat-attempts pattern once the count EXCEEDS this (so >3 ⇒ fires on the 4th run),
# matching OBSERVER.md / QC-PROCESS.md ("same function run >3 times on the same image").
DEFAULT_ATTEMPT_THRESHOLD = 3

# Terminal chain node states that are NOT a real attempt at the task (see chain.jl: node:failed is
# fired for :failed, :skipped AND :cancelled). Only genuine failures count toward the pattern.
_NON_ATTEMPT_NODE_STATES = frozenset({"skipped", "cancelled"})
# task:status values that are not a terminal outcome — ignored for counting.
_NON_TERMINAL_TASK_STATUS = frozenset({"queued", "cancelled"})

# ── Throttle / token-cost defaults (OBSERVER.md §6) ───────────────────────────────────────────────
# Max observations SURFACED to the chat per session before the observer goes quiet — after this it
# stops emitting to chat and the suppressed observations are logged to the lab log instead
# ("silent-equivalent"). The doc's honest caveat: heavy sessions can be ~10x the ~10-20-event
# estimate, so this cap is the hard stop. Calibrate against a real live run (it's just a default).
DEFAULT_SURFACE_CAP = 20
# Rough tokens per surfaced observation packet (event + the context Claude pulls around it), from the
# doc's "~2-3k tokens of context" estimate. Used ONLY for a running per-session ESTIMATE the observer
# can report — the server can't see Claude's real token usage. Deliberately conservative.
EST_TOKENS_PER_OBSERVATION = 2500


def normalize_frame(frame: dict) -> dict | None:
    """Map one raw WS frame to a normalized observer event, or None if it isn't one we track.

    Handles both launch paths: whiteboard chain nodes (``chain:node:*`` — carry ``fn``) and
    module-page single tasks (``task:status`` — carry ``fun``, added backend-side for the observer).
    Also the two new backend broadcasts wired in this slice: ``image_note_added`` / ``lab_log_entry_added``.
    """
    t = frame.get("type")

    if t == "chain:node:running":
        return {"kind": "task_running", "image_uid": frame.get("imageUid", ""),
                "fn": frame.get("fn", ""), "project_uid": frame.get("projectUid", "")}
    if t == "chain:node:done":
        return {"kind": "task_completed", "image_uid": frame.get("imageUid", ""),
                "fn": frame.get("fn", ""), "project_uid": frame.get("projectUid", "")}
    if t == "chain:node:failed":
        # node:failed doubles as skipped/cancelled — only a real failure is an attempt.
        if str(frame.get("status", "failed")) in _NON_ATTEMPT_NODE_STATES:
            return None
        return {"kind": "task_failed", "image_uid": frame.get("imageUid", ""),
                "fn": frame.get("fn", ""), "project_uid": frame.get("projectUid", "")}

    if t == "task:status":
        status = str(frame.get("status", ""))
        if status in _NON_TERMINAL_TASK_STATUS or status == "running":
            # running isn't a terminal outcome; queued/cancelled aren't attempts.
            return None if status != "running" else {
                "kind": "task_running", "image_uid": frame.get("imageUid", ""),
                "fn": frame.get("fun") or frame.get("funName") or "", "project_uid": ""}
        kind = "task_completed" if status == "done" else ("task_failed" if status == "failed" else None)
        if kind is None:
            return None
        return {"kind": kind, "image_uid": frame.get("imageUid", ""),
                "fn": frame.get("fun") or frame.get("funName") or "", "project_uid": ""}

    if t == "image_note_added":
        return {"kind": "image_note_added", "image_uid": frame.get("imageUid", ""),
                "note": frame.get("note", ""), "project_uid": frame.get("projectUid", "")}
    if t == "lab_log_entry_added":
        return {"kind": "lab_log_entry_added", "summary": frame.get("summary", ""),
                "project_uid": frame.get("projectUid", "")}

    return None


class SessionMonitor:
    """Accumulates observations from normalized events; ``poll`` drains them.

    Thread-safe: ``record`` runs on the WS listener thread, ``poll`` on the MCP tool thread.
    """

    def __init__(self, threshold: int = DEFAULT_ATTEMPT_THRESHOLD,
                 surface_cap: int = DEFAULT_SURFACE_CAP):
        self._threshold = threshold
        self._surface_cap = surface_cap
        self._lock = threading.Lock()
        # (image_uid, fn) → {"completed": int, "failed": int}
        self._outcomes: dict[tuple[str, str], dict[str, int]] = {}
        # (image_uid, fn) → latest repeat-attempts observation (coalesced: one per key, updated in place)
        self._pending_attempts: dict[tuple[str, str], dict] = {}
        # note / lab-log events — each distinct, kept as an ordered list
        self._pending_events: list[dict] = []
        # ── throttle / off-switch state ──
        self._enabled = True             # the off switch: when False, poll surfaces nothing
        self._surfaced_count = 0         # observations returned to the chat this session
        # observations suppressed by the cap, awaiting a silent flush to the lab log (server drains)
        self._to_log: list[dict] = []

    def record(self, event: dict) -> None:
        """Fold one normalized event into session state. Unknown/None-ish events are ignored."""
        if not event:
            return
        kind = event.get("kind")
        if kind in ("task_completed", "task_failed"):
            self._record_attempt(event)
        elif kind == "image_note_added":
            with self._lock:
                self._pending_events.append({
                    "type": "image_note_added",
                    "imageUid": event.get("image_uid", ""),
                    "note": event.get("note", ""),
                    "projectUid": event.get("project_uid", ""),
                })
        elif kind == "lab_log_entry_added":
            with self._lock:
                self._pending_events.append({
                    "type": "lab_log_entry_added",
                    "summary": event.get("summary", ""),
                    "projectUid": event.get("project_uid", ""),
                })
        # task_running is tracked implicitly (we count terminal outcomes only) — ignored here.

    def _record_attempt(self, event: dict) -> None:
        uid = event.get("image_uid") or ""
        fn = event.get("fn") or ""
        if not uid or not fn:
            # Can't attribute (e.g. a module-page task frame from a backend without `fun`) — don't
            # count it against a phantom key. Better to under-count than misattribute.
            return
        key = (uid, fn)
        outcome = "failed" if event["kind"] == "task_failed" else "completed"
        with self._lock:
            tally = self._outcomes.setdefault(key, {"completed": 0, "failed": 0})
            tally[outcome] += 1
            total = tally["completed"] + tally["failed"]
            if total > self._threshold:
                self._pending_attempts[key] = {
                    "type": "repeat_attempts",
                    "imageUid": uid,
                    "fn": fn,
                    "attempts": total,
                    "completed": tally["completed"],
                    "failed": tally["failed"],
                    "lastOutcome": outcome,
                    "projectUid": event.get("project_uid", ""),
                }

    def poll(self, project_uid: str = "") -> list[dict]:
        """Return and clear pending observations for surfacing to the chat.

        - If ``project_uid`` is given, only observations for that project (or of unknown project —
          module-page task frames don't carry one) are drained; others stay pending for a poll that
          names their project.
        - **Off switch:** when disabled, returns [] and leaves everything pending (so re-enabling
          resurfaces it). The WS listener keeps counting attempts regardless.
        - **Throttle:** once ``surface_cap`` observations have been surfaced this session, further
          drained observations are NOT returned — they're queued for a silent lab-log flush
          (``drain_for_log``) instead. This is the doc's "stop emitting to chat, only append to the
          lab log" cap that bounds token cost.
        """
        with self._lock:
            if not self._enabled:
                return []
            drained = self._drain_matching(project_uid)
            remaining = max(0, self._surface_cap - self._surfaced_count)
            surface, overflow = drained[:remaining], drained[remaining:]
            self._surfaced_count += len(surface)
            self._to_log.extend(overflow)   # suppressed → silent lab-log flush
            return surface

    def _drain_matching(self, project_uid: str) -> list[dict]:
        """Remove and return pending observations matching project_uid (attempts first, then events).
        Non-matching ones are left pending. Caller holds the lock."""
        keep_attempts: dict[tuple[str, str], dict] = {}
        out: list[dict] = []
        for key, obs in self._pending_attempts.items():
            if _matches_project(obs, project_uid):
                out.append(obs)
            else:
                keep_attempts[key] = obs
        keep_events: list[dict] = []
        for obs in self._pending_events:
            (out if _matches_project(obs, project_uid) else keep_events).append(obs)
        self._pending_attempts = keep_attempts
        self._pending_events = keep_events
        return out

    def drain_for_log(self) -> list[dict]:
        """Return and clear observations the throttle suppressed — the server flushes these to the
        lab log so a throttled session still records patterns silently, without spending chat tokens."""
        with self._lock:
            out, self._to_log = self._to_log, []
            return out

    def set_enabled(self, enabled: bool) -> None:
        """The off switch. Disabling stops chat surfacing (poll → []); attempt counting continues."""
        with self._lock:
            self._enabled = bool(enabled)

    def stats(self) -> dict:
        """Running per-session observer state — surfaced count, throttle status, token ESTIMATE."""
        with self._lock:
            return {
                "enabled": self._enabled,
                "surfacedCount": self._surfaced_count,
                "surfaceCap": self._surface_cap,
                "throttled": self._surfaced_count >= self._surface_cap,
                "estimatedTokens": self._surfaced_count * EST_TOKENS_PER_OBSERVATION,
                "estTokensPerObservation": EST_TOKENS_PER_OBSERVATION,
                "pendingLogFlush": len(self._to_log),
            }


def _matches_project(obs: dict, project_uid: str) -> bool:
    if not project_uid:
        return True
    op = obs.get("projectUid", "")
    return op == "" or op == project_uid
