import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref, computed } from 'vue'
import {
  DEFAULT_GROUPS, gapBefore, logGroup, restoreGroups, storeGroups,
  type LogGroup, type LogLevel,
} from '../utils/logFilter'
import { clearUiLogRing, onUiLog, publishUiLog, readUiLogRing } from '../lib/uiLogChannel'

export type { LogLevel, LogGroup }

export interface LogEntry {
  id: number
  level: LogLevel
  message: string
  detail?: string   // stack trace, server response body, a child's traceback block
  source?: string   // 'backend' | 'napari' | 'preview' | 'runner' | 'notebooks' | a UI tag
  timestamp: Date
  /** The backend ring's monotonic sequence, when this entry came from there. Absent for UI entries. */
  seq?: number
}

let _id = 0

const GROUPS_KEY = 'cc.consoleGroups'

/** Read the persisted chip selection. A user-settable option must survive a reload (CLAUDE.md), and a
 *  chip added since the selection was saved must arrive ON — see `restoreGroups`. */
function loadGroups(): LogGroup[] {
  try { return restoreGroups(localStorage.getItem(GROUPS_KEY)) }
  catch { return [...DEFAULT_GROUPS] }     // private mode
}

export const useLogStore = defineStore('log', () => {
  const entries = ref<LogEntry[]>([])
  const unreadErrors = ref(0)
  const consoleOpen = ref(false)

  /** Which component chips are on. Persisted — see `DEFAULT_GROUPS` for why the children start off. */
  const groups = ref<LogGroup[]>(loadGroups())
  const query = ref('')
  /** Autoscroll. Pauses itself when you scroll up — see ErrorConsole; a console that yanks you back
   *  to the bottom while a task streams cannot be read, and that is the state it is now usually in. */
  const follow = ref(true)

  /** Highest backend `seq` this tab has seen. The cursor for backfill + gap repair. */
  const lastSeq = ref(0)
  /** Which ring that cursor belongs to. A restarted backend counts from 1 again, so a cursor held
   *  across the restart would make the new ring's first N records look like ones we already had. */
  const ringId = ref('')
  let repairing = false
  // Seq numbers already taken from the CURRENT ring. Kept here rather than derived from `entries`,
  // which was subtly wrong: after a backend restart the store still holds the previous ring's entries
  // (correctly — they are history), and their seq numbers overlap the new ring's. Deriving the de-dup
  // set from them would make a genuine new record with seq 7 look like one we already had.
  let seenSeq = new Set<number>()

  function push(level: LogLevel, message: string, opts?: { detail?: string; source?: string; seq?: number; ts?: string }) {
    entries.value.push({
      id: _id++,
      level,
      message,
      detail: opts?.detail,
      source: opts?.source,
      // The backend stamps `ts` where the line happened. Honour it: a reconnect backfill arrives
      // minutes late and all at once, and stamping locally would file it all under "now".
      timestamp: opts?.ts ? new Date(opts.ts) : new Date(),
      seq: opts?.seq,
    })
    // cap history so the teed server-log stream can't grow the store unbounded over a long session
    if (entries.value.length > 3000) entries.value.splice(0, entries.value.length - 3000)
    if (level === 'error' && !consoleOpen.value) unreadErrors.value++
  }

  // The three UI-side entry points also tee to `publishUiLog`, which is what makes a `logStore.info()`
  // in the main window reach the console popout — a separate app instance with its own store — and
  // land in the persisted ring a popout opened LATER hydrates from. `push` itself does NOT publish, so
  // `pushServer` (backend ring, arrives on each window's own WS) and the `onUiLog` handler below
  // (already a peer's line) never echo. See `lib/uiLogChannel.ts`.
  function info(message: string, opts?: { detail?: string; source?: string }) {
    push('info', message, opts)
    publishUiLog({ level: 'info', message, detail: opts?.detail, source: opts?.source ?? 'app' })
  }
  function warn(message: string, opts?: { detail?: string; source?: string }) {
    push('warn', message, opts)
    publishUiLog({ level: 'warn', message, detail: opts?.detail, source: opts?.source ?? 'app' })
  }
  function error(message: string, opts?: { detail?: string; source?: string }) {
    push('error', message, opts)
    publishUiLog({ level: 'error', message, detail: opts?.detail, source: opts?.source ?? 'app' })
  }

  /**
   * A `server:log` frame from the backend ring.
   *
   * Two things happen that a plain `push` cannot do. The entry is filed under the backend's OWN
   * timestamp, and the `seq` is checked for a gap: WS telemetry here is lossy by design (a frame is
   * dropped rather than blocking a worker thread), and until now a dropped LOG line was simply gone
   * with nothing able to notice. Now the arithmetic notices, and `repairGap` fetches what was missed.
   */
  function pushServer(rec: { level?: string; message?: string; detail?: string; source?: string; seq?: number; ts?: string }) {
    const seq = Number(rec.seq ?? 0)
    if (seq && seenSeq.has(seq)) return          // a repair got here first
    const gap = gapBefore(seq, lastSeq.value)
    if (seq) seenSeq.add(seq)
    if (seq > lastSeq.value) lastSeq.value = seq
    const level = (rec.level === 'error' || rec.level === 'warn') ? rec.level : 'info'
    push(level as LogLevel, String(rec.message ?? ''), {
      detail: rec.detail, source: rec.source ?? 'backend', seq: seq || undefined, ts: rec.ts,
    })
    if (gap !== null) void repairGap(gap)
  }

  /**
   * Pull everything after `since` from the backend ring and splice it in.
   *
   * Also the cold-start path: a freshly-loaded page calls this with `since = 0` so the console opens
   * on what already happened rather than on the next line to arrive. (Only the pop-out window used to
   * do that, so the docked console — the one actually in front of you — always started empty.)
   *
   * Re-entrancy guarded: a burst of dropped frames triggers one repair, not one per frame.
   */
  async function repairGap(since: number) {
    if (repairing) return
    repairing = true
    try {
      const res = await fetch(`/api/logs/recent?since=${since}`)
      if (!res.ok) return
      const body = await res.json() as {
        ringId?: string
        logs: { seq?: number; level?: string; message?: string; detail?: string; source?: string; ts?: string }[]
      }
      // A different ring means a restarted backend: its seq numbers overlap ours but mean nothing in
      // common, so the cursor and the de-dup set both have to be abandoned rather than trusted. The
      // entries themselves stay — they are history, and the restart is the interesting part of it.
      if (!body.ringId || body.ringId !== ringId.value) {
        ringId.value = body.ringId ?? ''
        lastSeq.value = 0
        seenSeq = new Set<number>()
      }
      for (const l of body.logs ?? []) {
        if (l.seq !== undefined && seenSeq.has(l.seq)) continue    // the live frame beat us to it
        if (l.seq !== undefined) seenSeq.add(l.seq)
        const level = (l.level === 'error' || l.level === 'warn') ? l.level : 'info'
        push(level as LogLevel, String(l.message ?? ''),
             { detail: l.detail, source: l.source ?? 'backend', seq: l.seq, ts: l.ts })
        if ((l.seq ?? 0) > lastSeq.value) lastSeq.value = l.seq ?? lastSeq.value
      }
      // Backfill arrives out of order relative to what is already here (its stamps are older), so sort
      // by time — the console is a timeline, and an out-of-order splice reads as a second incident.
      entries.value.sort((a, b) => a.timestamp.getTime() - b.timestamp.getTime())
    } catch { /* server down — the live stream fills in once it is back */ }
    finally { repairing = false }
  }

  /** Cold start / reconnect: read the whole ring the backend still holds. */
  function backfill() { void repairGap(0) }

  function setGroups(next: LogGroup[]) {
    groups.value = next
    try { localStorage.setItem(GROUPS_KEY, storeGroups(next)) } catch { /* private mode */ }
  }

  function toggleGroup(g: LogGroup) {
    setGroups(groups.value.includes(g) ? groups.value.filter(x => x !== g) : [...groups.value, g])
  }

  function openConsole() {
    consoleOpen.value = true
    unreadErrors.value = 0
  }
  function closeConsole() {
    consoleOpen.value = false
  }
  function toggleConsole() {
    if (consoleOpen.value) closeConsole()
    else openConsole()
  }

  function clear() {
    entries.value = []
    unreadErrors.value = 0
    // Also wipe the cross-window ring — a popout opened next would otherwise hydrate the lines the
    // user just cleared, and Clear looks broken across windows. The backend server ring is untouched
    // (its cursor is `lastSeq`, held here; resetting it would make the next frame look like a 500-line
    // gap and refetch everything just cleared).
    clearUiLogRing()
  }

  const lastEntry = computed(() =>
    entries.value.length ? entries.value[entries.value.length - 1] : null
  )

  /** Per-group counts for the chip badges — computed over everything, so a hidden group still counts. */
  const groupCounts = computed(() => {
    const out: Record<string, number> = {}
    for (const e of entries.value) {
      const g = logGroup(e.source)
      out[g] = (out[g] ?? 0) + 1
    }
    return out
  })

  // Hydrate from the cross-window ring — the persisted history that makes a console popout opened
  // AFTER a line was said still see it. The live channel below covers going-forward; without this
  // rehydration the popout would still start blank of everything before it opened, which is the
  // reported bug. Kept small and best-effort: an unreadable ring falls back to no history rather than
  // taking the store down. See `lib/uiLogChannel.ts`.
  for (const l of readUiLogRing()) {
    entries.value.push({
      id: _id++, level: l.level, message: l.message, detail: l.detail,
      source: l.source, timestamp: new Date(l.ts),
    })
  }

  // Lines from the app's OTHER windows. A pop-out is a second app instance with its own store, and two
  // of them (the volume viewer, the Task Manager) render no console at all — so without this their
  // diagnostics have nowhere to go. Backend lines already reach every window from one ring, which is
  // why napari prints everywhere; this is the same for the ones the browser says. Installed on the
  // store rather than in App.vue because `main.ts` creates the store in every window, console or not,
  // and a window that cannot show a line is not the window that should decide to drop it.
  onUiLog(line => push(line.level, line.message,
                       { detail: line.detail, source: line.source, ts: line.ts }))

  return {
    entries, unreadErrors, consoleOpen, lastEntry, groups, query, follow, lastSeq, ringId, groupCounts,
    info, warn, error, push, pushServer, backfill, repairGap, setGroups, toggleGroup,
    openConsole, closeConsole, toggleConsole, clear,
  }
})

// Replace the live instance on hot-reload — see the note in `stores/customModules.ts`.
if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useLogStore, import.meta.hot))
