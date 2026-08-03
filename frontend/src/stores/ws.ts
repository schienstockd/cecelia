import { defineStore } from 'pinia'
import { ref } from 'vue'
import { useLogStore } from './log'
import { useTaskStore } from './tasks'
import { useProjectStore } from './project'
import { useProjectMetaStore } from './projectMeta'
import { useTaskDefsStore } from './taskDefs'
import { useLabCaptureStore } from './labCapture'
import { fetchRecentOutcomes, newestFinishedAt, recoveredTaskFrames } from '../utils/taskReconcile'
import { parseRailTime } from '../utils/taskElapsed'

export type WsStatus = 'connecting' | 'connected' | 'disconnected' | 'error'

type MessageHandler = (data: Record<string, unknown>) => void
const handlers = new Map<string, MessageHandler[]>()

const TERMINAL_STATUS = new Set(['done', 'failed', 'cancelled'])

export const useWsStore = defineStore('ws', () => {
  const status = ref<WsStatus>('disconnected')
  const lastPong = ref<string | null>(null)

  let socket: WebSocket | null = null
  let reconnectTimer: ReturnType<typeof setTimeout> | null = null
  let connectTimeoutTimer: ReturnType<typeof setTimeout> | null = null
  // Outcome backstop (see utils/taskReconcile.ts): polls how in-flight tasks ENDED, because the frame
  // that says so is droppable. Only runs while something is actually in flight in this tab, so an idle
  // app issues no extra requests.
  let outcomePollTimer: ReturnType<typeof setInterval> | null = null
  let outcomeSince = ''
  // Tasks whose terminal frame we RECONSTRUCTED. If the real one then turns up late, it must be
  // swallowed: re-running the completion side effects would refetch plots, reload napari and — the one
  // that actually corrupts something — count a second attempt in the observer's completion watch.
  // Same idea as the console's SEEN_TERM. Cleared per id, so `task:restart` on that id works normally.
  const recovered = new Set<string>()
  const OUTCOME_POLL_MS = 3000

  function connect() {
    // Only skip if already OPEN; do not skip if stuck in CONNECTING — the
    // socket may be hung from a previous attempt during server startup.
    if (socket && socket.readyState === WebSocket.OPEN) return

    // Kill any hung socket that never left CONNECTING.
    if (socket && socket.readyState === WebSocket.CONNECTING) {
      socket.onopen = null; socket.onerror = null; socket.onclose = null
      socket.close()
      socket = null
    }

    status.value = 'connecting'
    const log = useLogStore()
    log.info('Connecting to Julia backend…', { source: 'ws' })

    socket = new WebSocket(`ws://${location.host}/ws`)

    // Force-retry if the handshake hasn't completed within 5 s (e.g. Julia still starting).
    if (connectTimeoutTimer) clearTimeout(connectTimeoutTimer)
    connectTimeoutTimer = setTimeout(() => {
      if (socket && socket.readyState === WebSocket.CONNECTING) {
        socket.onopen = null; socket.onerror = null; socket.onclose = null
        socket.close()
        socket = null
        scheduleReconnect()
      }
    }, 5000)

    socket.onopen = () => {
      if (connectTimeoutTimer) { clearTimeout(connectTimeoutTimer); connectTimeoutTimer = null }
      status.value = 'connected'
      if (reconnectTimer) { clearTimeout(reconnectTimer); reconnectTimer = null }
      useLogStore().info('Connected to Julia backend', { source: 'ws' })
      // A reconnect means the backend may have restarted, so its outcome ring is a different one —
      // start reading it from the beginning again rather than from a cursor it never issued.
      outcomeSince = ''
      recovered.clear()
      startOutcomePoll()
      ping()
    }

    socket.onmessage = (event) => {
      dispatch(JSON.parse(event.data) as Record<string, unknown>)
    }

    socket.onclose = () => {
      if (connectTimeoutTimer) { clearTimeout(connectTimeoutTimer); connectTimeoutTimer = null }
      stopOutcomePoll()
      const wasConnected = status.value === 'connected'
      status.value = 'disconnected'
      if (wasConnected) {
        useLogStore().warn('Connection to Julia backend lost — retrying in 3 s', { source: 'ws' })
      }
      scheduleReconnect()
    }

    socket.onerror = () => {
      status.value = 'error'
      useLogStore().error('WebSocket error — check that Julia server is running on port 8080', { source: 'ws' })
    }
  }

  // ONE path for every task/chain frame — whether it arrived on the socket or was RECONSTRUCTED from
  // the backend's outcome ring (see startOutcomePoll). That's what makes the backstop a fix for all five
  // completion listeners (this store's image-status/dataVersion/meta refresh, ViewerPanel's napari
  // reload, TasksModule's auto-follow, useNapariAutoShow, the observer's completion watch) instead of a
  // second, drifting copy of the completion side effects.
  function dispatch(data: Record<string, unknown>) {
    const type = data.type as string | undefined

    // A late REAL terminal frame for a task we already recovered: drop it whole, listeners included.
    // Keyed by the SCHEDULER task id, which both carriers put on `taskId`.
    if (!data.recovered && (type === 'task:status' || type === 'chain:node:done' ||
                            type === 'chain:node:failed')) {
      const tid = String(data.taskId ?? '')
      const terminal = type !== 'task:status' || TERMINAL_STATUS.has(String(data.status ?? ''))
      if (tid && terminal && recovered.has(tid)) {
        recovered.delete(tid)
        return
      }
    }

    if (type === 'pong') {
      lastPong.value = new Date().toISOString()
    }

    if (type === 'error') {
      useLogStore().error(
        String(data.message ?? 'Unknown server error'),
        { source: 'ws', detail: JSON.stringify(data, null, 2) }
      )
    }

    // backend's own @info/@warn/@error (startup, napari warnings, …), teed by the server so the
    // console window is a real "pixi console" — not just task logs. See server.jl BroadcastLogger.
    if (type === 'server:log') {
      const level = (data.level === 'error' || data.level === 'warn') ? data.level : 'info'
      useLogStore().push(level as any, String(data.message ?? ''), { source: 'server' })
    }

    // a lab-log entry was appended by ANY path (incl. an external Chat-to-Claude MCP session) — reload
    // an open panel if it's for the current project. Reuses notifyAppended() (bumps the tick the panel
    // watches). Covers the case the frontend didn't initiate the append, so it has no other signal.
    if (type === 'lab_log_updated') {
      const puid = String(data.projectUid ?? '')
      if (puid && puid === useProjectMetaStore().current?.uid) useLabCaptureStore().notifyAppended()
    }

    if (type === 'task:progress') {
      const taskId   = String(data.taskId ?? '')
      const progress = Number(data.progress ?? 0)
      if (taskId) useTaskStore().setProgress(taskId, progress)
    }

    if (type === 'task:log') {
      const taskId = String(data.taskId ?? '')
      const line   = String(data.line   ?? '')
      if (taskId) useTaskStore().appendLog(taskId, line)
    }

    if (type === 'task:status') {
      const taskId  = String(data.taskId ?? '')
      const status  = String(data.status ?? '')
      // imageUid is now sent by the backend on every status message
      const imageUid = String(data.imageUid ?? '')
        || useTaskStore().tasks.find(t => t.id === taskId)?.imageUid
        || ''
      if (taskId && status) {
        // the backend's own timestamps — see utils/taskElapsed.ts for why they beat stamping locally
        useTaskStore().setStatus(taskId, status as any, {
          startedAt:  parseRailTime(data.startedAt),
          finishedAt: parseRailTime(data.finishedAt),
        })
        if (imageUid) {
          if (status === 'running') {
            useProjectStore().updateImageStatus(imageUid, 'converting')
          } else if (status === 'done' || status === 'failed') {
            useProjectStore().updateImageStatus(imageUid, status as any)
          }
        }
        // successful completion → the touched image(s)' data on disk may have changed (in place, same
        // value_name/suffix → no filepath change to react to). Bump each image's data version so only
        // the plots showing them refetch (targeted, not project-wide). A set/combined task sends the
        // full member list in `imageUids` (else fall back to the single `imageUid`) so EVERY member is
        // invalidated, not just the representative. Replaces the reload buttons.
        if (status === 'done') {
          const uids = Array.isArray(data.imageUids) && data.imageUids.length
            ? (data.imageUids as string[]) : (imageUid ? [imageUid] : [])
          for (const u of uids) useProjectStore().bumpDataVersion(u)
          // the task may have changed on-disk metadata (filepaths/labels/value_names) the store
          // doesn't know yet — refresh the touched image so the viewer/table aren't stale (fixes
          // legacy-migrate showing "No versions registered"; also import/segmentation/tracking).
          const projectUid = useProjectMetaStore().current?.uid
          if (projectUid && imageUid) useProjectStore().refreshImageMeta(projectUid, imageUid)
        }
      }
    }

    if (type === 'chain:log') {
      const line = String(data.line ?? '')
      // Forward to console bar
      if (line.startsWith('ERROR ')) {
        useLogStore().error(line, { source: 'chain' })
      } else {
        useLogStore().info(line, { source: 'chain' })
      }
      // Also route to the matching task entry's log.
      // Chain log format: "[imageUid/nodeId] actual line"
      // runId is NOT in the log prefix — match all task entries whose nodeId+imageUid fits.
      const m = line.match(/^\[([^/\]]+)\/([^\]]+)\] (.*)$/)
      if (m) {
        const [, imageUid, nodeId, rest] = m
        const taskStore = useTaskStore()
        // Find the most-recent chain task entry that matches this imageUid+nodeId
        const entry = taskStore.tasks.find(t =>
          t.chainNodeId === nodeId && t.imageUid === imageUid
        )
        if (entry) taskStore.appendLog(entry.id, rest)
      }
    }

    if (type === 'chain:run:failed') {
      useLogStore().error(
        `Chain run failed: ${String(data.error ?? 'unknown error')}`,
        { source: 'chain', detail: JSON.stringify(data) }
      )
    }

    if (type === 'chain:node:queued' || type === 'chain:node:running' ||
        type === 'chain:node:done'   || type === 'chain:node:failed') {
      const fn         = String(data.fn         ?? '')
      const imageUid   = String(data.imageUid   ?? '')
      const projectUid = String(data.projectUid ?? '')

      const defsStore = useTaskDefsStore()
      void defsStore.ensureLoaded()  // fire-and-forget; label falls back to fn if not ready
      const label = defsStore.labelFor(fn)

      const project   = useProjectStore()
      const imageName = project.sets.flatMap(s => s.images)
        .find(i => i.uid === imageUid)?.name ?? imageUid

      let status: 'queued' | 'running' | 'done' | 'cancelled' | 'failed'
      if (type === 'chain:node:queued') {
        status = 'queued'
      } else if (type === 'chain:node:running') {
        status = 'running'
      } else if (type === 'chain:node:done') {
        status = 'done'
      } else {
        // node:failed carries the real terminal status — :cancelled must not look :failed
        status = String(data.status ?? 'failed') === 'cancelled' ? 'cancelled' : 'failed'
      }

      useTaskStore().addFromChainEvent({
        runId:      String(data.runId      ?? ''),
        nodeId:     String(data.nodeId     ?? ''),
        chainName:  String(data.chainName  ?? ''),
        imageUid,
        imageName,
        fn,
        label,
        status,
        projectUid,
        // the scheduler task this node ran as — "" / absent for a node with no task id yet (skipped
        // before submission, set-scope). Recorded so a dropped terminal frame can be recovered.
        taskId:     String(data.taskId ?? ''),
        // the scheduler's own timing. A chain run emits no `task:status`, so these frames are the only
        // live carrier of it — without them a node's elapsed is timed from frame arrival.
        startedAt:  parseRailTime(data.startedAt),
        finishedAt: parseRailTime(data.finishedAt),
      })
    }

    if (type === 'napari:opened') {
      const imageUid = String(data.imageUid ?? '')
      if (imageUid) useProjectStore().napariImageUid = imageUid
    }

    if (type === 'task:result') {
      const imageUid = String(data.imageUid ?? '')
      const meta = (data.meta ?? {}) as Record<string, unknown>
      if (imageUid) {
        const patch: Record<string, unknown> = {}
        const removedValue = meta.removedValue as string | undefined
        if (removedValue) {
          // remove task: drop the deleted valueName from filepaths
          const store = useProjectStore()
          for (const set of store.sets) {
            const img = set.images.find(i => i.uid === imageUid)
            if (img?.filepaths) { delete img.filepaths[removedValue] }
          }
        }

        const addedValueName = meta.valueName as string | undefined
        const addedFilename  = meta.filename  as string | undefined
        if (addedValueName && addedFilename) {
          const store = useProjectStore()
          for (const set of store.sets) {
            const img = set.images.find(i => i.uid === imageUid)
            if (img) {
              if (!img.filepaths) img.filepaths = {}
              img.filepaths[addedValueName] = addedFilename
              break
            }
          }
        }
        // cropImage (and any task that produces a NEW image) reports the new uid + its set; pull the
        // fresh image payload and add it to the set so it appears without a full project reload.
        const newImageUid    = meta.newImageUid as string | undefined
        const newImageSetUid = meta.setUid as string | undefined
        if (newImageUid && newImageSetUid) {
          const projectUid = String((data.projectUid as string | undefined) ?? '')
            || useProjectMetaStore().current?.uid || ''
          if (projectUid) {
            // copyImage can target a brand-new set the store doesn't know yet — ensure it exists first
            // (no-op if already present; addImagesFromApi silently drops images for an unknown set).
            const newImageSetName = meta.setName as string | undefined
            if (newImageSetName) useProjectStore().ensureSet(newImageSetUid, newImageSetName)
            fetch(`/api/images/meta?projectUid=${projectUid}&imageUid=${newImageUid}`)
              .then(r => (r.ok ? r.json() : null))
              .then(d => { if (d?.image) useProjectStore().addImagesFromApi(newImageSetUid, [d.image]) })
              .catch(() => {})
          }
        }

        const labelValueName = meta.labelValueName as string | undefined
        const labelFiles     = meta.labelFiles as string[] | undefined
        if (labelValueName) {
          const store = useProjectStore()
          for (const set of store.sets) {
            const img = set.images.find(i => i.uid === imageUid)
            if (img) {
              if (!img.labels) img.labels = {}
              img.labels[labelValueName] = labelFiles ?? ['labels.zarr']
              break
            }
          }
        }

        if (meta.cleared) {
          // primary image removed: wipe dimensions/channels, reset image to pending
          patch.sizeC            = undefined
          patch.sizeT            = undefined
          patch.sizeZ            = undefined
          patch.channelNames     = []
          patch.physicalSizeX    = undefined
          patch.physicalSizeY    = undefined
          patch.physicalSizeZ    = undefined
          patch.physicalSizeUnit = undefined
          patch.physicalSizeZCorrected = undefined
          patch.timeIncrement    = undefined
          patch.timeIncrementUnit = undefined
          useProjectStore().updateImageStatus(imageUid, 'pending')
        } else {
          if (meta.SizeC !== undefined) patch.sizeC = Number(meta.SizeC)
          if (meta.SizeT !== undefined) patch.sizeT = Number(meta.SizeT)
          if (meta.SizeZ !== undefined) patch.sizeZ = Number(meta.SizeZ)
          if (Array.isArray(meta.channel_names)) patch.channelNames = meta.channel_names as string[]
          if (meta.PhysicalSizeX !== undefined) patch.physicalSizeX = Number(meta.PhysicalSizeX)
          if (meta.PhysicalSizeY !== undefined) patch.physicalSizeY = Number(meta.PhysicalSizeY)
          if (meta.PhysicalSizeZ !== undefined) patch.physicalSizeZ = Number(meta.PhysicalSizeZ)
          if (meta.PhysicalSizeUnit !== undefined) patch.physicalSizeUnit = String(meta.PhysicalSizeUnit)
          if (meta.PhysicalSizeZ_raw !== undefined) patch.physicalSizeZCorrected = true
          if (meta.TimeIncrement !== undefined) patch.timeIncrement = Number(meta.TimeIncrement)
          if (meta.TimeIncrementUnit !== undefined) patch.timeIncrementUnit = String(meta.TimeIncrementUnit)
        }
        useProjectStore().updateImageMeta(imageUid, patch)

        // QC findings are written to disk during the run but NOT carried in task:result, so pull the
        // fresh image meta to surface the QC badge live (no full project reload). See docs/todo/QC_PLAN.md.
        const projectUid = String((data.projectUid as string | undefined) ?? '')
          || useProjectMetaStore().current?.uid || ''
        if (projectUid) {
          fetch(`/api/images/meta?projectUid=${projectUid}&imageUid=${imageUid}`)
            .then(r => (r.ok ? r.json() : null))
            .then(d => {
              if (d?.image?.qc !== undefined) useProjectStore().updateImageMeta(imageUid, { qc: d.image.qc })
              if (d?.image?.runLog !== undefined) useProjectStore().updateImageMeta(imageUid, { runLog: d.image.runLog })
            })
            .catch(() => {})
        }
      }
    }

    if (type) {
      handlers.get(type)?.forEach(h => h(data))
    }
  }

  // ── Outcome backstop ────────────────────────────────────────────────────────
  // Asks the backend how this tab's in-flight tasks ended and re-emits the frame the socket didn't
  // deliver. Idle-quiet: no request while nothing is in flight here. Only acts on an outcome the server
  // can NAME — a task that vanished with no recorded outcome (e.g. the backend restarted under us) is
  // left alone rather than guessed at, so this never invents a completion.
  async function pollOutcomes() {
    if (!useTaskStore().running().length) return          // idle-quiet: nothing of ours to reconcile
    const rows = await fetchRecentOutcomes(outcomeSince)
    outcomeSince = newestFinishedAt(rows, outcomeSince)
    // Re-read the in-flight set AFTER the await, never before it. The real frame may have landed while
    // the request was open (or an overlapping poll may have already recovered the task), and matching
    // against a pre-fetch list would then re-announce a task that is already finished — running the
    // completion side effects a second time, which is exactly what `recovered` exists to prevent.
    for (const frame of recoveredTaskFrames(useTaskStore().running(), rows)) {
      recovered.add(String(frame.recoveredFrom))
      useLogStore().info(
        `Recovered task outcome from the backend (${frame.status}) — the live update was dropped`,
        { source: 'ws', detail: JSON.stringify(frame) },
      )
      dispatch(frame)
    }
  }

  function startOutcomePoll() {
    if (outcomePollTimer) return
    outcomePollTimer = setInterval(() => { void pollOutcomes() }, OUTCOME_POLL_MS)
  }

  function stopOutcomePoll() {
    if (outcomePollTimer) { clearInterval(outcomePollTimer); outcomePollTimer = null }
  }

  function scheduleReconnect() {
    reconnectTimer = setTimeout(connect, 3000)
  }

  function send(msg: Record<string, unknown>) {
    if (socket?.readyState === WebSocket.OPEN) {
      socket.send(JSON.stringify(msg))
    } else {
      useLogStore().warn(
        'Cannot send message — not connected to Julia backend',
        { source: 'ws', detail: JSON.stringify(msg) }
      )
    }
  }

  function ping() {
    send({ type: 'ping' })
  }

  function on(type: string, handler: MessageHandler) {
    if (!handlers.has(type)) handlers.set(type, [])
    handlers.get(type)!.push(handler)
  }

  function off(type: string, handler: MessageHandler) {
    const list = handlers.get(type)
    if (list) handlers.set(type, list.filter(h => h !== handler))
  }

  return { status, lastPong, connect, send, ping, on, off }
})
