<!--
  Standalone Task Manager window (opened from the Tasks page's pop-out button via
  window.open('#/tasks-window?project=<uid>')). Like the console window it is a bare, full-window
  mount of the SAME TasksModule the /tasks page uses — one task manager, two mount points.

  A popup is a fresh app instance with nothing open (Cecelia has no "reopen last project on load" —
  `/` is a neutral welcome page), and the task list is scoped to the open project: with no project the
  window opens empty, adopts none of the work already in flight, and has no image names to put on the
  rows it does receive. So this window has to be TOLD which project, twice over:

   - at mount, from `lib/openProjectChannel.ts` (what the app has open now) falling back to the
     `?project=` in the URL — the seed written when the window was opened, and the answer on a first
     ever run, before the channel has been written;
   - and thereafter by following switches made in the main window, because a popped-out list quietly
     scoped to the project you just left, still labelled as if it were current, is worse than an empty
     one. Not a toggle: turning OFF the manager's own "This project" is how you watch across projects.

  Following costs a real `openProject` per switch (sets, boards) in this window — the ordinary path,
  not a lighter copy of it. Two windows on one project is a state the app already supports; the
  boards' optimistic-concurrency check exists for exactly it.

  **The loads are serialised, and that is not a nicety.** `openProject` awaits a fetch before it writes
  anything, so two overlapping calls resolve LAST-RESPONDED rather than last-requested: switch A→B
  faster than A's response comes back and this window lands on A while the app is on B, silently and
  permanently (nothing re-checks afterwards). The window is at its most exposed exactly at mount, when
  the first load is already in flight. So requests go through `utils/debouncedLatest.ts` — the shared
  scheduler for this, which collapses a burst, and (the rule that matters here) queues a request that
  arrives during a run instead of racing it. Last requested wins.
-->
<script setup lang="ts">
import { onMounted, onUnmounted, watch } from 'vue'
import { useRoute } from 'vue-router'
import TasksModule from './TasksModule.vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useLogStore } from '../stores/log'
import { readOpenProject, openProjectFromStorageEvent, onOpenProjectChange } from '../lib/openProjectChannel'
import { debouncedLatest } from '../utils/debouncedLatest'

const route = useRoute()
const projectMeta = useProjectMetaStore()

/**
 * Move this window onto `uid` (`''` = the app closed its project). Serialised and latest-wins — see
 * the header. The short `wait` also collapses flipping through three projects into one load; it is
 * invisible beside the load itself.
 */
const show = debouncedLatest<string>(async (uid) => {
  if (uid === (projectMeta.current?.uid ?? '')) return
  if (uid) await projectMeta.openProject(uid)
  else projectMeta.closeProject()
}, {
  wait: 100,
  // openProject already reports its own failures; this is for anything the scheduler catches that it
  // did not, which would otherwise be an unhandled rejection out of a timer.
  onError: e => useLogStore().error(
    `Could not follow the project switch: ${e instanceof Error ? e.message : String(e)}`,
    { source: 'project' }),
})

let stop: (() => void) | undefined

// The window's own title names the project, and re-titles when the window follows a switch. It is the
// only always-on signal that following happened at all: this is a bare route, so the docked console
// that would otherwise carry a log line is not mounted here, and the OS window title is visible without
// opening anything.
watch(() => projectMeta.current?.name, name => {
  document.title = `Cecelia — Task Manager${name ? ` — ${name}` : ''}`
}, { immediate: true })

onMounted(() => {
  // Subscribed BEFORE the first load, so a switch made while this window is still opening its project
  // is not missed — the scheduler queues it behind that load rather than racing it.
  stop = onOpenProjectChange(e => {
    const next = openProjectFromStorageEvent(e, projectMeta.current?.uid)
    if (next !== null) show.schedule(next)
  })
  // `ws.adoptInFlight()` re-runs whenever a project appears, so the rows already running arrive
  // without anything here waiting for the socket.
  show.schedule(readOpenProject() || String(route.query.project ?? ''))
})

// Drop a queued switch AND supersede one in flight: this window is going away.
onUnmounted(() => { stop?.(); show.cancel() })
</script>

<template>
  <div class="tasks-window cc-dark">
    <TasksModule standalone />
  </div>
</template>

<style scoped>
.tasks-window { height: 100vh; width: 100vw; overflow: hidden; background: var(--cc-bg); }
</style>
