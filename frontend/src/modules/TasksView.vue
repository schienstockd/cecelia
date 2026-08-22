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
-->
<script setup lang="ts">
import { onMounted, onUnmounted } from 'vue'
import { useRoute } from 'vue-router'
import TasksModule from './TasksModule.vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { readOpenProject, openProjectFromStorageEvent, onOpenProjectChange } from '../lib/openProjectChannel'

const route = useRoute()
const projectMeta = useProjectMetaStore()

/** Move this window onto `uid` (`''` = the app closed its project). No-op when already there. */
async function show(uid: string) {
  if (uid === (projectMeta.current?.uid ?? '')) return
  if (uid) await projectMeta.openProject(uid)
  else projectMeta.closeProject()
}

let stop: (() => void) | undefined

onMounted(async () => {
  document.title = 'Cecelia — Task Manager'
  // Subscribed BEFORE the first load, so a switch made while this window is still opening its project
  // is not missed — `show` is idempotent, so the two can't fight.
  stop = onOpenProjectChange(e => {
    const next = openProjectFromStorageEvent(e, projectMeta.current?.uid)
    if (next !== null) void show(next)
  })
  // `ws.adoptInFlight()` re-runs whenever a project appears, so the rows already running arrive
  // without anything here waiting for the socket.
  await show(readOpenProject() || String(route.query.project ?? ''))
})

onUnmounted(() => stop?.())
</script>

<template>
  <div class="tasks-window cc-dark">
    <TasksModule standalone />
  </div>
</template>

<style scoped>
.tasks-window { height: 100vh; width: 100vw; overflow: hidden; background: var(--cc-bg); }
</style>
