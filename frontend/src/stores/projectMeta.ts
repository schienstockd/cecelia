import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref, computed, watch } from 'vue'
import { useLogStore } from './log'
import { useProjectStore, type CciaSet } from './project'
import { useAnalysisTabsStore } from './analysisTabs'
import { useAnalysisLayoutStore } from './analysisLayout'
import { useCanvasPanelsStore } from './canvasPanels'
import { useAnimationStore } from './animation'
import { tabGroupOf, type BoardsDoc } from '../utils/boardDoc'
import { publishOpenProject } from '../lib/openProjectChannel'

export interface ProjectRecord {
  uid: string
  name: string
  path: string
  createdAt: string
  lastOpenedAt: string | null
  // USER_PROFILE_PLAN Phase 5 — profile names authorised to see this project. Empty/missing =
  // visible to every profile (Decision 8, safe default for pre-identity projects). Presence in
  // the list is the grant; there is no owner-vs-collaborator distinction (Decision 5).
  owners?: string[]
}

export const useProjectMetaStore = defineStore('projectMeta', () => {
  const log = useLogStore()
  const current = ref<ProjectRecord | null>(null)
  const recent = ref<ProjectRecord[]>([])
  const projectsDir = ref<string>('')
  const loading = ref(false)

  const hasProject = computed(() => current.value !== null)

  // Tell the app's OTHER windows which project is open, so a pop-out can follow a switch made here
  // (`lib/openProjectChannel.ts`). One watch rather than a call at each of the four sites that move
  // `current`, since the invariant is about the value, not about who set it.
  //
  // NOT `immediate`, and that is the whole correctness of it: every window boots with `current` null,
  // so publishing on boot would have each new window announce "no project open" — a reload of the main
  // window would drag an open pop-out to nothing, and opening a pop-out would erase the value the next
  // one reads at mount. Only a real CHANGE is news.
  watch(current, p => publishOpenProject(p?.uid ?? ''))

  async function fetchRecent() {
    try {
      const res = await fetch('/api/projects')
      if (!res.ok) throw new Error(`HTTP ${res.status}`)
      const data = await res.json() as { projects: ProjectRecord[]; projectsDir: string }
      recent.value = data.projects ?? []
      projectsDir.value = data.projectsDir ?? ''
    } catch (e) {
      log.error(`Failed to load project list: ${e instanceof Error ? e.message : String(e)}`, { source: 'project' })
    }
  }

  async function createProject(name: string): Promise<boolean> {
    loading.value = true
    let newUid = ''
    try {
      const res = await fetch('/api/projects/create', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ name }),
      })
      const body = await res.json().catch(() => ({})) as { project?: ProjectRecord; error?: string }
      if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
      newUid = body.project!.uid
      log.info(`Created project "${name}".`, { source: 'project' })
    } catch (e) {
      log.error(`Failed to create project: ${e instanceof Error ? e.message : String(e)}`, { source: 'project' })
      return false
    } finally {
      loading.value = false
    }
    // Switch to it through the canonical open path so ALL per-project state resets (sets/images, and
    // the analysis boards / module canvases / animations). Previously createProject only set `current`
    // and never loaded the new project's sets, so the image table kept showing the PREVIOUS project's
    // images (and its boards/animations) until a browser reload.
    return openProject(newUid)
  }

  async function openProject(uid: string): Promise<boolean> {
    loading.value = true
    const projectStore = useProjectStore()
    try {
      const res = await fetch('/api/projects/load', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ uid }),
      })
      const body = await res.json().catch(() => ({})) as {
        project?: ProjectRecord
        sets?: CciaSet[]
        boards?: Partial<BoardsDoc> | null
        moduleCanvases?: { entries?: Record<string, unknown>; geom?: Record<string, unknown> } | null
        animations?: { snapshots?: unknown[] } | null
        error?: string
      }
      if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
      // Sets FIRST, then `current`. They are written in the same tick either way, but the order is
      // what a reader sees if anything between them ever throws: with `current` first, the app names
      // the new project while the store still holds the old one's images — which is exactly the state
      // that was reported. `loadedProjectUid` makes the pairing checkable rather than assumed.
      projectStore.loadFromApi(body.sets ?? [], body.project!.uid)   // NB: clears the canvas/analysis stores — restore AFTER
      current.value = body.project!
      // rehydrate the Analysis-canvas boards saved with the project (analysisBoards.json)
      if (body.boards) {
        const groupKey = `analysis:${body.project!.uid}`
        // The server normalises the document (both historical shapes) and stamps `version` — the
        // optimistic-concurrency token the autosave must echo back. See utils/boardDoc.ts.
        useAnalysisTabsStore().load(groupKey, tabGroupOf(body.boards) as never)
        useAnalysisLayoutStore().load(groupKey, body.boards.layouts as never)
        useAnalysisLayoutStore().setVersion(body.project!.uid, body.boards.version ?? 0)
      }
      // rehydrate per-image module-page canvases (moduleCanvases.json)
      if (body.moduleCanvases) useCanvasPanelsStore().load(body.moduleCanvases as never)
      // rehydrate the Animation page's captured view snapshots (animations.json); always call so a
      // project with none clears any leftover from the previously-open project
      useAnimationStore().load(body.animations as never)
      await fetchRecent()
      const nSets   = body.sets?.length ?? 0
      const nImages = body.sets?.reduce((n, s) => n + s.images.length, 0) ?? 0
      log.info(
        `Opened "${body.project!.name}" — ${nSets} set${nSets !== 1 ? 's' : ''}, ${nImages} image${nImages !== 1 ? 's' : ''}.`,
        { source: 'project' }
      )
      return true
    } catch (e) {
      log.error(`Failed to open project: ${e instanceof Error ? e.message : String(e)}`, { source: 'project' })
      return false
    } finally {
      loading.value = false
    }
  }

  // NOTE: there is no manual "save project" anymore. The /analysis boards autosave (debounced) from the
  // analysisLayout store; everything else (image metadata, inclusion/notes, attrs, gating pops, chains,
  // module canvases, viewer layer props) already persists immediately via its own routes; and
  // `lastOpenedAt` is stamped on open (api_projects_load). See docs/todo/ANIMATION_PLAN.md.

  async function renameProject(name: string): Promise<boolean> {
    if (!current.value) return false
    const trimmed = name.trim()
    if (!trimmed) return false
    try {
      const res = await fetch('/api/projects/rename', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ uid: current.value.uid, name: trimmed }),
      })
      const body = await res.json().catch(() => ({})) as { error?: string }
      if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
      current.value = { ...current.value, name: trimmed }
      log.info(`Project renamed to "${trimmed}".`, { source: 'project' })
      return true
    } catch (e) {
      log.error(`Failed to rename project: ${e instanceof Error ? e.message : String(e)}`, { source: 'project' })
      return false
    }
  }

  function closeProject() {
    const projectStore = useProjectStore()
    current.value = null
    projectStore.clear()
    log.info('Project closed.', { source: 'project' })
  }

  // Permanently delete a project's directory from disk, then refresh the recent list (which is a scan
  // of projects_dir). Guarded by the caller against deleting the currently-open project. Irreversible.
  async function deleteProject(uid: string): Promise<boolean> {
    try {
      const res = await fetch('/api/projects/delete', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ uid }),
      })
      const body = await res.json().catch(() => ({})) as { error?: string }
      if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
      await fetchRecent()
      log.info('Project deleted.', { source: 'project' })
      return true
    } catch (e) {
      log.error(`Failed to delete project: ${e instanceof Error ? e.message : String(e)}`, { source: 'project' })
      return false
    }
  }

  // Claim / unclaim: add or remove the active profile from a project's `owners` list. Both are
  // idempotent server-side. The local `recent` list is updated in place so the UI reacts without
  // another round-trip. See USER_PROFILE_PLAN.md Phase 5.
  async function claimProject(uid: string): Promise<boolean> {
    return await _writeOwners(uid, '/api/projects/claim')
  }
  async function unclaimProject(uid: string): Promise<boolean> {
    return await _writeOwners(uid, '/api/projects/unclaim')
  }
  async function _writeOwners(uid: string, url: string): Promise<boolean> {
    try {
      const res = await fetch(url, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ uid }),
      })
      const body = await res.json().catch(() => ({})) as { ok?: boolean; owners?: string[]; error?: string }
      if (!res.ok || !body.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
      const owners = body.owners ?? []
      recent.value = recent.value.map(p => p.uid === uid ? { ...p, owners } : p)
      if (current.value?.uid === uid) current.value = { ...current.value, owners }
      return true
    } catch (e) {
      log.error(`Ownership update failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'project' })
      return false
    }
  }

  return { current, recent, projectsDir, loading, hasProject, fetchRecent, createProject, openProject, renameProject, deleteProject, closeProject, claimProject, unclaimProject }
})

// Replace the live instance on hot-reload — see the note in `stores/customModules.ts`.
if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useProjectMetaStore, import.meta.hot))
