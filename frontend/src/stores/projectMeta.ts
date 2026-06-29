import { defineStore } from 'pinia'
import { ref, computed } from 'vue'
import { useLogStore } from './log'
import { useProjectStore, type CciaSet } from './project'

export type ProjectType = 'static' | 'live' | 'flow'

export interface ProjectRecord {
  uid: string
  name: string
  type: ProjectType
  path: string
  createdAt: string
  lastOpenedAt: string | null
}

export const useProjectMetaStore = defineStore('projectMeta', () => {
  const log = useLogStore()
  const current = ref<ProjectRecord | null>(null)
  const recent = ref<ProjectRecord[]>([])
  const projectsDir = ref<string>('')
  const loading = ref(false)

  const hasProject = computed(() => current.value !== null)

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

  async function createProject(name: string, type: ProjectType): Promise<boolean> {
    loading.value = true
    try {
      const res = await fetch('/api/projects/create', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ name, type }),
      })
      const body = await res.json().catch(() => ({})) as { project?: ProjectRecord; error?: string }
      if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
      current.value = body.project!
      await fetchRecent()
      log.info(`Created project "${name}".`, { source: 'project' })
      return true
    } catch (e) {
      log.error(`Failed to create project: ${e instanceof Error ? e.message : String(e)}`, { source: 'project' })
      return false
    } finally {
      loading.value = false
    }
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
        error?: string
      }
      if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
      current.value = body.project!
      projectStore.loadFromApi(body.sets ?? [])
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

  async function saveProject(): Promise<void> {
    if (!current.value) return
    try {
      const res = await fetch('/api/projects/save', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ uid: current.value.uid }),
      })
      const body = await res.json().catch(() => ({})) as { error?: string }
      if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
      log.info(`Project "${current.value.name}" saved.`, { source: 'project' })
    } catch (e) {
      log.error(`Failed to save project: ${e instanceof Error ? e.message : String(e)}`, { source: 'project' })
    }
  }

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

  return { current, recent, projectsDir, loading, hasProject, fetchRecent, createProject, openProject, saveProject, renameProject, closeProject }
})
