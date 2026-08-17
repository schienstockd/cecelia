import { defineStore } from 'pinia'
import { computed, ref } from 'vue'
import { useSettingsStore } from './settings'
import type { ViewProfile } from '../utils/viewProfiles'

// View profiles — a curated sidebar (docs/todo/VIEW_PROFILES_PLAN.md).
//
// Two separate things, and keeping them separate is the whole design: the profile DEFINITIONS are
// drop-in files under <config_dir>/profiles/ (this store mirrors /api/profiles), while the ACTIVE
// SELECTION is a per-user setting (`settings.viewProfile`) — a profile says who is driving, not what
// the data is, so it must not travel with a shared project.
//
// The empty id '' is the implicit "All" profile: today's full menu, always the fallback, never a file.
export interface ProfileFileError { file: string; error: string }

export const ALL_PROFILE_ID = ''

export const useViewProfilesStore = defineStore('viewProfiles', () => {
  const settings = useSettingsStore()

  const dir      = ref('')
  const profiles = ref<ViewProfile[]>([])
  const errors   = ref<ProfileFileError[]>([])
  const loading  = ref(false)
  const saving   = ref(false)
  let   loadedOnce = false

  /** The selected profile, or null for "All". A selection naming a deleted file resolves to All. */
  const active = computed<ViewProfile | null>(() =>
    profiles.value.find(p => p.id === settings.viewProfile) ?? null)

  /** The paths the active profile shows, or null for All (= everything). */
  const activeItems = computed<string[] | null>(() => active.value?.items ?? null)

  function apply(data: { dir?: string; profiles?: ViewProfile[]; errors?: ProfileFileError[] }) {
    if (data.dir !== undefined) dir.value = data.dir
    if (data.profiles) profiles.value = data.profiles
    errors.value = data.errors ?? []
  }

  async function refresh() {
    loading.value = true
    try {
      const res = await fetch('/api/profiles')
      if (res.ok) apply(await res.json())
    } catch { /* ignore — leave last-known state; a missing profile just means the full menu */ }
    finally { loading.value = false; loadedOnce = true }
  }

  async function ensureLoaded() {
    if (!loadedOnce && !loading.value) await refresh()
  }

  function select(id: string) {
    settings.viewProfile = id
  }

  /**
   * Create or update one profile. Pass `id` to keep an existing profile's identity while renaming its
   * label — otherwise the id is derived from the label server-side. Returns the stored profile, or an
   * error message for the editor to show inline.
   */
  async function save(label: string, items: string[], id?: string):
    Promise<{ profile?: ViewProfile; error?: string }> {
    saving.value = true
    try {
      const res = await fetch('/api/profiles/save', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ label, items, ...(id ? { id } : {}) }),
      })
      const data = await res.json().catch(() => ({}))
      if (!res.ok) return { error: data?.error || 'Could not save the profile.' }
      await refresh()
      return { profile: data.profile as ViewProfile }
    } catch {
      return { error: 'Could not reach the server.' }
    } finally { saving.value = false }
  }

  async function remove(id: string): Promise<{ ok: boolean; error?: string }> {
    saving.value = true
    try {
      const res = await fetch('/api/profiles/delete', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ id }),
      })
      const data = await res.json().catch(() => ({}))
      if (!res.ok) return { ok: false, error: data?.error || 'Could not delete the profile.' }
      // Deleting the active profile falls back to All rather than leaving a dangling selection.
      if (settings.viewProfile === id) settings.viewProfile = ALL_PROFILE_ID
      await refresh()
      return { ok: true }
    } catch {
      return { ok: false, error: 'Could not reach the server.' }
    } finally { saving.value = false }
  }

  return {
    dir, profiles, errors, loading, saving,
    active, activeItems,
    ensureLoaded, refresh, select, save, remove,
  }
})
