<script setup lang="ts">
// PreferencesModal — the active profile's per-user preferences (USER_PROFILE_PLAN Phase 4).
// Two-pane layout (Firefox / Thunderbird / GNOME Settings pattern): category list left, the
// selected category's fields right. Every field two-way-binds to a ref on `useSettingsStore`;
// the store's own PATCH debouncer coalesces changes into one `/api/profile/settings/patch`
// round-trip per burst, so this component has no save button — writes are automatic.
//
// This modal edits the *active* profile's settings only; it is NOT a shared machine panel.
// Machine-scoped settings (VRAM budget, image compressor, storage layout, plugin registry,
// port toggles) stay on SettingsModule.vue (soon rebranded "System settings" as Phase 4
// tidies up).
import { ref, computed, onMounted } from 'vue'
import { useSettingsStore } from '../stores/settings'
import { useAppControlStore } from '../stores/appControl'
import BaseModal from './BaseModal.vue'
import CcToggle from './CcToggle.vue'
import ConfirmButton from './ConfirmButton.vue'
import ConfirmDeleteButton from './ConfirmDeleteButton.vue'
import CreateProfileDialog from './profile/CreateProfileDialog.vue'
import { useCopyFlash } from '../composables/useCopyFlash'
import { fetchProfiles, selectProfile, retireProfile, renameProfile, deleteProfile,
         fetchTerminalCommand, isValidProfileName,
         type ProfileRoster } from '../utils/profileApi'

defineEmits<{ (e: 'close'): void }>()

const settings = useSettingsStore()
const appCtl   = useAppControlStore()

// Profiles is FIRST — it's the identity primitive every other pane's setting hangs off, and the
// answer to "how do I switch" needs to be one click away. The other panes follow roughly by how
// often they get touched.
type CategoryId = 'profiles' | 'interface' | 'viewer' | 'movies' | 'layout' | 'kiwi' | 'lablog'
const CATEGORIES: { id: CategoryId; label: string; icon: string }[] = [
  { id: 'profiles',  label: 'Profiles',  icon: 'pi-user' },
  { id: 'interface', label: 'Interface', icon: 'pi-sliders-h' },
  { id: 'viewer',    label: 'Viewer',    icon: 'pi-eye' },
  { id: 'movies',    label: 'Movies',    icon: 'pi-video' },
  { id: 'layout',    label: 'Layout',    icon: 'pi-window-maximize' },
  { id: 'kiwi',      label: 'Kiwi',      icon: 'pi-comments' },
  { id: 'lablog',    label: 'Lab log',   icon: 'pi-book' },
]
const active = ref<CategoryId>('profiles')

// ── Profiles pane state ──────────────────────────────────────────────────────
// Roster is server truth (custom.toml [ai].profile + kiwi-profiles subdirectories). Local state is
// a cache so the pane renders while the round-trip runs. Failure → `default`-only fallback via
// `fetchProfiles`, which keeps the pane usable rather than blanking it out.
const roster = ref<ProfileRoster>({
  active: 'default',
  profiles: [{ name: 'default', dir: '', isDefault: true, retired: false }],
  legacyReserved: ['legacy'],
})
const profilesBusy = ref<string>('')      // name currently being selected/retired — drives per-row spinner
const profilesError = ref<string | null>(null)
const showCreateProfile = ref(false)
const { isCopied: termCopied, copy: copyTerm } = useCopyFlash()

async function refreshRoster() {
  try {
    roster.value = await fetchProfiles()
  } catch (e) {
    profilesError.value = e instanceof Error ? e.message : String(e)
  }
}
onMounted(() => { void refreshRoster() })

// Switching the profile mid-session works but half the app is already mounted against the
// previous identity (per-profile settings, project filter, MCP registration). The plan is
// explicit: change-of-profile requires reload. Server-side write goes out first so a reload
// picks up the new active profile.
async function pickProfile(name: string) {
  if (name === roster.value.active || profilesBusy.value) return
  profilesBusy.value = name
  profilesError.value = null
  try {
    const r = await selectProfile(name)
    if (!r.ok) { profilesError.value = r.error ?? 'Select failed'; return }
    const nowActive = r.active ?? name
    roster.value = { ...roster.value, active: nowActive }
    // Skip the launch picker on the imminent reload — the user JUST answered "who's driving"
    // by clicking Switch here. Without this the reload re-fires the picker with the just-picked
    // profile pre-highlighted, forcing a second click for the same decision.
    appCtl.markProfileJustPicked(nowActive)
    // Give the toast pattern a beat, then reload — every store that hydrated under the previous
    // profile needs to re-read (settings.toml, project owners, Kiwi credential dir). Simpler and
    // more honest than trying to invalidate 20+ store paths in place.
    setTimeout(() => window.location.reload(), 250)
  } catch (e) {
    profilesError.value = e instanceof Error ? e.message : String(e)
  } finally { profilesBusy.value = '' }
}

// Inline rename — the name span becomes an <input> when `renamingName` matches. Enter commits,
// Escape cancels. Client-side validation is a fast-fail; the server re-validates and can still
// reject (collision, stale roster).
const renamingName = ref<string>('')
const renameDraft  = ref<string>('')
const renameDraftValid = computed(() =>
  isValidProfileName(renameDraft.value.trim()) && renameDraft.value.trim() !== renamingName.value)
function startRename(name: string) {
  renamingName.value = name
  renameDraft.value  = name
}
function cancelRename() {
  renamingName.value = ''
  renameDraft.value  = ''
}
async function commitRename() {
  const oldName = renamingName.value
  const newName = renameDraft.value.trim()
  if (!renameDraftValid.value || profilesBusy.value) return
  profilesBusy.value = oldName
  profilesError.value = null
  try {
    const r = await renameProfile(oldName, newName)
    if (!r.ok) { profilesError.value = r.error ?? 'Rename failed'; return }
    cancelRename()
    await refreshRoster()
    // Reload only when the RENAMED profile was active — otherwise this pane keeps working.
    if (r.active === newName) {
      appCtl.markProfileJustPicked(newName)   // suppress picker on reload — user didn't change identity, they renamed it
      setTimeout(() => window.location.reload(), 250)
    }
  } finally { profilesBusy.value = '' }
}

async function deleteProfileByName(name: string) {
  if (profilesBusy.value) return
  profilesBusy.value = name
  profilesError.value = null
  try {
    const r = await deleteProfile(name)
    if (!r.ok) { profilesError.value = r.error ?? 'Delete failed'; return }
    await refreshRoster()
  } finally { profilesBusy.value = '' }
}

async function retireActive() {
  const cur = roster.value.profiles.find(p => p.name === roster.value.active)
  if (!cur || cur.isDefault || cur.retired || profilesBusy.value) return
  profilesBusy.value = cur.name
  profilesError.value = null
  try {
    const r = await retireProfile(cur.name)
    if (!r.ok) { profilesError.value = r.error ?? 'Retire failed'; return }
    await refreshRoster()
    // Server snapped active back to `default` when the active profile was retired — mark that
    // decision as "just picked" so the reload doesn't drop the user into the launch picker
    // (which would then reject the retired profile it's still preselected on).
    appCtl.markProfileJustPicked('default')
    setTimeout(() => window.location.reload(), 250)
  } finally { profilesBusy.value = '' }
}

function onProfileCreated(_name: string) {
  showCreateProfile.value = false
  // Create+select already happened server-side inside the dialog. Reload to hydrate every store
  // under the new profile.
  setTimeout(() => window.location.reload(), 250)
}

// The "Copy login command" one-liner (LOGIN_CREDENTIAL_ISOLATION_PLAN P6). Fetched fresh per
// click — the string is short and having the server generate it means the flag list stays in
// one place (`kiwi_terminal_command` in `agent_runner.jl`). The footer form fetches for the
// ACTIVE profile; the per-row form takes an explicit profile name so a user can set up
// credentials for `alice` without switching to alice first.
const termFetching = ref<string>('')      // name currently fetching — drives the row spinner
async function copyTerminalCommand(profile?: string) {
  const key = profile ?? '__active__'
  if (termFetching.value) return
  termFetching.value = key
  try {
    const r = await fetchTerminalCommand(profile)
    if (r?.command) await copyTerm(r.command)
  } finally { termFetching.value = '' }
}

// No loading hint: values are always usable — hydration overrides localStorage in place if the
// TOML has anything to say (Vue reactivity picks up the flip). The hint used to spin indefinitely
// when the backend hadn't been restarted to register /api/profile/settings; a stale endpoint is
// now bounded by fetchProfileSettings' 5s AbortSignal timeout, not by user perception.
</script>

<template>
  <BaseModal title="Preferences" icon="pi-user" width="720px" height="560px" @close="$emit('close')">
    <div class="pref-cols">
      <!-- Left: category list. Keeps focus discipline (arrow keys, click). -->
      <nav class="pref-nav" aria-label="Preference categories">
        <button v-for="c in CATEGORIES" :key="c.id"
                class="pref-nav-btn"
                :class="{ 'pref-nav-btn-active': active === c.id }"
                @click="active = c.id">
          <i :class="['pi', c.icon]" />
          <span>{{ c.label }}</span>
        </button>
      </nav>

      <!-- Right: the selected category's fields. Every control writes straight to the store. -->
      <section class="pref-pane">

        <!-- ── Profiles ──────────────────────────────────────────────────── -->
        <template v-if="active === 'profiles'">
          <p class="pref-note cc-muted cc-fs-xs">
            Your identity for this session — drives Kiwi credentials, per-profile preferences and
            which projects you see. Switching reloads the app.
          </p>
          <ul class="pp-list" data-guide="prefs.profilesPane">
            <li v-for="p in roster.profiles" :key="p.name" class="pp-row"
                :class="{ 'pp-row-active': p.name === roster.active,
                          'pp-row-retired': p.retired }">
              <!-- Inline rename: the name span becomes an <input> when this row is being renamed.
                   Enter commits, Escape cancels. Client-side validity mirrors the server rule
                   (`isValidProfileName`) so the save button gates before the round-trip. -->
              <template v-if="renamingName === p.name">
                <span class="pp-name pp-name-edit">
                  <i class="pi pi-pencil" />
                  <input v-model="renameDraft" class="pp-rename-input"
                         v-tooltip.bottom="'1-32 chars, lower-ASCII alnum + `-` / `_`'"
                         @keyup.enter="commitRename"
                         @keyup.escape="cancelRename" />
                </span>
                <span class="cc-row cc-row-tight">
                  <button class="cc-btn cc-btn-primary cc-fs-xs"
                          :disabled="!renameDraftValid || !!profilesBusy"
                          @click="commitRename"
                          v-tooltip.bottom="'Save the new name'">
                    <i class="pi pi-check" /> Save
                  </button>
                  <button class="cc-btn cc-btn-ghost cc-fs-xs" @click="cancelRename"
                          v-tooltip.bottom="'Cancel — keep the old name'">
                    <i class="pi pi-times" />
                  </button>
                </span>
              </template>
              <template v-else>
                <span class="pp-name">
                  <i class="pi" :class="p.retired ? 'pi-user-minus' : 'pi-user'" />
                  {{ p.name }}
                  <span v-if="p.isDefault" class="pp-tag cc-muted cc-fs-2xs">(~/.claude)</span>
                  <span v-if="p.retired" class="pp-tag cc-muted cc-fs-2xs">(retired)</span>
                  <span v-if="p.name === roster.active" class="pp-tag pp-tag-active cc-fs-2xs">active</span>
                </span>
                <span class="cc-row cc-row-tight">
                  <!-- Switch: any non-active, non-retired profile — INCLUDING `default`. Excluding
                       default was the bug that stranded users who wanted to go back to `~/.claude`. -->
                  <button v-if="p.name !== roster.active && !p.retired"
                          class="cc-btn cc-btn-ghost cc-fs-xs"
                          :disabled="!!profilesBusy"
                          @click="pickProfile(p.name)"
                          v-tooltip.bottom="`Switch to ${p.name} — the app will reload`">
                    <i :class="['pi', profilesBusy === p.name ? 'pi-spin pi-spinner' : 'pi-arrow-right']" />
                    Switch
                  </button>
                  <!-- Per-row login: copies a terminal one-liner scoped to THIS profile's
                       CLAUDE_CONFIG_DIR — so a user can set up credentials for `alice` without
                       first switching to alice. Not offered for `default` (uses ~/.claude,
                       already covered by any `claude login` outside Cecelia) or retired profiles
                       (non-selectable = login pointless). -->
                  <button v-if="!p.isDefault && !p.retired"
                          class="cc-btn cc-btn-ghost cc-btn-icon cc-fs-xs"
                          :disabled="!!termFetching"
                          @click="copyTerminalCommand(p.name)"
                          v-tooltip.bottom="termCopied()
                            ? `Copied — paste in a terminal to log ${p.name} in`
                            : `Copy a login command for ${p.name} — paste in a terminal, then run /login inside claude`">
                    <i :class="['pi', termFetching === p.name ? 'pi-spin pi-spinner' : 'pi-sign-in']" />
                  </button>
                  <!-- Rename: not offered for `default` (magic name) or retired profiles (the
                       retired marker is a stable reference — renaming would silently invalidate
                       both semantics). See USER_PROFILE_PLAN Decision 11. -->
                  <button v-if="!p.isDefault && !p.retired"
                          class="cc-btn cc-btn-ghost cc-btn-icon cc-fs-xs"
                          :disabled="!!profilesBusy"
                          @click="startRename(p.name)"
                          v-tooltip.bottom="'Rename — past turn logs still reference the old name'">
                    <i class="pi pi-pencil" />
                  </button>
                  <!-- Delete: not offered for `default` or the active profile (server enforces
                       both; the button just doesn't render). Canonical ConfirmDeleteButton. -->
                  <ConfirmDeleteButton v-if="!p.isDefault && p.name !== roster.active"
                                       :disabled="!!profilesBusy"
                                       title="Delete this profile — credentials + settings + turn provenance gone"
                                       armed-title="Click again to permanently delete this profile"
                                       @confirm="deleteProfileByName(p.name)" />
                </span>
              </template>
            </li>
          </ul>

          <p v-if="profilesError" class="pref-error cc-fs-md">
            <i class="pi pi-exclamation-circle" /> {{ profilesError }}
          </p>

          <div class="cc-row cc-row-tight" style="flex-wrap:wrap; margin-top:0.6rem;">
            <button class="cc-btn cc-btn-ghost cc-fs-xs" data-guide="prefs.newProfile"
                    @click="showCreateProfile = true"
                    v-tooltip.top="'A separate credential + MCP scope for this seat login'">
              <i class="pi pi-user-plus" /> New profile
            </button>
            <button class="cc-btn cc-btn-ghost cc-fs-xs" data-guide="prefs.copyLogin"
                    :disabled="!!termFetching"
                    @click="copyTerminalCommand()"
                    v-tooltip.top="termCopied()
                      ? `Copied — paste in a terminal to log ${roster.active} in`
                      : `Copy a login one-liner for ${roster.active} — the active profile`">
              <i :class="['pi', termFetching === '__active__' ? 'pi-spin pi-spinner'
                                : termCopied() ? 'pi-check' : 'pi-sign-in']" />
              {{ termCopied() ? 'Copied' : 'Copy login command' }}
            </button>
            <!-- Retire (LOGIN_CREDENTIAL_ISOLATION_PLAN D11). Only offered when the active
                 profile is a named, non-retired one — retiring `default` is meaningless. -->
            <ConfirmButton v-if="roster.active !== 'default' &&
                                 !roster.profiles.find(p => p.name === roster.active)?.retired"
                           @confirm="retireActive" v-slot="{ armed, arm, confirm, cancel }">
              <button v-if="!armed" class="cc-btn cc-btn-ghost cc-fs-xs"
                      :disabled="!!profilesBusy" @click="arm"
                      v-tooltip.top="'Retire this profile — non-selectable after, data stays on disk'">
                <i class="pi pi-user-minus" /> Retire active
              </button>
              <template v-else>
                <button class="cc-btn cc-btn-danger cc-fs-xs" @click="confirm"
                        v-tooltip.top="'Confirm retire'">
                  <i class="pi pi-check" /> Retire {{ roster.active }}
                </button>
                <button class="cc-btn cc-btn-ghost cc-fs-xs" @click="cancel">Cancel</button>
              </template>
            </ConfirmButton>
          </div>
        </template>

        <!-- ── Interface ─────────────────────────────────────────────────── -->
        <!-- Grouped by WHAT THE TOGGLE IS ABOUT, not alphabetically — 10 unrelated switches read
             as noise. Subheadings match the primitive on the page they affect (Task Manager,
             the moment tasks finish, the Import dialog, the update checker, first launch). Two
             items relocated to the panes where the affected surface lives:
             `animationSyncViewer` → Movies (animation is the Movies page's sibling),
             `viewerAutoSaveLayerProps` → Viewer (formerly Overlays). -->
        <template v-if="active === 'interface'">
          <div class="pref-group cc-eyebrow cc-fs-2xs">Task Manager</div>
          <CcToggle v-model="settings.taskListAutoFollow"
                    label="Auto-follow running tasks"
                    v-tooltip.bottom="'When a task starts, select it in the manager log panel'" />
          <CcToggle v-model="settings.tasksThisProjectOnly"
                    label="Show only the open project"
                    v-tooltip.bottom="'Off: also shows tasks from other projects'" />
          <CcToggle v-model="settings.tasksShowHistory"
                    label="Also show durable run history"
                    v-tooltip.bottom="'On: previous runs from the log, not just this session'" />

          <div class="pref-group cc-eyebrow cc-fs-2xs" style="margin-top:0.8rem">When tasks finish</div>
          <CcToggle v-model="settings.autoRefreshOnTask"
                    label="Refresh plots"
                    v-tooltip.bottom="'Reload plots automatically when a task succeeds'" />
          <CcToggle v-model="settings.viewerAutoUpdate"
                    label="Refresh the viewer"
                    v-tooltip.bottom="'Heavy on large images — default off'" />

          <div class="pref-group cc-eyebrow cc-fs-2xs" style="margin-top:0.8rem">Import</div>
          <CcToggle v-model="settings.importPyramidAdvisor"
                    label="Suggest pyramid levels"
                    v-tooltip.bottom="'Peek source dims to advise a level count'" />

          <div class="pref-group cc-eyebrow cc-fs-2xs" style="margin-top:0.8rem">Software updates</div>
          <CcToggle v-model="settings.preferDevChannel"
                    label="Track main (dev builds)"
                    v-tooltip.bottom="'Applying the update still needs Node.js on the machine'" />

          <div class="pref-group cc-eyebrow cc-fs-2xs" style="margin-top:0.8rem">Launch</div>
          <CcToggle v-model="settings.tipsOnLaunch"
                    label="Show tip of the day"
                    v-tooltip.bottom="'Prepends the daily tip to the What is New modal once per day'" />
        </template>

        <!-- ── Viewer (behaviour + on-image chrome + sizes) ─────────────── -->
        <template v-else-if="active === 'viewer'">
          <div class="pref-group cc-eyebrow cc-fs-2xs">Behaviour</div>
          <CcToggle v-model="settings.viewerAutoSaveLayerProps"
                    label="Autosave viewer layer props per image"
                    v-tooltip.bottom="'Contrast, colormap, T/Z snapshot every image change'" />

          <div class="pref-group cc-eyebrow cc-fs-2xs" style="margin-top:0.8rem">On-image chrome</div>
          <CcToggle v-model="settings.viewerScaleBar" label="Show scale bar"
                    v-tooltip.bottom="'Draw the calibrated scale bar over every frame'" />
          <CcToggle v-model="settings.viewerTimestamp" label="Show elapsed timestamp"
                    v-tooltip.bottom="'Draw elapsed time on time-lapse frames'" />
          <CcToggle v-model="settings.viewerGrid" label="Show grid (SoM overlay)"
                    v-tooltip.bottom="'Set-of-Mark reference grid for pointing at regions'" />
          <CcToggle v-model="settings.viewerLandscape" label="Show landscape overlay"
                    v-tooltip.bottom="'Tile-level semantic heatmap over the frame'" />
          <CcToggle v-model="settings.viewerLandscapeLabels" label="Show landscape debug labels"
                    v-tooltip.bottom="'Print the k-means category on each landscape tile'" />

          <div class="pref-group cc-eyebrow cc-fs-2xs" style="margin-top:0.6rem">Sizes (px)</div>
          <label class="pref-num">Scale bar text
            <input type="number" min="8" max="60" v-model.number="settings.viewerScaleBarPx"
                   v-tooltip.bottom="'Overlay text size in screen pixels'" /></label>
          <label class="pref-num">Timestamp text
            <input type="number" min="8" max="60" v-model.number="settings.viewerTimestampPx"
                   v-tooltip.bottom="'Overlay text size in screen pixels'" /></label>
          <label class="pref-num">Point size
            <input type="number" min="1" max="30" v-model.number="settings.viewerPointSize"
                   v-tooltip.bottom="'Cell marker size in screen pixels'" /></label>
          <label class="pref-num">Point outline
            <input type="number" min="0" max="10" v-model.number="settings.viewerPointBorder"
                   v-tooltip.bottom="'Black outline width around each point; 0 = no outline'" /></label>
          <label class="pref-num">Grid density
            <input type="number" min="2" max="32" v-model.number="settings.viewerGridDensity"
                   v-tooltip.bottom="'Cells per axis on the grid + landscape overlays'" /></label>
          <label class="pref-num">Tail length (frames)
            <input type="number" min="0" max="500" v-model.number="settings.viewerTailLength"
                   v-tooltip.bottom="'Track tail length in frames; 0 hides tails'" /></label>
          <label class="pref-num">Tail width
            <input type="number" min="0" max="12" v-model.number="settings.viewerTailWidth"
                   v-tooltip.bottom="'Track tail width in screen pixels'" /></label>
          <label class="pref-num">Label opacity (0-1)
            <input type="number" min="0" max="1" step="0.05" v-model.number="settings.viewerLabelOpacity"
                   v-tooltip.bottom="'Segmentation mask fill opacity; 0 = invisible, 1 = solid'" /></label>
          <label class="pref-num">Label contour (voxels)
            <input type="number" min="0" max="8" v-model.number="settings.viewerLabelContour"
                   v-tooltip.bottom="'Outline width in voxels; 0 = filled mask'" /></label>
          <label class="pref-num">Point z-tolerance
            <input type="number" min="0" max="20" v-model.number="settings.viewerPointZTol"
                   v-tooltip.bottom="'How many z planes either side still draw a cell marker'" /></label>
          <label class="pref-num">Track z-tolerance
            <input type="number" min="0" max="20" v-model.number="settings.viewerTrackZTol"
                   v-tooltip.bottom="'How many z planes either side still draw a track tail'" /></label>
        </template>

        <!-- ── Movies ────────────────────────────────────────────────────── -->
        <template v-else-if="active === 'movies'">
          <div class="pref-group cc-eyebrow cc-fs-2xs">Playback</div>
          <label class="pref-num">Playback rate
            <input type="number" min="0.1" max="8" step="0.1" v-model.number="settings.moviesPlaybackRate"
                   v-tooltip.bottom="'Movies page playback speed multiplier'" /></label>
          <label class="pref-num">Zoom
            <input type="number" min="0.1" max="8" step="0.1" v-model.number="settings.moviesZoom"
                   v-tooltip.bottom="'Movies page zoom multiplier'" /></label>
          <CcToggle v-model="settings.moviesAutoplay" label="Autoplay on select"
                    v-tooltip.bottom="'Start playback as soon as a movie is opened'" />

          <div class="pref-group cc-eyebrow cc-fs-2xs" style="margin-top:0.8rem">On end</div>
          <select class="pref-input" v-model="settings.moviesEndMode"
                  v-tooltip.bottom="'What happens when a movie ends'">
            <option value="stop">Stop</option>
            <option value="repeat">Repeat</option>
            <option value="next">Play next</option>
          </select>

          <div class="pref-group cc-eyebrow cc-fs-2xs" style="margin-top:0.8rem">Movie list</div>
          <CcToggle v-model="settings.moviesShowDetails" label="Show Details columns"
                    v-tooltip.bottom="'Extra columns on the movie list — channels + attributes'" />
          <label class="pref-num" style="margin-top:0.35rem">Channel scope
            <select class="pref-input" v-model="settings.moviesChannelMode"
                    v-tooltip.bottom="'Which channels fill the Details columns'">
              <option value="image">Image channels</option>
              <option value="movie">Movie channels only</option>
            </select></label>

          <!-- Animation page settings live here (sibling of Movies) — the Animation page banks
               per-keyframe view state on top of movie-page state, so a user who cares about one
               often cares about the other. -->
          <div class="pref-group cc-eyebrow cc-fs-2xs" style="margin-top:0.8rem">Animation page</div>
          <CcToggle v-model="settings.animationSyncViewer"
                    label="Sync the animation page to the viewer"
                    v-tooltip.bottom="'Selecting a keyframe pushes its view into the viewer'" />
        </template>

        <!-- ── Layout ────────────────────────────────────────────────────── -->
        <template v-else-if="active === 'layout'">
          <CcToggle v-model="settings.sidebarCollapsed" label="Collapse main sidebar"
                    v-tooltip.bottom="'Hide the left nav sidebar to free working space'" />
          <CcToggle v-model="settings.rightPanelCollapsed" label="Collapse module task panel"
                    v-tooltip.bottom="'Hide the right task panel on module pages'" />
          <CcToggle v-model="settings.viewerWindowSideCollapsed" label="Collapse viewer-window controls"
                    v-tooltip.bottom="'Hide the viewer pop-out sidebar (independent of the module page)'" />

          <div class="pref-group cc-eyebrow cc-fs-2xs" style="margin-top:0.6rem">Floating panels</div>
          <CcToggle v-model="settings.viewerPanelOpen" label="Viewer panel open"
                    v-tooltip.bottom="'Show the floating viewer controls panel'" />
          <CcToggle v-model="settings.labLogPanelOpen" label="Lab log panel open"
                    v-tooltip.bottom="'Show the floating lab log panel'" />
          <CcToggle v-model="settings.correctionCockpitOpen" label="Correction cockpit open"
                    v-tooltip.bottom="'Show the floating correction cockpit'" />
          <CcToggle v-model="settings.kiwiOpen" label="Kiwi cockpit open"
                    v-tooltip.bottom="'Show the floating Kiwi assist cockpit'" />

          <div class="pref-group cc-eyebrow cc-fs-2xs" style="margin-top:0.6rem">Correction cockpit mode</div>
          <select class="pref-input" v-model="settings.correctionCockpitMode"
                  v-tooltip.bottom="'Which mode the correction cockpit opens in'">
            <option value="tracks">Tracks</option>
            <option value="labels">Labels</option>
            <option value="review">Review</option>
          </select>

          <div class="pref-group cc-eyebrow cc-fs-2xs" style="margin-top:0.6rem">Viewer selection mode</div>
          <select class="pref-input" v-model="settings.viewerSelectMode"
                  v-tooltip.bottom="'What a viewer click does — pan/zoom, or Select a cell'">
            <option value="off">Off</option>
            <option value="select">Select</option>
          </select>
        </template>

        <!-- ── Kiwi ──────────────────────────────────────────────────────── -->
        <template v-else-if="active === 'kiwi'">
          <CcToggle v-model="settings.kiwiReasoning"
                    label="Think first (write reasoning before claims)"
                    v-tooltip.bottom="'~40% more tokens; no measurable quality gain in comparison'" />
          <div class="pref-group cc-eyebrow cc-fs-2xs" style="margin-top:0.8rem">Model</div>
          <select class="pref-input" v-model="settings.kiwiModel"
                  v-tooltip.bottom="'Which Claude model Kiwi turns run under'">
            <option value="haiku">Haiku</option>
            <option value="sonnet">Sonnet</option>
            <option value="opus">Opus</option>
          </select>
        </template>

        <!-- ── Lab log ───────────────────────────────────────────────────── -->
        <!-- Distinct from Kiwi: the lab log is the durable per-project activity + narrative
             surface (Cecelia digests + human notes + Claude append), independent of whether any
             assistant is paired. Bunched together in the first draft; split out per feedback. -->
        <template v-else-if="active === 'lablog'">
          <CcToggle v-model="settings.labLogAutoContext"
                    label="Auto-capture app activity digests"
                    v-tooltip.bottom="'Cecelia appends a rolling daily digest of task activity'" />
          <CcToggle v-model="settings.labLogShowNames"
                    label="Show image names in the lab log (else UIDs)"
                    v-tooltip.bottom="'Show human-readable names instead of the stable UIDs'" />
        </template>

      </section>
    </div>
    <CreateProfileDialog v-if="showCreateProfile"
                         @close="showCreateProfile = false"
                         @created="onProfileCreated" />
  </BaseModal>
</template>

<style scoped>
.pref-hint { padding: 0.4rem 1rem; border-bottom: 1px solid var(--cc-border);
             display: flex; align-items: center; gap: 0.35rem; }
.pref-cols { display: grid; grid-template-columns: 12rem 1fr; gap: 1rem; height: 100%; min-height: 0; }
.pref-nav { display: flex; flex-direction: column; gap: 0.2rem;
            border-right: 1px solid var(--cc-border); padding-right: 0.6rem; }
.pref-nav-btn {
  display: flex; align-items: center; gap: 0.5rem;
  padding: 0.45rem 0.7rem;
  background: transparent; border: 1px solid transparent;
  border-radius: var(--cc-radius-sm);
  color: var(--cc-text-dim);
  cursor: pointer;
  text-align: left;
  font-size: var(--cc-fs-md);
}
.pref-nav-btn:hover { background: var(--cc-surface-2); color: var(--cc-text); }
.pref-nav-btn-active { background: var(--cc-surface-2); color: var(--cc-text);
                       border-color: var(--cc-border); }
.pref-pane {
  min-width: 0;
  overflow-y: auto;
  display: flex;
  flex-direction: column;
  gap: 0.55rem;
  padding-right: 0.25rem;
}
/* No color override here — `.cc-eyebrow` (the utility applied alongside) already provides the
   muted colour, and re-stating it would silently shadow the utility (`cssScenarios.test.ts`). */
.pref-num {
  display: grid;
  grid-template-columns: 1fr 5rem;
  align-items: center;
  gap: 0.5rem;
  font-size: var(--cc-fs-md);
  color: var(--cc-text);
}
.pref-num input { width: 100%; text-align: right; }
/* `.pref-input` inherits border/background/colour from the global input base (style.css).
   Only shape overrides live here so the ratchet in `cssScenarios.test.ts` stays green. */
.pref-input {
  width: 100%;
  padding: 0.3rem 0.4rem;
  border-radius: var(--cc-radius-sm);
  font-size: var(--cc-fs-md);
}
/* Profiles pane */
.pref-note { margin: 0 0 0.5rem; }
.pref-error { color: var(--cc-sev-fail); display: flex; align-items: center; gap: 0.35rem; margin: 0.35rem 0 0; }
.pp-list { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 0.3rem; }
.pp-row {
  display: flex; align-items: center; justify-content: space-between;
  padding: 0.5rem 0.7rem;
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-sm);
}
.pp-row-active   { border-color: var(--cc-kiwi); }
.pp-row-retired  { opacity: 0.6; }
.pp-name { display: flex; align-items: center; gap: 0.4rem; }
.pp-name-edit { flex: 1; min-width: 0; }
.pp-rename-input { flex: 1; min-width: 6rem; }
.pp-tag  { padding-inline: 0.25rem; }
.pp-tag-active { color: var(--cc-kiwi); }
</style>
