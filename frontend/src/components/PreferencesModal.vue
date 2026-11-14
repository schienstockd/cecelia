<script setup lang="ts">
// PreferencesModal — the active profile's per-user preferences (USER_PROFILE_PLAN Phase 4).
// Two-pane layout (`SettingsShell`, shared with Settings): category list left, the selected
// category's fields right. Every field two-way-binds to a ref on `useSettingsStore`;
// the store's own PATCH debouncer coalesces changes into one `/api/profile/settings/patch`
// round-trip per burst, so this component has no save button — writes are automatic.
//
// This modal edits the *active* profile's settings only; it is NOT a shared machine panel.
// Machine-scoped settings (image compressor, storage layout, plugin registry, port toggles) live
// on SettingsModal.vue, the same shell.
import { ref, computed, onMounted } from 'vue'
import { useSettingsStore } from '../stores/settings'
import { useAppControlStore } from '../stores/appControl'
import { renameLiveMirrorOwner } from '../utils/profileStorage'
import SettingsShell, { type SettingsCategory } from './SettingsShell.vue'
import CcToggle from './CcToggle.vue'
import ChipSelect from './ChipSelect.vue'
import { COLOUR_SCHEMES, type ColourScheme } from '../utils/colourScheme'
import ConfirmButton from './ConfirmButton.vue'
import ConfirmDeleteButton from './ConfirmDeleteButton.vue'
import SelectionTable, { type SelectionColumn } from './SelectionTable.vue'
import CreateProfileDialog from './profile/CreateProfileDialog.vue'
import HpcSettings from './HpcSettings.vue'
import { preferencesPane, type PreferencesPane } from '../lib/preferencesOpen'
import { useCopyFlash } from '../composables/useCopyFlash'
import { useMcpConnections } from '../composables/useMcpConnections'
import { useObserverStore } from '../stores/observer'
import { terminalCta, terminalSetupTooltip } from '../utils/observerSetup'
import { CLAUDE_TERMINAL, claudeChatCommand } from '../lib/claudeOverview'
import { dotClass } from '../utils/statusDot'
import { fetchSharedWorkstation } from '../utils/sharedWorkstation'
import { AI_OFF_COPY } from '../utils/aiSwitch'
import { openAiOptIn } from '../lib/aiOptInOpen'
import { fetchProfiles, selectProfile, retireProfile, renameProfile, deleteProfile,
         isValidProfileName, profileDisplayName, profileRowActions,
         type Profile, type ProfileRoster } from '../utils/profileApi'

defineEmits<{ (e: 'close'): void }>()

const settings = useSettingsStore()
const appCtl   = useAppControlStore()

// Profiles is FIRST — it's the identity primitive every other pane's setting hangs off, and the
// answer to "how do I switch" needs to be one click away. The other panes follow roughly by how
// often they get touched.
type CategoryId = PreferencesPane
const CATEGORIES: SettingsCategory<CategoryId>[] = [
  { id: 'profiles',  label: 'Profiles',  icon: 'pi-user' },
  { id: 'interface', label: 'Interface', icon: 'pi-sliders-h' },
  { id: 'viewer',    label: 'Viewer',    icon: 'pi-eye' },
  { id: 'movies',    label: 'Movies',    icon: 'pi-video' },
  { id: 'layout',    label: 'Layout',    icon: 'pi-window-maximize' },
  { id: 'kiwi',      label: 'Kiwi',      icon: 'pi-comments', guide: 'prefs.kiwi' },
  { id: 'lablog',    label: 'Lab log',   icon: 'pi-book' },
  { id: 'hpc',       label: 'HPC',       icon: 'pi-server' },
]
const active = ref<CategoryId>(preferencesPane.value)

// ── Profiles pane state ──────────────────────────────────────────────────────
// Roster is server truth (custom.toml [ai].profile + user-profiles subdirectories). Local state is
// a cache so the pane renders while the round-trip runs. Failure → `default`-only fallback via
// `fetchProfiles`, which keeps the pane usable rather than blanking it out.
const roster = ref<ProfileRoster>({
  active: 'default',
  profiles: [{ name: 'default', dir: '', isDefault: true, retired: false }],
  legacyReserved: ['legacy'],
})
// Name fills; each action is a fixed column so its button holds its place across rows.
const PROFILE_COLUMNS: SelectionColumn[] = [
  { key: 'name',   label: 'Profile' },
  { key: 'switch', label: '', fixed: true, width: 96 },
  { key: 'rename', label: '', fixed: true, width: 40 },
  { key: 'delete', label: '', fixed: true, width: 40 },
]
const rowActions = (p: Profile) => profileRowActions(p, roster.value.active, appCtl.canManageMachine)
const profilesBusy = ref<string>('')      // name currently being selected/retired — drives per-row spinner
const profilesError = ref<string | null>(null)
const showCreateProfile = ref(false)

async function refreshRoster() {
  try {
    roster.value = await fetchProfiles()
  } catch (e) {
    profilesError.value = e instanceof Error ? e.message : String(e)
  }
}
onMounted(refreshRoster)

// Kiwi → AI features: turning ON asks first (AiOptInDialog); turning OFF needs no consent.
function onAiToggle(on: boolean) {
  if (on) openAiOptIn()
  else void observer.setAiEnabled(false)
}

// Kiwi → "Delete conversations on log out": the workstation policy is the floor — when it's on, the
// personal switch shows on and locked. A failed read leaves the switch editable.
const purgeForcedByWorkstation = ref(false)
onMounted(async () => {
  try { purgeForcedByWorkstation.value = (await fetchSharedWorkstation()).purgeClaudeConversations === true }
  catch { /* older backend — the personal switch still works */ }
})

// Switching the profile mid-session works but half the app is already mounted against the
// previous identity (per-profile settings, project filter, MCP registration). The plan is
// explicit: change-of-profile requires reload. Server-side write goes out first so a reload
// picks up the new active profile.
async function pickProfile(name: string) {
  if (name === roster.value.active || profilesBusy.value) return
  profilesBusy.value = name
  profilesError.value = null
  try {
    await settings.flushProfileSettings()   // pending changes belong to THIS profile, and the pick reads them
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
    renameLiveMirrorOwner(oldName, newName)   // same person — keep this browser's mirror theirs
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

// ── Kiwi pane: Cecelia's MCP registration in THIS profile's Claude config ─────────────────────────
// Personal, not machine-wide (it is the active profile's `.claude.json`), so it lives here rather than
// in Settings → System (docs/todo/SETTINGS_SCOPE_PLAN.md). Kiwi's Terminal row offers the same
// Set up / Fix; this is the place that also shows which checkout is registered and the fallback.
const observer = useObserverStore()
const { observerRow, observerInstallShort } = useMcpConnections()
const { isCopied: observerCmdCopied, copy: copyCmdFlash } = useCopyFlash()
const observerFallbackCommand = computed(() => claudeChatCommand(observer.mcpConfigPath))
const copyObserverFallback = () => copyCmdFlash(observerFallbackCommand.value)

// No loading hint: values are always usable — hydration overrides localStorage in place if the
// TOML has anything to say (Vue reactivity picks up the flip). The hint used to spin indefinitely
// when the backend hadn't been restarted to register /api/profile/settings; a stale endpoint is
// now bounded by fetchProfileSettings' 5s AbortSignal timeout, not by user perception.
</script>

<template>
  <SettingsShell v-model="active" title="Preferences" icon="pi-user" :categories="CATEGORIES"
                 aria-label="Preference categories" @close="$emit('close')">
    <!-- ── Profiles ──────────────────────────────────────────────────── -->
    <template #profiles>
      <p class="pref-note cc-muted cc-fs-xs">
        Profiles set Kiwi's login, preferences and projects. Switching reloads the app.
      </p>
      <!-- Switch / Rename / Delete are fixed columns, so a row that doesn't offer one leaves
           its cell empty and Switch never moves. -->
      <SelectionTable selection-mode="none" headerless data-guide="prefs.profilesPane"
                      :columns="PROFILE_COLUMNS" :rows="roster.profiles" id-key="name"
                      :row-class="p => ({ 'pp-row-active': p.name === roster.active,
                                          'pp-row-retired': p.retired })"
                      :row-tooltip="p => profileDisplayName(p)">
        <template #cell-name="{ row: p }">
          <!-- Inline rename: Enter commits, Escape cancels. Client-side validity mirrors the
               server rule (`isValidProfileName`) so Save gates before the round-trip. -->
          <span v-if="renamingName === p.name" class="pp-name">
            <input v-model="renameDraft" class="pp-rename-input"
                   v-tooltip.bottom="'1-32 chars, lower-ASCII alnum + `-` / `_`'"
                   @keyup.enter="commitRename"
                   @keyup.escape="cancelRename" />
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
          <span v-else class="pp-name">
            <i class="pi" :class="p.retired ? 'pi-user-minus' : 'pi-user'" />
            {{ profileDisplayName(p) }}
            <span v-if="p.isDefault" class="pp-tag cc-muted cc-fs-2xs">(~/.claude)</span>
            <span v-if="p.retired" class="pp-tag cc-muted cc-fs-2xs">(retired)</span>
            <span v-if="p.name === roster.active" class="pp-tag pp-tag-active cc-fs-2xs">active</span>
          </span>
        </template>
        <!-- Which actions a row offers: `profileRowActions` (utils/profileApi.ts) — peanut manages
             every profile; anyone else renames only their own. -->
        <template #cell-switch="{ row: p }">
          <button v-if="rowActions(p).switch && renamingName !== p.name"
                  class="cc-btn cc-btn-ghost cc-fs-xs"
                  :disabled="!!profilesBusy"
                  @click="pickProfile(p.name)"
                  v-tooltip.bottom="`Switch to ${profileDisplayName(p)} — the app will reload`">
            <i :class="['pi', profilesBusy === p.name ? 'pi-spin pi-spinner' : 'pi-arrow-right']" />
            Switch
          </button>
          <span v-else />
        </template>
        <template #cell-rename="{ row: p }">
          <button v-if="rowActions(p).rename && renamingName !== p.name"
                  class="cc-btn cc-btn-ghost cc-btn-icon cc-fs-xs"
                  :disabled="!!profilesBusy"
                  @click="startRename(p.name)"
                  v-tooltip.bottom="'Rename — past turn logs still reference the old name'">
            <i class="pi pi-pencil" />
          </button>
          <span v-else />
        </template>
        <template #cell-delete="{ row: p }">
          <ConfirmDeleteButton v-if="rowActions(p).delete && renamingName !== p.name"
                               :disabled="!!profilesBusy"
                               title="Delete this profile — credentials + settings + turn provenance gone"
                               armed-title="Click again to permanently delete this profile"
                               @confirm="deleteProfileByName(p.name)" />
          <span v-else />
        </template>
      </SelectionTable>

      <p v-if="profilesError" class="pref-error cc-fs-md">
        <i class="pi pi-exclamation-circle" /> {{ profilesError }}
      </p>

      <div class="cc-row cc-row-tight" style="flex-wrap:wrap; margin-top:0.6rem;">
        <button class="cc-btn cc-btn-ghost cc-fs-xs" data-guide="prefs.newProfile"
                @click="showCreateProfile = true"
                v-tooltip.top="'A separate credential + MCP scope for this seat login'">
          <i class="pi pi-user-plus" /> New profile
        </button>
        <!-- Retire (LOGIN_CREDENTIAL_ISOLATION_PLAN D11). Only offered when the active
             profile is a named, non-retired one — retiring `default` is meaningless. -->
        <ConfirmButton v-if="roster.active !== 'default' &&
                             !roster.profiles.find(p => p.name === roster.active)?.retired"
                       @confirm="retireActive" v-slot="{ armed, press }">
          <button class="cc-btn cc-fs-xs" :class="armed ? 'cc-btn-danger' : 'cc-btn-ghost'"
                  :disabled="!!profilesBusy" @click="press"
                  v-tooltip.top="armed ? `Click again to retire ${roster.active}`
                                       : 'Retire this profile — non-selectable after, data stays on disk'">
            <i class="pi" :class="armed ? 'pi-exclamation-triangle' : 'pi-user-minus'" />
            Retire active
          </button>
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
    <template #interface>
      <div class="pref-group cc-eyebrow cc-fs-2xs">Appearance</div>
      <ChipSelect :options="[...COLOUR_SCHEMES]" :model-value="settings.colourScheme"
                  aria-label="Colour scheme"
                  v-tooltip.bottom="'App colours only — images and exports stay as they are'"
                  @update:model-value="settings.colourScheme = $event as ColourScheme" />

      <div class="pref-group cc-eyebrow cc-fs-2xs" style="margin-top:0.8rem">Task Manager</div>
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

      <div class="pref-group cc-eyebrow cc-fs-2xs" style="margin-top:0.8rem">Launch</div>
      <CcToggle v-model="settings.tipsOnLaunch"
                label="Show tip of the day"
                v-tooltip.bottom="'Prepends the daily tip to the What is New modal once per day'" />
    </template>

    <!-- ── Viewer (behaviour + on-image chrome + sizes) ─────────────── -->
    <template #viewer>
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
               v-tooltip.bottom="'Cell marker size in image pixels — scales with zoom'" /></label>
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
               v-tooltip.bottom="'Track tail width in image pixels — scales with zoom'" /></label>
      <label class="pref-num">Label opacity (0-1)
        <input type="number" min="0" max="1" step="0.05" v-model.number="settings.viewerLabelOpacity"
               v-tooltip.bottom="'Segmentation mask fill opacity; 0 = invisible, 1 = solid'" /></label>
      <label class="pref-num">Label contour (voxels)
        <input type="number" min="0" max="8" v-model.number="settings.viewerLabelContour"
               v-tooltip.bottom="'Outline width in voxels; 0 = filled mask'" /></label>
      <label class="pref-num">Point z-tolerance
        <input type="number" min="0" max="20" v-model.number="settings.viewerPointZTol"
               v-tooltip.bottom="'Planes either side of a single 2D plane that still show a cell'" /></label>
      <label class="pref-num">Track z-tolerance
        <input type="number" min="0" max="20" v-model.number="settings.viewerTrackZTol"
               v-tooltip.bottom="'Planes either side of a single 2D plane that still show a tail'" /></label>
    </template>

    <!-- ── Movies ────────────────────────────────────────────────────── -->
    <template #movies>
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
    <template #layout>
      <!-- Only what no live control already sets. The sidebar (☰), the task panel (its chevron) and
           the floating panels (the header launchers) persist through those controls. -->
      <CcToggle v-model="settings.viewerWindowSideCollapsed" label="Collapse viewer-window controls"
                v-tooltip.bottom="'Hide the viewer pop-out sidebar (independent of the module page)'" />

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
    <template #kiwi>
      <!-- The AI switch for this profile (utils/aiSwitch.ts). ON goes through the opt-in dialog;
           OFF is immediate. Locked when peanut turned AI off for the workstation. -->
      <CcToggle :model-value="observer.ai.enabled" :disabled="observer.ai.setBy === 'workstation'"
                label="AI features" data-guide="prefs.aiFeatures"
                v-tooltip.bottom="observer.ai.setBy === 'workstation' ? AI_OFF_COPY.workstation
                                  : 'Kiwi, claim review and Claude in the terminal'"
                @update:model-value="onAiToggle" />
      <CcToggle v-if="!observer.ai.enabled" style="margin-top:0.4rem"
                :model-value="!settings.kiwiHidden" label="Show the Kiwi button"
                v-tooltip.bottom="'Opens a short note while AI is off'"
                @update:model-value="settings.kiwiHidden = !$event" />
      <template v-if="observer.ai.enabled">
      <CcToggle v-model="settings.kiwiReasoning"
                label="Think first (write reasoning before claims)"
                v-tooltip.bottom="'~40% more tokens; no measurable quality gain in comparison'" />
      <div class="pref-group cc-eyebrow cc-fs-2xs" style="margin-top:0.8rem">Model</div>
      <select class="pref-input" v-model="settings.kiwiModel"
              v-tooltip.bottom="'Which Claude model Kiwi turns run under'">
        <!-- the ONE Kiwi model picker; options are the backend allow-list (OBSERVER_MODELS) -->
        <option v-for="m in observer.models" :key="m" :value="m">{{ m.charAt(0).toUpperCase() + m.slice(1) }}</option>
      </select>
      </template>
      <!-- never peanut: its conversations are terminal claude's (claude_conversations.jl) -->
      <CcToggle v-if="appCtl.activeProfileName !== 'default'" style="margin-top:0.8rem"
                :model-value="settings.purgeClaudeConversations || purgeForcedByWorkstation"
                :disabled="purgeForcedByWorkstation"
                label="Delete my conversations on log out"
                v-tooltip.bottom="purgeForcedByWorkstation ? 'Required on this workstation'
                                  : 'Transcripts and prompt history; your Claude memory stays'"
                @update:model-value="settings.purgeClaudeConversations = $event" />
      <template v-if="observer.ai.enabled">
      <!-- The Cecelia plugin (observer MCP + caller hook) in this profile's Claude config — set up once per profile. -->
      <template v-if="observerRow">
        <div class="pref-group cc-eyebrow cc-fs-2xs" style="margin-top:0.8rem">Cecelia plugin</div>
        <div class="pk-mcp">
          <span class="pk-state cc-fs-sm" v-tooltip.top="observerRow.hint">
            <span class="cc-dot" :class="dotClass(observerRow.tone)" /> {{ observerRow.label }}
          </span>
          <span v-if="observerInstallShort" class="pk-install cc-muted cc-fs-xs"
                v-tooltip.top="observerRow.installPath">{{ observerInstallShort }}</span>
          <button v-if="observerRow.tone === 'warn'" class="cc-btn cc-btn-ghost cc-fs-xs"
                  :disabled="observer.registering" @click="observer.registerMcp()"
                  v-tooltip.top="terminalSetupTooltip(observer.terminalState)">
            <i :class="['pi', observer.registering ? 'pi-spin pi-spinner' : 'pi-download']" />
            {{ observer.registering ? CLAUDE_TERMINAL.busy
               : terminalCta(observer.available, observer.terminalState) === 'resync'
                 ? CLAUDE_TERMINAL.resync : CLAUDE_TERMINAL.action }}
          </button>
        </div>
        <!-- one-click setup FAILED: the resolved command to run by hand, next to the row it fixes -->
        <div v-if="observer.registerError" class="pk-note cc-row cc-row-tight">
          <strong class="cc-fs-xs">{{ observer.registerError }}</strong>
          <template v-if="observerFallbackCommand">
            <code class="cc-fs-2xs">{{ observerFallbackCommand }}</code>
            <button class="cc-btn cc-btn-bare cc-btn-icon" @click="copyObserverFallback"
                    v-tooltip.left="observerCmdCopied() ? 'Copied!' : 'Copy command'">
              <i :class="observerCmdCopied() ? 'pi pi-check' : 'pi pi-copy'" />
            </button>
          </template>
        </div>
      </template>
      </template>
    </template>

    <!-- ── Lab log ───────────────────────────────────────────────────── -->
    <!-- Distinct from Kiwi: the lab log is the durable per-project activity + narrative
         surface (Cecelia digests + human notes + Claude append), independent of whether any
         assistant is paired. Bunched together in the first draft; split out per feedback. -->
    <template #lablog>
      <CcToggle v-model="settings.labLogAutoContext"
                label="Summarise app activity automatically"
                v-tooltip.bottom="'Cecelia appends a rolling daily digest of task activity'" />
      <CcToggle v-model="settings.labLogShowNames"
                label="Show image names in the lab log (else UIDs)"
                v-tooltip.bottom="'Show human-readable names instead of the stable UIDs'" />
    </template>

    <!-- ── HPC ───────────────────────────────────────────────────────── -->
    <!-- Your cluster login and job classes (HPC_PLAN D9). The cluster itself (scheduler, host,
         port) and the environment install are machine-wide, on Settings → HPC. -->
    <template #hpc>
      <HpcSettings scope="user" project-uid="" />
    </template>

    <template #after>
      <CreateProfileDialog v-if="showCreateProfile"
                           @close="showCreateProfile = false"
                           @created="onProfileCreated" />
    </template>
  </SettingsShell>
</template>

<style scoped>
.pref-hint { padding: 0.4rem 1rem; border-bottom: 1px solid var(--cc-border);
             display: flex; align-items: center; gap: 0.35rem; }
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
.pk-mcp { display: flex; align-items: center; gap: 0.6rem; min-width: 0; }
.pk-state { display: inline-flex; align-items: center; gap: 0.35rem; color: var(--cc-text); }
.pk-install { font-family: var(--cc-mono); overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.pk-note { margin: 0.4rem 0 0; padding: 0.3rem 0.5rem; border-left: 3px solid var(--cc-sev-warn); min-width: 0; }
.pk-note strong { color: var(--cc-sev-warn); }
.pk-note code { font-family: var(--cc-mono); overflow-x: auto; white-space: nowrap; min-width: 0; }
.pref-error { color: var(--cc-sev-fail); display: flex; align-items: center; gap: 0.35rem; margin: 0.35rem 0 0; }
:deep(.pp-row-active) td:first-child { box-shadow: inset 3px 0 0 var(--cc-kiwi); }
:deep(.pp-row-retired) { opacity: 0.6; }
.pp-name { display: flex; align-items: center; gap: 0.4rem; }
.pp-rename-input { flex: 1; min-width: 6rem; }
.pp-tag  { padding-inline: 0.25rem; }
.pp-tag-active { color: var(--cc-kiwi); }
</style>
