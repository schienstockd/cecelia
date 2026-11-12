<!--
  The header's avatar menu (docs/todo/GUI_LAYOUT_PLAN.md → Today → new home). Who is driving, and the
  session controls that used to sit in the sidebar footer: Preferences, Restart backend (dev), Log out,
  Quit. Quit and Log out arm in place (`ConfirmButton`); Quit counts running tasks when armed, because
  shutdown does not wait for in-flight work.
-->
<script setup lang="ts">
import { computed, ref } from 'vue'
import TeleportPopover from './TeleportPopover.vue'
import ConfirmButton from './ConfirmButton.vue'
import { useAppControlStore } from '../stores/appControl'
import { useSettingsStore } from '../stores/settings'
import { nextColourScheme, colourSchemeLabel } from '../utils/colourScheme'
import { openPreferences } from '../lib/preferencesOpen'
import { runningTaskCount } from '../utils/runningTasks'
import { quitConfirmTooltip } from '../utils/quitWarning'
import { useToast } from '../composables/useToast'

const appCtl = useAppControlStore()
const settings = useSettingsStore()
// one click to the other colour scheme; the menu stays open so the change is seen in place
const nextScheme = computed(() => nextColourScheme(settings.colourScheme))
const open = ref(false)
const anchor = ref<HTMLElement | null>(null)

// two letters of the profile name: the avatar is a glance answer to "who am I logged in as?"
const initials = computed(() => appCtl.activeProfileDisplayName.slice(0, 2).toLowerCase())

// The count is read when Quit is ARMED (not on open) — one request per intent to quit.
const quitTasks = ref(0)
const quitConfirm = computed(() => quitConfirmTooltip(quitTasks.value))
async function armQuit(arm: () => void) {
  arm()                                        // arm first: the confirm must appear immediately
  quitTasks.value = await runningTaskCount()   // then fill in what it will cost
}
// The store refuses (and says why) if the Claude logout failed, so the toast is the only failure path.
async function logOutUser() {
  const err = await appCtl.logOutUser()
  if (err) useToast().add({ severity: 'warn', summary: err, life: 6000 })
}
const logOutTip = computed(() => appCtl.activeProfileName === 'default'
  ? `Log ${appCtl.activeProfileDisplayName} out — its terminal Claude login stays`
  : `Log ${appCtl.activeProfileDisplayName} out of Cecelia and Claude`)
// A refused restart (tasks would die, or not supervised) has no status line here, so it is a toast.
async function restartBackend() {
  open.value = false
  const err = await appCtl.restartBackend()
  if (err) useToast().add({ severity: 'warn', summary: err, life: 6000 })
}
function prefs(pane: 'profiles' | 'interface') { open.value = false; openPreferences(pane) }
</script>

<template>
  <button ref="anchor" type="button" class="avatar cc-btn cc-btn-ghost cc-btn-icon" data-guide="header.profile"
          :class="{ 'cc-btn-on': open }"
          aria-haspopup="menu" :aria-label="`Profile ${appCtl.activeProfileDisplayName} — menu`"
          v-tooltip.bottom="`Profile: ${appCtl.activeProfileDisplayName}`"
          @click="open = !open">
    {{ initials }}
  </button>
  <TeleportPopover v-model="open" :anchor="anchor" placement="bottom-end" flush>
    <div class="cc-actions-menu" role="menu">
      <div class="cc-actions-head">{{ appCtl.activeProfileDisplayName }}</div>
      <button class="cc-actions-item" role="menuitem" @click="prefs('profiles')">
        <i class="pi pi-user" /> Switch profile
      </button>
      <button class="cc-actions-item" role="menuitem" @click="settings.colourScheme = nextScheme">
        <i class="pi pi-palette" /> {{ colourSchemeLabel(nextScheme) }}
      </button>
      <button class="cc-actions-item" role="menuitem" @click="prefs('interface')">
        <i class="pi pi-cog" /> Preferences
      </button>
      <button v-if="appCtl.dev" class="cc-actions-item" role="menuitem" :disabled="appCtl.busy"
              v-tooltip.left="'Reconnects when it is back'" @click="restartBackend">
        <i :class="['pi', appCtl.busy ? 'pi-spin pi-spinner' : 'pi-refresh']" /> Restart backend (dev)
      </button>
      <ConfirmButton @confirm="logOutUser" v-slot="{ armed, press }">
        <button class="cc-actions-item" role="menuitem" :class="{ armed }" :disabled="appCtl.busy"
                v-tooltip.left="armed ? 'Click again to log out' : logOutTip" @click.stop="press">
          <i :class="['pi', armed ? 'pi-exclamation-triangle' : 'pi-sign-out']" /> Log out
        </button>
      </ConfirmButton>
      <ConfirmButton @confirm="appCtl.quit()" v-slot="{ armed, arm, confirm }">
        <button class="cc-actions-item danger" role="menuitem" :class="{ armed }" :disabled="appCtl.busy"
                data-guide="header.quit"
                v-tooltip.left="armed ? quitConfirm : 'Stop notebooks and the backend'"
                @click.stop="armed ? confirm() : armQuit(arm)">
          <i :class="['pi', armed ? 'pi-exclamation-triangle' : 'pi-power-off']" /> Quit Cecelia
        </button>
      </ConfirmButton>
    </div>
  </TeleportPopover>
</template>

<style scoped>
/* a round icon button carrying two letters instead of a glyph */
.avatar { width: 26px; height: 26px; border-radius: var(--cc-radius-pill); font-size: var(--cc-fs-xs); }
</style>
