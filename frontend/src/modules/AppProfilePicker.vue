<script setup lang="ts">
// Launch-time profile picker (bare route /profile-picker). One screen: pick which profile drives
// this window. Every above-project surface (Kiwi identity + CLAUDE_CONFIG_DIR, per-profile settings,
// project list filter) reads from the choice made here. See docs/todo/USER_PROFILE_PLAN.md Phase 2.
//
// Gate is in main.ts's boot guard, keyed off `appCtl.needsProfilePick` set by refreshStartup():
//   - single profile → picker is auto-skipped (never mounted).
//   - > 1 profile   → boot guard bounces every other route here until the user picks.
//   - reload re-arms — the pick does NOT survive a page reload (a shared box switches drivers).
//
// Layout: card matching /setup, one clickable row per profile. Retired profiles surface greyed out
// (D11 immutable-name lifecycle — the record must stay resolvable, but new turns can't run under
// it). The `+` opens CreateProfileDialog which POSTs /create then /select; on success this
// window has an active profile and we call completeProfilePick + navigate to the app.
import { ref, computed, onMounted } from 'vue'
import { useRouter } from 'vue-router'
import { useAppControlStore } from '../stores/appControl'
import { useLogStore } from '../stores/log'
import { fetchProfiles, selectProfile, type Profile, type ProfileRoster } from '../utils/profileApi'
import CreateProfileDialog from '../components/profile/CreateProfileDialog.vue'

const router = useRouter()
const appCtl = useAppControlStore()
const log = useLogStore()

const roster = ref<ProfileRoster>({ active: 'default', profiles: [], legacyReserved: ['legacy'] })
const loading = ref(true)
const submitting = ref('')   // profile name currently being selected — drives the row spinner
const error = ref<string | null>(null)
const showCreate = ref(false)

const selectable = computed(() => roster.value.profiles.filter(p => !p.retired))
const retired = computed(() => roster.value.profiles.filter(p => p.retired))

onMounted(async () => {
  try {
    roster.value = await fetchProfiles()
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
    log.error(`Profile picker: fetch failed — ${error.value}`, { source: 'profile-picker' })
  } finally { loading.value = false }
})

async function pick(p: Profile) {
  if (p.retired || submitting.value) return
  submitting.value = p.name
  error.value = null
  try {
    const r = await selectProfile(p.name)
    if (!r.ok) { error.value = r.error ?? 'Select failed'; return }
    appCtl.completeProfilePick()
    router.replace('/')
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  } finally { submitting.value = '' }
}

function onProfileCreated(name: string) {
  // The dialog already POSTs /select for us on create — the newly-created profile is active
  // server-side. Mirror the pick() success path so the user lands in the app without a second
  // click.
  showCreate.value = false
  appCtl.completeProfilePick()
  void router.replace('/')
  log.info(`Profile picker: created and selected '${name}'`, { source: 'profile-picker' })
}
</script>

<template>
  <div class="pp-wrap">
    <div class="pp-card">
      <img class="pp-logo" src="/feijoa.svg" alt="" aria-hidden="true" width="40" height="40" />
      <h1 class="pp-title">Who's driving?</h1>
      <p class="pp-sub cc-muted">
        Pick a profile — this drives Kiwi's credentials, your preferences and which projects you see.
      </p>

      <div v-if="loading" class="pp-loading cc-muted cc-fs-md">
        <i class="pi pi-spin pi-spinner" /> Loading profiles…
      </div>

      <ul v-else class="pp-list">
        <li v-for="p in selectable" :key="p.name">
          <button class="pp-row cc-btn cc-btn-ghost"
                  :class="{ 'pp-row-active': p.name === roster.active }"
                  :disabled="!!submitting"
                  @click="pick(p)"
                  v-tooltip.right="p.isDefault
                    ? 'Default profile — uses your shared ~/.claude credentials'
                    : `Named profile — credentials live under kiwi-profiles/${p.name}/`">
            <span class="pp-name">
              {{ p.name }}
              <span v-if="p.isDefault" class="pp-tag cc-muted cc-fs-xs">(~/.claude)</span>
              <span v-if="p.name === roster.active" class="pp-tag pp-tag-active cc-fs-xs">last used</span>
            </span>
            <i v-if="submitting === p.name" class="pi pi-spin pi-spinner" />
            <i v-else class="pi pi-arrow-right" />
          </button>
        </li>
        <li v-for="p in retired" :key="p.name" class="pp-row-retired">
          <span class="pp-name cc-muted">
            {{ p.name }} <span class="pp-tag cc-fs-xs">(retired)</span>
          </span>
        </li>
      </ul>

      <p v-if="error" class="pp-error cc-fs-md">
        <i class="pi pi-exclamation-circle" /> {{ error }}
      </p>

      <div class="pp-footer">
        <button class="cc-btn cc-btn-ghost" :disabled="loading || !!submitting"
                @click="showCreate = true"
                v-tooltip.top="'Create a new profile — a separate credential + MCP scope for this seat login'">
          <i class="pi pi-plus" /> New profile
        </button>
      </div>
    </div>

    <CreateProfileDialog v-if="showCreate"
                         @close="showCreate = false"
                         @created="onProfileCreated" />
  </div>
</template>

<style scoped>
.pp-wrap {
  height: 100vh;
  display: flex;
  align-items: center;
  justify-content: center;
  background: var(--cc-bg);
  color: var(--cc-text);
}
.pp-card {
  width: 460px;
  max-width: calc(100vw - 2rem);
  background: var(--cc-surface-1);
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-lg);
  padding: 2rem;
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}
.pp-logo { display: block; margin: 0 auto; }
.pp-title { margin: 0.5rem 0 0; font-size: 1.4rem; font-weight: 600; text-align: center; }
.pp-sub { margin: 0 0 1rem; text-align: center; }
.pp-loading { display: flex; align-items: center; justify-content: center; gap: 0.4rem; padding: 1rem 0; }
.pp-list { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 0.35rem; }
.pp-row {
  width: 100%;
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 0.6rem 0.85rem;
  text-align: left;
  border-radius: var(--cc-radius-sm);
}
.pp-row-active { border-color: var(--cc-kiwi); }
.pp-row-retired {
  padding: 0.5rem 0.85rem;
  border: 1px dashed var(--cc-border);
  border-radius: var(--cc-radius-sm);
}
.pp-name { display: flex; align-items: center; gap: 0.5rem; }
.pp-tag { padding-inline: 0.35rem; }
.pp-tag-active { color: var(--cc-kiwi); }
.pp-error { color: var(--cc-sev-fail); display: flex; align-items: center; gap: 0.35rem; margin: 0.35rem 0 0; }
.pp-footer { display: flex; justify-content: flex-end; margin-top: 0.5rem; }
</style>
