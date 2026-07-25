<script setup lang="ts">
// First-launch setup wizard (bare route /setup). One screen: pick where projects are stored, which
// the backend writes to the user's custom.toml (~/.cecelia in prod). Removes the terminal-editing
// barrier for non-technical users. See docs/todo/ONBOARDING_PLAN.md.
import { ref, onMounted, watch } from 'vue'
import { useRouter } from 'vue-router'
import { useAppControlStore } from '../stores/appControl'
import { useLogStore } from '../stores/log'

const router = useRouter()
const appCtl = useAppControlStore()
const log = useLogStore()

const path = ref('')
const submitting = ref(false)
const restarting = ref(false)   // fallback path: backend asked for a restart (D3) — poll health then go
// live validation feedback from GET /api/setup/validate (debounced)
const check = ref<{ ok: boolean; message: string; willCreate: boolean } | null>(null)
let _validateTimer: ReturnType<typeof setTimeout> | undefined

onMounted(async () => {
  try {
    const d = await (await fetch('/api/setup/defaults')).json()
    path.value = d.projectsDir ?? ''
  } catch { path.value = '' }
})

// debounce validation so we don't hit the backend on every keystroke
watch(path, () => {
  check.value = null
  clearTimeout(_validateTimer)
  const p = path.value.trim()
  if (!p) return
  _validateTimer = setTimeout(validate, 300)
})

async function validate() {
  const p = path.value.trim()
  if (!p) return
  try {
    const res = await fetch(`/api/setup/validate?path=${encodeURIComponent(p)}`)
    check.value = await res.json()
  } catch { /* leave check null; submit still guards server-side */ }
}

async function submit() {
  const p = path.value.trim()
  if (!p || submitting.value) return
  submitting.value = true
  try {
    const res = await fetch('/api/setup/init', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectsDir: p }),
    })
    const body = await res.json().catch(() => ({})) as { projectsDir?: string; restartRequired?: boolean; error?: string }
    if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
    appCtl.completeSetup()
    if (body.restartRequired) {
      // config couldn't hot-reload — wait for the backend to come back, then enter the app (D3)
      restarting.value = true
      await waitForBackend()
    }
    router.replace('/import')
  } catch (e) {
    log.error(e instanceof Error ? e.message : String(e), { source: 'setup' })
    submitting.value = false
    restarting.value = false
  }
}

// poll /api/health until the restarted backend answers (fallback path only)
async function waitForBackend(timeoutMs = 60000) {
  await new Promise(r => setTimeout(r, 1500))
  const start = Date.now()
  while (Date.now() - start < timeoutMs) {
    try { if ((await fetch('/api/health', { cache: 'no-store' })).ok) return } catch { /* not up yet */ }
    await new Promise(r => setTimeout(r, 800))
  }
}
</script>

<template>
  <div class="setup-wrap">
    <div class="setup-card">
      <div class="setup-logo">🍍</div>
      <h1 class="setup-title">Welcome to Cecelia</h1>
      <p class="setup-sub cc-muted">Where would you like to store your projects?</p>

      <label class="setup-label" for="setup-path">Projects folder</label>
      <input id="setup-path" v-model="path" class="setup-input" type="text" spellcheck="false"
             autocomplete="off" placeholder="~/cecelia-projects"
             @keyup.enter="submit" />

      <!-- validation hint: neutral until checked, green when usable, danger when not -->
      <p class="setup-hint" :class="check ? (check.ok ? 'ok' : 'bad') : ''">
        <template v-if="check">
          <i :class="check.ok ? 'pi pi-check-circle' : 'pi pi-exclamation-circle'" />
          {{ check.message }}
        </template>
        <template v-else>
          A project holds all your images and analysis. You can change this later in Settings.
        </template>
      </p>

      <button class="cc-btn cc-btn-primary setup-go" :disabled="!path.trim() || submitting || (check ? !check.ok : false)"
              @click="submit">
        <i v-if="submitting" class="pi pi-spin pi-spinner" />
        {{ restarting ? 'Restarting…' : submitting ? 'Setting up…' : 'Get started →' }}
      </button>
    </div>
  </div>
</template>

<style scoped>
.setup-wrap {
  height: 100vh;
  display: flex;
  align-items: center;
  justify-content: center;
  background: var(--cc-bg);
  color: var(--cc-text);
}
.setup-card {
  width: 420px;
  max-width: calc(100vw - 2rem);
  background: var(--cc-surface-1);
  border: 1px solid var(--cc-border);
  border-radius: 12px;
  padding: 2rem;
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}
.setup-logo { font-size: 2.5rem; line-height: 1; }
.setup-title { margin: 0.5rem 0 0; font-size: 1.4rem; font-weight: 600; }
.setup-sub { margin: 0 0 1rem; }   /* + .cc-muted */
.setup-label { font-size: 0.8rem; color: var(--cc-text-dim); margin-bottom: 0.15rem; }
.setup-input { width: 100%; font-family: var(--cc-mono, monospace); }
.setup-hint {
  min-height: 1.2rem;
  margin: 0.15rem 0 0.5rem;
  font-size: 0.8rem;
  color: var(--cc-text-dim);
  display: flex;
  align-items: center;
  gap: 0.35rem;
}
.setup-hint.ok  { color: var(--cc-viewer); }
.setup-hint.bad { color: var(--cc-danger); }
.setup-go { margin-top: 0.5rem; align-self: flex-end; }
</style>
