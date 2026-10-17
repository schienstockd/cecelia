<!--
  Log a profile in to Claude — Cecelia runs `claude auth login` for it (LOGIN_CREDENTIAL_ISOLATION_PLAN
  P9). Mounting starts the login, unmounting cancels one still waiting. The CLI opens a PRIVATE browser
  window (so the shared browser's last Google/claude.ai session can't approve it); when none opens,
  the fallback is the link + the code its page shows. Body only — hosts wrap it: CreateProfileDialog's
  second step, and the Preferences → Profiles row's Log in.
-->
<script setup lang="ts">
import { ref, onMounted, onUnmounted } from 'vue'
import { useCopyFlash } from '../../composables/useCopyFlash'
import { startClaudeLogin, fetchClaudeLoginRun, sendClaudeLoginCode, cancelClaudeLogin,
         fetchClaudeLogin, claudeLoginPrompt, type ClaudeLoginRun } from '../../utils/profileApi'

const props = defineProps<{ name: string }>()
const emit = defineEmits<{ (e: 'loggedIn', email: string): void }>()

const run = ref<ClaudeLoginRun | null>(null)
const email = ref('')
const code = ref('')
const sending = ref(false)
const codeError = ref('')
const { isCopied: linkCopied, copy: copyLink } = useCopyFlash()

const POLL_MS = 1000
let timer: ReturnType<typeof setInterval> | null = null
const stopPolling = () => { if (timer) { clearInterval(timer); timer = null } }

async function onRun(r: ClaudeLoginRun) {
  // A dropped poll (`none` + error) keeps the last real state rather than flickering to "Starting…".
  if (r.state === 'none' && r.error && run.value) return
  run.value = r
  if (r.state === 'loggedIn' || r.state === 'failed') stopPolling()
  if (r.state === 'loggedIn') {
    email.value = (await fetchClaudeLogin(props.name, true)).email
    emit('loggedIn', email.value)
  }
}

async function start() {
  stopPolling()
  run.value = null; code.value = ''; codeError.value = ''
  const first = await startClaudeLogin(props.name)
  await onRun(first)
  if (first.state === 'waiting')
    timer = setInterval(async () => onRun(await fetchClaudeLoginRun(props.name)), POLL_MS)
}

async function submitCode() {
  if (!code.value.trim() || sending.value) return
  sending.value = true; codeError.value = ''
  try {
    const r = await sendClaudeLoginCode(props.name, code.value)
    if (!r.ok) codeError.value = r.error ?? 'Could not send the code'
  } finally { sending.value = false }
}

onMounted(start)
onUnmounted(() => {
  stopPolling()
  if (run.value?.state === 'waiting') void cancelClaudeLogin(props.name)
})
</script>

<template>
  <div class="cl-body">
    <p class="cl-status cc-fs-xs" :class="{ 'cl-ok': run?.state === 'loggedIn', 'cl-bad': run?.state === 'failed' }">
      <i :class="['pi', run?.state === 'loggedIn' ? 'pi-check-circle'
                       : run?.state === 'failed' ? 'pi-times-circle' : 'pi-spin pi-spinner']" />
      <template v-if="run?.state === 'loggedIn'">Logged in{{ email ? ` as ${email}` : '' }}</template>
      <template v-else>{{ claudeLoginPrompt(run) }}</template>
    </p>

    <template v-if="run?.state === 'waiting' && run.url">
      <div class="cc-row cc-row-tight">
        <button class="cc-btn cc-btn-ghost cc-fs-xs" @click="copyLink(run.url)"
                v-tooltip.bottom="'Paste it into a private window'">
          <i :class="['pi', linkCopied() ? 'pi-check' : 'pi-copy']" />
          {{ linkCopied() ? 'Copied' : run.browser ? 'No window? Copy the link' : 'Copy the link' }}
        </button>
      </div>
      <form class="cc-row cc-row-tight" @submit.prevent="submitCode">
        <input v-model="code" class="cl-code cc-input-xs" autocomplete="off" spellcheck="false"
               placeholder="Paste the code from the sign-in page"
               v-tooltip.bottom="'Only needed when signing in from the link'" />
        <button class="cc-btn cc-btn-primary cc-fs-xs" type="submit" :disabled="!code.trim() || sending"
                v-tooltip.bottom="'Send the code to finish logging in'">
          <i :class="['pi', sending ? 'pi-spin pi-spinner' : 'pi-sign-in']" /> Log in
        </button>
      </form>
      <p v-if="codeError" class="cl-bad cc-fs-2xs">{{ codeError }}</p>
    </template>

    <div v-if="run?.state === 'failed'" class="cc-row cc-row-tight">
      <button class="cc-btn cc-btn-ghost cc-fs-xs" @click="start"
              v-tooltip.bottom="'Start the login again'">
        <i class="pi pi-refresh" /> Try again
      </button>
    </div>
  </div>
</template>

<style scoped>
.cl-body { display: flex; flex-direction: column; gap: 0.5rem; }
.cl-status { margin: 0; display: flex; align-items: center; gap: 0.35rem; }
.cl-ok  { color: var(--cc-sev-ok); }
.cl-bad { color: var(--cc-sev-fail); margin: 0; }
.cl-code { flex: 1; min-width: 0; }
</style>
