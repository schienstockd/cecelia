<!--
  New user-profile — the create-flow dialog opened from the launch picker (`+ New profile`) and
  from the Preferences modal / project-panel entry points (USER_PROFILE_PLAN Phase 6 moved this
  file out of `components/kiwi/` so it is no longer tied to the Kiwi cockpit's profile row).
  Underlying primitive: LOGIN_CREDENTIAL_ISOLATION_PLAN P3 (backend) + P6 (terminal one-liner).

  Three server calls chained in one modal so the profile is FULLY ready after Close:
    1. POST /api/kiwi/profiles/create      — mkpath under user-profiles/<name>/ (URL contract
                                             preserved; the on-disk name stays user-profiles per
                                             USER_PROFILE_PLAN Decision 7)
    2. POST /api/kiwi/profiles/select      — set [ai].profile = <name>
    3. POST /api/observer/register         — write the observer MCP entry into the new profile's
                                             .claude.json (routed via _apply_claude_env). Without
                                             step 3, a raw `claude` in the profile's shell would
                                             have no MCP tools → wouldn't pair back to Cecelia.
                                             Best-effort: a create+select stays even if it fails
                                             so the profile isn't lost, and the failure is
                                             surfaced inline.

  The one-liner Cecelia hands back launches `claude` directly. First-time login: `claude` itself
  prompts `/login` on first run inside a fresh CLAUDE_CONFIG_DIR.

  Built on the shared BaseModal shell (docs/UI.md → "Modals & dialogs").
-->
<script setup lang="ts">
import { ref, computed, nextTick, onMounted, useTemplateRef } from 'vue'
import BaseModal from '../BaseModal.vue'
import { useCopyFlash } from '../../composables/useCopyFlash'
import { createProfile, selectProfile,
         isValidProfileName } from '../../utils/profileApi'
// serviceApi.observerApi lives under `../` from the previous location; unchanged now.
import { observerApi } from '../../utils/serviceApi'

const emit = defineEmits<{
  (e: 'close'): void
  (e: 'created', name: string): void
}>()

const name = ref('')
const submitting = ref(false)
const errorMsg = ref<string | null>(null)
const created = ref<{ name: string; command: string; mcpReady: boolean } | null>(null)

const nameInput = useTemplateRef<HTMLInputElement>('nameInput')
onMounted(() => { void nextTick(() => nameInput.value?.focus()) })

const nameValid = computed(() => isValidProfileName(name.value.trim()))
const canSubmit = computed(() =>
  !submitting.value && nameValid.value && created.value === null)

const { isCopied: cmdCopied, copy: copyCmd } = useCopyFlash()

async function onSubmit() {
  const trimmed = name.value.trim()
  if (!canSubmit.value) return
  submitting.value = true
  errorMsg.value = null
  try {
    const c = await createProfile(trimmed)
    if (!c.ok || !c.terminalCommand || !c.name) {
      errorMsg.value = c.error ?? 'Create failed.'
      return
    }
    // Auto-select — the whole point of creating one is to use it. A failed select is not fatal:
    // the profile still exists on disk; the parent picker will show it and the user can select
    // manually. Surface the reason without discarding the terminal command.
    const s = await selectProfile(c.name)
    if (!s.ok) errorMsg.value = `Created, but couldn't select: ${s.error ?? 'unknown'}`

    // Register the observer MCP into the new profile's .claude.json. The backend routes this via
    // _apply_claude_env, which reads the active profile — so this write lands under user-profiles/
    // <name>/, not ~/.claude.json. Without it, a raw `claude` in the profile's shell has no MCP
    // tools and won't pair back to Cecelia. Best-effort: any failure is surfaced but does NOT
    // roll back create/select.
    let mcpReady = false
    try {
      const r = await observerApi.register()
      mcpReady = !!(r && (r as { ok?: boolean }).ok !== false)
      if (!mcpReady) {
        const rmsg = (r as { message?: string; error?: string })
        errorMsg.value = `Created, but MCP registration failed: ${rmsg?.error ?? rmsg?.message ?? 'unknown'}`
      }
    } catch (e) {
      errorMsg.value = `Created, but MCP registration failed: ${e instanceof Error ? e.message : 'network error'}`
    }
    created.value = { name: c.name, command: c.terminalCommand, mcpReady }
    emit('created', c.name)
  } finally { submitting.value = false }
}

function onDone() { emit('close') }
</script>

<template>
  <BaseModal title="New profile" icon="pi-user-plus" width="480px" @close="emit('close')">
    <div class="kp-body">
      <template v-if="!created">
        <p class="cc-fs-xs cc-muted kp-tip">
          A separate credential + MCP scope for this seat login. You'll log in
          <code>claude</code> once inside the profile's terminal.
        </p>
        <form class="kp-form" @submit.prevent="onSubmit">
          <label class="kp-label cc-eyebrow cc-fs-2xs" for="kp-name">Name</label>
          <input id="kp-name" ref="nameInput" class="kp-input cc-input-xs"
                 v-model="name" autocomplete="off" spellcheck="false"
                 placeholder="alice"
                 v-tooltip.bottom="'Lower-ASCII alnum + `-` / `_`, 1-32 chars'" />
          <p class="kp-hint cc-fs-2xs"
             :class="{ 'kp-hint-bad': name.length > 0 && !nameValid }">
            1-32 chars, lower-ASCII alnum + <code>-</code> / <code>_</code>.
            <code>default</code>, <code>legacy</code> and <code>peanut</code> are reserved.
          </p>
        </form>
        <p v-if="errorMsg" class="kp-err cc-fs-xs">
          <i class="pi pi-times-circle" /> {{ errorMsg }}
        </p>
      </template>
      <template v-else>
        <p class="kp-ok cc-fs-xs">
          <i class="pi pi-check-circle" /> Created <code>{{ created.name }}</code> — now the
          active profile{{ created.mcpReady ? ', MCP tools registered' : '' }}.
        </p>
        <p class="cc-fs-xs cc-muted kp-tip">
          Paste this in a terminal to launch <code>claude</code> in this profile — on first run
          it prompts <code>/login</code>. Anything Kiwi spawns from now on uses these credentials.
        </p>
        <div class="kp-cmd">
          <code class="kp-cmd-text cc-fs-2xs">{{ created.command }}</code>
          <button class="cc-btn cc-btn-ghost cc-btn-icon"
                  @click="copyCmd(created.command)"
                  v-tooltip.left="cmdCopied() ? 'Copied' : 'Copy the one-liner'">
            <i :class="['pi', cmdCopied() ? 'pi-check' : 'pi-copy']" />
          </button>
        </div>
        <p v-if="errorMsg" class="kp-err cc-fs-xs">
          <i class="pi pi-exclamation-triangle" /> {{ errorMsg }}
        </p>
      </template>
    </div>
    <template #footer>
      <div class="kp-footer">
        <template v-if="!created">
          <button class="cc-btn cc-btn-ghost cc-fs-xs" @click="emit('close')">Cancel</button>
          <button class="cc-btn cc-btn-primary cc-fs-xs"
                  :disabled="!canSubmit" @click="onSubmit"
                  v-tooltip.top="submitting ? 'Creating…' : nameValid ? 'Create + select' : 'Enter a valid name'">
            <i :class="['pi', submitting ? 'pi-spin pi-spinner' : 'pi-plus']" />
            {{ submitting ? 'Creating…' : 'Create' }}
          </button>
        </template>
        <template v-else>
          <button class="cc-btn cc-btn-primary cc-fs-xs" @click="onDone">Done</button>
        </template>
      </div>
    </template>
  </BaseModal>
</template>

<style scoped>
.kp-body { padding: 0.75rem 0.9rem; display: flex; flex-direction: column; gap: 0.6rem; }
.kp-tip { margin: 0; }
.kp-form { display: flex; flex-direction: column; gap: 0.3rem; }
.kp-input { width: 100%; }
.kp-hint { color: var(--cc-text-dim); margin: 0; }
.kp-hint-bad { color: var(--cc-sev-fail); }
.kp-err  { color: var(--cc-sev-fail); margin: 0; display: flex; align-items: center; gap: 0.35rem; }
.kp-ok   { color: var(--cc-sev-ok);   margin: 0; display: flex; align-items: center; gap: 0.35rem; }
.kp-cmd  {
  display: flex; align-items: center; gap: 0.4rem;
  padding: 0.4rem 0.5rem;
  background: var(--cc-surface-2);
  border-radius: var(--cc-radius-xs);
  font-family: var(--cc-mono);
}
.kp-cmd-text { flex: 1; overflow-x: auto; white-space: nowrap; }
.kp-footer   { display: flex; justify-content: flex-end; gap: 0.4rem;
               padding: 0.5rem 0.8rem; }
</style>
