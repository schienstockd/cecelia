<!--
  New Kiwi profile — the create-flow dialog opened from KiwiCockpit's profile row (`+` button).
  LOGIN_CREDENTIAL_ISOLATION_PLAN P3 (frontend) + P6 (terminal one-liner).

  Two-step flow in one modal so the user never loses the terminal command:
    1. Name field (client-validated against the same rule as the backend). Submit → POST /create.
    2. Backend returns the terminal one-liner + we auto-select the new profile server-side.
       The dialog re-renders with the command as a copyable block + a one-line "now run
       `claude login` inside it" instruction. Close ends the flow.

  Kiwi ratchet (kiwiNamingRatchet.test.ts) bans the literal "Claude" in this directory. The
  instruction text uses the lower-case CLI form `claude login`, which is the actual command and
  not covered by the ratchet. Any capital-C mention would need a `// ratchet: cite-technical`.

  Built on the shared BaseModal shell (docs/UI.md → "Modals & dialogs").
-->
<script setup lang="ts">
import { ref, computed, nextTick, onMounted, useTemplateRef } from 'vue'
import BaseModal from '../BaseModal.vue'
import { useCopyFlash } from '../../composables/useCopyFlash'
import { createKiwiProfile, selectKiwiProfile,
         isValidKiwiProfileName } from '../../utils/kiwiProfileApi'

const emit = defineEmits<{
  (e: 'close'): void
  (e: 'created', name: string): void
}>()

const name = ref('')
const submitting = ref(false)
const errorMsg = ref<string | null>(null)
const created = ref<{ name: string; command: string } | null>(null)

const nameInput = useTemplateRef<HTMLInputElement>('nameInput')
onMounted(() => { void nextTick(() => nameInput.value?.focus()) })

const nameValid = computed(() => isValidKiwiProfileName(name.value.trim()))
const canSubmit = computed(() =>
  !submitting.value && nameValid.value && created.value === null)

const { isCopied: cmdCopied, copy: copyCmd } = useCopyFlash()

async function onSubmit() {
  const trimmed = name.value.trim()
  if (!canSubmit.value) return
  submitting.value = true
  errorMsg.value = null
  try {
    const c = await createKiwiProfile(trimmed)
    if (!c.ok || !c.terminalCommand || !c.name) {
      errorMsg.value = c.error ?? 'Create failed.'
      return
    }
    // Auto-select — the whole point of creating one is to use it. A failed select is not fatal:
    // the profile still exists on disk; the parent picker will show it and the user can select
    // manually. Surface the reason without discarding the terminal command.
    const s = await selectKiwiProfile(c.name)
    if (!s.ok) errorMsg.value = `Created, but couldn't select: ${s.error ?? 'unknown'}`
    created.value = { name: c.name, command: c.terminalCommand }
    emit('created', c.name)
  } finally { submitting.value = false }
}

function onDone() { emit('close') }
</script>

<template>
  <BaseModal title="New Kiwi profile" icon="pi-user-plus" width="480px" @close="emit('close')">
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
            <code>default</code> and <code>legacy</code> are reserved.
          </p>
        </form>
        <p v-if="errorMsg" class="kp-err cc-fs-xs">
          <i class="pi pi-times-circle" /> {{ errorMsg }}
        </p>
      </template>
      <template v-else>
        <p class="kp-ok cc-fs-xs">
          <i class="pi pi-check-circle" /> Created <code>{{ created.name }}</code> — now the
          active profile.
        </p>
        <p class="cc-fs-xs cc-muted kp-tip">
          Run this in a terminal, then <code>claude login</code> inside it. Anything Kiwi spawns
          from now on uses this profile's credentials.
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
