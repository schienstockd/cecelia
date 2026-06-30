<script setup lang="ts">
import { ref, onMounted } from 'vue'

interface CheckResult {
  current: string
  latest: string | null
  updateAvailable: boolean
  url?: string
  error?: string
}

const current = ref('')
const latest = ref<string | null>(null)
const available = ref(false)
const busy = ref(false)
const message = ref('')   // post-apply confirmation, or an error (e.g. dev-checkout refusal)

async function check() {
  try {
    const res = await fetch('/api/update/check')
    const d: CheckResult = await res.json()
    current.value = d.current
    latest.value = d.latest
    available.value = d.updateAvailable
  } catch {
    /* offline or backend down — stay quiet */
  }
}

async function applyUpdate() {
  if (!latest.value || busy.value) return
  busy.value = true
  message.value = ''
  try {
    const res = await fetch('/api/update/apply', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ version: latest.value }),
    })
    const d = await res.json()
    message.value = res.ok
      ? (d.message ?? `Update ${latest.value} staged — restart Cecelia to finish.`)
      : (d.error ?? 'Update failed.')
    if (res.ok) available.value = false
  } catch {
    message.value = 'Update failed (could not reach the server).'
  } finally {
    busy.value = false
  }
}

onMounted(check)
</script>

<template>
  <span v-if="current" class="upd">
    <button
      v-if="available"
      class="upd-btn"
      :disabled="busy"
      v-tooltip.bottom="`Download ${latest} and stage it; restart Cecelia to finish installing.`"
      @click="applyUpdate"
    >
      <i class="pi pi-download" /> {{ busy ? 'Updating…' : `Update to ${latest}` }}
    </button>
    <span v-else class="upd-ver" v-tooltip.bottom="'You are on the latest version.'">{{ current }}</span>
    <span v-if="message" class="upd-msg">{{ message }}</span>
  </span>
</template>

<style scoped>
.upd { display: flex; align-items: center; gap: 0.5rem; font-size: 0.75rem; white-space: nowrap; }
.upd-ver { color: var(--cc-text-muted, #71717a); }
.upd-btn {
  display: flex;
  align-items: center;
  gap: 0.35rem;
  font-size: 0.75rem;
  font-weight: 600;
  padding: 0.2rem 0.6rem;
  border-radius: 999px;
  cursor: pointer;
  color: #93c5fd;
  background: #1e3a5f55;
  border: 1px solid #3b82f655;
}
.upd-btn:disabled { opacity: 0.6; cursor: default; }
.upd-msg { color: var(--cc-text-muted, #a1a1aa); max-width: 22rem; overflow: hidden; text-overflow: ellipsis; }
</style>
