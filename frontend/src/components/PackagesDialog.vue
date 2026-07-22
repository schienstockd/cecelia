<!--
  Installed-package inventory — a read-only modal opened from Settings ▸ Diagnostics. Shows BOTH
  ecosystems because they are provisioned separately and neither tool sees the other: the Python
  analysis env (conda + pypi) from `pixi list`, and the Julia packages from the server's own
  Pkg.dependencies() (docs/SHIPPING.md). Backs bug reports — "Copy" yields a plain-text manifest.
  Built on the shared BaseModal shell (docs/UI.md → "Modals & dialogs").
-->
<script setup lang="ts">
import { ref, computed, onMounted } from 'vue'
import BaseModal from './BaseModal.vue'

const emit = defineEmits<{ (e: 'close'): void }>()

interface JlPkg { name: string; version: string }
interface PyPkg { name: string; version: string; kind: string; explicit: boolean }
interface PkgResp { julia: JlPkg[]; python: PyPkg[]; pythonError: string | null }

const loading = ref(true)
const error   = ref<string | null>(null)
const data    = ref<PkgResp | null>(null)
const q       = ref('')
const copied  = ref(false)

async function load() {
  loading.value = true; error.value = null
  try {
    const r = await fetch('/api/diagnostics/packages')
    if (!r.ok) throw new Error(`HTTP ${r.status}`)
    data.value = await r.json() as PkgResp
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  } finally {
    loading.value = false
  }
}
onMounted(load)

const match = (name: string) => name.toLowerCase().includes(q.value.trim().toLowerCase())
const py = computed(() => (data.value?.python ?? []).filter(p => match(p.name)))
const jl = computed(() => (data.value?.julia  ?? []).filter(p => match(p.name)))

function copyAll() {
  const d = data.value
  if (!d) return
  const lines = [
    '# Python (pixi: conda + pypi)',
    ...d.python.map(p => `${p.name}\t${p.version}\t${p.kind}`),
    '',
    '# Julia',
    ...d.julia.map(p => `${p.name}\t${p.version}`),
  ]
  navigator.clipboard.writeText(lines.join('\n'))
  copied.value = true
  setTimeout(() => { copied.value = false }, 1500)
}
</script>

<template>
  <BaseModal title="Installed packages" icon="pi-box" width="560px" height="76vh" @close="emit('close')">
    <template #toolbar>
      <div class="pk-toolbar">
        <span class="search-wrap">
          <i class="pi pi-search" />
          <input class="search-input" v-model="q" placeholder="Filter by name…" />
        </span>
        <button class="mini-btn" :disabled="!data" @click="copyAll"
          v-tooltip.bottom="'Copy the full list as text (for bug reports)'">
          <i :class="['pi', copied ? 'pi-check' : 'pi-copy']" /> {{ copied ? 'Copied' : 'Copy' }}
        </button>
      </div>
    </template>

    <div class="pk-body">
      <p v-if="loading" class="state-line"><i class="pi pi-spin pi-cog" /> Reading packages…</p>
      <p v-else-if="error" class="state-line err"><i class="pi pi-times-circle" /> {{ error }}</p>

      <template v-else-if="data">
        <!-- Python env (pixi: conda + pypi) -->
        <div class="grp-head">
          Python env <span class="grp-sub">pixi · conda + pypi</span>
          <span class="grp-count">{{ py.length }}</span>
        </div>
        <p v-if="data.pythonError" class="state-line warn">
          <i class="pi pi-exclamation-triangle" /> {{ data.pythonError }}
        </p>
        <div v-else class="pkg-grid">
          <template v-for="p in py" :key="'py-' + p.name">
            <span class="pk-name">{{ p.name }}</span>
            <span class="pk-ver mono">{{ p.version }}</span>
            <span class="pk-kind" :class="p.kind">{{ p.kind }}</span>
          </template>
          <p v-if="!py.length" class="state-line dim">No matches.</p>
        </div>

        <!-- Julia (juliaup + Manifest — outside pixi) -->
        <div class="grp-head">
          Julia <span class="grp-sub">juliaup · Manifest</span>
          <span class="grp-count">{{ jl.length }}</span>
        </div>
        <div class="pkg-grid">
          <template v-for="p in jl" :key="'jl-' + p.name">
            <span class="pk-name">{{ p.name }}</span>
            <span class="pk-ver mono">{{ p.version }}</span>
            <span class="pk-kind" />
          </template>
          <p v-if="!jl.length" class="state-line dim">No matches.</p>
        </div>
      </template>
    </div>
  </BaseModal>
</template>

<style scoped>
.pk-toolbar { display: flex; gap: 0.5rem; align-items: center; padding: 0.6rem 1rem; border-bottom: 1px solid var(--cc-border); flex-shrink: 0; }
.search-wrap { flex: 1; display: flex; align-items: center; gap: 0.4rem; background: var(--cc-surface-2); border: 1px solid var(--cc-border); border-radius: 0.35rem; padding: 0.25rem 0.5rem; }
.search-wrap .pi-search { font-size: 0.75rem; color: var(--cc-text-dim); }
.search-input { flex: 1; background: none; border: none; outline: none; color: var(--cc-text); font-size: 0.8rem; }
.mini-btn { display: flex; align-items: center; gap: 0.3rem; font-size: 0.75rem; padding: 0.3rem 0.6rem; border-radius: 0.35rem; border: 1px solid var(--cc-border); background: var(--cc-surface-2); color: var(--cc-text-dim); cursor: pointer; white-space: nowrap; }
.mini-btn:hover:not(:disabled) { color: var(--cc-text); }
.mini-btn:disabled { opacity: 0.4; cursor: not-allowed; }

.grp-head { display: flex; align-items: baseline; gap: 0.5rem; font-size: 0.8rem; font-weight: 600; color: var(--cc-text); margin: 0.85rem 0 0.4rem; position: sticky; top: 0; background: var(--cc-surface-1); padding: 0.2rem 0; }
.grp-head:first-child { margin-top: 0.2rem; }
.grp-sub { font-size: 0.68rem; font-weight: 400; color: var(--cc-text-dim); }
.grp-count { margin-left: auto; font-size: 0.7rem; color: var(--cc-text-dim); background: var(--cc-surface-2); border-radius: 999px; padding: 0.05rem 0.5rem; }

.pkg-grid { display: grid; grid-template-columns: 1fr auto auto; gap: 0.1rem 0.75rem; align-items: baseline; }
.pk-name { font-size: 0.78rem; color: var(--cc-text); overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.pk-ver { font-size: 0.74rem; color: var(--cc-text-dim); }
.pk-kind { font-size: 0.62rem; text-transform: uppercase; letter-spacing: 0.03em; color: var(--cc-text-dim); min-width: 3.2rem; }
.pk-kind.conda { color: #34d399; }
.pk-kind.pypi  { color: #60a5fa; }

.mono { font-variant-numeric: tabular-nums; }
.state-line { display: flex; align-items: center; gap: 0.4rem; font-size: 0.78rem; color: var(--cc-text-dim); margin: 0.4rem 0; }
.state-line.err  { color: #f87171; }
.state-line.warn { color: #fbbf24; }
.state-line.dim  { grid-column: 1 / -1; }
</style>
