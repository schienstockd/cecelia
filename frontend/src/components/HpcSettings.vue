<!--
  HpcSettings — the body of Settings → HPC (docs/todo/HPC_PLAN.md P1, Decision 9). Three scopes on one
  form, each stored by the backend where it belongs: the SITE (custom.toml [hpc], machine-wide), YOUR
  LOGIN (username + SSH key, per profile) and THIS PROJECT (an account override). Each field saves on
  change (blur / Enter), like the other Settings controls, from its own draft (useHpcDrafts); a refused
  value shows the backend's message and snaps back.

  "Test connection" runs against the SAVED settings. It is the only call that may trust a new host key,
  and it shows the fingerprint (Q10). The number of jobs in flight is the `hpc` pool in PoolThrottle,
  not a field here. Windows is deferred (Q8): the section says so instead of rendering controls.
-->
<script setup lang="ts">
import { ref, computed, watch } from 'vue'
import FileBrowser from './FileBrowser.vue'
import InlineNote from './InlineNote.vue'
import { useHpcDrafts } from '../composables/useHpcDrafts'
import { fetchHpcSettings, saveHpcSettings, testHpcConnection, hpcTestBlocker, hpcTestLines,
  keyPickerStart, hpcFieldPatch, hpcFieldValue, type HpcSettings, type HpcPatch, type HpcTestResult,
  type HpcClassName, type HpcClassSettings } from '../utils/hpcSettings'

const props = defineProps<{ projectUid: string }>()

const s       = ref<HpcSettings | null>(null)
// One draft per field (`v-model`, committed on `@change`), each re-seeded only when ITS stored value
// changes — a reply to saving one field must not wipe another that is still being typed in.
const d       = useHpcDrafts(() => s.value)
const error   = ref('')
const saving  = ref(false)
const testing = ref(false)
const result  = ref<HpcTestResult | null>(null)
const showKeyBrowser = ref(false)

async function load() {
  try { s.value = await fetchHpcSettings(props.projectUid); error.value = '' }
  catch (e) { error.value = e instanceof Error ? e.message : String(e) }
}
watch(() => props.projectUid, load, { immediate: true })

async function save(patch: HpcPatch, path = '') {
  saving.value = true
  try {
    s.value = await saveHpcSettings(patch, props.projectUid)
    error.value = ''
    result.value = null          // a passing test no longer describes what is stored
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
    await load()
    // A refused value snaps back: its stored value did not change, so its draft would not re-seed.
    if (path) d[path].value = hpcFieldValue(s.value, path)
  } finally { saving.value = false }
}

const commit = (path: string) => save(hpcFieldPatch(path, d[path].value, props.projectUid), path)
const classPath = (cls: HpcClassName, key: keyof HpcClassSettings) => `site.${cls}.${key}`
function pickKey(paths: string[]) {
  showKeyBrowser.value = false
  if (paths[0]) void save({ identity: { keyFile: paths[0] } }, 'identity.keyFile')
}

const blocker = computed(() => hpcTestBlocker(s.value))
const schedLabel = computed(() =>
  s.value?.schedulers.find(x => x.name === s.value?.site.scheduler)?.label ?? 'SLURM')
const lines = computed(() => result.value ? hpcTestLines(result.value, schedLabel.value) : [])

async function runTest() {
  testing.value = true
  result.value = null
  try { result.value = await testHpcConnection() }
  catch (e) { error.value = e instanceof Error ? e.message : String(e) }
  finally { testing.value = false }
}

const CLASSES: { name: HpcClassName; label: string }[] = [
  { name: 'cpu', label: 'CPU jobs' }, { name: 'gpu', label: 'GPU jobs' },
]
const CLASS_FIELDS: { key: keyof HpcClassSettings; label: string; tip: string; multi?: boolean }[] = [
  { key: 'queue',      label: 'Queue',      tip: 'Partition (SLURM) or queue (PBS); empty = site default' },
  { key: 'account',    label: 'Account',    tip: 'Account jobs are billed to; empty = your default' },
  { key: 'qos',        label: 'QoS',        tip: 'Quality of service; empty = none' },
  { key: 'directives', label: 'Directives', tip: 'Extra scheduler lines, one per line', multi: true },
  { key: 'preamble',   label: 'Preamble',   tip: 'Shell lines run before the task, e.g. module load', multi: true },
]
</script>

<template>
  <p v-if="s && !s.available" class="cc-muted cc-fs-md">Not available on Windows yet.</p>

  <div v-else-if="s" class="hpc" :class="{ busy: saving }">
    <div class="hpc-grid">
      <label class="cc-muted">Scheduler</label>
      <select v-model="d['site.scheduler'].value" @change="commit('site.scheduler')"
              v-tooltip.bottom="'Batch system on the cluster'">
        <option v-for="o in s.schedulers" :key="o.name" :value="o.name" :disabled="!o.available">
          {{ o.label }}{{ o.available ? '' : ' (coming)' }}
        </option>
      </select>

      <label class="cc-muted">Host</label>
      <input v-model="d['site.host'].value" placeholder="login.cluster.org"
             @change="commit('site.host')" v-tooltip.bottom="'Login node address'" />

      <label class="cc-muted">Port</label>
      <input class="hpc-port" type="text" inputmode="numeric" v-model="d['site.port'].value"
             @change="commit('site.port')" v-tooltip.bottom="'SSH port (usually 22)'" />

      <label class="cc-muted">Remote folder</label>
      <input class="mono" v-model="d['site.remoteDir'].value" @change="commit('site.remoteDir')"
             v-tooltip.bottom="'Where Cecelia keeps its files on the cluster'" />

      <label class="cc-muted">Email</label>
      <input v-model="d['site.email'].value" placeholder="optional" @change="commit('site.email')"
             v-tooltip.bottom="'Job notifications; optional'" />
    </div>

    <h3 class="cc-eyebrow hpc-sub">Your login</h3>
    <div class="hpc-grid">
      <label class="cc-muted">Username</label>
      <input v-model="d['identity.user'].value" @change="commit('identity.user')"
             v-tooltip.bottom="'Your cluster username, for this profile'" />

      <label class="cc-muted">SSH key</label>
      <div class="cc-row">
        <input class="hpc-grow mono" v-model="d['identity.keyFile'].value" placeholder="~/.ssh/id_ed25519"
               @change="commit('identity.keyFile')"
               v-tooltip.bottom="'Private key; a passphrase key must be in ssh-agent'" />
        <button class="cc-btn cc-btn-ghost cc-btn-icon" @click="showKeyBrowser = true"
                v-tooltip.top="'Browse for the key file'"><i class="pi pi-folder-open" /></button>
      </div>
    </div>

    <template v-if="s.project">
      <h3 class="cc-eyebrow hpc-sub">This project</h3>
      <div class="hpc-grid">
        <label class="cc-muted">Account</label>
        <input v-model="d['project.account'].value" placeholder="site account" @change="commit('project.account')"
               v-tooltip.bottom="'Billing account for this project only'" />
      </div>
    </template>

    <div class="hpc-classes">
      <span />
      <h3 v-for="c in CLASSES" :key="c.name" class="cc-eyebrow">{{ c.label }}</h3>
      <template v-for="f in CLASS_FIELDS" :key="f.key">
        <label class="cc-muted">{{ f.label }}</label>
        <template v-for="c in CLASSES" :key="c.name">
          <textarea v-if="f.multi" class="cc-input-xs mono" rows="2" v-model="d[classPath(c.name, f.key)].value"
                    @change="commit(classPath(c.name, f.key))" v-tooltip.bottom="f.tip" />
          <input v-else v-model="d[classPath(c.name, f.key)].value" @change="commit(classPath(c.name, f.key))"
                 v-tooltip.bottom="f.tip" />
        </template>
      </template>
    </div>

    <div class="cc-row hpc-test">
      <button class="cc-btn cc-btn-primary" :disabled="testing || !!blocker" @click="runTest"
              v-tooltip.top="blocker || 'Connect over SSH and check the folder and scheduler'">
        <i :class="['pi', testing ? 'pi-spin pi-spinner' : 'pi-bolt']" /> Test connection
      </button>
    </div>
    <div v-if="lines.length" class="hpc-result cc-fs-sm">
      <InlineNote v-for="(l, i) in lines" :key="i" :short="l.text" :severity="l.ok ? 'ok' : 'fail'" />
    </div>
  </div>

  <p v-if="error" class="cc-muted-error">{{ error }}</p>

  <Teleport to="body">
    <FileBrowser v-if="showKeyBrowser" mode="file" title="Select your SSH private key"
                 :start-path="keyPickerStart(s?.identity.keyFile ?? '')"
                 @select="pickKey" @close="showKeyBrowser = false" />
  </Teleport>
</template>

<style scoped>
.hpc.busy { opacity: 0.7; }
.hpc-grid, .hpc-classes {
  display: grid;
  grid-template-columns: 7.5rem 1fr;
  gap: 0.4rem 0.6rem;
  align-items: center;
}
.hpc-classes { grid-template-columns: 7.5rem 1fr 1fr; margin-top: 1.1rem; }
.hpc-classes textarea { resize: vertical; min-width: 0; }
.hpc-classes input { min-width: 0; }
.hpc-sub { margin: 1.1rem 0 0.5rem; }
.hpc-grow { flex: 1; min-width: 0; }
.hpc-port { width: 8ch; }
.mono { font-family: var(--cc-mono); }
.hpc-test { margin-top: 1.1rem; }
.hpc-result { display: flex; flex-direction: column; gap: 0.15rem; margin-top: 0.5rem; word-break: break-all; }
</style>
