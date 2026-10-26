<!--
  HpcSettings — the HPC fields, on two surfaces by who they belong to (docs/todo/HPC_PLAN.md Decision 9):
  `scope="machine"` is Settings → HPC: the CLUSTER (scheduler, host, port — custom.toml, shared by every
  profile), THIS PROJECT (an account override) and the cluster environment install. `scope="user"` is
  Preferences → HPC: YOUR login, remote folder, email and job classes (this profile). Each field saves on
  change (blur / Enter), like the other Settings controls, from its own draft (useHpcDrafts); a refused
  value shows the backend's message and snaps back.

  A successful save flashes "Saved" by the Test connection button (useSavedFlash); why the button is
  disabled shows beside it, and Settings carries a small button to Preferences → HPC.

  "Test connection" runs against the SAVED settings. It is the only call that may trust a new host key,
  and it shows the fingerprint (Q10). The number of jobs in flight is the `hpc` pool in PoolThrottle,
  not a field here. Windows is deferred (Q8): the section says so instead of rendering controls.
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted, onUnmounted } from 'vue'
import FileBrowser from './FileBrowser.vue'
import InlineNote from './InlineNote.vue'
import { useHpcDrafts } from '../composables/useHpcDrafts'
import { useSavedFlash } from '../composables/useFlash'
import { fetchHpcSettings, saveHpcSettings, testHpcConnection, hpcTestBlocker, hpcTestLines,
  keyPickerStart, hpcFieldPatch, hpcFieldValue, fetchHpcEnv, installHpcEnv, hpcEnvLine, onHpcEnvChanged,
  type HpcScope, type HpcSettings, type HpcPatch, type HpcTestResult, type HpcClassName, type HpcClassSettings,
  type HpcEnvStatus } from '../utils/hpcSettings'
import { isPreferencesOpen, openPreferences } from '../lib/preferencesOpen'

const props = defineProps<{ projectUid: string; scope: HpcScope }>()
const machine = computed(() => props.scope === 'machine')

const s       = ref<HpcSettings | null>(null)
// One draft per field (`v-model`, committed on `@change`), each re-seeded only when ITS stored value
// changes — a reply to saving one field must not wipe another that is still being typed in.
const d       = useHpcDrafts(() => s.value)
const error   = ref('')
const saving  = ref(false)
const testing = ref(false)
const result  = ref<HpcTestResult | null>(null)
const showKeyBrowser = ref(false)
const { saved, flash: flashSaved } = useSavedFlash()

async function load() {
  try { s.value = await fetchHpcSettings(props.projectUid); error.value = '' }
  catch (e) { error.value = e instanceof Error ? e.message : String(e) }
}
watch(() => props.projectUid, load, { immediate: true })
// The login is edited in Preferences, over this page: re-read when it closes, so the test and the
// environment row see it.
watch(isPreferencesOpen, open => { if (!open && machine.value) void load() })

// The save in flight: leaving a field for Test connection or Install fires both at once, and they
// must see the value just typed, not the one before it.
let pendingSave: Promise<void> = Promise.resolve()
function save(patch: HpcPatch, path = ''): Promise<void> {
  pendingSave = pendingSave.then(() => doSave(patch, path))
  return pendingSave
}
async function doSave(patch: HpcPatch, path: string) {
  saving.value = true
  try {
    s.value = await saveHpcSettings(patch, props.projectUid)
    error.value = ''
    result.value = null          // a passing test no longer describes what is stored
    flashSaved()
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

const blocker = computed(() => hpcTestBlocker(s.value, props.scope))
const schedLabel = computed(() =>
  s.value?.schedulers.find(x => x.name === s.value?.site.scheduler)?.label ?? 'SLURM')
const lines = computed(() => result.value ? hpcTestLines(result.value, schedLabel.value) : [])

async function runTest() {
  testing.value = true
  result.value = null
  await pendingSave
  try { result.value = await testHpcConnection() }
  catch (e) { error.value = e instanceof Error ? e.message : String(e) }
  finally { testing.value = false }
  if (result.value?.ok && machine.value) void loadEnv()
}

// The cluster environment (P2): asked of the cluster once the login is filled in, again after a
// passing test and when the install job ends.
const env = ref<HpcEnvStatus | null>(null)
const envLine = computed(() => hpcEnvLine(env.value))
async function loadEnv() {
  if (blocker.value || !machine.value) return
  env.value = null
  try { env.value = await fetchHpcEnv() }
  catch (e) { env.value = { configured: false, lock: '', installed: false, job: '', installing: false,
                            error: e instanceof Error ? e.message : String(e) } }
}
async function install() {
  await pendingSave              // an Internet queue typed just before the click must reach the job
  try { await installHpcEnv(); env.value = env.value && { ...env.value, installing: true, error: '' } }
  catch (e) { error.value = e instanceof Error ? e.message : String(e) }
}
let offEnv: (() => void) | null = null
onMounted(() => { offEnv = onHpcEnvChanged(() => void loadEnv()) })
onUnmounted(() => offEnv?.())
watch(blocker, b => { if (!b && !env.value) void loadEnv() }, { immediate: true })

const CLASSES: { name: HpcClassName; label: string }[] = [
  { name: 'cpu', label: 'CPU jobs' }, { name: 'gpu', label: 'GPU jobs' },
]
// What this is for, on both surfaces: most people open it once, to segment more or bigger images than
// the workstation can.
const WHAT = 'For segmenting many or large images on your institute\'s cluster'
const WHAT_DETAIL = 'Needs a cluster account and an SSH key; your support team has the host and queue names'

// Grey "e.g." examples (generic, never a real site's values): someone looking at an empty form can
// tell what shape each field wants. Directives are written for the chosen scheduler.
const pbs = computed(() => s.value?.site.scheduler === 'pbs')
const CLASS_FIELDS = computed<{ key: keyof HpcClassSettings; label: string; tip: string; ph: Record<HpcClassName, string>; multi?: boolean }[]>(() => [
  { key: 'queue',      label: 'Queue',      tip: 'Partition (SLURM) or queue (PBS); SLURM takes a comma list; empty = site default',
    ph: { cpu: 'e.g. cpu,long', gpu: 'e.g. gpu-a100' } },
  { key: 'account',    label: 'Account',    tip: 'Project or group jobs are billed to; empty = your default',
    ph: { cpu: 'e.g. proj1234', gpu: 'e.g. proj1234' } },
  { key: 'qos',        label: 'QoS',        tip: 'Quality of service; most sites need none',
    ph: { cpu: 'e.g. normal', gpu: 'e.g. gpu' } },
  { key: 'directives', label: 'Directives', tip: `Extra ${pbs.value ? '#PBS' : '#SBATCH'} lines, used as written, one per line`,
    ph: pbs.value ? { cpu: 'e.g. #PBS -l place=excl', gpu: 'e.g. #PBS -l place=excl' }
                  : { cpu: 'e.g. #SBATCH --constraint=avx2', gpu: 'e.g. #SBATCH --constraint=a100' }, multi: true },
  { key: 'preamble',   label: 'Preamble',   tip: 'Shell lines run on the node before the task',
    ph: { cpu: 'e.g. module purge', gpu: 'e.g. module load cuda/12' }, multi: true },
])
</script>

<template>
  <p v-if="s && !s.available" class="cc-muted cc-fs-md">Not available on Windows yet.</p>

  <div v-else-if="s" class="hpc" :class="{ busy: saving }">
   <template v-if="machine">
    <InlineNote class="cc-fs-sm hpc-what" :short="WHAT" :detail="WHAT_DETAIL" />
    <div class="hpc-grid">
      <label class="cc-muted">Scheduler</label>
      <select v-model="d['site.scheduler'].value" @change="commit('site.scheduler')"
              v-tooltip.bottom="'Batch system on the cluster'">
        <option v-for="o in s.schedulers" :key="o.name" :value="o.name" :disabled="!o.available">
          {{ o.label }}{{ o.available ? '' : ' (coming)' }}
        </option>
      </select>

      <label class="cc-muted">Host</label>
      <input v-model="d['site.host'].value" placeholder="e.g. hpc.university.edu"
             @change="commit('site.host')" v-tooltip.bottom="'Login node address'" />

      <label class="cc-muted">Port</label>
      <input class="hpc-port" type="text" inputmode="numeric" v-model="d['site.port'].value" placeholder="22"
             @change="commit('site.port')" v-tooltip.bottom="'SSH port (usually 22)'" />

      <label class="cc-muted">Internet queue</label>
      <input v-model="d['site.netQueue'].value" placeholder="e.g. copyq; empty = CPU queue"
             @change="commit('site.netQueue')"
             v-tooltip.bottom="'Queue whose nodes can download, for Install on cluster'" />
    </div>

    <template v-if="s.project">
      <h3 class="cc-eyebrow hpc-sub">This project</h3>
      <div class="hpc-grid">
        <label class="cc-muted">Account</label>
        <input v-model="d['project.account'].value" placeholder="e.g. proj1234" @change="commit('project.account')"
               v-tooltip.bottom="'Billing account for this project only; empty = your own'" />
      </div>
    </template>
   </template>

   <template v-else>
    <InlineNote class="cc-fs-sm hpc-cluster" icon="pi-server" detail="Set in Settings → HPC, shared by every profile"
                :short="`Cluster: ${s.site.host ? `${s.site.host}:${s.site.port}` : 'not set'} · ${schedLabel}`" />
    <InlineNote class="cc-fs-sm hpc-what" :short="WHAT" :detail="WHAT_DETAIL" />
    <div class="hpc-grid">
      <label class="cc-muted">Username</label>
      <input v-model="d['identity.user'].value" placeholder="e.g. jsmith" @change="commit('identity.user')"
             v-tooltip.bottom="'Your cluster username, for this profile'" />

      <label class="cc-muted">SSH key</label>
      <div class="cc-row">
        <input class="hpc-grow mono" v-model="d['identity.keyFile'].value" placeholder="~/.ssh/id_ed25519"
               @change="commit('identity.keyFile')"
               v-tooltip.bottom="'Private key; a passphrase key must be in ssh-agent'" />
        <button class="cc-btn cc-btn-ghost cc-btn-icon" @click="showKeyBrowser = true"
                v-tooltip.top="'Browse for the key file'"><i class="pi pi-folder-open" /></button>
      </div>

      <label class="cc-muted">Remote folder</label>
      <input class="mono" v-model="d['site.remoteDir'].value" placeholder="e.g. /scratch/proj1234/jsmith/cecelia"
             @change="commit('site.remoteDir')"
             v-tooltip.bottom="'Where Cecelia keeps your files on the cluster; scratch space, it holds the images'" />

      <label class="cc-muted">Email</label>
      <input v-model="d['site.email'].value" placeholder="optional, e.g. you@university.edu" @change="commit('site.email')"
             v-tooltip.bottom="'Mails you when a job ends or fails; optional'" />
    </div>

    <InlineNote class="cc-fs-sm hpc-ask" short="Ask your cluster's support team for these names"
                detail="Most sites need only a queue. GPU tasks (Cellpose, Coastal) use the GPU column" />
    <div class="hpc-classes">
      <span />
      <h3 v-for="c in CLASSES" :key="c.name" class="cc-eyebrow">{{ c.label }}</h3>
      <template v-for="f in CLASS_FIELDS" :key="f.key">
        <label class="cc-muted">{{ f.label }}</label>
        <template v-for="c in CLASSES" :key="c.name">
          <textarea v-if="f.multi" class="cc-input-xs mono" rows="2" v-model="d[classPath(c.name, f.key)].value"
                    :placeholder="f.ph[c.name]" @change="commit(classPath(c.name, f.key))" v-tooltip.bottom="f.tip" />
          <input v-else v-model="d[classPath(c.name, f.key)].value" :placeholder="f.ph[c.name]" @change="commit(classPath(c.name, f.key))"
                 v-tooltip.bottom="f.tip" />
        </template>
      </template>
    </div>
   </template>

    <div class="cc-row hpc-test">
      <button class="cc-btn cc-btn-primary" :disabled="testing || !!blocker" @click="runTest"
              v-tooltip.top="'Connect over SSH and check the folder and scheduler'">
        <i :class="['pi', testing ? 'pi-spin pi-spinner' : 'pi-bolt']" /> Test connection
      </button>
      <InlineNote v-if="blocker" class="cc-fs-sm" :short="blocker" />
      <button v-if="machine" class="cc-btn cc-btn-ghost cc-fs-xs" @click="openPreferences('hpc')"
              v-tooltip.top="'Your login, folder and queues, in Preferences'">
        <i class="pi pi-user" /> Your HPC settings
      </button>
      <InlineNote v-if="saved" class="cc-fs-sm" severity="ok" short="Saved" />
    </div>
    <div v-if="lines.length" class="hpc-result cc-fs-sm">
      <InlineNote v-for="(l, i) in lines" :key="i" :short="l.text" :severity="l.ok ? 'ok' : 'fail'" />
    </div>

    <template v-if="machine && !blocker">
      <h3 class="cc-eyebrow hpc-sub">Cluster environment</h3>
      <div class="cc-row hpc-env">
        <InlineNote class="cc-fs-sm hpc-grow" :short="envLine.text" :severity="envLine.severity" />
        <button class="cc-btn cc-btn-ghost cc-btn-icon" :disabled="!env" @click="loadEnv"
                v-tooltip.top="'Check again'"><i class="pi pi-refresh" /></button>
        <button v-if="envLine.canInstall" class="cc-btn cc-btn-primary" @click="install"
                v-tooltip.top="'Install Cecelia\'s Python environment on the cluster, as a job'">
          <i class="pi pi-download" /> Install on cluster
        </button>
      </div>
    </template>
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
.hpc-classes { grid-template-columns: 7.5rem 1fr 1fr; margin-top: 0.5rem; }
.hpc-ask { display: flex; margin-top: 1.1rem; }
.hpc-classes textarea { resize: vertical; min-width: 0; }
.hpc-classes input { min-width: 0; }
.hpc-sub { margin: 1.1rem 0 0.5rem; }
.hpc-first { margin-top: 0; }
.hpc-cluster { display: flex; margin: 0 0 0.8rem; }
.hpc-what { display: flex; margin: 0 0 0.8rem; }
.hpc-grow { flex: 1; min-width: 0; }
/* content-box: 5ch is the text width (a 5-digit port); the global input padding sits outside it */
.hpc-port { box-sizing: content-box; width: 5ch; }
.mono { font-family: var(--cc-mono); }
.hpc-test { margin-top: 1.1rem; gap: 0.5rem; flex-wrap: wrap; }
.hpc-env { gap: 0.5rem; }
.hpc-result { display: flex; flex-direction: column; gap: 0.15rem; margin-top: 0.5rem; word-break: break-all; }
</style>
