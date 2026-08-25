<!--
  Settings → Diagnostics → WebGPU.

  Click "Run WebGPU check" to detect the adapter, its limits and required format support, and report a
  one-line verdict. Non-blocking either way — some machines legitimately only have integrated GPU and
  can still use the app; the OS-specific guidance is collapsed by default.

  The live "mini glxgears" scene from the archived brief is not shipped here — it froze the browser
  under test on 2026-08-25 and shipping a hang is worse than shipping nothing. Reinstate only after the
  freeze is understood.

  Adapter probe lives in `utils/webgpuProbe.ts`; brief was docs/archive/gpu-diagnostic-prompt.md.
-->
<script setup lang="ts">
import { computed, ref } from 'vue'
import CollapsibleSection from './CollapsibleSection.vue'
import { probeWebGpu, adapterNameText, type GpuProbeReport } from '../utils/webgpuProbe'
import { SEVERITY, type Severity } from '../lib/severity'

const report  = ref<GpuProbeReport | null>(null)
const probing = ref(false)

const severity = computed<Severity>(() => {
  switch (report.value?.verdict) {
    case 'ready':    return 'ok'
    case 'reduced':  return 'warn'
    default:         return 'fail'
  }
})
const sevStyle = computed(() => SEVERITY[severity.value])
const adapterText = computed(() => report.value ? adapterNameText(report.value.name) : '')

async function runProbe() {
  probing.value = true
  try { report.value = await probeWebGpu() }
  finally { probing.value = false }
}

const os = /Mac/.test(navigator.platform) ? 'mac'
         : /Win/.test(navigator.platform) ? 'win'
         : 'linux'

function fmt(n: number | undefined | null): string {
  if (n == null) return '—'
  if (n >= 1e9) return `${(n / 1e9).toFixed(1)} GB`
  if (n >= 1e6) return `${(n / 1e6).toFixed(0)} MB`
  if (n >= 1e3) return `${(n / 1e3).toFixed(0)} KB`
  return String(n)
}
</script>

<template>
  <div class="gpu-diag">
    <div v-if="!report" class="verdict-row">
      <button class="cc-btn cc-btn-primary"
              :disabled="probing" @click="runProbe"
              v-tooltip.right="'Check which GPU the browser is using'">
        <i :class="['pi', probing ? 'pi-spin pi-spinner' : 'pi-play']" /> Run WebGPU check
      </button>
    </div>

    <template v-if="report">
      <div class="verdict-row">
        <i :class="['pi', sevStyle.icon, 'verdict-icon']" :style="{ color: sevStyle.color }" />
        <span class="verdict-text">{{ report.reason }}</span>
        <button class="cc-btn cc-btn-ghost"
                :disabled="probing" @click="runProbe"
                v-tooltip.left="'Re-check'">
          <i :class="['pi', probing ? 'pi-spin pi-spinner' : 'pi-refresh']" /> Refresh
        </button>
      </div>

      <div v-if="report.limits" class="kv">
        <!-- The adapter's own name first, where it gives one. The row below is a proxy off a texture
             limit, and on Linux it is wrong: Mesa's `iris` reports 16384 for Intel integrated, so it
             read "Discrete" on a laptop running the iGPU right up until Mesa segfaulted the browser
             (Dominik, 2026-08-25). -->
        <template v-if="adapterText">
          <span>Adapter</span>
          <span>{{ adapterText }}</span>
        </template>

        <span>GPU type</span>
        <span>{{ report.looksDiscrete ? 'Discrete' : 'Integrated' }}<template
          v-if="!adapterText"> (from limits — the browser gives no adapter name)</template></span>

        <span>Ready to render the volume viewer</span>
        <span v-if="report.hasR16Uint === true">Yes</span>
        <span v-else-if="report.hasR16Uint === false" :style="{ color: SEVERITY.fail.color }">
          No — required texture format refused
        </span>
        <span v-else>—</span>
      </div>

      <CollapsibleSection v-if="report.verdict !== 'ready'"
          label="How to route the browser to the discrete GPU"
          :default-open="false"
          storage-key="gpuDiag.osGuide"
          max-height="none"
          tip="OS-specific steps to fix the browser routing">
        <div class="os-guide cc-muted cc-fs-xs">
          <template v-if="os === 'win'">
            Settings → System → Display → Graphics — set the browser to <em>High performance</em>.
            Check your laptop vendor's utility (NVIDIA app, Lenovo Vantage, Dell / HP equivalents)
            and the BIOS graphics-switching mode.
          </template>
          <template v-else-if="os === 'mac'">
            Apple Silicon has no discrete / integrated split — nothing to change. Intel Macs: Battery
            settings → uncheck <em>Automatic graphics switching</em> to force discrete.
          </template>
          <template v-else>
            Distro-dependent: <code>prime-select nvidia</code> (Ubuntu with prime-select) or
            <code>optimus-manager --switch nvidia</code> (Optimus setups) route the browser to the
            discrete card. Reboot after switching.
          </template>
        </div>
      </CollapsibleSection>

      <CollapsibleSection v-if="report.limits"
          label="Raw details"
          :default-open="false"
          storage-key="gpuDiag.rawDetails"
          max-height="none"
          tip="Adapter limits and features as reported">
        <div class="kv kv-mono">
          <span>maxTextureDimension3D</span>
          <span>{{ report.limits.maxTextureDimension3D }}</span>
          <span>maxBufferSize</span>
          <span>{{ fmt(report.limits.maxBufferSize) }}</span>
          <span>maxStorageBufferBindingSize</span>
          <span>{{ fmt(report.limits.maxStorageBufferBindingSize) }}</span>
          <span>r16uint 3D texture</span>
          <span>
            <template v-if="report.hasR16Uint === true">supported</template>
            <template v-else-if="report.hasR16Uint === false">refused</template>
            <template v-else>—</template>
          </span>
          <span>timestamp-query</span>
          <span>{{ report.hasTimestamps ? 'yes' : 'no' }}</span>
        </div>
      </CollapsibleSection>
    </template>
  </div>
</template>

<style scoped>
.gpu-diag { display: flex; flex-direction: column; gap: 0.6rem; }

.verdict-row { display: flex; align-items: center; gap: 0.6rem; }
.verdict-icon { font-size: var(--cc-fs-lg); }
.verdict-text { flex: 1; font-size: var(--cc-fs-sm); }

.kv {
  display: grid;
  grid-template-columns: max-content 1fr;
  gap: 0.3rem 0.9rem;
  font-size: var(--cc-fs-sm);
  color: var(--cc-text);
}
.kv > span:nth-child(odd) { color: var(--cc-text-dim); }
.kv-mono > span { font-family: var(--cc-mono); word-break: break-all; }

.os-guide { padding: 0.5rem 1rem; }
.os-guide code { background: var(--cc-surface-2); padding: 0.05rem 0.3rem; border-radius: var(--cc-radius-xs); }
</style>
