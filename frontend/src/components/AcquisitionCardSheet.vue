<!--
  The acquisition settings card as one printable A4 page (content: utils/acquisitionCard.ts). Black and
  white: where a value came from is a SHAPE (◆ file, ● image metadata, ▲ measured, ○ not recorded) and
  "?" marks a value whose meaning is unverified. Rendered twice by AcquisitionCardDialog — as the
  on-screen preview and as the copy the browser prints — so it uses fixed print units, not app tokens.
-->
<script setup lang="ts">
import { CARD_GLYPH, type AcquisitionCard, type CardCell } from '../utils/acquisitionCard'
import { LIGHT_PATH_SOURCES } from '../utils/acquisitionLightPath'

defineProps<{ card: AcquisitionCard }>()

const glyph = (c: { source: CardCell['source'] }) => CARD_GLYPH[c.source]
</script>

<template>
  <div class="acs">
    <div class="acs-head">
      <div>
        <div class="acs-title">Acquisition settings card</div>
        <div><b>{{ card.title }}</b></div>
        <div class="acs-small">{{ card.scope ?? 'Microscope not recorded' }}</div>
        <div v-if="card.sourceFile" class="acs-small acs-mono">{{ card.sourceFile }}</div>
      </div>
      <div class="acs-tag">
        <span class="acs-tier">Experimental</span><br>
        Cecelia · project <span class="acs-mono">{{ card.projectUid }}</span><br>
        image <span class="acs-mono">{{ card.imageUid }}</span><br>
        printed {{ card.printedOn }}
      </div>
    </div>
    <div class="acs-legend">
      <span v-for="src in LIGHT_PATH_SOURCES" :key="src.key">{{ CARD_GLYPH[src.key] }} {{ src.label }}</span>
      <span>? unverified</span>
    </div>

    <h2>1 · Settings, in light-path order</h2>
    <table>
      <thead><tr><th /><th>Setting</th><th>Last time</th><th class="acs-run">This run</th></tr></thead>
      <tbody>
        <tr v-for="r in card.settings" :key="r.label">
          <td class="acs-n">{{ r.n }}</td>
          <td class="acs-k">{{ r.label }}</td>
          <td>
            <div v-for="(it, i) in r.items" :key="i" :class="{ 'acs-blank': it.source === 'missing' }">
              {{ glyph(it) }} {{ it.text }}<span v-if="it.unverified" class="acs-q"> ?</span>
            </div>
          </td>
          <td class="acs-run" />
        </tr>
      </tbody>
    </table>

    <h2>2 · Detectors, per channel</h2>
    <table>
      <thead><tr><th>Channel</th><th>Dye preset</th><th>Laser</th><th>Detector</th><th>Emission filter</th><th>HV</th><th>Gain · offset</th><th class="acs-run">This run</th></tr></thead>
      <tbody>
        <tr v-for="(d, i) in card.detectors" :key="d.channel">
          <td><b>{{ d.channel }}</b></td><td>{{ d.dye }}</td><td>{{ d.laser }}</td><td>{{ d.detector }}</td>
          <td :class="{ 'acs-blank': d.filter.source === 'missing' }">{{ glyph(d.filter) }} {{ d.filter.text }}</td>
          <td v-if="card.autoHV && i === 0" :rowspan="card.detectors.length" class="acs-hv">
            {{ glyph(card.autoHV) }} {{ card.autoHV.text }}<span v-if="card.autoHV.unverified" class="acs-q"> ?</span>
          </td>
          <td v-else-if="d.hv" :class="{ 'acs-blank': d.hv.source === 'missing' }">{{ glyph(d.hv) }} {{ d.hv.text }}</td>
          <td :class="{ 'acs-blank': d.gainOffset.source === 'missing' }">{{ glyph(d.gainOffset) }} {{ d.gainOffset.text }}</td>
          <td class="acs-run" />
        </tr>
      </tbody>
    </table>

    <h2>3 · Target — check your first snap against this</h2>
    <table>
      <thead><tr><th>Channel</th><th>Clipped (of signal)</th><th>Zeros</th><th>Brightest 1 %, top → bottom</th><th>Brightest 1 %, first → last frame</th><th class="acs-run">Your snap</th></tr></thead>
      <tbody>
        <tr v-for="t in card.targets" :key="t.channel">
          <td><b>{{ t.channel }}</b></td>
          <td v-for="(c, i) in [t.clipped, t.zeros, t.byDepth, t.byTime]" :key="i" :class="{ 'acs-blank': c.source === 'missing' }">{{ glyph(c) }} {{ c.text }}</td>
          <td class="acs-run" />
        </tr>
      </tbody>
    </table>
    <div v-if="card.targetsMissing" class="acs-small">Not measured: Refresh metadata in Cecelia before printing.</div>

    <h2>4 · This run</h2>
    <div class="acs-lines">
      <div v-for="f in card.fillIn" :key="f">{{ f }}</div>
    </div>

    <div class="acs-caveat">{{ card.caveat }}</div>
  </div>
</template>

<style scoped>
.acs { width: 186mm; color: #000; background: #fff; font: 8.5pt/1.25 "DejaVu Sans", Arial, sans-serif; }
.acs-head { display: grid; grid-template-columns: 1fr auto; gap: 2px 16px; border: 1.5px solid #000; padding: 6px 8px; }
.acs-title { font-size: 13pt; font-weight: 700; }
.acs-small { font-size: 8pt; color: #333; }
.acs-mono { font-family: "DejaVu Sans Mono", monospace; }
.acs-tag { text-align: right; font-size: 8pt; }
.acs-tier { display: inline-block; border: 1px solid #000; padding: 0 4px; font-size: 7.5pt; text-transform: uppercase; }
.acs-legend { font-size: 8pt; margin-top: 3px; display: flex; gap: 12px; }
h2 { font-size: 9pt; margin: 7px 0 2px; padding-bottom: 2px; border-bottom: 1.5px solid #000; text-transform: uppercase; letter-spacing: .04em; }
table { width: 100%; border-collapse: collapse; }
td, th { border: .6px solid #888; padding: 1px 4px; vertical-align: top; text-align: left; }
th { font-size: 8pt; background: #eee; font-weight: 600; }
.acs-n { width: 5mm; text-align: center; color: #555; }
.acs-k { width: 26mm; }
.acs-hv { width: 34mm; }
.acs-run { width: 30mm; border-left: 1.5px solid #000; }
.acs-blank { color: #555; font-style: italic; }
.acs-q { color: #444; }
.acs-lines { display: grid; grid-template-columns: 1fr 1fr; gap: 0 10mm; }
.acs-lines div { border-bottom: .6px solid #888; height: 6mm; display: flex; align-items: flex-end; font-size: 8pt; color: #444; }
.acs-caveat { margin-top: 8px; border: 1px solid #000; padding: 4px 8px; font-size: 8.5pt; }
</style>
