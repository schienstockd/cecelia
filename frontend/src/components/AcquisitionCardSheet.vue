<!--
  The acquisition settings card as printable A4 pages (content: utils/acquisitionCard.ts, which part on
  which page: utils/acquisitionCardPages.ts). Black and white: where a value came from is a SHAPE
  (◆ file, ● image metadata, ▲ measured, ○ not recorded) and "?" marks a value whose meaning is
  unverified. Rendered twice by AcquisitionCardDialog — as the on-screen preview and as the copy the
  browser prints — so it uses fixed print units, not app tokens. Each `.acs` is one page.
-->
<script setup lang="ts">
import { computed } from 'vue'
import { CARD_GLYPH, NONE, type AcquisitionCard, type CardCell } from '../utils/acquisitionCard'
import { cardPages } from '../utils/acquisitionCardPages'
import { LIGHT_PATH_SOURCES } from '../utils/acquisitionLightPath'

const props = defineProps<{ card: AcquisitionCard }>()

const glyph = (c: CardCell | { source: CardCell['source'] }) => ('text' in c && c.text === NONE.text ? '' : CARD_GLYPH[c.source])
const pages = computed(() => cardPages(props.card))
const det = computed(() => props.card.detectorTable)
const groupsOf = (from: number, to: number) => det.value?.groups.slice(from, to) ?? []
const rowsIn = (from: number, to: number) => groupsOf(from, to).reduce((s, g) => s + g.rows.length, 0)
const channelTitle = computed(() => {
  const c = props.card
  if (c.markers) return 'Markers, by cycle'
  if (c.camera) return 'Camera, per channel'
  const by = det.value?.groupBy
  return by === 'Track' ? 'Tracks, one row per channel' : by === 'Sequence' ? 'Sequences, one row per channel' : 'Detectors, per channel'
})
/** Lines · power in its own column when not grouped (no group cell to hold them). */
const linesPerRow = computed(() => !!det.value?.cols.lines && !det.value.groupBy)
</script>

<template>
  <div class="acs-pages">
    <div v-for="pg in pages" :key="pg.n" class="acs">
      <template v-if="pg.n === 1">
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
            printed {{ card.printedOn }}<template v-if="pg.of > 1"> · page 1 of {{ pg.of }}</template>
          </div>
        </div>
        <div class="acs-legend">
          <span v-for="src in LIGHT_PATH_SOURCES" :key="src.key">{{ CARD_GLYPH[src.key] }}&nbsp;{{ src.label }}</span>
          <span>? unverified</span>
        </div>
      </template>
      <div v-else class="acs-cont"><b>{{ card.title }}</b> · settings card · page {{ pg.n }} of {{ pg.of }}</div>

      <template v-for="(b, bi) in pg.blocks" :key="bi">
        <template v-if="b.kind === 'settings'">
          <h2>1 · Settings, in light-path order</h2>
          <table>
            <thead><tr><th /><th>Setting</th><th>Last time</th></tr></thead>
            <tbody>
              <tr v-for="r in card.settings" :key="r.label">
                <td class="acs-n">{{ r.n }}</td>
                <td class="acs-k">{{ r.label }}</td>
                <td>
                  <div v-for="(it, i) in r.items" :key="i" :class="{ 'acs-blank': it.source === 'missing' }">
                    {{ glyph(it) }}&nbsp;{{ it.text }}<span v-if="it.unverified" class="acs-q"> ?</span>
                  </div>
                </td>
              </tr>
            </tbody>
          </table>
        </template>

        <template v-else-if="b.kind === 'channels'">
          <h2>2 · {{ channelTitle }}<template v-if="b.cont"> (continued)</template></h2>

          <table v-if="card.markers">
            <thead><tr><th>Cycle</th><th>Marker</th><th>Clone</th><th>Reporter · barcode</th><th>Dye</th><th>Ex → em (nm)</th><th>Filter cube</th><th>Exposure</th></tr></thead>
            <tbody>
              <template v-for="cy in card.markers.slice(b.from, b.to)" :key="cy.cycle">
                <tr v-for="(m, i) in cy.rows" :key="i" :class="{ 'acs-grp': i === 0 }">
                  <td v-if="i === 0" :rowspan="cy.rows.length"><b>{{ cy.cycle }}</b></td>
                  <td><b>{{ m.marker }}</b></td>
                  <td v-for="(x, k) in [m.clone, m.reagent]" :key="k" :class="{ 'acs-blank': x.source === 'missing' }">{{ glyph(x) }}&nbsp;{{ x.text }}</td>
                  <td>{{ m.dye }}</td>
                  <td v-for="(x, k) in [m.band, m.cube, m.exposure]" :key="'c' + k" :class="{ 'acs-blank': x.source === 'missing' }">{{ glyph(x) }}&nbsp;{{ x.text }}</td>
                </tr>
              </template>
            </tbody>
          </table>

          <table v-else-if="card.camera">
            <thead><tr><th>Channel</th><th>Filter cube</th><th v-if="card.camera.some(c => c.band)">Ex → em (nm)</th><th>Exposure</th><th>Gain</th><th>Lamp intensity</th></tr></thead>
            <tbody>
              <tr v-for="c in card.camera.slice(b.from, b.to)" :key="c.channel">
                <td><b>{{ c.channel }}</b></td>
                <td :class="{ 'acs-blank': c.cube.source === 'missing' }">{{ glyph(c.cube) }}&nbsp;{{ c.cube.text }}</td>
                <td v-if="card.camera.some(x => x.band)">{{ c.band ? `${glyph(c.band)}\u00a0${c.band.text}` : '—' }}</td>
                <td v-for="(x, i) in [c.exposure, c.gain, c.lamp]" :key="i" :class="{ 'acs-blank': x.source === 'missing' }">{{ glyph(x) }}&nbsp;{{ x.text }}</td>
              </tr>
            </tbody>
          </table>

          <table v-else-if="det">
            <thead><tr>
              <th v-if="det.groupBy">{{ det.groupBy === 'Track' ? 'Track' : 'Seq' }}</th>
              <th v-if="det.groupBy && det.cols.lines">Laser lines · power</th>
              <th v-if="det.cols.splitters">Beam splitters</th>
              <th>Channel</th><th>Dye preset</th>
              <th v-if="linesPerRow">Laser lines · power</th><th v-else-if="!det.cols.lines">Laser</th>
              <th>Detector</th><th>Band</th><th v-if="det.cols.pinhole">Pinhole</th><th>HV</th><th>Gain · offset</th>
              <th v-if="det.cols.airyscan">Airyscan mode</th>
            </tr></thead>
            <tbody>
              <template v-for="(g, gi) in groupsOf(b.from, b.to)" :key="gi">
                <tr v-for="(d, i) in g.rows" :key="i" :class="{ 'acs-grp': det.groupBy && i === 0 }">
                  <template v-if="i === 0">
                    <td v-if="det.groupBy" :rowspan="g.rows.length"><b>{{ g.label ?? '—' }}</b></td>
                    <td v-if="det.groupBy && det.cols.lines" :rowspan="g.rows.length">
                      <template v-if="g.lines"><div v-for="(l, k) in g.lines" :key="k">{{ glyph(l) }}&nbsp;{{ l.text }}</div></template>
                      <template v-else>
                        <div v-for="(r, k) in g.rows" :key="k">{{ r.channel }}: {{ (r.lines ?? []).map(l => l.text).join(', ') || '—' }}</div>
                      </template>
                    </td>
                    <td v-if="det.cols.splitters" :rowspan="g.rows.length">{{ g.splitters ? `${glyph(g.splitters)}\u00a0${g.splitters.text}` : '—' }}</td>
                  </template>
                  <td><b>{{ d.channel }}</b><div v-if="d.spans" class="acs-small">{{ d.spans }} channels</div></td>
                  <td>{{ d.dye }}</td>
                  <td v-if="linesPerRow"><div v-for="(l, k) in d.lines ?? []" :key="k">{{ glyph(l) }}&nbsp;{{ l.text }}</div></td>
                  <td v-else-if="!det.cols.lines">{{ d.laser }}</td>
                  <td>{{ d.detector }}</td>
                  <td :class="{ 'acs-blank': d.filter.source === 'missing' }">{{ glyph(d.filter) }}&nbsp;{{ d.filter.text }}</td>
                  <td v-if="det.cols.pinhole">{{ d.pinhole ? `${glyph(d.pinhole)}\u00a0${d.pinhole.text}` : '—' }}</td>
                  <td v-if="card.autoHV && gi === 0 && i === 0" :rowspan="rowsIn(b.from, b.to)" class="acs-hv">
                    {{ glyph(card.autoHV) }}&nbsp;{{ card.autoHV.text }}<span v-if="card.autoHV.unverified" class="acs-q"> ?</span>
                  </td>
                  <td v-else-if="d.hv" :class="{ 'acs-blank': d.hv.source === 'missing' }">{{ glyph(d.hv) }}&nbsp;{{ d.hv.text }}</td>
                  <td :class="{ 'acs-blank': d.gainOffset.source === 'missing' }">{{ glyph(d.gainOffset) }}&nbsp;{{ d.gainOffset.text }}</td>
                  <td v-if="det.cols.airyscan" class="acs-wrap">{{ d.airyscan ? `${glyph(d.airyscan)}\u00a0${d.airyscan.text}` : '—' }}</td>
                </tr>
              </template>
            </tbody>
          </table>
        </template>

        <template v-else-if="b.kind === 'targets'">
          <h2>3 · Target: check your first snap against this<template v-if="b.cont"> (continued)</template></h2>
          <table v-if="card.targets.length">
            <thead><tr>
              <th>Channel</th><th>Clipped (of signal)</th><th>Zeros</th>
              <th v-if="card.targetCols.depth">Brightest 1 %, top → bottom</th><th v-if="card.targetCols.time">Brightest 1 %, first → last frame</th>
              <th class="acs-run">Your snap</th>
            </tr></thead>
            <tbody>
              <tr v-for="t in card.targets.slice(b.from, b.to)" :key="t.channel">
                <td><b>{{ t.channel }}</b></td>
                <td v-for="(c, i) in [t.clipped, t.zeros, ...(card.targetCols.depth ? [t.byDepth] : []), ...(card.targetCols.time ? [t.byTime] : [])]"
                    :key="i" :class="{ 'acs-blank': c.source === 'missing' }">{{ glyph(c) }}&nbsp;{{ c.text }}</td>
                <td class="acs-run" />
              </tr>
            </tbody>
          </table>
          <template v-if="b.to === card.targets.length">
            <div v-if="card.tissue" class="acs-small">Tissue: {{ glyph(card.tissue) }}&nbsp;{{ card.tissue.text }}</div>
            <div v-if="card.targetsMissing" class="acs-small">Not measured: Refresh metadata in Cecelia before printing.</div>
          </template>
        </template>

        <template v-else-if="b.kind === 'fillIn'">
          <h2>4 · This run</h2>
          <div class="acs-lines">
            <div v-for="f in card.fillIn" :key="f">{{ f }}</div>
          </div>
        </template>

        <div v-else-if="b.kind === 'caveat'" class="acs-caveat">{{ card.caveat }}</div>
      </template>
    </div>
  </div>
</template>

<style scoped>
.acs { width: 186mm; color: #000; background: #fff; font: 8.5pt/1.25 "DejaVu Sans", Arial, sans-serif; }
.acs + .acs { break-before: page; }
.acs-head { display: grid; grid-template-columns: 1fr auto; gap: 2px 16px; border: 1.5px solid #000; padding: 6px 8px; }
.acs-title { font-size: 13pt; font-weight: 700; }
.acs-small { font-size: 8pt; color: #333; }
.acs-mono { font-family: "DejaVu Sans Mono", monospace; }
.acs-tag { text-align: right; font-size: 8pt; }
.acs-tier { display: inline-block; border: 1px solid #000; padding: 0 4px; font-size: 7.5pt; text-transform: uppercase; }
.acs-legend { font-size: 8pt; margin-top: 3px; display: flex; gap: 12px; }
.acs-cont { font-size: 8pt; border-bottom: 1.5px solid #000; padding-bottom: 2px; }
h2 { font-size: 9pt; margin: 7px 0 2px; padding-bottom: 2px; border-bottom: 1.5px solid #000; text-transform: uppercase; letter-spacing: .04em; }
table { width: 100%; border-collapse: collapse; }
td, th { border: .6px solid #888; padding: 1px 4px; vertical-align: top; text-align: left; }
th { font-size: 8pt; background: #eee; font-weight: 600; }
.acs-grp td { border-top: 1.5px solid #000; }
.acs-n { width: 5mm; text-align: center; color: #555; }
.acs-k { width: 26mm; }
.acs-hv { width: 34mm; }
.acs-run { width: 30mm; border-left: 1.5px solid #000; }
.acs-blank { color: #555; font-style: italic; }
.acs-q { color: #444; }
/* one long vendor token ("FastAiryScanSheppardSum") must not set the column's width */
.acs-wrap { overflow-wrap: anywhere; }
.acs-lines { display: grid; grid-template-columns: 1fr 1fr; gap: 0 10mm; }
.acs-lines div { border-bottom: .6px solid #888; height: 6mm; display: flex; align-items: flex-end; font-size: 8pt; color: #444; }
.acs-caveat { margin-top: 8px; border: 1px solid #000; padding: 4px 8px; font-size: 8.5pt; }
</style>
