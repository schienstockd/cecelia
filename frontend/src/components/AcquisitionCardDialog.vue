<!--
  Preview, print and save of an image's acquisition settings card (AcquisitionCardSheet). The preview
  scales the A4 sheet down to fit the dialog. While this dialog is open a second, unscaled copy of the
  sheet is teleported to <body>, off-screen: Print hides everything else (the print stylesheet below),
  so only the card prints on one A4 page; Save renders that copy into a one-page PDF download.
-->
<script setup lang="ts">
import { computed, ref } from 'vue'
import { usePlotResize } from '../composables/usePlotResize'
import { loadImg, svgDoc } from '../plots/export'
import { exportTabsToPdf } from '../plots/pdf'
import BaseModal from './BaseModal.vue'
import AcquisitionCardSheet from './AcquisitionCardSheet.vue'
import ComingSoonBadge from './ComingSoonBadge.vue'
import { acquisitionCard, type CardImage } from '../utils/acquisitionCard'
import { PAPER } from '../plots/fixedColours'

const props = defineProps<{ image: CardImage; projectUid: string }>()
const emit = defineEmits<{ (e: 'close'): void }>()

const today = () => {
  const d = new Date()
  return `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, '0')}-${String(d.getDate()).padStart(2, '0')}`
}
const print = () => window.print()
// The sheet is A4-wide (186 mm); the preview shrinks it to fit the dialog (narrow window, scrollbar,
// padding) instead of clipping its right-hand "This run" column. Print and Save use the unscaled copy.
const SHEET_PX = 186 * 96 / 25.4
const preview = ref<HTMLElement | null>(null)
const zoom = ref(1)
usePlotResize(preview, () => {
  const el = preview.value
  if (!el) return
  const cs = getComputedStyle(el)
  zoom.value = Math.min(1, (el.clientWidth - parseFloat(cs.paddingLeft) - parseFloat(cs.paddingRight)) / SHEET_PX)
})

// Save = the same one-page A4 card as a PDF download, no print dialog: the off-screen copy rendered
// to an image and placed on the page by the Analysis board's PDF writer. Captured with the sheet's own
// (scoped) stylesheet, not `elementToImageURL`'s inlined computed styles: those force content-box and
// fixed widths, the legend wraps, the copy grows taller than the box measured for it, and the bottom
// of the card is cut off.
const printCopy = ref<HTMLElement | null>(null)
const saving = ref(false)
const fileStem = () => (props.image.name || props.image.uid).replace(/[^\w.-]+/g, '_')
function sheetCss(scope: string): string {
  const out: string[] = []
  for (const sheet of Array.from(document.styleSheets)) {
    let rules: CSSRuleList
    try { rules = sheet.cssRules } catch { continue }   // a cross-origin sheet can't be read
    for (const r of Array.from(rules)) if (r.cssText.includes(scope)) out.push(r.cssText)
  }
  return out.join('\n')
}
async function sheetPng(el: HTMLElement, scale = 3): Promise<string | null> {
  const w = el.offsetWidth, h = el.offsetHeight
  const scope = Array.from(el.attributes).find(a => a.name.startsWith('data-v-'))?.name ?? 'acs'
  const html = new XMLSerializer().serializeToString(el)
  const svg = svgDoc({ width: w, height: h, body: `<foreignObject width="${w}" height="${h}">` +
    `<div xmlns="http://www.w3.org/1999/xhtml"><style>${sheetCss(scope).replace(/</g, '\\3c ')}</style>${html}</div></foreignObject>` })
  const img = await loadImg('data:image/svg+xml;charset=utf-8,' + encodeURIComponent(svg))
  if (!img) return null
  const c = document.createElement('canvas')
  c.width = w * scale; c.height = h * scale
  const ctx = c.getContext('2d')
  if (!ctx) return null
  ctx.fillStyle = PAPER; ctx.fillRect(0, 0, c.width, c.height)
  ctx.scale(scale, scale); ctx.drawImage(img, 0, 0)
  return c.toDataURL('image/png')
}
const save = async () => {
  const el = printCopy.value?.firstElementChild as HTMLElement | null
  if (!el || saving.value) return
  saving.value = true
  try {
    const png = await sheetPng(el)
    if (!png) return
    await exportTabsToPdf([{ title: '', aspect: el.offsetWidth / el.offsetHeight,
      slots: [{ rect: { x: 0, y: 0, w: 1, h: 1 }, png }] }], `settings_card_${fileStem()}.pdf`)
  } finally { saving.value = false }
}
const card = computed(() => acquisitionCard(props.image, { projectUid: props.projectUid, printedOn: today() }))
</script>

<template>
  <BaseModal width="calc(186mm + 90px)" @close="emit('close')">
    <template #title><i class="pi pi-print" /> Settings card <ComingSoonBadge tier="experimental" /></template>
    <div ref="preview" class="acd-preview"><AcquisitionCardSheet :card="card" :style="{ zoom }" /></div>
    <template #footer>
      <button class="cc-btn" :disabled="saving" v-tooltip="'Save the card as a PDF'" @click="save">
        <i :class="saving ? 'pi pi-spin pi-spinner' : 'pi pi-download'" /> Save
      </button>
      <button class="cc-btn cc-btn-primary" v-tooltip="'Print, or save as PDF from the print dialog'" @click="print">
        <i class="pi pi-print" /> Print
      </button>
    </template>
  </BaseModal>
  <Teleport to="body">
    <div ref="printCopy" class="cc-print-card"><AcquisitionCardSheet :card="card" /></div>
  </Teleport>
</template>

<style scoped>
.acd-preview { background: var(--cc-paper); padding: 12px; border-radius: var(--cc-radius-sm); overflow-x: hidden; }
</style>

<style>
/* Not scoped: the print rule has to reach <body>'s other children. */
/* Off-screen, not display:none — Save renders it to an image, which needs it laid out. */
@media screen { .cc-print-card { position: fixed; left: -10000px; top: 0; pointer-events: none; } }
@media print {
  @page { size: A4; margin: 12mm; }
  body > *:not(.cc-print-card) { display: none !important; }
  .cc-print-card { display: block; position: static; }
}
</style>
