<!--
  The header's one worded Help menu (docs/todo/GUI_CLARITY_PLAN.md Decision 7, built by
  docs/todo/GUI_LAYOUT_PLAN.md). It holds what used to be six icon-only header buttons, in two groups:
  Learn (how does this work?) and Get help (the app is not the answer). Each item opens the same
  shared modal or link the old button did; nothing here owns state.
-->
<script setup lang="ts">
import { ref } from 'vue'
import TeleportPopover from './TeleportPopover.vue'
import { openGuides } from '../lib/guideOpen'
import { openMaturity } from '../lib/maturityOpen'
import { openIconLegend } from '../lib/iconLegendOpen'
import { openColorLegend } from '../lib/colorLegendOpen'
import { openWhatsNew } from '../lib/whatsNew'
import { openCallForDatasets } from '../lib/callForDatasetsOpen'
import { CECELIA_ISSUES_URL, CECELIA_CHAT_URL } from '../lib/links'

// `windowed`: a pop-out window (the viewer) has no app shell, so the guides and glossary dialogs —
// mounted once in the main window's App.vue — are not here. It keeps the outward links, and the host
// adds its own items through the `learn` slot (the viewer's Shortcuts).
withDefaults(defineProps<{ windowed?: boolean }>(), { windowed: false })
const open = ref(false)
const anchor = ref<HTMLElement | null>(null)

// every item closes the menu first: the modals it opens sit above the popover anyway
function pick(fn: () => void) { open.value = false; fn() }
</script>

<template>
  <button ref="anchor" type="button" class="help-btn cc-btn cc-btn-ghost cc-btn-dense"
          data-guide="header.helpMenu" :class="{ 'cc-btn-on': open }" aria-haspopup="menu"
          v-tooltip.bottom="'Guides, glossaries, what’s new, and where to ask'"
          @click="open = !open">
    Help <i class="pi pi-chevron-down cc-fs-2xs" />
  </button>
  <TeleportPopover v-model="open" :anchor="anchor" placement="bottom-end" flush>
    <div class="cc-actions-menu" role="menu">
      <div class="cc-actions-head">Learn</div>
      <slot name="learn" :close="() => { open = false }" />
      <template v-if="!windowed">
      <button class="cc-actions-item" role="menuitem" @click="pick(openGuides)">
        <i class="pi pi-compass" /> Guides
      </button>
      <button class="cc-actions-item" role="menuitem" @click="pick(openMaturity)">
        <i class="pi pi-verified" /> Feature maturity
      </button>
      <button class="cc-actions-item" role="menuitem" @click="pick(openIconLegend)">
        <i class="pi pi-key" /> What the icons mean
      </button>
      <button class="cc-actions-item" role="menuitem" @click="pick(openColorLegend)">
        <i class="pi pi-palette" /> What the colours mean
      </button>
      <button class="cc-actions-item" role="menuitem" @click="pick(() => openWhatsNew({ withTip: true }))">
        <i class="pi pi-gift" /> What’s new and tips
      </button>
      </template>
      <div class="cc-actions-head">Get help</div>
      <a class="cc-actions-item" role="menuitem" :href="CECELIA_ISSUES_URL" target="_blank" rel="noopener"
         @click="open = false">
        <i class="pi pi-github" /> Report a problem (GitHub)
      </a>
      <a class="cc-actions-item" role="menuitem" :href="CECELIA_CHAT_URL" target="_blank" rel="noopener"
         @click="open = false">
        <i class="pi pi-comments" /> Ask in the chat (Zulip)
      </a>
      <button v-if="!windowed" class="cc-actions-item" role="menuitem" @click="pick(() => openCallForDatasets())">
        <i class="pi pi-megaphone" /> Call for datasets
      </button>
    </div>
  </TeleportPopover>
</template>

<style scoped>
.help-btn { gap: 0.3rem; }
</style>
