<!--
  The ONE delete affordance for the whole app — a single icon button that ARMS on the first click
  (trash → warning, solid danger fill) and fires @confirm on the second (the ViewerPanel labels
  pattern D picked as the standard). Before this, delete confirmations diverged: some flipped one icon,
  some sprouted a Confirm+Cancel pair, some deleted instantly. Use this everywhere a destructive icon
  action needs an inline "are you sure" (docs/UI.md → confirm-delete).

  It WRAPS ConfirmButton for the arm/confirm/cancel + auto-dismiss + outside-click logic (so that
  logic lives in exactly one place) and adds the standard, self-contained chrome — self-styled because
  hosts' button classes (opt-btn / pm-icon / wb-btn / btn-danger) are component-scoped and can't reach
  a button rendered here; a single owned style is also what makes the affordance identical app-wide.

  <ConfirmDeleteButton title="Delete label set from disk"
                       armed-title="Click again to permanently delete"
                       @confirm="deleteLabel(vn)" />
  Optional default slot → a text label beside the icon (e.g. "Delete set"). Tooltip position is left to
  PrimeVue's default + out-of-bounds flip (dynamic modifiers can't be set from a prop).
-->
<script setup lang="ts">
import ConfirmButton from './ConfirmButton.vue'

withDefaults(defineProps<{
  title?: string            // tooltip while idle (the verb, e.g. "Delete population")
  armedTitle?: string       // tooltip once armed
  disabled?: boolean
  needsConfirm?: boolean     // false → deletes on the FIRST click (no arm step)
  autoDismissMs?: number
}>(), {
  title: 'Delete', armedTitle: 'Click again to confirm', disabled: false,
  needsConfirm: true, autoDismissMs: 3500,
})
defineEmits<{ confirm: [] }>()
</script>

<template>
  <ConfirmButton :needs-confirm="needsConfirm" :auto-dismiss-ms="autoDismissMs" @confirm="$emit('confirm')"
                 v-slot="{ armed, arm, confirm }">
    <button type="button" class="cc-del" :class="{ armed }" :disabled="disabled"
            @click.stop="armed ? confirm() : arm()"
            v-tooltip="{ value: armed ? armedTitle : title }">
      <i class="pi" :class="armed ? 'pi-exclamation-triangle' : 'pi-trash'" />
      <span v-if="$slots.default" class="cc-del-lbl"><slot /></span>
    </button>
  </ConfirmButton>
</template>

<style scoped>
/* compact, self-contained danger icon button (dim by default → danger on hover → solid danger when
   armed), so it reads the same in every toolbar/row/footer. */
.cc-del { display: inline-flex; align-items: center; gap: 5px; background: none;
  border: 1px solid transparent; border-radius: 5px; color: var(--cc-text-dim); cursor: pointer;
  padding: 4px 6px; font-size: 12px; line-height: 1; }
.cc-del:hover:not(:disabled) { color: var(--cc-danger); background: var(--cc-surface-2); }
.cc-del.armed { color: #fff; background: var(--cc-danger); border-color: var(--cc-danger); }
.cc-del:disabled { opacity: 0.35; cursor: not-allowed; }
.cc-del-lbl { white-space: nowrap; }
</style>
