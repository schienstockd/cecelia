/**
 * Which open surface answers Escape. Dialogs (`BaseModal`) and popovers (`TeleportPopover`) stack in
 * DOM order: a nested dialog sits inside its parent's DOM, popovers teleport to the end of <body>, and
 * App.vue mounts the app-level modals in stacking order — so the LAST one in the document is on top.
 *
 * Every handler also checks `e.defaultPrevented` FIRST: the surface that took an Escape may already be
 * out of the DOM by the time the next listener looks (Vue re-renders between listeners), so this query
 * alone would hand the same key to whatever is underneath (TeleportPopover.browser.test.ts).
 */
export const SURFACES = '.cc-modal-overlay, .cc-popover'

/** `el` is the last of `selector` in the document — or nothing of the kind is open at all. */
export function isTopmost(el: Element | null, selector: string = SURFACES): boolean {
  const all = document.querySelectorAll(selector)
  return all.length === 0 || all[all.length - 1] === el
}

/** Some dialog or popover is open, so Escape belongs to it, not to what lies underneath. */
export function anySurfaceOpen(): boolean {
  return document.querySelector(SURFACES) !== null
}
