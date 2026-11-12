<!--
  The viewer window's playback bar (docs/todo/GUI_LAYOUT_PLAN.md, V2; mock frame 4): one floating
  strip at the bottom centre of the image for time, z and level. A shell: the window fills it (the
  controls stay bound to the window's state), so this owns only the placement and the look. It floats
  above the bottom strip of the image, where the scale bar (bottom right, part of what Save image
  composites) and the status chips (bottom left) sit, so it never covers either.
-->
<template>
  <div class="vw-pbar" role="toolbar" aria-label="Playback">
    <slot />
  </div>
</template>

<style scoped>
.vw-pbar {
  position: absolute; left: 50%; bottom: 3rem; transform: translateX(-50%);
  max-width: calc(100% - 1.5rem); overflow-x: auto;
  display: flex; align-items: center; gap: 0.4rem; padding: 0.3rem 0.6rem;
  background: var(--cc-surface-1); border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-sm); box-shadow: 0 2px 10px var(--cc-shadow);
  white-space: nowrap; z-index: 2;
}
</style>
