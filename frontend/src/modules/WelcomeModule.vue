<script setup lang="ts">
/*
  The app's home page at `/` — the brand lockup as a watermark, and nothing else.

  It exists to make `/` a real destination. `/` used to `redirect` to `/manage-images`, which a view
  profile can hide, so the landing page had to be resolved from the active profile — and a record's
  `redirect` is resolved BEFORE any router guard, i.e. before the profile list has arrived, so a cold
  boot landed on the fallback and then bounced. A neutral page removes the problem instead of timing
  around it: nothing to resolve, nothing to race, and no page is privileged as "the start".

  Deliberately empty of copy (docs/ui/COPY.md). The sidebar beside it already says what this
  install can do, and whatever a paragraph here said would be read once and skipped forever. The
  wordmark is not copy — it is the mark.
*/
</script>

<template>
  <!-- aria-hidden: decorative. The lockup is the app's name, which the header already states, and the
       sidebar is the real landmark — a screen reader announcing "Cecelia" twice adds nothing. -->
  <div class="welcome">
    <div class="welcome-lockup" aria-hidden="true">
      <span class="welcome-word">Cecelia</span>
      <img class="welcome-mark" src="/feijoa.svg" alt="" width="240" height="240" />
    </div>
  </div>
</template>

<style scoped>
/* `min-height`, NOT `flex: 1` — `.cc-main` (App.vue) is a flex ITEM with `overflow-y: auto`, not a
   flex container, so `flex` on a child does nothing and the page sat at the top of the area. Its
   height is definite (stretched by `.cc-content`), so a percentage resolves against it. */
.welcome {
  min-height: 100%;
  display: flex;
  align-items: center;
  justify-content: center;
  user-select: none;
}

/* Horizontal lockup — wordmark then mark (Dominik 2026-08-17; the R logo and the header both lead with
   the mark, this trails it). ONE opacity on the group, so the two halves recede by exactly the same
   amount; setting it per element is how a lockup ends up with a word darker than its mark. */
.welcome-lockup {
  display: flex;
  align-items: center;
  gap: clamp(0.5rem, 2vmin, 1.75rem);
  opacity: 0.07;
  pointer-events: none;
}

/* Grey: `grayscale` drops the brand's lime/ink palette to one value so the mark reads as surface
   rather than as content. The blending is the group's opacity above — no hardcoded colour to keep in
   step with the tokens, and it works on whatever background the shell has. */
.welcome-mark {
  display: block;
  flex: none;
  filter: grayscale(1);
  width: clamp(4rem, 22vmin, 15rem);
  height: auto;
}

/* Weight 800 + tight tracking is the logo's own lettering (the R logo's wordmark, and the feijoa
   sketch port's `wordmarkWeight: 800`), in the body font the app already ships. Deliberately NOT a
   `cursive` family: that resolves to a different face on every OS — Comic Sans on Windows, Apple
   Chancery on macOS, frequently nothing on Linux — so the brand would render differently per machine. */
.welcome-word {
  font-size: clamp(2rem, 13vmin, 9rem);
  font-weight: 800;
  letter-spacing: -0.02em;
  line-height: 1;
  color: var(--cc-text);
  white-space: nowrap;
}
</style>
