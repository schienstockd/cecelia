<!--
  Animation page (/animation) — a "super-simple OpenShot": a per-image TIMELINE of view snapshots
  (keyframes), rendered to an mp4 by interpolating between them. See docs/todo/ANIMATION_PLAN.md (F2).

  A module page like every other one: it used to be a standalone `ModulePage` that
  read whichever image the viewer happened to have open, so its empty state — "open an image in the
  viewer" — could only be acted on by leaving the page. The image table IS that action, and it brings
  the set bar, the filters and the eye with it. The controls moved into the side panel
  (animation/AnimationPanel.vue), the timeline into the standard plot canvas (AnimationTimeline.vue).

  Single-select: a timeline interpolates views of ONE image, so there is nothing a second one could mean.
-->
<script setup lang="ts">
import { useViewerStore } from '../stores/viewer'
import { activeAnimationUid } from '../utils/animationTimeline'
import ModuleLayout from '../components/ModuleLayout.vue'
import AnimationPanel from './animation/AnimationPanel.vue'
import AnimationTimeline from './animation/AnimationTimeline.vue'

const viewer = useViewerStore()
</script>

<template>
  <ModuleLayout module="animation" :single-select="true" plots-label="Timeline" :right-default-width="290">
    <template #right="{ setUid, selectedUids }">
      <AnimationPanel :selected-uids="selectedUids" :set-uid="setUid" />
    </template>
    <!-- With nothing selected the timeline still shows whatever the viewer has open, so landing on
         the page is never a blank canvas — the fallback is the viewer's published `openImage`, as in AnimationPanel. -->
    <template #plots="{ selectedUids }">
      <AnimationTimeline :image-uid="activeAnimationUid(selectedUids, viewer.openImage?.imageUid ?? null)" />
    </template>
  </ModuleLayout>
</template>
