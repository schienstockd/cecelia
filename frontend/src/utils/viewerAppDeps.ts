// The app state the viewer engine's helpers take as inputs, built from the stores.
//
// The viewer engine (`lib/webgpu`, `utils/viewer`) imports no app store but its own `viewer` store and
// `log` (docs/architecture/README.md → the `viewer-store` rule). Its helpers that drive the app take
// what they read and write as a parameter instead; this file is where the app side fills it in, so
// every caller wires them the same way.

import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'
import { useLinkedSelectionStore, linkedSourceKey } from '../stores/linkedSelection'
import type { ShowTracksDeps } from './viewer/showTracksInViewer'

/** `showTracksInViewer`'s inputs, read now — call it at the Show, not once at setup. */
export function showTracksDeps(): ShowTracksDeps {
  const project = useProjectStore()
  const linked = useLinkedSelectionStore()
  return {
    openImageUid: project.openImageUid,
    currentProjectUid: useProjectMetaStore().current?.uid,
    settings: useSettingsStore(),
    publishTrackSelection: (imageUid, valueName, trackIds, source) =>
      linked.set({ scope: 'tracks', ids: trackIds, source, sourcePlotId: source,
                   perSource: { [linkedSourceKey(imageUid, valueName)]: [...trackIds] } }),
  }
}
