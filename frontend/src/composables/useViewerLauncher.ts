import { computed, onMounted, onUnmounted } from 'vue'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useShareTargetStore } from '../stores/shareTarget'
import { raiseViewerWindow } from '../utils/viewerWindow'

/**
 * The main window's way to the viewer (docs/todo/VIEWER_ON_THE_IMAGE_PLAN.md, S4): the header's Viewer
 * chip and the sidebar launcher bring the viewer window forward, or open it on the image last opened
 * (`openImageUid`, which the viewer's focus bridge keeps current). The viewer's back button is the way
 * home. `open` is the share store's viewer-availability poll (a window closing fires nothing here).
 */
export function useViewerLauncher() {
  const project = useProjectStore()
  const meta = useProjectMetaStore()
  const share = useShareTargetStore()
  onMounted(() => share.startViewerPoll())
  onUnmounted(() => share.stopViewerPoll())

  const open = computed(() => share.viewerAvailable)
  const target = computed(() => meta.current?.uid && project.openImageUid
    ? { projectUid: meta.current.uid, imageUid: project.openImageUid } : null)
  const disabled = computed(() => !open.value && !target.value)
  const tip = computed(() => open.value ? 'Bring the viewer to the front'
    : target.value ? 'Open the viewer' : 'Open an image with its ↗ first')
  function raise() { raiseViewerWindow(target.value) }
  return { open, disabled, tip, raise }
}
