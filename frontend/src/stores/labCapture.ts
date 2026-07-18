import { defineStore } from 'pinia'
import { ref } from 'vue'
import { useTaskCompletionWatch } from '../composables/useTaskCompletionWatch'
import { useSettingsStore } from './settings'
import { useProjectMetaStore } from './projectMeta'
import { worstDigestLevel, firstActionableLine } from '../lib/labDigest'

// Cecelia's automatic activity summaries. `capture_context!` (backend) aggregates run/gating/exclusion
// activity since the last capture into a [Cecelia] digest; this store fires it AUTOMATICALLY when a
// task/chain node finishes (debounced via useTaskCompletionWatch — the backbone freed when the
// auto-Claude "Watch" was removed), so Cecelia is the always-on reporter. Installed once from App.vue
// (the lab-log panel is v-if'd). The manual "Capture" button routes through here too. When a digest
// with ⚠️/❌ appends while the panel is closed, it lights the sidebar badge (bell, coloured by level);
// routine ✅ digests append silently. See docs/todo/QC_OBSERVER_PLAN.md (B2).
export const useLabCaptureStore = defineStore('labCapture', () => {
  const settings = useSettingsStore()
  const pm = useProjectMetaStore()
  const captureTick = ref(0)              // bumped when a capture appends → an open panel reloads

  const projectUid = () => pm.current?.uid ?? ''

  // POST the capture; on a real append bump the tick and, if the panel is closed and the digest is
  // actionable (⚠️/❌), light the Cecelia badge. Returns the response body (for the caller's note).
  async function capture(): Promise<{ captured: boolean; block?: string } | null> {
    const uid = projectUid()
    if (!uid) return null
    try {
      const r = await fetch('/api/lablog/capture', {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ projectUid: uid }),
      })
      if (!r.ok) return null
      const body = await r.json()
      if (body.captured) {
        captureTick.value++
        const level = body.block ? worstDigestLevel(body.block) : 'ok'
        if (!settings.labLogPanelOpen && (level === 'warn' || level === 'fail')) {
          settings.labLogUnseen = firstActionableLine(body.block) || 'Activity flagged'
          settings.labLogUnseenKind = 'cecelia'
          settings.labLogUnseenLevel = level
        }
      }
      return body
    } catch { return null }
  }

  // Auto-capture after a task/chain node finishes (debounced + coalesced by the composable; the
  // backend digest is "since last capture", so bursts collapse to one entry). App-lifetime install,
  // gated on the SAME "Auto" toggle (`labLogAutoContext`) that governs capture-on-project-open — one
  // control for all automatic capture; when off, only the manual "Capture" button fires.
  const watcher = useTaskCompletionWatch({
    enabled: () => !!projectUid() && settings.labLogAutoContext,
    onComplete: () => { capture() },
  })
  const installAutoCapture = () => watcher.install()

  return { captureTick, capture, installAutoCapture }
})
