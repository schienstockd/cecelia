import { defineStore } from 'pinia'
import { ref } from 'vue'
import { useTaskCompletionWatch } from '../composables/useTaskCompletionWatch'
import { useSettingsStore } from './settings'
import { useProjectMetaStore } from './projectMeta'
import { worstDigestLevel, firstActionableLine, newFlagLines } from '../lib/labDigest'

// Cecelia's automatic activity summaries. `capture_context!` (backend) upserts the rolling DAILY
// [Cecelia] digest — one block per day, regenerated from run/gating/exclusion activity and rewritten
// in place — so firing it often is cheap and idempotent (an unchanged block is a no-op). This store
// fires it AUTOMATICALLY when a task/chain node finishes (debounced via useTaskCompletionWatch — the
// backbone freed when the auto-Claude "Watch" was removed), so Cecelia is the always-on reporter, and
// a fresh ⚠️/❌ shows up in today's block the moment its task lands. Installed once from App.vue (the
// lab-log panel is v-if'd). The manual "Capture" button routes through here too. When the day's block
// changes and carries ⚠️/❌ while the panel is closed, it lights the sidebar badge (bell, coloured by
// level); routine ✅-only changes are silent. See docs/todo/QC_OBSERVER_PLAN.md (B2).
export const useLabCaptureStore = defineStore('labCapture', () => {
  const settings = useSettingsStore()
  const pm = useProjectMetaStore()
  const captureTick = ref(0)              // bumped when a capture appends → an open panel reloads
  // the day's block as we last saw it — the rolling block holds ALL of today's flags, so we badge only
  // on flags NEW vs this (a later clean task must not re-alert a standing warning). Empty until reset.
  const lastBlock = ref('')

  const projectUid = () => pm.current?.uid ?? ''

  // POST the capture; on a real change bump the tick and, if the panel is closed and a NEW ⚠️/❌ flag
  // appeared in today's block (not a standing one), light the Cecelia badge. Returns the response body.
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
        const block = body.block ?? ''
        const fresh = newFlagLines(lastBlock.value, block).join('\n')   // only newly-appeared ⚠️/❌
        lastBlock.value = block
        const level = worstDigestLevel(fresh)
        if (!settings.labLogPanelOpen && (level === 'warn' || level === 'fail')) {
          settings.labLogUnseen = firstActionableLine(fresh) || 'Activity flagged'
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

  // Signal that a [Cecelia] entry was appended by a path OTHER than capture() — the Check-cohort button
  // appends its "[Cecelia — Cohort check]" entry server-side (POST /api/qc/cohort/check), which doesn't
  // route through capture(), so an open panel wouldn't reload without this. Same tick the panel watches.
  const notifyAppended = () => { captureTick.value++ }

  return { captureTick, capture, installAutoCapture, notifyAppended }
})
