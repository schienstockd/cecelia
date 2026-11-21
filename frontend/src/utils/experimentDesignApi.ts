// The experiment design routes (api/src/routes/experiment_design.jl), one function each. Shapes in
// `experimentDesign.ts`.

import { ref } from 'vue'
import type { DesignReply, ExperimentDesign } from './experimentDesign'

/** Bumped on every save and attribute rename, so a view showing a design (the Design check, a plot
 *  canvas) reloads after a change made elsewhere. */
export const designSaves = ref(0)

async function call(url: string, init?: RequestInit): Promise<DesignReply> {
  const res = await fetch(url, init)
  const body = await res.json().catch(() => ({})) as DesignReply & { error?: string }
  if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
  return body
}

const post = (url: string, body: unknown) =>
  call(url, { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) })

/** The set's design, version, history and grid (`design` null = none). */
export const fetchDesign = (projectUid: string, setUid: string) =>
  call(`/api/sets/experiment-design?projectUid=${encodeURIComponent(projectUid)}&setUid=${encodeURIComponent(setUid)}`)

/** The grid for an unsaved design, `attrs` standing in for values not written yet. Writes nothing. */
export const previewDesign = (projectUid: string, setUid: string, design: ExperimentDesign,
                              attrs: Record<string, Record<string, string>>) =>
  post('/api/sets/experiment-design/preview', { projectUid, setUid, design, attrs })

/** Save the design (null clears it); `change` is the history note. */
export async function saveDesign(projectUid: string, setUid: string, design: ExperimentDesign | null, change: string) {
  const reply = await post('/api/sets/experiment-design', { projectUid, setUid, design, change })
  designSaves.value++
  return reply
}
