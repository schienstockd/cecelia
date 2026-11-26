// The experiment design routes (api/src/routes/experiment_design.jl), one function each. Shapes in
// `experimentDesign.ts`.

import { ref } from 'vue'
import type { DesignReply, ExperimentDesign } from './experimentDesign'
import { getJson, postJson } from './api'

/** Bumped on every save and attribute rename, so a view showing a design (the Design check, a plot
 *  canvas) reloads after a change made elsewhere. */
export const designSaves = ref(0)

const post = (url: string, body: unknown) => postJson<DesignReply>(url, body)

/** The set's design, version, history and grid (`design` null = none). */
export const fetchDesign = (projectUid: string, setUid: string) =>
  getJson<DesignReply>('/api/sets/experiment-design', { query: { projectUid, setUid } })

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
