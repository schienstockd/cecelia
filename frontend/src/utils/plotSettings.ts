// The settings a plot panel lets Claude change (MCP `set_plot_view`) and reports with its picture
// (`get_plot_image`). A panel DECLARES each one — its live value, its allowed values — so a change is
// checked against what the panel's own controls offer, never written blind into its saved state.
// Declared by the plot (ClusterMapView, SummaryPanel …); passed to `usePanelExport` (stores/canvasPanelExports.ts).

type Prim = string | number | boolean

export interface PlotSetting {
  get(): unknown
  set(v: unknown): void
  /** the allowed values (each one, for a `multi` setting); absent = any value of the current type */
  options?: () => Prim[] | Promise<Prim[]>
  /** a list of values (e.g. the populations to colour by), not one */
  multi?: boolean
}
export type PlotSettings = Record<string, PlotSetting>

/** A setting over a writable ref (`computed({get, set})` — what the panel's own control writes). */
export function setting(r: { value: unknown }, options?: PlotSetting['options'], multi = false): PlotSetting {
  return { get: () => r.value, set: v => { r.value = v }, ...(options ? { options } : {}), ...(multi ? { multi } : {}) }
}

export interface SettingDescription { value: unknown; options?: Prim[]; multi?: true }

/** What the panel reports: each setting's value and allowed values. */
export async function describeSettings(s: PlotSettings): Promise<Record<string, SettingDescription>> {
  const out: Record<string, SettingDescription> = {}
  for (const [k, d] of Object.entries(s)) {
    const v = d.get()
    out[k] = { value: Array.isArray(v) ? [...v] : v,
               ...(d.options ? { options: [...await d.options()] } : {}), ...(d.multi ? { multi: true as const } : {}) }
  }
  return out
}

const isPrim = (v: unknown): v is Prim => typeof v === 'string' || typeof v === 'number' || typeof v === 'boolean'

/** Check `patch` against the declared settings: which values to write, and why each other key was
 *  refused. Nothing is written here — the caller applies `apply` in order. */
export async function checkSettingsPatch(s: PlotSettings, patch: unknown):
    Promise<{ apply: [string, unknown][]; rejected: Record<string, string> }> {
  const apply: [string, unknown][] = []
  const rejected: Record<string, string> = {}
  if (!patch || typeof patch !== 'object' || Array.isArray(patch)) return { apply, rejected: { '': 'settings must be an object' } }
  for (const [k, v] of Object.entries(patch as Record<string, unknown>)) {
    const d = s[k]
    if (!d) { rejected[k] = `not a setting of this plot (has: ${Object.keys(s).join(', ') || 'none'})`; continue }
    const opts = d.options ? await d.options() : null
    if (d.multi) {
      if (!Array.isArray(v) || !v.every(isPrim)) { rejected[k] = 'expects a list'; continue }
      const bad = opts ? v.filter(x => !opts.includes(x)) : []
      if (bad.length) { rejected[k] = `not offered: ${bad.join(', ')}`; continue }
      apply.push([k, [...v]])
      continue
    }
    if (!isPrim(v)) { rejected[k] = 'expects one value'; continue }
    if (opts) {
      if (!opts.includes(v)) { rejected[k] = `not offered (one of: ${opts.join(', ')})`; continue }
    } else {
      const cur = d.get()
      if (cur != null && typeof cur !== typeof v) { rejected[k] = `expects a ${typeof cur}`; continue }
    }
    apply.push([k, v])
  }
  return { apply, rejected }
}
