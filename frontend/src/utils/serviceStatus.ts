// Pure helpers for the Settings "System" control panel: normalise each service's raw status payload
// into a single {running|starting|stopped} state + a display label/tone. Kept out of the SFC so it's
// unit-testable (see serviceStatus.test.ts) — the component only maps state → which buttons to show.

export type ServiceState = 'running' | 'starting' | 'stopped'
export type Tone = 'ok' | 'warn' | 'idle'

/** GET /api/napari/status → { alive, starting } */
export function napariState(s: { alive?: boolean; starting?: boolean } | null | undefined): ServiceState {
  if (!s) return 'stopped'
  if (s.starting) return 'starting'
  return s.alive ? 'running' : 'stopped'
}

/** GET /api/notebooks/status → { running, starting, … } */
export function notebooksState(s: { running?: boolean; starting?: boolean } | null | undefined): ServiceState {
  if (!s) return 'stopped'
  if (s.starting) return 'starting'
  return s.running ? 'running' : 'stopped'
}

/** Human uptime from a whole-second count: "45s", "12m", "3h 4m". "—" for missing/invalid. Used to show
 *  how long the backend / napari bridge has been up (spotting a stale process that didn't restart). */
export function formatUptime(seconds: number | null | undefined): string {
  if (seconds == null || !Number.isFinite(seconds) || seconds < 0) return '—'
  const s = Math.floor(seconds)
  if (s < 60) return `${s}s`
  const m = Math.floor(s / 60)
  if (m < 60) return `${m}m`
  const h = Math.floor(m / 60)
  return `${h}h ${m % 60}m`
}

/** Display label + colour tone for a state pill. */
export function stateInfo(state: ServiceState): { label: string; tone: Tone } {
  switch (state) {
    case 'running':  return { label: 'Running',   tone: 'ok' }
    case 'starting': return { label: 'Starting…', tone: 'warn' }
    default:         return { label: 'Stopped',   tone: 'idle' }
  }
}
