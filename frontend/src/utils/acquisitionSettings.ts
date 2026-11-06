/**
 * The source file's acquisition settings (`image.acquisition`, read at import by
 * `python/cecelia/utils/oir_meta.py`) turned into label/value rows for the image metadata dialog.
 * Values are shown as recorded; nothing here judges them.
 */

import { fmtNum } from './physicalSize'

export interface AcquisitionLaser { name: string; wavelengthNm?: number | null; transmissionPct?: number | null }
export interface AcquisitionChannel {
  order: number; name: string | null; dyePreset: string | null; laser: string | null
  detector: string | null; hv: number | null; gain: number | null; offset: number | null
  bitDepth?: number | null
  /** Barrier (emission) filter in front of this channel's detector, as the file names it. */
  emissionFilter?: string | null
}
/** A dichroic or mirror cube in the path. `slot` is the vendor's element name (`oir_meta._dichroics`). */
export interface AcquisitionDichroic { slot: string; name: string }
export interface AcquisitionSettings {
  /** `oir_meta.READER_VERSION`; absent on settings read before it existed. */
  readerVersion?: number | null
  system?: string | null
  softwareVersion?: string | null
  /** As recorded: ISO 8601 with the acquisition PC's UTC offset. */
  acquiredAt?: string | null
  /** Microscope stand, e.g. "BX63LF". */
  microscopeBody?: string | null
  dichroics?: AcquisitionDichroic[]
  objective?: { name?: string | null; magnification?: number | null; na?: number | null; immersion?: string | null } | null
  lasers?: AcquisitionLaser[]
  brightZ?: { enabled: boolean | null; points: { zFromStartUm: number | null; transmissionPct: Record<string, number | null> }[] } | null
  scanner?: {
    type?: string | null; zoom?: number | null; pixelDwellUs?: number | null
    integration?: { type?: string | null; count?: number | null; average?: boolean | null } | null
    kalman?: { average?: number | null; accumulate?: number | null } | null
  } | null
  autoHV?: boolean | null
  pinholeUm?: number | null
  channels?: AcquisitionChannel[]
  phaseCount?: number | null
  zStack?: { stepUm?: number | null; count?: number | null } | null
  /** `acquired` is set when the store has fewer frames than the file planned (stopped early, or cropped). */
  timelapse?: { intervalS?: number | null; count?: number | null; acquired?: number | null } | null
}

/** Why an image has no settings (`ACQUISITION_MISSING` in omezarr/acquisition.jl). */
export interface AcquisitionMissing { reason: 'format' | 'source' | 'unread' | 'other-stack' | 'registered' | 'no-source'; ext?: string }

/** "FVMPE-RS · software 2.6.1.243", or null when the file does not name the system. */
export function microscopeName(acq: AcquisitionSettings | null | undefined): string | null {
  return acq?.system ? join([acq.system, acq.softwareVersion && `software ${acq.softwareVersion}`]) : null
}

/** The dialog's one-line header over the diagram: "FV4000 · software 3.2.1.85 · BX63LF · 2026-07-16 13:44". */
export function microscopeLine(acq: AcquisitionSettings | null | undefined): string | null {
  const name = microscopeName(acq)
  return name ? join([name, acq?.microscopeBody, acquiredAtText(acq)]) : null
}

/** "2026-07-16 13:44" — the acquisition PC's local time as recorded, not converted to this one's. */
export function acquiredAtText(acq: AcquisitionSettings | null | undefined): string | null {
  const m = acq?.acquiredAt?.match(/^(\d{4}-\d{2}-\d{2})T(\d{2}:\d{2})/)
  return m ? `${m[1]} ${m[2]}` : null
}

/** "21 of 45 planned frames" when a time-lapse stopped early (or was cropped), else null. */
export function framesShortOf(acq: AcquisitionSettings | null | undefined): string | null {
  const t = acq?.timelapse
  return t?.acquired != null && t.count != null && t.acquired < t.count ? `${t.acquired} of ${n(t.count)} planned frames` : null
}

export interface AcquisitionNotice { text: string; action?: { label: string; url: string } }

/** What a "request your microscope" issue asks for: a settings reader for a file type, or the diagram
 *  and settings card for a microscope whose settings are read but which is not two-photon. */
export type MicroscopeRequest =
  | { what: 'settings'; ext: string }
  | { what: 'diagram'; system: string | null; softwareVersion: string | null }

/**
 * What the Acquisition section says instead of (or under) the settings, so it never silently vanishes:
 * why there are no settings, or why there is no diagram (and so no settings card). `requestUrl(req)`
 * builds the "support this microscope" issue link (lib/links.ts) for both.
 */
export function acquisitionNotice(
  acq: AcquisitionSettings | null | undefined, missing: AcquisitionMissing | null | undefined,
  hasDiagram: boolean, requestUrl: (req: MicroscopeRequest) => string,
): AcquisitionNotice | null {
  if (acq) {
    if (hasDiagram) return null
    const req: MicroscopeRequest = { what: 'diagram', system: acq.system ?? null, softwareVersion: acq.softwareVersion ?? null }
    return { text: 'Light-path diagram and settings card: two-photon only so far',
             action: { label: 'Request your microscope', url: requestUrl(req) } }
  }
  switch (missing?.reason) {
    case 'format': {
      const ext = missing.ext || 'this file type'
      return { text: `Settings are not read from ${ext} files yet`,
               action: { label: 'Request your microscope', url: requestUrl({ what: 'settings', ext }) } }
    }
    case 'source': return { text: 'Source file not reachable: refresh metadata when it is' }
    case 'unread': return { text: 'No readable settings in the source file' }
    case 'other-stack': return { text: 'Source settings describe another stack (a projection, crop or resample)' }
    case 'registered': return { text: 'Registered from several cycles: no single source file' }
    case 'no-source': return { text: 'No original file recorded' }
    default: return { text: 'Not read yet: refresh metadata' }
  }
}

export interface AcquisitionRow { k: string; v: string }

const n = (x: number | null | undefined): string => (x == null ? '—' : fmtNum(x))

const join = (parts: (string | null | undefined | false)[]) => parts.filter(Boolean).join(' · ')

/** "MaiTai 14.4 → 21 %" per laser over "0 → 60 µm" — every stored point, so a ramp that peaks
 *  mid-stack reads as one ("10 → 40 → 20 %"), not as its two ends. */
function brightZRamp(bz: NonNullable<AcquisitionSettings['brightZ']>): string {
  const pts = bz.points ?? []
  if (pts.length < 2) return ''
  const lasers = Object.keys(pts[0].transmissionPct ?? {})
  const depths = pts.map(p => n(p.zFromStartUm)).join(' → ')
  return lasers.map(l => `${l} ${pts.map(p => n(p.transmissionPct[l])).join(' → ')} %`)
    .join('; ') + ` over ${depths} µm`
}

export function acquisitionRows(acq: AcquisitionSettings | null | undefined): AcquisitionRow[] {
  if (!acq) return []
  const rows: AcquisitionRow[] = []
  const scope = microscopeName(acq)
  if (scope) rows.push({ k: 'Microscope', v: scope })
  const when = acquiredAtText(acq)
  if (when) rows.push({ k: 'Acquired', v: when })
  const short = framesShortOf(acq)
  if (short) rows.push({ k: 'Time-lapse', v: `${short} (stopped early or cropped)` })
  if ((acq.phaseCount ?? 1) > 1) rows.push({ k: 'Phases', v: `${acq.phaseCount} · first shown` })
  const o = acq.objective
  if (o) rows.push({ k: 'Objective', v: join([o.name, o.magnification != null && `${n(o.magnification)}×`, o.na != null && `NA ${n(o.na)}`, o.immersion]) })
  for (const l of acq.lasers ?? []) {
    rows.push({ k: 'Laser', v: join([l.name, l.wavelengthNm != null && `${n(l.wavelengthNm)} nm`, l.transmissionPct != null && `${n(l.transmissionPct)} %`]) })
  }
  const bz = acq.brightZ
  if (bz && bz.enabled != null) {
    rows.push({ k: 'Bright Z', v: bz.enabled ? join(['on', brightZRamp(bz)]) : 'off' })
  }
  const s = acq.scanner
  if (s) {
    rows.push({ k: 'Scanner', v: join([s.type?.toLowerCase(), s.zoom != null && `zoom ${n(s.zoom)}×`, s.pixelDwellUs != null && `dwell ${n(s.pixelDwellUs)} µs`]) })
    const integ = s.integration
    if (integ?.type) {
      const how = integ.average === false ? 'sum' : integ.average === true ? 'average' : null
      rows.push({ k: 'Integration', v: integ.type === 'None' ? 'none' : join([`${integ.type.toLowerCase()} ×${n(integ.count)}`, how]) })
    }
  }
  if (acq.autoHV != null) rows.push({ k: 'Auto HV', v: acq.autoHV ? 'on' : 'off' })
  if (acq.pinholeUm != null) rows.push({ k: 'Pinhole', v: `${n(acq.pinholeUm)} µm` })
  for (const c of acq.channels ?? []) {
    // Under Auto HV the recorded HV is not the one used (the Auto HV row says so), so it is not listed.
    rows.push({ k: c.name ?? `Channel ${c.order}`, v: join([c.dyePreset, c.laser, c.detector, c.emissionFilter,
      acq.autoHV !== true && c.hv != null && `HV ${n(c.hv)}`]) })
  }
  return rows
}
