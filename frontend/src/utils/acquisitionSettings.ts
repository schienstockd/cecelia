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
}
export interface AcquisitionSettings {
  system?: string | null
  softwareVersion?: string | null
  objective?: { name?: string | null; magnification?: number | null; na?: number | null; immersion?: string | null } | null
  lasers?: AcquisitionLaser[]
  brightZ?: { enabled: boolean | null; points: { zFromStartUm: number | null; transmissionPct: Record<string, number | null> }[] } | null
  scanner?: {
    type?: string | null; zoom?: number | null; pixelDwellUs?: number | null
    integration?: { type?: string | null; count?: number | null; average?: boolean | null } | null
  } | null
  autoHV?: boolean | null
  pinholeUm?: number | null
  channels?: AcquisitionChannel[]
  phaseCount?: number | null
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
  if (acq.system) rows.push({ k: 'Microscope', v: join([acq.system, acq.softwareVersion && `software ${acq.softwareVersion}`]) })
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
    rows.push({ k: c.name ?? `Channel ${c.order}`, v: join([c.dyePreset, c.laser, c.detector, c.hv != null && `HV ${n(c.hv)}`]) })
  }
  return rows
}
