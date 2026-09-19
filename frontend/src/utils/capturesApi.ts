// Client for the share-in capture surface (`api/src/captures_api.jl`, PR #3 of
// `docs/todo/BIDIR_CONTEXT_PLAN.md`). Pure fetch helpers — no reactivity, no store bindings —
// so the shape can be unit-tested without a browser and the callers (ViewerPanel's shared-frames
// strip + CapturePreviewModal) share ONE code path against the endpoint.
//
// The endpoints are the READ side of what Claude sees via `get_recent_captures` / `get_capture`.
// Deliberately no write helper — the POST route is authored by the pop-out viewer's Save flow
// (ViewerWindow.vue) and never by us; that boundary is what keeps the observer-side allow-list
// honest (see `mcp/cecelia_mcp/client.py::ALLOWED_ROUTES`).

// Same shape the Julia handler returns; kept loose (typed only where the frontend touches
// something specific) so a future field addition doesn't force a schema churn here.
export interface CaptureAddress {
  projectUid: string
  imageUid?: string
  valueName?: string
  t?: number | [number, number]
  z?: number
  extentUm?: { x: number; y: number; unit?: string | null }
  domAnchor?: string
}
export interface CaptureListItem {
  captureId: string
  createdAt: number         // unix seconds
  surface: string           // "viewer_frame" today; PR #5+ will add other kinds
  address: CaptureAddress
}
export interface CaptureOverlayEntry {
  kind: string              // rect / poly / stroke / circle / arrow — see freeformRender.KNOWN_KINDS
  geom: unknown
  label?: string
}
export interface CaptureFull {
  captureId: string
  createdAt: number
  surface: string
  address: CaptureAddress
  overlay: CaptureOverlayEntry[]
}
export interface CaptureEnvelope {
  capture: CaptureFull
  frame: string             // "data:image/png;base64,…" — inlined so ONE request paints the modal
}

// The list endpoint is newest-first — the strip UI reads position 0 as "the last thing shared"
// and shows the top N as thumbnails.
export async function listCaptures(projectUid: string, limit = 10, signal?: AbortSignal):
    Promise<CaptureListItem[]> {
  if (!projectUid) return []
  const url = `/api/viewer/captures?projectUid=${encodeURIComponent(projectUid)}&limit=${limit}`
  const res = await fetch(url, { signal })
  if (!res.ok) throw new Error(`captures list HTTP ${res.status}`)
  const j = await res.json() as { items?: unknown }
  return Array.isArray(j.items) ? (j.items as CaptureListItem[]) : []
}

// The get endpoint returns the frame inline so the modal doesn't need a second request for the
// PNG — a captureId that renders takes ONE round-trip.
export async function getCapture(projectUid: string, captureId: string, signal?: AbortSignal):
    Promise<CaptureEnvelope | null> {
  if (!projectUid || !captureId) return null
  const url = `/api/viewer/capture?projectUid=${encodeURIComponent(projectUid)}`
             + `&captureId=${encodeURIComponent(captureId)}`
  const res = await fetch(url, { signal })
  if (res.status === 404) return null
  if (!res.ok) throw new Error(`capture read HTTP ${res.status}`)
  return await res.json() as CaptureEnvelope
}

// Format the address into a short human line — same convention `ViewerWindow.vue::drawAddressLine`
// uses for the toolbar caption. Kept here so the modal + the strip render it identically.
export function addressLine(a: CaptureAddress | null | undefined): string {
  if (!a) return ''
  const bits: string[] = []
  if (a.imageUid)  bits.push(a.imageUid)
  if (a.valueName) bits.push(a.valueName)
  if (typeof a.t === 'number') bits.push(`t=${a.t}`)
  else if (Array.isArray(a.t)) bits.push(`t=${a.t[0]}..${a.t[1]}`)
  if (typeof a.z === 'number') bits.push(`z=${a.z}`)
  return bits.join(' · ')
}

// `createdAt` is unix seconds. Short label for the strip: "just now" / "5 min ago" / "2 h ago" /
// absolute date past a day. Deliberately not internationalised — the whole app is en-AU today.
export function relativeAgo(createdAt: number, nowSec: number = Date.now() / 1000): string {
  const dt = Math.max(0, nowSec - createdAt)
  if (dt < 60)    return 'just now'
  if (dt < 3600)  return `${Math.round(dt / 60)} min ago`
  if (dt < 86400) return `${Math.round(dt / 3600)} h ago`
  const d = new Date(createdAt * 1000)
  return d.toLocaleDateString()
}

// Cross-window ping: ViewerWindow's Save writes here so ViewerPanel (running in the OPENER window)
// knows to refetch the list without polling. Using localStorage + `storage` event is the same
// pattern `stores/viewer.ts` uses for its other cross-window bumps — pop-out doesn't `focus` back
// on save, so a `focus` refetch alone would miss the new capture until the user alt-tabs.
const K_CAPTURES_TICK = 'cc.viewer.capturesTick'
export function publishCapturesTick(): void {
  if (typeof window === 'undefined') return
  try { window.localStorage.setItem(K_CAPTURES_TICK, String(Date.now())) }
  catch { /* private mode / quota — drop, same as other viewer bumps */ }
}
export function capturesTickKey(): string { return K_CAPTURES_TICK }
