// Shared workstation — install-wide policies for a box several people use. Backend:
// `app/src/config/shared_workstation.jl` (`[sharedWorkstation]` in custom.toml), routes
// GET /api/config/shared-workstation and POST /api/config/shared-workstation/set.
//
// Settings shows the section to the default profile only (`SettingsModule.vue`) — a
// visibility gate, not security. Each item is its own toggle; there is no master "shared machine"
// switch. To add an item: one entry in SHARED_WORKSTATION_ITEMS on BOTH sides.

export interface SharedWorkstationItem {
  key: string
  label: string
  tip: string      // tooltip on the toggle
  hint: string     // the line under it
}

export const SHARED_WORKSTATION_ITEMS: readonly SharedWorkstationItem[] = [
  {
    key: 'oneClaudeLogin',
    label: 'One Claude login at a time',
    tip: 'Picking a profile logs every other profile out of Claude',
    hint: 'Includes peanut — and so terminal claude on this account',
  },
]

export interface SharedWorkstationRow extends SharedWorkstationItem { value: boolean }

/** The catalogue joined with the server's values. A key the server doesn't know yet (older
 *  backend) is dropped rather than shown as a toggle that can't be saved; a server key with no
 *  catalogue entry is ignored (no label to show). */
export function sharedWorkstationRows(items: Record<string, unknown>): SharedWorkstationRow[] {
  return SHARED_WORKSTATION_ITEMS
    .filter(it => typeof items[it.key] === 'boolean')
    .map(it => ({ ...it, value: items[it.key] as boolean }))
}

export async function fetchSharedWorkstation(): Promise<Record<string, boolean>> {
  const res = await fetch('/api/config/shared-workstation')
  const data = await res.json().catch(() => ({}))
  if (!res.ok) throw new Error((data as any)?.error ?? `HTTP ${res.status}`)
  return (data as any).items ?? {}
}

export async function setSharedWorkstation(key: string, value: boolean): Promise<Record<string, boolean>> {
  const res = await fetch('/api/config/shared-workstation/set', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ key, value }),
  })
  const data = await res.json().catch(() => ({}))
  if (!res.ok) throw new Error((data as any)?.error ?? `HTTP ${res.status}`)
  return (data as any).items ?? {}
}
