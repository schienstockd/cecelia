/**
 * Which models the vault manager lists — by user, by project, by search.
 *
 * The vaults are per INSTALL, not per project (`<config_dir>/models/…`). One person in one project never
 * notices; on a shared machine every user's models from every project land in one list. So the
 * manager lists YOUR models from the OPEN project by default, and two toggles widen it. Same shape as
 * the Task Manager's "This project" (`utils/taskScope.ts`), with the same kind of exception:
 *
 * - **Unknown is never out of scope.** A model trained before the stamp (`Cecelia.vault_origin_stamp`)
 *   has no creator, and one whose source images are gone has no project. Hiding those would empty the
 *   vault of everything trained so far; "nobody else's" is the honest reading.
 *
 * The server resolves the origin onto the row (`vault_model_origin`, api/src/vault_api.jl), including
 * the project of a pre-stamp model, recovered from its source image uids.
 */
import { profileNameLabel } from './profileApi'
import type { DetailGroup } from './flowManifest'

/** The origin fields of a vault row. Structural, so `ModelVault`'s row type satisfies it. */
export interface VaultOrigin {
  createdBy?: string
  createdVia?: string
  projectUid?: string
  projectName?: string
  projectInferred?: boolean
}

export interface VaultScope {
  profile: string
  projectUid: string | undefined | null
  otherUsers: boolean
  otherProjects: boolean
}

export function vaultInScope(m: VaultOrigin, s: VaultScope): boolean {
  if (!s.otherUsers && m.createdBy && m.createdBy !== s.profile) return false
  if (!s.otherProjects && s.projectUid && m.projectUid && m.projectUid !== s.projectUid) return false
  return true
}

/** Who trained it, as the list shows it — '' when not recorded. The default profile goes by its
 *  display name rather than blank: in a list that mixes users, blank would read as "unknown". */
export const vaultUserLabel = (m: VaultOrigin): string => m.createdBy
  ? profileNameLabel(m.createdBy) : ''

/** The project as the list shows it: its name, else its uid (it may have been deleted), else ''. */
export const vaultProjectLabel = (m: VaultOrigin): string => m.projectName || m.projectUid || ''

/** Case-insensitive substring over everything a row shows: name, channels, user, project. */
export function vaultMatchesQuery(m: VaultOrigin & { stem: string; label?: string },
                                  query: string): boolean {
  const q = query.trim().toLowerCase()
  if (!q) return true
  return [m.stem, m.label, m.createdBy, vaultUserLabel(m), m.projectName, m.projectUid]
    .some(v => !!v && v.toLowerCase().includes(q))
}

/** The details modal's Origin group — recorded or recovered, said which. */
export function vaultOriginGroup(m: VaultOrigin): DetailGroup {
  const fields: DetailGroup['fields'] = []
  const user = vaultUserLabel(m)
  const who = user && m.createdVia === 'claude' ? `Claude for ${user}` : user
  fields.push({ label: 'Trained by', value: who || 'not recorded' })
  const proj = vaultProjectLabel(m)
  fields.push({ label: 'Project', value: proj
    ? (m.projectInferred ? `${proj} (from its source images)` : proj)
    : 'not recorded' })
  return { label: 'Origin', fields }
}
