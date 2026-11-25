// Which projects a profile sees by default (USER_PROFILE_PLAN Phase 5): its own (`owners` lists it)
// plus shared ones (no owners — pre-identity, or shared on purpose). The open project always counts,
// so whoever has it in front of them can always get back to it. Used by the Project manager's "Mine"
// scope and the Welcome page's recent list; other profiles' projects stay reachable via "All".

export function visibleToProfile(p: { uid: string; owners?: string[] }, active: string,
                                 openUid?: string | null): boolean {
  if (openUid && p.uid === openUid) return true
  const os = p.owners
  return !os || os.length === 0 || os.includes(active)
}
