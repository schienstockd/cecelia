// Compact form of `/api/update/check`'s `current` string for the header version chip. The server
// returns `.cecelia-version` verbatim (see api/src/update_api.jl `_installed_version_provenance`),
// which is either a stable tag ("v0.2.4") or a dev provenance line ("dev @ main 1a2b3c4",
// "dev @ feat/foo abcdef1", or the source-checkout fallback "dev (source checkout)"). The header
// has ~20 chars of comfortable room next to the ws-badge, so the long dev form gets shortened;
// stable tags and unknown shapes pass through unchanged. Tooltip in AppHeader.vue still shows the
// full string so nothing is lost.

// Matches the format `_installed_version_provenance` writes:
//   "dev @ <branch> <sha7-or-longer>"
// Branch is `_valid_branch`-shaped (letters, digits, _.-/), sha is short-or-full (7+ hex).
const DEV_RE = /^dev @ (\S+) ([0-9a-f]{7,40})$/i

export function shortVersionLabel(raw: string): string {
  const s = (raw ?? '').trim()
  if (!s) return ''
  const m = DEV_RE.exec(s)
  if (!m) return s
  const [, branch, sha] = m
  const short = sha.slice(0, 7)
  // On main (the common dev case), the branch adds nothing — the whole point of the dev channel is
  // "main HEAD". Off-main dev builds keep the branch because that's the info that matters most.
  return branch === 'main' ? `dev@${short}` : `${branch}@${short}`
}
