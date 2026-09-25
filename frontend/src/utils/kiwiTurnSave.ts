// KIWI_CAPTURE_AND_BLACKBOARD_PLAN P2 — pure builder that turns a Kiwi turn (or one of its claims)
// into the shape `POST /api/blackboard/create` accepts. Two outputs:
//
//   • `markdown` — a structured entry.md body: the question, the claims (with `kind` prefix), a
//     following-up note, and Kiwi's `note` line. Human-readable in a plain editor; the discipline
//     is the same as `renderBlackboardMarkdown` reads today.
//   • `kiwiRefs` — a per-ref sidecar map (`{refKey → {ref, label, snapshot?, savedAt}}`) for the
//     SIX FRAGILE kinds only (Decision 7): population, cells, tracks, plot, tile, ui. The other
//     six (project/set/image/viewer/task/blackboard/proposedPlot) resolve-or-say-"gone" without a
//     snapshot — their identifiers are already stable enough (Decision 7's table).
//
// The sidecar lets the chip fall back to "was: <label> · <snapshot>" when the underlying object is
// gone months later (`KiwiRefChip`). It also rides in `meta.json` as an additive field, so no
// Blackboard schema change (Decision 9).
//
// Pure. Tests in `kiwiTurnSave.test.ts` — no DOM, no store access.

import type { KiwiRef } from './kiwiRef'
import type { KiwiTurn, KiwiClaim, KiwiClaimRef } from './kiwiTurn'
import { refKey, refLabel } from './kiwiTurn'

/** One entry in the `kiwiRefs` sidecar. Keyed by `refKey(ref)`. `snapshot` present only for the six
 *  fragile kinds; absent for the stable kinds (resolve-or-"gone"). */
export interface KiwiRefSidecar {
  ref: KiwiRef
  label: string
  snapshot?: KiwiRefSnapshot
  savedAt: string
}

/** Per-kind snapshot payload. Discriminated by kind so a reader can render kind-specific fallback
 *  text ("was: cells 1, 2, 3 · default in image_005"). Keep small — `plotSummary` is the outlier
 *  at up to 12 KB (`plotSummaryText`'s cap), which Decision 8 accepts. */
export type KiwiRefSnapshot =
  | { kind: 'population'; imageName: string; label: string }
  | { kind: 'cells';      imageName: string; count: number }
  | { kind: 'tracks';     imageName: string; count: number }
  | { kind: 'plot';       plotSummary: string; label: string }
  | { kind: 'tile';       imageName: string; cellId: string }
  | { kind: 'ui';         anchor: string }

export type KiwiRefsSidecar = Record<string, KiwiRefSidecar>

/** Callbacks the builder needs from the caller. Kept as functions so the builder stays pure and
 *  the caller supplies live store data (plot registry, project meta) at save time. */
export interface KiwiSaveCallbacks {
  /** Human name for an image uid — the project meta store answers this in the caller. Fallback to
   *  the uid on unknown so a saved entry never shows an empty image name. */
  imageName: (uid: string) => string
  /** The plot's `plotSummary` from `usePlotRegistryStore().getLast(plotId)?.meta.summary`. Returns
   *  `''` when the plot's panel is closed and no summary was ever registered — the sidecar falls
   *  through to a bare label in that case. */
  plotSummary: (plotId: string) => string
}

/** Build a sidecar entry for one ref if it's a fragile kind; return `null` for a stable kind (no
 *  sidecar needed — live resolve-or-"gone" is enough per Decision 7). `savedAtISO` is threaded from
 *  the top so every entry on one turn shares one timestamp — no time-of-flight drift within a
 *  single Save. */
export function snapshotForRef(
  ref: KiwiRef,
  cb: KiwiSaveCallbacks,
  savedAtISO: string,
): KiwiRefSidecar | null {
  const label = refLabel(ref)
  switch (ref.kind) {
    case 'population':
      return { ref, label, savedAt: savedAtISO,
               snapshot: { kind: 'population', imageName: cb.imageName(ref.imageUid), label } }
    case 'cells':
      return { ref, label, savedAt: savedAtISO,
               snapshot: { kind: 'cells', imageName: cb.imageName(ref.imageUid), count: ref.labelIds.length } }
    case 'tracks':
      return { ref, label, savedAt: savedAtISO,
               snapshot: { kind: 'tracks', imageName: cb.imageName(ref.imageUid), count: ref.trackIds.length } }
    case 'plot':
      return { ref, label, savedAt: savedAtISO,
               snapshot: { kind: 'plot', plotSummary: cb.plotSummary(ref.plotId), label } }
    case 'tile':
      return { ref, label, savedAt: savedAtISO,
               snapshot: { kind: 'tile', imageName: cb.imageName(ref.imageUid), cellId: ref.cellId } }
    case 'ui':
      return { ref, label, savedAt: savedAtISO,
               snapshot: { kind: 'ui', anchor: ref.anchor } }
    // Stable kinds — Decision 7. No sidecar; the chip resolves live or shows "gone".
    default: return null
  }
}

/** Collect sidecars for every ref cited by the given claims + top-level turn refs. Deduped by
 *  `refKey`, keyed by the same string. Only fragile kinds appear. */
function collectSidecars(
  turnRefs: readonly { ref: KiwiRef }[],
  claimRefs: readonly KiwiClaimRef[],
  cb: KiwiSaveCallbacks,
  savedAtISO: string,
): KiwiRefsSidecar {
  const out: KiwiRefsSidecar = {}
  const consider = (ref: KiwiRef) => {
    const k = refKey(ref)
    if (out[k]) return
    const snap = snapshotForRef(ref, cb, savedAtISO)
    if (snap) out[k] = snap
  }
  for (const r of turnRefs) consider(r.ref)
  for (const r of claimRefs) consider(r.ref)
  return out
}

const KIND_LABEL: Record<KiwiClaim['kind'], string> = {
  observation: 'observation',
  interpretation: 'interpretation',
  question: 'question',
}

/** Render one claim as a Markdown bullet — kind prefix, text, and a compact `— refs: label, label`
 *  tail so the chips render below map back to specific claim rows. Escapes markdown-active leading
 *  characters in `text` (`*`, `#`, `-`, `>`) so a claim starting with one doesn't hijack layout. */
function claimBullet(c: KiwiClaim, resolved: (r: KiwiRef) => string): string {
  const text = c.text.replace(/^([*#\->])/, '\\$1')
  const refs = c.refs.map(x => resolved(x.ref)).filter(Boolean)
  const tail = refs.length ? `  \n  _refs:_ ${refs.map(l => `\`${l}\``).join(', ')}` : ''
  return `- **${KIND_LABEL[c.kind]}** — ${text}${tail}`
}

/** Build the entry.md body for a whole-turn save. Header → optional follow-up note → question →
 *  Kiwi's own note line → claims. Keep the markdown structured and small; a reader who opens the
 *  raw file in an editor can follow it. */
function turnMarkdown(t: KiwiTurn, resolved: (r: KiwiRef) => string): string {
  const parts: string[] = []
  parts.push(`# Kiwi turn — ${t.startedAt}`)
  if (t.followUp) parts.push(`_Follow-up to ${t.followUp}._`)
  const q = (t.prompt || '').trim()
  parts.push(`## Question\n${q || '_(attachments only — no question text)_'}`)
  if (t.refs.length) {
    const list = t.refs.map(r => `- \`${resolved(r.ref)}\``).join('\n')
    parts.push(`## Attachments\n${list}`)
  }
  const reply = t.reply
  if (reply?.note) parts.push(`> ${reply.note}`)
  if (reply?.claims?.length) {
    const bullets = reply.claims.map(c => claimBullet(c, resolved)).join('\n')
    parts.push(`## Claims\n${bullets}`)
  } else if (reply?.abstain) {
    parts.push(`## Claims\n_Kiwi abstained from claiming based on what it looked at._`)
  }
  return parts.join('\n\n') + '\n'
}

/** Build the entry.md body for a single-claim extract. Same header + claim as a whole-turn save,
 *  minus the sibling claims. */
function claimMarkdown(t: KiwiTurn, claim: KiwiClaim, resolved: (r: KiwiRef) => string): string {
  const parts: string[] = []
  parts.push(`# Kiwi claim — ${t.startedAt}`)
  const q = (t.prompt || '').trim()
  parts.push(`## Question\n${q || '_(attachments only — no question text)_'}`)
  parts.push(`## Claim\n${claimBullet(claim, resolved)}`)
  return parts.join('\n\n') + '\n'
}

/** A short title for the Blackboard entry list — first line of the question (or first claim text)
 *  capped so the row stays legible. */
function shortTitle(base: string, fallback: string, cap = 80): string {
  const first = base.split(/\r?\n/, 1)[0].trim() || fallback
  return first.length > cap ? first.slice(0, cap - 1) + '…' : first
}

export interface KiwiSavePayload {
  title: string
  content: string
  attachments: string[]
  kiwiRefs: KiwiRefsSidecar
}

/** Whole-turn Save (Decision 6 primary). The `capture` kind isn't a fragile-kind (no sidecar) but
 *  is collected into `attachments` so the entry surfaces its capture chips the same way any other
 *  Blackboard entry does. */
export function buildTurnSave(
  turn: KiwiTurn,
  cb: KiwiSaveCallbacks,
  now: Date = new Date(),
): KiwiSavePayload {
  const savedAt = now.toISOString()
  const claimRefs: KiwiClaimRef[] = (turn.reply?.claims ?? []).flatMap(c => c.refs)
  const kiwiRefs = collectSidecars(turn.refs, claimRefs, cb, savedAt)
  const resolved = (r: KiwiRef) => refLabel(r)
  const content = turnMarkdown(turn, resolved)
  const attachments = extractCaptureIds([...turn.refs.map(x => x.ref), ...claimRefs.map(x => x.ref)])
  const title = shortTitle(turn.prompt, 'Kiwi turn')
  return { title, content, attachments, kiwiRefs }
}

/** Per-claim Save (Decision 6 secondary). Same sidecar discipline as the whole-turn save — the
 *  turn's top-level refs are dropped and only the claim's own refs contribute. */
export function buildClaimSave(
  turn: KiwiTurn,
  claim: KiwiClaim,
  cb: KiwiSaveCallbacks,
  now: Date = new Date(),
): KiwiSavePayload {
  const savedAt = now.toISOString()
  const kiwiRefs = collectSidecars([], claim.refs, cb, savedAt)
  const resolved = (r: KiwiRef) => refLabel(r)
  const content = claimMarkdown(turn, claim, resolved)
  const attachments = extractCaptureIds(claim.refs.map(x => x.ref))
  const title = shortTitle(claim.text, 'Kiwi claim')
  return { title, content, attachments, kiwiRefs }
}

function extractCaptureIds(refs: readonly KiwiRef[]): string[] {
  const seen = new Set<string>()
  const out: string[] = []
  for (const r of refs) {
    if (r.kind !== 'capture') continue
    if (seen.has(r.captureId)) continue
    seen.add(r.captureId)
    out.push(r.captureId)
  }
  return out
}
