// Shared CLUSTER context — the run list, per-run feature/cluster metadata, valid-image resolution, the
// gating-store drive that loads a run's pop tree set-wide, and highlight→shownPops resolution. Extracted
// from ClusterPlots so the Analysis canvas can host the cluster UMAP/heatmap without a divergent
// re-implementation (feedback_use_existing_framework). Both callers own the `suffix` ref (page-level on
// the cluster module; board-level on the analysis canvas — one run per board) and pass it in.
//
// NB: cluster pops live in the SINGLETON useGatingStore (loaded via g.selectImage(uid, vn, popType)); it
// holds one (popType, suffix) at a time. That's why the analysis canvas enforces ONE cluster run per
// board — so this context drives the store once, unambiguously.
import { ref, computed, watch, type Ref } from 'vue'
import { useGatingStore } from '../stores/gating'
import { useLogStore } from '../stores/log'

export interface ShownPop { path: string; name: string; colour: string; clusterIds: number[] }

export function useClusterContext(opts: {
  projectUid: Ref<string>
  imageUids: Ref<string[]>
  popType: Ref<'clust' | 'trackclust'>
  suffix: Ref<string>
}) {
  const g = useGatingStore()
  const log = useLogStore()
  const { projectUid, imageUids, popType, suffix } = opts

  // per-suffix feature list each run actually used (interpretive columns), from the channels endpoint's
  // clusterFeatures sidecar; fall back to markers/motility if a run predates feature tracking.
  const clusterFeatures = ref<Record<string, string[]>>({})
  const featureFallback = ref<string[]>([])
  const nameMap = ref<Record<string, string>>({})     // raw channel column → display name
  const clusterIds = ref<Record<string, number[]>>({})     // per-suffix tickable cluster universe
  const clusterMembers = ref<Record<string, string[]>>({}) // per-suffix uids the run clustered together
  const resolvedVn = ref('default')                    // segmentation value_name the channels resolved to
  const hmmStateCols = ref<string[]>([])               // live.cell.hmm.state.<name>
  const hmmTransitionCols = ref<string[]>([])          // live.cell.hmm.transitions.<name>
  const suffixes = ref<string[]>([])

  // HMM behaviour columns are categorical-behaviour features → the dedicated HMM plots, NOT the heatmap.
  const HMM_RE = /live\.cell\.hmm\.(state|transitions)\b/
  const allFeatures = computed<string[]>(() => clusterFeatures.value[suffix.value] ?? featureFallback.value)
  const featureOptions = computed<string[]>(() => allFeatures.value.filter(f => !HMM_RE.test(f)))

  async function loadFeatures() {
    if (!projectUid.value || !imageUids.value[0]) { clusterFeatures.value = {}; featureFallback.value = []; return }
    const q = new URLSearchParams({ projectUid: projectUid.value, imageUid: imageUids.value[0], popType: popType.value })
    try {
      const r = await fetch(`/api/gating/channels?${q}`)
      if (!r.ok) { clusterFeatures.value = {}; featureFallback.value = []; return }
      const d = await r.json()
      clusterFeatures.value = d.clusterFeatures ?? {}
      clusterIds.value = d.clusterIds ?? {}
      clusterMembers.value = d.clusterMembers ?? {}
      resolvedVn.value = d.valueName ?? 'default'
      featureFallback.value = popType.value === 'trackclust' ? (d.columns ?? []) : (d.channels ?? [])
      const chans: string[] = d.channels ?? [], names: string[] = d.channelNames ?? []
      nameMap.value = Object.fromEntries(chans.map((c, i) => [c, names[i] ?? c]))
      const obs: string[] = d.cellObsMeasures ?? []
      hmmStateCols.value = obs.filter(c => c.startsWith('live.cell.hmm.state.'))
      hmmTransitionCols.value = obs.filter(c => c.startsWith('live.cell.hmm.transitions.'))
      suffixes.value = d.clusterSuffixes ?? []
      if (suffixes.value.length && !suffixes.value.includes(suffix.value)) suffix.value = suffixes.value[0]
    } catch (e) {
      log.error(`Cluster features: ${e instanceof Error ? e.message : String(e)}`, { source: 'cluster' })
      clusterFeatures.value = {}; featureFallback.value = []
    }
  }

  // Cluster pops only apply to images IN THE RUN (carry clusters.{suffix} — the recorded `partOf`).
  const runMembers = computed<string[]>(() => clusterMembers.value[suffix.value] ?? [])
  const validUids = computed<string[]>(() =>
    runMembers.value.length ? imageUids.value.filter(u => runMembers.value.includes(u)) : imageUids.value)
  const strayUids = computed<string[]>(() =>
    runMembers.value.length ? imageUids.value.filter(u => !runMembers.value.includes(u)) : [])
  const missingUids = computed<string[]>(() =>
    runMembers.value.filter(u => !imageUids.value.includes(u)))

  // resolve a highlight set (pop paths) to shown populations (colour + owned cluster IDs) via the store
  const shownPopsFor = (hl: string[]): ShownPop[] => g.flat
    .filter(p => hl.includes(p.path))
    .map(p => ({ path: p.path, name: p.name, colour: p.colour,
                 clusterIds: Array.isArray(p.filter?.values) ? (p.filter!.values as unknown[]).map(Number) : [] }))

  // drive the (shared, pop_type-agnostic) gating store for the pop tree: primary = first valid image,
  // the rest mirror every mutation so cluster pops land set-wide. Re-sync on selection/suffix change.
  watch([validUids, suffix, popType, projectUid], () => {
    if (!projectUid.value || !validUids.value.length) return
    g.selectImage(validUids.value[0], resolvedVn.value, popType.value).then(() => {
      g.mirrorUids = validUids.value.slice(1)
    })
  }, { immediate: true })
  // reload the run/feature metadata when the image set / project / popType changes
  watch([projectUid, () => imageUids.value.join(','), popType], loadFeatures, { immediate: true })

  return {
    suffixes, clusterFeatures, featureFallback, nameMap, clusterIds, clusterMembers, resolvedVn,
    hmmStateCols, hmmTransitionCols, allFeatures, featureOptions,
    runMembers, validUids, strayUids, missingUids, loadFeatures, shownPopsFor,
  }
}
