// Offering a model that DOES NOT EXIST YET — the one thing a chain can produce mid-run that its own
// pickers cannot see.
//
// A `model` select's options are enumerated from the global vault server-side
// (`_inject_dynamic_options!` → `list_coastal_models`), which is correct everywhere except inside a
// chain that TRAINS the model it then segments with. There the producer is a node in the same graph,
// so at author time the vault has nothing to offer and the select cannot express the wiring at all —
// the user picks "None", and the run fails at the segment step with "No optical-flow model selected".
//
// This is the counterpart of the same fix on the Julia side (`_chain_produced_names` in
// tasks/chain.jl, handed to `validate_params` as `extra_options`): validation must ACCEPT the forward
// reference, and the picker must OFFER it. Neither half is any use alone.
//
// Kept a pure util rather than SFC-local so it is unit-testable, same reason as utils/startDot.ts.
import type { TaskDef, ParamDef } from '../tasks/types'

/** The `field` a select declares when its options name entries in the global model vault. */
export const MODELS_FIELD = 'models'

/**
 * The vault FILENAME for a model stem — the value a `model` select carries, built from the stem
 * `opticalFlow.train`'s `modelName` holds. Mirrors Julia's `flow_model_filename` (app/src/config.jl);
 * `utils/chainModelOptions.test.ts` pins the pair. Idempotent.
 */
export function modelFilename(stem: string): string {
  return stem.endsWith('.pt') ? stem : `${stem}.pt`
}

/**
 * `def` with every models-consuming select extended by the models `stems` an upstream node produces.
 *
 * Appended, never replacing: a real vault entry stays offered and stays first, so the ordinary case
 * (segment with a model trained last week) is untouched. An upstream name that is ALREADY in the
 * vault is not duplicated — training over an existing name is a legitimate overwrite, and the option
 * it produces is the same one.
 *
 * Returns `def` itself when there is nothing to add, so the common path allocates nothing and the
 * caller's `computed` does not invalidate on every unrelated edit.
 */
export function withChainProducedModels(def: TaskDef, stems: string[]): TaskDef {
  if (!def || !stems.length) return def
  const wanted = stems.map(modelFilename)
  let touched = false

  const walk = (params: ParamDef[]): ParamDef[] => params.map(p => {
    if (p.type === 'group' || p.type === 'section') {
      const inner = walk(p.params ?? [])
      return inner === p.params ? p : { ...p, params: inner }
    }
    if (p.type !== 'select' || p.field !== MODELS_FIELD) return p
    const have = new Set((p.options ?? []).map(o => o.value))
    const add = wanted.filter(v => !have.has(v))
    if (!add.length) return p
    touched = true
    return {
      ...p,
      options: [
        ...(p.options ?? []),
        // Says WHY a name with no file behind it is listed — otherwise it reads as an available model
        // and a user wonders where it went if they run the segment node on its own.
        ...add.map(v => ({ label: `${v.replace(/\.pt$/, '')} (trained in this chain)`, value: v })),
      ],
    }
  })

  const params = walk(def.params ?? [])
  return touched ? { ...def, params } : def
}
