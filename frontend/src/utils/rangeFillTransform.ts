// Compile-time: give every `<input type="range">` the `v-range-fill` directive (`directives/rangeFill.ts`).
// Registered in vite.config.ts, so it runs for the app, the build and vitest alike. Only a STATIC
// `type="range"` is matched — a bound `:type` is not a slider anyone writes here, and guessing at it
// would decorate text inputs.
//
// The node kinds are Vue's `NodeTypes` / `ElementTypes` const-enum values. They are written as numbers
// because `vue` does not re-export the enums at runtime (`@vue/compiler-core` is only a transitive
// dependency); `rangeFillTransform.test.ts` compiles real templates, so a renumbering would fail there.
import type { NodeTransform, DirectiveNode } from '@vue/compiler-core'

const ELEMENT = 1, ATTRIBUTE = 6, DIRECTIVE = 7     // NodeTypes
const PLAIN_ELEMENT = 0                              // ElementTypes.ELEMENT

export const rangeFillTransform: NodeTransform = (node) => {
  if (node.type !== ELEMENT || node.tagType !== PLAIN_ELEMENT || node.tag !== 'input') return
  const isRange = node.props.some(p => p.type === ATTRIBUTE && p.name === 'type' && p.value?.content === 'range')
  if (!isRange) return
  if (node.props.some(p => p.type === DIRECTIVE && p.name === 'range-fill')) return
  node.props.push({
    type: DIRECTIVE, name: 'range-fill', rawName: 'v-range-fill',
    exp: undefined, arg: undefined, modifiers: [], loc: node.loc,
  } as DirectiveNode)
}
