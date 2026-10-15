import { describe, it, expect } from 'vitest'
import { compileTemplate } from 'vue/compiler-sfc'
import { rangeFillTransform } from './rangeFillTransform'

// Compiles real templates, so a renumbering of Vue's NodeTypes / ElementTypes (written as numbers in
// the transform) fails here.
const code = (tpl: string) => compileTemplate({ source: tpl, filename: 't.vue', id: 't',
  compilerOptions: { nodeTransforms: [rangeFillTransform] } }).code

describe('rangeFillTransform', () => {
  it('adds v-range-fill to a static range input, and only there', () => {
    expect(code('<input type="range" :value="v" />')).toContain('_directive_range_fill')
    expect(code('<input :type="\'range\'" />')).not.toContain('_directive_range_fill')   // bound type: left alone
    expect(code('<input type="number" />')).not.toContain('_directive_range_fill')
  })
  it('does not add it twice', () => {
    const c = code('<input type="range" v-range-fill />')
    expect(c.match(/\[_directive_range_fill\]/g)?.length).toBe(1)
  })
})
