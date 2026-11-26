import { describe, it, expect, vi, afterEach } from 'vitest'
import { writeImportPlan } from './imageAttrApi'

// The stub answers as `api/src/routes/metadata.jl` does: `attr/create` → `{ok}`, `attr/set` →
// `{ok, attrName, values}` with each value trimmed (`_norm_attr`).
function stubServer() {
  const calls: { path: string; body: any }[] = []
  vi.stubGlobal('fetch', vi.fn(async (path: string, init: { body: string }) => {
    const body = JSON.parse(init.body)
    calls.push({ path, body })
    const reply = path.endsWith('/attr/set')
      ? { ok: true, attrName: body.attrName, values: Object.fromEntries(Object.entries(body.values as Record<string, string>).map(([k, v]) => [k, v.trim()])) }
      : { ok: true }
    return new Response(JSON.stringify(reply), { status: 200 })
  }))
  return calls
}
afterEach(() => vi.unstubAllGlobals())

describe('writeImportPlan', () => {
  it('creates new columns on every image first, then one attr/set per attribute with changed cells', async () => {
    const calls = stubServer()
    const keys: string[] = []
    const stored: Record<string, Record<string, string>> = {}
    const n = await writeImportPlan('p1', ['a', 'b', 'c'],
      { newAttrs: ['Donor'], writes: { Donor: { a: '007 ' }, Tissue: { b: 'Liver' }, Empty: {} } },
      { addAttrKey: k => keys.push(k), setAttrValues: (k, v) => { stored[k] = v } })
    expect(calls.map(c => c.path)).toEqual(['/api/images/attr/create', '/api/images/attr/set', '/api/images/attr/set'])
    expect(calls[0].body).toEqual({ projectUid: 'p1', imageUids: ['a', 'b', 'c'], attrName: 'Donor' })
    expect(keys).toEqual(['Donor'])
    expect(stored).toEqual({ Donor: { a: '007' }, Tissue: { b: 'Liver' } })   // the store gets what was stored
    expect(n).toBe(2)
  })
})
