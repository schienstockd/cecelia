import { describe, it, expect } from 'vitest'
import { crc32, zipTextFiles } from './zip'

const enc = new TextEncoder()

describe('crc32', () => {
  it('matches the standard "123456789" check value', () => {
    expect(crc32(enc.encode('123456789'))).toBe(0xcbf43926)
  })
  it('is 0 for empty input', () => {
    expect(crc32(new Uint8Array(0))).toBe(0)
  })
})

describe('zipTextFiles', () => {
  async function bytes(files: { name: string; text: string }[]) {
    return new Uint8Array(await zipTextFiles(files).arrayBuffer())
  }
  const u32 = (b: Uint8Array, o: number) => new DataView(b.buffer, b.byteOffset).getUint32(o, true)
  const u16 = (b: Uint8Array, o: number) => new DataView(b.buffer, b.byteOffset).getUint16(o, true)
  const countSig = (b: Uint8Array, sig: number) => {
    let n = 0
    for (let i = 0; i + 4 <= b.length; i++) if (u32(b, i) === sig) n++
    return n
  }

  it('writes one local + one central record per file and a matching EOCD count', async () => {
    const b = await bytes([{ name: 'a.csv', text: 'x,y\n1,2\n' }, { name: 'b.csv', text: 'p\n9\n' }])
    expect(countSig(b, 0x04034b50)).toBe(2)              // local file headers
    expect(countSig(b, 0x02014b50)).toBe(2)              // central directory records
    const eocd = b.length - 22                            // no zip comment
    expect(u32(b, eocd)).toBe(0x06054b50)
    expect(u16(b, eocd + 10)).toBe(2)                    // total entries
  })

  it('stores the exact bytes + crc of each file (STORE, no compression)', async () => {
    const text = 'hello,world\n'
    const b = await bytes([{ name: 'a.csv', text }])
    const data = enc.encode(text)
    expect(u32(b, 8)).toBe(0)                             // method 0 = store
    expect(u32(b, 14)).toBe(crc32(data))                 // crc in local header
    expect(u32(b, 18)).toBe(data.length)                 // compressed size == size
    const nameLen = u16(b, 26)
    const stored = b.slice(30 + nameLen, 30 + nameLen + data.length)
    expect(new TextDecoder().decode(stored)).toBe(text)
  })

  it('disambiguates duplicate names', async () => {
    const b = await bytes([{ name: 'plot.csv', text: '1' }, { name: 'plot.csv', text: '2' }])
    const dec = new TextDecoder().decode(b)
    expect(dec).toContain('plot.csv')
    expect(dec).toContain('plot (2).csv')
  })
})
