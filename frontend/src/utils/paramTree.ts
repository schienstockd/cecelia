// A step's non-default parameters as a tree, for reading rather than scanning one long line.
//
// The run history writes them as `key=value; key=value` (`history_params_table` → `changedFromDefault`):
// a key can be a path (`models[0].seedSize`) and a value can be JSON (`imageTiling={"overlap":0,…}`).
// Both become nesting here: path parts and JSON object/array members are children, scalars are leaves.

export interface ParamNode { key: string; value?: string; children?: ParamNode[] }
/** One displayed line: a key at a depth, with its value when it is a leaf. */
export interface ParamLine { depth: number; key: string; value?: string }

/**
 * Split a `key=value; key=value` (or `key: spread; …`) list into entries: on a `; ` that is followed by
 * the next KEY (`name`, `a.b`, `models[0].x`, then `=` or `: `). Values are written raw — a population
 * path can hold an unbalanced `[` — so bracket counting can't be trusted; a `; ` inside a value is kept.
 */
export function splitParams(s: string): string[] {
  return s.split(/;\s+(?=[A-Za-z_][\w.[\]]*(?:=|: ))/).map(x => x.trim()).filter(Boolean)
}

const pathParts = (key: string) => key.match(/[^.[\]]+/g) ?? [key]

function fromJson(key: string, v: unknown): ParamNode {
  if (Array.isArray(v)) {
    return v.every(x => x === null || typeof x !== 'object')
      ? { key, value: v.map(x => String(x)).join(', ') }
      : { key, children: v.map((x, i) => fromJson(String(i), x)) }
  }
  if (v !== null && typeof v === 'object') return { key, children: Object.entries(v).map(([k, x]) => fromJson(k, x)) }
  return { key, value: String(v) }
}

function leaf(key: string, raw: string): ParamNode {
  const t = raw.trim()
  if (/^[[{"]/.test(t)) { try { return fromJson(key, JSON.parse(t)) } catch { /* not JSON: keep the text */ } }
  return { key, value: t }
}

/**
 * Put one setting into a tree at its key path (`models[0].seedSize` → models › 0 › seedSize), sharing the
 * branch with settings already there. `make` builds the leaf from its last path part.
 */
export function insertParam(root: ParamNode[], key: string, make: (last: string) => ParamNode): void {
  const parts = pathParts(key.trim())
  let level = root
  for (const p of parts.slice(0, -1)) {
    let n = level.find(x => x.key === p && x.children)
    if (!n) { n = { key: p, children: [] }; level.push(n) }
    level = n.children!
  }
  level.push(make(parts[parts.length - 1]))
}

/** `key=value; …` → a tree; entries sharing a path prefix (`models[0].a`, `models[0].b`) share a node. */
export function paramTree(s: string, root: ParamNode[] = []): ParamNode[] {
  for (const entry of splitParams(s)) {
    const eq = entry.indexOf('=')
    insertParam(root, eq < 0 ? entry : entry.slice(0, eq), last => (eq < 0 ? { key: last } : leaf(last, entry.slice(eq + 1))))
  }
  return root
}

/** The tree as indented lines, depth-first, every branch open. */
export function paramLines(nodes: readonly ParamNode[], depth = 0): ParamLine[] {
  return nodes.flatMap(n => [{ depth, key: n.key, ...(n.value !== undefined ? { value: n.value } : {}) },
                             ...paramLines(n.children ?? [], depth + 1)])
}

/** A one-line summary for a table cell: `key=value` for a single setting, else the top-level keys. */
export function paramSummary(nodes: readonly ParamNode[]): string {
  if (!nodes.length) return ''
  if (nodes.length === 1 && nodes[0].value !== undefined) return `${nodes[0].key} = ${nodes[0].value}`
  return nodes.map(n => n.key).join(', ')
}

/** Every leaf with its full path (`imageTiling.blockSize`), for one row per setting in a table. */
export function paramLeaves(nodes: readonly ParamNode[], prefix = ''): { path: string; value: string }[] {
  return nodes.flatMap(n => {
    const path = prefix ? `${prefix}.${n.key}` : n.key
    return n.children ? paramLeaves(n.children, path) : [{ path, value: n.value ?? '' }]
  })
}
