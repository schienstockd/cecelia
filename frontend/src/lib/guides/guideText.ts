// The guides and recipes as plain text, for a reader that cannot click: Claude through the MCP
// (`get_guide` in mcp/cecelia_mcp/server.py). Same source as the in-app bubbles, so the text an agent
// is handed is the text a user is shown — nothing written twice to drift apart.
//
// What is dropped is only what needs a screen: anchors, placements, reveals and the `when` predicates.
// What is added is the function a task guide runs (its `awaitTask.fun`), because "pick it from the
// dropdown" names a label, and a REPL caller needs the fun_name.
//
// The rendered catalogue is committed as mcp/cecelia_mcp/guides.json and checked by guideText.test.ts
// (a file snapshot): edit a guide, re-run `npx vitest run -u src/lib/guides/guideText.test.ts`.

import { GUIDES, guideById, RECIPES, isWanted } from './index'
import type { GuideDef, WrittenRecipe } from './index'

export interface GuideTextEntry {
  id: string
  kind: 'guide' | 'recipe'
  title: string
  summary: string
  text: string
}

const funsOf = (g: GuideDef): string[] =>
  [...new Set(g.steps.flatMap(s => (s.awaitTask?.fun ? [s.awaitTask.fun] : [])))]

export function guideText(g: GuideDef): string {
  const lines = [`# ${g.title}`, '', g.summary, '']
  if (g.prereqs.length) lines.push(`Needs: ${g.prereqs.map(p => p.label).join('; ')}.`)
  const funs = funsOf(g)
  if (funs.length) lines.push(`Runs: ${funs.map(f => `\`${f}\``).join(', ')}.`)
  lines.push('')
  g.steps.forEach((s, i) => {
    lines.push(`${i + 1}. ${s.title ? `**${s.title}** — ` : ''}${s.text}`)
    for (const b of s.bullets ?? []) lines.push(`   - ${b}`)
  })
  return lines.join('\n')
}

export function recipeText(r: WrittenRecipe): string {
  const lines = [`# Recipe: ${r.title}`, '', `When this is you: ${r.whenThisIsYou}`, '', '## Steps', '']
  r.steps.forEach((s, i) => {
    const g = guideById(s.guide)
    lines.push(`${i + 1}. ${g?.title ?? s.guide} (guide \`${s.guide}\`)${s.optional ? ' — optional' : ''}: ${s.why}`)
  })
  for (const s of r.steps) {
    const g = guideById(s.guide)
    if (g) lines.push('', '---', '', guideText(g))
  }
  return lines.join('\n')
}

export function guideCatalogue(): GuideTextEntry[] {
  const recipes: GuideTextEntry[] = RECIPES.filter(r => !isWanted(r)).map(r => {
    const w = r as WrittenRecipe
    return { id: w.id, kind: 'recipe', title: w.title, summary: w.whenThisIsYou, text: recipeText(w) }
  })
  const guides: GuideTextEntry[] = GUIDES.map(g => ({
    id: g.id, kind: 'guide', title: g.title, summary: g.summary, text: guideText(g),
  }))
  return [...recipes, ...guides]
}
