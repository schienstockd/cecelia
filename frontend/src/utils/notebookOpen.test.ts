import { describe, it, expect } from 'vitest'
import { notebookOpenUrl } from './notebookOpen'

describe('notebookOpenUrl', () => {
  it('opens a project notebook at its path, with the secret', () => {
    expect(notebookOpenUrl({ scope: 'project', path: '/p/notebooks/a b.jl' }, 'http://localhost:7660/', 's&1'))
      .toBe('http://localhost:7660/open?path=%2Fp%2Fnotebooks%2Fa%20b.jl&secret=s%261')
  })

  it('omits the secret param when there is none', () => {
    expect(notebookOpenUrl({ scope: 'project', path: '/p/x.jl' }, 'http://h/', ''))
      .toBe('http://h/open?path=%2Fp%2Fx.jl')
  })

  it('never opens a shipped example in place — Pluto would save into the install', () => {
    expect(notebookOpenUrl({ scope: 'example', path: '/install/notebooks/example_pop_df.jl' }, 'http://h/', 's'))
      .toBeNull()
  })
})
