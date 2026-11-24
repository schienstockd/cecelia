import { describe, expect, it } from 'vitest'
import { paddedPopoverContent } from './popoverPadding'

const sfc = (rule: string, root = '<div class="x-pop cc-fs-xs">') =>
  `<template><TeleportPopover v-model="o" :anchor="a">${root}hi</div></TeleportPopover></template>\n<style scoped>\n${rule}\n</style>`

describe('paddedPopoverContent', () => {
  it('flags a content root that pads itself', () => {
    expect(paddedPopoverContent(sfc('.x-pop { display: flex; padding: 8px; }'))).toEqual(['x-pop'])
  })
  it('layout without padding is fine, and a padded CHILD rule is not the root', () => {
    expect(paddedPopoverContent(sfc('.x-pop { gap: 6px; }\n.x-pop-row { padding: 2px; }'))).toEqual([])
  })
  it('ignores padding on elements outside a popover', () => {
    expect(paddedPopoverContent('<template><div class="x-pop" /></template><style>.x-pop { padding: 4px; }</style>')).toEqual([])
  })
})

// repo-wide: no TeleportPopover content root re-adds the shell's padding
const RAW = import.meta.glob('/src/**/*.vue', { query: '?raw', import: 'default', eager: true }) as Record<string, string>
describe('every TeleportPopover', () => {
  it('the glob resolved', () => { expect(Object.keys(RAW).length).toBeGreaterThan(50) })
  it('leaves its padding to the shell', () => {
    const bad = Object.entries(RAW).flatMap(([p, t]) => paddedPopoverContent(t).map(c => `${p} .${c}`))
    expect(bad).toEqual([])
  })
})
