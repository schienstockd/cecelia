import { describe, it, expect } from 'vitest'
import {
  COPY_MAX, normalise, isMultiSentence, isTooLong, isTitleCase,
  tooltipStrings, hintStrings, attrStrings, textStrings, uncoveredControls, duplicateTooltips, nestedTooltips,
  hasPerOptionTips, unnamedToggles, misplacedTooltips,
} from './uiCopy'

describe('normalise', () => {
  it('collapses whitespace so a wrapped literal measures the same as a joined one', () => {
    expect(normalise('a\n   b')).toBe('a b')
  })

  it('unescapes the quote form Vue attributes use', () => {
    expect(normalise("viewer\\'s scale bar")).toBe("viewer's scale bar")
  })
})

describe('isMultiSentence', () => {
  it('flags a second sentence', () => {
    expect(isMultiSentence('Remove this image. The original file is not deleted.')).toBe(true)
    expect(isMultiSentence('Render on the GPU. Restarts viewer. Linux only.')).toBe(true)
  })

  it('does not flag an abbreviation', () => {
    expect(isMultiSentence('Colour by a cell property (e.g. HMM state)')).toBe(false)
    expect(isMultiSentence('Second attribute to combine (e.g. Treatment × Mouse)')).toBe(false)
    expect(isMultiSentence('Regex over the name; a (group) if present, e.g. M(\\d+)→4')).toBe(false)
  })

  it('does not flag one sentence, however it is punctuated', () => {
    expect(isMultiSentence('Delete the set and all its images — cannot be undone')).toBe(false)
    expect(isMultiSentence('all = ignore failures, require_all = abort')).toBe(false)
    expect(isMultiSentence('Output suffix → clusters.<suffix>.')).toBe(false)
  })

  it('does not treat a decimal or a version as a sentence break', () => {
    expect(isMultiSentence('Lower it to 99.9 if a hot pixel pins the max')).toBe(false)
  })
})

describe('isTooLong', () => {
  it('measures the normalised string against the budget', () => {
    expect(isTooLong('x'.repeat(COPY_MAX))).toBe(false)
    expect(isTooLong('x'.repeat(COPY_MAX + 1))).toBe(true)
    expect(isTooLong('x'.repeat(COPY_MAX + 1).replace(/x/g, 'x '))).toBe(true)
  })
})

describe('tooltipStrings', () => {
  it('reads the literals a user sees, not the binding expression', () => {
    // The regression this whole module exists for: measuring the expression makes this one
    // 95-character violation, when neither branch is close to the budget.
    const src = `<i v-tooltip="flaggedActive ? 'Deselect flagged images' : 'Select all flagged images'" />`
    expect(tooltipStrings(src)).toEqual(['Deselect flagged images', 'Select all flagged images'])
    expect(tooltipStrings(src).some(isTooLong)).toBe(false)
  })

  it('handles every placement modifier and both attribute quote styles', () => {
    expect(tooltipStrings(`<i v-tooltip.right="'a'" />`)).toEqual(['a'])
    expect(tooltipStrings(`<i v-tooltip.top='"unused" + \`b\`' />`)).toEqual(['b'])
  })

  it('drops ${…} rather than guessing its rendered width', () => {
    expect(tooltipStrings('<i v-tooltip="`Apply to ${n} image(s)`" />')).toEqual(['Apply to image(s)'])
  })

  it('joins a literal split across lines', () => {
    expect(tooltipStrings(`<i v-tooltip="'one\n         two'" />`)).toEqual(['one two'])
  })

  it('ignores a binding with no literal — the text is checked where it is built', () => {
    expect(tooltipStrings('<i v-tooltip="someLabel" />')).toEqual([])
  })

  it('does not run past the end of one binding into the next', () => {
    expect(tooltipStrings(`<i v-tooltip="'a'" /><i v-tooltip="'b'" />`)).toEqual(['a', 'b'])
  })
})

describe('hintStrings', () => {
  it('reads the static hint props', () => {
    expect(hintStrings('<ModuleLayout hint-key="segment" hint="Detect cells here." />'))
      .toEqual(['Detect cells here.'])
    expect(hintStrings('<X no-set-hint="Create a set first." />')).toEqual(['Create a set first.'])
  })

  it('ignores a bound hint, and does not match hint-key', () => {
    expect(hintStrings('<X :hint="dynamicHint" hint-key="segment" />')).toEqual([])
  })
})

describe('attrStrings', () => {
  it('reads static copy attributes', () => {
    expect(attrStrings('<Button label="Crop image" />')).toEqual(['Crop image'])
    expect(attrStrings('<Input placeholder="New set name" aria-label="Set name" />'))
      .toEqual(['New set name', 'Set name'])
  })

  it('reads the literals inside a bound attribute, not the expression', () => {
    expect(attrStrings(`<Dialog :header="editing ? 'Edit gate' : 'New gate'" />`))
      .toEqual(['Edit gate', 'New gate'])
  })

  it('does not match an attribute that merely ends in a copy-attr name', () => {
    expect(attrStrings('<X hint-label="x" data-title="y" />')).toEqual([])
  })
})

describe('textStrings', () => {
  it('reads bare text nodes', () => {
    expect(textStrings('<template><h2>Projects</h2><p>No images yet</p></template>'))
      .toEqual(['Projects', 'No images yet'])
  })

  it('does not leak attributes out of a tag containing an arrow function', () => {
    // The bug this guard exists for: `/<[^>]*>/` ends the tag at the `>` of `v =>`, spilling the
    // rest of the attribute list into the output as if a user could read it.
    const src = `<template><input @blur="commit(img.uid, v => save(img, v))" />Channel name</template>`
    expect(textStrings(src)).toEqual(['Channel name'])
  })

  it('drops interpolation but keeps the words around it', () => {
    expect(textStrings('<template><span>{{ count }} selected</span></template>')).toEqual(['selected'])
  })

  it('drops snake_case identifiers but keeps plain lowercase copy', () => {
    // A blanket all-lowercase guard looks reasonable and is wrong: measured over the real SFCs it
    // threw away 79 strings, ~74 of them genuine copy ("cancel", "clear", "median", "reset").
    expect(textStrings('<template><i>require_all</i><i>cancel</i></template>')).toEqual(['cancel'])
  })
})

describe('isTitleCase', () => {
  it('flags a label that Title Cases an ordinary word', () => {
    expect(isTitleCase('Bayesian Tracking')).toBe(true)
    expect(isTitleCase('Drift Correction')).toBe(true)
  })

  it('does not flag acronyms, proper nouns or single letters', () => {
    // These read as violations without the allowance, and drown the real hits.
    expect(isTitleCase('Calculate UMAP')).toBe(false)
    expect(isTitleCase('Use Dask')).toBe(false)
    expect(isTitleCase('Flatten Z')).toBe(false)
    expect(isTitleCase('Segmentation QC')).toBe(false)
  })

  it('treats a word after a separator as a new phrase, not Title Case', () => {
    expect(isTitleCase('Spatial / Time')).toBe(false)
    // ...but the word before the separator is still judged: "Correction" is the violation here.
    expect(isTitleCase('AF + Drift Correction')).toBe(true)
  })

  it('does not flag sentence case or a single word', () => {
    expect(isTitleCase('Crop image')).toBe(false)
    expect(isTitleCase('Source image version')).toBe(false)
    expect(isTitleCase('Tracking')).toBe(false)
  })
})

describe('uncoveredControls', () => {
  it('flags a settable control with no hover help', () => {
    expect(uncoveredControls('<template><label>Bins<input type="number" /></label></template>'))
      .toEqual([{ tag: 'input', line: 1 }])
  })

  it('counts a tooltip on the control itself', () => {
    expect(uncoveredControls(`<template><select v-tooltip.left="'Palette'" /></template>`)).toEqual([])
  })

  it('counts a tooltip on an ANCESTOR — the row, not the input, usually carries it', () => {
    // The pattern most of the app is written in. Checking the tag alone calls this a violation and
    // over-reports by ~90%, which is enough noise to make the whole signal ignorable.
    const src = `<template><label class="po-row" v-tooltip.left="'X tick angle'"><span>X angle</span>
      <input type="range" /></label></template>`
    expect(uncoveredControls(src)).toEqual([])
  })

  it('does not let a closed ancestor keep covering its siblings', () => {
    const src = `<template><div v-tooltip="'a'"><input /></div><select /></template>`
    expect(uncoveredControls(src)).toEqual([{ tag: 'select', line: 1 }])
  })

  it('does not treat a void input as an open element that swallows what follows', () => {
    // Without the VOID guard the unclosed `<input>` stays on the ancestor stack and every later
    // control inherits its (absent) tooltip state — or worse, a tooltip it never had.
    const src = `<template><input v-tooltip="'a'"><select /></template>`
    expect(uncoveredControls(src)).toEqual([{ tag: 'select', line: 1 }])
  })

  // A chip row is many small hit targets, and a tooltip anchored to it renders ON TOP of the chips —
  // so the hover help hides what you were about to click. The blanket "every control carries its own
  // v-tooltip" rule is therefore actively wrong here, not merely redundant (Dominik, 2026-08-07).
  it('a tipped heading covers a chip select that follows it', () => {
    const src = `<template><div class="param-row"><label v-tooltip.left="'Frame lags'">Scales</label>
                 <ChipSelect multiple /></div></template>`
    expect(uncoveredControls(src)).toEqual([])
  })

  it('…and covers one nested a wrapper deeper (the channelSelection shape)', () => {
    const src = `<template><div class="param-row"><label v-tooltip.left="'Channels'">Ch</label>
                 <div class="channel-select-wrap"><ChipSelect multiple /></div></div></template>`
    expect(uncoveredControls(src)).toEqual([])
  })

  it('a chip select with no tipped heading anywhere is still reported', () => {
    expect(uncoveredControls('<template><div><ChipSelect multiple /></div></template>').map(c => c.tag))
      .toEqual(['ChipSelect'])
  })

  it('heading coverage does not leak into a later sibling row', () => {
    const src = `<template><div><label v-tooltip="'a'">A</label><ChipSelect /></div>
                 <div><select /></div></template>`
    expect(uncoveredControls(src).map(c => c.tag)).toEqual(['select'])
  })

  it('a tipped heading also covers a toggle — the tooltip would sit on the switch', () => {
    const src = `<template><div><label v-tooltip.left="param.tip">Overwrite</label>
                 <CcToggle /></div></template>`
    expect(uncoveredControls(src)).toEqual([])
  })

  it('the exemption is the overlay-prone controls — a plain select still needs its own tooltip', () => {
    const src = `<template><div><label v-tooltip="'a'">A</label><select /></div></template>`
    expect(uncoveredControls(src).map(c => c.tag)).toEqual(['select'])
  })

  it('does not accept a native title — only v-tooltip is coverage', () => {
    // A `title=` renders as the browser's own unstyled tooltip and is invisible to the copy
    // ratchets, so accepting it would let a control pass this check looking nothing like the app.
    expect(uncoveredControls('<template><input title="Frames per second" /></template>'))
      .toEqual([{ tag: 'input', line: 1 }])
  })

  it('ignores inputs a user sets no value on', () => {
    expect(uncoveredControls('<template><input type="file" /><input type="hidden" /></template>'))
      .toEqual([])
  })

  it('ignores a button with a caption — the caption is the help', () => {
    expect(uncoveredControls('<template><button>Run</button></template>')).toEqual([])
    expect(uncoveredControls('<template><button><i class="pi pi-play" /> Run</button></template>'))
      .toEqual([])
  })

  it('flags an icon-only button — no caption, nothing to read', () => {
    expect(uncoveredControls('<template><button><i class="pi pi-trash" /></button></template>'))
      .toEqual([{ tag: 'button', line: 1 }])
    expect(uncoveredControls(`<template><button v-tooltip="'Delete'"><i class="pi pi-trash" /></button></template>`))
      .toEqual([])
  })

  it('treats an INTERPOLATED caption as a caption', () => {
    // Stripping `{{ … }}` the way the copy extractors do is wrong here — they measure text width,
    // this asks whether the user sees any words. Dropping it flagged four captioned buttons.
    const src = `<template><button><span>{{ group.heading }}</span><i class="pi pi-chevron-down" /></button></template>`
    expect(uncoveredControls(src)).toEqual([])
  })

  it('does not treat an aria-label as a visible caption', () => {
    // Read out by a screen reader, never shown on hover — so it is not this rule's coverage.
    const src = '<template><button aria-label="Next tip"><i class="pi pi-chevron-right" /></button></template>'
    expect(uncoveredControls(src)).toEqual([{ tag: 'button', line: 1 }])
  })

  it('skips the wrapper primitives own definitions', () => {
    // `CcToggle.vue` holds the checkbox every toggle in the app renders through; its tooltip belongs
    // at the call site, so counting the internal input reports a violation no caller can fix.
    const src = '<template><input type="checkbox" /></template>'
    expect(uncoveredControls(src, '/src/components/CcToggle.vue')).toEqual([])
    expect(uncoveredControls(src, '/src/components/ManageImagesModule.vue')).toHaveLength(1)
  })

  it('does not end a tag early on a > inside an attribute value', () => {
    const src = `<template><div v-tooltip="'a'" @input="f(v => g(v))"><select /></div></template>`
    expect(uncoveredControls(src)).toEqual([])
  })

  it('reports the line the control is on', () => {
    expect(uncoveredControls('<template>\n\n  <select />\n</template>'))
      .toEqual([{ tag: 'select', line: 3 }])
  })
})

// ── The ratchet: every rendered string in the app is inside budget ────────────────────────────────
//
// An exact allow-list, not a count — the `cssScenarios` lesson. A count-based baseline silently
// permits swapping one violation for another inside the same file, and stops meaning anything once
// it reaches zero. Both surfaces were swept to zero, so the bar is simply: this list, and nothing
// else. Adding an entry should require the argument below, not a bumped number.
//
// Before adding: is the removed explanation in `docs/`? That was true of every one of the ~100
// strings shortened in the two sweeps — pseudoreplication in PLOTS.md, chain failure policy in
// SCHEDULER.md, resource pools in MODULES.md, import staging on `_stage_source!`. If the fact
// matters, it belongs there, and the UI can then be short without losing it.
const ALLOWED_LONG: string[] = []

// Multi-sentence copy is allowed only where the second sentence is a call to ACTION rather than an
// explanation — a notification that tells you what to do about it.
const ALLOWED_MULTI: string[] = [
  'Update available — . Open Settings to install.',   // status + what to do; `${version}` stripped
]

const SFC = import.meta.glob('/src/**/*.vue', { query: '?raw', import: 'default', eager: true }) as
  Record<string, string>


describe('UI copy stays short (docs/ui/COPY.md)', () => {
  const sfcs = Object.entries(SFC)

  it('found the sources it is meant to police', () => {
    expect(sfcs.length).toBeGreaterThan(50)      // the glob resolved
  })

  it('no tooltip or hint is over the budget', () => {
    const over: string[] = []
    for (const [path, src] of sfcs) {
      for (const s of [...tooltipStrings(src), ...hintStrings(src)]) {
        if (isTooLong(s) && !ALLOWED_LONG.includes(s)) over.push(`${path}: [${s.length}] ${s}`)
      }
    }
    expect(over).toEqual([])
  })

  it('nothing explains itself in a second sentence', () => {
    const multi: string[] = []
    for (const [path, src] of sfcs) {
      for (const s of [...tooltipStrings(src), ...hintStrings(src)]) {
        if (isMultiSentence(s) && !ALLOWED_MULTI.includes(s)) multi.push(`${path}: ${s}`)
      }
    }
    expect(multi).toEqual([])
  })
})

// ── The ratchet: it is written the house way (docs/UI.md → House style) ──────────────────────────
//
// Length was enforced; consistency was not, and it drifted where nothing could see the whole corpus
// at once — the frontend stayed sentence case while task specs went Title Case, and 100 of 482
// tooltips grew a trailing period nobody had decided on. `pixi run ui-copy` is the review tool; these
// two are the build-failing subset. Same exact-allow-list rule as above. The task-spec half of both
// checks lives in `app/test/runtests.jl`, for the same reason the `tip` budget does.

// A label whose Title Case is a real proper name rather than a style slip.
const ALLOWED_TITLE_CASE: string[] = []

// A tooltip that ends in a period because it is genuinely a sentence, not a fragment.
const ALLOWED_TRAILING_PERIOD: string[] = []

describe('UI copy is written the house way (docs/UI.md → House style)', () => {
  const sfcs = Object.entries(SFC)

  it('labels are sentence case, not Title Case', () => {
    const titled: string[] = []
    for (const [path, src] of sfcs) {
      for (const s of attrStrings(src)) {
        if (isTitleCase(s) && !ALLOWED_TITLE_CASE.includes(s)) titled.push(`${path}: ${s}`)
      }
    }
    expect(titled).toEqual([])
  })

  it('no tooltip ends in a trailing period', () => {
    // `…` and `...` are continuations, not sentence ends, so they are not periods for this purpose.
    const dotted: string[] = []
    for (const [path, src] of sfcs) {
      for (const s of tooltipStrings(src)) {
        if (/[^.]\.$/.test(s) && !ALLOWED_TRAILING_PERIOD.includes(s)) dotted.push(`${path}: ${s}`)
      }
    }
    expect(dotted).toEqual([])
  })

  it('uses one verb per action', () => {
    // Only the words with a decided winner. Create/Add, Delete/Remove and Run/Start are NOT
    // synonyms (see the vocabulary table) and are deliberately absent.
    const BANNED: Record<string, string> = {
      Choose: 'Select', Pick: 'Select', Display: 'Show', Execute: 'Run', Modify: 'Edit',
      Discard: 'Remove',
    }
    const found: string[] = []
    for (const [path, src] of sfcs) {
      for (const s of [...tooltipStrings(src), ...hintStrings(src), ...attrStrings(src)]) {
        for (const [bad, good] of Object.entries(BANNED)) {
          if (new RegExp(`\\b${bad}\\b`, 'i').test(s)) found.push(`${path}: "${s}" — use ${good}`)
        }
      }
    }
    expect(found).toEqual([])
  })
})

// ── The other ratchet: every settable control HAS hover help ──────────────────────────────────────
//
// Length was policed; presence wasn't, and that is the half that actually bit — a panel would get
// tooltips on six of its ten rows and nobody could see the four. `docs/UI.md` asks for
// CellProfiler-style tip DENSITY, so this makes the gap visible the same way: an exact allow-list,
// swept to zero first. See `uncoveredControls` for what counts as covered (ancestor tooltips do) and
// why buttons are out of scope. The task-spec half — a param with no `tip` — is in
// `app/test/runtests.jl`, same split as everything else here.
//
// Before adding an entry, try writing the tooltip. It is nearly always shorter than the argument for
// skipping it, and one line under 90 characters is the whole bar.
const ALLOWED_NO_TOOLTIP: string[] = []

describe('every settable control has a tooltip (docs/UI.md → Tooltips)', () => {
  const sfcs = Object.entries(SFC)

  it('found the sources it is meant to police', () => {
    expect(sfcs.length).toBeGreaterThan(50)
  })

  it('no input, select, textarea or toggle is left without hover help', () => {
    const bare: string[] = []
    for (const [path, src] of sfcs) {
      for (const c of uncoveredControls(src, path)) {
        const at = `${path}:${c.line} <${c.tag}>`
        if (!ALLOWED_NO_TOOLTIP.includes(at)) bare.push(at)
      }
    }
    expect(bare).toEqual([])
  })

  // A tooltip is not a NAME. CcToggle hides its real <input>, so a toggle whose caption sits outside
  // the component announces as an unlabelled checkbox — which 17 call sites did until `ariaLabel`
  // existed. Separate from the tooltip checks on purpose: their ancestor rule (a captioned row covers
  // its children) is exactly what cannot supply a name.
  it('every CcToggle carries its own accessible name', () => {
    const unnamed: string[] = []
    for (const [path, src] of sfcs)
      for (const c of unnamedToggles(src, path)) unnamed.push(`${path}:${c.line}`)
    expect(unnamed).toEqual([])
  })

  // The other half of the same rule, and the reason it has to be enforced from both sides: the
  // presence ratchet above is what put a second `param.tip` on every bool param's switch, where it
  // rendered on top of the control. Fixing one without pinning the other just re-breaks it.
  it('no chip row, swatch or toggle repeats the tooltip its heading already carries', () => {
    const dupes: string[] = []
    for (const [path, src] of sfcs)
      for (const d of duplicateTooltips(src, path)) dupes.push(`${path}:${d.line} <${d.tag}> ${d.tooltip}`)
    expect(dupes).toEqual([])
  })

  // Tooltips whose HOVER AREAS nest — the row is tipped AND so is a control inside it, so hovering
  // the control fires both and they overlap. Different from the duplicate check above: the two texts
  // usually differ, which is why nothing caught this until it was noticed on screen.
  //
  // Was a shrinking allow-list of 29 pre-existing sites (measured 2026-08-10, 15 files); all fixed
  // in the same change, so it is now empty and stays that way — this is a plain no-violations check.
  const ALLOWED_NESTED: string[] = []

  it('no NEW tooltip nests inside another element\'s tooltip', () => {
    const found: string[] = []
    for (const [path, src] of sfcs)
      for (const n of nestedTooltips(src, path)) found.push(`${path}:${n.line} <${n.tag}>`)
    expect(found.filter(f => !ALLOWED_NESTED.includes(f))).toEqual([])
    // Fails on improvement too: fix one and delete its line, so the list can only shrink.
    expect(found.sort()).toEqual([...ALLOWED_NESTED].sort())
  })

  // The THIRD way a tooltip lands on top of the UI, and the one the two checks above are blind to,
  // because it is neither a repeated text nor a nested hover area. PrimeVue's `isOutOfBounds` tests
  // the VIEWPORT and nothing else, and `alignLeft` is `left = hostLeft - tooltipWidth` — so on a
  // target that spans its panel, `.left` puts the tooltip outside that panel by construction, over
  // whatever column is next door, and the library reports it in bounds because it is still on screen.
  // 123 sites were doing this, 26 of them in PlotOptions alone, where every row tip landed on the
  // plot it described (Dominik, 2026-08-22: "the tooltip just overlays the actual element").
  //
  // Swept to zero in the same change, so this is a plain no-violations check. Before adding an entry:
  // the fix is `.top` on a label or heading and `.bottom` on a control, never a nudge to the other
  // side — those are the only two placements PrimeVue clamps horizontally. An entry is only justified
  // for a host this cannot measure as narrow, and `radio`/`checkbox` are already exempt by type.
  const ALLOWED_SIDEWAYS: string[] = []

  it('no tooltip on a column-wide target is placed sideways', () => {
    const found: string[] = []
    for (const [path, src] of sfcs)
      for (const m of misplacedTooltips(src, path)) found.push(`${path}:${m.line} <${m.tag}> .${m.side}`)
    expect(found.filter(f => !ALLOWED_SIDEWAYS.includes(f))).toEqual([])
    expect(found.sort()).toEqual([...ALLOWED_SIDEWAYS].sort())
  })
})

describe('misplacedTooltips', () => {
  const sfc = (tpl: string) => `<template>${tpl}</template>`

  it('flags a sideways tooltip on a target that fills its column', () => {
    expect(misplacedTooltips(sfc(`<label v-tooltip.left="'Suffix'">Name</label>`)))
      .toEqual([{ tag: 'label', line: 1, side: 'left' }])
    expect(misplacedTooltips(sfc(`<select v-tooltip.right="'Palette'" />`)))
      .toEqual([{ tag: 'select', line: 1, side: 'right' }])
  })

  it('accepts the two placements PrimeVue clamps horizontally', () => {
    expect(misplacedTooltips(sfc(`<label v-tooltip.top="'Suffix'">Name</label>`))).toEqual([])
    expect(misplacedTooltips(sfc(`<select v-tooltip.bottom="'Palette'" />`))).toEqual([])
  })

  // No modifier is not "no opinion" — `align()` falls through to `alignRight`, whose flip chain ends
  // by re-applying itself with no bounds test, so it is the one placement that can land anywhere.
  it('flags a BARE v-tooltip on a wide host', () => {
    expect(misplacedTooltips(sfc(`<div v-tooltip="'Row'">x</div>`)))
      .toEqual([{ tag: 'div', line: 1, side: 'none' }])
  })

  // Sideways is CORRECT for these: an icon or a word has room beside it and no row above or below
  // worth covering. Widening the rule to them would turn a real signal into a style sweep.
  it('leaves narrow, inline hosts alone', () => {
    expect(misplacedTooltips(sfc(`<button v-tooltip.left="'Delete'"><i class="pi pi-trash" /></button>`)))
      .toEqual([])
    expect(misplacedTooltips(sfc(`<span v-tooltip.right="'Note'">n</span>`))).toEqual([])
    expect(misplacedTooltips(sfc(`<i v-tooltip.right="'Info'" />`))).toEqual([])
  })

  // A checkbox is ~13px, so beside it is the only sensible place and `.bottom` would drop the tip
  // onto the next row. Exempt by TYPE, not by allow-list: it is a class of control, not a site.
  it('exempts a radio or checkbox, but not a range', () => {
    expect(misplacedTooltips(sfc(`<input type="checkbox" v-tooltip.right="'All'" />`))).toEqual([])
    expect(misplacedTooltips(sfc(`<input type="radio" v-tooltip.right="'Pick'" />`))).toEqual([])
    expect(misplacedTooltips(sfc(`<input type="range" v-tooltip.right="'42'" />`)))
      .toEqual([{ tag: 'input', line: 1, side: 'right' }])
  })

  it('ignores an untipped element and a primitive\'s own definition', () => {
    expect(misplacedTooltips(sfc(`<label>Name</label>`))).toEqual([])
    expect(misplacedTooltips(sfc(`<CcToggle v-tooltip.left="'x'" />`), 'src/components/CcToggle.vue'))
      .toEqual([])
  })
})

describe('nestedTooltips', () => {
  const sfc = (tpl: string) => `<template>${tpl}</template>`

  it('flags a tipped control inside a tipped container', () => {
    expect(nestedTooltips(sfc(`<div v-tooltip="'Row'"><button v-tooltip="'Hide'">x</button></div>`)))
      .toEqual([{ tag: 'button', line: 1, tooltip: "'Hide'", why: 'heading' }])
  })

  it('does NOT flag siblings — they can never both be hovered', () => {
    expect(nestedTooltips(sfc(`<div><span v-tooltip="'A'">a</span><button v-tooltip="'B'">b</button></div>`)))
      .toEqual([])
  })

  it('does NOT flag a tipped container whose children carry none', () => {
    expect(nestedTooltips(sfc(`<div v-tooltip="'Row'"><span>a</span></div>`))).toEqual([])
  })
})

describe('unnamedToggles', () => {
  const sfc = (tpl: string) => `<template>${tpl}</template>`

  it('flags a toggle whose caption sits outside the component', () => {
    // the shape the whole plot-options panel is written in — the row says what it is, the control
    // itself says nothing, and a screen reader only ever sees the control
    expect(unnamedToggles(sfc(`<div><span>Legend</span><CcToggle v-model="x" /></div>`)))
      .toEqual([{ tag: 'CcToggle', line: 1 }])
  })

  it('accepts any of the three ways to name one', () => {
    expect(unnamedToggles(sfc(`<CcToggle label="Legend" v-model="x" />`))).toEqual([])
    expect(unnamedToggles(sfc(`<CcToggle :label="lbl" v-model="x" />`))).toEqual([])
    expect(unnamedToggles(sfc(`<CcToggle aria-label="Legend" v-model="x" />`))).toEqual([])
    expect(unnamedToggles(sfc(`<CcToggle :aria-label="p.label" v-model="x" />`))).toEqual([])
    expect(unnamedToggles(sfc(`<CcToggle v-model="x">Use discrete GPU</CcToggle>`))).toEqual([])
  })

  it('does NOT accept a tooltip as a name', () => {
    // hover help and an accessible name are different things; the ancestor rule the tooltip checks
    // use is exactly what cannot supply the latter
    expect(unnamedToggles(sfc(`<div v-tooltip="'Show the key'"><CcToggle v-model="x" /></div>`)))
      .toEqual([{ tag: 'CcToggle', line: 1 }])
  })

  it('is not confused by a > inside a handler', () => {
    expect(unnamedToggles(sfc(`<CcToggle @update:model-value="v => set(v)" label="X" />`))).toEqual([])
  })

  it('skips CcToggle.vue itself — the primitive cannot name its own callers', () => {
    expect(unnamedToggles(sfc(`<CcToggle v-model="x" />`), 'src/components/CcToggle.vue')).toEqual([])
  })
})

describe('duplicateTooltips', () => {
  it('flags a toggle repeating its heading, expression for expression', () => {
    const src = `<template><div><label v-tooltip.left="param.tip">Overwrite</label>
                 <CcToggle v-tooltip.right="param.tip" /></div></template>`
    expect(duplicateTooltips(src))
      .toEqual([{ tag: 'CcToggle', line: 2, tooltip: 'param.tip', why: 'heading' }])
  })

  it('flags a repeated literal too', () => {
    const src = `<template><div><label v-tooltip="'Frame lags'">Scales</label>
                 <ChipSelect v-tooltip="'Frame lags'" /></div></template>`
    expect(duplicateTooltips(src).map(d => d.tag)).toEqual(['ChipSelect'])
  })

  it('leaves a control whose tooltip says something the heading does not', () => {
    const src = `<template><div><label v-tooltip="'Frame lags'">Scales</label>
                 <ChipSelect v-tooltip="'Pick at least one'" /></div></template>`
    expect(duplicateTooltips(src)).toEqual([])
  })

  it('leaves an uncovered control that carries its own tooltip', () => {
    expect(duplicateTooltips(`<template><ChipSelect v-tooltip="'Planes'" /></template>`)).toEqual([])
  })

  it('is scoped to the overlay-prone controls — a select repeating its label is only redundant', () => {
    const src = `<template><div><label v-tooltip="param.tip">A</label>
                 <select v-tooltip="param.tip" /></div></template>`
    expect(duplicateTooltips(src)).toEqual([])
  })

  it('does not pair a control with a heading from an earlier, closed row', () => {
    const src = `<template><div><label v-tooltip="param.tip">A</label></div>
                 <div><CcToggle v-tooltip="param.tip" /></div></template>`
    expect(duplicateTooltips(src)).toEqual([])
  })

  // The second double, and the one no string comparison can find: the group tooltip and the
  // per-option tips say the same thing in different words. The tips live in the SCRIPT, so this only
  // works by resolving the `:options` identifier back into it.
  it('flags a group tooltip sitting on top of per-option tips', () => {
    const src = `<script setup>
const MODES = [{ value: 'a', label: '', icon: 'pi pi-stop', tip: 'Rectangle gate' }]
</` + `script>
<template><ChipSelect :options="MODES" v-tooltip="'Shape to draw with'" /></template>`
    expect(duplicateTooltips(src)).toEqual([
      { tag: 'ChipSelect', line: 4, tooltip: "'Shape to draw with'", why: 'per-option' },
    ])
  })

  it('leaves a row that has one or the other', () => {
    const withTips = `<script setup>
const MODES = [{ value: 'a', tip: 'Rectangle gate' }]
</` + `script>
<template><ChipSelect :options="MODES" /></template>`
    const withGroup = `<script setup>
const MODES = [{ value: 'a', label: 'Rect' }]
</` + `script>
<template><ChipSelect :options="MODES" v-tooltip="'Shape to draw with'" /></template>`
    expect(duplicateTooltips(withTips)).toEqual([])
    expect(duplicateTooltips(withGroup)).toEqual([])
  })

  it('does not guess when it cannot follow the options binding', () => {
    // `:options` is a PROP here, so the tips are unknowable. Reporting on a guess would send someone
    // to delete the only help the row has.
    const src = `<template><ChipSelect :options="props.options" v-tooltip="'Pick one'" /></template>`
    expect(duplicateTooltips(src)).toEqual([])
  })
})

describe('hasPerOptionTips', () => {
  const script = `
const WITH = [{ value: 'a', tip: 'x' }]
const WITHOUT = [{ value: 'a', label: 'A' }]
`
  it('resolves an identifier back into the script', () => {
    expect(hasPerOptionTips(script, ':options="WITH"')).toBe(true)
    expect(hasPerOptionTips(script, ':options="WITHOUT"')).toBe(false)
  })

  it('reads an inline array literal directly', () => {
    expect(hasPerOptionTips('', `:options="[{ value: 'a', tip: 'x' }]"`)).toBe(true)
    expect(hasPerOptionTips('', `:options="[{ value: 'a' }]"`)).toBe(false)
  })

  it('answers null — not false — when it cannot tell', () => {
    expect(hasPerOptionTips(script, ':options="fromAProp"')).toBeNull()
    expect(hasPerOptionTips(script, 'multiple')).toBeNull()
  })

  it('stops at the next binding, so a later tip is not attributed to this one', () => {
    expect(hasPerOptionTips(script, ':options="WITHOUT"')).toBe(false)
  })

  // The blind spot that let a real double tooltip ship: options built by a FUNCTION rather than bound
  // as an identifier answered a flat `false`, so coverage demanded a tooltip on a chip row whose chips
  // were already tipped, and the per-option duplicate check never fired (Dominik, 2026-08-17).
  const built = `
function tippedOptions(k: string) {
  return items(k).map(i => ({ value: i.to, label: i.label, tip: i.tip }))
}
const plainOptions = (k: string) => items(k).map(i => ({ value: i.to, label: i.label }))
`
  it('follows a function CALL to the function that builds the options', () => {
    expect(hasPerOptionTips(built, ':options="tippedOptions(g.heading)"')).toBe(true)
    expect(hasPerOptionTips(built, ':options="plainOptions(g.heading)"')).toBe(false)
  })

  it('follows the root of a member/index expression', () => {
    const byGroup = `const byGroup = { data: [{ value: 'a', tip: 'x' }] }`
    expect(hasPerOptionTips(byGroup, ':options="byGroup[g.heading]"')).toBe(true)
  })

  it('still answers null for a v-for alias it cannot resolve', () => {
    expect(hasPerOptionTips(built, ':options="g.options"')).toBeNull()
  })
})
