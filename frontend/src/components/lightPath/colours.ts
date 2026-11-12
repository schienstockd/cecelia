// Excitation / emission strokes for the light-path figures (Okabe–Ito, plots/palettes.json).
import palettes from '../../plots/palettes.json'

const oi = palettes.palettes['okabe-ito']
export const LIGHT_PATH_COLOURS = { exc: oi[5], em: oi[4] }
