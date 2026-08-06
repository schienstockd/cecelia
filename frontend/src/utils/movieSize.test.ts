import { describe, it, expect } from 'vitest'
import { MAX_MOVIE_AXIS, movieAxisPlaceholder, movieSizeParams, parseMovieAxis } from './movieSize'

describe('parseMovieAxis', () => {
  it('reads a positive integer', () => {
    expect(parseMovieAxis('1920')).toBe(1920)
    expect(parseMovieAxis(1080)).toBe(1080)
  })

  it('treats blank and junk as unset, not as an error', () => {
    // an empty field means "the canvas size" — the default every movie had before these fields
    for (const v of ['', null, undefined, '0', '-4', 'wide', NaN]) {
      expect(parseMovieAxis(v as never)).toBeNull()
    }
  })

  it('floors a fractional entry', () => {
    expect(parseMovieAxis('1080.7')).toBe(1080)
  })

  it('clamps to the ceiling the renderer enforces', () => {
    expect(parseMovieAxis(99999)).toBe(MAX_MOVIE_AXIS)
  })
})

describe('movieAxisPlaceholder', () => {
  it('shows the canvas size when napari reported one', () => {
    expect(movieAxisPlaceholder(1512)).toBe('1512')
  })

  it('falls back to a word when it has not', () => {
    for (const v of [null, undefined, 0]) expect(movieAxisPlaceholder(v)).toBe('canvas')
  })
})

describe('movieSizeParams', () => {
  it('sends both axes or neither', () => {
    expect(movieSizeParams(1920, 1080)).toEqual({ sizeX: 1920, sizeY: 1080 })
    // one axis cannot describe a frame; sending half a size would letterbox without saying so
    expect(movieSizeParams(1920, null)).toEqual({})
    expect(movieSizeParams(null, 1080)).toEqual({})
    expect(movieSizeParams(null, null)).toEqual({})
  })
})
