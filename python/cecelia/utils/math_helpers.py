"""
Rounding utilities used by DimUtils for snapping padded crop regions to
multiples of tile size.
"""


def round_up(x, mult):
    """Round x up to the nearest multiple of mult."""
    x -= x % -mult
    return x


def round_down(x, mult):
    """Round x down to the nearest multiple of mult."""
    x -= x % mult
    return x
