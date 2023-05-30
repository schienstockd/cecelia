# see http://alisterburt.com/napari-workshops/notebooks/custom_colormaps.html

import matplotlib.colors
from napari.utils import Colormap

"""
Create single color map
"""
def cmap_single(values, name = 'custom'):
  return Colormap(
    [matplotlib.colors.to_rgba(x) for x in values],
    name = name)

