
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cecelia

<!-- badges: start -->
<!-- badges: end -->

The goal of `cecelia` is to simplify image analysis for immunologists
and integrate static and live cell imaging with flow cytometry data. The
package primarily builds upon [`napari`](https://napari.org) and
[`shiny`](https://shiny.rstudio.com/). Our aim was to combine shiny’s
grahp plotting engine with napari’s image display.

## Installation

\*\* This package currently only works on Unix systems \*\* You can
install the development version of cecelia like so:

``` r
if (!require("remotes", quietly = TRUE))
  install.packages("remotes")
remotes::install_github("schienstockd/cecelia")
```

For first time users, you will need to define base directory where
configuration files, models and the shiny app will be stored. `cecelia`
depends on a python environment which needs to be created. There are
multiple options available depending on how you would like to use the
app: \* For image analysis on Desktop \* For image processing without
GUI \* For flow cytometry analysis

``` r
library(cecelia)

# setup cecelia directory
cciaSetup("~/path/to/cecelia")

# create conda environment
cciaCondaCreate()
# cciaCondaCreate(envType = "image-nogui") # to use without gui
# cciaCondaCreate(envType = "flow") # for flow based only

# download models for deep-learning segmentation
cciaModels()
```

You have to adjust the parameters in `~/path/to/cecelia/custom.yml` to
your system. You need download/install: \*
[`bftools`](https://downloads.openmicroscopy.org/bio-formats/6.7.0/artifacts/bftools.zip)
\*
[`bioformats2raw`](https://github.com/glencoesoftware/bioformats2raw/releases/download/v0.4.0/bioformats2raw-0.4.0.zip)
\* [`ImageJ` if using Spot
segmentation](https://imagej.net/imagej-wiki-static/Fiji/Downloads)

For `ImageJ`, activate the following update sites: \* IJPB-plugins \*
3D-ImageJ-Suite \* Bio-Formats

``` yml
default:
  dirs:
    bftools: "/Applications/BFTools"
    bioformats2raw: "/Applications/glencoe/bioformats2raw-0.3.0"
    projects: "/your/project/directory/"
  volumes:
    SSD: "/your/ssd/directory/"
    home: "~/"
    computer: "/"
  python:
    conda:
      env: "r-cecelia-env"
      dir: "~/opt/anaconda3/"
      source:
        env: "r-cecelia-env"
  imagej:
    path: "/Applications/Fiji.app/Contents/MacOS/ImageJ-macosx"
```

## Example

``` r
library(cecelia)
## basic example code
```
