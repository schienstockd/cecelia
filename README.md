
<!-- README.md is generated from README.Rmd. Please edit that file -->

![Image](./im/logo.png)

<!-- badges: start -->
<!-- badges: end -->

The goal of `cecelia` is to simplify image analysis for immunologists
and integrate static and live cell imaging with flow cytometry data. The
package primarily builds upon [`napari`](https://napari.org) and
[`shiny`](https://shiny.rstudio.com/). Our aim was to combine shiny’s
graph plotting engine with napari’s image display.

**This package is pre-alpha**

## Installation

**This package currently only works on Unix systems.** We have a Docker
version to support other systems if necessary, so do open an issue if
that is needed.

You can install the development version of cecelia like so:

``` r
if (!require("remotes", quietly = TRUE))
  install.packages("remotes")
remotes::install_github("schienstockd/cecelia")
```

For first time users, you will need to define base directory where
configuration files, models and the shiny app will be stored. `cecelia`
depends on a python environment which needs to be created. There are
multiple options available depending on how you would like to use the
app:

- `image` For image analysis on Desktop

- `image-nogui` For image processing without GUI

- `flow` For flow cytometry analysis

``` r
library(cecelia)

# install all required R libraries
cciaRequirements()

# setup cecelia directory
cciaSetup("~/path/to/cecelia")

# create conda environment
cciaCondaCreate()
# cciaCondaCreate(envType = "image-nogui") # to use without gui
# cciaCondaCreate(envType = "flow") # for flow based only

# download models for deep-learning segmentation
cciaModels()

# create app
cciaCreateApp()
```

You have to adjust the parameters in `~/path/to/cecelia/custom.yml` to
your system. You need download/install:

- [`bftools`](https://downloads.openmicroscopy.org/bio-formats/6.7.0/artifacts/bftools.zip)

- [`bioformats2raw`](https://github.com/glencoesoftware/bioformats2raw/releases/download/v0.4.0/bioformats2raw-0.4.0.zip)

- [`ImageJ`](https://imagej.net/imagej-wiki-static/Fiji/Downloads) if
  using Spot segmentation

For `ImageJ`, activate the following update sites:

- IJPB-plugins

- 3D-ImageJ-Suite

- Bio-Formats

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
      source:
        env: "r-cecelia-env"
  imagej:
    path: "/Applications/Fiji.app/Contents/MacOS/ImageJ-macosx"
```

## Image analysis general workflow

``` r
library(cecelia)
cciaUse("~/path/to/cecelia", initJupyter = TRUE)
cciaRunApp(port = 6860)
```

1.  Create a Project for `static` or `live`analysis.

2.  Images have to be imported as `OME-ZARR`. Choose `Import Images` and
    create an `Experimental Set`. It is helpful if all images within
    this set have the same colour combinations. Add Images. Select all
    images you want to import and choose `OME-ZARR`. Select the required
    `pyramid scales` and `run` the task.

3.  Select `Image Metadata` and click `Load Metadata` to load the
    channel information. You can assign channels either one by one by
    selecting a channel and `Specify Value` \> `Assign Value`.
    Alternatively, you can give a list of channels in the box and click
    `Assing channels`. You can add further experimental attributes by
    `Create Attribute` and adding respecive values for the individual
    images.

4.  Select `Cell Segmentation` to segment your images.

5.  Select `Population Gating` to use flow cytometry like gating to
    define populations. Select `Populations Clustering` to use cluster
    algorithms to define populations.

6.  Select `Spatial Analysis` to define spatial neighbourhoods, detect
    clustering cells or detect contact between cells.

## 2D static image analysis - Spleen example

- [`Download 2D spleen example`](https://cloudstor.aarnet.edu.au/plus/s/cJsQOyk6d1Fsg4M/download?path=%2F&files=WT2a-A-3-TCRb-421-CD169-FITC-33D1-PE-XCR1-APC.czi)

1.  Create project

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/2D_spleen/1_create_project.png" height="300"/>
</p>

2.  Import image

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/2D_spleen/2_import_image.png" height="300"/>
</p>

3.  Assign channel names

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/2D_spleen/3_assign_channels.png" height="300"/>
</p>

4.  Segment cells

With conventional confocal microscopy it is not always possible to
include a nuclear stain for cell segmentation. In this case, we can
check whether `cellpose` or a sequence of morphological filters which
segments donut- and blob-like objects (`donblo`) works for a partiular
image.

- `cellpose` is a good choice for most cases. We use `cellpose` to
  segment a single merged image. We can create a sequence of merged
  images to create individual segmentations if necessary.

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/2D_spleen/4_seg_cellpose_params.png" height="300"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/2D_spleen/4_seg_cellpose.png" height="300"/>
</p>

- In this case, `cellpose` did not capture some of the more dense and
  noisy cells. We have implemented a simple sequence of morphological
  filters with subsequent spot detection and segmentation in `ImageJ`
  using `TrackMate` and the `3D Image Suite`. The quality of the
  segmentation is lower than `cellpose` but it will capture more cells,
  such as the `yellow XCR1+ DCs` within the `T cell zone`.

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/2D_spleen/4_seg_donblo_params.png" height="300"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/2D_spleen/4_seg_donblo.png" height="300"/>
</p>

5.  Gate cells

Cell populations can be created using `clustering` or `gating`. In this
case, we will utilise `gating`. We have to create a `GatingSet` from the
`label properties`.

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/2D_spleen/5_create_gatingset.png" height="300"/>
</p>

After this we can open the image and do sequential gating for `T cells`,
`Macrophages`, `cDC1` and `cDC2`.

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/2D_spleen/5_gating.png" height="250"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/2D_spleen/5_gating_image.png" height="250"/>
</p>

6.  Create spatial neighbours

We can use these populations to create `spatial neighbours`.

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/2D_spleen/6_spatial_neighbours.png" height="300"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/2D_spleen/6_spatial_neighbours_image.png" height="300"/>
</p>

7.  Custom plotting of interactions

Our aim is to provide custom plots within `cecelia` but this is still in
development. The following is illustrating how to use the generated
populations for customised plotting within `RMarkdown`. This simple
example shows the interactions between `T cells` and `cDC1` in the
`T cell zone`.

``` r
# set test variables
pID <- "s3n6dR"
versionID <- 1

# init ccia object
cciaObj <- initCciaObject(
  pID = pID, uID = "LvfcHB", versionID = versionID, initReactivity = FALSE
)

# get populations
popDT <- cciaObj$popDT("flow", pops = cciaObj$popPaths("flow"))

# get spatial information
spatialDT <- cciaObj$spatialDT()

# join pops
spatialDT[popDT[, c("label", "pop")],
          on = c("to" = "label"),
          pop.to := pop]
spatialDT[popDT[, c("label", "pop")],
          on = c("from" = "label"),
          pop.from := pop]

# filter same type associations
spatialDT <- spatialDT[pop.to != pop.from]

# get interaction frequencies
freqRegions <- spatialDT %>%
  group_by(pop.from, pop.to) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  drop_na() %>%
  ungroup() %>%
  complete(pop.from, pop.to, fill = list(freq = 0))

ggplot(freqRegions,
       aes(pop.from, pop.to)) +
  theme_classic() +
  geom_tile(aes(fill = freq), colour = "white", size = 0.5) +
  viridis::scale_fill_viridis(
    breaks = c(0, 0.5),
    labels = c(0, 0.5)
  ) +
  theme(
    legend.title = element_blank(),
    legend.key.size = unit(3, "mm"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  ) + xlab("") + ylab("")
```

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/2D_spleen/7_interactions_heat.png" height="300"/>
</p>

## Flow Cytometry general workflow

``` r
library(cecelia)
cciaUse("~/path/to/cecelia", initJupyter = FALSE)
cciaRunApp(port = 6860)
```

1.  Create a Project for `flow`analysis.

2.  `FCS` files can be imported either from raw or other sources such as
    `FlowJo`. They will converted into an
    [`Anndata`](https://anndata.readthedocs.io) to perform clustering
    and a [`GatingSet`](https://github.com/RGLab/flowWorkspace) to
    perform manual gating.

3.  The rest of the pipeline is the same as for image analysis.

## Running workflows from `RMarkdown`

All Processing available in the app can be done from `RMarkdown` as
well. Every image is `ReactivePersistentObject` whose state is saved in
an `RDS` file and is `reactive` in a `shiny` app context. These objects
can be loaded and manipulated as such:

``` r
library(cecelia)

# set test variables
pID <- "pEdOoZ"   # project ID
versionID <- 2    # version ID
uID <- "tPl6da"   # image ID

# init ccia object
cciaObj <- initCciaObject(
  pID = pID, uID = uID, versionID = versionID, initReactivity = FALSE
)

funParams <- list(
  pyramidScale = 4,
  dimOrder = "",
  createMIP = FALSE,
  rescaleImage = FALSE
)

cciaObj$runTask(
  funName = "importImages.omezarr",
  funParams = funParams,
  runInplace = TRUE
)
```

## Creating plots

One aim of `ceelia` is to provide plotting over the GUI with `shiny`. We
are currently in the process of testing processing data. This part of
the app is less developed as we learn which plots and statistics are
relevant. Plotting is currently done in `RMarkdown` files but we are
planning to incorporate these into the app.

``` r
# init experimental set
cciaObj <- initCciaObject(
  pID = pID, uID = "U7LRc9", versionID = versionID, initReactivity = FALSE
)

# get image attributes
exp.info <- cciaObj$summary(withSelf = FALSE, fields = c("Attr"))

# get cluster populations
popDT <- cciaObj$popDT(popType = "clust", includeFiltered = TRUE)

# now you can plot this as a normal data.table
library(ggplot2)

ggplot(popDT, aes(centroid_x, centroid_y, color = pop)) +
  theme_classic() +
  geom_point(aes(color = as.factor(clusters)), size = 0.5) +
  facet_wrap(.~uID, scales = "free")
```
