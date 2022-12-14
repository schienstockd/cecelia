
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

**This package currently only works on Unix systems.** For `Windows`
system, or if you prefer a containerised version, we also have a [Docker
image](https://github.com/schienstockd/ceceliaDocker).

We designed `cecelia` to also process jobs on the `HPC` (High
Performance Computing) system. We currently only support `Slurm` as a
scheduler. If you want to set this up on your system - please open an
`issue` and we will get you started.

(Optional) All components can be packaged within a `conda` environment.
We recommend to install
[`miniconda`](https://docs.conda.io/en/latest/miniconda.html) if you
want to keep a separate environment. If you opt for this, then you
should also install `RStudio` within that conda environment.

``` bash
conda create -y -n r-cecelia-env -c conda-forge python=3.9
conda activate r-cecelia-env
conda install -y -c conda-forge r-base=4.1.2 rstudio
rstudio
```

You can install the development version of `cecelia` like so:

``` r
if (!require("remotes", quietly = TRUE))
  install.packages("remotes")
remotes::install_github("schienstockd/cecelia", Ncpus = 4, repos = "https://cloud.r-project.org")
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

# install App requirements
# (i) they are not needed when using only markdown files or on HPC
cciaAppRequirements(repos = "https://cloud.r-project.org")

# install Bioconductor requirements
cciaBiocRequirements(repos = "https://cloud.r-project.org")

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

Compensation controls can be added into one `experimental set` which can
be used by `autospill` to calculate a `compensation matrix`. This matrix
can then be applied to the other samples.

3.  The rest of the pipeline for `gating` and `plotting` is the same as
    for image analysis.

## Running workflows from `RMarkdown`

All Processing available in the app can be done from `RMarkdown` as
well. Every image is `ReactivePersistentObject` whose state is saved in
an `RDS` file and is `reactive` in a `shiny` app context. These objects
can be loaded and manipulated as such:

``` r
library(cecelia)
cciaUse("~/path/to/cecelia")

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

Cell populations can be created using `clustering` or `gating`. `Gating`
cell populations will give you more control when using fewer markers.
`Clustering` will be more beneficial when using `multiplex` images to
identify multiparameter cell populations. In this case, we will utilise
`gating`. We have to create a `GatingSet` from the `label properties`.

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
library(ggplot2)
library(tidyverse)

library(cecelia)
cciaUse("~/path/to/cecelia")

# set test variables
pID <- "s3n6dR"   # project ID
versionID <- 1    # version ID
uID <- "LvfcHB"   # image ID

# init ccia object
cciaObj <- initCciaObject(
  pID = pID, uID = uID, versionID = versionID, initReactivity = FALSE
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
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/2D_spleen/7_interactions_heat.png" height="250"/>
</p>

8.  Population detection by clustering

We have incorporated `UMAP` and `Leiden` to detect populations by
clustering. Clustering can be done on `individual` images or combined on
`multiple` images from the same set. If `multiple` images are used, the
clustering results will be written back to the original `individual`
images and subsequent analysis such as `neighbour detection` can be done
on the `individual` images again. This is useful when processing a
`batch` of images that have the same staining.

It is also possible to do `sequential clustering`, as a kind of
multidimensional gate, by selecting `Root populations` from which to
calculate clusters. **When you do not tick `Keep other populations` the
other populations will be removed during clustering. So if you want to
sequential clusters, please tick that box.**

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/2D_spleen/8_pop_clustering_params.png" height="220"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/2D_spleen/8_pop_clustering_heat.png" height="220"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/2D_spleen/8_pop_clustering_UMAP.png" height="220"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/2D_spleen/8_pop_clustering_image.png" height="220"/>
</p>

## 3D static image analysis - Lymph node example

- [`Download 3D lymph node example`](https://cloudstor.aarnet.edu.au/plus/s/cJsQOyk6d1Fsg4M/download?path=%2F&files=M4-v1%20CTRL%20z_100um%20tile_10x7_stitch%20COMPED%20WORKING%20GATED%20DOWNSAMPLED%204C-scaled.tif)

1.  Create project

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_LN/1_create_project.png" height="300"/>
</p>

2.  Import image

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_LN/2_import_image.png" height="300"/>
</p>

3.  Assign channel names

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_LN/3_assign_channels.png" height="300"/>
</p>

4.  Segment cells

We commonly use fluorescently stained cells for `two-photon` and
`histology` imaging. We trained a `cellpose` model, called
`ccia Fluorescent`, to detect these fluorescent cells as the pre-trained
models could not segment them. We can use this model to sequentially
segment `dendritic cells` (stained with `TRITC`) and `T cells`.

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_LN/4_seg_cellpose_DC.png" height="200"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_LN/4_seg_cellpose_T.png" height="200"/>
</p>

Sometimes the segmentation can benefit from a small morphological
filter. In this case we can use a `Gaussian` filter of `1` to improve
segmentation results.

5.  Gate cells

During imaging of thick tissue slices that were stained with antibodies
it can happen that staining intensity varies across the `depth` of the
tissue due to antibody penetration or differences in light properties
and scattering within the tissue or increase of laser power or gain due
to reduction of signal with increasing depth. For this reason, we
incorporated `depth correction` by fitting `polynomial` function to the
signal across `depth`.

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_LN/depth_correction/TRITC_original.png" height="250"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_LN/depth_correction/TRITC_corrected.png" height="250"/>
</p>

We can use `gating` to identify `TRITC+ dendritic cells` and `T cells`.
In this image, we can also gate on `CD68+ T cells`.

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_LN/5_gating.png" height="300"/>
</p>

6.  Detect spatial interactions

For 3D objects it is helpful to generate `3D meshes` during
segmentation. These `meshes` can then be utilised to detect clusters of
cells and interactions between cells. Then we can check whether
`CD69+ T cells` are in contact with `migrating TRITC+ cells` and
visualise these in `napari`.

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_LN/6_spatial_clusters.png" height="220"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_LN/6_spatial_contacts.png" height="220"/>
</p>

``` r
library(ggplot2)
library(tidyverse)

library(cecelia)
cciaUse("~/path/to/cecelia")

# set test variables
pID <- "4UryU2"   # project ID
versionID <- 1    # version ID
uID <- "jpVjeh"   # image ID

# init ccia object
cciaObj <- initCciaObject(
  pID = pID, uID = uID, versionID = versionID, initReactivity = FALSE
)

# get populations
popDT <- cciaObj$popDT(
  "flow", pops = c("/nonDebris/gBT/clustered", "/nonDebris/gBT/non.clustered"),
  includeFiltered = TRUE)

# get summary
summaryToPlot <- popDT %>%
  group_by(pop, `flow.cell.contact#flow./nonDebris/others/TRITC`) %>%
  summarise(n = n()) %>%
  mutate(
    freq = n/sum(n),
    pop = str_extract(pop, "[^\\/]+$")
    )
    
# plot frequencies
ggplot(summaryToPlot) +
  aes(1, freq, fill = `flow.cell.contact#flow./nonDebris/others/TRITC`) +
  theme_classic() +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  xlim(0, 1.8) +
  facet_wrap(.~pop, nrow = 1) +
  ggtitle("TRITC contact") +
  theme(
    axis.text = element_text(size = 5),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
```

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_LN/6_spatial_napari.png" height="250"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_LN/6_TRITC_contact.png" height="250"/>
</p>

## 3D live image analysis - Two-photon lymph node example

- [`Download 2P example`](https://cloudstor.aarnet.edu.au/plus/s/cJsQOyk6d1Fsg4M)

1.  Create project

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_2P/1_create_project.png" height="300"/>
</p>

2.  Import image

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_2P/2_import_image.png" height="300"/>
</p>

3.  Assign channel names

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_2P/3_assign_channels.png" height="300"/>
</p>

4.  Autofluorescence and drift correction

We correct autofluorescence by dividing channels from each other. In
this example there are only two which we can use for channel correction.
The same module function will also do drift correction based on
[`phase cross correlation`](https://scikit-image.org/docs/stable/api/skimage.registration.html#skimage.registration.phase_cross_correlation).
We will use the `AF generated` channel that is generated during
autofluorescence correction.

\[IMAGES HERE\]

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_2P/4_AF_drift_A.png" height="300"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_2P/4_AF_drift_image.png" height="300"/>
</p>

5.  Segment cells

We can utilise `cellpose` and the `ccia Fluorescent` model to segment
both cell types.

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_2P/5_seg_cellpose_A.png" height="220"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_2P/5_seg_cellpose_B.png" height="220"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_2P/5_seg_cellpose_image.png" height="220"/>
</p>

6.  Track cells

We incorporated
[`btrack`](https://github.com/quantumjot/BayesianTracker/tree/38b144c09f384cb30c2fd9572f19f8dfb3007fca)
to track segmented cells. `Filters` can be used to filter based on
object measurements.

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_2P/6_tracking_A.png" height="220"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_2P/6_tracking_B.png" height="220"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_2P/6_tracking_B_filters.png" height="220"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_2P/6_tracking_image.png" height="220"/>
</p>

7.  Extract cell behaviour

We utilised `HMM` (Hidden markov model) to extract behaviour from the
generated tracks. This analysis extracts a defined number of cellular
behaviours based on `shape` and `movement` parameters. For this method,
we combine tracking data from multiple images - therefore, we need to
tick the box to `Combine images` when running the task. This will run
the task with the selected images from the `set`.

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_2P/7_HMM_task.png" height="220"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_2P/7_HMM.png" height="220"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_2P/7_HMM_result.png" height="220"/>
</p>

We can visualise these behaviours on the image by creating a
`filtered population`.

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_2P/7_HMM_pop.png" height="250"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_2P/7_HMM_pop_image.png" height="250"/>
</p>

Then we can map these behaviours in `time` and `space`.

<p float="left">
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_2P/7_HMM_time.png" height="220"/>
<img src="https://github.com/schienstockd/cecelia/raw/master/im/examples/3D_2P/7_HMM_space.png" height="220"/>
</p>

``` r
library(ggplot2)
library(tidyverse)

library(cecelia)
cciaUse("~/path/to/cecelia")

# set test variables
pID <- "kicbHw"   # project ID
versionID <- 1    # version ID
uID <- "SRMXQH"   # image ID

# init ccia object
cciaObj <- initCciaObject(
  pID = pID, uID = uID, versionID = versionID, initReactivity = FALSE
)

# get popDTs for set
popDTs <- cciaObj$popDT(
  popType = "live", pops = c("cellA/tracked", "cellB/tracked"),
  includeFiltered = TRUE, flushCache = TRUE)

# get frequencies of HMM at time points
hmmTime <- popDTs %>%
  dplyr::filter(
    !is.na(live.cell.hmm.state.default),
    live.cell.track.clusters.default != "NA"
    ) %>%
  group_by(uID,
    centroid_t,
    live.cell.hmm.state.default
    ) %>%
  summarise(n = n()) %>%
  mutate(
    live.cell.hmm.state.default = as.factor(live.cell.hmm.state.default),
    freq = n/sum(n)
    )

time.interval <- cciaObj$cciaObjects()[[1]]$omeXMLTimelapseInfo()$interval

ggplot(hmmTime,
       aes((centroid_t * time.interval), freq,
           color = live.cell.hmm.state.default,
           fill = live.cell.hmm.state.default,
           )) +
  geom_smooth() +
  theme_classic() +
  scale_color_brewer(name = NULL, palette = "Dark2") +
  scale_fill_brewer(name = NULL, palette = "Dark2") +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
    ) +
  xlab("Time (min)") + ylab("HMM frequency") +
  ylim(0, 1) + facet_grid(uID~.)
  
# get frequencies of hmm states at time points
hmmSpace <- copy(popDTs) %>%
  dplyr::filter(!is.na(live.cell.hmm.state.default))

# get density colours
hmmSpace$density <- ""
for (i in unique(hmmSpace$uID)) {
  for (j in unique(hmmSpace$live.cell.hmm.state.default)) {
    x <- hmmSpace[hmmSpace$uID == i & hmmSpace$live.cell.hmm.state.default == j,]
    
    hmmSpace[hmmSpace$uID == i & hmmSpace$live.cell.hmm.state.default == j,]$density <- .flowColours(
      x$centroid_x, x$centroid_y)
  }
}

ggplot(hmmSpace, aes(centroid_x, centroid_y)) +
  theme_classic() +
  plotThemeDark(
    fontSize = 8,
    legend.justification = "centre"
    ) +
  geom_point(
    color = hmmSpace$density, size = 0.5
    ) +
  scale_color_brewer(name = NULL, palette = "Set3") +
  coord_fixed() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = 'bottom'
    ) +
  facet_grid(uID~live.cell.hmm.state.default)
```
