
<!-- README.md is generated from README.Rmd. Please edit that file -->

<figure>
<img src="./im/cciaLogo.png" alt="Image" />
<figcaption aria-hidden="true">Image</figcaption>
</figure>

<!-- badges: start -->
<!-- badges: end -->

The goal of `cecelia` is to simplify image analysis for immunologists
and integrate static and live cell imaging with flow cytometry data. The
package primarily builds upon [`napari`](https://napari.org) and
[`shiny`](https://shiny.rstudio.com/). Our aim was to combine shiny’s
graph plotting engine with napari’s image display.

**This package is pre-alpha**

## Genereal installation guidelines

# System requirements

Software dependencies will be installed during the installation
processing of the R-package. The framework has primarily been tested on
an old MacOS 12.7.6 (with HPC access) and new M2 14.6.1. To install the
software on Windows PCs you must install the
[`Docker`](https://github.com/schienstockd/ceceliaDocker) version as
shiny requires a UNIX system. For cell segmentation and deep learning
denoising we recommend a GPU that is recognised by
[`torch`](https://pytorch.org/get-started/locally/).

# Installation guide

See
[`read-the-docs`](https://cecelia.readthedocs.io/en/latest/index.html)
for installation and tutorials. Installation of the R-package with all
shiny and python dependencies should require less than 1 h. The Docker
based installation depends on the user’s bandwidth but typically
requires less than 10 min.
