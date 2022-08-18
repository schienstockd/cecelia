
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cecelia

<!-- badges: start -->
<!-- badges: end -->

The goal of cecelia is to …

## Installation

You can install the development version of cecelia like so:

``` r
if (!require("remotes", quietly = TRUE))
  install.packages("remotes")
remotes::install_github("schienstockd/cecelia")
```

For first time users, you will need to define base directory where
configuration files, models and the shiny app will be stored. ‘Cecelia’
depends on a python environment which needs to be created.

``` r
library(cecelia)

# setup cecelia directory
cciaSetup("~/path/to/cecelia")

# create conda environment
cciaCondaCreate()
# cciaCondaCreate(envType = "image-nogui") # to use without gui
# cciaCondaCreate(envType = "flow") # for flow based only

# download models for deep-learning
cciaModels()
```

(!) Adjust the ‘\~/path/to/cecelia/custom.yml’ to your system.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(cecelia)
## basic example code
```
