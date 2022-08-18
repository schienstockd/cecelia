
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
#> Downloading GitHub repo schienstockd/cecelia@HEAD
#> 
#> * checking for file ‘/private/var/folders/rb/m9wptf8x58x_82t7lg081kfs1fj7kv/T/Rtmp8yjggh/remotes5c7f576765e/schienstockd-cecelia-0e338b7/DESCRIPTION’ ... OK
#> * preparing ‘cecelia’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> * building ‘cecelia_0.01.tar.gz’
#> Warning: invalid uid value replaced by that for user 'nobody'
#> Warning: invalid gid value replaced by that for user 'nobody'
#> Installing package into '/private/var/folders/rb/m9wptf8x58x_82t7lg081kfs1fj7kv/T/RtmpDx9lBN/temp_libpath3be62a4653eb'
#> (as 'lib' is unspecified)
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
