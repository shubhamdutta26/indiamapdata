
<!-- README.md is generated from README.Rmd. Please edit that file -->

# indiamapdata

<!-- badges: start -->
<!-- badges: end -->

The goal of indiamapdata is to serve as a container package for the maps
data used in indiamap package. This data has been separated to keep the
size of the indiamap package manageable and facilitate easier
maintenance, while enabling more frequent updates to the Indian map data
frame without relying on indiamap package updates.

## Map data

The geojson files are stored in the data-raw/geojson_files folder. These
files were read and appropriately modified using the `create_india_map`
function and saved as gpkg files in the inst/extdata folder. The
centroids were also calculated using the `compute_centroids` function.

## Installation

You can install the development version of indiamapdata from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("shubhamdutta26/indiamapdata")
```

## Usage

To begin using `indiamapdata`, import the package using the `library`
command:

``` r
library(indiamapdata)
```

## Acknowledgments

I would like to thank [pdil](https://github.com/pdil) since I have used
his `usmap` package to write most of this package. I would like to thank
[datta07](https://github.com/datta07) for the geojson files.
