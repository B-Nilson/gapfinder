
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gapfinder

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/gapfinder)](https://CRAN.R-project.org/package=gapfinder)
<!-- badges: end -->

Many monitoring networks do not have full coverage of their intended
targets. Network managers are usually working with limited budgets for
expanding exisiting networks, and knowing where to prioritize
installations to maximize coverage is a useful tool for planning.

gapfinder provides the abiliity to identify optimal installation
locations to meet coverage goals.

You can provide potential installation locations, exisiting monitoring
locations (if any), locations desired to be covered, and individual
prioritization weights for potential locations and locations to be
covered. In return, gapfinder will calculate the optimal locations to
cover as much as possible, minimizing overlap, and allows for
prioritizing installations based on unique coverage of each
installation.

## Installation

You can install the development version of gapfinder like so:

``` r
# install.packages("pak")
pak::pak("B-Nilson/gapfinder")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(gapfinder)
rlang::check_installed("canadata")
rlang::check_installed("aqmapr")
# pak::pak("B-Nilson/canadata") # in case above fails

# Define where we could install monitors (Yukon communities)
install_at <- canadata::communities |>
  dplyr::filter(prov_terr == "YT") |>
  sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84") |>
  # Cities are easier to install than hamlets
  # here they provide 4x more coverage (4 levels of type)
  dplyr::mutate(ease_of_install = length(levels(type)) + 1 - as.numeric(type))

# Define what we want the monitors to cover (Yukon population)
to_cover <- canadata::gridded_2016_population |>
  dplyr::filter(stringr::str_detect(prov_terr, "YT")) |> # any cell that intersects Yukon
  sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")

# Define existing monitors (Canadian PM2.5 monitoring network - NAPS, PurpleAir, and AQEgg networks)
existing_locations <- "https://aqmap.ca/aqmap/data/aqmap_most_recent_obs.csv" |>
  read.csv() |>
  dplyr::select(site_id = sensor_index, network, lng, lat, name = monitor) |>
  sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")

# Find the optimal locations to get all YT population within 10 km of a monitor
# Coverage depends on population x ease of install
optimized_locations <- install_at |>
  optimize_coverage(
    to_cover = to_cover,
    existing_locations = existing_locations,
    cover_distance = units::set_units(10, "km"),
    weight_columns = c("ease_of_install", "total_population")
  )

# View the results
install_at |> print()
#> Simple feature collection with 66 features and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -140.8773 ymin: 60.00194 xmax: -128.7068 ymax: 68.98877
#> Geodetic CRS:  WGS 84
#> # A tibble: 66 × 6
#>    name      type  prov_terr fcst_zone             geometry ease_of_install
#>  * <chr>     <fct> <fct>     <chr>              <POINT [°]>           <dbl>
#>  1 Whitehor… city  YT        Whitehor… (-135.0549 60.72157)               4
#>  2 Carcross  town  YT        Whitehor… (-134.7087 60.16679)               3
#>  3 Carmacks  town  YT        Pelly - … (-136.2899 62.08877)               3
#>  4 Dawson C… town  YT        Dawson    (-139.4317 64.06066)               3
#>  5 Faro      town  YT        Faro - R… (-133.3553 62.22954)               3
#>  6 Mayo      town  YT        Mayo      (-135.8961 63.59269)               3
#>  7 Ross Riv… town  YT        Faro - R…  (-132.4495 61.9798)               3
#>  8 Watson L… town  YT        Watson L… (-128.7068 60.06243)               3
#>  9 Haines J… vill… YT        Haines J…   (-137.51 60.75272)               2
#> 10 Mount Lo… vill… YT        Whitehor… (-134.8568 60.44817)               2
#> # ℹ 56 more rows
to_cover |> print()
#> Simple feature collection with 105 features and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -140.9239 ymin: 60.0065 xmax: -128.6312 ymax: 67.6117
#> Geodetic CRS:  WGS 84
#> # A tibble: 105 × 6
#>    prov_terr fcst_zone         total_land_area total_population rural_population
#>  * <chr>     <chr>                      [km^2]            <int>            <int>
#>  1 YT        Haines Junction              94.3               20               20
#>  2 YT        Kluane Lake                  94.4               50               50
#>  3 YT        Kluane Lake                  42                 55               55
#>  4 YT        Kluane Lake                  41.8               70               70
#>  5 YT        Kluane Lake                  94.2                5                5
#>  6 YT        Beaver Creek,Klu…            94.2               15               15
#>  7 YT        Beaver Creek,Klu…            94.1               25               25
#>  8 YT        Beaver Creek                 85.3               85               85
#>  9 YT        Beaver Creek                 94.1                5                5
#> 10 YT        Pelly - Carmacks…            94.1               20               20
#> # ℹ 95 more rows
#> # ℹ 1 more variable: geometry <POINT [°]>
existing_locations |> print()
#> Simple feature collection with 2877 features and 3 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -166.07 ymin: 41.28884 xmax: -52.71147 ymax: 71.98604
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>    site_id network                               name
#> 1    85493      PA                         EAS_PA_034
#> 2   228711      PA                       EPL Feed LLc
#> 3   131991      PA AQSU-E63E, Lillooet Municipal Yard
#> 4   235975      PA                       St. Clements
#> 5    59777      PA                      Tetlin NWR HQ
#> 6   232453      PA     1327 Hwy 25 S, Kettle falls WA
#> 7   127005      PA    Copper River Native Association
#> 8    79343      PA                     WCAS_Lodgepole
#> 9   262993      PA                      Varsity Creek
#> 10  244739      PA                     Heathfield Sub
#>                      geometry
#> 1  POINT (-139.4264 64.06696)
#> 2  POINT (-122.2688 48.99162)
#> 3  POINT (-121.9433 50.69153)
#> 4  POINT (-80.65214 43.52306)
#> 5  POINT (-143.0391 63.32384)
#> 6  POINT (-118.0872 48.57004)
#> 7  POINT (-145.4295 62.06746)
#> 8   POINT (-115.317 53.10379)
#> 9  POINT (-122.7749 53.87064)
#> 10 POINT (-113.2009 53.85244)
optimized_locations |> print()
#> Simple feature collection with 15 features and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -139.2241 ymin: 60.0523 xmax: -128.7068 ymax: 64.02769
#> Geodetic CRS:  WGS 84
#> # A tibble: 15 × 6
#>    name      type  prov_terr fcst_zone             geometry ease_of_install
#>    <chr>     <fct> <fct>     <chr>              <POINT [°]>           <dbl>
#>  1 Mount Lo… vill… YT        Whitehor… (-134.8568 60.44817)               2
#>  2 Ross Riv… town  YT        Faro - R…  (-132.4495 61.9798)               3
#>  3 McClinto… haml… YT        Whitehor…  (-134.5415 60.5672)               1
#>  4 Takhini   haml… YT        Whitehor… (-135.5244 60.85375)               1
#>  5 Upper Li… haml… YT        Watson L…  (-128.9121 60.0523)               1
#>  6 Watson L… town  YT        Watson L… (-128.7068 60.06243)               3
#>  7 Destruct… haml… YT        Kluane L… (-138.7998 61.25143)               1
#>  8 Eureka C… haml… YT        Dawson    (-138.8768 63.58632)               1
#>  9 Mendenha… haml… YT        Whitehor… (-136.0167 60.76667)               1
#> 10 Stewart … vill… YT        Mayo       (-136.679 63.37544)               2
#> 11 Bear Cre… haml… YT        Haines J… (-137.6682 60.79649)               1
#> 12 Keno City haml… YT        Mayo      (-135.3022 63.90958)               1
#> 13 Bear Cre… haml… YT        Dawson    (-139.2241 64.02769)               1
#> 14 Clear Cr… haml… YT        Mayo      (-137.2788 63.78159)               1
#> 15 Jakes Co… haml… YT        Whitehor… (-133.9859 60.33931)               1
```
