# tSeriesTools

R package for testing for unit root and stationarizing series.

The package contains functions that: (1) test for unit root and display
type of non-stationarity based on Ayat and Burridge (2000) sequencial
procedure (2) based on the exact type of non-stationarity identified,
stationarize the series

## Installation

``` r
library(devtools)
```

    ## Loading required package: usethis

``` r
install_github("marinaferent/tSeriesTools")
```

    ## Skipping install of 'tSeriesTools' from a github remote, the SHA1 (e32e4bd0) has not changed since last install.
    ##   Use `force = TRUE` to force installation

## Usage

``` r
library(tSeriesTools)
```

**unitRootDF_ABsequential()** - Tests a series for unit root using the
Dickey-Fuller test and following the Ayat and Burridge (2000) sequential
procedure. It returns a text: A text: “Stationary, no trend”,
“Stationary around a linear trend”, “Random walk with drift”, “Unit root
and linear trend”.

*Example*

``` r
library(quantmod)
```

    ## Loading required package: xts

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Loading required package: TTR

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
library(urca)
getSymbols("AAPL", src="yahoo")
```

    ## [1] "AAPL"

``` r
unitRootDF_ABsequential(AAPL$AAPL.Close)
```

    ## [1] "Unit root and linear trend"

``` r
unitRootDF_ABsequential(AAPL$AAPL.Close, 0.01)
```

    ## [1] "Random walk with drift"

**stationarize()** - Takes a time series of type “Stationary around a
linear trend”, “Random walk with drift”, or “Unit root and linear trend”
and returns a time series that is “Stationary, no trend”.

*Example*

``` r
library(quantmod)
library(urca)
getSymbols("AAPL", src="yahoo")
```

    ## [1] "AAPL"

``` r
type=unitRootDF_ABsequential(AAPL$AAPL.Close)
aapl_stationary=stationarize(ts(AAPL$AAPL.Close), type=type)
```
