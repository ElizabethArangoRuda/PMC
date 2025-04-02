
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PMC

<!-- badges: start -->
<!-- badges: end -->

The goal of PMC is to …

## Installation

You can install the development version of PMC from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ElizabethArangoRuda/PMC")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PMC)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
data("Mildred_Lake_Data")
summary(Mildred_Lake_Data)
#>       Date                 Tmin              Tmax              PPT         
#>  Min.   :2014-01-01   Min.   :-39.000   Min.   :-27.000   Min.   : 0.0000  
#>  1st Qu.:2014-07-02   1st Qu.:-14.150   1st Qu.: -4.800   1st Qu.: 0.0000  
#>  Median :2014-12-31   Median : -0.350   Median :  8.600   Median : 0.1000  
#>  Mean   :2014-12-31   Mean   : -3.141   Mean   :  7.406   Mean   : 0.9121  
#>  3rd Qu.:2015-07-01   3rd Qu.:  8.300   3rd Qu.: 20.600   3rd Qu.: 0.6000  
#>  Max.   :2015-12-31   Max.   : 18.800   Max.   : 34.000   Max.   :36.9000  
#>       ISI        
#>  Min.   : 0.000  
#>  1st Qu.: 0.800  
#>  Median : 1.800  
#>  Mean   : 3.569  
#>  3rd Qu.: 4.900  
#>  Max.   :24.700
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
