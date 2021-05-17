
<!-- README.md is generated from README.Rmd. Please edit that file -->

# biostats.apps

<!-- badges: start -->
<!-- badges: end -->

biostats.apps has some shiny apps that can be used for teaching
statistics to biologists.

## Installation

You can install the development version of biostats.apps from
[GitHub](https://github.com/biostats-r/biostats.apps) with:

``` r
# install.packages("remotes")
remotes::install_github("biostats-r/biostats.apps")
```

## Running biostats.apps

To run biostats.apps, load the package then run one of the apps.

``` r
library(biostats.apps)

power_lm_app()
```

## Available apps

-   `publication_bias_app()` How much could pubication bias, the
    tendency not to publish non-significant results, bias the
    literature?
-   `power_lm_app()` Find out how much statistical power a linear model
    has for an experiment has given the effect size, standard deviation
    of residuals and number of observations.
-   `influence_leverage_app()` Explore how the position of an point
    changes diagnostic plots.
