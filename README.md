
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

-   `power_lm_app()` Find out how much statistical power a linear model
    has for an experiment has given the effect size, standard deviation
    of residuals and number of observations.

## Apps in development

To run the apps that are in development you will need to clone the repo
(or just copy the file from github). All the apps are in the R/ folder.

-   influence\_leverage\_app.R Explore how the position of an point
    changes diagnostic plots.
