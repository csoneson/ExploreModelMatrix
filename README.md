# ExploreModelMatrix
[![Travis CI build status](https://travis-ci.com/csoneson/ExploreModelMatrix.svg?branch=master)](https://travis-ci.com/csoneson/ExploreModelMatrix)
[![Codecov.io coverage status](https://codecov.io/github/csoneson/ExploreModelMatrix/coverage.svg?branch=master)](https://codecov.io/github/csoneson/ExploreModelMatrix)
[![R build status](https://github.com/csoneson/ExploreModelMatrix/workflows/R-CMD-check/badge.svg)](https://github.com/csoneson/ExploreModelMatrix/actions)

`ExploreModelMatrix` is a small R package that lets the user interactively
explore a design matrix. In particular, given a table with sample information
and a design formula, `ExploreModelMatrix` will illustrate the fitted values
(or, more generally, the value of the linear predictor) for each combination of
input variables, simplifying understanding and generation of contrasts. A 
number of other visualizations are also included in the interactive interface, 
particularly simplifying the interpretation of linear models. 

`ExploreModelMatrix` is still under development and we welcome feedback. Please
open an issue if you encounter unexpected behaviour.

![](https://github.com/csoneson/ExploreModelMatrix/blob/master/inst/www/ExploreModelMatrix.png?raw=true)

## Installation

You can install `ExploreModelMatrix` with the `remotes` (or `devtools`) package,
like so:

```
install.packages("remotes")
remotes::install_github("csoneson/ExploreModelMatrix", 
                        build_vignettes = TRUE, dependencies = TRUE)
```

## Usage

The main function in the `ExploreModelMatrix` package is called
`ExploreModelMatrix`. When calling `ExploreModelMatrix`, simply provide a
_data.frame_ with sample information and a design formula:

```
sampleData <- data.frame(genotype = rep(c("A", "B"), each = 4),
                         treatment = rep(c("ctrl", "trt"), 4))
ExploreModelMatrix(sampleData = sampleData,
                   designFormula = ~ genotype + treatment)
```

This will open up an [R/Shiny](https://shiny.rstudio.com/) application where you
can explore the specified design matrix and the fitted values for each
combination of predictor values.

For more examples of designs, we refer to the package vignette. 
