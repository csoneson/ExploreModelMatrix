# ExploreModelMatrix

`ExploreModelMatrix` is a small R package that lets the user interactively
explore a design matrix. In particular, given a table with sample information
and a design formula, `ExploreModelMatrix` will illustrate the fitted values for
each combination of input variables, simplifying understanding and generation of
contrasts.

`ExploreModelMatrix` is still under development, please open an issue if you
encounter unexpected behaviour.

![](inst/www/ExploreModelMatrix.png)

## Installation

You can install `ExploreModelMatrix` with the `remotes` (or `devtools`) package,
like so:

```
remotes::install_github("csoneson/ExploreModelMatrix")
```

For this to work, you need to have the `remotes` R package installed. If you
don't already have it, you can install it like this:

```
install.packages("remotes")
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

This will open up a shiny application where you can explore the specified design
matrix and the fitted values for each combination of predictor values.

For more examples of designs, please see the vignette. 
