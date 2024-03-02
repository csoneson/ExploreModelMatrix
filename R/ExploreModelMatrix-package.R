#' ExploreModelMatrix
#'
#' ExploreModelMatrix is an R package for visualizing design matrices generated
#' by the \code{model.matrix()} R function.
#' Provided with a sample data table and a design formula, the
#' \code{ExploreModelMatrix()} function launches a shiny app where the user can
#' explore the fitted values (in terms of the model coefficients) for each
#' combination of predictor values.
#'
#' @keywords internal
#' @aliases ExploreModelMatrix-package NULL
"_PACKAGE"

globalVariables(c("colorby", "value", "vjust", "coefficient",
                  "Sample", "vif", "nSamples", "rows", "cols",
                  "correlation"))
