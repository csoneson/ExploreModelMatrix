#' Calculate variance inflation factors from a design matrix
#'
#' The calculation of VIFs is done by fitting a linear model for each column in
#' the design matrix, with all the other columns (except the intercept) as
#' predictors. If there is an intercept (a column named "(Intercept)") in the
#' original design matrix, each linear model will be fit with an intercept. If
#' there is no such column in the original design matrix, the linear models will
#' be fit without an intercept. After fitting the linear model for column i, the
#' corresponding VIF is calculated as 1/(1-R^2), where R^2 is the coefficient of
#' determination of the model. "Inf" results (obtained when R^2=1) are replaced
#' by NAs.
#'
#' @param mm A model.matrix object
#'
#' @return A data.frame with estimated VIFs for each coefficient, or NULL if the
#'   calculations could not be performed (there are no non-intercept columns in
#'   the design matrix, or the linear model fitting fails).
#'
#' @author Charlotte Soneson
#'
#' @keywords internal
#'
#' @importFrom stats lm
#'
calculateVIFsLM <- function(mm) {
  ## Remove intercept from mm, if present
  if ("(Intercept)" %in% colnames(mm)) {
    mm0 <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
  } else {
    mm0 <- mm
  }

  ## Get all columns of the design matrix
  cols <- seq_len(ncol(mm0))
  names(cols) <- colnames(mm0)

  if (length(cols) == 0) {## no non-intercept columns in mm
    vifs <- NULL
  } else if (length(cols) == 1) {## a single non-intercept column, so VIF=1
    vifs <- data.frame(coefficient = names(cols),
                       vif = 1,
                       stringsAsFactors = FALSE)
  } else {
    vifs <- tryCatch({
      if (ncol(mm) == ncol(mm0)) {## no intercept
        suppressWarnings({
          data.frame(vif = vapply(cols, function(i) {
            1/(1 - summary(
              stats::lm(mm0[, i] ~ 0 + ., data = data.frame(mm0[, -i, drop = FALSE]))
            )$r.squared)
          }, NA_real_)) %>% tibble::rownames_to_column("coefficient")
        })
      } else {## with intercept
        suppressWarnings({
          data.frame(vif = vapply(cols, function(i) {
            1/(1 - summary(
              stats::lm(mm0[, i] ~ ., data = data.frame(mm0[, -i, drop = FALSE]))
            )$r.squared)
          }, NA_real_)) %>% tibble::rownames_to_column("coefficient")
        })
      }
    }, error = function(e) {## linear model fit fails
      NULL
    })
  }

  if (!is.null(vifs)) {
    vifs$vif[!is.finite(vifs$vif)] <- NA_real_
  }

  vifs
}

#' Calculate variance inflation factors from the overall lm fit
#'
#' A generalization of the implementation in rms::vif(), allowing for specified
#' columns to be removed from the variance/covariance matrix.
#'
#' @param designFormula A formula
#' @param sampleData A data.frame with sample information
#' @param removeCols A character vector of columns to remove from the vcov matrix
#'
#' @return A data.frame with estimated VIFs for each coefficient.
#'
#' @author Charlotte Soneson
#'
#' @keywords internal
#'
#' @importFrom stats rnorm lm vcov
#'
calculateVIFsVCOV <- function(designFormula, sampleData, removeCols = "") {
  y <- stats::rnorm(nrow(sampleData))
  fit <- stats::lm(as.formula(paste("y", paste(designFormula, collapse = " "))), sampleData)
  v <- stats::vcov(fit)
  nam <- dimnames(v)[[1]]
  keep <- setdiff(nam, c("(Intercept)", removeCols))
  v <- v[keep, keep, drop = FALSE]
  nam <- keep
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  data.frame(vif = v) %>% tibble::rownames_to_column("coefficient")
}
