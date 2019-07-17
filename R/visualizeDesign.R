#' Visualize design matrix
#'
#' @param sampleData A \code{data.frame} with sample information.
#' @param designFormula A \code{formula}. All components of the terms must be
#'   present as columns in \code{sampleData}.
#' @param flipCoord A \code{logical}, whether to flip the coordinate axes in the
#'   plot.
#' @param textSize A \code{numeric} scalar giving the text size in the plot.
#' @param textSizeLabs A \code{numeric} scalar giving the text size for the axis
#'   labels in the plot.
#' @param lineWidth A \code{numeric} scalar giving the maximal length of a row
#'   in the plot, before it is split and printed on multiple lines
#' @param dropCols A character vector with columns to drop from the design
#'   matrix, or NULL if no columns should be dropped.
#'
#' @author Charlotte Soneson
#'
#' @export
#'
#' @return A list with two elements:
#' \itemize{
#' \item sampledata A \code{data.frame}, expanded from the input
#' \code{sampleData}
#' \item plotlist A list of plots, displaying the fitted values for each
#' combination of predictor values, in terms of the model coefficients.
#' }
#'
#' @examples
#' VisualizeDesign(
#'   sampleData = data.frame(genotype = rep(c("A", "B"), each = 4),
#'                           treatment = rep(c("treated", "untreated"), 4)),
#'   designFormula = ~genotype + treatment
#' )
#'
#' @importFrom dplyr select distinct mutate mutate_all
#' @importFrom tidyr unite
#' @importFrom ggplot2 ggplot ggtitle annotate geom_vline theme geom_hline
#'   theme_bw geom_text aes_string element_blank coord_flip
#' @importFrom stats model.matrix as.formula
#' @importFrom methods is
#'
VisualizeDesign <- function(sampleData, designFormula,
                            flipCoord = FALSE, textSize = 5,
                            textSizeLabs = 12, lineWidth = 25,
                            dropCols = NULL) {
  ## TODO: Allow design of ~1 (currently fails, needs at least 1 term)

  ## ----------------------------------------------------------------------- ##
  ## Check input arguments
  ## ----------------------------------------------------------------------- ##
  if (!methods::is(sampleData, "data.frame")) {
    stop("'sampleData' must be a data.frame")
  }

  if (!methods::is(designFormula, "formula") &&
      !methods::is(designFormula, "character")) {
    stop("'designFormula' must be a formula or a character string")
  }
  if (methods::is(designFormula, "character") &&
      substr(designFormula, 1, 1) != "~") {
    stop("If 'designFormula' is a character string, it must start with ~")
  }
  if (any(grepl("\\|", designFormula))) {
    stop("'designFormula' can't contain |")
  }

  if (!methods::is(flipCoord, "logical") | length(flipCoord) != 1) {
    stop("'flipCoord' must be a logical scalar")
  }

  if ((!is.numeric(textSize) | length(textSize) != 1) ||
      (!is.numeric(textSizeLabs) | length(textSizeLabs) != 1) ||
      (!is.numeric(lineWidth) | length(lineWidth) != 1)) {
    stop("'textSize', 'textSizeLabs' and 'lineWidth' must be numeric scalars")
  }

  if (length(dropCols) > 0 && !methods::is(dropCols, "character")) {
    stop("'dropCols' must be NULL or a character vector")
  }

  ## ----------------------------------------------------------------------- ##
  ## Extract terms from the design formula
  ## ----------------------------------------------------------------------- ##
  designFormula <- stats::as.formula(designFormula)
  terms <- strsplit(gsub(" ", "", as.character(designFormula)[2]),
                    "\\~|\\+|\\:|\\*|\\^|\\-|/")[[1]]
  terms <- setdiff(terms, c("0", "1", ""))
  terms <- unique(terms)
  if (!all(terms %in% colnames(sampleData))) {
    stop("Not all terms in the design matrix can be generated from ",
         "the column names of the sample data")
  }
  sampleData <- sampleData %>% dplyr::select(terms)

  ## ----------------------------------------------------------------------- ##
  ## Create design matrix
  ## ----------------------------------------------------------------------- ##
  mm <- stats::model.matrix(designFormula, data = sampleData)
  if (!all(dropCols %in% colnames(mm))) {
    warning("Not all values in 'dropCols' are present in the design matrix. ",
            "Missing: ", paste(dropCols[!(dropCols %in% colnames(mm))],
                               collapse = ", "))
  }
  mm <- mm[, !(colnames(mm) %in% dropCols), drop = FALSE]

  ## ----------------------------------------------------------------------- ##
  ## Add modeled value column to sample data
  ## ----------------------------------------------------------------------- ##
  sampleData$value <- ""
  for (i in seq_len(nrow(sampleData))) {
    idxkeep <- which(mm[i, ] != 0)
    nmtmp <- colnames(mm)[idxkeep]
    mmtmp <- as.character(mm[i, idxkeep])
    idxneg <- grep("^-", mmtmp)
    mmtmp[idxneg] <- paste0("(", mmtmp[idxneg], ")")
    mmtmp[mmtmp != "1"] <- paste0(mmtmp[mmtmp != "1"], "*")
    mmtmp[mmtmp == "1"] <- ""
    v <- paste0(mmtmp, nmtmp)
    v <- paste(v, collapse = " + ")
    sampleData$value[i] <- v
  }
  sampleData <- sampleData %>% dplyr::distinct()

  ## ----------------------------------------------------------------------- ##
  ## Define terms to include in the plot, and terms used for splitting plots
  ## ----------------------------------------------------------------------- ##
  if (length(terms) <= 1) {
    plot_terms <- terms
  } else {
    plot_terms <- terms[(length(terms) - 1):length(terms)]
  }
  if (length(terms) > 2) {
    split_terms <- terms[seq_len(length(terms) - 2)]
  } else {
    split_terms <- c()
  }

  ## ----------------------------------------------------------------------- ##
  ## Add \n if the modeled value has too many characters
  ## ----------------------------------------------------------------------- ##
  plot_data <- sampleData %>%
    dplyr::mutate(value = vapply(value, function(i)
      addNewLine(i, lineWidth), ""))

  ## ----------------------------------------------------------------------- ##
  ## Convert all columns to factors for plotting
  ## ----------------------------------------------------------------------- ##
  plot_data <- plot_data %>%
    dplyr::mutate_all(as.factor)

  ## ----------------------------------------------------------------------- ##
  ## Add value of split terms (to use for plot titles)
  ## ----------------------------------------------------------------------- ##
  if (length(split_terms) > 0) {
    for (st in split_terms) {
      plot_data[[st]] <- paste0(st, " = ", plot_data[[st]])
    }
    plot_data <- plot_data %>%
      tidyr::unite("groupby", split_terms, sep = ", ")
  } else {
    plot_data$groupby <- ""
  }

  ## ----------------------------------------------------------------------- ##
  ## Create plot(s)
  ## ----------------------------------------------------------------------- ##
  ggp <- lapply(split(
    plot_data, f = plot_data$groupby),
    function(w) {
      gg <- ggplot2::ggplot(w,
                   ggplot2::aes_string(
                     x = ifelse(length(plot_terms) == 1, 1, plot_terms[2]),
                     y = plot_terms[1],
                     label = "value")) +
        ggplot2::geom_text(size = 0) +
        ggplot2::theme_bw() +
        ggplot2::geom_hline(yintercept = 0.5 +
                              seq_len(length(unique(
                                sampleData[, plot_terms[1]])) - 1)) +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       axis.text = ggplot2::element_text(size = textSizeLabs),
                       axis.title = ggplot2::element_text(size = textSizeLabs))
      if (length(plot_terms) > 1) {
        gg <- gg +
          ggplot2::geom_vline(xintercept = 0.5 +
                                seq_len(length(unique(
                                  sampleData[, plot_terms[2]])) - 1))
      }
      for (i in seq_len(nrow(w))) {
        gg <- gg +
          ggplot2::annotate(
            "text",
            x = ifelse(length(plot_terms) == 1, 1, w[i, plot_terms[2]]),
            y = w[i, plot_terms[1]],
            label = w[i, "value"],
            size = textSize, parse = FALSE)
      }
      gg <- gg + ggplot2::ggtitle(w$groupby[1])
      if (flipCoord) {
        gg <- gg + ggplot2::coord_flip()
      }
      gg
    })

  ## ----------------------------------------------------------------------- ##
  ## Return
  ## ----------------------------------------------------------------------- ##
  list(sampledata = sampleData, plotlist = ggp)
}

## Add \n if a string is longer than lineWidth
addNewLine <- function(st, lineWidth) {
  if (nchar(st) > lineWidth) {
    st0 <- strsplit(st, "\\+")[[1]]
    cs <- cumsum(vapply(st0, nchar, 0))
    csgr <- cs %/% lineWidth
    st1 <- vapply(split(st0, csgr),
                  function(x) paste(x, collapse = "+"), "")
    st <- paste(st1, collapse = "+\n")
  }
  st
}

