#' Visualize design matrix
#'
#' Given a sample table and a design formula, generate a collection of
#' static plots for exploring the resulting design matrix graphically.
#' This function is called internally by \code{ExploreModelMatrix()}, but
#' can also be used directly if interactivity is not required.
#'
#' @param sampleData A \code{data.frame} of \code{DataFrame} with sample
#'   information.
#' @param designFormula A \code{formula}. All components of the terms must be
#'   present as columns in \code{sampleData}.
#' @param flipCoordFitted,flipCoordCoocc A \code{logical}, whether to flip the
#'   coordinate axes in the fitted values/co-occurrence plot, respectively.
#' @param textSizeFitted,textSizeCoocc A \code{numeric} scalar giving the text
#'   size in the fitted values/co-occurrence plot, respectively.
#' @param textSizeLabsFitted,textSizeLabsCoocc A \code{numeric} scalar giving
#'   the text size for the axis labels in the fitted values/co-occurrence plot,
#'   respectively.
#' @param lineWidthFitted A \code{numeric} scalar giving the maximal length of a
#'   row in the fitted values plot, before it is split and printed on multiple
#'   lines
#' @param addColorFitted A \code{logical} scalar indicating whether the terms
#'   in the fitted values plot should be shown in different colors.
#' @param colorPaletteFitted A \code{function} returning a color palette to use
#'   for coloring the model coefficients in the fitted values plot.
#' @param dropCols A character vector with columns to drop from the design
#'   matrix, or NULL if no columns should be dropped.
#' @param designMatrix A \code{numeric} matrix, which can be supplied as an
#'   alternative to \code{designFormula}. Rows must be in the same order as
#'   the rows in \code{sampleData}.
#'
#' @author Charlotte Soneson
#'
#' @export
#'
#' @return A list with the following elements:
#' \itemize{
#' \item \code{sampledata}: A \code{data.frame}, expanded from the input
#' \code{sampleData}
#' \item \code{plotlist}: A list of plots, displaying the fitted values for
#' each combination of predictor values, in terms of the model coefficients.
#' \item \code{designmatrix}: The design matrix, after removing any columns in
#' \code{dropCols}
#' \item \code{pseudoinverse}: The pseudoinverse of the design matrix
#' \item \code{vifs}: A \code{data.frame} with calculated variance inflation
#' factors
#' \item \code{colors}: A vector with colors to use for different model
#' coefficients
#' \item \code{cooccurrenceplots}: A list of plots, displaying the
#' co-occurrence pattern for the predictors (i.e., the number of observations
#' for each combination of predictor values)
#' \item \code{totnbrrows}: The total number of "rows" in the list of plots
#' of fiitted values. Useful for deciding the required size of the plot canvas.
#' }
#'
#' @examples
#' VisualizeDesign(
#'   sampleData = data.frame(genotype = rep(c("A", "B"), each = 4),
#'                           treatment = rep(c("treated", "untreated"), 4)),
#'   designFormula = ~genotype + treatment
#' )
#'
#' @importFrom dplyr select distinct mutate mutate_all n group_by_at all_of
#' @importFrom tidyr unite separate_rows
#' @importFrom ggplot2 ggplot ggtitle annotate geom_vline theme geom_hline
#'   theme_bw geom_text aes_string element_blank coord_flip aes element_text
#'   scale_color_manual scale_x_discrete scale_y_discrete expansion
#' @importFrom stats model.matrix as.formula cor var
#' @importFrom methods is as
#' @importFrom MASS ginv
#' @importFrom magrittr %>%
#' @importFrom S4Vectors DataFrame
#'
VisualizeDesign <- function(sampleData, designFormula = NULL,
                            flipCoordFitted = FALSE, flipCoordCoocc = FALSE,
                            textSizeFitted = 5, textSizeCoocc = 5,
                            textSizeLabsFitted = 12, textSizeLabsCoocc = 12,
                            lineWidthFitted = 25, addColorFitted = TRUE,
                            colorPaletteFitted = scales::hue_pal(),
                            dropCols = NULL, designMatrix = NULL) {
  ## TODO: Allow design of ~1 (currently fails, needs at least 1 term)

  ## ----------------------------------------------------------------------- ##
  ## Check input arguments
  ## ----------------------------------------------------------------------- ##
  if (!(methods::is(sampleData, "data.frame") ||
        methods::is(sampleData, "DataFrame"))) {
    stop("'sampleData' must be a data.frame or a DataFrame")
  }

  if (methods::is(sampleData, "DataFrame")) {
    sampleData <- methods::as(sampleData, "data.frame")
  }

  if (!is.null(designFormula)) {
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
    if (!is.null(designMatrix)) {
      stop("Only one of 'designFormula' and 'designMatrix' should be provided")
    }
  }

  if (is.null(designFormula) && is.null(designMatrix)) {
    stop("Either 'designFormula' or 'designMatrix' must be provided")
  }
  if (!is.null(designMatrix)) {
    if (nrow(sampleData) != nrow(designMatrix)) {
      stop("'sampleData' and 'designMatrix' must have the same number of rows")
    }
    if ((!is.null(rownames(designMatrix)) || !is.null(rownames(sampleData))) &&
        !all(rownames(designMatrix) == rownames(sampleData))) {
      stop("'sampleData' and 'designMatrix' must have the same row names")
    }
    if (mode(designMatrix) != "numeric") {
      stop("'designMatrix' must be a numeric matrix")
    }
  }

  if (!methods::is(flipCoordFitted, "logical") | length(flipCoordFitted) != 1) {
    stop("'flipCoordFitted' must be a logical scalar")
  }
  if (!methods::is(flipCoordCoocc, "logical") | length(flipCoordCoocc) != 1) {
    stop("'flipCoordCoocc' must be a logical scalar")
  }

  if ((!is.numeric(textSizeFitted) | length(textSizeFitted) != 1) ||
      (!is.numeric(textSizeLabsFitted) | length(textSizeLabsFitted) != 1) ||
      (!is.numeric(lineWidthFitted) | length(lineWidthFitted) != 1) ||
      (!is.numeric(textSizeCoocc) | length(textSizeCoocc) != 1) ||
      (!is.numeric(textSizeLabsCoocc) | length(textSizeLabsCoocc) != 1)) {
    stop("'textSizeFitted', 'textSizeCoocc', 'textSizeLabs',",
         "'textSizeLabsCoocc' and 'lineWidthFitted' must be numeric scalars")
  }
  if (addColorFitted) {
    lineWidthFitted <- 1
  }

  if (length(dropCols) > 0 && !methods::is(dropCols, "character")) {
    stop("'dropCols' must be NULL or a character vector")
  }

  ## ----------------------------------------------------------------------- ##
  ## Extract terms from the design formula
  ## ----------------------------------------------------------------------- ##
  if (!is.null(designFormula)) {
    designFormula <- stats::as.formula(designFormula)
    terms <- all.vars(designFormula)
    if (!all(terms %in% colnames(sampleData))) {
      stop("Not all terms in the design formula can be generated from ",
           "the column names of the sample data")
    }
    sampleData <- sampleData %>% dplyr::select(dplyr::all_of(terms))
    if (any(is.na(sampleData))) {
      stop("The sample data frame should not contain NA values.")
    }
  } else {
    ## If we're only given a design matrix, assume that all columns of
    ## sampleData are relevant
    terms <- colnames(sampleData)
  }

  ## ----------------------------------------------------------------------- ##
  ## Create design matrix
  ## ----------------------------------------------------------------------- ##
  if (!is.null(designFormula)) {
    mm <- stats::model.matrix(designFormula, data = sampleData)
    if (!all(dropCols %in% colnames(mm))) {
      warning("Not all values in 'dropCols' are present in the design matrix. ",
              "Missing: ", paste(dropCols[!(dropCols %in% colnames(mm))],
                                 collapse = ", "))
    }
    mm <- mm[, !(colnames(mm) %in% dropCols), drop = FALSE]
  } else {
    mm <- designMatrix
  }

  ## ----------------------------------------------------------------------- ##
  ## Calculate pseudoinverse of design matrix
  ## ----------------------------------------------------------------------- ##
  psinverse <- MASS::ginv(mm)
  rownames(psinverse) <- colnames(mm)
  colnames(psinverse) <- rownames(mm)

  ## ----------------------------------------------------------------------- ##
  ## Calculate variance inflation factors
  ## ----------------------------------------------------------------------- ##
  vifs <- .CalculateVIFsLM(mm)

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
  sampleData <- sampleData %>% dplyr::group_by(value) %>%
    dplyr::mutate(nSamples = length(value)) %>% dplyr::ungroup() %>%
    dplyr::distinct() %>% as.data.frame()

  ## ----------------------------------------------------------------------- ##
  ## Define terms to include in the plot, and terms used for splitting plots
  ## ----------------------------------------------------------------------- ##
  if (length(terms) <= 1) {
    plot_terms <- terms
  } else {
    plot_terms <- terms[seq(length(terms) - 1, length(terms))]
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
      .AddNewLine(i, lineWidthFitted), ""))

  ## ----------------------------------------------------------------------- ##
  ## Convert all columns to factors for plotting
  ## ----------------------------------------------------------------------- ##
  plot_data <- plot_data %>%
    dplyr::mutate_at(dplyr::vars(-nSamples, -value), as.factor)

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
  ## Split terms into individual rows
  ## ----------------------------------------------------------------------- ##
  plot_data <- plot_data %>%
    tidyr::separate_rows(value, sep = "\\\n") %>%
    dplyr::mutate(value = gsub("^ ", "", value)) %>%
    dplyr::group_by_at(c(plot_terms, "groupby")) %>%
    dplyr::mutate(vjust = 1.5 * (seq(0, dplyr::n() - 1, by = 1) -
                                   (dplyr::n() - 1)/2) + 0.5) %>%
    dplyr::ungroup()

  ## ----------------------------------------------------------------------- ##
  ## Pre-define colors
  ## ----------------------------------------------------------------------- ##
  if (addColorFitted) {
    plot_data <- plot_data %>%
      dplyr::mutate(colorby = gsub("[ ]*\\+[ ]*", "",
                                   gsub("(\\(-)*[0-9]*\\)*[ ]*\\*[ ]*", "",
                                        value)))
    colors <- structure(colorPaletteFitted(length(unique(plot_data$colorby))),
                        names = unique(plot_data$colorby))
  }

  ## ----------------------------------------------------------------------- ##
  ## Create plot(s)
  ## ----------------------------------------------------------------------- ##
  ## First, get the total number of "rows" in the final plot.
  ## Will be used to determine the size of the panel.
  if (flipCoordFitted & length(plot_terms) == 1) {
    totnbrrows <- length(unique(plot_data$groupby))
  } else if (flipCoordFitted) {
    totnbrrows <- length(unique(plot_data$groupby)) *
      length(unique(plot_data[[plot_terms[2]]]))
  } else {
    totnbrrows <- length(unique(plot_data$groupby)) *
      length(unique(plot_data[[plot_terms[1]]]))
  }

  ggp <- lapply(split(
    plot_data, f = plot_data$groupby),
    function(w) {
      gg <- ggplot2::ggplot(
        w,
        ggplot2::aes_string(
          x = ifelse(length(plot_terms) == 1, 1, plot_terms[2]),
          y = plot_terms[1],
          label = "value")) +
        ggplot2::scale_x_discrete(
          expand = ggplot2::expansion(mult = 0, add = 0.5)
        ) +
        ggplot2::scale_y_discrete(
          expand = ggplot2::expansion(mult = 0, add = 0.5)
        )
      if (addColorFitted) {
        gg <- gg +
          ggplot2::geom_text(size = textSizeFitted,
                             ggplot2::aes(vjust = vjust,
                                          color = colorby)) +
          ggplot2::scale_color_manual(values = colors)
      } else {
        gg <- gg +
          ggplot2::geom_text(size = textSizeFitted,
                             ggplot2::aes(vjust = vjust))
      }
      gg <- gg +
        ggplot2::theme_bw() +
        ggplot2::geom_hline(yintercept = 0.5 +
                              seq_len(length(unique(
                                sampleData[[plot_terms[1]]])) - 1)) +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(size = textSizeLabsFitted),
          axis.title = ggplot2::element_text(size = textSizeLabsFitted),
          legend.position = "none"
        )
      if (length(plot_terms) > 1) {
        gg <- gg +
          ggplot2::geom_vline(xintercept = 0.5 +
                                seq_len(length(unique(
                                  sampleData[[plot_terms[2]]])) - 1))
      } else {
        gg <- gg + theme(axis.text.x = element_blank(),
                         axis.title.x = element_blank(),
                         axis.ticks.x = element_blank())
      }
      gg <- gg + ggplot2::ggtitle(w$groupby[1])
      if (flipCoordFitted) {
        gg <- gg + ggplot2::coord_flip()
      }
      gg
    })

  ## ----------------------------------------------------------------------- ##
  ## Create co-occurrence plot
  ## ----------------------------------------------------------------------- ##
  keepcols <- setdiff(colnames(plot_data), c("value", "vjust", "colorby"))
  maxN <- max(plot_data$nSamples)
  ggcoocc <- lapply(split(
    plot_data, f = plot_data$groupby),
    function(w) {
      w <- w %>% dplyr::select(dplyr::all_of(keepcols)) %>% dplyr::distinct()
      gp <- ggplot2::ggplot(
        w,
        ggplot2::aes_string(
          x = ifelse(length(plot_terms) == 1, 1, plot_terms[2]),
          y = plot_terms[1],
          fill = "nSamples",
          label = "nSamples"
        )) +
        ggplot2::geom_tile(color = "black") +
        ggplot2::scale_x_discrete(
          expand = ggplot2::expansion(mult = 0, add = 0)
        ) +
        ggplot2::scale_y_discrete(
          expand = ggplot2::expansion(mult = 0, add = 0)
        ) +
        ggplot2::theme_bw() +
        ggplot2::geom_text(size = textSizeCoocc) +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(size = textSizeLabsCoocc),
          axis.title = ggplot2::element_text(size = textSizeLabsCoocc)
        ) +
        ggplot2::scale_fill_gradient(
          low = "white", high = "deepskyblue3",
          name = "Number of\nobservations",
          limits = c(0, maxN)) +
        ggplot2::ggtitle(w$groupby[1])
      if (length(plot_terms) == 1) {
        gp <- gp + theme(axis.text.x = element_blank(),
                         axis.title.x = element_blank(),
                         axis.ticks.x = element_blank())
      }
      if (flipCoordCoocc) {
        gp <- gp + ggplot2::coord_flip()
      }
      gp
    })

  ## ----------------------------------------------------------------------- ##
  ## Return
  ## ----------------------------------------------------------------------- ##
  list(sampledata = sampleData, plotlist = ggp, designmatrix = mm,
       pseudoinverse = psinverse, vifs = vifs, colors = colors,
       cooccurrenceplots = ggcoocc, totnbrrows = totnbrrows)
}

#' Split a string into multiple lines if it's longer than a certain length
#'
#' @param st A string
#' @param lineWidth The maximum length of a line
#'
#' @return A string
#'
#' @keywords internal
#'
#' @rdname INTERNAL_.AddNewLine
#'
.AddNewLine <- function(st, lineWidth) {
  if (nchar(st) > lineWidth) {
    st0 <- strsplit(st, "\\+")[[1]]
    nchs <- vapply(st0, nchar, 0)
    cs <- cumsum(nchs)
    grp <- rep(0, length(st0))
    curtot <- 0
    curgrp <- 0
    for (i in seq_along(st0)) {
      if (curtot + nchs[i] > lineWidth) {
        curgrp <- curgrp + 1
        curtot <- nchs[i]
      } else {
        curtot <- curtot + nchs[i]
      }
      grp[i] <- curgrp
    }
    st1 <- vapply(split(st0, grp),
                  function(x) paste(x, collapse = "+"), "")
    st <- paste(st1, collapse = "+\n")
  }
  st
}

