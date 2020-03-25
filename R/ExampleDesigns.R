#' Define example designs
#'
#' @param exampleID The name of the example design. One of "One factor, unpaired
#'   samples", "One factor, paired samples", "Two crossed factors", "Two
#'   crossed, one blocking factor", "Two crossed, one nested factor", "Two
#'   crossed, one nested factor, dummy coded"
#'
#' @author Charlotte Soneson
#'
#' @return A list with two elements: the sample data table and the design
#'   formula
#'
#' @keywords internal
#'
#' @rdname INTERNAL_.ExampleDesigns
#'
.ExampleDesigns <- function(exampleID) {
  if (exampleID == "One factor, unpaired samples") {
    sampledata <- data.frame(
      treatment = rep(c("control", "drug1", "drug2"), 3),
      subject = factor(seq_len(9))
    )
    design <- "~ treatment"
  } else if (exampleID == "One factor, paired samples") {
    sampledata <- data.frame(
      treatment = rep(c("control", "drug1", "drug2"), 3),
      subject = factor(rep(seq_len(3), each = 3))
    )
    design <- "~ subject + treatment"
  } else if (exampleID == "Two crossed factors") {
    sampledata <- data.frame(
      treatment = rep(c("placebo", "drug"), 3),
      time = rep(c("0h", "1h", "2h"), each = 2)
    )
    design <- "~ time * treatment"
  } else if (exampleID == "Two crossed, one blocking factor") {
    sampledata <- data.frame(
      treatment = rep(c("placebo", "drug"), 6),
      time = rep(rep(c("0h", "1h", "2h"), each = 2), 2),
      batch = rep(c("day1", "day2"), each = 6)
    )
    design <- "~ time * treatment + batch"
  } else if (exampleID == "Two crossed, one nested factor") {
    sampledata <- data.frame(
      diagnosis = rep(c("healthy", "disease1", "disease2"), 6),
      treatment = rep(rep(c("none", "hormone"), each = 3), 3),
      subject = paste0(rep(c("healthy", "disease1", "disease2"), 6),
                       rep(c("A", "B", "C"), each = 6))
    )
    design <- "~ diagnosis:treatment + subject"
  } else if (exampleID == "Two crossed, one nested factor, dummy coded") {
    sampledata <- data.frame(
      diagnosis = rep(c("healthy", "disease1", "disease2"), 6),
      treatment = rep(rep(c("none", "hormone"), each = 3), 3),
      dummy = rep(c("A", "B", "C"), each = 6),
      subject = paste0(rep(c("healthy", "disease1", "disease2"), 6),
                       rep(c("A", "B", "C"), each = 6))
    )
    design <- "~ diagnosis + diagnosis:dummy + diagnosis:treatment"
  } else {
    stop("Unidentified example ID")
  }

  list(sampledata = sampledata, design = design)
}
