context("VisualizeDesign")

test_that("VisualizeDesign fails with incorrect inputs", {
  sampleData <- data.frame(
    genotype = rep(c("A", "B"), each = 4),
    treatment = rep(c("trt", "ctrl"), 4),
    stringsAsFactors = FALSE
  )
  designFormula <- ~genotype

  expect_error(VisualizeDesign(sampleData = 0,
                               designFormula = designFormula,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = as.matrix(sampleData),
                               designFormula = designFormula,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = as.list(sampleData),
                               designFormula = designFormula,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))

  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = TRUE,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = genotype,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = "genotype",
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = "~genotyp",
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotyp,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype|treatment,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))

  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = 1, textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = "TRUE", textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = c(TRUE, FALSE), textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))

  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordCoocc = 1, textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordCoocc = "TRUE", textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordCoocc = c(TRUE, FALSE), textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))

  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = FALSE, textSizeFitted = c(1, 2),
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = FALSE, textSizeFitted = "1",
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = FALSE, textSizeFitted = TRUE,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))

  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = FALSE, textSizeCoocc = c(1, 2),
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = FALSE, textSizeCoocc = "1",
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = FALSE, textSizeCoocc = TRUE,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL))

  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = c(1, 2), lineWidthFitted = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = "1", lineWidthFitted = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = TRUE, lineWidthFitted = 25,
                               dropCols = NULL))

  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsCoocc = c(1, 2), lineWidthFitted = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsCoocc = "1", lineWidthFitted = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsCoocc = TRUE, lineWidthFitted = 25,
                               dropCols = NULL))

  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = c(1, 2),
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = "5",
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = TRUE,
                               dropCols = NULL))

  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = 1))
  expect_warning(VisualizeDesign(sampleData = sampleData,
                                 designFormula = ~genotype,
                                 flipCoordFitted = FALSE, textSizeFitted = 5,
                                 textSizeLabsFitted = 12, lineWidthFitted = 25,
                                 dropCols = "nonexistent"))

  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL, colorPaletteFitted = "string"))
})

test_that("VisualizeDesign works", {
  sampleData <- data.frame(
    genotype = rep(c("A", "B"), each = 4),
    treatment = rep(c("trt", "ctrl"), 4),
    stringsAsFactors = FALSE
  )

  res <- VisualizeDesign(sampleData = sampleData,
                         designFormula = ~genotype)$sampledata

  expect_equal(res$value[res$genotype == "A"], "(Intercept)")
  expect_equal(res$value[res$genotype == "B"], "(Intercept) + genotypeB")
  expect_equal(nrow(res), 2L)
  expect_equal(colnames(res), c("genotype", "value", "nSamples"))

  res <- VisualizeDesign(sampleData = sampleData,
                         designFormula = ~genotype + treatment)$sampledata

  expect_equal(res$value[res$genotype == "A" & res$treatment == "trt"],
               "(Intercept) + treatmenttrt")
  expect_equal(res$value[res$genotype == "A" & res$treatment == "ctrl"],
               "(Intercept)")
  expect_equal(res$value[res$genotype == "B" & res$treatment == "trt"],
               "(Intercept) + genotypeB + treatmenttrt")
  expect_equal(res$value[res$genotype == "B" & res$treatment == "ctrl"],
               "(Intercept) + genotypeB")

  ## Check that dropCols = NULL and dropCols = c() give the same results
  res1 <- VisualizeDesign(sampleData = sampleData,
                          designFormula = ~genotype + treatment,
                          dropCols = NULL)
  res2 <- VisualizeDesign(sampleData = sampleData,
                          designFormula = ~genotype + treatment,
                          dropCols = c())
  expect_equal(res1, res2)
})
