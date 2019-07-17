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
                               flipCoord = FALSE, textSize = 5,
                               textSizeLabs = 12, lineWidth = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = as.matrix(sampleData),
                               designFormula = designFormula,
                               flipCoord = FALSE, textSize = 5,
                               textSizeLabs = 12, lineWidth = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = as.list(sampleData),
                               designFormula = designFormula,
                               flipCoord = FALSE, textSize = 5,
                               textSizeLabs = 12, lineWidth = 25,
                               dropCols = NULL))

  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = genotype,
                               flipCoord = FALSE, textSize = 5,
                               textSizeLabs = 12, lineWidth = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = "genotype",
                               flipCoord = FALSE, textSize = 5,
                               textSizeLabs = 12, lineWidth = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = "~genotyp",
                               flipCoord = FALSE, textSize = 5,
                               textSizeLabs = 12, lineWidth = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotyp,
                               flipCoord = FALSE, textSize = 5,
                               textSizeLabs = 12, lineWidth = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype|treatment,
                               flipCoord = FALSE, textSize = 5,
                               textSizeLabs = 12, lineWidth = 25,
                               dropCols = NULL))

  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoord = 1, textSize = 5,
                               textSizeLabs = 12, lineWidth = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoord = "TRUE", textSize = 5,
                               textSizeLabs = 12, lineWidth = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoord = c(TRUE, FALSE), textSize = 5,
                               textSizeLabs = 12, lineWidth = 25,
                               dropCols = NULL))

  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoord = FALSE, textSize = c(1, 2),
                               textSizeLabs = 12, lineWidth = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoord = FALSE, textSize = "1",
                               textSizeLabs = 12, lineWidth = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoord = FALSE, textSize = TRUE,
                               textSizeLabs = 12, lineWidth = 25,
                               dropCols = NULL))

  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoord = FALSE, textSize = 5,
                               textSizeLabs = c(1, 2), lineWidth = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoord = FALSE, textSize = 5,
                               textSizeLabs = "1", lineWidth = 25,
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoord = FALSE, textSize = 5,
                               textSizeLabs = TRUE, lineWidth = 25,
                               dropCols = NULL))

  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoord = FALSE, textSize = 5,
                               textSizeLabs = 12, lineWidth = c(1, 2),
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoord = FALSE, textSize = 5,
                               textSizeLabs = 12, lineWidth = "5",
                               dropCols = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoord = FALSE, textSize = 5,
                               textSizeLabs = 12, lineWidth = TRUE,
                               dropCols = NULL))

  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoord = FALSE, textSize = 5,
                               textSizeLabs = 12, lineWidth = 25,
                               dropCols = 1))
  expect_warning(VisualizeDesign(sampleData = sampleData,
                                 designFormula = ~genotype,
                                 flipCoord = FALSE, textSize = 5,
                                 textSizeLabs = 12, lineWidth = 25,
                                 dropCols = "nonexistent"))
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
  expect_equal(colnames(res), c("genotype", "value"))

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
