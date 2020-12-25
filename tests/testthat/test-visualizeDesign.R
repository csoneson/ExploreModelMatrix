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

  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = ~genotype,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL,
                               designMatrix = model.matrix(~genotype,
                                                           data = sampleData)))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = NULL,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL,
                               designMatrix = NULL))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = NULL,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL,
                               designMatrix = data.frame(
                                 model.matrix(~genotype,
                                              data = sampleData))))
  expect_error(VisualizeDesign(sampleData = sampleData,
                               designFormula = NULL,
                               flipCoordFitted = FALSE, textSizeFitted = 5,
                               textSizeLabsFitted = 12, lineWidthFitted = 25,
                               dropCols = NULL,
                               designMatrix = model.matrix(
                                 ~genotype,
                                 data = sampleData)[1:5, ]))
})

test_that("VisualizeDesign works with intercept", {
  sampleData <- data.frame(
    genotype = rep(c("A", "B"), each = 4),
    treatment = rep(c("trt", "ctrl"), 4),
    stringsAsFactors = FALSE
  )

  res0 <- VisualizeDesign(sampleData = sampleData,
                          designFormula = ~genotype)
  res <- res0$sampledata

  expect_equal(res0$totnbrrows, 2L)
  expect_equal(nrow(res0$designmatrix), 8L)
  expect_equal(ncol(res0$designmatrix), 2L)
  expect_equivalent(res0$designmatrix[, "(Intercept)"], rep(1L, 8L))
  expect_equivalent(res0$designmatrix[, "genotypeB"], rep(c(0L, 1L), each = 4))
  expect_equal(res$value[res$genotype == "A"], "(Intercept)")
  expect_equal(res$value[res$genotype == "B"], "(Intercept) + genotypeB")
  expect_equal(nrow(res), 2L)
  expect_equal(colnames(res), c("genotype", "value", "nSamples"))

  res0 <- VisualizeDesign(sampleData = sampleData,
                          designFormula = ~genotype + treatment)
  res <- res0$sampledata

  expect_equal(res0$totnbrrows, 2L)
  expect_equal(nrow(res0$designmatrix), 8L)
  expect_equal(ncol(res0$designmatrix), 3L)
  expect_equivalent(res0$designmatrix[, "(Intercept)"], rep(1L, 8L))
  expect_equivalent(res0$designmatrix[, "genotypeB"], rep(c(0L, 1L), each = 4))
  expect_equivalent(res0$designmatrix[, "treatmenttrt"], rep(c(1L, 0L), 4))
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
  for (nm in names(res1)) {
    if (!(nm %in% c("plotlist", "cooccurrenceplots"))) {
      expect_equal(res1[[nm]], res2[[nm]])
    }
  }
})

test_that("VisualizeDesign works without intercept", {
  sampleData <- data.frame(
    genotype = rep(c("A", "B"), each = 4),
    treatment = rep(c("trt", "ctrl"), 4),
    stringsAsFactors = FALSE
  )

  res0 <- VisualizeDesign(sampleData = sampleData,
                          designFormula = ~0 + genotype)
  res <- res0$sampledata

  expect_equal(res0$totnbrrows, 2L)
  expect_equal(nrow(res0$designmatrix), 8L)
  expect_equal(ncol(res0$designmatrix), 2L)
  expect_equivalent(res0$designmatrix[, "genotypeA"], rep(c(1L, 0L), each = 4))
  expect_equivalent(res0$designmatrix[, "genotypeB"], rep(c(0L, 1L), each = 4))
  expect_equal(res$value[res$genotype == "A"], "genotypeA")
  expect_equal(res$value[res$genotype == "B"], "genotypeB")
  expect_equal(nrow(res), 2L)
  expect_equal(colnames(res), c("genotype", "value", "nSamples"))

  res0 <- VisualizeDesign(sampleData = sampleData,
                          designFormula = ~0 + genotype + treatment)
  res <- res0$sampledata

  expect_equal(res0$totnbrrows, 2L)
  expect_equal(nrow(res0$designmatrix), 8L)
  expect_equal(ncol(res0$designmatrix), 3L)
  expect_equivalent(res0$designmatrix[, "genotypeA"], rep(c(1L, 0L), each = 4))
  expect_equivalent(res0$designmatrix[, "genotypeB"], rep(c(0L, 1L), each = 4))
  expect_equivalent(res0$designmatrix[, "treatmenttrt"], rep(c(1L, 0L), 4))
  expect_equal(res$value[res$genotype == "A" & res$treatment == "trt"],
               "genotypeA + treatmenttrt")
  expect_equal(res$value[res$genotype == "A" & res$treatment == "ctrl"],
               "genotypeA")
  expect_equal(res$value[res$genotype == "B" & res$treatment == "trt"],
               "genotypeB + treatmenttrt")
  expect_equal(res$value[res$genotype == "B" & res$treatment == "ctrl"],
               "genotypeB")

  ## Check that dropCols = NULL and dropCols = c() give the same results
  res1 <- VisualizeDesign(sampleData = sampleData,
                          designFormula = ~0 + genotype + treatment,
                          dropCols = NULL)
  res2 <- VisualizeDesign(sampleData = sampleData,
                          designFormula = ~0 + genotype + treatment,
                          dropCols = c())
  for (nm in names(res1)) {
    if (!(nm %in% c("plotlist", "cooccurrenceplots"))) {
      expect_equal(res1[[nm]], res2[[nm]])
    }
  }
})

test_that("VisualizeDesign works with DataFrame input", {
  sampleData <- S4Vectors::DataFrame(
    genotype = rep(c("A", "B"), each = 4),
    treatment = rep(c("trt", "ctrl"), 4)
  )

  res <- VisualizeDesign(sampleData = sampleData,
                         designFormula = ~0 + genotype)$sampledata

  expect_equal(res$value[res$genotype == "A"], "genotypeA")
  expect_equal(res$value[res$genotype == "B"], "genotypeB")
  expect_equal(nrow(res), 2L)
  expect_equal(colnames(res), c("genotype", "value", "nSamples"))

  res <- VisualizeDesign(sampleData = sampleData,
                         designFormula = ~0 + genotype + treatment)$sampledata

  expect_equal(res$value[res$genotype == "A" & res$treatment == "trt"],
               "genotypeA + treatmenttrt")
  expect_equal(res$value[res$genotype == "A" & res$treatment == "ctrl"],
               "genotypeA")
  expect_equal(res$value[res$genotype == "B" & res$treatment == "trt"],
               "genotypeB + treatmenttrt")
  expect_equal(res$value[res$genotype == "B" & res$treatment == "ctrl"],
               "genotypeB")

  ## Check that dropCols = NULL and dropCols = c() give the same results
  res1 <- VisualizeDesign(sampleData = sampleData,
                          designFormula = ~0 + genotype + treatment,
                          dropCols = NULL)
  res2 <- VisualizeDesign(sampleData = sampleData,
                          designFormula = ~0 + genotype + treatment,
                          dropCols = c())
  for (nm in names(res1)) {
    if (!(nm %in% c("plotlist", "cooccurrenceplots"))) {
      expect_equal(res1[[nm]], res2[[nm]])
    }
  }

  ## Check that flipCoordFitted = TRUE/FALSE give same results except for the plot
  res1 <- VisualizeDesign(sampleData = sampleData,
                          designFormula = ~0 + genotype + treatment,
                          flipCoordFitted = TRUE)
  res2 <- VisualizeDesign(sampleData = sampleData,
                          designFormula = ~0 + genotype + treatment,
                          flipCoordFitted = FALSE)
  for (nm in names(res1)) {
    if (!(nm %in% c("plotlist", "cooccurrenceplots"))) {
      expect_equal(res1[[nm]], res2[[nm]])
    }
  }
})

test_that("VisualizeDesign works with design matrix input", {
  sampleData <- data.frame(
    genotype = rep(c("A", "B"), each = 4),
    treatment = rep(c("trt", "ctrl"), 4),
    stringsAsFactors = FALSE
  )

  res0 <- VisualizeDesign(sampleData = sampleData %>% dplyr::select(genotype),
                          designFormula = NULL,
                          designMatrix = model.matrix(
                            ~0 + genotype,
                            data = sampleData))
  res <- res0$sampledata

  expect_equal(res0$totnbrrows, 2L)
  expect_equal(nrow(res0$designmatrix), 8L)
  expect_equal(ncol(res0$designmatrix), 2L)
  expect_equivalent(res0$designmatrix[, "genotypeA"], rep(c(1L, 0L), each = 4))
  expect_equivalent(res0$designmatrix[, "genotypeB"], rep(c(0L, 1L), each = 4))
  expect_equal(res$value[res$genotype == "A"], "genotypeA")
  expect_equal(res$value[res$genotype == "B"], "genotypeB")
  expect_equal(nrow(res), 2L)
  expect_equal(colnames(res), c("genotype", "value", "nSamples"))

  res0 <- VisualizeDesign(sampleData = sampleData,
                          designFormula = NULL,
                          designMatrix = model.matrix(
                            ~0 + genotype + treatment,
                            data = sampleData))
  res <- res0$sampledata

  expect_equal(res0$totnbrrows, 2L)
  expect_equal(nrow(res0$designmatrix), 8L)
  expect_equal(ncol(res0$designmatrix), 3L)
  expect_equivalent(res0$designmatrix[, "genotypeA"], rep(c(1L, 0L), each = 4))
  expect_equivalent(res0$designmatrix[, "genotypeB"], rep(c(0L, 1L), each = 4))
  expect_equivalent(res0$designmatrix[, "treatmenttrt"], rep(c(1L, 0L), 4))
  expect_equal(res$value[res$genotype == "A" & res$treatment == "trt"],
               "genotypeA + treatmenttrt")
  expect_equal(res$value[res$genotype == "A" & res$treatment == "ctrl"],
               "genotypeA")
  expect_equal(res$value[res$genotype == "B" & res$treatment == "trt"],
               "genotypeB + treatmenttrt")
  expect_equal(res$value[res$genotype == "B" & res$treatment == "ctrl"],
               "genotypeB")
})

test_that("VisualizeDesign works with three predictors", {
  sampleData <- data.frame(
    genotype = rep(c("A", "B"), each = 4),
    treatment = rep(c("trt", "ctrl"), 4),
    pred3 = rep(rep(paste0("C", 1:2), each = 2), 2),
    stringsAsFactors = FALSE
  )

  res0 <- VisualizeDesign(sampleData = sampleData,
                          designFormula = ~genotype + treatment + pred3)
  res <- res0$sampledata

  expect_equal(res0$totnbrrows, 4L)
  expect_equal(nrow(res0$designmatrix), 8L)
  expect_equal(ncol(res0$designmatrix), 4L)
  expect_equivalent(res0$designmatrix[, "(Intercept)"], rep(1L, 8L))
  expect_equivalent(res0$designmatrix[, "genotypeB"],
                    as.integer(sampleData$genotype == "B"))
  expect_equivalent(res0$designmatrix[, "treatmenttrt"],
                    as.integer(sampleData$treatment == "trt"))
  expect_equivalent(res0$designmatrix[, "pred3C2"],
                    as.integer(sampleData$pred3 == "C2"))

  expect_equal(res$value[res$genotype == "A" & res$treatment == "ctrl" &
                           res$pred3 == "C1"], "(Intercept)")
  expect_equal(res$value[res$genotype == "A" & res$treatment == "trt" &
                           res$pred3 == "C1"], "(Intercept) + treatmenttrt")
  expect_equal(res$value[res$genotype == "A" & res$treatment == "ctrl" &
                           res$pred3 == "C2"], "(Intercept) + pred3C2")
  expect_equal(res$value[res$genotype == "A" & res$treatment == "trt" &
                           res$pred3 == "C2"], "(Intercept) + treatmenttrt + pred3C2")
  expect_equal(res$value[res$genotype == "B" & res$treatment == "ctrl" &
                           res$pred3 == "C1"], "(Intercept) + genotypeB")
  expect_equal(res$value[res$genotype == "B" & res$treatment == "trt" &
                           res$pred3 == "C1"], "(Intercept) + genotypeB + treatmenttrt")
  expect_equal(res$value[res$genotype == "B" & res$treatment == "ctrl" &
                           res$pred3 == "C2"], "(Intercept) + genotypeB + pred3C2")
  expect_equal(res$value[res$genotype == "B" & res$treatment == "trt" &
                           res$pred3 == "C2"], "(Intercept) + genotypeB + treatmenttrt + pred3C2")

  expect_equal(nrow(res), 8L)
  expect_equal(colnames(res), c("genotype", "treatment", "pred3", "value", "nSamples"))
})

test_that(".AddNewLine works", {
  st <- "abcdefgh+ijklmnopqrst+abstkdlsi"
  expect_equal(.AddNewLine(st, lineWidth = 5),
               "abcdefgh+\nijklmnopqrst+\nabstkdlsi")
  expect_equal(.AddNewLine(st, lineWidth = 10),
               "abcdefgh+\nijklmnopqrst+\nabstkdlsi")
  expect_equal(.AddNewLine(st, lineWidth = 20),
               "abcdefgh+ijklmnopqrst+\nabstkdlsi")

  st <- "abc+defgh+ijklmn+opqrst+abstkdlsi"
  expect_equal(.AddNewLine(st, lineWidth = 20),
               "abc+defgh+ijklmn+opqrst+\nabstkdlsi")

  st <- "abc defgh+ijklmnopqrst+abstkdlsi"
  expect_equal(.AddNewLine(st, lineWidth = 20),
               "abc defgh+\nijklmnopqrst+\nabstkdlsi")
})
