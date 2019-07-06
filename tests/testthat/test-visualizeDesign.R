test_that("visualizeDesign works", {
  sampleData <- data.frame(
    genotype = rep(c("A", "B"), each = 4),
    treatment = rep(c("trt", "ctrl"), 4),
    stringsAsFactors = FALSE
  )

  res <- visualizeDesign(sampleData = sampleData,
                         designFormula = ~genotype)$sampledata

  expect_equal(res$value[res$genotype == "A"], "(Intercept)")
  expect_equal(res$value[res$genotype == "B"], "(Intercept) + genotypeB")
  expect_equal(nrow(res), 2L)
  expect_equal(colnames(res), c("genotype", "value"))

  res <- visualizeDesign(sampleData = sampleData,
                         designFormula = ~genotype + treatment)$sampledata

  expect_equal(res$value[res$genotype == "A" & res$treatment == "trt"],
               "(Intercept) + treatmenttrt")
  expect_equal(res$value[res$genotype == "A" & res$treatment == "ctrl"],
               "(Intercept)")
  expect_equal(res$value[res$genotype == "B" & res$treatment == "trt"],
               "(Intercept) + genotypeB + treatmenttrt")
  expect_equal(res$value[res$genotype == "B" & res$treatment == "ctrl"],
               "(Intercept) + genotypeB")


})
