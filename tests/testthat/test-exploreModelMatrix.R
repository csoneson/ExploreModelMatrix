test_that("ExploreModelMatrix fails with incorrect inputs", {
  sampleData <- data.frame(
    genotype = rep(c("A", "B"), each = 4),
    treatment = rep(c("trt", "ctrl"), 4),
    stringsAsFactors = FALSE
  )
  designFormula <- ~genotype

  expect_error(ExploreModelMatrix(sampleData = 1,
                                  designFormula = designFormula))
  expect_error(ExploreModelMatrix(sampleData = as.matrix(sampleData),
                                  designFormula = designFormula))
  expect_error(ExploreModelMatrix(sampleData = as.list(sampleData),
                                  designFormula = designFormula))
  expect_error(ExploreModelMatrix(sampleData = sampleData,
                                  designFormula = "~genotype"))
})

test_that("ExploreModelMatrix generates a shiny app object", {
  sampleData <- data.frame(
    genotype = rep(c("A", "B"), each = 4),
    treatment = rep(c("trt", "ctrl"), 4),
    stringsAsFactors = FALSE
  )
  designFormula <- ~genotype

  expect_s3_class(ExploreModelMatrix(sampleData = sampleData,
                                     designFormula = ~genotype),
                  "shiny.appobj")
})
