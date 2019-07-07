test_that("exploreModelMatrix fails with incorrect inputs", {
  sampleData <- data.frame(
    genotype = rep(c("A", "B"), each = 4),
    treatment = rep(c("trt", "ctrl"), 4),
    stringsAsFactors = FALSE
  )
  designFormula <- ~genotype

  expect_error(exploreModelMatrix(sampleData = 1,
                                  designFormula = designFormula))
  expect_error(exploreModelMatrix(sampleData = as.matrix(sampleData),
                                  designFormula = designFormula))
  expect_error(exploreModelMatrix(sampleData = as.list(sampleData),
                                  designFormula = designFormula))
  expect_error(exploreModelMatrix(sampleData = sampleData,
                                  designFormula = "~genotype"))
})

test_that("exploreModelMatrix generates a shiny app object", {
  sampleData <- data.frame(
    genotype = rep(c("A", "B"), each = 4),
    treatment = rep(c("trt", "ctrl"), 4),
    stringsAsFactors = FALSE
  )
  designFormula <- ~genotype

  expect_s3_class(exploreModelMatrix(sampleData = sampleData,
                                     designFormula = ~genotype),
                  "shiny.appobj")
})
