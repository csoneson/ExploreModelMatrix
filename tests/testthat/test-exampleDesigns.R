context("ExampleDesigns")

test_that(".ExampleDesigns fails with incorrect inputs", {
  expect_error(ExploreModelMatrix:::.ExampleDesigns("unknown"),
               regexp = "Unidentified example ID")
})

test_that(".ExampleDesign works correctly", {
  ed <- ExploreModelMatrix:::.ExampleDesigns("One factor, unpaired samples")
  expect_equal(ed$sampledata,
               data.frame(
                 treatment = rep(c("control", "drug1", "drug2"), 3),
                 subject = factor(seq_len(9))
               ))
  expect_equal(ed$design, "~ treatment")

  ed <- ExploreModelMatrix:::.ExampleDesigns("One factor, paired samples")
  expect_equal(ed$sampledata,
               data.frame(
                 treatment = rep(c("control", "drug1", "drug2"), 3),
                 subject = factor(rep(seq_len(3), each = 3))
               ))
  expect_equal(ed$design, "~ subject + treatment")


  ed <- ExploreModelMatrix:::.ExampleDesigns("Two crossed factors")
  expect_equal(ed$sampledata,
               data.frame(
                 treatment = rep(c("placebo", "drug"), 3),
                 time = rep(c("0h", "1h", "2h"), each = 2)
               ))
  expect_equal(ed$design, "~ time * treatment")

  ed <- ExploreModelMatrix:::.ExampleDesigns("Two crossed, one blocking factor")
  expect_equal(ed$sampledata,
               data.frame(
                 treatment = rep(c("placebo", "drug"), 6),
                 time = rep(rep(c("0h", "1h", "2h"), each = 2), 2),
                 batch = rep(c("day1", "day2"), each = 6)
               ))
  expect_equal(ed$design, "~ time * treatment + batch")

  ed <- ExploreModelMatrix:::.ExampleDesigns("Two crossed, one nested factor")
  expect_equal(ed$sampledata,
               data.frame(
                 diagnosis = rep(c("healthy", "disease1", "disease2"), 6),
                 treatment = rep(rep(c("none", "hormone"), each = 3), 3),
                 subject = paste0(rep(c("healthy", "disease1", "disease2"), 6),
                                  rep(c("A", "B", "C"), each = 6))
               ))
  expect_equal(ed$design, "~ diagnosis:treatment + subject")

  ed <- ExploreModelMatrix:::.ExampleDesigns(
    "Two crossed, one nested factor, dummy coded"
  )
  expect_equal(ed$sampledata,
               data.frame(
                 diagnosis = rep(c("healthy", "disease1", "disease2"), 6),
                 treatment = rep(rep(c("none", "hormone"), each = 3), 3),
                 dummy = rep(c("A", "B", "C"), each = 6),
                 subject = paste0(rep(c("healthy", "disease1", "disease2"), 6),
                                  rep(c("A", "B", "C"), each = 6))
               ))
  expect_equal(ed$design,
               "~ diagnosis + diagnosis:dummy + diagnosis:treatment")
})
