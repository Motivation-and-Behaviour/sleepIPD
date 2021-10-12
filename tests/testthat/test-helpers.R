test_that("correct thresholds are returned", {
  # Simple
  expect_equal(apply_thresholds("light", "children", "geneactiv","wrist"),
               51.6)
  # Multiple
  expect_equal(apply_thresholds("mod", c("children","adults"),
                                "geneactiv","wrist"),
               c(191.6, 93.2))
})

test_that("Windows file paths are translated", {
  expect_match(translate_filepath("C:\\Documents\\Study Name With Spaces"),
               "C:/Documents/Study Name With Spaces",
               fixed = TRUE)
})

test_that("study names are validated", {
  expect_match(translate_studyname("A study name with spaces"),
               "A-study-name-with-spaces",
               fixed = TRUE)
})
