test_that("correct thresholds are returned", {
  # Simple
  expect_equal(
    apply_thresholds("light", "children", "geneactiv", "wrist"),
    51.6
  )
  # Multiple
  expect_equal(
    apply_thresholds(
      "mod", c("children", "adults"),
      "geneactiv", "wrist"
    ),
    c(191.6, 93.2)
  )
})

test_that("Windows file paths are translated", {
  expect_match(translate_filepath("C:\\Documents\\Study Name With Spaces"),
    "C:/Documents/Study Name With Spaces",
    fixed = TRUE
  )
})

test_that("study names are validated", {
  expect_match(translate_studyname("A study name with spaces"),
    "A-study-name-with-spaces",
    fixed = TRUE
  )
})

test_that("GGIR dates parse to the local calendar date", {
  iso <- c(
    "2016-02-26T00:00:00+0100", "2016-02-26T15:15:00+0100",
    "2016-02-26T00:00:00+0200", "2016-02-26T00:00:00+1000",
    "2016-02-26T00:00:00+1300", "2016-02-26T00:00:00+0000",
    "2016-02-26T00:00:00-0300", "2016-02-26T15:15:00-0300",
    "2016-02-26T00:00:00-0500", "2016-02-26T23:59:00-0500",
    "2016-02-25T23:59:55+0100"
  )
  expect_equal(parse_ggir_date(iso), rep(as.Date("2016-02-26"), length(iso)))

  other <- c(
    "26/02/2016 00:00", "26/02/2016 15:15", "26/02/2016",
    "2016-02-26 00:00:00", "2016-02-26"
  )
  expect_equal(
    parse_ggir_date(other), rep(as.Date("2016-02-26"), length(other))
  )

  expect_equal(
    parse_ggir_date(c(NA, "2016-02-26")), as.Date(c(NA, "2016-02-26"))
  )
  expect_error(parse_ggir_date("02/26/2016"), "02/26/2016")
  expect_error(parse_ggir_date("2016-02-26T00:00:00Z"), "UTC")
})

test_that("collated part 5 values are kept as written", {
  outdir <- withr::local_tempdir()
  results <- file.path(outdir, "results")
  dir.create(results)
  writeLines("filename", file.path(results, "part2_daysummary.csv"))
  writeLines(
    "filename", file.path(results, "part4_nightsummary_sleep_cleaned.csv")
  )
  writeLines(
    c("filename,M16TIME", "P01.bin.RData,2016-02-25T14:55:25+0100"),
    file.path(results, "part5_daysummary_WW_L48.1M201.4V707_T5A5.csv")
  )
  collate_outputs(outdir, "test", verbose = FALSE)
  p5 <- readLines(file.path(outdir, "sleepIPD_output", "part5_test.csv"))
  expect_match(p5[2], "2016-02-25T14:55:25+0100", fixed = TRUE)
})
