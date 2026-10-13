read_merged <- function(dir) {
  assemble_datasheet(dir, verbose = FALSE)
  merged <- readr::read_csv(
    file.path(dir, "merged_data.csv"),
    show_col_types = FALSE
  )
  merged[order(merged$calendar_date), ]
}

expect_complete_merge <- function(merged) {
  expect_equal(
    merged$calendar_date,
    as.Date(c("2016-02-25", "2016-02-26", "2016-02-27"))
  )
  expect_equal(merged$weekday.x, c("Thursday", "Friday", "Saturday"))
  expect_equal(merged$weekday.x, merged$weekday.y)
  expect_false(anyNA(merged$`N valid hours`))
  expect_false(anyNA(merged$night))
  expect_false(anyNA(merged$dur_day_min))
  expect_false(anyNA(merged$dur_day_min_MM))
}

test_that("part 2 dates keep the local day regardless of UTC offset", {
  east <- c(
    "2016-02-25T15:15:00+0100",
    "2016-02-26T00:00:00+0100",
    "2016-02-27T00:00:00+0100"
  )
  far_east <- c(
    "2016-02-25T09:00:00+1300",
    "2016-02-26T00:00:00+1300",
    "2016-02-27T00:00:00+1300"
  )
  west <- c(
    "2016-02-25T22:00:00-0300",
    "2016-02-26T00:00:00-0300",
    "2016-02-27T00:00:00-0300"
  )
  for (dates in list(east, far_east, west)) {
    dir <- withr::local_tempdir()
    write_ggir_fixture(dir, part2_dates = dates)
    expect_warning(merged <- read_merged(dir), NA)
    expect_complete_merge(merged)
  }
})

test_that("other part 2 date formats still parse", {
  dmy <- c("25/02/2016 15:15", "26/02/2016 00:00", "27/02/2016 00:00")
  ymd <- c("2016-02-25 15:15:00", "2016-02-26 00:00:00", "2016-02-27 00:00:00")
  for (dates in list(dmy, ymd)) {
    dir <- withr::local_tempdir()
    write_ggir_fixture(dir, part2_dates = dates)
    expect_warning(merged <- read_merged(dir), NA)
    expect_complete_merge(merged)
  }
})

test_that("part 5 files with a prefix are kept", {
  dir <- withr::local_tempdir()
  write_ggir_fixture(dir, part5_prefix = "a_")
  expect_warning(merged <- read_merged(dir), NA)
  expect_complete_merge(merged)
})

test_that("missing part 5 data is reported", {
  dir <- withr::local_tempdir()
  write_ggir_fixture(dir, part5_threshold = "L1M2V3")
  expect_error(
    suppressWarnings(assemble_datasheet(dir, verbose = FALSE)),
    "No part 5 WW rows"
  )

  dir <- withr::local_tempdir()
  write_ggir_fixture(dir, part5_windows = "WW")
  expect_warning(assemble_datasheet(dir, verbose = FALSE), "MM")
})

test_that("inconsistent part 2 dates are reported", {
  dir <- withr::local_tempdir()
  write_ggir_fixture(dir, part2_weekdays = c("Thursday", "Monday", "Saturday"))
  expect_warning(assemble_datasheet(dir, verbose = FALSE), "weekday")

  dir <- withr::local_tempdir()
  write_ggir_fixture(
    dir,
    part2_dates = c(
      "2016-02-25T15:15:00+0100",
      "2016-02-25T23:00:00+0100",
      "2016-02-27T00:00:00+0100"
    ),
    part2_weekdays = c("Thursday", "Thursday", "Saturday")
  )
  expect_warning(assemble_datasheet(dir, verbose = FALSE), "duplicated")
})

test_that("missing SleepRegularityIndex is reported", {
  dir <- withr::local_tempdir()
  write_ggir_fixture(dir, sri = NULL)
  expect_warning(
    assemble_datasheet(dir, verbose = FALSE), "SleepRegularityIndex"
  )

  dir <- withr::local_tempdir()
  write_ggir_fixture(dir, sri = c(NA, NA, NA))
  expect_warning(
    assemble_datasheet(dir, verbose = FALSE), "SleepRegularityIndex"
  )
})
