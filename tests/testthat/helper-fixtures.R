# Collated GGIR output for one file over 25-27 Feb 2016 (Thursday to Saturday)
write_ggir_fixture <- function(dir,
                               part2_dates = c(
                                 "2016-02-25T15:15:00+0100",
                                 "2016-02-26T00:00:00+0100",
                                 "2016-02-27T00:00:00+0100"
                               ),
                               part5_prefix = "",
                               part5_windows = c("WW", "MM"),
                               part5_threshold = "L48.1M201.4V707",
                               sri = c(NA, 0.6, 0.7)) {
  dir.create(dir, showWarnings = FALSE)
  dates <- as.Date(c("2016-02-25", "2016-02-26", "2016-02-27"))
  weekdays <- c("Thursday", "Friday", "Saturday")

  part2 <- data.frame(
    filename = "P01.bin",
    `N valid hours` = c(8.7, 24, 24),
    `N hours` = c(8.75, 24, 24),
    weekday = weekdays,
    measurementday = 1:3,
    calendar_date = part2_dates,
    check.names = FALSE
  )
  readr::write_csv(part2, file.path(dir, "part2_test.csv"))

  part4 <- data.frame(
    filename = "P01.bin.RData",
    weekday = weekdays,
    calendar_date = format(dates, "%d/%m/%Y"),
    night = 1:3,
    sleeponset = c(23.1, 22.9, 23.4),
    wakeup = c(31.2, 30.8, 31.0),
    sleeponset_ts = c("23:06:00", "22:54:00", "23:24:00"),
    wakeup_ts = c("07:12:00", "06:48:00", "07:00:00")
  )
  if (!is.null(sri)) part4$SleepRegularityIndex <- sri
  readr::write_csv(part4, file.path(dir, "part4_test.csv"), na = "")

  part5 <- lapply(part5_windows, function(window) {
    data.frame(
      p5filename = paste0(
        part5_prefix, "part5_daysummary_", window, "_", part5_threshold,
        "_T5A5.csv"
      ),
      filename = "P01.bin.RData",
      window_number = 1:3,
      weekday = weekdays,
      calendar_date = format(dates, "%Y-%m-%d"),
      sleeponset = c(23.1, 22.9, 23.4),
      wakeup = c(31.2, 30.8, 31.0),
      sleeponset_ts = c("23:06:00", "22:54:00", "23:24:00"),
      wakeup_ts = c("07:12:00", "06:48:00", "07:00:00"),
      dur_day_total_IN_min = c(600, 610, 590),
      dur_day_total_LIG_min = c(200, 190, 210),
      dur_day_total_MOD_min = c(60, 55, 65),
      dur_day_total_VIG_min = c(10, 8, 12),
      dur_day_min = c(870, 863, 877),
      dur_spt_min = c(570, 577, 563),
      ACC_day_mg = c(30.1, 28.4, 31.7),
      M16TIME = c(
        "2016-02-25T14:55:25+0100", "2016-02-26T14:10:00+0100",
        "2016-02-27T13:40:00+0100"
      )
    )
  })
  readr::write_csv(do.call(rbind, part5), file.path(dir, "part5_test.csv"))

  invisible(dir)
}
