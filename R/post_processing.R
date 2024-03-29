#' Assemble Datasheet
#'
#' Find the outputs of reprocessing and assemble them into a datasheet.
#'
#' @param folder The folder that [collate_outputs()] (or [reprocess()]) wrote to
#' during reprocessing.
#' @param verbose Should a success messages be displayed? `TRUE` by default.
#'
#' @return Nothing is returned. A new datasheet is saved to the folder.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' outfolder <- "C:\\Documents\\My Study\\Output\\sleepIPD_output"
#' assemble_datasheet(outfolder)
#' }
assemble_datasheet <- function(folder, verbose = TRUE) {
  folder_files <- list.files(folder, pattern = ".csv$")
  part2_filepath <-
    file.path(folder, folder_files[grepl("^part2", folder_files)])
  part4_filepath <-
    file.path(folder, folder_files[grepl("^part4", folder_files)])
  part5_filepath <-
    file.path(folder, folder_files[grepl("^part5", folder_files)])

  if (!all(file.exists(part2_filepath, part4_filepath, part5_filepath))) {
    usethis::ui_stop(
      "Could not find all required files. Are you in the right folder?"
    )
  }
  if (verbose) usethis::ui_done("Located files.")

  # Part 2

  part2_cols <- c(
    # Meta
    "filename", "N valid hours", "N hours", "weekday", "measurementday",
    "calendar_date",
    # Intensity gradient
    "ig_gradient_ENMO_0-24hr", "ig_intercept_ENMO_0-24hr",
    "ig_rsquared_ENMO_0-24hr", "M16hr_ENMO_mg_0-24hr", "M16_ENMO_mg_0-24hr",
    "M16_ig_gradient_ENMO_mg_0-24hr", "M16_ig_intercept_ENMO_mg_0-24hr",
    "M16_ig_rsquared_ENMO_mg_0-24hr"
  )

  part2_df <-
    readr::read_csv(
      part2_filepath,
      col_types = readr::cols(), progress = FALSE
    ) %>%
    dplyr::select(
      dplyr::any_of(part2_cols),
      dplyr::matches("^p.*_ENMO_mg_0-24hr$")
    )

  if (verbose) usethis::ui_done("Loaded part 2.")

  # Part 4

  part4_cols <- c(
    # Meta
    "filename", "weekday", "calendar_date", "night",
    # Sleep meta
    "guider", "daysleeper", "sleepparam", "fraction_night_invalid",
    # Sleep data
    "sleeponset", "wakeup", "SptDuration", "guider_onset", "guider_wakeup",
    "guider_SptDuration", "guider_inbedStart", "guider_inbedEnd",
    "guider_inbedDuration", "guider_inbedStart_ts", "guider_inbedEnd_ts",
    "SleepDurationInSpt", "WASO", "sleeponset_ts", "wakeup_ts",
    "guider_onset_ts", "guider_wakeup_ts", "SleepRegularityIndex",
    "SriFractionValid", "sleeplatency", "sleepefficiency"
  )

  part4_df <- readr::read_csv(
    part4_filepath,
    col_types = readr::cols(), progress = FALSE
  ) %>%
    dplyr::select(dplyr::any_of(part4_cols))

  if (verbose) usethis::ui_done("Loaded part 4.")

  # Part 5

  # TODO: check if this can be further reduced
  part5_cols <- c(
    # Meta
    "filename", "window_number", "weekday", "calendar_date", "night_number",
    "nonwear_perc_day", "nonwear_perc_spt",
    "nonwear_perc_day_spt", "daytype",
    # PA and Sleep
    "sleeponset", "sleeponset_ts", "wakeup", "wakeup_ts", "dur_spt_sleep_min",
    "dur_spt_wake_IN_min", "dur_spt_wake_LIG_min", "dur_spt_wake_MOD_min",
    "dur_spt_wake_VIG_min", "dur_day_IN_unbt_min", "dur_day_LIG_unbt_min",
    "dur_day_MOD_unbt_min", "dur_day_VIG_unbt_min", "dur_day_MVPA_bts_1_min",
    "dur_day_IN_bts_30_min", "dur_day_IN_bts_10_30_min",
    "dur_day_IN_bts_1_10_min", "dur_day_LIG_bts_10_min",
    "dur_day_LIG_bts_1_10_min", "dur_day_total_IN_min", "dur_day_total_LIG_min",
    "dur_day_total_MOD_min", "dur_day_total_VIG_min", "dur_day_min",
    "dur_spt_min", "dur_day_spt_min", "sleep_efficiency", "ACC_spt_sleep_mg",
    "ACC_spt_wake_IN_mg", "ACC_spt_wake_LIG_mg", "ACC_spt_wake_MOD_mg",
    "ACC_spt_wake_VIG_mg", "ACC_day_IN_unbt_mg", "ACC_day_LIG_unbt_mg",
    "ACC_day_MOD_unbt_mg", "ACC_day_VIG_unbt_mg", "ACC_day_MVPA_bts_1_mg",
    "ACC_day_IN_bts_30_mg", "ACC_day_IN_bts_10_30_mg", "ACC_day_IN_bts_1_10_mg",
    "ACC_day_LIG_bts_10_mg", "ACC_day_LIG_bts_1_10_mg", "ACC_day_total_IN_mg",
    "ACC_day_total_LIG_mg", "ACC_day_total_MOD_mg", "ACC_day_total_VIG_mg",
    "ACC_day_mg", "ACC_spt_mg", "ACC_day_spt_mg", "quantile_mostactive60min_mg",
    "quantile_mostactive30min_mg", "L16TIME", "L16VALUE", "M16TIME",
    "M16VALUE", "L16TIME_num", "M16TIME_num", "L1TIME", "L1VALUE",
    "M1TIME", "M1VALUE", "L1TIME_num", "M1TIME_num", "L0.5TIME",
    "L0.5VALUE", "M0.5TIME", "M0.5VALUE", "L0.5TIME_num", "M0.5TIME_num",
    "L0.25TIME", "L0.25VALUE", "M0.25TIME", "M0.25VALUE", "L0.25TIME_num",
    "M0.25TIME_num", "L0.166666666666667TIME", "L0.166666666666667VALUE",
    "M0.166666666666667TIME", "M0.166666666666667VALUE",
    "L0.166666666666667TIME_num", "M0.166666666666667TIME_num",
    "L0.0833333333333333TIME", "L0.0833333333333333VALUE",
    "M0.0833333333333333TIME", "M0.0833333333333333VALUE",
    "L0.0833333333333333TIME_num", "M0.0833333333333333TIME_num",
    "Nbouts_day_MVPA_bts_1", "Nbouts_day_IN_bts_30", "Nbouts_day_IN_bts_10_30",
    "Nbouts_day_IN_bts_1_10", "Nbouts_day_LIG_bts_10",
    "Nbouts_day_LIG_bts_1_10", "Nblocks_spt_sleep", "Nblocks_spt_wake_IN",
    "Nblocks_spt_wake_LIG", "Nblocks_spt_wake_MOD", "Nblocks_spt_wake_VIG",
    "Nblocks_day_IN_unbt", "Nblocks_day_LIG_unbt", "Nblocks_day_MOD_unbt",
    "Nblocks_day_VIG_unbt", "Nblocks_day_MVPA_bts_1", "Nblocks_day_IN_bts_30",
    "Nblocks_day_IN_bts_10_30", "Nblocks_day_IN_bts_1_10",
    "Nblocks_day_LIG_bts_10", "Nblocks_day_LIG_bts_1_10",
    "Nblocks_day_total_IN", "Nblocks_day_total_LIG", "Nblocks_day_total_MOD",
    "Nblocks_day_total_VIG", "boutcriter.in", "boutcriter.lig",
    "boutcriter.mvpa", "boutdur.in", "boutdur.lig", "boutdur.mvpa",
    "bout.metric", "ig_gradient", "ig_intercept", "ig_rsquared",
    "dur_day_total_IN_min_MM", "dur_day_total_LIG_min_MM",
    "dur_day_total_MOD_min_MM", "dur_day_total_VIG_min_MM", "dur_day_min_MM",
    "dur_spt_min_MM"
  )

  thresh_strings <-
    thresholds %>%
    dplyr::transmute(
      thresh = paste0("L", .data$light, "M", .data$mod, "V", .data$vig),
      thresh_age = .data$age,
      thresh_dev = .data$dev,
      thresh_wear_loc = .data$wear_loc
    )

  part5_df <- readr::read_csv(
    part5_filepath,
    col_types = readr::cols(), progress = FALSE
  )

  part5_df_mm <-
    part5_df %>%
    dplyr::filter(
      # Just use the MM files
      stringr::str_detect(.data$p5filename, "^part5_daysummary_MM") &
        stringr::str_detect(
          .data$p5filename,
          paste(dplyr::pull(thresh_strings, .data$thresh), collapse = "|")
        )
    ) %>%
    dplyr::rename_with(.cols = c(
      "dur_day_total_IN_min", "dur_day_total_LIG_min",
      "dur_day_total_MOD_min", "dur_day_total_VIG_min", "dur_day_min",
      "dur_spt_min"
    ), ~ paste0(., "_MM")) %>%
    fuzzyjoin::regex_left_join(
      thresh_strings,
      by = c("p5filename" = "thresh")
    ) %>%
    dplyr::select(
      "filename", "calendar_date", dplyr::ends_with("_MM"),
      tidyselect::starts_with("thresh_")
    )

  part5_df_all <-
    part5_df %>%
    dplyr::filter(
      stringr::str_detect(.data$p5filename, "^part5_daysummary_WW") &
        stringr::str_detect(
          .data$p5filename,
          paste(dplyr::pull(thresh_strings, .data$thresh), collapse = "|")
        )
    ) %>%
    fuzzyjoin::regex_left_join(
      thresh_strings,
      by = c("p5filename" = "thresh")
    ) %>%
    dplyr::left_join(
      part5_df_mm,
      by = c(
        "filename", "calendar_date", "thresh_age", "thresh_dev",
        "thresh_wear_loc"
      )
    ) %>%
    dplyr::select(
      dplyr::any_of(part5_cols),
      tidyselect::starts_with("thresh_")
    )

  if (verbose) usethis::ui_done("Loaded part 5.")

  # Make the dataframes consistent
  part2_df_clean <-
    part2_df %>%
    dplyr::mutate(
      calendar_date = lubridate::as_date(
        lubridate::parse_date_time(
          .data$calendar_date,
          orders = c("dmY HM", "Ymd HMS")
        )
      ),
      filename = as.character(.data$filename)
    )

  part4_df_clean <-
    part4_df %>%
    dplyr::mutate(
      calendar_date =
        lubridate::as_date(.data$calendar_date, format = "%d/%m/%Y"),
      filename = as.character(
        stringr::str_replace(.data$filename, ".RData", "")
      )
    ) %>%
    dplyr::rename(
      sleeponset_p4 = "sleeponset", wakeup_p4 = "wakeup",
      sleeponset_ts_p4 = "sleeponset_ts", wakeup_ts_p4 = "wakeup_ts"
    )

  part5_df_clean <-
    part5_df_all %>%
    dplyr::mutate(
      calendar_date =
        lubridate::as_date(.data$calendar_date, format = "%d/%m/%Y"),
      filename =
        as.character(stringr::str_replace(.data$filename, ".RData", ""))
    ) %>%
    dplyr::rename(
      sleeponset_p5 = "sleeponset", wakeup_p5 = "wakeup",
      sleeponset_ts_p5 = "sleeponset_ts", wakeup_ts_p5 = "wakeup_ts"
    )

  # Merge the dataframes together
  merged_df <-
    part2_df_clean %>%
    dplyr::full_join(part4_df_clean, by = c("filename", "calendar_date")) %>%
    dplyr::left_join(part5_df_clean, by = c("filename", "calendar_date"))

  # Write out the data
  readr::write_csv(merged_df, file.path(folder, "merged_data.csv"))
  if (verbose) usethis::ui_done("Wrote datasheet.")
}
