#' Choose the correct intensity thresholds
#'
#' A helper function for choosing intensity thresholds. Not intended to be
#' invoked directly by the user.
#'
#' @param intensity The thresholds to return. Must be one of 'light', 'mod', or
#' 'vig'
#' @param ages The age group (i.e., children or adults or both)
#' @param device The device (i.e., actigraph or geneactiv or both)
#' @param wear_location The wear location of the device (i.e., 'wrist' or 'hip')
#'
#' @importFrom rlang .data
#'
#' @return vector of threshold cut-points
apply_thresholds <- function(intensity, ages, device, wear_location) {
  thresholds %>%
    dplyr::filter(.data$age %in% ages &
      .data$dev %in% device &
      .data$wear_loc %in% wear_location) %>%
    dplyr::pull(intensity)
}

#' Convert a File or Folder Path
#'
#' GGIR uses some code which does not play nice with some file paths on Windows
#' (e.g., those produced by \code{tempdir()}). This function converts between a
#' file path to the format that works with GGIR.
#'
#' @param path The file or folder path to convert.
#'
#' @return The same file path with \code{\\\\} replaced with \code{/}
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' example_path <- file.path("C:", "Documents", "Study Files", fsep = "\\")
#' translate_filepath(example_path)
translate_filepath <- function(path) {
  gsub("\\\\", "/", path)
}

#' Collate GGIR Outputs
#'
#' This function simply finds the outputs from GGIR that are required for the
#' project, and copies them to a new folder. If there are multiple copies of
#' part 5 outputs, they are collapsed to a single csv file.
#'
#' @param outputdir the outputdir that was provided in the processing call
#' @param studyname the name of the study. Use the first author's surname if
#' there is no study name.
#' @param verbose If success messages should be displayed
#'
#' @export
collate_outputs <- function(outputdir, studyname, verbose) {
  outputdir <- translate_filepath(outputdir)
  studyname <- gsub("\\s", "-", studyname)

  # Find all of the files
  # Part 2
  p2files <- list.files(
    path = outputdir, pattern = "part2_daysummary.csv",
    recursive = TRUE
  )

  check_outfiles("2", 1, length(p2files), verbose)

  p2files_renamed <- basename(gsub(
    "part2_daysummary",
    paste0("part2_", studyname),
    p2files
  ))

  p4files <- list.files(
    path = outputdir, pattern = "part4_nightsummary_sleep_cleaned.csv",
    recursive = TRUE
  )

  check_outfiles("4", 1, length(p4files), verbose)

  p4files_renamed <- basename(gsub(
    "part4_nightsummary_sleep_cleaned",
    paste0("part4_", studyname),
    p4files
  ))

  p5files <- list.files(
    path = outputdir, pattern = "part5_daysummary_.*.csv",
    recursive = TRUE
  )

  check_outfiles("5", 1024, length(p5files), verbose)


  # Create the directory
  collate_outdir <- file.path(outputdir, "sleepIPD_output")
  dir.create(collate_outdir, showWarnings = FALSE)

  # Copy files
  file.copy(file.path(outputdir, p2files),
    file.path(collate_outdir, p2files_renamed),
    overwrite = TRUE
  )

  file.copy(file.path(outputdir, p4files),
    file.path(collate_outdir, p4files_renamed),
    overwrite = TRUE
  )

  # Read all part 5 into one data frame
  p5df <- readr::read_csv(file.path(outputdir, p5files),
    id = "p5filename",
    show_col_types = FALSE,
    lazy = FALSE
  )

  p5df <- p5df %>%
    dplyr::mutate(p5filename = basename(.data$p5filename))

  readr::write_csv(p5df,
    file.path(collate_outdir, paste0("part5_", studyname, ".csv")),
    append = FALSE, na = ""
  )

  if (verbose) {
    usethis::ui_done(
      "Collated data in {usethis::ui_field(collate_outdir)}"
    )
  }
}

build_meta <- function() {
  # TODO build the metadata file for the user with the file paths
}

check_outfiles <- function(part, expect, found, verbose) {
  files <- if (expect == 1) "file" else "files"

  if (found == 0) {
    usethis::ui_stop(
      "Could not find part {part} {files}. Did GGIR complete?"
    )
  } else if (found > expect) {
    msg <- paste0(
      "Found {usethis::ui_value(found)} part {part} files ",
      "(expected {usethis::ui_value(expect)}).\n",
      "Does your output directory have previous processing?"
    )
    usethis::ui_stop(msg)
  } else if (verbose) {
    usethis::ui_done("Found {usethis::ui_value(found)} part {part} {files}")
  }
}

