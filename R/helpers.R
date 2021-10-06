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
#' @family helper functions (internal)
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
#' @family helper functions
#'
#' @examples
#' example_path <- file.path("C:", "Documents", "Study Files", fsep = "\\")
#' translate_filepath(example_path)
translate_filepath <- function(path) {
  gsub("\\\\", "/", path)
}

#' Remove White Space from a Study Name
#'
#' @param studyname Name of the study
#' @family helper functions (internal)
#'
#' @return Study name with whitespace replaced with \code{-}
translate_studyname <- function(studyname){
  gsub("\\s", "-", studyname)
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
#' @family helper functions
#'
#' @export
collate_outputs <- function(outputdir, studyname, verbose) {
  outputdir <- translate_filepath(outputdir)
  studyname <- translate_studyname(studyname)

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

#' Create Pre-filled Metadata Template
#'
#' @param outputdir The directory where the output files were created.
#' @param studyname The name of the study. Use the first author's surname where
#' there is no study name.
#' @param overwrite If an existing metadata template is found, should it be
#' replaced? Default is `FALSE`.
#' @param wear_location Optionally pre-fill the wear location field of the
#' metadata. If there are multiple wear locations, this is ignored.
#'
#' @family helper functions
#'
#' @export
build_meta <- function(outputdir,
                       studyname,
                       overwrite = FALSE,
                       wear_location = NULL
                       ) {
  studyname <- translate_studyname(studyname)
  new_filename <- file.path(outputdir, paste0(studyname, "_metadata.xlsx"))
  template <- system.file("extdata", "sleepIPD_metadata_template.xlsx",
                          package = "sleepIPD")

  if (!overwrite & file.exists(new_filename)) {
    err_msg <- "A file already exists at {usethis::ui_path(new_filename)}.
    Use {usethis::ui_code('overwrite = TRUE')} to replace."
    usethis::ui_stop(err_msg)
  }

  # Check we can find the part 2 files
  part2file <- file.path(outputdir,
                         "sleepIPD_output",
                         paste0("part2_", studyname, ".csv"))

  if (!file.exists(part2file)) {
    err_msg <- "Could not find part 2 data at {usethis::ui_path(part2file)}."
    usethis::ui_stop(err_msg)
  }

  # Create the data for writing
  unique_files <- readr::read_csv(part2file, show_col_types = FALSE) %>%
    dplyr::distinct(.data$filename) %>%
    dplyr::pull(.data$filename)



  # Add new data in
  file.copy(template, new_filename, overwrite = overwrite)
  wb <- openxlsx::loadWorkbook(new_filename)
  openxlsx::writeData(wb = wb, sheet= "Metadata", x = unique_files,
                      startCol = 1, startRow = 2)
  if (length(wear_location)==1){
    loc_out <- rep(switch(wear_location, "wrist"=1, "hip"=2),
                   length(unique_files))
    openxlsx::writeData(wb = wb, sheet= "Metadata", x = loc_out,
                        startCol = 3, startRow = 2)
  }
  openxlsx::saveWorkbook(wb, new_filename, overwrite = TRUE)


  msg <- "Saved metadata template ({usethis::ui_path(new_filename)})"
  usethis::ui_done(msg)


}

#' Check Number of Files Matches Expectations
#'
#' An internal function. Performs a check for output files to confirm the
#' correct number are found.
#'
#' @param part The GGIR part the files relate to (as a string).
#' @param expect The number of files expected to be found.
#' @param found The number of files actually found.
#' @param verbose Should a success message be displayed?
#'
#' @family helper functions (internal)
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
