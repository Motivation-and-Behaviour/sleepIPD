#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param folder DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#'
#' @importFrom dplyr %>%
#' @examples
#' # ADD_EXAMPLES_HERE
assemble_datasheet <- function(folder) {
  folder_files <- list.files(folder, pattern = ".csv$")

  # Part 2 file
  part2_filepath <-
    file.path(folder, folder_files[grepl("^part2", folder_files)])

  part2_df <- readr::read_csv(part2_filepath)

  part2_df <-
    part2_df %>%
    dplyr::select(
      # Meta
      "filename", "N valid hours", "N hours", "weekday", "measurementday",
      # Intensity gradient
      "ig_gradient_ENMO_0-24hr", "ig_intercept_ENMO_0-24hr",
      "ig_rsquared_ENMO_0-24hr", "M16hr_ENMO_mg_0-24hr", "M16_ENMO_mg_0-24hr",
      "M16_ig_gradient_ENMO_mg_0-24hr", "M16_ig_intercept_ENMO_mg_0-24hr",
      "M16_ig_rsquared_ENMO_mg_0-24hr", dplyr::matches("^p.*_ENMO_mg_0-24hr$")
    )

  # Part 4 file
  part4_filepath <-
    file.path(folder, folder_files[grepl("^part4", folder_files)])

  part4_df <- readr::read_csv(part4_filepath)

  # TODO: Select the columns that we want to keep

  # Part 5 file
  part5_filepath <-
    file.path(folder, folder_files[grepl("^part5", folder_files)])

  part5_df <- readr::read_csv(part5_filepath)

  # TODO: Select the columns that we want to keep

  # TODO: Merge the dataframes together

  # TODO: Write out the data
}
