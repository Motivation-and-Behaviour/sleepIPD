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
apply_thresholds <- function(intensity, ages, device, wear_location){
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
#'
#' @examples
#' example_path <- file.path("C:","Documents","Study Files", fsep = "\\")
#' translate_filepath(example_path)
translate_filepath <- function(path){
  gsub("\\\\", "/", path)
}

collate_outputs <- function(outputdir, verbose){

}

build_meta <- function(){
  # TODO build the metadata file for the user with the file paths
}

tempdir()
