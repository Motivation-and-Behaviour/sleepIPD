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
#' @return vector of threshold cut-points
apply_thresholds <- function(intensity, ages, device, wear_location){
  thresholds %>%
    dplyr::filter(age %in% ages &
                    dev %in% device &
                    wear_loc %in% wear_location) %>%
    dplyr::pull(intensity)

}

build_meta <- function(){
  # TODO build the metadata file for the user with the file paths
}
