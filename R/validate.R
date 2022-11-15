#' Check GGIR Version
#'
#' Performs a check of the installed version of GGIR. The user is invited to
#' install the correct version if it cannot be located.
#'
#' @param verbose Should a success message be displayed? `TRUE` by default.
#'
#' @family validation functions
#'
#' @export
validate_GGIR <- function(verbose = TRUE) { # nolint
  ggir_ver <- utils::packageVersion("GGIR")
  req_ver <- "2.5.1"

  if (ggir_ver == req_ver) {
    if (verbose) {
      usethis::ui_done(
        "Correct version of GGIR located ({usethis::ui_value(ggir_ver)})."
      )
    }
  } else {
    msg <- "Incorrect version of GGIR installed ({usethis::ui_value(ggir_ver)}).
            Version {usethis::ui_value(req_ver)} is required."
    usethis::ui_oops(msg)

    request <-
      "sleepIPD can install the correct version for you. OK to proceed?"

    resp <- usethis::ui_yeah(request, yes = "Yes", no = "No", shuffle = FALSE)

    if (resp) {
      if (!requireNamespace("devtools", quietly = TRUE)) {
        usethis::ui_info(
          paste0(
            "{usethis::ui_code('devtools')} is also required ",
            "and will be installed."
          )
        )
        utils::install.packages("devtools")
      }
      detach("package:GGIR", unload = TRUE)
      devtools::install_github("wadpac/GGIR@2.5-1")
      validate_GGIR()
    } else {
      stop_msg <-
        "Processing cannot be completed without the correct version of GGIR."
      usethis::ui_stop(stop_msg)
    }
  }
}
