#' Process files to SleepIPD specs
#'
#' `process_files` collects information about your study sample and reprocesses
#' the files accordingly.
#'
#' The purpose of this function is to standardise the processing of the
#' accelerometery files. As such, most of the arguments come preset and should
#' not be modified. However, options which do not affect the output (e.g., the
#' decision to process in parallel or not) are available via `...`. See
#' [GGIR::g.shell.GGIR()] for details of these arguments.
#'
#' You can specify parameters of your study by supplying `ages`, `device`,
#' and/or `wear_location` (see Arguments for details). Not supplying these will
#' cause sleepIPD to calculate every combination of thresholds (resulting in
#' \eqn{8^3 \times 2 = 1024} output files). So, supplying them should slightly
#' reduce processing time.
#'
#' @param datadir The directory with the files to process or a vector of
#' file paths to process (as per [GGIR::g.shell.GGIR()]).
#' @param outputdir the directory to store the output (as per
#' [GGIR::g.shell.GGIR()]).
#' @param ages The age of the sample. Can be either `"children"` (for those 18
#' or less), `"adults"` (for >18) or both (i.e., `c("children", "adults")`). If
#' you do not specify, both are used.
#' @param device The device used in the study. Can be either `"geneactiv"`,
#'  `"actigraph"` or (in the unlikely event two devices were used) both (i.e.,
#'  `c("geneactiv", "actigraph")`). If you do not specify, both are used.
#' @param wear_location The location the device was warn. Can be either
#' `"wrist"`, `"hip"` or (in the unlikely event multiple locations were used)
#' both (i.e., `c("wrist","hip")`). If you do not specify, both are used.
#' @param overwrite If the existing output should be replaced. `FALSE` by
#' default.
#' @param verbose Determines how much output the function returns. Useful for
#' understanding what the package is doing. `TRUE` by default.
#' @param ... Pass additional parameters to [GGIR::g.shell.GGIR()]. Refer to the
#' `GGIR` manual for details.
#'
#' @family processing functions
#'
#' @export
process_files <- function(datadir,
                          outputdir,
                          ages = c("children", "adults"),
                          device = c("geneactiv", "actigraph"),
                          wear_location = c("wrist", "hip"),
                          overwrite = FALSE,
                          verbose = TRUE,
                          ...) {
  validate_GGIR(verbose)


  # Build the cut point settings
  ages <- match.arg(ages, c("children", "adults"), several.ok = TRUE)
  device <- match.arg(device, c("geneactiv", "actigraph"), several.ok = TRUE)
  wear_location <- match.arg(wear_location, c("wrist", "hip"),
    several.ok = TRUE
  )

  threshold.lig <- apply_thresholds("light", ages, device, wear_location)
  threshold.mod <- apply_thresholds("mod", ages, device, wear_location)
  threshold.vig <- apply_thresholds("vig", ages, device, wear_location)
  if (verbose) usethis::ui_done("Cutpoints set")

  # Check file paths
  datadir <- translate_filepath(datadir)
  outputdir <- translate_filepath(outputdir)

  # Call GGIR
  if (verbose) usethis::ui_info("Calling GGIR")

  GGIR::g.shell.GGIR(
    mode = c(1, 2, 3, 4, 5),
    datadir = datadir,
    outputdir = outputdir,
    do.report = c(2, 4, 5),
    overwrite = overwrite,
    do.enmo = TRUE,
    do.hfen = TRUE,
    do.enmoa = TRUE,
    # =====================
    # Part 2
    # =====================
    strategy = 2,
    maxdur = 0,
    includedaycrit = 16,
    qwindow = c(0, 24),
    mvpathreshold = 100,
    bout.metric = 4,
    excludefirstlast = TRUE,
    includenightcrit = 16,
    MX.ig.min.dur = 10,
    iglevels = 50,
    epochvalues2csv = FALSE,
    winhr = 16,
    qlevels = c(
      960 / 1440, # 2/3rds of the day
      1320 / 1440, # Top 120min
      1380 / 1440, # Top 60min
      1410 / 1440, # Top 30min
      1425 / 1440, # Top 15min
      1435 / 1440 # Top 5min
    ),
    # =====================
    # Part 3 + 4
    # =====================
    def.noc.sleep = 1,
    outliers.only = TRUE,
    criterror = 4,
    do.visual = TRUE,
    # =====================
    # Part 5
    # =====================
    threshold.lig = threshold.lig,
    threshold.mod = threshold.mod,
    threshold.vig = threshold.vig,
    boutcriter = 0.8, boutcriter.in = 0.9, boutcriter.lig = 0.8,
    boutcriter.mvpa = 0.8, boutdur.in = c(1, 10, 30), boutdur.lig = c(1, 10),
    boutdur.mvpa = c(1),
    includedaycrit.part5 = 2 / 3,
    # =====================
    # Visual report
    # =====================
    timewindow = c("WW"),
    visualreport = FALSE,
    ...
  )
}

reprocess <- function(){
  # TODO - The wrapper function
  # STEPS

  # 1. Run process_files (includes validation check)
  # 2. Collate the returned files into a single csv
  # 3. Generate the meta_data

}
