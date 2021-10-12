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
#' @param studyname The name of the study. Only used if `datadir` is a vector of
#' files.
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
                          studyname = "NA",
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

  threshold_lig <- apply_thresholds("light", ages, device, wear_location)
  threshold_mod <- apply_thresholds("mod", ages, device, wear_location)
  threshold_vig <- apply_thresholds("vig", ages, device, wear_location)
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
    studyname = studyname,
    # =====================
    # Part 2
    # =====================
    do.imp = TRUE,
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
    ilevels = seq(0, 4000, 50),
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
    threshold.lig = threshold_lig,
    threshold.mod = threshold_mod,
    threshold.vig = threshold_vig,
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

#' Reprocess Accelerometer Data and Generate Metadata
#'
#' This is the primary wrapper function for sleepIPD. In most cases, this should
#' be the only function that is needed. `reprocess` collects data about your
#' study, and then calls the functions to reprocess the data, collate the
#' results, and generate the metadata.
#'
#' The purpose of sleepIPD is to standardise the processing of the
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
#' You should also supply a `studyname` argument. This is required by GGIR if
#' your `datadir` is a vector of files, but is additionally used by sleepIPD to
#' name the output files. If your study did not have a name, the last name of
#' the first author is sufficent (if you are supplying multiple studies, name
#' each one with the year the data were collected; e.g.,
#' \code{studyname = 'Hartwig 2021'}).
#'
#' @param datadir The directory with the files to process or a vector of
#' file paths to process (as per [GGIR::g.shell.GGIR()]).
#' @param outputdir the directory to store the output (as per
#' [GGIR::g.shell.GGIR()]).
#' @param studyname The name of the study. This is required to name the
#' metadata.
#' @param ages The age of the sample. Can be either `"children"` (for those 18
#' or less), `"adults"` (for >18) or both (i.e., `c("children", "adults")`). If
#' you do not specify, both are used.
#' @param device The device used in the study. Can be either `"geneactiv"`,
#'  `"actigraph"` or (in the unlikely event two devices were used) both (i.e.,
#'  `c("geneactiv", "actigraph")`). If you do not specify, both are used.
#' @param wear_location The location the device was warn. Can be either
#' `"wrist"`, `"hip"` or (in the unlikely event multiple locations were used)
#' both (i.e., `c("wrist","hip")`). If you do not specify, both are used.
#' @param overwrite If the existing output (including metadata) should be
#' replaced. `FALSE` by default.
#' @param verbose Determines how much output the function returns. Useful for
#' understanding what the package is doing. `TRUE` by default.
#' @param ... Pass additional parameters to [GGIR::g.shell.GGIR()]. Refer to the
#' `GGIR` manual for details.
#'
#' @family processing functions
#' @export
#'
#' @examples
#' \dontrun{
#' datadir <- "C:\\Documents\\My Study\\Data"
#' outputdir <- "C:\\Documents\\My Study\\Output"
#' studyname <- "Hartwig 2021"
#' ages <- "children"
#' device <- "geneactiv"
#' wear_location <- "wrist"
#'
#' reprocess(
#'   datadir = datadir,
#'   outputdir = outputdir,
#'   studyname = studyname,
#'   ages = ages,
#'   device = device,
#'   wear_location = wear_location
#' )
#' }
reprocess <- function(datadir,
                      outputdir,
                      studyname,
                      ages = c("children", "adults"),
                      device = c("geneactiv", "actigraph"),
                      wear_location = c("wrist", "hip"),
                      overwrite = FALSE,
                      verbose = TRUE,
                      ...) {
  start_time <- Sys.time()
  if (verbose) {
    usethis::ui_info(
      "Starting {usethis::ui_code('process_files()')}"
    )
  }
  process_files(
    datadir, outputdir, studyname, ages, device,
    wear_location, overwrite, verbose
  )
  end_process_files <- Sys.time()

  if (verbose) {
    print("/n")
    usethis::ui_done("Completed {usethis::ui_code('process_files()')}")
    usethis::ui_info("Starting {usethis::ui_code('collate_outputs()')}")
  }

  collate_outputs(outputdir, studyname, verbose)
  end_collate_outputs <- Sys.time()

  if (verbose) {
    usethis::ui_done("Completed {usethis::ui_code('collate_outputs()')}")
    usethis::ui_info("Starting {usethis::ui_code('build_meta()')}")
  }

  build_meta(outputdir, studyname, overwrite, wear_location)
  end_build_meta <- Sys.time()

  if (verbose) usethis::ui_done("Completed {usethis::ui_code('build_meta()')}")

  if (verbose & requireNamespace("knitr", quietly = TRUE)) {
    usethis::ui_done("Processing complete! Here is how long it took:")

    time_str <- function(endtime, starttime) {
      out <- difftime(endtime, starttime)
      paste(round(as.numeric(out), 2), units(out))
    }

    opts <- options(knitr.kable.NA = "-")

    print(
      knitr::kable(data.frame(
        "Process" = c(
          "GGIR Processing", "Collate Outputs",
          "Build Metadata", "Total"
        ),
        "Start Time" = c(
          start_time, end_process_files,
          end_collate_outputs, NA
        ),
        "End Time" = c(
          end_process_files, end_collate_outputs,
          end_build_meta, NA
        ),
        "Duration" = c(
          time_str(end_process_files, start_time),
          time_str(end_collate_outputs, end_process_files),
          time_str(end_build_meta, end_collate_outputs),
          time_str(end_build_meta, start_time)
        )
      ), format = "simple")
    )

    options(opts)
  }
}
