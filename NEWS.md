# sleepIPD 0.2.3

* `assemble_datasheet()` now reads `calendar_date` as text and parses it with
  a new internal helper, so part 2 dates keep their local day. Previously readr
  converted part 2's timestamps to UTC, which moved every midnight day east of
  UTC back by one day. Parts 4 and 5 are parsed the same way.
* `assemble_datasheet()` now keeps part 5 files whose names carry a prefix
  (e.g. `a_part5_daysummary_WW_...`). `collate_outputs()` already accepted
  these, but the datasheet silently dropped every part 5 column.
* `assemble_datasheet()` now errors when no part 5 rows match, and warns on
  duplicated part 2 dates, part 2 weekdays that disagree with the date, missing
  `SleepRegularityIndex` values, missing MM rows, and part 5 data that does not
  join.
* Datasheets built with earlier versions should be rebuilt by re-running
  `assemble_datasheet()` on the existing collated output. GGIR does not need to
  be re-run.

# sleepIPD 0.2.2

# sleepIPD 0.2.1

# sleepIPD 0.2.0

* Added a helper function (`assemble_datasheet`) for collecting and merging the reprocessed data.

# sleepIPD 0.1.2

* Fix the algorithm for the hip-worn measurement of sleep.

# sleepIPD 0.1.1

* Fixed the issue related to hip-worn measurement of sleep.

# sleepIPD 0.1.0

* Bumped to version used for reprocessing

# sleepIPD 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
* Added base functionality to the package.
* Created a 'getting started' vignette for contributors.
