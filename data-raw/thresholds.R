# These are the thresholds for the intensity cut-points. These come from
# Hildebrand 2014 (https://doi.org/10.1249/mss.0000000000000289) and
# Hildebrand 2016 (https://doi.org/10.1111/sms.12795)

thresholds <-
  dplyr::tribble(
        ~age,        ~dev, ~wear_loc, ~light,  ~mod,  ~vig,
  "children", "geneactiv",   "wrist",   56.3, 191.6, 695.8,
  "children", "actigraph",   "wrist",   35.6, 201.4,   707,
  "children", "geneactiv",     "hip",   64.1, 152.8, 514.3,
  "children", "actigraph",     "hip",   63.3, 142.6, 464.6,
  "adults",   "geneactiv",   "wrist",   45.8,  93.2, 418.3,
  "adults",   "actigraph",   "wrist",   44.8, 100.6, 428.8,
  "adults",   "geneactiv",     "hip",   46.9,  68.7, 266.8,
  "adults",   "actigraph",     "hip",   47.4,  61.1, 258.7
)


usethis::use_data(thresholds, overwrite = TRUE, internal = TRUE)
