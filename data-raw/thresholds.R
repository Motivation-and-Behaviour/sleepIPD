# These are the thresholds for the intensity cut-points. These come from
# Hildebrand 2014 (https://doi.org/10.1249/mss.0000000000000289),
# Hildebrand 2016 (https://doi.org/10.1111/sms.12795),
# Hurter 2018 (https://doi.org/10.3390/children5120172) using non-dominant hand

thresholds <-
  dplyr::tribble(
        ~age,        ~dev, ~wear_loc, ~light,  ~mod,  ~vig, ~source,
  "children", "geneactiv",   "wrist",   51.6, 191.6, 695.8, "Light: [Hurter et al., 2018](https://doi.org/10.3390/children5120172)<br>
                                                             Mod: [Hildebrand et al., 2014](https://doi.org/10.1249/mss.0000000000000289)<br>
                                                             Vig: [Hildebrand et al., 2014](https://doi.org/10.1249/mss.0000000000000289)",
  "children", "actigraph",   "wrist",   48.1, 201.4,   707, "Light: [Hurter et al., 2018](https://doi.org/10.3390/children5120172)<br>
                                                             Mod: [Hildebrand et al., 2014](https://doi.org/10.1249/mss.0000000000000289)<br>
                                                             Vig: [Hildebrand et al., 2014](https://doi.org/10.1249/mss.0000000000000289)",
  "children", "geneactiv",     "hip",   64.1, 152.8, 514.3, "Light: [Hildebrand et al., 2016](https://doi.org/10.1111/sms.12795)<br>
                                                             Mod: [Hildebrand et al., 2014](https://doi.org/10.1249/mss.0000000000000289)<br>
                                                             Vig: [Hildebrand et al., 2014](https://doi.org/10.1249/mss.0000000000000289)",
  "children", "actigraph",     "hip",   32.6, 142.6, 464.6, "Light: [Hurter et al., 2018](https://doi.org/10.3390/children5120172)<br>
                                                             Mod: [Hildebrand et al., 2014](https://doi.org/10.1249/mss.0000000000000289)<br>
                                                             Vig: [Hildebrand et al., 2014](https://doi.org/10.1249/mss.0000000000000289)",
  "adults",   "geneactiv",   "wrist",   45.8,  93.2, 418.3, "Light: [Hildebrand et al., 2016](https://doi.org/10.1111/sms.12795)<br>
                                                             Mod: [Hildebrand et al., 2014](https://doi.org/10.1249/mss.0000000000000289)<br>
                                                             Vig: [Hildebrand et al., 2014](https://doi.org/10.1249/mss.0000000000000289)",
  "adults",   "actigraph",   "wrist",   44.8, 100.6, 428.8, "Light: [Hildebrand et al., 2016](https://doi.org/10.1111/sms.12795)<br>
                                                             Mod: [Hildebrand et al., 2014](https://doi.org/10.1249/mss.0000000000000289)<br>
                                                             Vig: [Hildebrand et al., 2014](https://doi.org/10.1249/mss.0000000000000289)",
  "adults",   "geneactiv",     "hip",   46.9,  68.7, 266.8, "Light: [Hildebrand et al., 2016](https://doi.org/10.1111/sms.12795)<br>
                                                             Mod: [Hildebrand et al., 2014](https://doi.org/10.1249/mss.0000000000000289)<br>
                                                             Vig: [Hildebrand et al., 2014](https://doi.org/10.1249/mss.0000000000000289)",
  "adults",   "actigraph",     "hip",   47.4,  61.1, 258.7, "Light: [Hildebrand et al., 2016](https://doi.org/10.1111/sms.12795)<br>
                                                             Mod: [Hildebrand et al., 2014](https://doi.org/10.1249/mss.0000000000000289)<br>
                                                             Vig: [Hildebrand et al., 2014](https://doi.org/10.1249/mss.0000000000000289)"
)


usethis::use_data(thresholds, overwrite = TRUE, internal = TRUE)
